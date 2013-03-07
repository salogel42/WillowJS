/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var fractionUtils = require('./fractionUtils').fractionUtils;
}

var expr = (function() {
	/**
	 * Object that stores all the relevant properties of an operator.
	 * @constructor
	 * @param identity {number} The value for which num op identity == num.
	 * @param precedence {number} The operator's precedence with respect to other operators
	 *                            (they go in pairs, so 2 and 3 are at the same level).
	 * @param commutative {boolean} True if a op b == b op a.
	 * @param associative {boolean} True if (a op b) op c == a op (b op c).
	 * @param evaluation {function} Defines how to perform the operation given two operands.
	 */
	function Operator(identity, precedence, commutative, associative, evaluate, evaluateValues) {
		this.identity = identity;
		this.precedence = precedence;
		this.commutative = commutative;
		this.associative = associative;
		this.evaluate = evaluate;
		this.evaluateValues = evaluateValues;
	}
	function getEquivalentOp(op) {
		if (op === '\\cdot') { return '*'; }
		if (op === '\\div') { return '/'; }
		return op;
	}
	function multiplyNodesMaybeNull(nodeOne, nodeTwo) {
		if (nodeTwo === null) { return nodeOne; }
		if (nodeOne === null) { return nodeTwo; }
		return expr.createCompoundExpression(nodeOne, nodeTwo, '*');
	}
	function pullOutRadical(node) {
		if (node.type === 'number' && fractionUtils.isValueRadical(node.value)) {
			return { radical : node, rest : null };
		}
		if (node.type === 'compound' && node.op === '*') {
			var left = pullOutRadical(node.lhs);
			var right = pullOutRadical(node.rhs);
			return {
				radical : multiplyNodesMaybeNull(left.radical, right.radical),
				rest : multiplyNodesMaybeNull(left.rest, right.rest)
			};
		}
		return { radical : null, rest : node };
	}
	function simplifyRadicalIntoNode(value) {
		if (fractionUtils.isValueFraction(value.radicand)) {
			return self.operatorProperties['/'].evaluate(
				simplifyRadicalIntoNode(
					fractionUtils.createRadicalValue(value.radicand.top, value.power)),
				simplifyRadicalIntoNode(
					fractionUtils.createRadicalValue(value.radicand.bottom, value.power)));
		}
		var result = fractionUtils.simplifyRadical(value);
		if (!fractionUtils.isValueMixed(result)) { return self.makeNumber(result); }
		result = self.operatorProperties['*'].evaluateValues(result.rational, result.radical);
		result.simplified = true;
		return result;
	}
	function combineRadicals(node) {
		if (node.type === 'compound' && node.op === '*') {
			var left = combineRadicals(node.lhs);
			var right = combineRadicals(node.rhs);
			if (left.type === 'number' && fractionUtils.isValueRadical(left.value) &&
				right.type === 'number' && fractionUtils.isValueRadical(right.value)) {
				var newValue = fractionUtils.combineRadicals(left.value, right.value);
				if (newValue !== errorNode) {
					newNode = simplifyRadicalIntoNode(newValue);
					// if we have \sqrt{-a}*\sqrt{-b} where a,b\in\N, that's equivalent to
					// \sqrt{a}i\sqrt{b}i, or \sqrt{ab}i^2, or -\sqrt{ab}.  We skip those steps and
					// go straight from \sqrt{-a}\sqrt{-b} to -\sqrt{ab}.
					if (fractionUtils.computeApproxNumericValue(left.value.radicand) < 0 &&
						fractionUtils.computeApproxNumericValue(right.value.radicand) < 0) {
						newNode = self.operatorProperties['-'].unaryEvaluate(newNode);
					}
					return newNode;
				}
			}
			return expr.createCompoundExpression(left, right, '*');
		}
		return node;
	}
	var self = {
		/**
		 * Takes two nodes and attempts to compute a numeric value for each, then returns errorNode
		 * if not comparable, a negative number if first is less than second, 0 if they are equal,
		 * and a positive number if first is bigger.
		 * @param  {Expression} first  First node to compare.
		 * @param  {Expression} second Second node to compare.
		 * @return {Number|errorNode}  Not numeric: errorNode
		 *                             first < second: negative num
		 *                             first = second: 0
		 *                             first > second: positive num
		 */
		compareNodesNumerically: function(first, second) {
			var firstNumeric = self.computeWithApproxNumericValue(first);
			var secondNumeric = self.computeWithApproxNumericValue(second);
			if (firstNumeric.type !== 'number' || secondNumeric.type !== 'number') {
				return errorNode;
			}
			return (firstNumeric.value - secondNumeric.value);
		},

		computeWithApproxNumericValue: function(node) {
			var computed = node;
			if (node.type === 'unary') {
				var child = self.computeWithApproxNumericValue(node.child);
				computed = self.operatorProperties[node.op].unaryEvaluate(child);
			}
			if (node.type === 'compound') {
				var lhs = self.computeWithApproxNumericValue(node.lhs);
				var rhs = self.computeWithApproxNumericValue(node.rhs);
				if (self.getPrecedence(node.op) === 0) {
					return expr.createCompoundExpression(lhs, rhs, node.op);
				}
				computed = self.operatorProperties[node.op].evaluate(lhs, rhs);
			}
			if (node.type === 'ternary') {
				var left = self.computeWithApproxNumericValue(node.left);
				var middle = self.computeWithApproxNumericValue(node.middle);
				var right = self.computeWithApproxNumericValue(node.right);
				return expr.createTernaryExpression(left, middle, right, node.op1, node.op2);
			}
			if (computed.type === 'number') {
				return expr.createSimpleExpression('number',
					fractionUtils.computeApproxNumericValue(computed.value));
			}
			return self.copyNode(node);
		},
		rationalizeDenominator: function(node) {
			if (node.type === 'number' && fractionUtils.isValueFraction(node.value) &&
				fractionUtils.isValueRadical(node.value.bottom)) {
				node = expr.createCompoundExpression(
					expr.createSimpleExpression('number', node.value.top),
					expr.createSimpleExpression('number', node.value.bottom), '/');
			}
			if (!(node.type === 'compound' && node.op === '/')) {
				return node;
			}
			var radicalOnBottom = pullOutRadical(node.rhs);

			if (radicalOnBottom.radical === null) {
				return node;
			}
			var rationalizingTerm = radicalOnBottom.radical;
			if (rationalizingTerm.type === 'compound') {
				rationalizingTerm = combineRadicals(rationalizingTerm);
			}
			rationalizingTerm.value.power = self.operatorProperties['-'].evaluateValues(1,
				rationalizingTerm.value.power).value;
			var bottom = self.makeNumber(rationalizingTerm.value.radicand);
			if (radicalOnBottom.rest !== null) {
				bottom = self.operatorProperties['*'].evaluate(radicalOnBottom.rest, bottom);
			}
			return self.operatorProperties['/'].evaluate(
				self.operatorProperties['*'].evaluate(node.lhs, rationalizingTerm),
				bottom);
		},
		makeNumber: function(value) {
			return self.createSimpleExpression('number', value);
		},
		/**
		 * Define all of the operators with all their individual properties.
		 * @dict
		 */
		operatorProperties: {
			'^' : new Operator(1, 3, false, false, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number' ||
							fractionUtils.isValueRadical(rhs.value)) {
							return self.createCompoundExpression(lhs, rhs, '^');
						}
						if (lhs.value === 1) { return lhs; }
						return self.operatorProperties['^'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						if (fractionUtils.isValueFraction(rhsValue)) {
							var result = simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
								lhsValue, rhsValue, false));
							return result;
						}
						var resultValue = 1;
						if (fractionUtils.isValueFraction(lhsValue)) {
							return self.operatorProperties['/'].evaluate(
								self.operatorProperties['^'].evaluateValues(
									lhsValue.top, rhsValue),
								self.operatorProperties['^'].evaluateValues(
									lhsValue.bottom, rhsValue));
						} else if (fractionUtils.isValueRadical(lhsValue)) {
							return simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
								lhsValue.radicand, self.operatorProperties['*'].evaluateValues(
									lhsValue.power, rhsValue).value, false));
						} else if (fractionUtils.isNegative(rhsValue)) {
							resultValue = fractionUtils.createFractionValue(
								1, Math.pow(lhsValue, -1*rhsValue));
						} else {
							resultValue = Math.pow(lhsValue, rhsValue);
						}
						return self.createSimpleExpression('number', resultValue);
					}),
			'/' : new Operator(1, 2, false, false, function(lhs, rhs) {
						if (!(lhs.numeric && rhs.numeric) &&
							(!lhs.simplified || !rhs.simplified)) {
							return self.createCompoundExpression(lhs, rhs, '/');
						}
						if (lhs.type !== 'number' || rhs.type !== 'number') {
							var result = expr.createCompoundExpression(lhs, rhs, '/');
							result.simplified = true;
							return result;
						}
						return self.operatorProperties['/'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (fractionUtils.isValueRadical(rhsValue)) {
							return self.operatorProperties['*'].evaluate(
								expr.createSimpleExpression('number', lhsValue),
								self.rationalizeDenominator(expr.createSimpleExpression('number',
									fractionUtils.invertFraction(rhsValue))));
						}
						if (fractionUtils.isValueRadical(lhsValue)) {
							var result = expr.createCompoundExpression(
								expr.createSimpleExpression('number', lhsValue),
								expr.createSimpleExpression('number', rhsValue),
								'/');
							result.simplified = true;
							return result;
						}
						if (fractionUtils.isValueFraction(rhsValue)) {
							return self.operatorProperties['*'].evaluateValues(
								lhsValue, fractionUtils.invertFraction(rhsValue));
						} else if (fractionUtils.isValueFraction(lhsValue)) {
							return self.operatorProperties['/'].evaluate(
								self.makeNumber(lhsValue.top),
								self.operatorProperties['*'].evaluateValues(
									lhsValue.bottom, rhsValue));
						} else {
							resultValue = fractionUtils.simplifyFraction(
								fractionUtils.createFractionValue(lhsValue, rhsValue));
						}
						return self.createSimpleExpression('number', resultValue);
					}),
			'*' : new Operator(1, 2, true, true, function(lhs, rhs) {
						if (!lhs.numeric || !rhs.numeric) {
							return self.createCompoundExpression(lhs, rhs, '*');
						}
						if (lhs.type === 'compound') {
							if (getPrecedence(lhs.op) === 2) {
								// (2/x)*3 = (2*3)/x
								if (lhs.lhs.type === 'number' &&
									!fractionUtils.isValueRadical(lhs.lhs.value)) {
									return self.operatorProperties[lhs.op].evaluate(
										self.operatorProperties['*'].evaluate(lhs.lhs, rhs),
										lhs.rhs);
								}
								// (x/2)*3 = x*(3/2)
								if (lhs.rhs.type === 'number' &&
									!fractionUtils.isValueRadical(lhs.rhs.value)) {
									return self.operatorProperties['*'].evaluate(lhs.lhs,
										self.operatorProperties[lhs.op].evaluate(rhs, lhs.rhs));
								}
							}
						}
						if (rhs.type === 'compound') {
							if (getPrecedence(rhs.op) === 2) {
								// 2*(3/x) = (2*3)/x
								if (rhs.lhs.type === 'number' &&
									!fractionUtils.isValueRadical(rhs.lhs.value)) {
									return self.operatorProperties[rhs.op].evaluate(
										self.operatorProperties['*'].evaluate(lhs, rhs.lhs),
										rhs.rhs);
								}
								// 2*(x/3) = (2/3)*x
								if (rhs.rhs.type === 'number' &&
									!fractionUtils.isValueRadical(rhs.rhs.value)) {
									return self.operatorProperties['*'].evaluate(
										self.operatorProperties[rhs.op].evaluate(lhs, rhs.rhs),
										rhs.lhs);
								}
							}
						}
						if (lhs.type !== 'number' || rhs.type !== 'number') {
							return self.createCompoundExpression(lhs, rhs, '*');
						}
						return self.operatorProperties['*'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (fractionUtils.isValueRadical(lhsValue) && !lhsValue.simplified) {
							return self.operatorProperties['*'].evaluate(
								simplifyRadicalIntoNode(lhsValue),
								self.makeNumber(rhsValue));
						}
						if (fractionUtils.isValueRadical(rhsValue) && !rhsValue.simplified) {
							return self.operatorProperties['*'].evaluate(self.makeNumber(lhsValue),
								simplifyRadicalIntoNode(rhsValue));
						}
						if (fractionUtils.isValueRadical(lhsValue) ||
							fractionUtils.isValueRadical(rhsValue)) {
							return combineRadicals(self.createCompoundExpression(
								self.makeNumber(lhsValue), self.makeNumber(rhsValue), '*'));
						}
						return self.createSimpleExpression('number',
							fractionUtils.multiplyFractions(lhsValue, rhsValue));
					}),
			'-' : new Operator(0, 1, false, false, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number' ||
							(fractionUtils.isValueRadical(lhs.value) ||
								fractionUtils.isValueRadical(rhs.value))) {
							return self.createCompoundExpression(lhs, rhs, '-');
						}
						return self.operatorProperties['-'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (!fractionUtils.isValueFraction(lhsValue) &&
							!fractionUtils.isValueFraction(rhsValue)) {
							resultValue = lhsValue - rhsValue;
						} else {
							return self.operatorProperties['+'].evaluateValues(lhsValue,
								this.unaryEvaluateValues(rhsValue).value);
						}
						return self.createSimpleExpression('number', resultValue);
					}),
			'+' : new Operator(0, 1, true, true, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number') {
							return self.createCompoundExpression(lhs, rhs, '+');
						} else if (fractionUtils.isValueRadical(lhs.value) ||
								fractionUtils.isValueRadical(rhs.value)) {
							if (lhs.syntacticEquals(rhs)) {
								var newNode = self.createCompoundExpression(
									self.makeNumber(2), lhs, '*');
								newNode.simplified = true;
								return newNode;
							}
							return self.createCompoundExpression(lhs, rhs, '+');
						}
						return self.operatorProperties['+'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (!fractionUtils.isValueFraction(lhsValue) &&
							!fractionUtils.isValueFraction(rhsValue)) {
							resultValue = lhsValue + rhsValue;
						} else {
							if (!fractionUtils.isValueFraction(lhsValue)) {
								lhsValue = fractionUtils.createFractionValue(lhsValue, 1);
							}
							if (!fractionUtils.isValueFraction(rhsValue)) {
								rhsValue = fractionUtils.createFractionValue(rhsValue, 1);
							}
							resultValue = fractionUtils.addFractions(lhsValue, rhsValue);
						}
						return self.createSimpleExpression('number', resultValue);
					}),
			'\\sqrt' : new Operator(0, -1, false, false, function(lhs, rhs) {
						return self.operatorProperties['^'].evaluate(rhs,
							self.operatorProperties['/'].evaluateValues(1, lhs.value));
					}, function(lhsValue, rhsValue) {
						// Assumes lhs is the root, and rhs is the value under the sqrt.
						return self.operatorProperties['^'].evaluate(self.makeNumber(rhsValue),
							self.operatorProperties['/'].evaluateValues(1, lhsValue));
					}),
			'|' : {
				'precedence' : -1,
				'commutative' : false,
				'associative' : false,
				'unaryEvaluate' : function(child) {
						return self.makeNumber(fractionUtils.abs(child.value));
					}
				},
			// equality is a bit out of place here, as it's not an operator we evaluate.  Since it
			// only has some of the properties of other operators, don't use the same constructor,
			// just define it here.
			'=' : {
					'precedence' : 0,
					'commutative' : true,
					'associative' : false
				  },
			'!=' : {
					'precedence' : 0,
					'commutative' : true,
					'associative' : false
				  },
			'<' : {
					'precedence' : 0,
					'commutative' : false,
					'associative' : false
				  },
			'>' : {
					'precedence' : 0,
					'commutative' : false,
					'associative' : false
				  },
			'<=' : {
					'precedence' : 0,
					'commutative' : false,
					'associative' : false
				  },
			'>=' : {
					'precedence' : 0,
					'commutative' : false,
					'associative' : false
				  },
			getInverseInequality: function(op) {
				if (op === '<') { return '>'; }
				if (op === '>') { return '<'; }
				if (op === '<=') { return '>='; }
				if (op === '>=') { return '<='; }
				if (op === '=') { return '='; }
				return '';
			},
			getDirection: function(op) {
				if (op === '<') { return 'less'; }
				if (op === '>') { return 'greater'; }
				if (op === '<=') { return 'less'; }
				if (op === '>=') { return 'greater'; }
				return '';
			},
			inverseOp: function(op) {
				switch (op) {
					case '+' : return '-';
					case '-' : return '+';
					case '*' : return '/';
					case '/' : return '*';
				}
			}
		},
		/**
		 * Convenience function to make things more readable.
		 * @param {string} op The string representing the operator, to look up in the dict above.
		 * @return {number} The operator's precedence.
		 */
		getPrecedence: function(op) {
			op = getEquivalentOp(op);
			return self.operatorProperties[op].precedence;
		},
		nextId: 0,
		nodeMap: (typeof window === 'undefined') ? null : {},
		makeIntoFractionNodeIfApplicable: function(node) {
			if (!utils.isNumericFraction(node)) { return node; }
			var frac = fractionUtils.simplifyFraction(
					fractionUtils.createFractionValue(node.lhs.value, node.rhs.value));
			if (frac === errorNode) { return errorNode; }
			return self.createSimpleExpression('number', frac);
		},
		copyNode: function(node) {
			if (utils.isSimpleExpression(node)) {
				return self.createSimpleExpression(node.type, node.value);
			}
			if (node.type === 'unary') {
				return self.createUnaryExpression(self.copyNode(node.child), node.op);
			}
			// 'compound'
			return self.createCompoundExpression(
				self.copyNode(node.lhs), self.copyNode(node.rhs), node.op);
		},
		makeAdditionCommutative: function(node) {
			if (node !== null && node.type === 'compound' && node.op === '-') {
				// Take a T-T expression and turn it into a T + (-T)
				return self.createCompoundExpression(self.copyNode(node.lhs),
					self.createUnaryExpression(self.copyNode(node.rhs), '-'), '+');
			}
			return node;
		},
		makeCommutativeIfNecessary: function(node) {
			if (node !== null && node.type === 'compound' && node.op === '/' &&
				!utils.isOneOverSomething(node)) {
				return self.createCompoundExpression(self.copyNode(node.lhs),
					self.createCompoundExpression(self.createSimpleExpression('number', 1),
						self.copyNode(node.rhs), '/'), '*');
			}
			return self.makeAdditionCommutative(node);
		},
		makeCommutativeIfNecessaryRec: function(node, op) {
			if ((node.type === 'compound' || node.type === 'ternary') &&
				getPrecedence(node.op) === getPrecedence(op)) {
				var newNode = node;
				if (node.type === 'compound') {
					newNode = self.createCompoundExpression(
						self.makeCommutativeIfNecessaryRec(node.lhs, op),
						self.makeCommutativeIfNecessaryRec(node.rhs, op),
						node.op);
				}
				if (node.type === 'ternary') {
					newNode = self.createTernaryExpression(
						self.makeCommutativeIfNecessaryRec(node.left, op),
						self.makeCommutativeIfNecessaryRec(node.middle, op),
						self.makeCommutativeIfNecessaryRec(node.right, op),
						node.op1,
						node.op2);
				}
				return self.makeCommutativeIfNecessary(newNode);
			}
			return node;
		},
		arithmeticEvaluation: function(node, expand) {
			if (node.simplified) { return node; }
			var newNode = expr.rationalizeDenominator(node);
			if (!node.syntacticEquals(newNode)) { return newNode; }
			if (node.type === 'number' && fractionUtils.isValueFraction(node.value)) {
				newNode = self.createSimpleExpression('number',
					fractionUtils.simplifyFraction(node.value));
				newNode.simplified = true;
				return newNode;
			}
			if (node.type === 'number' && fractionUtils.isValueRadical(node.value)) {
				return simplifyRadicalIntoNode(node.value);
			}
			if (node.type === 'unary') {
				if (node.child.type === 'number') {
					if (fractionUtils.isValueRadical(node.child.value)) { return newNode; }
					else { newNode = self.operatorProperties[node.op].unaryEvaluate(node.child); }
				} else {
					var newChild = self.arithmeticEvaluation(node.child);
					if (utils.isUnaryNegative(node) && utils.isUnaryNegative(newChild)) {
						return newChild.child;
					}
					if (expand && utils.isUnaryNegative(node)) {
						newNode = self.createCompoundExpression(
							self.createSimpleExpression('number', -1),
							newChild, '*');
					} else {
						newNode = self.createUnaryExpression(newChild, node.op);
					}
				}
				return newNode;
			}
			if (node.type !== 'compound' || self.getPrecedence(node.op) === 0) { return node; }
			node.lhs = self.makeIntoFractionNodeIfApplicable(node.lhs);
			node.rhs = self.makeIntoFractionNodeIfApplicable(node.rhs);
			if (node.lhs === errorNode || node.rhs === errorNode) { return errorNode; }
			newNode = expr.createCompoundExpression(node.lhs, node.rhs, node.op);
			if (node.lhs.numeric && node.rhs.numeric) {
				if ((node.lhs.type !== 'number' && node.rhs.type !== 'number') &&
					(!node.lhs.simplified || !node.rhs.simplified)) { return newNode; }
				newNode = self.operatorProperties[node.op].evaluate(node.lhs, node.rhs);
				if (!newNode.syntacticEquals(node)) {
					newNode.simplified = true;
					return newNode;
				}
			}
			if (node.rhs.type === 'number' &&
				node.rhs.value === self.operatorProperties[node.op].identity) {
				newNode = node.lhs;
			} else if (node.rhs.type === 'number' && node.rhs.value === 0) {
				if (node.op === '*') { newNode = node.rhs; }
				if (node.op === '^') { newNode = self.createSimpleExpression('number', 1); }
				if (node.op === '/') { return errorNode; }
			} else if (self.getPrecedence(node.op) === 2 && utils.isNegativeOne(node.rhs) &&
				!expand) {
				newNode = self.createUnaryExpression(node.lhs, '-');
			} else if (node.op === '*' && utils.isNegativeOne(node.lhs) && !expand) {
				newNode = self.createUnaryExpression(node.rhs, '-');
			} else if (node.op === '^' && utils.isNegativeOne(node.rhs) && !expand) {
				newNode = self.createCompoundExpression(
					self.createSimpleExpression('number', 1), node.lhs, '/');
			} else {
				node = self.makeCommutativeIfNecessary(node);
				if (node.lhs.type === 'number') {
					if (self.operatorProperties[node.op].commutative &&
						node.lhs.value === self.operatorProperties[node.op].identity) {
						newNode = node.rhs;
					} else if (node.lhs.value === 0 &&
						self.operatorProperties[node.op].precedence > 1) {
						newNode = node.lhs;
					}
				}
			}
			return newNode;
		},
		createExpression: function(type, numeric) {
			var id = self.nextId++;
			var simplified = (type === 'number');
			var expression = {
				'id' : id,
				'type': type,
				'parent' : null,
				'numeric' : numeric,
				'simplified' : simplified,
				'syntacticEquals' : function(node) {
					if (this.type !== node.type) {
						return false;
					} else if (utils.isSimpleExpression(this)) {
						return fractionUtils.compareFractions(this.value, node.value, false);
					} else if (this.type === 'ternary') {
						return (this.op1 === node.op2) && (this.op2 === node.op2) &&
							this.left.syntacticEquals(node.left) &&
							this.middle.syntacticEquals(node.middle) &&
							this.right.syntacticEquals(node.right);
					} else if (this.op !== node.op) {
						return false;
					} else if (this.type === 'unary') {
						return this.child.syntacticEquals(node.child);
					} else if (this.type === 'compound') {
						return this.lhs.syntacticEquals(node.lhs) &&
							this.rhs.syntacticEquals(node.rhs);
					}
					return false;
				},
				'evaluateArithmetic' : function(expand) {
					if (this.simplified) { return null; }
					var parent = this.parent;
					var side = utils.getSide(this);
					var copy = self.copyNode(this);
					var node = self.arithmeticEvaluation(this, expand);
					if (node === errorNode) { return errorNode; }
					if (node.syntacticEquals(copy)) { return null; }
					utils.setChild(node, parent, side);
					return node;
				},
				'combineEqualExpressions' : function() {
					var newNode = null;
					if (this.type === 'compound' && this.lhs.syntacticEquals(this.rhs) &&
						this.op !== '^') {
						var parent = this.parent;
						var side = utils.getSide(this);
						if (this.op === '+') {
							newNode = self.createCompoundExpression(
								self.createSimpleExpression('number', 2), this.lhs, '*');
						} else if (this.op === '-') {
							newNode = self.createSimpleExpression('number', 0);
						} else if (this.op === '/') {
							newNode = self.createSimpleExpression('number', 1);
						} else if (this.op === '*') {
							newNode = self.createCompoundExpression(this.lhs,
								self.createSimpleExpression('number', 2), '^');
						}
						utils.setChild(newNode, parent, side);
					}
					return newNode;
				},
				'associate' : function() {
					var newParent = null;
					if (type === 'compound') {
						var parent = this.parent;
						if (parent === null) { return null; }
						var side = utils.getSide(this);
						if (self.getPrecedence(this.op) !== self.getPrecedence(parent.op) ||
							(side === 'right' && this.op !== parent.op)) { return null; }
						var grandparent = parent.parent;
						var parentSide = utils.getSide(parent);
						var node = this;
						if (side === 'left') { node = self.makeCommutativeIfNecessary(this); }
						if (node.op === parent.op &&
							!self.operatorProperties[node.op].associative) {
							return null;
						}
						utils.setChild(node, parent, side);

						var otherChild = parent.lhs;
						if (node === otherChild) {
							otherChild = parent.rhs;
							var newRhs = self.createCompoundExpression(
								node.rhs, otherChild, parent.op);
							newParent = self.createCompoundExpression(node.lhs, newRhs, node.op);
						} else {
							var newLhs = self.createCompoundExpression(
								otherChild, node.lhs, parent.op);
							newParent = self.createCompoundExpression(newLhs, node.rhs, node.op);
						}
						utils.setChild(newParent, grandparent, parentSide);
					}
					return newParent;
				},
				'commute' : function() {
					var parent = this.parent;
					var side = utils.getSide(this);
					var node = self.makeCommutativeIfNecessary(this);
					if (type === 'compound' && self.operatorProperties[node.op].commutative) {
						var newNode = self.createCompoundExpression(node.rhs, node.lhs, node.op);
						utils.setChild(newNode, parent, side);
						return newNode;
					}
					return null;
				},
				'distributeLeft' : function() {
					var newNode = null;
					if (this.type === 'compound' && this.op === '*') {
						var parent = this.parent;
						var side = utils.getSide(this);
						if (this.rhs.type === 'compound' &&
							self.getPrecedence(this.rhs.op) === 1) {
							newNode = self.createCompoundExpression(
								self.createCompoundExpression(this.lhs, this.rhs.lhs, this.op),
								self.createCompoundExpression(this.lhs, this.rhs.rhs, this.op),
								this.rhs.op);
						}
						utils.setChild(newNode, parent, side);
					}
					return newNode;
				},
				'distributeRight' : function() {
					var newNode = null;
					if (this.type === 'compound' && this.op === '*' || this.op === '/') {
						var parent = this.parent;
						var side = utils.getSide(this);
						if (this.lhs.type === 'compound' &&
							self.getPrecedence(this.lhs.op) === 1) {
							newNode = self.createCompoundExpression(
								self.createCompoundExpression(this.lhs.lhs, this.rhs, this.op),
								self.createCompoundExpression(this.lhs.rhs, this.rhs, this.op),
								this.lhs.op);
						}
						utils.setChild(newNode, parent, side);
					}
					return newNode;
				},
				'factorLeft' : function() {
					var newNode = null;
					if (this.type === 'compound' && self.getPrecedence(this.op) === 1) {
						var newLhs = self.makeCommutativeIfNecessary(this.lhs);
						var newRhs = self.makeCommutativeIfNecessary(this.rhs);
						if (this.lhs.type === 'compound' && self.getPrecedence(newLhs.op) === 2 &&
							newRhs.type === 'compound' && newRhs.op === newLhs.op &&
							newLhs.lhs.syntacticEquals(newRhs.lhs)) {
							var parent = this.parent;
							var side = utils.getSide(this);
							newNode = self.createCompoundExpression(newLhs.lhs,
								self.createCompoundExpression(newLhs.rhs, newRhs.rhs, this.op),
								newLhs.op);
							utils.setChild(newNode, parent, side);
						}
					}
					return newNode;
				},
				'factorRight' : function() {
					var newNode = null;
					if (this.type === 'compound' && self.getPrecedence(this.op) === 1 &&
						this.lhs.type === 'compound' && self.getPrecedence(this.lhs.op) === 2 &&
						this.rhs.type === 'compound' && this.rhs.op === this.lhs.op &&
						this.lhs.rhs.syntacticEquals(this.rhs.rhs)) {
						var parent = this.parent;
						var side = utils.getSide(this);
						newNode = self.createCompoundExpression(
							self.createCompoundExpression(this.lhs.lhs, this.rhs.lhs, this.op),
							this.lhs.rhs,
							this.lhs.op);
						utils.setChild(newNode, parent, side);
					}
					return newNode;
				}
			};
			if (self.nodeMap !== null) {
				self.nodeMap[id] = expression;
			}
			return expression;
		},
		createSimpleExpression: function(type, value) {
			var isNum = (type === 'number');
			var expression = self.createExpression(type, isNum);
			expression.value = value;
			if (isNum && typeof value !== 'number') {
				expression.simplified = false;
			}
			return expression;
		},
		createCompoundExpression: function(lhs, rhs, op) {
			if (lhs === errorNode || rhs === errorNode) { return errorNode; }
			var expression = self.createExpression('compound', (lhs.numeric && rhs.numeric));
			if (expression === errorNode) { return errorNode; }
			expression.lhs = lhs;
			expression.rhs = rhs;
			op = getEquivalentOp(op);
			expression.op = op;

			lhs.parent = expression;
			rhs.parent = expression;
			return expression;
		},
		createTernaryExpression: function(left, middle, right, op1, op2) {
			if (left === errorNode || middle === errorNode || right === errorNode) {
				return errorNode;
			}
			var expression = self.createExpression('ternary',
				(left.numeric && middle.numeric && right.numeric));
			if (expression === errorNode) { return errorNode; }
			expression.left = left;
			expression.middle = middle;
			expression.right = right;
			op1 = getEquivalentOp(op1);
			expression.op1 = op1;
			op2 = getEquivalentOp(op2);
			expression.op2 = op2;
			// Set op so that display works properly, really only need the precedence so
			// it doesn't matter which one we use, as both should be precedence 0.
			expression.op = op1;

			left.parent = expression;
			right.parent = expression;
			return expression;
		},
		createUnaryExpression: function(child, op) {
			if (child === errorNode) { return errorNode; }
			var expression = self.createExpression('unary', child.numeric);
			expression.child = child;
			expression.op = op;

			child.parent = expression;
			return expression;
		}
	};
	/**
	 * Add a unaryEvaluate function to '-' and '\\sqrt' (I don't do it in the constructor
	 * since only a few have such a function).
	 * @param {number} childValue The number to invert.
	 * @return {number} The inverted number.
	 */
	self.operatorProperties['-'].unaryEvaluate = function(child) {
		return self.operatorProperties['*'].evaluate(
			self.createSimpleExpression('number', -1), child);
	};
	self.operatorProperties['-'].unaryEvaluateValues = function(child) {
		return self.operatorProperties['*'].evaluateValues(-1, child);
	};

	self.operatorProperties['\\sqrt'].unaryEvaluate = function(child, expand) {
		if (typeof expand === 'undefined') { expand = false; }
		// TODO: respect surds?
		return self.operatorProperties['^'].evaluateValues(child.value,
			fractionUtils.createFractionValue(1, 2));
	};
	
	return self;
}());

var operatorProperties = expr.operatorProperties;
var getPrecedence = expr.getPrecedence;

if (typeof exports !== 'undefined') {
	exports.expr = expr;
	exports.operatorProperties = expr.operatorProperties;
	exports.getPrecedence = expr.getPrecedence;
}