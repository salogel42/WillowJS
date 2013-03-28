/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var fractionUtils = require('./fractionUtils').fractionUtils;
	var expression = require('./expression.js').expression;
}

var expr = (function() {
	/**
	 * Object that stores all the relevant properties of an operator.
	 * @constructor
	 * @param identity   {number}    The value for which num op identity == num.
	 * @param precedence {number}    The operator's precedence with respect to other operators
	 *                               (they go in pairs, so 2 and 3 are at the same level).
	 * @param commutative {boolean}  True if a op b == b op a.
	 * @param associative {boolean}  True if (a op b) op c == a op (b op c).
	 * @param evaluation  {function} Defines how to perform the operation given two operands.
	 */
	function Operator(identity, precedence, commutative, associative, evaluate, evaluateValues) {
		this.identity = identity;
		this.precedence = precedence;
		this.commutative = commutative;
		this.associative = associative;
		this.evaluate = evaluate;
		this.evaluateValues = evaluateValues;
	}
	function multiplyNodesMaybeNull(nodeOne, nodeTwo) {
		if (nodeTwo === null) { return nodeOne; }
		if (nodeOne === null) { return nodeTwo; }
		return expression.createCompoundExpression(nodeOne, nodeTwo, '*');
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
	function combineRadicals(node) {
		if (node.type === 'compound' && node.op === '*') {
			var left = combineRadicals(node.lhs);
			var right = combineRadicals(node.rhs);
			if (left.type === 'number' && fractionUtils.isValueRadical(left.value) &&
				right.type === 'number' && fractionUtils.isValueRadical(right.value)) {
				var newValue = fractionUtils.combineRadicals(left.value, right.value);
				if (newValue !== errorNode) {
					newNode = self.simplifyRadicalIntoNode(newValue);
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
			return expression.createCompoundExpression(left, right, '*');
		}
		return node;
	}
	var self = {
		simplifyRadicalIntoNode: function(value) {
			if (fractionUtils.isValueFraction(value.radicand)) {
				return self.operatorProperties['/'].evaluate(
					self.simplifyRadicalIntoNode(
						fractionUtils.createRadicalValue(value.radicand.top, value.power)),
					self.simplifyRadicalIntoNode(
						fractionUtils.createRadicalValue(value.radicand.bottom, value.power)));
			}
			var result = fractionUtils.simplifyRadical(value);
			if (!fractionUtils.isValueMixed(result)) { return self.makeNumber(result); }
			result = self.operatorProperties['*'].evaluateValues(result.rational, result.radical);
			result.simplified = true;
			return result;
		},
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
					return expression.createCompoundExpression(lhs, rhs, node.op);
				}
				computed = self.operatorProperties[node.op].evaluate(lhs, rhs);
			}
			if (node.type === 'ternary') {
				var left = self.computeWithApproxNumericValue(node.left);
				var middle = self.computeWithApproxNumericValue(node.middle);
				var right = self.computeWithApproxNumericValue(node.right);
				return expression.createTernaryExpression(left, middle, right, node.op1, node.op2);
			}
			if (computed.type === 'number') {
				return expression.createSimpleExpression('number',
					fractionUtils.computeApproxNumericValue(computed.value));
			}
			return self.copyNode(node);
		},
		rationalizeDenominator: function(node) {
			if (node.type === 'number' && fractionUtils.isValueFraction(node.value) &&
				fractionUtils.isValueRadical(node.value.bottom)) {
				node = expression.createCompoundExpression(
					expression.createSimpleExpression('number', node.value.top),
					expression.createSimpleExpression('number', node.value.bottom), '/');
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
			return expression.createSimpleExpression('number', value);
		},
		/**
		 * Define all of the operators with all their individual properties.
		 * @dict
		 */
		operatorProperties: {
			'^' : new Operator(1, 3, false, false, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number' ||
							fractionUtils.isValueRadical(rhs.value)) {
							return expression.createCompoundExpression(lhs, rhs, '^');
						}
						if (lhs.value === 1) { return lhs; }
						return self.operatorProperties['^'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						if (fractionUtils.isValueFraction(rhsValue)) {
							var result = self.simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
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
							return self.simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
								lhsValue.radicand, self.operatorProperties['*'].evaluateValues(
									lhsValue.power, rhsValue).value, false));
						} else if (fractionUtils.isNegative(rhsValue)) {
							resultValue = fractionUtils.createFractionValue(
								1, Math.pow(lhsValue, -1*rhsValue));
						} else {
							resultValue = Math.pow(lhsValue, rhsValue);
						}
						return expression.createSimpleExpression('number', resultValue);
					}),
			'/' : new Operator(1, 2, false, false, function(lhs, rhs) {
						if (!(lhs.numeric && rhs.numeric) &&
							(!lhs.simplified || !rhs.simplified)) {
							return expression.createCompoundExpression(lhs, rhs, '/');
						}
						if (lhs.type !== 'number' || rhs.type !== 'number') {
							var result = expression.createCompoundExpression(lhs, rhs, '/');
							result.simplified = true;
							return result;
						}
						return self.operatorProperties['/'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (fractionUtils.isValueRadical(rhsValue)) {
							return self.operatorProperties['*'].evaluate(
								expression.createSimpleExpression('number', lhsValue),
								self.rationalizeDenominator(expression.createSimpleExpression('number',
									fractionUtils.invertFraction(rhsValue))));
						}
						if (fractionUtils.isValueRadical(lhsValue)) {
							var result = expression.createCompoundExpression(
								expression.createSimpleExpression('number', lhsValue),
								expression.createSimpleExpression('number', rhsValue),
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
						return expression.createSimpleExpression('number', resultValue);
					}),
			'*' : new Operator(1, 2, true, true, function(lhs, rhs) {
						if (!lhs.numeric || !rhs.numeric) {
							return expression.createCompoundExpression(lhs, rhs, '*');
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
							return expression.createCompoundExpression(lhs, rhs, '*');
						}
						return self.operatorProperties['*'].evaluateValues(lhs.value, rhs.value);
					}, function(lhsValue, rhsValue) {
						var resultValue = null;
						if (fractionUtils.isValueRadical(lhsValue) && !lhsValue.simplified) {
							return self.operatorProperties['*'].evaluate(
								self.simplifyRadicalIntoNode(lhsValue),
								self.makeNumber(rhsValue));
						}
						if (fractionUtils.isValueRadical(rhsValue) && !rhsValue.simplified) {
							return self.operatorProperties['*'].evaluate(self.makeNumber(lhsValue),
								self.simplifyRadicalIntoNode(rhsValue));
						}
						if (fractionUtils.isValueRadical(lhsValue) ||
							fractionUtils.isValueRadical(rhsValue)) {
							return combineRadicals(expression.createCompoundExpression(
								self.makeNumber(lhsValue), self.makeNumber(rhsValue), '*'));
						}
						return expression.createSimpleExpression('number',
							fractionUtils.multiplyFractions(lhsValue, rhsValue));
					}),
			'-' : new Operator(0, 1, false, false, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number' ||
							(fractionUtils.isValueRadical(lhs.value) ||
								fractionUtils.isValueRadical(rhs.value))) {
							return expression.createCompoundExpression(lhs, rhs, '-');
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
						return expression.createSimpleExpression('number', resultValue);
					}),
			'+' : new Operator(0, 1, true, true, function(lhs, rhs) {
						if (lhs.type !== 'number' || rhs.type !== 'number') {
							return expression.createCompoundExpression(lhs, rhs, '+');
						} else if (fractionUtils.isValueRadical(lhs.value) ||
								fractionUtils.isValueRadical(rhs.value)) {
							if (lhs.syntacticEquals(rhs)) {
								var newNode = expression.createCompoundExpression(
									self.makeNumber(2), lhs, '*');
								newNode.simplified = true;
								return newNode;
							}
							return expression.createCompoundExpression(lhs, rhs, '+');
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
						return expression.createSimpleExpression('number', resultValue);
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
			op = utils.getEquivalentOp(op);
			return self.operatorProperties[op].precedence;
		},
		makeIntoFractionNodeIfApplicable: function(node) {
			if (!utils.isNumericFraction(node)) { return node; }
			var frac = fractionUtils.simplifyFraction(
					fractionUtils.createFractionValue(node.lhs.value, node.rhs.value));
			if (frac === errorNode) { return errorNode; }
			return expression.createSimpleExpression('number', frac);
		},
		copyNode: function(node) {
			if (utils.isSimpleExpression(node)) {
				return expression.createSimpleExpression(node.type, node.value);
			}
			if (node.type === 'unary') {
				return expression.createUnaryExpression(self.copyNode(node.child), node.op);
			}
			// 'compound'
			return expression.createCompoundExpression(
				self.copyNode(node.lhs), self.copyNode(node.rhs), node.op);
		},
		makeAdditionCommutative: function(node) {
			if (node !== null && node.type === 'compound' && node.op === '-') {
				// Take a T-T expression and turn it into a T + (-T)
				return expression.createCompoundExpression(self.copyNode(node.lhs),
					expression.createUnaryExpression(self.copyNode(node.rhs), '-'), '+');
			}
			return node;
		},
		makeCommutativeIfNecessary: function(node) {
			if (node !== null && node.type === 'compound' && node.op === '/' &&
				!utils.isOneOverSomething(node)) {
				return expression.createCompoundExpression(self.copyNode(node.lhs),
					expression.createCompoundExpression(expression.createSimpleExpression('number', 1),
						self.copyNode(node.rhs), '/'), '*');
			}
			return self.makeAdditionCommutative(node);
		},
		makeCommutativeIfNecessaryRec: function(node, op) {
			if ((node.type === 'compound' || node.type === 'ternary') &&
				getPrecedence(node.op) === getPrecedence(op)) {
				var newNode = node;
				if (node.type === 'compound') {
					newNode = expression.createCompoundExpression(
						self.makeCommutativeIfNecessaryRec(node.lhs, op),
						self.makeCommutativeIfNecessaryRec(node.rhs, op),
						node.op);
				}
				if (node.type === 'ternary') {
					newNode = expression.createTernaryExpression(
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
		// TODO(sdspikes): tests
		combineEqualExpressions: function(node) {
			var newNode = null;
			if (node.type === 'compound' && node.lhs.syntacticEquals(node.rhs) &&
				node.op !== '^') {
				var parent = node.parent;
				var side = utils.getSide(node);
				if (node.op === '+') {
					newNode = expression.createCompoundExpression(
						expression.createSimpleExpression('number', 2), node.lhs, '*');
				} else if (node.op === '-') {
					newNode = expression.createSimpleExpression('number', 0);
				} else if (node.op === '/') {
					newNode = expression.createSimpleExpression('number', 1);
				} else if (node.op === '*') {
					newNode = expression.createCompoundExpression(node.lhs,
						expression.createSimpleExpression('number', 2), '^');
				}
				utils.setChild(newNode, parent, side);
			}
			return newNode;
		},
		associate : function(node) {
			var newParent = null;
			if (node.type === 'compound') {
				var parent = node.parent;
				if (parent === null) { return null; }
				var side = utils.getSide(node);
				if (self.getPrecedence(node.op) !== self.getPrecedence(parent.op) ||
					(side === 'right' && node.op !== parent.op)) { return null; }
				var grandparent = parent.parent;
				var parentSide = utils.getSide(parent);
				if (side === 'left') { node = self.makeCommutativeIfNecessary(node); }
				if (node.op === parent.op &&
					!self.operatorProperties[node.op].associative) {
					return null;
				}
				utils.setChild(node, parent, side);

				var otherChild = parent.lhs;
				if (node === otherChild) {
					otherChild = parent.rhs;
					var newRhs = expression.createCompoundExpression(
						node.rhs, otherChild, parent.op);
					newParent = expression.createCompoundExpression(node.lhs, newRhs, node.op);
				} else {
					var newLhs = expression.createCompoundExpression(
						otherChild, node.lhs, parent.op);
					newParent = expression.createCompoundExpression(newLhs, node.rhs, node.op);
				}
				utils.setChild(newParent, grandparent, parentSide);
			}
			return newParent;
		},
		commute : function(node) {
			var parent = node.parent;
			var side = utils.getSide(node);
			node = self.makeCommutativeIfNecessary(node);
			if (node.type === 'compound' && self.operatorProperties[node.op].commutative) {
				var newNode = expression.createCompoundExpression(node.rhs, node.lhs, node.op);
				utils.setChild(newNode, parent, side);
				return newNode;
			}
			return null;
		},
		distributeLeft : function(node) {
			var newNode = null;
			if (node.type === 'compound' && node.op === '*') {
				var parent = node.parent;
				var side = utils.getSide(node);
				if (node.rhs.type === 'compound' &&
					self.getPrecedence(node.rhs.op) === 1) {
					newNode = expression.createCompoundExpression(
						expression.createCompoundExpression(node.lhs, node.rhs.lhs, node.op),
						expression.createCompoundExpression(node.lhs, node.rhs.rhs, node.op),
						node.rhs.op);
				}
				utils.setChild(newNode, parent, side);
			}
			return newNode;
		},
		distributeRight : function(node) {
			var newNode = null;
			if (node.type === 'compound' && node.op === '*' || node.op === '/') {
				var parent = node.parent;
				var side = utils.getSide(node);
				if (node.lhs.type === 'compound' &&
					self.getPrecedence(node.lhs.op) === 1) {
					newNode = expression.createCompoundExpression(
						expression.createCompoundExpression(node.lhs.lhs, node.rhs, node.op),
						expression.createCompoundExpression(node.lhs.rhs, node.rhs, node.op),
						node.lhs.op);
				}
				utils.setChild(newNode, parent, side);
			}
			return newNode;
		},
		// TODO(sdspikes): add tests
		factorLeft : function(node) {
			var newNode = null;
			if (node.type === 'compound' && self.getPrecedence(node.op) === 1) {
				var newLhs = self.makeCommutativeIfNecessary(node.lhs);
				var newRhs = self.makeCommutativeIfNecessary(node.rhs);
				if (node.lhs.type === 'compound' && self.getPrecedence(newLhs.op) === 2 &&
					newRhs.type === 'compound' && newRhs.op === newLhs.op &&
					newLhs.lhs.syntacticEquals(newRhs.lhs)) {
					var parent = node.parent;
					var side = utils.getSide(node);
					newNode = expression.createCompoundExpression(newLhs.lhs,
						expression.createCompoundExpression(newLhs.rhs, newRhs.rhs, node.op),
						newLhs.op);
					utils.setChild(newNode, parent, side);
				}
			}
			return newNode;
		},
		factorRight : function(node) {
			var newNode = null;
			if (node.type === 'compound' && self.getPrecedence(node.op) === 1 &&
				node.lhs.type === 'compound' && self.getPrecedence(node.lhs.op) === 2 &&
				node.rhs.type === 'compound' && node.rhs.op === node.lhs.op &&
				node.lhs.rhs.syntacticEquals(node.rhs.rhs)) {
				var parent = node.parent;
				var side = utils.getSide(node);
				newNode = expression.createCompoundExpression(
					expression.createCompoundExpression(node.lhs.lhs, node.rhs.lhs, node.op),
					node.lhs.rhs,
					node.lhs.op);
				utils.setChild(newNode, parent, side);
			}
			return newNode;
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
			expression.createSimpleExpression('number', -1), child);
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