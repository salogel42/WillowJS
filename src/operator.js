/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var fractionUtils = require('./fractionUtils').fractionUtils;
	var expression = require('./expression.js').expression;
	var getPrecedence = require('./operatorProperties.js').operatorProperties.getPrecedence;
}

var operator = (function() {
	/**
	 * Object that knows how to evaluate all the evaluable operators.
	 * @constructor
	 * @param evaluate       {Function} Defines how to perform the operation given two Expression
	 *                                  operands.
	 * @param evaluateValues {Function} Defines how to perform the operation given two numbers as
	 *                                  operands.
	 */
	function Operator(evaluate, evaluateValues) {
		this.evaluate = evaluate;
		this.evaluateValues = evaluateValues;
	}

	function makeNumber(value) {
		return expression.createSimpleExpression('number', value);
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
						newNode = operator['-'].unaryEvaluate(newNode);
					}
					return newNode;
				}
			}
			return expression.createCompoundExpression(left, right, '*');
		}
		return node;
	}
	/**
	 * Define all of the operators with all their individual properties.
	 * @dict
	 */
	self = {
		'^' : new Operator(
			function(lhs, rhs) {
				if (lhs.type !== 'number' || rhs.type !== 'number' ||
					fractionUtils.isValueRadical(rhs.value)) {
					return expression.createCompoundExpression(lhs, rhs, '^');
				}
				if (lhs.value === 1) { return lhs; }
				return operator['^'].evaluateValues(lhs.value, rhs.value);
			}, function(lhsValue, rhsValue) {
				if (fractionUtils.isValueFraction(rhsValue)) {
					var result = self.simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
						lhsValue, rhsValue, false));
					return result;
				}
				var resultValue = 1;
				if (fractionUtils.isValueFraction(lhsValue)) {
					return operator['/'].evaluate(
						operator['^'].evaluateValues(
							lhsValue.top, rhsValue),
						operator['^'].evaluateValues(
							lhsValue.bottom, rhsValue));
				} else if (fractionUtils.isValueRadical(lhsValue)) {
					return self.simplifyRadicalIntoNode(fractionUtils.createRadicalValue(
						lhsValue.radicand, operator['*'].evaluateValues(
							lhsValue.power, rhsValue).value, false));
				} else if (fractionUtils.isNegative(rhsValue)) {
					resultValue = fractionUtils.createFractionValue(
						1, Math.pow(lhsValue, -1*rhsValue));
				} else {
					resultValue = Math.pow(lhsValue, rhsValue);
				}
				return expression.createSimpleExpression('number', resultValue);
			}
		),
		'/' : new Operator(
			function(lhs, rhs) {
				if (!(lhs.numeric && rhs.numeric) && (!lhs.simplified || !rhs.simplified)) {
					return expression.createCompoundExpression(lhs, rhs, '/');
				}
				if (lhs.type !== 'number' || rhs.type !== 'number') {
					var result = expression.createCompoundExpression(lhs, rhs, '/');
					result.simplified = true;
					return result;
				}
				return operator['/'].evaluateValues(lhs.value, rhs.value);
			}, function(lhsValue, rhsValue) {
				var resultValue = null;
				if (fractionUtils.isValueRadical(rhsValue)) {
					return operator['*'].evaluate(
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
					return operator['*'].evaluateValues(
						lhsValue, fractionUtils.invertFraction(rhsValue));
				} else if (fractionUtils.isValueFraction(lhsValue)) {
					return operator['/'].evaluate(
						makeNumber(lhsValue.top),
						operator['*'].evaluateValues(
							lhsValue.bottom, rhsValue));
				} else {
					resultValue = fractionUtils.simplifyFraction(
						fractionUtils.createFractionValue(lhsValue, rhsValue));
				}
				return expression.createSimpleExpression('number', resultValue);
			}
		),
		'*' : new Operator(
			function(lhs, rhs) {
				var node = expression.createCompoundExpression(lhs, rhs, '*');
				node = pullOutRadical(node);
				if (node.radical !== null) {
					var orig = node.radical;
					var combined = combineRadicals(node.radical);
					node = multiplyNodesMaybeNull(node.rest, combined);
					if (orig.syntacticEquals(combined) || node.type !== 'compound') {
						return node;
					}
					lhs = node.lhs;
					rhs = node.rhs;
				}
				if (!lhs.numeric || !rhs.numeric) {
					return expression.createCompoundExpression(lhs, rhs, '*');
				}
				if (lhs.type === 'compound') {
					if (getPrecedence(lhs.op) === 2) {
						// (2/x)*3 = (2*3)/x
						if (lhs.lhs.type === 'number' &&
							!fractionUtils.isValueRadical(lhs.lhs.value)) {
							return operator[lhs.op].evaluate(
								operator['*'].evaluate(lhs.lhs, rhs),
								lhs.rhs);
						}
						// (x/2)*3 = x*(3/2)
						if (lhs.rhs.type === 'number' &&
							!fractionUtils.isValueRadical(lhs.rhs.value)) {
							return operator['*'].evaluate(lhs.lhs,
								operator[lhs.op].evaluate(rhs, lhs.rhs));
						}
					}
				}
				if (rhs.type === 'compound') {
					if (getPrecedence(rhs.op) === 2) {
						// 2*(3/x) = (2*3)/x
						if (rhs.lhs.type === 'number' &&
							!fractionUtils.isValueRadical(rhs.lhs.value)) {
							return operator[rhs.op].evaluate(
								operator['*'].evaluate(lhs, rhs.lhs),
								rhs.rhs);
						}
						// 2*(x/3) = (2/3)*x
						if (rhs.rhs.type === 'number' &&
							!fractionUtils.isValueRadical(rhs.rhs.value)) {
							return operator['*'].evaluate(
								operator[rhs.op].evaluate(lhs, rhs.rhs),
								rhs.lhs);
						}
					}
				}
				if (lhs.type !== 'number' || rhs.type !== 'number') {
					return expression.createCompoundExpression(lhs, rhs, '*');
				}
				return operator['*'].evaluateValues(lhs.value, rhs.value);
			}, function(lhsValue, rhsValue) {
				var resultValue = null;
				if (fractionUtils.isValueRadical(lhsValue) && !lhsValue.simplified) {
					return operator['*'].evaluate(
						self.simplifyRadicalIntoNode(lhsValue),
						makeNumber(rhsValue));
				}
				if (fractionUtils.isValueRadical(rhsValue) && !rhsValue.simplified) {
					return operator['*'].evaluate(makeNumber(lhsValue),
						self.simplifyRadicalIntoNode(rhsValue));
				}
				if (fractionUtils.isValueRadical(lhsValue) ||
					fractionUtils.isValueRadical(rhsValue)) {
					return combineRadicals(expression.createCompoundExpression(
						makeNumber(lhsValue), makeNumber(rhsValue), '*'));
				}
				return expression.createSimpleExpression('number',
					fractionUtils.multiplyFractions(lhsValue, rhsValue));
			}
		),
		'-' : new Operator(
			function(lhs, rhs) {
				if (lhs.type !== 'number' || rhs.type !== 'number' ||
					(fractionUtils.isValueRadical(lhs.value) ||
						fractionUtils.isValueRadical(rhs.value))) {
					return expression.createCompoundExpression(lhs, rhs, '-');
				}
				return operator['-'].evaluateValues(lhs.value, rhs.value);
			}, function(lhsValue, rhsValue) {
				var resultValue = null;
				if (!fractionUtils.isValueFraction(lhsValue) &&
					!fractionUtils.isValueFraction(rhsValue)) {
					resultValue = lhsValue - rhsValue;
				} else {
					return operator['+'].evaluateValues(lhsValue,
						this.unaryEvaluateValue(rhsValue).value);
				}
				return expression.createSimpleExpression('number', resultValue);
			}
		),
		'+' : new Operator(
			function(lhs, rhs) {
				if (lhs.type !== 'number' || rhs.type !== 'number') {
					return expression.createCompoundExpression(lhs, rhs, '+');
				} else if (fractionUtils.isValueRadical(lhs.value) ||
						fractionUtils.isValueRadical(rhs.value)) {
					if (lhs.syntacticEquals(rhs)) {
						var newNode = expression.createCompoundExpression(
							makeNumber(2), lhs, '*');
						newNode.simplified = true;
						return newNode;
					}
					return expression.createCompoundExpression(lhs, rhs, '+');
				}
				return operator['+'].evaluateValues(lhs.value, rhs.value);
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
			}
		),
		'\\sqrt' : new Operator(
			function(lhs, rhs) {
				return operator['^'].evaluate(rhs,
					operator['/'].evaluateValues(1, lhs.value));
			}, function(lhsValue, rhsValue) {
				// Assumes lhs is the root, and rhs is the value under the sqrt.
				return operator['^'].evaluate(makeNumber(rhsValue),
					operator['/'].evaluateValues(1, lhsValue));
			}
		),
		'|' : new Operator(),
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
			rationalizingTerm.value.power = operator['-'].evaluateValues(1,
				rationalizingTerm.value.power).value;
			var bottom = makeNumber(rationalizingTerm.value.radicand);
			if (radicalOnBottom.rest !== null) {
				bottom = operator['*'].evaluate(radicalOnBottom.rest, bottom);
			}
			return operator['/'].evaluate(
				operator['*'].evaluate(node.lhs, rationalizingTerm),
				bottom);
		},
		simplifyRadicalIntoNode: function(value) {
			if (fractionUtils.isValueFraction(value.radicand)) {
				return operator['/'].evaluate(
					self.simplifyRadicalIntoNode(
						fractionUtils.createRadicalValue(value.radicand.top, value.power)),
					self.simplifyRadicalIntoNode(
						fractionUtils.createRadicalValue(value.radicand.bottom, value.power)));
			}
			var result = fractionUtils.simplifyRadical(value);
			if (!fractionUtils.isValueMixed(result)) { return makeNumber(result); }
			result = operator['*'].evaluateValues(result.rational, result.radical);
			result.simplified = true;
			return result;
		}
	};
	/**
	 * Add a unaryEvaluate function to '-' and '\\sqrt' (I don't do it in the constructor
	 * since only a few have such a function).
	 * @param {number} childValue The number to invert.
	 * @return {number} The inverted number.
	 */
	self['-'].unaryEvaluate = function(child) {
		return operator['*'].evaluate(
			expression.createSimpleExpression('number', -1), child);
	};

	self['-'].unaryEvaluateValue = function(childValue) {
		return operator['*'].evaluateValues(-1, childValue);
	};

	self['\\sqrt'].unaryEvaluate = function(child, expand) {
		if (typeof expand === 'undefined') { expand = false; }
		return operator['^'].evaluateValues(child.value,
			fractionUtils.createFractionValue(1, 2));
	};

	self['|'].unaryEvaluate = function(child) {
		return makeNumber(fractionUtils.abs(child.value));
	};
	
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.operator = operator;
}