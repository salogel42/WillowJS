/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js');
	var errorNode = utils.errorNode;
	utils = utils.utils;
	var operatorProperties = require('./operatorProperties.js').operatorProperties;
	var getPrecedence = operatorProperties.getPrecedence;
	var operator = require('./operator.js').operator;
	var expr = require('./expr.js').expr;
	var fractionUtils = require('./fractionUtils.js').fractionUtils;
	var expression = require('./expression.js').expression;
}

var equality = (function() {
	var coefficient = false;
	function getAllTermsAtLevel(node, op) {
		var processedNode = expr.makeCommutativeIfNecessary(node);
		if (utils.isSimpleExpression(processedNode) || processedNode.op !== op ||
			processedNode.type === 'unary') { return [processedNode]; }
		var processedLeft = processedNode.lhs;
		var processedRight = processedNode.rhs;
		return getAllTermsAtLevel(processedLeft, op).concat(getAllTermsAtLevel(processedRight, op));
	}
	function allTermsMatch(arr1, arr2, op) {
		if (arr1.length !== arr2.length) { return false; }
		for (var j = 0; j < arr1.length; j++) {
			var term = arr1[j];
			var origLength = arr2.length;
			var found = false;
			for (var i = 0; i < arr2.length; i++) {
				if (equivalentModuloCommutativityHelper(term, arr2[i])) {
					if (coefficient && op === '*' && term.type === 'number' &&
						(i !== 0  || j !== 0)) {
						return false;
					}
					arr2.splice(i, 1);
					found = true;
					break;
				}
			}
			if (arr2.length === origLength) { return false; }
		}
		return (arr2.length === 0);
	}
	function hasPlusNegative(node) {
		if (node.type === 'compound' && getPrecedence(node.op) === 1) {
			return ((node.type === 'compound' && node.op === '+' &&
				utils.isUnaryNegative(node.rhs)) ||
				hasPlusNegative(node.lhs) || hasPlusNegative(node.rhs));
		}
		if (node.type === 'unary') { return hasPlusNegative(node.child); }
		if (node.type === 'compound') {
			return hasPlusNegative(node.lhs) || hasPlusNegative(node.rhs);
		}
		if (node.type === 'ternary') {
			return hasPlusNegative(node.left) || hasPlusNegative(node.middle) ||
				hasPlusNegative(node.right);
		}
		return false;
	}

	function getLeftMostTerm(node) {
		if (node.type === 'compound' && getPrecedence(node.op) === 2) {
			return getLeftMostTerm(node.lhs);
		}
		return node;
	}

	function normalizeTermCoefficient(node) {
		node = pullBottomNumberIntoFractionCoeffecient(node);
		if (fractionUtils.isNegativeNumber(node)) {
			return expression.createUnaryExpression(expression.createSimpleExpression('number',
				fractionUtils.additiveInverseOfFraction(node.value)), '-');
		}
		if (node.type === 'unary') {
			return expression.createUnaryExpression(normalizeTermCoefficient(node.child), node.op);
		}
		if (node.type === 'ternary') {
			return expression.createTernaryExpression(normalizeTermCoefficient(node.left),
				normalizeTermCoefficient(node.middle), normalizeTermCoefficient(node.right),
				node.op1, node.op2);
		}
		if (node.type !== 'compound') { return node; }
		var processedNode = expression.createCompoundExpression(normalizeTermCoefficient(node.lhs),
				normalizeTermCoefficient(node.rhs), node.op);
		if (getPrecedence(node.op) !== 2) {
			return processedNode;
		}

		var left = getLeftMostTerm(processedNode);
		if (fractionUtils.isNodeNegative(left)) {
			node = expr.copyNode(processedNode);
			left = getLeftMostTerm(processedNode);
			var inverse = left.child;
			if (fractionUtils.isNegativeNumber(left)) {
				inverse = equality.additiveInverseOfNumber(left);
			}
			utils.setChild(inverse, left.parent, 'left');
			return expression.createUnaryExpression(processedNode, '-');
		}
		return processedNode;
	}

	function normalizeTermSignMaybeCoefficient(node) {
		var split = operator.pullOutRadical(node);
		if (split !== null) {
			if (split.rest !== null) {
				split.rest = expr.makeIntoFractionNodeIfApplicable(split.rest);
			}
			node = operator.multiplyNodesMaybeNull(split.rest, split.radical, '*');
		}
		if (coefficient) { return normalizeTermCoefficient(node); }
		return self.normalizeTermSign(node);
	}

	function equivalentModuloCommutativityHelper(node, other) {
		if (other === errorNode || node === errorNode) { return false; }
		if (other.syntacticEquals(node)) { return true; }
		if (other.type === node.type) {
			if (utils.isSimpleExpression(other)) {
				return fractionUtils.compareFractions(other.value, node.value, true);
			} else if (other.type === 'unary') {
				return other.op === node.op &&
					equivalentModuloCommutativityHelper(other.child, node.child);
			} else if (node.type === 'ternary') {
				var leftCompare = other.left;
				var rightCompare = other.right;
				if (operatorProperties.getInverseInequality(node.op1) === other.op1 &&
					operatorProperties.getInverseInequality(node.op2) === other.op2) {
					leftCompare = other.right;
					rightCompare = other.left;
				} else if (other.op1 !== node.op1 || other.op2 !== node.op2) { return false; }
				return equivalentModuloCommutativityHelper(node.left, leftCompare) &&
					equivalentModuloCommutativityHelper(node.middle, other.middle) &&
					equivalentModuloCommutativityHelper(node.right, rightCompare);
			}
			other = expr.makeCommutativeIfNecessary(other);
			node = expr.makeCommutativeIfNecessary(node);
			if (other.op !== node.op) {
				if (operatorProperties.getInverseInequality(node.op) === other.op) {
					other = expression.createCompoundExpression(other.rhs, other.lhs, node.op);
				} else { return false; }
			}
			if (!operatorProperties[other.op].commutative) {
				return equivalentModuloCommutativityHelper(node.lhs, other.lhs) &&
					equivalentModuloCommutativityHelper(node.rhs, other.rhs);
			}
			var otherTerms = getAllTermsAtLevel(other, other.op);
			var nodeTerms = getAllTermsAtLevel(node, node.op);
			return allTermsMatch(otherTerms, nodeTerms, node.op);
		}
		return false;
	}

	function pullBottomNumberIntoFractionCoeffecient(node) {
		if (node.type === 'compound' && node.op === '/' &&
			node.rhs.type === 'number' && node.lhs.type !== 'number') {
			var split = self.splitCoefficientFromRest(node.lhs, true);
			if (split !== null) {
				var topCoefficient = (split.co === null) ? 1 : split.co.value;
				node = expression.createSimpleExpression('number',
					fractionUtils.createFractionValue(topCoefficient, node.rhs.value));
				if (split.rest !== null) {
					node = expression.createCompoundExpression(node, split.rest, '*');
				}
			}
		}
		return node;
	}

	var self = {
		splitCoefficientFromRest: function(node, equality) {
			if (typeof equality === 'undefined') { equality = false; }
			if (node.type === 'number') {
				return { co: node, rest: null };
			}
			if (node.type === 'compound' && node.op === '*' && !utils.isOneOverSomething(node)) {
				if (node.lhs.type === 'number' && node.rhs.type === 'number') {
					return null;
				}
				if (node.lhs.type === 'number') {
					return { co: node.lhs, rest: node.rhs };
				}
				if (node.rhs.type === 'number') {
					return { co: node.rhs, rest: node.lhs };
				}
				var splitLeft = self.splitCoefficientFromRest(node.lhs, equality);
				var splitRight = self.splitCoefficientFromRest(node.rhs, equality);
				if (splitLeft === null || splitRight === null ||
					(splitLeft.co !== null && splitRight.co !== null) ||
					(splitLeft.co === null && splitRight.co === null)) {
					return null;
				}
				if (splitRight.co !== null) {
					var temp = splitRight;
					splitRight = splitLeft;
					splitLeft = temp;
				}
				var result = { co: splitLeft.co, rest: splitLeft.rest };
				if (splitRight.rest !== null) {
					if (splitLeft.rest === null) { result.rest = splitRight.rest; }
					else {
						result.rest = expression.createCompoundExpression(
							splitLeft.rest, splitRight.rest);
					}
				}
				return result;
			}
			if (node.type === 'compound' && getPrecedence(node.op) !== 2 && !equality) {
				return null;
			}
			return { co: null, rest: node };
		},
		// "Public"
		additiveInverseOfNumber: function(node) {
			if (node.type !== 'number') { return null; }
			return operator['-'].unaryEvaluate(node);
		},
		normalizeTermSign: function(node) {
			node = pullBottomNumberIntoFractionCoeffecient(node);
			if (node.type === 'identifier') { return expr.copyNode(node); }
			if (node.type === 'number') {
				if (fractionUtils.isNegativeNumber(node)) {
					return expression.createUnaryExpression(
						self.additiveInverseOfNumber(node), '-');
				}
				return expr.copyNode(node);
			}
			if (node.type === 'unary') {
				var normalizedChild = self.normalizeTermSign(node.child);
				if (utils.isUnaryNegative(normalizedChild)) { return normalizedChild.child; }
				return expression.createUnaryExpression(normalizedChild, node.op);
			}
			if (node.type === 'ternary') {
				return expression.createTernaryExpression(self.normalizeTermSign(node.left),
					self.normalizeTermSign(node.middle), self.normalizeTermSign(node.right),
					node.op1, node.op2);
			}
			if (node.type === 'compound') {
				var normalizedLhs = self.normalizeTermSign(node.lhs);
				var normalizedRhs = self.normalizeTermSign(node.rhs);
				if (getPrecedence(node.op) !== 2) {
					return expression.createCompoundExpression(
						normalizedLhs, normalizedRhs, node.op);
				}
				if (utils.isUnaryNegative(normalizedLhs) && utils.isUnaryNegative(normalizedRhs)) {
					return expression.createCompoundExpression(
						normalizedLhs.child, normalizedRhs.child, node.op);
				} else if (!utils.isUnaryNegative(normalizedLhs) &&
					!utils.isUnaryNegative(normalizedRhs)) {
					return expression.createCompoundExpression(
						normalizedLhs, normalizedRhs, node.op);
				}
				if (utils.isUnaryNegative(normalizedLhs)) { normalizedLhs = normalizedLhs.child; }
				if (utils.isUnaryNegative(normalizedRhs)) { normalizedRhs = normalizedRhs.child; }
				return expression.createUnaryExpression(expression.createCompoundExpression(
					normalizedLhs, normalizedRhs, node.op), '-');
			}
		},
		equivalentModuloCommutativity: function(node, other, requireCoefficientPlacement) {
			if (node === errorNode || other === errorNode) { return false; }
			coefficient = requireCoefficientPlacement;
			var processedOther = normalizeTermSignMaybeCoefficient(other);
			var processedNode = normalizeTermSignMaybeCoefficient(node);
			if (coefficient &&
				(hasPlusNegative(processedOther) || hasPlusNegative(processedNode))) {
				return false;
			}
			return equivalentModuloCommutativityHelper(processedNode, processedOther);
		}
	};
	return self;
}());

if (typeof exports !== 'undefined') { exports.equality = equality; }