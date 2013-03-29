/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var fractionUtils = require('./fractionUtils').fractionUtils;
	var expression = require('./expression.js').expression;
	var operatorProperties = require('./operatorProperties.js').operatorProperties;
	var getPrecedence = operatorProperties.getPrecedence;
	var operator = require('./operator.js').operator;
}

var expressionUtils = (function() {

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
				computed = operator[node.op].unaryEvaluate(child);
			}
			if (node.type === 'compound') {
				var lhs = self.computeWithApproxNumericValue(node.lhs);
				var rhs = self.computeWithApproxNumericValue(node.rhs);
				if (getPrecedence(node.op) === 0) {
					return expression.createCompoundExpression(lhs, rhs, node.op);
				}
				computed = operator[node.op].evaluate(lhs, rhs);
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
		makeIntoFractionNodeIfApplicable: function(node) {
			if (!utils.isNumericFraction(node)) { return node; }
			var frac = fractionUtils.simplifyFraction(
					fractionUtils.createFractionValue(node.lhs.value, node.rhs.value));
			if (frac === errorNode) { return errorNode; }
			return expression.createSimpleExpression('number', frac);
		},

		// The following functions are currently not used.  They were designed to be used for the
		// interactive, one-step-at-a-time solver, which is more useful for either kids who are
		// just learning arithmetic, or for number theory proofs.  I don't want to kill them, but
		// I also don't have a real use for them currently.

		// TODO(sdspikes): add tests
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
				if (getPrecedence(node.op) !== getPrecedence(parent.op) ||
					(side === 'right' && node.op !== parent.op)) { return null; }
				var grandparent = parent.parent;
				var parentSide = utils.getSide(parent);
				if (side === 'left') { node = self.makeCommutativeIfNecessary(node); }
				if (node.op === parent.op &&
					!operatorProperties[node.op].associative) {
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
			if (node.type === 'compound' && operatorProperties[node.op].commutative) {
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
				if (node.rhs.type === 'compound' && getPrecedence(node.rhs.op) === 1) {
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
				if (node.lhs.type === 'compound' && getPrecedence(node.lhs.op) === 1) {
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
			if (node.type === 'compound' && getPrecedence(node.op) === 1) {
				var newLhs = self.makeCommutativeIfNecessary(node.lhs);
				var newRhs = self.makeCommutativeIfNecessary(node.rhs);
				if (node.lhs.type === 'compound' && getPrecedence(newLhs.op) === 2 &&
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
			if (node.type === 'compound' && getPrecedence(node.op) === 1 &&
				node.lhs.type === 'compound' && getPrecedence(node.lhs.op) === 2 &&
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
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.expr = expressionUtils;
}