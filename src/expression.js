/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var fractionUtils = require('./fractionUtils').fractionUtils;
}
var expression = (function() {
	/**
	 * Object that stores the properties common to all expression nodes.
	 * @constructor
	 * @param id      {number}     Unique identifier for this expression node.
	 * @param type    {string}     The kind of node (number, identifier, unary, compound, ternary).
	 * @param parent  {Expression} The node that has this one as a child.  Null if root node.
	 * @param numeric {Boolean}    True if type is number or if all child nodes are numeric nodes.
	 */
	function Expression(id, type, numeric) {
		this.id = id;
		this.type = type;
		this.parent = null;
		this.numeric = numeric;
		this.simplified = (type === 'number');
	}
	var self = {
		nextId: 0,
		nodeMap: (typeof window === 'undefined') ? null : {},
		syntacticEquals : function(node, other) {
			if (node.type !== other.type) {
				return false;
			} else if (utils.isSimpleExpression(node)) {
				return fractionUtils.compareFractions(node.value, other.value, false);
			} else if (node.type === 'ternary') {
				return (node.op1 === other.op2) && (node.op2 === other.op2) &&
					node.left.syntacticEquals(other.left) &&
					node.middle.syntacticEquals(other.middle) &&
					node.right.syntacticEquals(other.right);
			} else if (node.op !== other.op) {
				return false;
			} else if (node.type === 'unary') {
				return node.child.syntacticEquals(other.child);
			} else if (node.type === 'compound') {
				return node.lhs.syntacticEquals(other.lhs) &&
					node.rhs.syntacticEquals(other.rhs);
			}
			return false;
		},
		createExpression: function(type, numeric) {
			var id = self.nextId++;
			var expression = new Expression(id, type, numeric);
			expression.syntacticEquals = function(node) { return self.syntacticEquals(this, node); };
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
			op = utils.getEquivalentOp(op);
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
			op1 = utils.getEquivalentOp(op1);
			expression.op1 = op1;
			op2 = utils.getEquivalentOp(op2);
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
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.expression = expression;
}