/*global require:true exports:true */

var errorNode = { type:'error', display: function() { return 'error'; } };

var utils = (function() {
	var nextId = 0;

	// TODO(sdspikes) : actually use this or a working version of it
	function doCommandInContext(node, command) {
		var parent = node.parent;
		var side = utils.getSide(node);
		node = command(node);
		if (node !== null) { utils.setChild(node, parent, side); }
		return node;
	}

	var self = {
		isUnaryNegative: function(node) {
			return node.type === 'unary' && node.op === '-';
		},
		isAbsValue: function(node) {
			return node.type === 'unary' && node.op === '|';
		},
		setChild: function(node, parent, side) {
			node.parent = parent;
			if (parent === null) { return; }
			if (parent.type === 'unary') { parent.child = node; }
			else if (side === 'left') { parent.lhs = node; }
			else { parent.rhs = node; }
		},
		isOneOverSomething: function(node) {
			return (node.type === 'compound' && node.op === '/' &&
				node.lhs.type === 'number' && node.lhs.value === 1);
		},
		isNumericFraction: function(node) {
			return (node.type === 'compound' && node.op === '/' &&
				node.lhs.type === 'number' && node.rhs.type === 'number');
		},
		isNegativeOne: function(node) {
			return (node.type === 'number' && node.value === -1) ||
				(self.isUnaryNegative(node) && node.child.value === 1);
		},
		isSimpleExpression: function(node) {
			return (node.type === 'identifier' || node.type === 'number');
		},
		// returns left if node has no parent or is left child, right otherwise
		getSide: function(node) {
			return (node.parent === null || node === errorNode || node.parent.lhs === node) ?
				'left' :
				'right';
		}
	};
	
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.utils = utils;
	exports.errorNode = errorNode;
}