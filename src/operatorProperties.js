/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
}

var operatorProperties = (function() {
	/**
	 * Object that stores all the relevant properties of an operator.
	 * @constructor
	 * @param identity    {number}   The value for which num op identity == num.
	 * @param precedence  {number}   The operator's precedence with respect to other operators
	 *                               (they go in pairs, so 2 and 3 are at the same level).
	 * @param commutative {boolean}  True if a op b == b op a.
	 * @param associative {boolean}  True if (a op b) op c == a op (b op c).
	 */
	function OperatorProperties(precedence, commutative, associative, identity) {
		this.precedence = precedence;
		this.commutative = commutative;
		this.associative = associative;
		if (typeof identity !== 'undefined') {
			this.identity = identity;
		}
	}
	/**
	 * Define all of the operators with all their individual properties.
	 * @dict
	 */
	self = {
		'^' : new OperatorProperties(3, false, false, 1),
		'/' : new OperatorProperties(2, false, false, 1),
		'*' : new OperatorProperties(2, true, true, 1),
		'-' : new OperatorProperties(1, false, false, 0),
		'+' : new OperatorProperties(1, true, true, 0),
		'\\pm' : new OperatorProperties(1, false, false, 0),
		'\\sqrt' : new OperatorProperties(-1, false, false),
		'\\log' : new OperatorProperties(1.5, false, false),
		'|' : new OperatorProperties(-1, false, false),
		'=' : new OperatorProperties(0, true, false),
		'!=' : new OperatorProperties(0, true, false),
		'<' : new OperatorProperties(0, false, false),
		'>' : new OperatorProperties(0, false, false),
		'<=' : new OperatorProperties(0, false, false),
		'>=' : new OperatorProperties(0, false, false),
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
		},
		/**
		 * Convenience function to make things more readable.
		 * @param {string} op The string representing the operator, to look up in the dict above.
		 * @return {number} The operator's precedence.
		 */
		getPrecedence: function(op) {
			op = utils.getEquivalentOp(op);
			return operatorProperties[op].precedence;
		}
	};
	
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.operatorProperties = operatorProperties;
}