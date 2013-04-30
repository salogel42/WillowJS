/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var fractionUtils = require('./fractionUtils.js').fractionUtils;
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
	var operatorProperties = require('./operatorProperties.js').operatorProperties;
	var getPrecedence = operatorProperties.getPrecedence;
	var expression = require('./expression.js').expression;
}

/*  Approximate CFG used by parser
	E -> T
	E -> E op E
	T -> number
	T -> identifier
	T -> (E)
	T -> -T
	T -> \frac{E}{E}
	T -> \left(E\right)

	op -> +|-|*|/|^
	number -> \d+\.?\d* |^\.\d+(float)
	identifier -> [a-zA-Z] (single char)

	// maybe add
	T -> \function{E}
	function -> [a-zA-Z]+
	T -> \left{E\right}
	T -> \left[E\right]
*/
var parser = (function() {
	/**
	 * Regular expression for detecting valid operators in this grammar.
	 * @type {RegExp}
	 */
	var OPERATOR = new RegExp(/^(\+|\-|\*|\/|\^|\\pm)/);
	var LATEX_COMMAND = new RegExp(/^\\[a-zA-Z]*/);
	var EQUALITY_OPERATOR = new RegExp(/<=|=|>=|!=|<|>/g);

	/**
	 * Regular expression for detecting valid floats.
	 * @type {RegExp}
	 */
	var FLOAT = new RegExp(/^\d+\.?\d*|^\.\d+/);

	/**
	 * Regular expression for detecting valid identifiers.
	 * @type {RegExp}
	 */
	var IDENTIFIER = new RegExp(/^[a-zA-Z]$/);

	/**
	 * Regular expression for determining whether or not we're looking at implicit multiplication.
	 * @type {RegExp}
	 */
	var IMPLICIT = new RegExp(/^(\w|\(|\\)/);

	/**
	 * Regular expression for determining if we're looking at a LaTeX fraction.
	 * @type {RegExp}
	 */
	var FRACTION = new RegExp(/^\\frac/);

	/**
	 * Regular expression for determining if we're looking at a LaTeX infinity symbol.
	 * @type {RegExp}
	 */
	var INFINITY = new RegExp(/^\\infty/);

	/**
	 * Regular expression for determining if we're looking at a LaTeX square root symbol.
	 * @type {RegExp}
	 */
	var SQUARE_ROOT = new RegExp(/^\\sqrt/);

	/**
	 * Regular expressions for determining if a string starts with a LaTeX open paren/close paren.
	 * @type {RegExp}
	 */
	var LEFT = new RegExp(/^\\left/);
	var PAREN_OPEN = new RegExp(/^\(/);
	var ABS_OPEN = new RegExp(/^\|/);

	var LEFT_PAREN = new RegExp(/^\\left\(/);
	var RIGHT_PAREN = new RegExp(/^\\right\)/);

	var LEFT_ABS = new RegExp(/^\\left\|/);
	var RIGHT_ABS = new RegExp(/^\\right\|/);

	/**
	 * Regular expressions for finding whitespace or MathQuill-generated spaces, which look like \:
	 * @type {RegExp}
	 */
	var STARTING_SPACE = new RegExp(/^(\\:|\s)+/);

	/**
	 * Regular expressions for finding LaTeX multiplication symbols, and less/greater than or
	 * equal, or not equal.
	 * @type {RegExp}
	 */
	var LATEX_MULT = new RegExp(/(\\cdot|\\times)/g);
	var LATEX_DIV = new RegExp(/(\\div)/g);
	var LATEX_LESS_OR_EQUAL = new RegExp(/(\\le)([^f])/g);
	var LATEX_GREATER_OR_EQUAL = new RegExp(/\\ge/g);
	var LATEX_NOT_EQUAL = new RegExp(/\\ne/g);

	var LOG = new RegExp(/\\log/);
	var LN = new RegExp(/\\ln/);

	var DEFAULT_LOG_BASE = 10;

	function getMatchingGroupingSymbol(open) {
		switch (open) {
			case '{': return '}';
			case '[': return ']';
			case '(': return ')';
			default: return '';
		}
	}
	function getLeftAndRightRegexes(open) {
		if (ABS_OPEN.test(open)) {
			return { left : LEFT_ABS, right : RIGHT_ABS };
		} else if (PAREN_OPEN.test(open)) {
			return { left : LEFT_PAREN, right : RIGHT_PAREN };
		}
	}

	function findClosingBrace(expressionString, open) {
		var close = getMatchingGroupingSymbol(open);
		var parity = 0;
		for (var i = 0; i < expressionString.length; i++) {
			if (expressionString.charAt(i) === close) { parity--; }
			if (expressionString.charAt(i) === open) { parity++; }
			if (parity === 0) { return i; }
			if (parity < 0) { break; }
		}
		console.error('Mismatched parentheses!');
		return -1;
	}

	function findMatchingLaTeXParen(expressionString, open) {
		var regexes = getLeftAndRightRegexes(expressionString);
		var parity = 1;
		for (var i = 1; i < expressionString.length; i++) {
			var expI = expressionString.substring(i);
			if (regexes.right.test(expI)) { parity--; }
			if (regexes.left.test(expI)) { parity++; }
			if (parity === 0) { return i; }
			if (parity < 0) { break; }
		}
		console.error('Mismatched absolute value!');
		return -1;
	}

	function makeFractionOrCompoundExp(top, bottom, op) {
		if (op === '/' && top.type === 'number' && bottom.type === 'number' &&
			fractionUtils.isInteger(top.value) && fractionUtils.isInteger(bottom.value)) {
			if (bottom.value === 0) {
				console.error('Divided by 0');
				return errorNode;
			}
			return expression.createSimpleExpression('number',
				fractionUtils.createFractionValue(top.value, bottom.value));
		}
		return expression.createCompoundExpression(top, bottom, op);
	}

	function getSubExpressionAtSubstring(expressionString, endLocation, start, end) {
		if (typeof start === 'undefined') { start = 1; }
		if (typeof end === 'undefined') { end = 1; }
		if (endLocation === -1) { return null; }
		var parse = parseExpression(expressionString.substring(start, endLocation), 0);
		if (parse === errorNode || parse.expression === errorNode) { return errorNode; }
		return {
			'expression' : parse.expression,
			'expressionString' : expressionString.substring(endLocation + end)
		};
	}

	function getExpressionWithinBrace(expressionString, open) {
		if (expressionString.charAt(0) !== open) {
			console.error('Badly formed LaTeX input');
			return errorNode;
		}
		var closingLocation = findClosingBrace(expressionString, open);
		return getSubExpressionAtSubstring(expressionString, closingLocation);
	}

	function makeSqrtIntoNumber(leftValue, rightValue) {
		if (typeof rightValue === 'undefined') {
			rightValue = fractionUtils.createFractionValue(1, 2);
		}
		return expression.createSimpleExpression('number',
			fractionUtils.createRadicalValue(leftValue, rightValue, false));
	}

	// Get the next subexpression.  Could be number, identifier, or (parenthesized expression), or
	// any of the above preceded by a '-'
	function parseSubExpression(expressionString) {
		var result = {
			'expression' : null,
			'expressionString' : expressionString
		};
		var closingLocation = -1;
		// First check for unary operator
		var op = getNextOp(expressionString);
		if (op !== null) {
			expressionString = expressionString.substring(op.length);
			// unary - has same precedence as * and / since it is the same as -1*
			var precedence = (/\d/.test(expressionString.charAt(0))) ? 2 : 1;
			var exp = parseExpression(expressionString, precedence);
			if (exp.expression === errorNode) { return errorNode; }
			result.expressionString = exp.expressionString;
			result.expression = expression.createUnaryExpression(exp.expression, op);
			if (op === '-' && exp.expression.type === 'number') {
				if (!fractionUtils.isNegative(exp.expression.value) &&
					(typeof exp.expression.value === 'number')) {
					result.expression = expression.createSimpleExpression(
						'number', -1 * exp.expression.value);
				}
			}
		} else if (expressionString.charAt(0) === 'i') {
			result.expression = makeSqrtIntoNumber(-1);
			result.expressionString = expressionString.substring(1);
		} else if (expressionString.charAt(0) === '(' || expressionString.charAt(0) === '{') {
			closingLocation = findClosingBrace(expressionString, expressionString.charAt(0));
			result = getSubExpressionAtSubstring(expressionString, closingLocation);
		} else if (expressionString.charAt(0) === '\\') { // it is probably LaTeX
			var latex = expressionString.match(/^\\[a-z]+/i);
			if (latex === null || (latex = latex[0]).length === 0) {
				console.error('Invalid syntax at: ' + expressionString);
				return errorNode;
			}
			expressionString = expressionString.substring(latex.length);

			if (INFINITY.test(latex)) {
				result.expressionString = expressionString;
				result.expression = expression.createSimpleExpression('identifier', '\\infty');
			} else if (SQUARE_ROOT.test(latex)) {
				var open = expressionString.charAt(0);
				var innerExpression = getExpressionWithinBrace(expressionString, open);
				if (open === '[') {
					var nthroot = innerExpression;
					innerExpression = getExpressionWithinBrace(
						innerExpression.expressionString, '{');
					result.expression = expression.createCompoundExpression(nthroot.expression,
						innerExpression.expression, '\\sqrt');
					if (result.expression.lhs.type === 'number' &&
						result.expression.rhs.type === 'number') {
						result.expression = makeSqrtIntoNumber(result.expression.rhs.value,
							fractionUtils.invertFraction(result.expression.lhs.value));
					}
				} else {
					if (innerExpression.expression.type === 'number') {
						result.expression = makeSqrtIntoNumber(innerExpression.expression.value);
					} else {
						result.expression = expression.createUnaryExpression(
							innerExpression.expression, '\\sqrt');
					}
				}
				result.expressionString = innerExpression.expressionString;
			} else if (FRACTION.test(latex)) {
				var topExpression = getExpressionWithinBrace(expressionString, '{');
				var bottomExpression = getExpressionWithinBrace(
					topExpression.expressionString, '{');
				result.expression = makeFractionOrCompoundExp(topExpression.expression,
					bottomExpression.expression, '/');
				if (result.expression === errorNode) { return errorNode; }
				result.expressionString = bottomExpression.expressionString;
			} else if (LOG.test(latex) || LN.test(latex)) {
				var base = expression.createSimpleExpression('number', DEFAULT_LOG_BASE);
				var currentOp = '\\log';
				if (LN.test(latex)) { base = expression.createSimpleExpression('identifier', 'e'); }
				expressionString = expressionString.replace(STARTING_SPACE, '');
				if (expressionString.charAt(0) === '_') {
					if (expressionString.charAt(1) === '{') {
						parsedBase = getExpressionWithinBrace(expressionString, '{');
						expressionString = parsedBase.expressionString;
					} else {
						parsedBase = parseSubExpression(expressionString.substring(1, 2));
						expressionString = expressionString.substring(2);
					}
					base = parsedBase.expression;
				}
				var logerand = parseExpression(expressionString,
					operatorProperties[currentOp].precedence);
				result.expression = expression.createCompoundExpression(base,
					logerand.expression, currentOp);
				if (result.expression === errorNode) { return errorNode; }
				result.expressionString = logerand.expressionString;
			} else if (PAREN_OPEN.test(expressionString) || ABS_OPEN.test(expressionString)) {
				var abs = ABS_OPEN.test(expressionString);
				var close = findMatchingLaTeXParen(expressionString);
				result = getSubExpressionAtSubstring(expressionString, close, 1, '\\right)'.length);
				if (abs && result.expression !== errorNode) {
					result.expression = expression.createUnaryExpression(result.expression, '|');
				}
			} else {
				result.expression = expression.createSimpleExpression('identifier', latex);
				result.expressionString = expressionString;
			}
		} else {
			// Now, should be an identifier or number. Grab it and
			// expression.create appropriate simple expression.
			var type = 'identifier';
			var value = expressionString.charAt(0);

			// Figure out if it is a number or an identifier.
			var match = expressionString.match(FLOAT);
			if (match !== null) {
				type = 'number';
				value = parseFloat(match);
				if (isNaN(value)) {
					console.error('Badly formed number: ' + match);
					return errorNode;
				}
				result.expressionString = expressionString.substring(match[0].length);
			} else {
				// TODO(sdspikes): add support for functions?
				result.expressionString = expressionString.substring(1);
				if (!IDENTIFIER.test(value)) {
					console.error('Badly formed identifier: ' + value);
					return errorNode;
				}
			}
			result.expression = expression.createSimpleExpression(type, value);
		}
		// If the T is followed by a starting char for another T, assume explicit multiplication.
		if (result === null || result === errorNode || result.expression === errorNode) {
			return errorNode;
		}
		if (result.expression === errorNode) { return errorNode; }
		result.expressionString = result.expressionString.replace(STARTING_SPACE,'');
		if (IMPLICIT.test(result.expressionString) && !OPERATOR.test(result.expressionString)) {
			result.expressionString = '*' + result.expressionString;
		}
		return result;
	}
	function getNextOp(expressionString) {
		if (expressionString.length !== 0) {
			var op = expressionString.charAt(0);
			var match = expressionString.match(LATEX_COMMAND);
			if (match !== null) { op = match[0]; }
			if (OPERATOR.test(op)) { return op; }
		}
		return null;
	}
	function parseExpression(expressionString, prec) {
		expressionString = expressionString.replace(STARTING_SPACE,'');
		// Get the first subexpression
		var exp = parseSubExpression(expressionString);

		var op;
		while (exp && exp !== errorNode &&
			(op = getNextOp(exp.expressionString)) !== null && getPrecedence(op) > prec) {
			// Now that we have the first expression, and know there are more, get the next one.
			var rhs = parseExpression(exp.expressionString.substring(op.length), getPrecedence(op));
			if (rhs === errorNode) { return errorNode; }
			// Since it was a valid expression, fold it in to a compound expression with all the
			// ones we have found so far at this level.
			exp.expression = makeFractionOrCompoundExp(exp.expression, rhs.expression, op);
			exp.expressionString = rhs.expressionString;
		}
		return exp;
	}
	// Call the actual parser and unpack the result

	var self = {
		// Public
		/**
		 * First two public only for testing.
		 */
		parseExpressionWrapper: function(expressionString) {
			expressionString = expressionString.replace(LATEX_MULT,'*');
			expressionString = expressionString.replace(LATEX_DIV,'/');
			// TODO(sdspikes) : disallow equals/inequality here?
		//			if (/(=|<|>|)/.)
			var exp = parseExpression(expressionString, 0);
			if (exp === errorNode) { return errorNode; }
			if (exp.expressionString !== '') {
				console.error('Expression contained extra characters: ' + exp.expressionString);
				return errorNode;
			}
			return exp.expression;
		},
		parseEquation: function(equationString) {
			var equalityOperator = equationString.match(EQUALITY_OPERATOR);
			if (equalityOperator === null ||
				!(equalityOperator.length === 1 || equalityOperator.length === 2)) {
				console.error('Please enter an equation, not an expression ' +
					'(should contain an "=" or inequality).');
				return errorNode;
			}
			// If it's a double inequality, make sure they point the same way.
			if (equalityOperator.length === 2) {
				if (equalityOperator[0] === '=' || equalityOperator[1] === '=') {
					console.error('Equation is not valid.');
					return errorNode;
				}
				if (operatorProperties.getDirection(equalityOperator[0]) !==
					operatorProperties.getDirection(equalityOperator[1])) {
					console.error('Both inequalities must face the same direction.');
					return errorNode;
				}
			}
			var equalsLocation = equationString.indexOf(equalityOperator[0]);
			var leftString = equationString.substring(0, equalsLocation);
			equationString = equationString.substring(equalsLocation + equalityOperator[0].length);
			var end = (equalityOperator.length === 2) ?
				equationString.indexOf(equalityOperator[1]) : equationString.length;

			var left = self.parseExpressionWrapper(leftString);
			var right = self.parseExpressionWrapper(equationString.substring(0, end));

			if (equalityOperator.length === 1) {
				return expression.createCompoundExpression(left, right, equalityOperator[0]);
			}
			var middle = right;
			right = self.parseExpressionWrapper(
				equationString.substring(end + equalityOperator[1].length));
			return expression.createTernaryExpression(
				left, middle, right, equalityOperator[0], equalityOperator[1]);
		},
		parseEquationOrExpression: function(equationString) {
			if (typeof equationString === 'undefined' || equationString === null ||
				equationString.length === 0) {
				return errorNode;
			}
			equationString = equationString.replace(LATEX_LESS_OR_EQUAL, '<=$2');
			equationString = equationString.replace(LATEX_GREATER_OR_EQUAL,'>=');
			equationString = equationString.replace(LATEX_NOT_EQUAL,'!=');
			// if we have ^ with no {} or (), take the one char after and put it in {}
			// to conform to LaTeX standards.
			equationString = equationString.replace(/(\^)([^\{\(])/g, '^{$2}');
			var equalityOperator = equationString.match(EQUALITY_OPERATOR);
			if (equalityOperator === null) {
				return self.parseExpressionWrapper(equationString);
			}
			else { return self.parseEquation(equationString); }
		}
	};
	return self;
}());

if (typeof exports !== 'undefined') { exports.parser = parser; }