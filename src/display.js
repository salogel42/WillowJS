/*global require:true exports:true */
/*global expr:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var fractionUtils = require('./fractionUtils.js').fractionUtils;
	var utils = require('./utils.js').utils;
	var getPrecedence = require('./operatorProperties.js').operatorProperties.getPrecedence;
	var errorNode = require('./utils.js').errorNode;
	var expression = require('./expression.js').expression;
}

var parenMode = {
	'full' : 0,
	'terms' : 1,
	'necessary' : 2
};

var outputType = {
	'text' : 0,
	'mathml' : 1,
	'latex' : 2
};

var divSign = {
	'never' : 0,
	'notNumeric' : 1,
	'always' : 2
};

var display = (function() {
	var mode = parenMode.terms;
	var output = outputType.mathml;
	var showDivSign = divSign.never;
	function isComplexExpression(node) {
		return (node.type === 'unary' || node.type === 'compound');
	}
	function leftInParenTerm(node) {
		var side = utils.getSide(node);
		if (node.parent === null || node.parent.op === '=') { return true; }
		if (addParens(node.parent, node)) { return true; }
		if (side !== 'left') { return false; }
		return leftInParenTerm(node.parent);
	}
	function addParens(parent, child) {
		if (parent === null || child === null) { return false; }
		if (child.type === 'unary' && child.op === '|') { return false; }
		if ((child.type === 'unary' || child.type === 'compound') && child.op === '\\sqrt') {
			return false;
		}
		if (parent.op === '/' && showDivSign === divSign.never && output !== outputType.text) {
			return false;
		}
		var childOp = '';
		if (fractionUtils.isNegativeNumber(child)) { childOp = '-'; }
		else if (!isComplexExpression(child)) { return false; }
		else { childOp = child.op; }
		if (getPrecedence(parent.op) === 0) { return false; }
		if (mode === parenMode.full) { return true; }
		if (utils.isUnaryPlusMinusOrNegative(parent)) { return getPrecedence(child.op) < 2; }
		var side = utils.getSide(child);
		// if it's a negative to a power, put parens around it
		if (childOp === '-'  && parent.op === '^' && side === 'left') { return true; }
		// if we're in an exponent and not straight text output, no parens, we'll display the
		// superscript however necessary separately later
		if (parent.op === '^' && side === 'right' && output !== outputType.text) { return false; }
		// TODO(sdspikes) : reason through this again and see if it actually makes sense.
		if ((child.type === 'unary' || child.type === 'number') && childOp === '-' &&
			(child.type === 'unary' && addParens(child, child.child) ||
				(side === 'left' && leftInParenTerm(parent)))) {
			return false;
		}
		var parentOpPrecedence = getPrecedence(parent.op);
		var childOpPrecedence = getPrecedence(childOp);
		if (mode === parenMode.necessary) {
			return parentOpPrecedence > childOpPrecedence ||
				(parentOpPrecedence === childOpPrecedence && side === 'right' &&
				(parent.op === '-' || parent.op === '^' ||
					(parent.op === '/' &&
						(showDivSign !== divSign.never || output === outputType.text))));
		}
		return !(((parent.op === '/' || childOp === '/') && showDivSign === divSign.never) &&
			output !== outputType.text) &&
			(parentOpPrecedence > childOpPrecedence || parentOpPrecedence === childOpPrecedence);
	}
	function addNodeMathML(contents, id, identifier) {
		if (output !== outputType.mathml) {
			return contents;
		}
		if (identifier) {
			if (contents === '\\infty') { contents = '&#x221E;'; }
			return '<mi id=node' + id + '>' + contents + '</mi>';
		}
		return '<mn id=node' + id + '>' + contents + '</mn>';
	}
	function addOpMathML(op, id) {
		if (output === outputType.text || op === '') { return op; }
		if (op === '*') { op = (output === outputType.mathml) ? '&#8729;' : '\\cdot'; }
		if (op === '<=') { op = (output === outputType.mathml) ? '&#x2264;' : '\\le'; }
		if (op === '>=') { op = (output === outputType.mathml) ? '&#x2265;' : '\\ge'; }
		if (op === '!=') { op = (output === outputType.mathml) ? '&#x2260;' : '\\ne'; }
		if (op === '\\pm') { op = (output === outputType.mathml) ? '&#x00B1;' : '\\pm'; }
		if (output === outputType.latex) { return op; }
		return '<mo class="op" id=node' + id + '>' + op + '</mo>';
	}
	function addFractionMathML(top, bottom, id, isNumeric) {
		var shouldShow = (showDivSign === divSign.always ||
				(showDivSign === divSign.notNumeric && !isNumeric));
		if (output === outputType.text) {
			var op = '/';
			if (shouldShow) { op = '÷'; }
			return top + op + bottom;
		} else if (output === outputType.latex) {
			return shouldShow ? (top + '\\div' + bottom) : ('\\frac{' + top + '}{' + bottom + '}');
		}
		if (shouldShow) {
			return top + addOpMathML('&#xF7;', id) + bottom;
		}
		return '<mfrac id=node' + id + '><mrow>' + top + '</mrow><mrow>' + bottom +
			'</mrow></mfrac>';
	}
	function addRadicalMathML(radicand, root, id) {
		var result = '';
		if (root === 2) { root = ''; }
		if (output === outputType.mathml) {
			result = '<mroot>' + radicand + root + '</mroot>';
		} else {
			result = '\\sqrt';
			if (root !== '') { result += '[' + root + ']'; }
			result += '{' + radicand + '}';
		}
		return result;
	}
	function numberOnSide(node, side) {
		if (node === null) { return false; }
		if (node.type === 'number') { return true; }
		if (node.type === 'unary') {
			return (node.op === '-') ? numberOnSide(node.child, side) : false;
		}
		if (node.type === 'compound') {
			if (node.op === '^' && side === 'right' && output !== outputType.text) {
				return false;
			}
			return (side === 'left') ? numberOnSide(node.lhs, side) : numberOnSide(node.rhs, side);
		}
		return false;
	}
	function rhsUnderDiv(node) {
		if (node.type !== 'compound') { return false; }
		if (node.op === '/') { return true; }
		return rhsUnderDiv(node.rhs);
	}
	function hasParens(node, side) {
		if (node.parent === null || node.type === 'number' || node.type === 'identifier') {
			return false;
		}
		if (node === null) { return false; }
		if (addParens(node.parent, node)) { return true; }
		return (side === 'left') ? hasParens(node.lhs, side) : hasParens(node.rhs, side);
	}
	function shouldAddTimesOperator(node) {
		if (output !== outputType.text && node.type === 'compound' &&
			((node.lhs.type === 'compound' && node.lhs.op === '^') ||
			(node.rhs.type === 'compound' && node.rhs.op === '^' && node.lhs.type !== 'number'))) {
			return false;
		}
		if (output !== outputType.text && node.type === 'compound' &&
			isComplexExpression(node.lhs) && isComplexExpression(node.rhs)) {
			if (node.lhs.type === 'compound' && node.lhs.op === '^') { return false; }
		}
		if ((utils.isSimpleExpression(node.lhs) && utils.isSimpleExpression(node.rhs)) ||
			(node.type === 'compound' && (addParens(node, node.lhs) ||
			addParens(node, node.rhs)))) {
			return false;
		} else if (fractionUtils.isNegativeNumber(node.rhs) || (node.rhs.type === 'unary' &&
			node.rhs.op === '-' && node.rhs.child.type === 'number')) {
			return true;
		}
		if (showDivSign === divSign.never) {
			var underDiv = rhsUnderDiv(node);
			if (underDiv) { return output === outputType.text; }
		}
		return ((node.lhs.type === 'compound' && numberOnSide(node.lhs, 'right') &&
			!hasParens(node.lhs, 'right')) ||
			((node.rhs.type === 'compound' && numberOnSide(node.rhs, 'left') &&
				!hasParens(node.rhs, 'left'))));
	}
	function displayNumberValue(value, id) {
		var result = '';
		if (fractionUtils.isValueFraction(value)) {
			var top = displayNumberValue(value.top, id);
			var bottom = displayNumberValue(value.bottom, id);
			var isNumeric = (typeof value.top === 'number') || (typeof value.bottom === 'number');
			result = addFractionMathML(top, bottom, id, isNumeric);
		} else if (fractionUtils.isValueRadical(value)) {
			var radicand = displayNumberValue(value.radicand, id);
			var root = displayNumberValue(fractionUtils.invertFraction(value.power), id);
			result = addRadicalMathML(radicand, root, id);
		}
		else { result = addNodeMathML(value, id, false); }
		return result;
	}
	/**
	 * If we're displaying the output in LaTeX and we have a LaTeX symbol, and the thing
	 * that follows it starts with a letter, we need to add a space so that we don't output
	 * y<=x as y\lex, but rather y\le x, so that it's parsed properly by MathQuill.  We don't just
	 * add spaces after every LaTeX operator because it's not necessary in cases like x\le2
	 * @param {String} command   The string that may contain a command at the end.
	 * @param {String} following The string representation of the term following the operation.
	 */
	function addSpaceAfterTeXCommandIfNecessary(command, following) {
		if (output !== outputType.mathml &&
			/\\[a-zA-Z]+$/.test(command) && /^[a-zA-Z]/.test(following)) {
			command += ' ';
		}
		return command;
	}
	function wrapInParens(exp, node) {
		if (output === outputType.mathml) {
			return '<mfenced id=node' + node.id + '><mrow>' + exp + '</mrow></mfenced>';
		} else if (output === outputType.latex) {
			return '\\left(' + exp + '\\right)';
		} else {
			return '(' + exp + ')';
		}
	}
	function displayNode(node) {
		if (node.type === 'identifier') { return addNodeMathML(node.value, node.id, true); }
		var result = '';
		if (node.type === 'number') {
			result = displayNumberValue(node.value, node.id);
		}
		var op = '';
		if (mode === parenMode.full || node.op !== '*' ||
			(node.type === 'compound' && node.op === '*' &&
				((node.lhs === null || node.rhs === null) ||
					((node.lhs.type === 'number' && node.rhs.type === 'number') ||
				shouldAddTimesOperator(node))))) {
			op = node.op;
			op = addOpMathML(op, node.id);
		}
		if (node.op === '\\sqrt') {
			var root = node.lhs;
			var radicand = node.rhs;
			if (node.type === 'unary') {
				root = null;
				radicand = node.child;
			}
			if (radicand.type === 'number' && radicand.value === -1) {
				result = addNodeMathML('i', node.id, true);
			} else {
				root = (root === null) ? '' : displayNode(root);
				radicand = displayNode(radicand);
				result = addRadicalMathML(radicand, root, node.id);
			}
		} else if (node.type === 'compound') {
			var lhsString = (node.lhs === null) ? '' : displayNode(node.lhs);
			var rhsString = (node.rhs === null) ? '' : displayNode(node.rhs);
			op = addSpaceAfterTeXCommandIfNecessary(op, rhsString);
			lhsString = addSpaceAfterTeXCommandIfNecessary(lhsString, op + rhsString);
			result = lhsString + op + rhsString;
			if (node.op === '^' && mode !== parenMode.full && output !== outputType.text) {
				if (output === outputType.mathml) {
					result = '<msup id=node' + node.id + '>' + lhsString + rhsString + '</msup>';
				} else if (output === outputType.latex) {
					if (rhsString.length > 1) { rhsString = '{' + rhsString + '}'; }
					result = lhsString + '^' + rhsString;
				}
			} else if (node.op === '/') {
				result = addFractionMathML(lhsString, rhsString, node.id, false);
			} else if (node.op === '\\log') {
				var base = '';
				if (lhsString === 'e') { op = '\\ln';}
				else if (node.lhs.type === 'number' && node.lhs.value !== 10) {
					base = '_' + lhsString;
				}
				result = op + base + wrapInParens(rhsString, node);
			}
		} else if (utils.isUnaryNegative(node)) {
			result = op + displayNode(node.child);
		} else if (utils.isAbsValue(node)) {
			result = displayNode(node.child);
			switch (output) {
				case outputType.text:
					result = '|' + result + '|';
					break;
				case outputType.latex:
					result = '\\left|' + result + '\\right|';
					break;
				case outputType.mathml:
					result = '<mfenced open=\'|\' close=\'|\' id=node' + node.id + '><mrow>' +
						result + '</mrow></mfenced>';
			}
		} else if (node.type === 'unary') {
			result = displayNode(node.child);
			op = addSpaceAfterTeXCommandIfNecessary(op, result);
			result = op + result;
		} else if (node.type === 'ternary') {
			var left = displayNode(node.left);
			var middle = displayNode(node.middle);
			var right = displayNode(node.right);
			var op1 = addSpaceAfterTeXCommandIfNecessary(addOpMathML(node.op1, node.id), middle);
			var op2 = addSpaceAfterTeXCommandIfNecessary(addOpMathML(node.op2, node.id), right);
			// Probably not necessary since the operators will not likely be hidden for ternary,
			// so there will not likely be an alpha char after left or middle, but putting these
			// here just in case.
			left = addSpaceAfterTeXCommandIfNecessary(left, op1 + middle);
			middle = addSpaceAfterTeXCommandIfNecessary(middle, op2 + right);

			result = left + op1 + middle + op2 + right;
		}
		if (node.parent !== null) {
			if (addParens(node.parent, node)) {
				result = wrapInParens(result, node);
			} else if (output === outputType.mathml && node.type !== 'number' &&
				(showDivSign !== divSign.never || node.op !== '/') && node.op !== '^') {
				result = '<mrow id=node' + node.id + '>' + result + '</mrow>';
			}
		}
		return result;
	}

	var self = {
		/**
		 * Takes an expression and returns a string in whatever form specified by the options.
		 * @param  {Expression} exp           The expression to stringify.
		 * @param  {outputType} desiredOutput Output options:
		 *                                    text
		 *                                    latex (default)
		 *                                    mathml
		 * @param  {parenMode}  desiredMode   Mode options:
		 *                                    full
		 *                                    terms
		 *                                    necessary (default)
		 * @param  {divSign}    divAsFraction Div options:
		 *                                    never (default)
		 *                                    notNumeric
		 *                                    always
		 * @return {String}                   The string representation of the expression.
		 */
		displayExpression: function(exp, desiredOutput, desiredMode, divAsFraction) {
			if (exp === null || exp === errorNode) { return ''; }
			showDivSign = (typeof divAsFraction !== 'undefined') ? divAsFraction : divSign.never;
			mode = (typeof desiredMode !== 'undefined') ? desiredMode : parenMode.necessary;
			output = (typeof desiredOutput !== 'undefined') ? desiredOutput : outputType.latex;
			var result = displayNode(exp);
			if (output === outputType.mathml) {
				var id = 'math`';
				if (typeof expression !== 'undefined' &&
					typeof expression.nextId !== 'undefined') {
					id = expression.nextId++;
				}
				result = '<math xmlns="http://www.w3.org/1998/Math/MathML" id=node' + id + '>' +
					result + '</math>';
			}
			return '' + result;
		}
	};
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.display = display;
	exports.parenMode = parenMode;
	exports.outputType = outputType;
	exports.divSign = divSign;
}