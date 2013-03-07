/*global process:true require:true exports:true */


/**
 * This file defines functions to be called by a little script at the bottom of the file, intended
 * to be run through node.js
 *
 * The general form of a call to that script will look like:
 * node nodeWrapper.js [newline] command exp [exp...]
 *
 * If newline is included, the output will include a newline, otherwise it will not.
 *
 * Some commands require at least 2 expressions, other only one.
 *
 * The expressions should be input as LaTeX (though only a limited set of LaTeX commands are
 * supported).  This means strings like '2xy' are also fine.
 * Note: multiple alphabetic characters in a row not preceeded by a \ are interpreted as separate
 *       identifiers.
 *
 *
 * Commands:
 *
 * parse : parses all the strings into expressions and displays them.  Mostly useful for
 *         checking whether things can be parsed/displayed correctly or not.
 *
 * sample usage:
 * node nodeWrapper.js parse [newline] '\sqrt{8}' '\pi*r^2' '4\le2' '5(3-2)'
 * output: \sqrt{8},\pi r^2,4<=2,5(3-2)
 * (followed by newline iff the newline option is passed)
 *
 *
 * simplify : parses each argument and evaluates each one as fully as possible, simplifying
 *            the expressions into a standard form.
 *
 * node nodeWrapper.js simplify newline '\sqrt{8}' '\pi*r^2' '4\le2' '5(3-2)'
 * output: 2*\sqrt{2},\pi r^2,4<=2,5
 *
 *
 * simplifyShowDiv : same as simplify but uses \div instead of / or \frac{}{} for division.
 *
 *
 * verbatim : checks that all the expressions passed in are exactly the same, ignoring
 *            whitespace and non-necessary parentheses.  Does not simplify expressions at
 *            all.
 *
 * node nodeWrapper.js verbatim newline '(2*3)*x' '2(3)x' '2*3*x' '2\cdot 3 * x' '2 3 x'
 * output: true
 * node nodeWrapper.js verbatim newline '(2*3)x' '2(3*x)'
 * output: false
 * (this is because they have different parse trees)
 *
 *
 * commute : checks that all expressions passed in have the same general form, but allows
 *           terms to be in any order, so x+y === y+x.  Again, does not simplify at all.
 *
 * node nodeWrapper.js commute newline '(2*y)x-2' '2(yx)-2' '-2+2yx' '-2+x2y' 'y2x-2' 'yx2+(-2)'
 * output: true
 * node nodeWrapper.js commute newline '2xy-2' '2(xy-1)'
 * output: false
 *
 *
 * commuteCo : same as above but if one of the expressions (probably the one you pass in)
 *             has numeric coefficients at the front of a term, that coefficient must be at
 *             the front of the other term for it to register as the same.
 *
 * node nodeWrapper.js commuteCo newline '(2*y)x-2' '2(yx)-2' '2 x y - 2' '-2+2yx' '-(2)+2xy'
 * output: true
 * node nodeWrapper.js commuteCo newline '2yx-2' 'y2x-2'
 * output: false
 *
 *
 * full : does the same thing as the simplify option on both expressions, and checks if they
 *        are equal under the commute definition.
 *
 * node nodeWrapper.js full newline '-2+2yx=4' '-2+x2y=4' 'y2x-2=4' '4=yx2+(-2)' '4=2(xy-1)'
 * output: true
 * node nodeWrapper.js full newline '-2+2yx=4' '0=2xy-6'
 * node nodeWrapper.js full newline '(x+3)^2/((x^2+5x+6))=0' '(x+3)/(x+2)=0'
 * output: false
 *
 * fullEq : does the same as full, but if that fails, tries doing synthetic division of one
 *          over the other (first pulling all terms to one side of equation/inequality),
 *          and if it gets a constant as the answer, returns true (depending on whether or
 *          not it’s positive in the inequality case, and must be 1 if it's an expression
 *          rather than an equation).
 *
 * node nodeWrapper.js fullEq newline '-2+2yx=4' '0=2xy-6' '2xy=6'
 * node nodeWrapper.js fullEq newline '(x+3)^2/((x^2+5x+6))=0' '(x+3)/(x+2)=0'
 * node nodeWrapper.js fullEq newline '-x>1' '-1>x' 'x<-1'
 * output: true
 *
 *
 * polydiv : does synthetic division on each pair of expressions and displays the result.
 *
 * node nodeWrapper.js polydiv newline '(x+y)*(w+z)' '(x+y)' '(x+3)^2' 'x^2+5x+6' 'x+3' 'x+2'
 * output: w+z,1+(x+3)/(x^2+5x+6),1+1/(x+2)
 *
 *
 * eqBreakdown : runs the gamut of equality checks from most stict to least strict and returns the
 *               strictist one applicable as a string, same as the equality commands above.
 *
 * node nodeWrapper.js eqBreakdown newline '(x+y)w' 'xw+yw' '2' '2' '4x=3' '2(2x-3/2)=0' '2' '3'
 * output: full,verbatim,fullEq,none
 *
 *
 * solve : solve an equation/inequality for a specific variable.  If it's not an equation or doesn't
 *         contain the variable, just spits back the expression you passed in.  If there are
 *         multiple solutions, all of them will be printed.
 *         Note that this makes things confusing if you do multiple equations at a time, because I
 *         don't group the answer, so if there might be multiple solutions for the expressions
 *         you're solving, just make several individual calls.
 *
 * node nodeWrapper.js solve newline 'x-2=0' 'x' 'c-2=0' 'x' '2>-\sqrt{x}' 'x' 'x^2=2' 'x'
 * output: x=2,c-2=0,x>4,x=\sqrt{2},x=-\sqrt{2}
 *
 *
 * compare : take two expressions which evaluate down to numbers and compare them.  Will return a
 *           negative number is the first is smaller, 0 if the same, and a positive number if the
 *           first is bigger.  Returns error if it's not the case that both sides are numbers.
 *
 * node nodeWrapper.js compare newline '3c' '3' '4' '-1' '\sqrt{3}-2' '4/7' '4-5' '-8/(4*2)'
 * output: error,5,-0.8393777638596942,0
 *
 *
 * compareOp : Same as above but translates the output into the relation between the two
 *             expressions.
 *
 * node nodeWrapper.js compareOp newline '3c' '3' '4' '-1' '\sqrt{3}-2' '4/7' '4-5' '-8/(4*2)'
 * output: incomparable,>,<,=
 *
 */



if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var processExpressions = require('./processExpressions.js').processExpressions;
	var errorNode = require('./utils.js').errorNode;
	var display = require('./display.js').display;
	var divSign = require('./display.js').divSign;
	var parenMode = require('./display.js').parenMode;
	var outputType = require('./display.js').outputType;
	var equalityType = require('./evaluation.js').equalityType;
	var evaluate = require('./evaluation.js').evaluate;
}

var nodeWrapper = (function() {
	/**
	 * How to display division.
	 * @type {divSign}
	 */
	var show = divSign.never;
	var output = outputType.latex;
	var paren = parenMode.necessary;

	/**
	 * Stringifies an array, using the callback to convert whatever's in the array into a string.
	 * @param  {Array}    arr      An array containing any type.
	 * @param  {Function} callback Function that takes one of whatever type the array contains and
	 *                             returns a string representing that object.
	 * @return {String}            String representing the items in the array, separated by commas.
	 */
	var stringifyArray = function(arr, callback) {
		var result = '';
		if (typeof callback === 'undefined') { callback = noOp; }
		for (var i = 0; i < arr.length; i++) {
			if (i > 0) { result += ','; }
			result += callback(arr[i]);
		}
		return result;
	};


	/**
	 * Used as the callback for stringifyArray when the object is an expression.
	 * @param  {Expression} expression The expression to stringify.
	 * @return {String}                The string representing the expression.
	 */
	var stringifyExpression = function(expression) {
		var result = display.displayExpression(expression, output, paren, show);
		/*
		result += ',' + display.displayExpression(expression);
		result += ',' + display.displayExpression(
			expression, outputType.mathml, parenMode.necessary);
		result = '(' + result + ')';
		*/
		return result;
	};


	/**
	 * Used as the callback for stringifyArray when the object is an equalityType.
	 * @param  {equalityType} type The enum type (really just an integer) representing an
	 *                             equality type.
	 * @return {String}            The string version of that equalityType.
	 */
	var stringifyEqualityType = function(type) {
		switch (type) {
			case equalityType.verbatim : return 'verbatim';
			case equalityType.commuteCo : return 'commuteCo';
			case equalityType.commute : return 'commute';
			case equalityType.full : return 'full';
			case equalityType.fullEq : return 'fullEq';
			case equalityType.none : return 'none';
		}
		return 'unknown';
	};


	/**
	 * Used as the calllback for stringifyArray when the object is what's returned by the comparison
	 * function for numbers.  Translates it to '=', '<' or '>' based on trichotomy!
	 * @param  {Number} comp The value returned by the comparison function
	 * @return {String}      The string representation of the relation between the two values.
	 */
	var stringifyCompareValue = function(comp) {
		if (typeof comp !== 'number') { return 'incomparable'; }
		return (comp === 0) ? '=' : ((comp <= 0) ? '<' : '>');
	};


	/**
	 * Used as the callback for stringifyArray when nothing is passed in, just returns the object
	 * that was passed in.
	 * @param  {[type]} obj The thing from the array.
	 * @return {[type]}     The same thing!
	 */
	var noOp = function(obj) {
		if (obj === errorNode) { return 'error'; }
		return obj;
	};


	var self = {
		/**
		 * Takes a terse command, runs the appropriate check/evaluation, and hands back a string
		 * representing the result.
		 * @param  {String}            command          The command passed in by the user.  See
		 *                                              comment block at top of file for desciptions
		 *                                              of the command options.
		 * @param  {Array<Expression>} parsedExpessions An array containing the parsed versions of
		 *                                              all the strings passed in on the command
		 *                                              line.
		 * @return {String}                             The result of running the command, as a
		 *                                              string.
		 */
		runCommand : function(command, parsedExpessions) {
			show = divSign.never;
			if (command === 'parse') {
				return stringifyArray(parsedExpessions, stringifyExpression);
			} else if (command === 'simplify') {
				return stringifyArray(
					processExpressions.simplifyExpressions(parsedExpessions), stringifyExpression);
			} else if (command === 'simplifyNumeric') {
				return stringifyArray(processExpressions.simplifyExpressionsNumeric(
					processExpressions.simplifyExpressions(parsedExpessions)), stringifyExpression);
			} else if (command === 'simplifyShowDiv') {
				show = divSign.notNumeric;
				return stringifyArray(
					processExpressions.simplifyExpressions(parsedExpessions), stringifyExpression);
			} else if (command === 'verbatim') {
				return processExpressions.checkVerbatimEquality(parsedExpessions);
			} else if (command === 'commute') {
				return processExpressions.checkCommuteEquality(parsedExpessions);
			} else if (command === 'commuteCo') {
				return processExpressions.checkCommuteEqualityCoefficient(parsedExpessions);
			} else if (command === 'full') {
				return processExpressions.checkFullEquality(parsedExpessions);
			} else if (command === 'fullEq') {
				return processExpressions.checkFullEqualityEquationOrInequality(parsedExpessions);
			} else if (command === 'polydiv') {
				return stringifyArray(processExpressions.dividePolynomials(parsedExpessions),
					stringifyExpression);
			} else if (command === 'eqBreakdown') {
				return stringifyArray(processExpressions.strictestEquality(parsedExpessions, true),
						stringifyEqualityType);
			} else if (command === 'eqBreakdownNoFullEq') {
				return stringifyArray(processExpressions.strictestEquality(parsedExpessions, false),
					stringifyEqualityType);
			} else if (command === 'solve') {
				return stringifyArray(
					processExpressions.solve(parsedExpessions), stringifyExpression);
			} else if (command === 'compare') {
				return stringifyArray(processExpressions.compare(parsedExpessions));
			} else if (command === 'compareOp') {
				return stringifyArray(
					processExpressions.compare(parsedExpessions), stringifyCompareValue);
			} else if (command === 'compute') {
				return stringifyArray(processExpressions.compute(parsedExpessions));
			} else if (command === 'getTerms' || command === 'getTermsNoSort') {
				if (parsedExpessions.length !== 1) {
					return 'need exactly one expression';
				}
				var sort = command === 'getTerms';
				var termsAndOp = evaluate.getAllTermsAtLevel(parsedExpessions[0], sort);
				if (typeof termsAndOp.operator === 'undefined') { termsAndOp.operator = 'none'; }
				output = outputType.text;
				return termsAndOp.operator + ',' +
					stringifyArray(termsAndOp.terms, stringifyExpression);
				output = outputType.latex;
			}
			return 'Unknown command: ' + command;
		}
	};

	return self;
}());


/**
 * If we're running on the command line as normal (as opposed to being run in a browser or from the
 * test file), look at the args and use them to call runCommand then print the result.
 */
if (typeof process !== 'undefined' && process.argv.length > 3) {
	var command = process.argv[2];
	var newline = false;
	var start = 3;
	if (process.argv[3] === 'newline') {
		newline = true;
		start = 4;
	}

	var expressions = process.argv.splice(start);
	var parsedExpessions = processExpressions.parseExpressions(expressions);
	var result = nodeWrapper.runCommand(command, parsedExpessions);

	if (newline) {
		console.log(result);
	} else {
		process.stdout.write('' + result);
	}
}

if (typeof exports !== 'undefined') {
	exports.nodeWrapper = nodeWrapper;
}