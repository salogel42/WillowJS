/*global require:true exports:true */


if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var pathToLib = '../willowjs/src/';
	var processExpressions = require(pathToLib + 'processExpressions.js').processExpressions;
	var display = require(pathToLib + 'display.js').display;
	var fractionUtils = require(pathToLib + 'fractionUtils.js').fractionUtils;
	var parenMode = require(pathToLib + 'display.js').parenMode;
	var outputType = require(pathToLib + 'display.js').outputType;
	var divSign = require(pathToLib + 'display.js').divSign;
	var parser = require(pathToLib + 'parser.js').parser;
	var evaluate = require(pathToLib + 'evaluation.js').evaluate;
	var equalityType = require(pathToLib + 'evaluation.js').equalityType;
	var equality = require(pathToLib + 'equality.js').equality;
	var expr = require(pathToLib + 'expr.js').expr;
	var operatorProperties = require(pathToLib + 'expr.js').operatorProperties;
	var getPrecedence = require(pathToLib + 'expr.js').getPrecedence;
	var errorNode = require(pathToLib + 'utils.js').errorNode;
}

var quizUtils = (function() {
	var debugNormalChecks = false;
	var debugQuad = false;
	/**
	 * List of identifiers to choose from.  No need to get too fancy, just want them to
	 * see things that aren't x or y sometimes.
	 * @type {String}
	 */
	var identifiers = 'abcxyz';

	/**
	 * The following two arrays contain the most common options for choosing a random operator.
	 * @type {Array}
	 * @type {Array}
	 * @type {Array}
	 */
	var additionAndSubtraction = ['+', '-'];
	var addSubMultDiv = ['+', '-', '\\cdot', '/'];
	var addSubDiv = ['+', '-', '/'];

	/**
	 * Used as an upper bound for the result of multiplying two numbers, or an upper bound for
	 * anything that might need to be factored.
	 * @type {Number}
	 */
	var MULT_UPPER_BOUND = 144;
	var TERM_UPPER_BOUND = Math.sqrt(MULT_UPPER_BOUND);

	/**
	 * Used for comparing to make sure we never give problems that involve multiplying numbers
	 * bigger than TERM_UPPER_BOUND.
	 * @type {Expression}
	 */
	var TERM_UPPER_BOUND_NODE = expr.createSimpleExpression('number', TERM_UPPER_BOUND);

	/**
	 * Convenience wrapper around Math.random()
	 * @param  {Number} min Smallest possible value.
	 * @param  {Number} max Largest possible value.
	 * @return {Number}     Random integer between min and max (inclusive), uniformly distributed.
	 */
	function getRandomInt(min, max) {
		// if the max was smaller than the min, just return min.
		if (min > max) { return Math.floor(min); }
		min = Math.floor(min);
		max = Math.floor(max);
		return Math.floor(Math.random() * (max - min + 1)) + min;
	}

	/**
	 * Convenience function to get a random bool.
	 * @param  {number} topRange See below.
	 * @param  {number} highest  See below.
	 * @return {Boolean} Should return true about highest/topRange of the time.
	 */
	function getRandomBool(topRange, highest) {
		if (typeof highest === 'undefined') { highest = 1; }
		if (typeof topRange === 'undefined') { topRange = 2; }
		return getRandomInt(1, topRange) <= highest;
	}

	/**
	 * Takes in a number and makes it negative about half the time.
	 * @param  {number} value The number to possibly make negative
	 * @return {number}       Either the original or the additive inverse of the number.
	 */
	function maybeMakeNegative(value) {
		return getRandomBool() ? -1 * value : value;
	}

	/**
	 * Convenience function to get an integer between -max and max, not including 0.
	 * @param  {Number} max Largest distance from 0.
	 * @return {Number}     Random integer between -max and max (inclusive), excluding 0.
	 */
	function getMaybeNegativeInt(max) {
		return maybeMakeNegative(getRandomInt(1, Math.abs(max)));
	}

	/**
	 * Convenience function to get a random character in a string.
	 * @param  {String} str Source string.
	 * @return {String}     A single character chosen uniformly at random from the string.
	 */
	function getRandomCharInString(str) {
		return str.charAt(getRandomInt(0, str.length - 1));
	}

	/**
	 * Convenience function to get a random elemnt from an array.
	 * @param  {Array<type>} arr Any kind of array.
	 * @return {type}            An element chosen uniformly at random from the array.
	 */
	function getRandomElementFromArray(arr) {
		return arr[getRandomInt(0, arr.length - 1)];
	}

	/**
	 * Convenience function to wrap an expression in LaTeX style parens.
	 * @param {String} expressionString The expression to wrap.
	 * @return {String}                 The same expression, but wrapped in parens.
	 */
	function addLaTeXParens(expressionString) {
		return '\\left(' + expressionString + '\\right)';
	}

	/**
	 * Convenience function to take a raw string (may have things like x+-y, etc.), parse it,
	 * simplify it as much as possible, then spit it back out as a nicely formatted LaTeX string.
	 *
	 * Note: this does compute values, not to be used if you want to maintain the original form,
	 *       i.e. (x-2)*x will become x^2-2x
	 *
	 * @param  {String} expressionString The original string.
	 * @return {String}                  The evaluated and formatted string.
	 */
	function parseEvalDisplay(expressionString) {
		return display.displayExpression(evaluate.evaluateRec(
			parser.parseEquationOrExpression(expressionString)));
	}

	/**
	 * Object containing all the properties of the problem.
	 * @param {String} questionString      The string representing the question.
	 * @param {expression} answer          The parsed version of the answer.
	 * @param {Array} customPrompt         An array containing the information needed to display the
	 *                                     question when we need extra explanatory text and math.
	 * @param {String} equalityStrictness  The least strict acceptable equality type.
	 */
	function ProblemDefinition(questionString, answer, customPrompt,
		equalityStrictness, prepopulateAnswer, customChecks, multiStep, history, useContinue) {
		this.questionString = questionString;
		this.answer = answer;
		this.customPrompt = customPrompt;
		this.equalityStrictness = equalityStrictness;
		this.prepopulateAnswer = prepopulateAnswer;
		this.customChecks = customChecks;
		this.multiStep = multiStep;
		this.history = history;
		this.useContinue = useContinue;
		this.getLastAttempt = function() {
			var lastAttempt = '';
			if (this.history.length !== 0) { lastAttempt = this.history[this.history.length - 1]; }
			else if (this.prepopulateAnswer) { lastAttempt = this.questionString; }
			return lastAttempt;
		};
	}

	/**
	 * Creates a ProblemDefinition with the most common parameters, filling in defaults for those
	 * not specified.
	 * @param {String} questionString      The string representing the question.
	 * @param {expression} answer          The parsed version of the answer.
	 */
	function DefaultProblemDefinition(questionString, answer, customPrompt) {
		if (typeof customPrompt === 'undefined') { customPrompt = null; }
		return new ProblemDefinition(questionString, answer, customPrompt,
			equalityType.verbatim, // equalityStrictness
			true, // prepopulateAnswer
			null, // customChecks
			true, // multiStep
			[], // history
			true); // useContinue
	}

	/**
	 * Call display on the node to get the expression string and evaluate it to get the answer.
	 * @param  {expression} question The expression representing the question.
	 * @param  {boolean} fraction    Whether or not it's a fraction problem.
	 * @return {ProblemDefinition}
	 */
	function computeAnswerFromQuestion(question, fraction) {
		var shouldShowDiv = (fraction) ? divSign.notNumeric : divSign.always;
		var questionString = display.displayExpression(
				question, outputType.latex, parenMode.necessary, shouldShowDiv);
		var answer = evaluate.evaluateRec(question);
		var problem = new DefaultProblemDefinition(questionString, answer);
		if (fraction) {
			problem.equalityStrictness = equalityType.commuteCo;
		} else {
			problem.prepopulateAnswer = false;
			problem.multiStep = false;
			problem.useContinue = false;
		}
		return problem;
	}

	/**
	 * Creates two expression strings based on the same set of terms, and combine them with an
	 * operation (will be + or - for these).
	 * @return {ProblemDefinition}
	 */
	function createPolynomialSimplificationWithTermSeeds(terms, opSet) {
		var left = evaluate.evaluateRec(
			parser.parseEquationOrExpression(createExpressionString(terms)));
		var right = evaluate.evaluateRec(
			parser.parseEquationOrExpression(createExpressionString(terms)));
		var op = getRandomElementFromArray(opSet);
		return createPolynomialSimplificationWithTerms(left, right, op);
	}

	/**
	 * Given two parsed expressions and a string representing an operation, create LaTeX and
	 * parsed versions of a question (done this way so that we can add the normally unnecessary
	 * parens to make the problem look the way we want it to), and calculate the answer.
	 *
	 * @return {ProblemDefinition}
	 */
	function createPolynomialSimplificationWithTerms(left, right, op) {
		var leftLaTeX = display.displayExpression(left);
		var rightLaTeX = display.displayExpression(right);
		var questionString = addLaTeXParens(leftLaTeX) + op + addLaTeXParens(rightLaTeX);
		var answer = evaluate.evaluateRec(expr.createCompoundExpression(left, right, op));

		return new DefaultProblemDefinition(questionString, answer);
	}

	/**
	 * Builds a string of the form x^2y^3 for whatever variables you pass in, randomly generating
	 * the coefficient and the exponents.
	 * @param  {Array<String>} vars  The variables to use in the term.
	 * @return {String}              The string representing the completed term.
	 */
	function createTermWithVariables(vars) {
		var expressionString = '';
		for (var i = 0; i < vars.length; i++) {
			expressionString += vars[i] + '^' + getRandomInt(1, 3);
		}
		return expressionString;
	}

	/**
	 * Take a list of variable names and create terms consisting of those variables raised to small
	 * powers.
	 * @param  {Array<String>} vars The variables to use.
	 * @param  {Number} num         The number of terms to create.
	 * @return {Array<String>}      A list of string terms.
	 */
	function generateTermFromVars(vars, num) {
		var terms = [];
		for (var i = 0; i < num; i++) {
			terms.push(createTermWithVariables(vars));
		}
		return terms;
	}

	/**
	 * Convenience function to get a string representing a possibly negative number from one up to
	 * a certain range.
	 * @param  {Number} topRange  The farthest from 0 the number should be.
	 * @param  {Boolean} negative Whether it should be allowed to be negative.
	 * @return {String}           The string representing the number.
	 */
	function createCoefficient(topRange, negative) {
		var num = getRandomInt(1, topRange);
		if (negative) { num = maybeMakeNegative(num); }
		return '' + num;
	}

	/**
	 * Convenience function combining some of the above functions to create strings of the form
	 * 2x^3y^1 (note: not simplified!).
	 * @param  {Array<String>} vars Strings representing the variables to use.
	 * @return {String}             String representing the term.
	 */
	function createTermWithVariablesAndCoefficient(vars) {
		return createCoefficient(4, true) + createTermWithVariables(vars);
	}

	/**
	 * Returns a string representing an expression consisting of the terms that were passed
	 * in (which will each be something like x^2 or x^3y^2) with coefficients, added together.
	 * @param  {Array<String>} terms A list of terms (expected to be terms with no coefficents).
	 * @return {String}              The complete string representing the expression.
	 */
	function createExpressionString(terms) {
		var expressionString = createCoefficient(10, true);
		for (var i = 0; i < terms.length; i++) {
			expressionString += '+' + createCoefficient(10, true) + terms[i];
		}
		return expressionString;
	}

	/**
	 * Constructor for AnswerAndComment Object, which is passed back to the caller to let them know
	 * how the student answer stacked up against the real one.
	 * @param {answerCategory} correct [description]
	 * @param {String} comment [description]
	 */
	function AnswerAndComment(correct, comment) {
		this.correct = correct;
		this.comment = comment;
	}

	/**
	 * Convenience constructor wrapper for the correct case, for consistency.
	 */
	function CorrectAnswerAndComment() {
		return new AnswerAndComment(self.answerCategory.complete, 'Correct!');
	}

	/**
	 * Helper for createSolveForVariableProblem, since both sides have the same form.
	 * Makes something that looks like 3-2(x+3), with the operations chosen at random and the order
	 * potentially switched.
	 * @param  {String} identifier The variable to use.
	 * @return {String}            The constructed string described above.
	 */
	function createSide(identifier) {
		var subSide = identifier;
		if (getRandomInt(0, 5) !== 0) {
			var num = getRandomInt(1, 5);
			var op = getRandomElementFromArray(additionAndSubtraction);
			subSide = identifier + op + num;
			if (getRandomBool()) { subSide = num + op + identifier; }
			subSide = addLaTeXParens(subSide);
		}
		subSide = getRandomInt(2,4) + subSide;
		var side = subSide;
		if (getRandomInt(0, 5) <= 4) {
			var secondNum = getRandomInt(1, 5);
			var secondOp = getRandomElementFromArray(additionAndSubtraction);
			side = secondNum + secondOp + subSide;
			if (getRandomBool()) { side = subSide + secondOp + secondNum; }
		}
		return side;
	}

	/**
	 * Creates a single-variable unsimplified problem, where the answer is of the form
	 * var = number
	 * @return {ProblemDefinition}
	 */
	function createSolveForVariableProblem() {
		var identifier = getRandomCharInString(identifiers);
		var questionString = createSide(identifier) + '=' + createSide(identifier);
		var question = parser.parseEquationOrExpression(questionString);
		var variable = parser.parseEquationOrExpression(identifier);
		var answer = evaluate.solveForIdentifier(question, variable)[0];
		if (answer === errorNode || answer.type !== 'compound' ||
			!answer.lhs.syntacticEquals(variable)) {
			return createSolveForVariableProblem();
		}

		var customPrompt = [
			new PromptEntry(false, 'Solve '),
			new PromptEntry(true, questionString),
			new PromptEntry(false, ' for '),
			new PromptEntry(true, identifier)
		];

		var problemDefinition = new DefaultProblemDefinition(questionString, answer, customPrompt);
		problemDefinition.equalityStrictness = equalityType.commuteCo;
		problemDefinition.customChecks = function(studentExpression) {
			return doAllNormalChecks(problemDefinition, studentExpression, function(answerString) {
				if (answerString.charAt(0) !== identifier) {
					return new AnswerAndComment(self.answerCategory.intermediate,
						'Correct, but the expression can be written in a more standard way.');
				}
				return new CorrectAnswerAndComment();
			});
		};
		return problemDefinition;
	}

	function pickOpWithinConstraints(left, right) {
		var op = getRandomElementFromArray(addSubMultDiv);
		// If it's multiplication, and either side is bigger than TERM_UPPER_BOUND, get a new op
		// that's not multiplication.
		if (op === '\\cdot' && expr.compareNodesNumerically(left, TERM_UPPER_BOUND_NODE) > 0 ||
				expr.compareNodesNumerically(right, TERM_UPPER_BOUND_NODE) > 0) {
				op = getRandomElementFromArray(addSubDiv);
		}
		/* TODO(sdspikes): actually make it so you never have to multiply things bigger than
		*                  MULT_UPPER_BOUND even when there are fractions involved.
		if () {
			var leftEval = evaluate.evaluateRec(left);
			var rightEval = evaluate.evaluateRec(right);
			if (fractionUtils.isValueFraction(leftEval))
			op = getRandomElementFromArray(addSubDiv);
		}
		*/
		return op;

	}

	/**
	 * Makes an arithmetic problem with lots of operations.
	 * @return {Expression}
	 */
	function createOrderOfOperationsProblem() {
		var left = createRandomBinaryNumericExpressionHelper(true, getRandomBool(5, 2));
		var numOps = getRandomInt(1, 3);
		for (var i = 0; i < numOps; i++) {
			var right = expr.createSimpleExpression('number', getRandomInt(1, 5));
			if (getRandomBool()) {
				right = createRandomBinaryNumericExpressionHelper(true, getRandomBool(5, 2));
			}
			var op = pickOpWithinConstraints(left, right);
			left = expr.createCompoundExpression(left, right, op);
		}
		var evaluated = evaluate.evaluateRec(left);
		if (evaluated === errorNode || evaluated.type !== 'number') {
			return createOrderOfOperationsProblem();
		}
		var problemDefinition = computeAnswerFromQuestion(left, true);
		if (!problemDefinition.answer.syntacticEquals(
			evaluate.evaluateRec(
				parser.parseEquationOrExpression(problemDefinition.questionString)))) {
			/**
			 * TODO(sdspikes) : Debug this! How does this even happen? Can't reproduce on command
			 * line, because i can't get this weird output to happen.  the \div should not be
			 * shown here because the -80/8 should be shown as \frac, like the others, can't
			 * figure out why not.
			 * \left(-5+-8+5+\frac{-6}{2}-\frac{7}{3}\right)-80\div8
			 * ((((-5)+(-8))+5)+((-6/2)-7/3))*((-80)÷8)
			 * \left(\left(\left(\left(-5\right)+\left(-8\right)\right)+5\right)+\left(\left(
			 * \frac{-6}{2}\right)-\frac{7}{3}\right)\right)\cdot\left(\left(-80\right)\div8\right)
			 *
			 * \frac{400}{3}
			 * another case:
			 * \left(\frac{12}{8}+\frac{5}{4}-3\right)-10\cdot4\cdot5
			 * (((12/8+5/4)-3)*((-10)*4))*5
			console.log("merp");
			console.log(problemDefinition.questionString);
			console.log(display.displayExpression(
				left, outputType.text, parenMode.full, divSign.notNumeric));
			console.log(display.displayExpression(
				left, outputType.latex, parenMode.full, divSign.notNumeric));
			console.log(display.displayExpression(problemDefinition.answer));
			console.log(left);
			 */
			// for now, punt and make a new problem that isn't going to break.
			return createOrderOfOperationsProblem();
		}
		return problemDefinition;

	}


	/**
	 * Makes a quadratic factoring problem by generating the factors and multiplying them together.
	 * @param  {Boolean} shouldHaveCoefficient Whether or not the x's should have coefficients.
	 *                                         i.e. if the unfactored form can have a coefficient on
	 *                                         the x^2 term.
	 * @return {ProblemDefinition}
	 */
	function createQuadraticFactoringProblem(shouldHaveCoefficient) {
		if (typeof shouldHaveCoefficient === 'undefined') { shouldHaveCoefficient = false; }
		var total = MULT_UPPER_BOUND;
		/**
		 * Declared inline to exploit the fact that I can use local variables from
		 * createQuadraticFactoringProblem, namely total, and modify it without going to the
		 * trouble of bundling it in to a complicated return type.  Also uses shouldHaveCoefficient
		 * but doesn't modify it.
		 *
		 * Uses total to ensure that in the computed quadratic, Ax^2 + Bx+ C, A*C does not exceed
		 * MULT_UPPER_BOUND, since one of the methods taught for factoring quadratics involves
		 * multiplying them.
		 *
		 * @return {String} A string of the form x-2 or 2x+3 depending on shouldHaveCoefficient
		 */
		var createFactor = function() {
			var number = getMaybeNegativeInt(Math.min(Math.abs(total), TERM_UPPER_BOUND));
			total /= number;
			var coefficient = shouldHaveCoefficient ?
				getMaybeNegativeInt(Math.min(Math.abs(total), TERM_UPPER_BOUND)) : 1;
			// If both numbers on one side have a common factor (not relatively prime), then the
			// answer is ambiguous, e.g. (x+1)(3x+6) vs (3x+3)(x+2), so we just pick a different
			// number so that we don't have to worry about checking for both.
			while (fractionUtils.getGcd(Math.abs(number), Math.abs(coefficient)) !== 1) {
				coefficient = getMaybeNegativeInt(Math.min(Math.abs(total), TERM_UPPER_BOUND));
			}
			total /= coefficient;
			return coefficient + 'x+' + number;
		};
		// We evaluate these since they may look like -1x+-2 or something.  Want it in the prettier
		// "standard" form: -x-2.  Evaluate separately because otherwise they'd get multiplied out.
		var left = evaluate.evaluateRec(parser.parseEquationOrExpression(createFactor()));
		var right = evaluate.evaluateRec(parser.parseEquationOrExpression(createFactor()));

		var answer = expr.createCompoundExpression(left, right, '*');
		// What if instead of (x-2)(x-3), they give the answer as (2-x)(3-x)?  Compute that
		// variation as well to use when checking the answer.
		var altAnswer = expr.createCompoundExpression(
			evaluate.evaluateRec(expr.createUnaryExpression(left, '-')),
			evaluate.evaluateRec(expr.createUnaryExpression(right, '-')),
			'*');
		var questionString = display.displayExpression(evaluate.evaluateRec(answer));
		var customPrompt = [
			new PromptEntry(false, 'Factor the following quadratic into two terms: '),
			new PromptEntry(true, questionString)
		];

		var problemDefinition = new ProblemDefinition(questionString, answer, customPrompt,
			equalityType.full, // equalityStrictness
			shouldHaveCoefficient, // prepopulateAnswer
			null, // customChecks
			shouldHaveCoefficient, // multiStep
			[], // history
			shouldHaveCoefficient); // useContinue

		problemDefinition.customChecks = function(studentExpression) {
			var studentExpressionParsed = parser.parseEquationOrExpression(studentExpression);

			var errors = doErrorChecks(
				problemDefinition, studentExpression, studentExpressionParsed);
			if (errors !== null) { return errors; }

			var breakdown = evaluate.strictestEqualityType(studentExpressionParsed, answer, true);
			var altBreakdown =
				evaluate.strictestEqualityType(studentExpressionParsed, altAnswer, true);
			var strictestEquality = Math.min(breakdown, altBreakdown);
			if (strictestEquality <= equalityType.commuteCo) {
				return new CorrectAnswerAndComment();
			}
			return generateAnswerAndCommentNotExact(problemDefinition, strictestEquality);
		};
		return problemDefinition;
	}

	/**
	 * Takes a number and returns the ordinal representing that number.  Didn't have a good way to
	 * do things like twenty-fourth without a general number to string...  I really only need first
	 * and second for current purposes, but wrote it to do things like 123rd or 45th for anything
	 * bigger than 3, just for funsies.
	 * @param  {Number} num The number to convert.
	 * @return {String}     The ordinal as a string.
	 */
	function numToOrdinal(num) {
		switch (num) {
			case 1 : return 'first';
			case 2 : return 'second';
			case 3 : return 'third';
		}
		if (num > 20) {
			switch (num % 10) {
				case 1 : return num + 'st';
				case 2 : return num + 'nd';
				case 3 : return num + 'rd';
			}
		}
		return num + 'th';
	}

	function isNodeFraction(node) {
		return node.type === 'compound' && node.op === '/';
	}

	function putOverCommonDenominator(node) {
		if (node.type === 'number' && fractionUtils.isValueFraction(node.value)) {
			return expr.createCompoundExpression(
				expr.createSimpleExpression('number', node.value.top),
				expr.createSimpleExpression('number', node.value.bottom),
				'/');
		}
		if (utils.isUnaryNegative(node)) {
			var childNode = putOverCommonDenominator(node.child);
			if (isNodeFraction(childNode)) {
				return expr.createCompoundExpression(expr.arithmeticEvaluation(
					expr.createUnaryExpression(equality.normalizeTermSign(childNode.lhs), '-')),
					childNode.rhs, '/');
			}
		}
		if (node.type !== 'compound') { return node; }
		var leftNode = putOverCommonDenominator(node.lhs);
		var rightNode = putOverCommonDenominator(node.rhs);
		if (!(isNodeFraction(leftNode) || isNodeFraction(rightNode))) { return node; }
		if (getPrecedence(node.op) === 2) {
			var top = null;
			var bottom = null;
			if (isNodeFraction(leftNode) && isNodeFraction(rightNode)) {
				top = expr.createCompoundExpression(leftNode.lhs, rightNode.lhs, '*');
				bottom = expr.createCompoundExpression(leftNode.rhs, rightNode.rhs, '*');
			} else if (isNodeFraction(leftNode)) {
				top = expr.createCompoundExpression(leftNode.lhs, rightNode, '*');
				bottom = leftNode.rhs;
			} else if (isNodeFraction(rightNode)) {
				top = expr.createCompoundExpression(leftNode, rightNode.lhs, '*');
				bottom = rightNode.rhs;
			}
			var x = expr.createCompoundExpression(
				expr.arithmeticEvaluation(top), expr.arithmeticEvaluation(bottom), '/');
			return x;
		}
		if (!isNodeFraction(leftNode)) {
			leftNode = expr.createCompoundExpression(leftNode,
				expr.createSimpleExpression('number', 1), '/');
		}
		if (!isNodeFraction(rightNode)) {
			rightNode = expr.createCompoundExpression(rightNode,
				expr.createSimpleExpression('number', 1), '/');
		}
		if (getPrecedence(node.op) === 1 &&
			leftNode.rhs.type === 'number' && rightNode.rhs.type === 'number') {
			if (fractionUtils.compareFractions(leftNode.rhs.value, rightNode.rhs.value, false)) {
				return expr.createCompoundExpression(
					expr.createCompoundExpression(leftNode.lhs, rightNode.lhs, node.op),
					leftNode.rhs, '/');
			}
			if (fractionUtils.isInteger(leftNode.rhs.value) &&
				fractionUtils.isInteger(rightNode.rhs.value)) {
				var lcm = fractionUtils.getLcm(leftNode.rhs.value, rightNode.rhs.value);
				var topLeft = expr.createCompoundExpression(
					expr.createSimpleExpression('number', lcm/leftNode.rhs.value),
					leftNode.lhs, '*');
				var topRight = expr.createCompoundExpression(
					expr.createSimpleExpression('number', lcm/rightNode.rhs.value),
					rightNode.lhs, '*');
				var top = evaluate.evaluateRec(
					expr.createCompoundExpression(topLeft, topRight, node.op));
				return expr.createCompoundExpression(top,
					expr.createSimpleExpression('number', lcm), '/');
			}
		}
		return node;
	}

	function getAllValidAnswers(origAnswers) {
		var allAnswers = [];
		for (var i = 0; i < origAnswers.length; i++) {
			var answer = evaluate.evaluateRec(parser.parseEquationOrExpression(origAnswers[i]));
			if (i !== 0 && allAnswers[0][0].syntacticEquals(answer)) { continue; }

			var answers = [answer];
			var altAnswer = putOverCommonDenominator(answer);
			if (altAnswer !== errorNode &&
				!equality.equivalentModuloCommutativity(answer, altAnswer)) {
				answers.push(altAnswer);
				if (altAnswer.type === 'compound' && altAnswer.op === '/') {
					altAnswer = expr.createCompoundExpression(
						evaluate.evaluateRec(expr.createUnaryExpression(altAnswer.lhs, '-')),
						evaluate.evaluateRec(expr.createUnaryExpression(altAnswer.rhs, '-')),
						'/');
					answers.push(altAnswer);
				}
			}
			allAnswers.push(answers);
		}
		return allAnswers;
	}

	function getCommentForAnswerSet(answers, studentExpression, problemDefinition) {
		var comment = new AnswerAndComment(self.answerCategory.incorrect, '');
		for (var j = 0; j < answers.length; j++) {
			problemDefinition.answer = answers[j];
			if (comment.correct < self.answerCategory.complete) {
				var currentAnswer = doAllNormalChecks(
					problemDefinition, studentExpression, null);
				if (currentAnswer.correct === self.answerCategory.unchanged) {
					var studentExpressionParsed =
						parser.parseEquationOrExpression(studentExpression);
					var strictestEquality = evaluate.strictestEqualityType(
						studentExpressionParsed, problemDefinition.answer, true);
					if (debugQuad) { console.log('strictestEquality: ' + strictestEquality); }
					var standard = doStandardChecks(problemDefinition, strictestEquality,
						studentExpression, studentExpressionParsed, null);
					currentAnswer = (standard !== null) ? standard :
						generateAnswerAndCommentNotExact(problemDefinition, strictestEquality);
				}
				if (debugQuad) { console.log(currentAnswer); }
				if (currentAnswer.correct > comment.correct) {
					if (debugQuad) { console.log('setting comment'); }
					comment = currentAnswer;
				}
			}
		}
		return comment;
	}

	/**
	 * Makes a quadratic that probably can't be solved easily without the quadratic formula.
	 *
	 * We want to be able to give quadratics with coefficients such that the answers are not complex
	 * (i.e. do not involve taking the square root of a negative number).  Since the roots of a
	 * quadratic equation of the form ax^2 + bx + c are \frac{-b \pm \sqrt{b^2-4ac}}{2b}, the roots
	 * will not be complex iff b^2 - 4ac >= 0.  We also want the students not to be overwhelmed with
	 * factoring the discriminant, since they will need to determine if there are any squares to
	 * take out, so we want to have b^2 - 4ac <= MULT_UPPER_BOUND.
	 *
	 * So we need 0 <= b^2 - 4ac <= MULT_UPPER_BOUND, or 4ac <= b^2 <= MULT_UPPER_BOUND + 4ac.  We
	 * note that b can be positive or negative without affecting these inequalities, so we ignore
	 * the sign of b.  The signs a and c on the other hand definitely affect the numerical values
	 * that can be chosen, so we branch on whether or not the quantity a * c is positive or negative
	 * (see generate(Positive|Negative)QuadraticFormulaCoefficients functions).
	 *
	 * @return {ProblemDefinition}
	 */
	function createQuadraticFormulaProblem() {
		// Get the coefficients.
		var co = (getRandomBool()) ? generatePositiveQuadraticFormulaCoefficients() :
			generateNegativeQuadraticFormulaCoefficients();

		// Create the string to display to the user in the question statement.
		var questionString = co.a + 'x^2+' + co.b + 'x+' + co.c + '=0';
		questionString = parseEvalDisplay(questionString);

		// Now manually do the quadratic formula.  Start with the discriminant since it's used in
		// both.
		var discriminant = '\\sqrt{(' + co.b + ')^2-4*' + co.a + '*' + co.c + '}';
		// Create an array to hold the answers.
		var answers = [
			'(-1*' + co.b + '+' + discriminant + ')/(2*' + co.a + ')',
			'(-1*' + co.b + '-' + discriminant + ')/(2*' + co.a + ')'
		];
		// Compute all the acceptable versions of the answers (and collapse to 1 if there is only
		// one unique root).
		var answers = getAllValidAnswers(answers);

		if (debugQuad) { console.log(answers); }

		var customPrompt = [
			new PromptEntry(false, 'Find the roots of the following quadratic: '),
			new PromptEntry(true, questionString),
			new PromptEntry(false,
				'<div>If there is more than one root, separate them with a comma.</div>')
		];

		var problemDefinition = new ProblemDefinition(questionString, answers[0][0], customPrompt,
			equalityType.commuteCo, // equalityStrictness
			false, // prepopulateAnswer
			null, // customChecks
			true, // multiStep
			[], // history
			true); // useContinue
		problemDefinition.customChecks = function(studentExpression, unused) {
			if (studentExpression === problemDefinition.getLastAttempt()) {
				return new AnswerAndComment(self.answerCategory.unchanged,
					'Nothing changed! Edit the expression to simplify it.');
			}
			var studentExpressions = studentExpression.split(',');
			if (studentExpressions.length > answers.length) {
				return new AnswerAndComment(self.answerCategory.incorrect,
					'Be sure to enter the exact roots with no extra values.');
			}

			var comments = Array(studentExpressions.length);

			for (var k = 0; k < studentExpressions.length; k++) {
				for (var i = 0; i < answers.length; i++) {
					comments[k] = getCommentForAnswerSet(
						answers[i], studentExpressions[k], problemDefinition);
					if (comments[k].correct !== self.answerCategory.incorrect) { break; }
				}
			}
			if (debugQuad) { console.log(comments); }

			if (studentExpressions.length === 1) {
				if (comments[0].correct !== self.answerCategory.incorrect) {
					if (answers.length === 1) { return comments[0]; }
					return new AnswerAndComment(self.answerCategory.intermediate,
						'The answer you entered is one of the roots, but please enter all roots.');
				}
				return new AnswerAndComment(self.answerCategory.incorrect,
					'Your answer is not a root of the quadratic.');
			}

			if (processExpressions.checkFullEquality(
				processExpressions.parseExpressions(studentExpressions))) {
				return new AnswerAndComment(self.answerCategory.intermediate,
					'Make sure to give two different roots.');
			}

			if (comments[0].correct === self.answerCategory.incorrect &&
				comments[1].correct === self.answerCategory.incorrect) {
					return new AnswerAndComment(self.answerCategory.incorrect,
						'Neither of your answers are roots.');
			}

			for (var l = 0; l < comments.length; l++) {
				if (comments[l].correct === self.answerCategory.incorrect) {
					return new AnswerAndComment(self.answerCategory.incorrect,
						'The ' + numToOrdinal(l + 1) + ' answer is not a root.');
				}
			}

			if (comments[0].correct === self.answerCategory.complete &&
				comments[1].correct === self.answerCategory.complete) {
				return comments[0];
			}

			if (comments[0].correct === self.answerCategory.intermediate &&
				comments[1].correct === self.answerCategory.intermediate) {
				return new AnswerAndComment(self.answerCategory.intermediate,
					'Great, continue simplifying your answers.');
			}

			var comment = '';
			for (var m = 0; m < comments.length; m++) {
				if (comments[m].correct === self.answerCategory.complete) {
					comment += 'The ' + numToOrdinal(m + 1) + ' answer is fully simplified. ';
				} else {
					comment += 'The ' + numToOrdinal(m + 1) + ' can be further simplified. ';
				}
			}
			return new AnswerAndComment(self.answerCategory.intermediate, comment);
		};
		return problemDefinition;
	}

	/**
	 * Generates coefficients for a quadradic in which a * c is negative.
	 * @return {Object} Contains coefficients a, b, c for which a * c < 0, and
	 *                  0 <= b^2 - 4ac <= MULT_UPPER_BOUND
	 */
	function generateNegativeQuadraticFormulaCoefficients() {
		// We want ac to be negative, so we automatically get b^2 - 4ac > 0, since ac < 0, thus
		// 4ac < 0, or -4ac > 0, and we know b^2 > 0 by virtue of squaring, so b^2 + -4ac >0.
		// So, we just need to make sure that b^2 - 4ac <= MULT_UPPER_BOUND.
		// So we want b^2 <= 4ac + MULT_UPPER_BOUND, and we know b^2 will always be >= 0 for any
		// value of b, which means we need to pick a and c so that a * c < 0 and
		// MULT_UPPER_BOUND + 4ac >= 0, then pick b so that
		// abs(b) <= \sqrt{MULT_UPPER_BOUND + 4ac}

		// We generally don't want to make them multiply numbers bigger than TERM_UPPER_BOUND, so
		// since TERM_UPPER_BOUND < MULT_UPPER_BOUND/4, just pick anything less than
		// TERM_UPPER_BOUND away from 0.
		var c = getMaybeNegativeInt(TERM_UPPER_BOUND);
		// We want MULT_UPPER_BOUND + 4ac >= 0 so we need a <= -MULT_UPPER_BOUND/(4c), and we know
		// either a or c is less than 0, so this is the same as saying that we need
		// abs(a) <= abs(MULT_UPPER_BOUND/(4c)).  Again, don't let it be bigger than
		// TERM_UPPER_BOUND no matter what.
		var a = getRandomInt(1, Math.min(Math.abs(MULT_UPPER_BOUND / (4 * c)), TERM_UPPER_BOUND));
		// Now that we know abs(a), we decide if if should be negative based on whether or not c is
		// negative, to ensure 4ac < 0 (since without that, all our scheming has been mathematically
		// unsound!)
		a = (c < 0) ? a : -1 * a;
		// Now pick a b smaller than \sqrt{MULT_UPPER_BOUND + 4ac}
		var b = getMaybeNegativeInt(
			Math.min(Math.sqrt(MULT_UPPER_BOUND + 4 * a * c), TERM_UPPER_BOUND));

		if (debugQuad) { console.log('pos a: ' + a + ', b: ' + b + ', c: ' + c); }
		return {'a' : a, 'b' : b, 'c' : c};
	}

	/**
	 * Generates coefficients for a quadradic in which a * c is positive.
	 * @return {Object} Contains coefficients a, b, c for which a * c > 0, and
	 * 0 <= b^2 - 4ac <= MULT_UPPER_BOUND
	 */
	function generatePositiveQuadraticFormulaCoefficients() {
		// We want 4ac to be positive, and we need b^2 >= 4ac so tha we have have real solutions,
		// so we need to pick b before we can figure out viable options for a and c.  We don't like
		// making them multiply things bigger than MULT_UPPER_BOUND so we limit b to be in that
		// range.
		// Since b^2 <= MULT_UPPER_BOUND, and we are picking a and c so that 0 < 4ac <= b^2,
		// no need to separately worry about the discriminant being too big to factor.  Since we do
		// want all three to be integers, we need to make sure that b^2/4 >= 1, so abs(b) >= 2.
		// Summarizing: need to pick b so 2 <= abs(b) <= sqrt(MULT_UPPER_BOUND) = TERM_UPPER_BOUND
		var b = maybeMakeNegative(getRandomInt(2, TERM_UPPER_BOUND));
		// Now we need to make sure ac <= b^2/4
		var limit = Math.pow(b, 2)/4;
		// Also never want things we multiply to be bigger than TERM_UPPER_BOUND.
		var c = getMaybeNegativeInt(Math.min(limit, TERM_UPPER_BOUND));
		// Now we need abs(a) <= abs(b^2/(4c))
		var a = getRandomInt(1, Math.min(Math.abs(limit/c), TERM_UPPER_BOUND));
		// if c was negative, a has to be as well so that 4ac > 0
		a = (c < 0) ? -1 * a : a;

		if (debugQuad) { console.log('pos a: ' + a + ', b: ' + b + ', c: ' + c); }
		return {'a' : a, 'b' : b, 'c' : c};
	}


	/**
	 * This function creates a simple arithmetic expression with integers. Addition/subtraction
	 * problems contain numbers in the range 0 to 100, multiplication/division problems use
	 * factors from 1 to TERM_UPPER_BOUND.
	 * @param  {Boolean} negative   If true, make each operand negative with 50% probability.
	 * @param  {Boolean} fraction   If true, make both operands into unsimplified fractions, and
	 *                              restrict all multiplication/division to be in the
	 *                              MULT_UPPER_BOUND range.
	 * @return {Expression}
	 */
	function createRandomBinaryNumericExpressionHelper(negative, fraction, ops) {
		if (typeof ops === 'undefined') { ops = addSubMultDiv; }
		var op = getRandomElementFromArray(ops);
		var leftValue = getRandomInt(1, TERM_UPPER_BOUND);
		var rightValue = getRandomInt(1, TERM_UPPER_BOUND);
		if (getPrecedence(op) === 1 && !fraction) {
			leftValue = getRandomInt(0, 100);
			rightValue = getRandomInt(0, 100);
		}
		if (negative) {
			leftValue = maybeMakeNegative(leftValue);
			rightValue = maybeMakeNegative(rightValue);
		}
		if (fraction) {
			leftValue = fractionUtils.createFractionValue(
				leftValue, getRandomInt(1, TERM_UPPER_BOUND));
			rightValue = fractionUtils.createFractionValue(
				rightValue, getRandomInt(1, TERM_UPPER_BOUND));
		}
		if (op === '/') {
			leftValue = operatorProperties['*'].evaluateValues(leftValue, rightValue).value;
		}
		return expr.createCompoundExpression(
			expr.createSimpleExpression('number', leftValue),
			expr.createSimpleExpression('number', rightValue),
			op);
	}

	/**
	 * Calls the helper function to create the question, then uses that to create the
	 * ProblemDefinition.
	 * @param  {Boolean} negative   If true, make each operand negative with 50% probability.
	 * @param  {Boolean} fraction   If true, make both operands into unsimplified fractions, and
	 *                              restrict all multiplication/division to be in the
	 *                              MUL_UPPER_BOUND range.
	 * @return {ProblemDefinition}
	 */
	function createRandomBinaryNumericExpression(negative, fraction, ops) {
		return computeAnswerFromQuestion(
			createRandomBinaryNumericExpressionHelper(negative, fraction, ops),
			fraction);
	}

	/**
	 * This function creates a multiplication expression with three or four integer
	 * multiplicands in the range -4 to 4 (not including 0).
	 * @return {ProblemDefinition}
	 */
	function createMultiplicationWithMoreMultiplicands() {
		var question = expr.createCompoundExpression(
			expr.createCompoundExpression(
				expr.createSimpleExpression('number', getMaybeNegativeInt(4)),
				expr.createSimpleExpression('number', getMaybeNegativeInt(4)),
				'*'),
			expr.createSimpleExpression('number', getMaybeNegativeInt(4)),
			'*');
		if (getRandomBool()) {
			question = expr.createCompoundExpression(
				question, expr.createSimpleExpression('number', getMaybeNegativeInt(4)), '*');
		}
		return computeAnswerFromQuestion(question, false);
	}

	/**
	 * Creates a simple pair.
	 * @param {Boolean} text  True if the value is MathQuill, false if simple text.
	 * @param {String} value  The string value, either text or LaTeX to be rendered by MathQuill.
	 */
	function PromptEntry(math, value) {
		this.math = math;
		this.value = value;
	}

	/**
	 * Creates a decimal rounding problem.
	 * @return {ProblemDefinition}
	 */
	function createRoundingProblem() {
		var digitsToLeft = getRandomInt(0, 3);
		var digitsToRight = getRandomInt(1, 100000);
		var questionString = '' + digitsToLeft + '.' + digitsToRight;
		var numDigitsToRoundTo = getRandomInt(1, 3);

		var customPrompt = [
			new PromptEntry(false, 'Round '),
			new PromptEntry(true, questionString),
			new PromptEntry(false, ' to '),
			new PromptEntry(true, '' + numDigitsToRoundTo),
			new PromptEntry(false, '  decimal place(s).')
		];

		var number = parseFloat(questionString, 10);
		var answerString = number * Math.pow(10, numDigitsToRoundTo);
		answerString = '' + Math.round(answerString);
		var position = answerString.length - numDigitsToRoundTo;
		var preDecimal = answerString.substring(0, position);
		if (preDecimal === '') { preDecimal = '0'; }
		answerString = preDecimal + '.' + answerString.substring(position);
		var answer = parser.parseEquationOrExpression(answerString);

		var problemDefinition = new DefaultProblemDefinition(questionString, answer, customPrompt);
		problemDefinition.customChecks = function(studentExpression) {
			return doAllNormalChecks(problemDefinition, studentExpression,
				/**
				 * Callback to use for more stringent tests for rounding, to be called if the answer
				 * is technically correct but may not match exactly.
				 * @param  {String} answerString            The actual exact answer.
				 * @param  {String} studentExpression       What the student typed.
				 * @return {AnswerAndComment}               Contains the self.answerCategory along
				 *                                          with the comment to present to the user.
				 */
				function(studentExpression) {
					if (studentExpression !== answerString) {
						if (studentExpression.charAt(0) === '.') {
							return new AnswerAndComment(self.answerCategory.intermediate,
								'Correct, but the expression can be written in a more standard way.'
								);
						} else {
							return new AnswerAndComment(self.answerCategory.intermediate,
								'Correct, but all specified decimal places should be filled.');
						}
					}
					return new CorrectAnswerAndComment();
				});
		};
		problemDefinition.useContinue = false;
		return problemDefinition;
	}

	/**
	 * Create problems of the form 3x^2*y -2x^3y^2 for x = -2 and y = 3
	 * @return {ProblemDefinition}
	 */
	function createXYSubstituteProblem() {
		var xNode = expr.createSimpleExpression('identifier', 'x');
		var yNode = expr.createSimpleExpression('identifier', 'y');
		var expressionString = createTermWithVariablesAndCoefficient(['x', 'y']) + '+' +
			createTermWithVariablesAndCoefficient(['x', 'y']);
		var question = parser.parseEquationOrExpression(expressionString);
		question = evaluate.evaluateRec(question);
		// If it came out to 0, generate a new one.
		if (question.value === 0) {
			createXYSubstituteProblem();
			return;
		}
		var xValue = getMaybeNegativeInt(3);
		var yValue = getMaybeNegativeInt(3);
		var questionString = display.displayExpression(question);
		var answer = evaluate.substituteNodeForOtherNode(
			question, xNode, expr.createSimpleExpression('number', xValue));
		answer = evaluate.substituteNodeForOtherNode(
			answer, yNode, expr.createSimpleExpression('number', yValue));
		answer = evaluate.evaluateRec(answer);

		var customPrompt = [
			new PromptEntry(false, 'Evaluate '),
			new PromptEntry(true, questionString),
			new PromptEntry(false, ' for '),
			new PromptEntry(true, 'x=' + xValue),
			new PromptEntry(false, ' and '),
			new PromptEntry(true, 'y=' + yValue)
		];

		var problem = new DefaultProblemDefinition(questionString, answer, customPrompt);
		problem.prepopulateAnswer = false;
		return problem;
	}


	/**
	 * Creates a problem with a single variable, picked at random from the options listed
	 * in the identifiers string.
	 * @return {ProblemDefinition}
	 */
	function createSimplifyPolynomialProblem() {
		var identifier = getRandomCharInString(identifiers);
		var terms = [];
		for (var i = getRandomInt(2,4); i >= 1; i--) {
			terms.push(identifier + '^' + i);
		}
		return createPolynomialSimplificationWithTermSeeds(terms, additionAndSubtraction);
	}

	/**
	 * Makes a multiplication problem 1/5 of the time, restricting to at most
	 * (2 terms) * (3 terms), where each term uses the variables given, each generated separately.
	 *
	 * The rest of the time, make an addition/subtraction problem with 2 to 4 terms, same terms
	 * in both sides.
	 * @param  {Array<String>} vars The variables to use as identifiers when creating terms.
	 * @return {ProblemDefinition}
	 */
	function createSimplifyPolynomialProblemMultipleIdentifiers(vars) {
		var problem = null;
		if (getRandomInt(0, 4) === 0) {
			var low = 0;
			var high = 2;
			var num = getRandomInt(low, high);
			var left = evaluate.evaluateRec(parser.parseEquationOrExpression(
				createExpressionString(generateTermFromVars(vars, num))));
			if (num === 0) { low = 1; }
			if (num === 2) { high = 1; }
			var right = evaluate.evaluateRec(parser.parseEquationOrExpression(
				createExpressionString(generateTermFromVars(vars, getRandomInt(low, high)))));
			problem = createPolynomialSimplificationWithTerms(left, right, '\\cdot');
		} else {
			var terms = generateTermFromVars(vars, getRandomInt(1, 3));
			problem = createPolynomialSimplificationWithTermSeeds(terms, additionAndSubtraction);
		}
		problem.equalityStrictness = equalityType.commuteCo;
		return problem;
	}

	/**
	 * Helper for checkExpression, called if the "correct" check failed, to break down what
	 * feedback we should give given their answer.
	 *
	 * Also called by customChecks in some cases where we had to make the equality strictness
	 * looser than actually desired in order to have customChecks be called at all.
	 *
	 * @param  {ProblemDefinition} problemDefinition The object containing all the info about the
	 *                                               problem that was presented to the user.
	 * @param  {equalityType} strictestEquality      The equality strictness that the student's
	 *                                               answer fell into compared to the real answer.
	 * @return {AnswerAndComment}                    The object describing what feedback we should
	 *                                               give the user based on their answer.
	 */
	function generateAnswerAndCommentNotExact(problemDefinition, strictestEquality) {
		// If it's a problem in which they are allowed to enter multiple steps, check if
		// what they entered is at all the same as what we're going for.
		if (problemDefinition.multiStep && strictestEquality < equalityType.none) {
			var comment = 'Great! Keep going!';
			// If it's the first time, give a custom message.
			if (problemDefinition.history.length === 0) {
				comment = 'Good start! Keep simplifying.';
			}
			// If they are super close, give a comment that might help a bit more in figuring
			// out why the answer they gave is not accepted.
			if (strictestEquality <= equalityType.commute) {
				comment = 'Correct, but please use standard form for your expression.';
			}
			return new AnswerAndComment(self.answerCategory.intermediate, comment);
		}
		// Not the same. Boo.
		return new AnswerAndComment(self.answerCategory.incorrect,
			'This is not equivalent. Try again!');
	}

	function doErrorChecks(problemDefinition, studentExpression, studentExpressionParsed) {
		// First see if there's anything technically wrong with what they submitted.
		if (studentExpressionParsed === errorNode || studentExpressionParsed === null) {
			return new AnswerAndComment(self.answerCategory.incorrect, 'Your response could ' +
				'not be graded. It may contain a typing mistake, please check for errors.');
		} else if (studentExpression === problemDefinition.getLastAttempt()) {
			return new AnswerAndComment(self.answerCategory.unchanged,
				'Nothing changed! Edit the expression to simplify it.');
		}
		return null;
	}

	function doStandardChecks(problemDefinition, strictestEquality, studentExpression,
		studentExpressionParsed, additionalChecks) {
		if (strictestEquality <= problemDefinition.equalityStrictness) {
			// It's at least as close as we require, but we may have nitpicky issues with
			// how they entered it, so we make any custom checks here.
			if (additionalChecks !== null) {
				return additionalChecks(studentExpression, studentExpressionParsed);
			}
			// If the problem doesn't come with specific checks, do a generic check to make
			// sure that the thing they entered comes back the same after running it through
			// my parser/display system (no evaluation).
			if (studentExpression !==
				display.displayExpression(parser.parseEquationOrExpression(studentExpression))) {
				return new AnswerAndComment(self.answerCategory.intermediate,
					'Correct, but the expression can be written in a more standard way.');
			}
			// Survived all the checks, so we're good! They got it right! Yay!
			return new CorrectAnswerAndComment();
		}
		return null;
	}

	function doAllNormalChecks(problemDefinition, studentExpression, additionalChecks) {
		// This takes the latex pulled from the mathquill box and parses it into an expression.
		var studentExpressionParsed = parser.parseEquationOrExpression(studentExpression);

		if (debugNormalChecks) {
			console.log('after parse, student: ' +
				display.displayExpression(studentExpressionParsed) +
				' and answer: ' + display.displayExpression(problemDefinition.answer));
		}
		// First see if there's anything technically wrong with what they submitted.
		var errors = doErrorChecks(problemDefinition, studentExpression, studentExpressionParsed);
		if (errors !== null) { return errors; }
		if (debugNormalChecks) { console.log('after error checks'); }

		if (debugNormalChecks) {
			console.log(studentExpressionParsed);
			console.log(problemDefinition.answer);
		}
		// Now figure out how close the student was to the cannonical answer
		var strictestEquality = evaluate.strictestEqualityType(
			studentExpressionParsed, problemDefinition.answer, true);
		if (debugNormalChecks) { console.log('strictestEquality: ' + strictestEquality); }

		var standard = doStandardChecks(problemDefinition, strictestEquality, studentExpression,
			studentExpressionParsed, additionalChecks);
		if (debugNormalChecks) {
			console.log('standard: ');
			console.log(standard);
		}
		if (standard !== null) { return standard; }

		return generateAnswerAndCommentNotExact(problemDefinition, strictestEquality);
	}
	/**
	 * Takes the student input and determines the validity of the answer.
	 * @param  {String} studentExpression LaTeX string pulled from mathquill-ified element.
	 * @return {AnswerAndComment}         Object contains:
	 *                                           correct An answerCategory representing the
	 *                                                   correctness of the answer.
	 *                                           comment The string comment to be displayed.
	 */
	function checkExpressionsHelper(studentExpression, problemDefinition) {
		if (problemDefinition === null) {
			// should only happen if invalid type was passed.
			console.error('There is no question.');
			return new AnswerAndComment(self.answerCategory.incorrect, 'No question present.');
		}

		if (problemDefinition.customChecks !== null) {
			return problemDefinition.customChecks(studentExpression);
		}

		return doAllNormalChecks(problemDefinition, studentExpression, null);
	}

	/**
	 * Converts the english word for the operation into an array containing that operation.
	 * @param  {String}        str The word for the operation.
	 * @return {Array<String>}     Array containing the operation as it should be passed to the
	 *                             parser.  If it's not one of the recognized words, just allow all
	 *                             four simple operations.
	 */
	function getOpsFromString(str) {
		switch (str) {
			case 'addition' : return ['+'];
			case 'subtraction' : return ['-'];
			case 'multiplication' : return ['\\cdot'];
			case 'division' : return ['/'];
		}
		return addSubMultDiv;
	}

	/**
	 * Maps from quiz type to arg-free or single-arg function that creates and returns a
	 * problemDefinition corresponding to the appropriate problem type.
	 * @type {Map}
	 */
	var quizGenerator = {
		'positive' : function() {
			return createRandomBinaryNumericExpression(false, false);
		},
		'negative' : function() {
			if (getRandomBool(5, 1)) {
				return createMultiplicationWithMoreMultiplicands();
			} else {
				return createRandomBinaryNumericExpression(true, false);
			}
		},
		'fraction' : function(opString) {
			return createRandomBinaryNumericExpression(true, true, getOpsFromString(opString));
		},
		'rounding' : createRoundingProblem,
		'sub' : createXYSubstituteProblem,
		'polynomial' : createSimplifyPolynomialProblem,
		'polyXY' : function() {
			return createSimplifyPolynomialProblemMultipleIdentifiers(['x', 'y']);
		},
		'solve' : createSolveForVariableProblem,
		'order' : createOrderOfOperationsProblem,
		'factor' : createQuadraticFactoringProblem,
		'quad' : createQuadraticFormulaProblem
	};

	var self = {
		/**
		 * Enum describing the possible answer types.
		 * @type {Object}
		 */
		answerCategory : {
			'incorrect' : 0,
			'unchanged' : 1,
			'intermediate' : 2,
			'complete' : 3
		},


		/**
		 * Takes a quizType in the form type_subtype, and gets the function to generate a quiz of
		 * type type, then if there is a subtype, call that function with arg subtype.
		 *
		 * If subtype is 'integer', just call the no-arg version -- the exception is because I
		 * created the positive_integer quiz type before I made up this naming convention, and
		 * did not want to retroactively change it.
		 *
		 * @param  {String} quizType String of the form type_subtype
		 * @return {function}        An arg-free function that creates and returns a
		 *                           problemDefinition corresponding to the appropriate problem
		 *                           type.
		 */
		getQuizGenerator : function(quizType) {
			var arr = quizType.split('_');
			var func = quizGenerator[arr[0]];
			if (arr.length === 2 && arr[2] !== 'integer') {
				return function() { return func(arr[1]); };
			}
			return func;
		},

		/**
		 * Takes the student input and determines the validity of the answer.
		 * @param  {String} studentExpression LaTeX string pulled from mathquill-ified element.
		 * @return {Object}                   Object contains:
		 *                                           correct A self.answerCategory representing
		 *                                                   the correctness of the answer.
		 *                                           comment The string comment to be displayed.
		 */
		checkExpressions: function(studentExpression, problemDefinition) {
			return checkExpressionsHelper(studentExpression, problemDefinition);
		}
	};
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.quizUtils = quizUtils;
}