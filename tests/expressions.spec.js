/*global process:true require:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var path = '../src';
	var display = require(path + '/display.js').display;
	var parenMode = require(path + '/display.js').parenMode;
	var outputType = require(path + '/display.js').outputType;
	var divSign = require(path + '/display.js').divSign;
	var evaluate = require(path + '/evaluation.js').evaluate;
	var equalityType = require(path + '/evaluation.js').equalityType;
	var equality = require(path + '/equality.js').equality;
	var parser = require(path + '/parser.js').parser;
	var errorNode = require(path + '/utils.js').errorNode;
	var expr = require(path + '/expr.js').expr;
	var processExpressions = require(path + '/processExpressions.js').processExpressions;
	var nodeWrapper = require(path + '/nodeWrapper.js').nodeWrapper;
	var expression = require(path + '/expression.js').expression;
}

describe("WillowJS tests", function() {
	// Can't decide if I like having this or not.
//	jasmine.getEnv().addReporter(new jasmine.ConsoleReporter(console.log));


	// Keep track of how many errors we think should've come up over the course of the test,
	// and at the end make sure we were right.  Normally, there should not be any errors, but
	// some tests purposefully make certain errors happen, and those should be checked within
	// the test to make sure it was the right error.
	var expectedErrors;

	beforeEach(function() {
		expectedErrors = 0;
		spyOn(console, 'error');
	});

	afterEach(function() {
		expect(console.error.calls.length).toEqual(expectedErrors);
	});

	// Helpers for the first few tests.
	function testNodeWrapper(command, expressionStringArray, expectedResult) {
		var parsedExpessions = processExpressions.parseExpressions(expressionStringArray);
		expect(nodeWrapper.runCommand(command, parsedExpessions, false, false))
			.toBe(expectedResult);
	}
	function testParseAndDisplay(
		eqString, resFull, resTerms, resNec, type, parseFunction, showDiv) {
		if (typeof showDiv === 'undefined') { showDiv = divSign.never; }
		if (typeof type === 'undefined') { type = outputType.text; }
		if (typeof parseFunction === 'undefined') { parseFunction = parser.parseEquation; }
		expect(display.displayExpression(parseFunction(eqString),
			type, parenMode.full, showDiv)).toBe(resFull);
		expect(display.displayExpression(parseFunction(eqString),
			type, parenMode.terms, showDiv)).toBe(resTerms);
		expect(display.displayExpression(parseFunction(eqString),
			type, parenMode.necessary, showDiv)).toBe(resNec);
	}

	it("Manual construction and display test", function() {
		var xExpr = expression.createSimpleExpression('identifier', 'x', 1);
		xExpr = expression.createCompoundExpression(expression.createCompoundExpression(
			expression.createSimpleExpression('number', 1, 1), xExpr, '+', 1),
			expression.createSimpleExpression('number',3, 1),
			'*');
		var lhs = expression.createCompoundExpression(
			expression.createSimpleExpression('number', 4, 1), xExpr, '+', 1);
		var rhs = expression.createSimpleExpression('number', 5, 1);
		var eq = expression.createCompoundExpression(lhs, rhs, '=', 1);

		expect(display.displayExpression(eq, outputType.text, parenMode.necessary))
			.toBe('4+(1+x)3=5');
		expect(display.displayExpression(eq, outputType.mathml, parenMode.necessary)).toBe(
			'<math xmlns="http://www.w3.org/1998/Math/MathML" id=node9><mrow id=node6>' +
			'<mn id=node5>4</mn><mo class="op" id=node6>+</mo><mrow id=node4><mfenced id=node2>' +
			'<mrow><mn id=node1>1</mn><mo class="op" id=node2>+</mo><mi id=node0>x</mi></mrow>' +
			'</mfenced><mn id=node3>3</mn></mrow></mrow><mo class="op" id=node8>=</mo>' +
			'<mn id=node7>5</mn></math>');
	});
	it("Parse and display paren mode necessary test", function() {
		// test order of operations and removal of whitespace
		testParseAndDisplay('x=5 + (4 +5)', 'x=5+(4+5)', 'x=5+(4+5)', 'x=5+4+5');
		testParseAndDisplay('x=5 \\:+\\: (4 +5)', 'x=5+(4+5)', 'x=5+(4+5)', 'x=5+4+5');
		testParseAndDisplay('x=5 - (4-5)', 'x=5-(4-5)', 'x=5-(4-5)', 'x=5-(4-5)');
		testParseAndDisplay('x=5 - (4+5)', 'x=5-(4+5)', 'x=5-(4+5)', 'x=5-(4+5)');
		testParseAndDisplay('x=5+(4-5)', 'x=5+(4-5)', 'x=5+(4-5)', 'x=5+4-5');
		testParseAndDisplay('x=(4-5)+5', 'x=(4-5)+5', 'x=(4-5)+5', 'x=4-5+5');
		testParseAndDisplay('x=(4/5)*5', 'x=4/5*5', 'x=4/5*5', 'x=4/5*5');
		testParseAndDisplay('x=(y/x)*5', 'x=(y/x)*5', 'x=(y/x)5', 'x=y/x5');
		testParseAndDisplay('x=4/(5*5)', 'x=4/(5*5)', 'x=4/(5*5)', 'x=4/(5*5)');
		testParseAndDisplay('x=4/(5-5)', 'x=4/(5-5)', 'x=4/(5-5)', 'x=4/(5-5)');
		testParseAndDisplay('x=(4/5)-5', 'x=4/5-5', 'x=4/5-5', 'x=4/5-5');
		testParseAndDisplay('x=(x/y)-5', 'x=(x/y)-5', 'x=x/y-5', 'x=x/y-5');
		testParseAndDisplay('x=(4+5)*5', 'x=(4+5)*5', 'x=(4+5)5', 'x=(4+5)5');
		testParseAndDisplay('x=4+(5*5)', 'x=4+(5*5)', 'x=4+5*5', 'x=4+5*5');

		// start with a negative number, expronent
		testParseAndDisplay('x=-4+(5*5)^2', 'x=(-4)+((5*5)^2)', 'x=-4+(5*5)^2', 'x=-4+(5*5)^2');
		// TODO(sdspikes): maybe make * disappear since these are both numbers, not unary
		testParseAndDisplay('x=-4+(-5*-5)',
			'x=(-4)+((-5)*(-5))', 'x=-4+(-5)*(-5)', 'x=-4+(-5)*(-5)');
		testParseAndDisplay('x=-4+(-a)*(-b)*x',
			'x=(-4)+(((-a)*(-b))*x)', 'x=-4+(-a(-b))x', 'x=-4+(-a)(-b)x');
		testParseAndDisplay('x=-4+(-a)*(-b)x',
			'x=(-4)+(((-a)*(-b))*x)', 'x=-4+(-a(-b))x', 'x=-4+(-a)(-b)x');
		testParseAndDisplay('x(4+3)y=3', '(x*(4+3))*y=3', '(x(4+3))y=3', 'x(4+3)y=3');
		testParseAndDisplay('3x+4y=7', '(3*x)+(4*y)=7', '3x+4y=7', '3x+4y=7');
		testParseAndDisplay('(-f)/5=8', '(-f)/5=8', '-f/5=8', '-f/5=8');
		testParseAndDisplay('(f^x)^2 + x^(y^2)=8',
			'((f^x)^2)+(x^(y^2))=8', '(f^x)^2+x^(y^2)=8', 'f^x^2+x^(y^2)=8');
		testParseAndDisplay('2--(6)=3', '2-(-6)=3', '2-(-6)=3', '2-(-6)=3');
		testParseAndDisplay('x=-2--6', 'x=(-2)-(-6)', 'x=-2-(-6)', 'x=-2-(-6)');
		testParseAndDisplay('x=-2-(-6+3)', 'x=(-2)-((-6)+3)', 'x=-2-(-6+3)', 'x=-2-(-6+3)');
		testParseAndDisplay('x=-2-(-6*3)', 'x=(-2)-((-6)*3)', 'x=-2-(-6)*3', 'x=-2-(-6)*3');
		testParseAndDisplay('x=(-2)^2(-3)^2-(-6)^2(-2)^2',
			'x=(((-2)^2)*((-3)^2))-(((-6)^2)*((-2)^2))',
			'x=(-2)^2*(-3)^2-(-6)^2*(-2)^2', 'x=(-2)^2*(-3)^2-(-6)^2*(-2)^2');
	});
	// test each error-producing line independently so we can be sure which error went with which
	// call.
	it("Mismatched parens", function() {
		expect(parser.parseEquation('x=-4+(-5*-5')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith('Mismatched parentheses!');
		expectedErrors++;
	});
	it("Extra chars", function() {
		expect(parser.parseEquation('x=-4+(5*-5))')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith('Expression contained extra characters: )');
		expectedErrors++;
	});
	it("Should be equation", function() {
		expect(parser.parseEquation('23x')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith(
			'Please enter an equation, not an expression (should contain an "=" or inequality).');
		expectedErrors++;
	});
	it("parens around equation (not expression)", function() {
		// This errors out because the first thing the parser does is to look for = or inquality
		// and then parses each side separately.  Up for debate whether that's the best behaviour.
		expect(parser.parseEquation('((x)3=4)')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith('Mismatched parentheses!');
		expect(console.error).toHaveBeenCalledWith('Expression contained extra characters: )');
		expectedErrors = 2;
	});
	it("close paren before open paren", function() {
		expect(parser.parseEquation(')re +43=3')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith('Badly formed identifier: )');
		expectedErrors++;
	});
	it("too many operators in a row", function() {
		expect(parser.parseEquation('2+=3')).toBe(errorNode);
		expect(console.error).toHaveBeenCalledWith('Badly formed identifier: ');
		expectedErrors++;
	});
	// more helpers for the tests that follow
	function testEvaluateEquation(eqString, expectedResult) {
		var evaluatedExpression = evaluate.evaluateFull(parser.parseEquation(eqString));
		testEvaluateEvaluated(evaluatedExpression, expectedResult, parenMode.terms);
	}
	function testEvaluate(eqString, expectedResult, mode) {
		var evaluatedExpression = evaluate.evaluateFull(parser.parseEquationOrExpression(eqString));
		if (typeof mode === 'undefined') { mode = parenMode.terms; }
		testEvaluateEvaluated(evaluatedExpression, expectedResult, mode);
	}
	function testSolveForVariable(exprString, varString, expectedResult) {
		var parsedExpression = parser.parseEquationOrExpression(exprString);
		var parsedVar = parser.parseEquationOrExpression(varString);
		var solvedExpression = evaluate.solveForIdentifier(parsedExpression, parsedVar);
		for (var i = 0; i < expectedResult.length && i < solvedExpression.length; i++) {
			testEvaluateEvaluated(solvedExpression[i], expectedResult[i], parenMode.necessary);
		}
		if (expectedResult.length !== solvedExpression.length) {
			expect('length' + solvedExpression.length).toBe('length' + expectedResult.length);
		}
	}
	function testEvaluateEvaluated(evaluatedExpression, expectedResult, mode) {
		if (expectedResult === errorNode || evaluatedExpression === errorNode ||
			expectedResult === null || evaluatedExpression === null) {
			expect(evaluatedExpression).toBe(expectedResult);
		} else {
			expect(display.displayExpression(evaluatedExpression, outputType.text, mode))
				.toBe(expectedResult);
		}
	}
	function indexToType(num) {
		switch (num) {
			case 0 : return 'verbatim';
			case 1 : return 'commuteCo';
			case 2 : return 'commute';
			case 3 : return 'full';
			case 4 : return 'fullEq';
		}
	}
	function testEqualityBreakdown(eqString1, eqString2, value) {
		var evaluatedExpression1 = parser.parseEquationOrExpression(eqString1);
		var evaluatedExpression2 = parser.parseEquationOrExpression(eqString2);
		if (evaluatedExpression1 === null || evaluatedExpression1 === errorNode ||
			evaluatedExpression2 === null || evaluatedExpression2 === errorNode) {
			expect(evaluatedExpression1).toBe(evaluatedExpression2);
			return;
		}
		var expressions = [evaluatedExpression1, evaluatedExpression2];

		var result = processExpressions.strictestEquality(expressions, true)[0];

		expect(result).toBe(value);
	}
	it("simple evaluation", function() {
		testEvaluate('x=3+2', 'x=5');
		testEvaluate('x=(84-2)+2', 'x=84');
		testEvaluate('(3-2)+2=x', '3=x');
		testEvaluate('(3*2)^2', '36');
		testEvaluate('x=3*2^2', 'x=12');
		testEvaluate('5/4*4', '5');
		testEvaluate('x=-5/3*6', 'x=-10');
		testEvaluate('2(x-3)/2 + 3', 'x');
	});
	function testEvaluateLeftSubExpression(eqString, expectedResult) {
		var exp = parser.parseEquation(eqString);
		var result = evaluate.evaluateFull(exp.lhs);
		if (result !== null) { exp.lhs = result; }
		expect(display.displayExpression(exp, outputType.text, parenMode.terms))
			.toBe(expectedResult);
	}
	it("evaluate left (make sure evaluating a subexpression changes the expression)", function() {
		testEvaluateLeftSubExpression('x=3+2', 'x=3+2');
		testEvaluateLeftSubExpression('(84-2)+2=x', '84=x');
		testEvaluateLeftSubExpression('(3-2)+2=x-2', '3=x-2');
		testEvaluateLeftSubExpression('x=(3*2)^2', 'x=(3*2)^2');
	});
	function testCommuteRightSubExpression(eqString, expectedResult) {
		var exp = parser.parseEquation(eqString);
		expr.commute(exp.rhs);
		expect(display.displayExpression(exp, outputType.text, parenMode.full))
			.toBe(expectedResult);
	}
	it("commute the terms on the left", function() {
		testCommuteRightSubExpression('x=3+2', 'x=2+3');
		testCommuteRightSubExpression('x=(84-2)+2', 'x=2+(84-2)');
		testCommuteRightSubExpression('x+2=x-2', 'x+2=(-2)+x');
		testCommuteRightSubExpression('x=(3*2)^2', 'x=(3*2)^2');
		testCommuteRightSubExpression('x=(4*7)', 'x=7*4');
		testCommuteRightSubExpression('x=(4/7)', 'x=4/7'); // nothing to commute, single exp
		testCommuteRightSubExpression('x=(x/y)', 'x=(1/y)*x');
	});
	function testAssociateRightSubExpression(eqString, expectedResult, side) {
		var exp = parser.parseEquation(eqString);
		if (side === 'left') { expr.associate(exp.rhs.lhs); }
		else { expr.associate(exp.rhs.rhs); }
		expect(display.displayExpression(exp, outputType.text, parenMode.full))
			.toBe(expectedResult);
	}
	it("associate the terms on the right", function() {
		testAssociateRightSubExpression('x=((3+2)+4)', 'x=3+(2+4)', 'left');
		testAssociateRightSubExpression('x=3+(2+4)', 'x=(3+2)+4', 'right');
		testAssociateRightSubExpression('x=(84-2)+2', 'x=84+((-2)+2)', 'left');
		testAssociateRightSubExpression('x+2=(x-2)-y', 'x+2=x+((-2)-y)', 'left');
		testAssociateRightSubExpression('x=x-(2-y)', 'x=x-(2-y)', 'right');
		testAssociateRightSubExpression('x=(3*2)/2', 'x=3*(2/2)', 'left');
		testAssociateRightSubExpression('x=(3*2)^2', 'x=(3*2)^2', 'left');
		testAssociateRightSubExpression('x=(3^2)^2', 'x=(3^2)^2', 'left');
		testAssociateRightSubExpression('x=(4*7)*2', 'x=4*(7*2)', 'left');
		testAssociateRightSubExpression('x=4*(7*2)', 'x=(4*7)*2', 'right');
		testAssociateRightSubExpression('x=(x/y)/z', 'x=x*((1/y)/z)', 'left');
	});
	it("evaluate expressions with identities", function() {
		testEvaluate('x=y+0', 'x=y');
		testEvaluate('x=0+y', 'x=y');
		testEvaluate('x=y-0', 'x=y');
		testEvaluate('x=(y+3)-0', 'x=y+3');
		testEvaluate('x=0+y-0', 'x=y');
		testEvaluate('x=y*1', 'x=y');
		testEvaluate('x=1*y', 'x=y');
		testEvaluate('x=1*y*1', 'x=y');
		testEvaluate('x=y/1', 'x=y');
		testEvaluate('x=1/y', null); // nothing to do
		testEvaluate('x=0-y', 'x=-y');

		testEvaluate('x=y*0', 'x=0');
		testEvaluate('x=0*y', 'x=0');
		testEvaluate('x=0*y*0', 'x=0');
		testEvaluate('x=0/y', 'x=0');
		testEvaluate('x=0^y', 'x=0');
		testEvaluate('x=y^0', 'x=1');

		// controversial! http://www.askamathematician.com/2010/12/q-what-does-00-zero-raised-to-the
		// -zeroth-power-equal-why-do-mathematicians-and-high-school-teachers-disagree/
		testEvaluate('x=0^0', 'x=1');
		// should produce an error
		testEvaluate('x=1/0', errorNode); // parse error
		expect(console.error).toHaveBeenCalledWith('Divided by 0');
		expectedErrors++;
//		testEvaluate('x=y/0', errorNode); // eval error
	});
	function testDistributeRightSubExpression(eqString, expectedResult) {
		var exp = parser.parseEquation(eqString);
		expr.distributeRight(exp.rhs);
		expect(display.displayExpression(exp, outputType.text, parenMode.full))
			.toBe(expectedResult);
	}
	function testDistributeLeftRightSubExpression(eqString, expectedResult) {
		var exp = parser.parseEquation(eqString);
		expr.distributeLeft(exp.rhs);
		expect(display.displayExpression(exp, outputType.text, parenMode.full))
			.toBe(expectedResult);
	}
	it("distribute the right subexpression, distributing the rhs over the lhs", function() {
		testDistributeRightSubExpression('x=(a+b)*c', 'x=(a*c)+(b*c)');
		testDistributeRightSubExpression('x=(a+(b-34))*c', 'x=(a*c)+((b-34)*c)');
		testDistributeRightSubExpression('x=(a-b)*c', 'x=(a*c)-(b*c)');
		testDistributeRightSubExpression('x=(a-b)/c', 'x=(a/c)-(b/c)');
		testDistributeRightSubExpression('x=(a+b)/c', 'x=(a/c)+(b/c)');
	});
	it("distribute left on the right subexpression", function() {
		testDistributeLeftRightSubExpression('x=(4*(7+3))', 'x=(4*7)+(4*3)');
		testDistributeLeftRightSubExpression('x=a*(b-c)', 'x=(a*b)-(a*c)');
		testDistributeLeftRightSubExpression('x=a/(b-c)', 'x=a/(b-c)');
		testDistributeLeftRightSubExpression('x=a/(b+c)', 'x=a/(b+c)');
	});
	function testEvaluateUndistribute(eqString, expectedResult) {
		testEvaluate('y=(a+b)x', 'y=ax+bx');
		testEvaluate('y=(a-b)x', 'y=ax-bx');
	}
	function testPullIdentfierToLhs(exprString, expectedResult, identifierValue) {
		var identifierNode = expression.createSimpleExpression('identifier', identifierValue);
		var exp = parser.parseExpressionWrapper(exprString);
		var result = null;
		if (exp !== null) {
			exp = evaluate.pullIdentfierToLhs(exp, identifierNode);
			result = display.displayExpression(exp, outputType.text, parenMode.full);
		}
		expect(result).toBe(expectedResult);
	}
	it("pull a variable to the left of the term if possible", function() {
		//*
		testPullIdentfierToLhs('x*a', 'x*a', 'x');
		testPullIdentfierToLhs('2*x', 'x*2', 'x');
		testPullIdentfierToLhs('a*x', 'x*a', 'x');
		testPullIdentfierToLhs('(a*x)*b', 'x*(a*b)', 'x');
		testPullIdentfierToLhs('a*(x*b)', 'x*(a*b)', 'x');
		testPullIdentfierToLhs('a*(x/b)', 'x*(a*(1/b))', 'x');
		testPullIdentfierToLhs('x^2', 'x*x', 'x');
		//*/
		// TODO(sdspikes) : also pull 2 xes out of the bottom
		/*
		testPullIdentfierToLhs('b/x', '(1/x)*b', 'x'); //TODO(sdspikes) : fix these!
		testPullIdentfierToLhs('a*(b/x)', '(1/x)*(a*b))', 'x'); //TODO(sdspikes) : fix these!
		testPullIdentfierToLhs('a*(b/(c*x))', '(1/x)*(a*(b/c))', 'x');
		*/
		testPullIdentfierToLhs('4x^2/(5x^3+3x^2)', 'x*((4*x)*(1/((5*(x^3))+(3*(x^2)))))', 'x');
	});
	it("factor common term from the right side of both expressions", function() {
		var exp = parser.parseExpressionWrapper('\\frac{1}{y+y}c+\\frac{2}{y+y}c');
		exp = expr.factorRight(exp);
		expect(display.displayExpression(exp, outputType.text, parenMode.terms))
			.toBe('(1/(y+y)+2/(y+y))c');
		expr.factorRight(exp.lhs);
		expect(display.displayExpression(exp, outputType.text, parenMode.terms))
			.toBe('((1+2)/(y+y))c');
	});
	function testEvaluateArithmeticExpression(exprressionString, expectedResult) {
		var exp = parser.parseExpressionWrapper(exprressionString);
		if (expectedResult === null || exp === null) { expect(exp).toBe(expectedResult); }
		exp = evaluate.evaluateArithmetic(exp, true);
		if (exp === null) { expect(exp).toBe(expectedResult); }
		else {
			expect(display.displayExpression(exp, outputType.text, parenMode.necessary))
				.toBe(expectedResult);
		}
	}
	it("evaluate fractions", function() {
		testEvaluateArithmeticExpression('(2/4)+1/3', '5/6');
		testEvaluate('y=0-2/4', 'y=-1/2');
		testEvaluate('y=0 + -2/(-4)', 'y=1/2');
		testEvaluate('y=(2/4)+1/3', 'y=5/6');
		testEvaluate('y=(2/7)+5/5', 'y=9/7');
		testEvaluate('y=5+(2/7)', 'y=37/7');
		testEvaluate('y=(2/4)^2', 'y=1/4');
		testEvaluate('y=(1/9)^(1/2)', 'y=1/3');
	});
	it("whether or not to show multiplication sign in various modes", function() {
		// test order of operations and removal of whitespace
		testParseAndDisplay('x=5*2', 'x=5*2', 'x=5*2', 'x=5*2');
		testParseAndDisplay('x=4/2*3', 'x=4/2*3', 'x=4/2*3', 'x=4/2*3');
		testParseAndDisplay('x=5^2*5', 'x=(5^2)*5', 'x=5^2*5', 'x=5^2*5');
		testParseAndDisplay('x=5*5^2', 'x=5*(5^2)', 'x=5*5^2', 'x=5*5^2');
		testParseAndDisplay('x=5*x^2', 'x=5*(x^2)', 'x=5x^2', 'x=5x^2');
		testParseAndDisplay('x=(x+5)x^2', 'x=(x+5)*(x^2)', 'x=(x+5)x^2', 'x=(x+5)x^2');
		testParseAndDisplay('x=(x+5)5', 'x=(x+5)*5', 'x=(x+5)5', 'x=(x+5)5');
		testParseAndDisplay('x=(x+5)2^2', 'x=(x+5)*(2^2)', 'x=(x+5)2^2', 'x=(x+5)2^2');
		testParseAndDisplay('x=x(-2)', 'x=x*(-2)', 'x=x(-2)', 'x=x(-2)');
		testParseAndDisplay('x=a+x(-2)', 'x=a+(x*(-2))', 'x=a+x(-2)', 'x=a+x(-2)');
		testParseAndDisplay('x=4(-2)', 'x=4*(-2)', 'x=4*(-2)', 'x=4*(-2)');
		testParseAndDisplay('x=1-4(-2)', 'x=1-(4*(-2))', 'x=1-4*(-2)', 'x=1-4*(-2)');
		testParseAndDisplay('x=4(-y)', 'x=4*(-y)', 'x=4(-y)', 'x=4(-y)');
		testParseAndDisplay('x=3-4(-y)', 'x=3-(4*(-y))', 'x=3-4(-y)', 'x=3-4(-y)');
		testParseAndDisplay('x=z(-y)', 'x=z*(-y)', 'x=z(-y)', 'x=z(-y)');
		testParseAndDisplay('x=(-x)2', 'x=(-x)*2', 'x=-x2', 'x=-x2');
		testParseAndDisplay('x=1-(-x)2', 'x=1-((-x)*2)', 'x=1-(-x)2', 'x=1-(-x)2');
		testParseAndDisplay('x=1+(-((-x)2))', 'x=1+(-((-x)*2))', 'x=1+(-(-x)2)', 'x=1+-(-x)2');
		testParseAndDisplay('x=1+(-x)2', 'x=1+((-x)*2)', 'x=1+(-x)2', 'x=1+(-x)2');
		testParseAndDisplay('x=-(x2)', 'x=-(x*2)', 'x=-x2', 'x=-x2');
		testParseAndDisplay('x=(-4)2', 'x=(-4)*2', 'x=-4*2', 'x=-4*2');
		testParseAndDisplay('x=(-4)y', 'x=(-4)*y', 'x=-4y', 'x=-4y');
		testParseAndDisplay('x=(-z)y', 'x=(-z)*y', 'x=-zy', 'x=-zy');
	});
	function testSyntacticEquals(exprressionString, otherExpString, expectedResult) {
		var exp = parser.parseExpressionWrapper(exprressionString);
		var otherExp = parser.parseExpressionWrapper(otherExpString);
		if (exp === null || otherExp === null) { expect(null).toBe(expectedResult); }
		expect(exp.syntacticEquals(otherExp)).toBe(expectedResult);
	}
	function testCommuteEquals(exprressionString, otherExpString, expectedResult) {
		var exp = parser.parseExpressionWrapper(exprressionString);
		var otherExp = parser.parseExpressionWrapper(otherExpString);
		if (exp === null || otherExp === null) { expect(null).toBe(expectedResult); }
		expect(equality.equivalentModuloCommutativity(exp, otherExp, false)).toBe(expectedResult);
	}
	it("lots of commute checks", function() {
		testSyntacticEquals('-(2x)', '-(2x)', true);
		testCommuteEquals('x+y', 'y+x', true);
		testCommuteEquals('x^y', 'y*x', false);
		testCommuteEquals('x*y', 'y*x', true);
		testCommuteEquals('x/y', 'x*(1/y)', true);
		testCommuteEquals('x/y', '(1/y)*x', true);
		testCommuteEquals('2x+3y', '2x+3y', true);
		testCommuteEquals('-2x-3y', '-2x+(-3)y', true);
		testCommuteEquals('x2+3y', '2x+3y', true);
		testCommuteEquals('3y+2x', 'x2+3y', true);
		testCommuteEquals('2x+3y+4z', '2x+4z+3y', true);
		testCommuteEquals('3x+2x', 'x(3+2)', false);
		testCommuteEquals('3(x+y)+2x', 'x2 + (y+x)*3', true);
		testCommuteEquals('3x^2-4xy-5y^2', '3x^2-5y^2+(-4)xy', true);
		testCommuteEquals('3x^2+3xy-7xy-5y^2', '3x^2-4xy-5y^2', false);
		testCommuteEquals('3x/2-2', '-2+x3/2', true);
		testCommuteEquals('3x/(2+3)-2', '-2+x3/(3+2)', true);
		testCommuteEquals('3x^(2+x)/(2+3)-2', '-2+x^(x+2)3/(3+2)', true);
	});
	it("simplify expressions with identifiers that can be grouped/canceled/etc", function() {
		testEvaluate('y=x+x', 'y=2x');
		testEvaluate('y=x-x', 'y=0');
		testEvaluate('y=x*x', 'y=x^2');
		testEvaluate('y=x/x', 'y=1');
		testEvaluate('y=x^2 - x^2', 'y=0');
		testEvaluate('y=-x*x-x^2', 'y=-2x^2');
		testEvaluate('y=a/x-a/x', 'y=0');
		testEvaluate('y=a(a-1)', 'y=a^2-a');
		testEvaluate('y=y(x(z-sw))', 'y=-((sw)x)y+(xy)z');
		testEvaluate('y=x*x^2', 'y=x^3');
		testEvaluate('y=a^2/x-a/x', 'y=(a^2-a)/x');
		testEvaluate('y=a/b+c/b', 'y=(a+c)/b');
		testEvaluate('y=x*z*x*x', 'y=x^3*z');
		testEvaluate('y=(x*z)^2*x*x', 'y=x^4*z^2');
		testEvaluate('z=(xy)^3x-2x^2+x^2', 'z=x^4*y^3-x^2');
		testEvaluate('z=x(x(x+1))', 'z=x^3+x^2');
		testEvaluate('z=x^(2z)+x', null);
		testEvaluate('z=1/(x+x)', 'z=1/(2x)');
		testEvaluate('z=(xyx^2)^2', 'z=x^6*y^2');
		testEvaluate('z=(x+y)^2', 'z=(x^2+(2x)y)+y^2');
		testEvaluate('a=xy^3+x^2y^2+x^3y', 'a=(x^3*y+x^2*y^2)+xy^3');
		testEvaluate('a=xy^3+x^3y^2+x^3y+x^2y^4', 'a=((x^3*y^2+x^3*y)+x^2*y^4)+xy^3');
		testEvaluate('y=x\\cdot \\left(2x+1-y\\right)+y+x+x^2y+xz+y^2+z^2+yz',
			'y=(((((((2x^2+x^2*y)+2x)-xy)+xz)+y^2)+y)+yz)+z^2');
		testEvaluate('y=-x^2\\left(-3y\\cdot \\left(-x\\right)\\right)-x^2y3x', 'y=(-6x^3)y');
		testEvaluate('y=(4x+4y)/4','y=x+y');
		testEvaluate('y=(4x+4y)/2','y=2x+2y');
		testEvaluate('y=(4x+3y)2','y=8x+6y');
		testEvaluate('y=(4x+4y)/x', null);
		testEvaluate('y=(xz+xw+yz+yw)/(x+y)','y=w+z');
		testEvaluate('y=(x^3+2x^2y+xy^2+x^2+2xy+y^2)/(x+1)','y=(x^2+(2x)y)+y^2');
		testEvaluate('y=(4x+4y)/(x+y)','y=4');
		testEvaluate('1/2a', null);
		testEvaluate('a/2', '1/2a');
		testEvaluate('a^2/2/a', '1/2a');
		testEvaluate('a^2/2/a+2', '1/2a+2');
		testEvaluate('a/b/c', 'a/(bc)');
		testEvaluate('1/2/c', '1/(2c)');
		testEvaluate('y=a^2/z/a', 'y=a/z');
		testEvaluate('y=x/z/x^2', 'y=1/(xz)');
		testEvaluate('y=x^{yz+2x^2-y^3x}-2x+3+3x+e+x+4+1','y=((e+x^((2x^2-xy^3)+yz))+2x)+8');
		testEvaluate('z=4/(x2+2y)', 'z=2/(x+y)');
		testEvaluate('a((2/3)a+1)', '2/3a^2+a');
		testEvaluate('1/2 (a^2)/b(2/1)+2', '2+a^2/b');
		testEvaluate('(a^2 +2ab+b^2)/((a+b)(a+b))', '1');
		testEvaluate('(-1*-4-2x)/(2*-1)', 'x-2');
	});
	function testPolyDiv(parsedExpressions, expected) {
		var result = processExpressions.dividePolynomials(parsedExpressions, true);
		if (result.length !== expected.length) {
			console.error('Badly formed test!');
		}
		for (var i = 0; i < result.length; i++) {
			if (expected[i] === errorNode || result[i] === errorNode || expected[i] === null) {
				expect(result[i]).toBe(expected[i]);
			} else {
				expect(display.displayExpression(
					result[i], outputType.text, parenMode.necessary)).toBe(expected[i]);
			}
		}
	}
	it("check equality of things that do not look the same to commute", function() {
		testEqualityBreakdown('x^y', 'y^x', equalityType.none);
		testEqualityBreakdown('z/z^2', '1/z', equalityType.full);
		testEqualityBreakdown('1/a', 'a^(-1)', equalityType.full);
		testEqualityBreakdown('(12-a)/(x)', '(12-a)/(x)+x', equalityType.none);
		testEqualityBreakdown('(12-a+x^2)/(x)', '(12-a)/(x)+x', equalityType.fullEq);
	});
	it("check equivalence of exponents written in different ways", function() {
		testEvaluate('z=\\left(x\\cdot 2\\right)^{2+z}x', 'z=(4*2^z)x^(z+3)');
		testEqualityBreakdown('2^{3+x}', '8*2^x', equalityType.full);
		testEqualityBreakdown('2^{3x}', '8^x', equalityType.full);
		testEqualityBreakdown('2^{3x}8^x', '8^{2x}', equalityType.full);
		testEqualityBreakdown('x^{-2}', '1/x^2', equalityType.full);
		testEqualityBreakdown('1/x^{-2}', 'x^2', equalityType.full);
		testEqualityBreakdown('yx^{-2}', 'y/x^2', equalityType.full);
		testEqualityBreakdown('yx^{-2}+yx', 'y/x^2+yx', equalityType.full);
	});
	it("absolute value basics", function() {
		testEvaluate('\\left|-1\\right|', '1');
		testEvaluate('\\left|0\\right|', '0');
		testEvaluate('\\left|8\\right|', '8');
		testEvaluate('\\left|8-3\\right|', '5');
		testEvaluate('\\left|8-15\\right|', '7');
		testEvaluate('\\left|8-x\\right|+1', '|x-8|+1', parenMode.necessary);
		testEqualityBreakdown('\\left|x-2\\right|', '\\left|2-x\\right|', equalityType.full);
	});
	it("simplification of numbers raised to fractional exponents", function() {
		testEqualityBreakdown('2^{1/2}', '\\sqrt{2}', equalityType.full);
		testEqualityBreakdown('2^{3/2}', '2*\\sqrt{2}', equalityType.full);
		testEqualityBreakdown('16^{2/3}', '4*\\sqrt[3]{4}', equalityType.full);
		testEvaluate('\\sqrt{2} + \\sqrt{3}', '\\sqrt{3}+\\sqrt{2}');
		testEvaluate('2\\sqrt{2} + 3\\sqrt{3}+\\sqrt{2}', '3*\\sqrt{3}+3*\\sqrt{2}');
		testEvaluate('6(\\sqrt{2}+\\sqrt{3})+ \\sqrt{8}', '6*\\sqrt{3}+8*\\sqrt{2}');
		testEvaluate('\\sqrt[3/2]{5}', '\\sqrt[3]{25}');
		testEvaluate('x^{3/2}+x+\\sqrt[2/3]{x}', '2*\\sqrt{x^3}+x');
		testEvaluate('2^{1/4}3^{1/4}', '\\sqrt[4]{6}');
		testEvaluate('2^{1/2}3^{1/3}', '\\sqrt[6]{72}');
		testEvaluate('(-3)^{1/2}', '\\sqrt{-3}');
		testEvaluate('(\\sqrt{4-4*5*7}-2)/(2*7)', '1/7*\\sqrt{-34}-1/7');
		testEqualityBreakdown('3i', '(-9)^{1/2}', equalityType.full);
		testEqualityBreakdown('3i', '3(-1)^{1/2}', equalityType.full);
		testEqualityBreakdown('3i', '3\\sqrt(-1)', equalityType.verbatim);
		testEvaluate('(-1*-4-\\sqrt{(-4)^2-4*-1*-2})/(2*-1)', '\\sqrt{2}-2');
		testEvaluate('\\sqrt{2}-2', null); // no simplification necessary
		testEvaluate('(((-1/2)*\\sqrt{37}))-(((-1/2)*\\sqrt{37}))', '0');
		testEvaluate('(-\\sqrt{5}-4)/(4-\\sqrt{5})', '(-\\sqrt{5}-4)/(-\\sqrt{5}+4)');
		testEvaluate('x^(-2)-x^(-2)', '0');
		testEvaluate('x^(-1/2)-x^(-1/2)', '0');
		testEvaluate('(-2/\\sqrt{2})*5', '-5*\\sqrt{2}');
		testEvaluate('-\\left(-2/3\\sqrt{2} +4\\right)', '2/3*\\sqrt{2}-4');
		testEvaluate('-\\left(\\frac{-1}{3}\\right)\\cdot\\sqrt{2}', '1/3*\\sqrt{2}');
		testEqualityBreakdown('\\frac{-1-\\sqrt{5}}{-2}', '-1/2*\\sqrt{5}+1/2',
			equalityType.none);
		testEvaluate('\\sqrt{-5}\\sqrt{-3}', '-\\sqrt{15}');
	});
	it("test full equality divided terms which have a common factor on the top and bottom",
		function() {
		/* TODO(sdspikes) : decide if this is worth caring about, given that we can check equality.
		// can fake this one with fullsimplify, but is it worth doing?
		testEvaluate('y=(x+y)/(xz+xw+yz+yw)','y=1/(w+z)');
		// could fake these by seeing if the top divides into the bottom
		testEvaluate('(a^2 +2ab+b^2)/((a^2+2ab +b^2)(a+b))', '1/(a+b)');
		testEvaluate('(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))', '1/(a^2 +2ab+b^2)');
		// this would be much harder
		testEvaluate('(x+y)(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))', '(x+y)/(a^2 +2ab+b^2)');
		testEvaluate('z=((4x+4y)/(x+y))/x^2', 'z=4/x^2');
		//*/

		testEqualityBreakdown('y=(x+y)/(xz+xw+yz+yw)','y=1/(w+z)', equalityType.fullEq);
		testEqualityBreakdown('(a^2 +2ab+b^2)/((a^2+2ab +b^2)(a+b))', '1/(a+b)',
			equalityType.fullEq);
		testEqualityBreakdown('(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))', '1/(a^2 +2ab+b^2)',
			equalityType.fullEq);
		testEqualityBreakdown('(x+y)(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))',
			'(x+y)/(a^2 +2ab+b^2)', equalityType.fullEq);
		testEqualityBreakdown('(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))', '1/(a^2 +2ab+b^2)',
			equalityType.fullEq);
		testEqualityBreakdown('1/(a^2 +2ab+b^2)', '(a^2 +2ab+b^2)/((a^3+3a^2b+3ab^2 +b^3)(a+b))',
			equalityType.fullEq);
		testEqualityBreakdown('z=((4x+4y)/(x+y))/x^2', 'z=4/x^2', equalityType.fullEq);
	});
	it("find strictest equality type", function() {
		testEqualityBreakdown('((-xafwey+x)x)=2', '2=((-y)xxafwe + xx)', equalityType.full);
		testEqualityBreakdown('a-b', 'a+(-b)', equalityType.commute);
		testEqualityBreakdown('-b+a', 'a+(-b)', equalityType.commute);
		testEqualityBreakdown('\\frac{6x^2}{\\sqrt{y+1}} + 11',
			'\\frac{6x^2}{\\sqrt{y+1}} + 10 + 1', equalityType.full);
	});
	it("equality between equivalent fractions and decimals", function() {
		testEqualityBreakdown('1/2', '.5', equalityType.commuteCo);
		testEqualityBreakdown('1/4', '0.25', equalityType.commuteCo);
		testEqualityBreakdown('1/8', '0.125', equalityType.commuteCo);
		testEqualityBreakdown('1/4x+.2y', '0.25x+1/5y', equalityType.commuteCo);
		testEqualityBreakdown('3/6', '0.5', equalityType.full);
	});
	it("make sure commuteCo works with fractional coefficients in various forms", function() {
		testEqualityBreakdown('(1/2)x', 'x/2', equalityType.commuteCo);
		testEqualityBreakdown('(3/2)x', '(3x)/2', equalityType.commuteCo);
		// TODO(sdspikes): Should this one work?
		testEqualityBreakdown('(5/6)x', '5(x/6)', equalityType.full);
	});
	it("extensive commuteCo checking", function() {
		testEqualityBreakdown('(-2)x', '-(2x)', equalityType.commuteCo);
		testEqualityBreakdown('(-5)x+3', '3-(5x)', equalityType.commuteCo);
		testEqualityBreakdown('(-y)x', '-(yx)', equalityType.commuteCo);
		testEqualityBreakdown('(-2)5', '-(2*5)', equalityType.commuteCo);
		testEqualityBreakdown('a-b', '-b+a', equalityType.commuteCo);
		testEqualityBreakdown('a*b*c-def', '-def+abc', equalityType.commuteCo);
		testEqualityBreakdown('abc-2def', '-(2def)+abc', equalityType.commuteCo);
		testEqualityBreakdown('abc+(-2)def', '-2def+abc', equalityType.commute);
		testEqualityBreakdown('-4(abc+(-2)def)', '-4(-2def+abc)', equalityType.commute);
		testEqualityBreakdown('(2x+5)(3x-y)', '(2x+5)(-y+3x)', equalityType.commuteCo);
		testEqualityBreakdown('2x-5x-y', '-5x-y+2x', equalityType.commuteCo);
		testEqualityBreakdown('2x-5x-y', 'x2+(-5)x-y', equalityType.commute);
		testEqualityBreakdown('(-2x-5x)(-y-2)', '(-5x-2x)(-2-y)', equalityType.commuteCo);
		testEqualityBreakdown('\\sqrt{x+y}', '\\left|y+x\\right|', equalityType.none);
		testEqualityBreakdown('\\frac{1}{8}(-\\sqrt{3}+2)','(2-\\sqrt{3})/8',
			equalityType.commuteCo);
	});
	it("check that double inequalities can be correctly evaluated and compared", function() {
		// Note: no error/sanity checking
		testEvaluate('2+3<x-5 <5', '5<x-5<5');
		testEqualityBreakdown('-2x+17-1<x-1<2x+7', '-2x+4*3+5<x<2x+4+4', equalityType.fullEq);
	});
	it("solving for a variable", function() {
		testSolveForVariable('-x=4', 'x', ['x=-4']);
		testSolveForVariable('2x-x(3-5)=4', 'x', ['x=1']);
		testSolveForVariable('2x-x(3-5)=4', 'c', ['2x-x(3-5)=4']);
		testSolveForVariable('2\\sqrt{x}=4', 'x', ['x=4']);
		testSolveForVariable('2\\sqrt[3]{x}=4', 'x', ['x=8']);
		testSolveForVariable('2x^2=4', 'x', ['x=\\sqrt{2}', 'x=-\\sqrt{2}']);
		testSolveForVariable('x^3=4', 'x', ['x=\\sqrt[3]{4}']);
		testSolveForVariable('\\sqrt[3c+2]{x}=4', 'x', ['x=16*64^c']);
		testSolveForVariable('\\sqrt[3c-2]{x}=4', 'x', ['x=1/16*64^c']);
		// Maybe deal with this at some point.
		testSolveForVariable('2x^2-x(3-5)=4', 'x', ['2x^2+2x=4']);
		testSolveForVariable('2x^{3c-2}=4', 'x', ['x^(3c)/x^2=2']);
	});
	it("nodeWrapper compare and compareOp", function() {
		testNodeWrapper('compare', ['2', '3'], '-1');
		testNodeWrapper('compare', ['5', '-2'], '7');
		testNodeWrapper('compare', ['2/3', '3/5'], '0.06666666666666665');
		testNodeWrapper('compare', ['\\sqrt{2}', '\\sqrt{2}'], '0');
		testNodeWrapper('compare', ['\\sqrt{104}', '\\sqrt{104}'], '0');
		testNodeWrapper('compare', ['\\sqrt[3]{2}', '\\sqrt{2}'], '-0.15429251247822195');
		testNodeWrapper('compareOp', ['\\sqrt[3]{2}', '\\sqrt{2}'], '<');
		testNodeWrapper('compareOp', ['\\sqrt[5]{4}', '1/100'], '>');
		testNodeWrapper('compareOp', ['2/7', '(5-3)/7'], '=');
	});
	it("nodeWrapper getTerms", function() {
		testNodeWrapper('getTerms', ['2/7'], 'none,2/7');
		testNodeWrapper('getTerms', ['x/7'], '*,x,1/7');
		testNodeWrapper('getTerms', ['5x^2*2'], '*,x^2,5,2');
		testNodeWrapper('getTerms', ['x^2/(2*x*y)'], '*,x^2,1/2,1/x,1/y');
		testNodeWrapper('getTerms', ['-(5x^2*2)'], '-,5x^2*2');
		testNodeWrapper('getTerms', ['\\sqrt{-x}'], '\\sqrt,-x');
		testNodeWrapper('getTerms', ['(x-1)3(xy)'], '*,(x-1),x,y,3');
	});
	it("nodeWrapper getIdentifiers", function() {
		testNodeWrapper('getIdentifiers', ['2/7'], '');
		testNodeWrapper('getIdentifiers', ['x/7'], 'x');
		testNodeWrapper('getIdentifiers', ['5x^2*2'], 'x');
		testNodeWrapper('getIdentifiers', ['x^2/(2*x*y)'], 'x,x,y');
		testNodeWrapper('getIdentifiers', ['\\sqrt{-x}'], 'x');
		testNodeWrapper('getIdentifiers', ['\\pi*(x-1)3(xy)'], '\\pi,x,x,y');
	});
	it("distributing sqrts", function() {
		testNodeWrapper('simplify',['(1-\\sqrt{2})\\sqrt{3}'], '-\\sqrt{6}+\\sqrt{3}');
		testNodeWrapper('simplify',['(1-2\\sqrt{2})2\\sqrt{2}'], '2\\cdot\\sqrt{2}-8');
		testNodeWrapper('eqBreakdownNoFullEq',['(x-3-2i)(x-3+2i)', '(x-3-2i)(x-(3-2i))'], 'full');
	});
	it("polydiv", function() {
		testNodeWrapper('polydiv', ['0', 'x+2'], '0');
		testNodeWrapper('polydiv', ['(x+2)(x+1)(x-3j)(x+3j)', 'x+2'],
			'\\left(-9\\right)j^2-9j^2x+x^3+x^2');
	});
	it("LaTex outputType", function() {
		testParseAndDisplay('(x+1)(x+2)', '\\left(x+1\\right)\\cdot\\left(x+2\\right)',
			'\\left(x+1\\right)\\left(x+2\\right)',
			'\\left(x+1\\right)\\left(x+2\\right)',
			outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('x^2y^2', '\\left(x^2\\right)\\cdot\\left(y^2\\right)',
			'x^2y^2', 'x^2y^2', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('x<=2', 'x\\le2',
			'x\\le2', 'x\\le2', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('x<=y', 'x\\le y',
			'x\\le y', 'x\\le y', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('\\pi*x', '\\pi\\cdot x',
			'\\pi x', '\\pi x', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('\\pi*3', '\\pi\\cdot3',
			'\\pi3', '\\pi3', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('x<=y\\le z', 'x\\le y\\le z',
			'x\\le y\\le z', 'x\\le y\\le z', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('a/(b*c)', '\\frac{a}{b\\cdot c}',
			'\\frac{a}{bc}', '\\frac{a}{bc}', outputType.latex, parser.parseEquationOrExpression);
		testParseAndDisplay('a/(b*c)', 'a\\div\\left(b\\cdot c\\right)', 'a\\div\\left(bc\\right)',
			'a\\div\\left(bc\\right)',
			outputType.latex, parser.parseEquationOrExpression, divSign.notNumeric);
	});
	it("make sure we can distinguish between different unary ops", function() {
		testEqualityBreakdown('\\sqrt{x}', '-x', equalityType.none);
		// TODO(sdspikes): appears to produce an infinite loop! (Max call stack exceeded)
//		testEqualityBreakdown('x+3+\\sqrt{6}', 'x+3-6', equalityType.none);
	});
	it("polynomial synthetic division problems that were going to be in math 6", function() {
		// Unit 5.4
		testPolyDiv(processExpressions.parseExpressions(
			['3a^4-12a^3+5a^2-30a+24','a-4']), ['3a^3+5a-10-16/(a-4)']);
		testPolyDiv(processExpressions.parseExpressions(
			['8x^7+12x^5-6x^3+ 4','2x']), ['4x^6+6x^4-3x^2+2/x']);
		testPolyDiv(processExpressions.parseExpressions(
			['5a^4b^3-40a^3b^3+15a^2b^2-60ab^2','5ab^2']), ['a^3*b-8a^2*b+3a-12']);
		testEvaluate('5a^2b-8a^2b+3a-12', '-3a^2*b+3a-12', parenMode.necessary);
		testPolyDiv(processExpressions.parseExpressions(
			['3f^3g^2h^4+24f^2g^2h^3-9f^2g^2h^2','6fg^2h']), ['1/2f^2*h^3+4fh^2-3/2fh']);
		testPolyDiv(processExpressions.parseExpressions(['x^2+7x +10','x+5']), ['x+2']);
		testPolyDiv(processExpressions.parseExpressions(['3x^3+x^2-32x+6','x-3']), ['3x^2+10x-2']);
		testPolyDiv(processExpressions.parseExpressions(['6x^2-7x -3','2x+1']), ['3x-5+2/(2x+1)']);
		testPolyDiv(processExpressions.parseExpressions(
			['p^3-5p +14', 'p+3']), ['p^2-3p+4+2/(p+3)']);
		testPolyDiv(processExpressions.parseExpressions(
			['3y^3+17y^2+17y +35', 'y+5']), ['3y^2+2y+7']);
		testPolyDiv(processExpressions.parseExpressions(
			['6a^2-17a -11', '3a+2']), ['2a-7+3/(3a+2)']);
		testPolyDiv(processExpressions.parseExpressions(
			['2m^3-7m^2-21m +1', '2m+3']), ['m^2-5m-3+10/(2m+3)']);
		testPolyDiv(processExpressions.parseExpressions(['3p^3-20p^2+42p-43', 'p-4']),
			['3p^2-8p+10-3/(p-4)']);
		testPolyDiv(processExpressions.parseExpressions(['w^3-47w-14', 'w-7']), ['w^2+7w+2']);
		testPolyDiv(processExpressions.parseExpressions(
			['x^3+2x -5', 'x+3']), ['x^2-3x+11-38/(x+3)']);
		testPolyDiv(processExpressions.parseExpressions(
			['b^5+3', 'b-1']), ['b^4+b^3+b^2+b+1+4/(b-1)']);
		testPolyDiv(processExpressions.parseExpressions(['1/2x^4', '2x^2']), ['1/4x^2']);
	});
	it("sqrt and fractions on commuteCo", function() {
		testEqualityBreakdown('\\sqrt{x}/2', '(1/2)\\sqrt{x}', equalityType.commuteCo);
		testEqualityBreakdown('\\sqrt{3}/2', '(1/2)\\sqrt{3}', equalityType.commuteCo);
		testEqualityBreakdown('\\sqrt{3/4}', '(1/2)\\sqrt{3}', equalityType.full);
		testEqualityBreakdown('\\sqrt{3/4}', '\\sqrt{3}/2', equalityType.full);
		testEqualityBreakdown('2\\sqrt{3/9}', '2\\sqrt{3}/3', equalityType.full);
		testEqualityBreakdown('(2/3)\\sqrt{3}', '2\\sqrt{3}/3', equalityType.commuteCo);
	});
	it("plus/minus", function() {
		testParseAndDisplay('1\\pm3', '1\\pm3', '1\\pm3', '1\\pm3',
			outputType.latex, parser.parseEquationOrExpression);
		testEqualityBreakdown('a\\pm1', 'a\\pm1', equalityType.verbatim);
		testEvaluate('a\\pm1', null);
		testEvaluate('2\\pm1+4', '\\pm1+6', parenMode.necessary);
		testEvaluate('a\\pm1+3+4', 'a\\pm1+7', parenMode.necessary);
		testEvaluate('3\\pm a+5', '\\pm a+8', parenMode.necessary);
	});
});
