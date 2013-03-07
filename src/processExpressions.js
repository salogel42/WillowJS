/*global process:true require:true exports:true */
if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var parser = require('./parser.js').parser;
	var evaluate = require('./evaluation.js').evaluate;
	var equality = require('./equality.js').equality;
	var expr = require('./expr.js').expr;
	var errorNode = require('./utils.js').errorNode;
}

var processExpressions = (function() {
	function buildArray(parsedExpessions, callback) {
		var arr = [];
		for(var i = 0; i < parsedExpessions.length; i++) {
			arr.push(callback(parsedExpessions[i]));
		}
		return arr;
	}

	function buildArrayPairs(parsedExpessions, callback) {
		var arr = [];
		for(var i = 0; i < parsedExpessions.length - 1; i += 2) {
			arr.push(callback(parsedExpessions[i], parsedExpessions[i + 1]));
		}
		return arr;
	}

	var self = {
		parseExpressions: function(expressions) {
			return buildArray(expressions, parser.parseEquationOrExpression);
		},
		checkVerbatimEquality: function(parsedExpessions) {
			for (var i = 0; i < parsedExpessions.length - 1; i++) {
				if (parsedExpessions[i] === errorNode) { return false; }
				if (!parsedExpessions[i].syntacticEquals(parsedExpessions[i + 1])) {
					return false;
				}
			}
			return parsedExpessions.length > 1;
		},
		checkCommuteEquality: function(parsedExpessions) {
			for (var i = 0; i < parsedExpessions.length - 1; i++) {
				if (!equality.equivalentModuloCommutativity(
					parsedExpessions[i], parsedExpessions[i + 1], false)) {
					return false;
				}
			}
			return parsedExpessions.length > 1;
		},
		checkCommuteEqualityCoefficient: function(parsedExpessions) {
			for (var i = 0; i < parsedExpessions.length - 1; i++) {
				if (!equality.equivalentModuloCommutativity(
					parsedExpessions[i], parsedExpessions[i + 1], true)) {
					return false;
				}
			}
			return parsedExpessions.length > 1;
		},
		checkFullEquality: function(parsedExpessions) {
			var simplifiedExpressions = [];
			for(var i = 0; i < parsedExpessions.length; i++) {
				var evaluated = evaluate.evaluateRec(parsedExpessions[i]);
				simplifiedExpressions.push( (evaluated === null) ?
					parsedExpessions[i] : evaluated);
			}
			for (var j = 0; j < parsedExpessions.length - 1; j++) {
				if (!equality.equivalentModuloCommutativity(
					simplifiedExpressions[j], simplifiedExpressions[j + 1])) {
					return false;
				}
			}
			return simplifiedExpressions.length > 1;
		},
		checkFullEqualityEquationOrInequality: function(parsedExpessions) {
			for (var j = 0; j < parsedExpessions.length - 1; j++) {
				if (!evaluate.equivalentEquationOrInequality(
					parsedExpessions[j], parsedExpessions[j + 1])) {
					return false;
				}
			}
			return parsedExpessions.length > 1;
		},
		simplifyExpressions: function(parsedExpessions) {
			return buildArray(parsedExpessions, evaluate.evaluateRec);
		},
		simplifyExpressionsNumeric: function(parsedExpessions) {
			return buildArray(parsedExpessions, expr.computeWithApproxNumericValue);
		},
		dividePolynomials: function(parsedExpessions) {
			var simplifiedExpressions = [];
			for(var i = 0; i < parsedExpessions.length - 1; i += 2) {
				var dividend = evaluate.evaluateRec(parsedExpessions[i]);
				var divisor = evaluate.evaluateRec(parsedExpessions[i + 1]);
				simplifiedExpressions.push(evaluate.doSyntheticDivision(dividend, divisor, true));
			}
			return simplifiedExpressions;
		},
		strictestEquality: function(parsedExpessions, fullEq) {
			return buildArrayPairs(parsedExpessions, function(node, other) {
				return evaluate.strictestEqualityType(node, other, fullEq);
			});
		},
		solve: function(parsedExpessions) {
			var solved = [];
			for(var i = 0; i < parsedExpessions.length - 1; i += 2) {
				solved = solved.concat(
					evaluate.solveForIdentifier(parsedExpessions[i], parsedExpessions[i + 1]));
			}
			return solved;
		},
		compare: function(parsedExpessions) {
			return buildArrayPairs(parsedExpessions, expr.compareNodesNumerically);
		}
	};
	return self;
}());


if (typeof exports !== 'undefined') {
	exports.processExpressions = processExpressions;
}