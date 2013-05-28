/*global process:true require:true exports:true */
if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var parser = require('./parser.js').parser;
	var evaluate = require('./evaluation.js').evaluate;
	var equality = require('./equality.js').equality;
	var expr = require('./expr.js').expr;
	var errorNode = require('./utils.js').errorNode;
}

var processExpressions = (function() {
	function buildArray(parsedExpressions, callback) {
		var arr = [];
		for(var i = 0; i < parsedExpressions.length; i++) {
			arr.push(callback(parsedExpressions[i]));
		}
		return arr;
	}

	function buildArrayPairs(parsedExpressions, callback) {
		var arr = [];
		for(var i = 0; i < parsedExpressions.length - 1; i += 2) {
			arr.push(callback(parsedExpressions[i], parsedExpressions[i + 1]));
		}
		return arr;
	}

	var self = {
		parseExpressions: function(expressions) {
			return buildArray(expressions, parser.parseEquationOrExpression);
		},
		checkVerbatimEquality: function(parsedExpressions) {
			for (var i = 0; i < parsedExpressions.length - 1; i++) {
				if (parsedExpressions[i] === errorNode) { return false; }
				if (!parsedExpressions[i].syntacticEquals(parsedExpressions[i + 1])) {
					return false;
				}
			}
			return parsedExpressions.length > 1;
		},
		checkCommuteEquality: function(parsedExpressions) {
			for (var i = 0; i < parsedExpressions.length - 1; i++) {
				if (!equality.equivalentModuloCommutativity(
					parsedExpressions[i], parsedExpressions[i + 1], false)) {
					return false;
				}
			}
			return parsedExpressions.length > 1;
		},
		checkCommuteEqualityCoefficient: function(parsedExpressions) {
			for (var i = 0; i < parsedExpressions.length - 1; i++) {
				if (!equality.equivalentModuloCommutativity(
					parsedExpressions[i], parsedExpressions[i + 1], true)) {
					return false;
				}
			}
			return parsedExpressions.length > 1;
		},
		checkFullEquality: function(parsedExpressions) {
			var simplifiedExpressions = [];
			for(var i = 0; i < parsedExpressions.length; i++) {
				var evaluated = evaluate.evaluateRec(parsedExpressions[i]);
				simplifiedExpressions.push( (evaluated === null) ?
					parsedExpressions[i] : evaluated);
			}
			for (var j = 0; j < parsedExpressions.length - 1; j++) {
				if (!equality.equivalentModuloCommutativity(
					simplifiedExpressions[j], simplifiedExpressions[j + 1])) {
					return false;
				}
			}
			return simplifiedExpressions.length > 1;
		},
		checkFullEqualityEquationOrInequality: function(parsedExpressions) {
			for (var j = 0; j < parsedExpressions.length - 1; j++) {
				if (!evaluate.equivalentEquationOrInequality(
					parsedExpressions[j], parsedExpressions[j + 1])) {
					return false;
				}
			}
			return parsedExpressions.length > 1;
		},
		simplifyExpressions: function(parsedExpressions) {
			return buildArray(parsedExpressions, evaluate.evaluateRec);
		},
		simplifyExpressionsNumeric: function(parsedExpressions) {
			return buildArray(parsedExpressions, expr.computeWithApproxNumericValue);
		},
		dividePolynomials: function(parsedExpressions, remainders) {
			var simplifiedExpressions = [];
			for(var i = 0; i < parsedExpressions.length - 1; i += 2) {
				var dividend = evaluate.evaluateRec(parsedExpressions[i]);
				var divisor = evaluate.evaluateRec(parsedExpressions[i + 1]);
				simplifiedExpressions.push(evaluate.doSyntheticDivision(dividend, divisor, remainders));
			}
			return simplifiedExpressions;
		},
		strictestEquality: function(parsedExpressions, fullEq) {
			return buildArrayPairs(parsedExpressions, function(node, other) {
				return evaluate.strictestEqualityType(node, other, fullEq);
			});
		},
		solve: function(parsedExpressions) {
			var solved = [];
			for(var i = 0; i < parsedExpressions.length - 1; i += 2) {
				solved = solved.concat(
					evaluate.solveForIdentifier(parsedExpressions[i], parsedExpressions[i + 1]));
			}
			return solved;
		},
		compare: function(parsedExpressions) {
			return buildArrayPairs(parsedExpressions, expr.compareNodesNumerically);
		}
	};
	return self;
}());


if (typeof exports !== 'undefined') {
	exports.processExpressions = processExpressions;
}