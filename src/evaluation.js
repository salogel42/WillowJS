/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var display = require('./display.js');
	var parenMode = display.parenMode;
	var outputType = display.outputType;
	display = display.display;
	var expr = require('./expr.js').expr;
	var utils = require('./utils.js');
	var errorNode = utils.errorNode;
	utils = utils.utils;
	var equality = require('./equality.js').equality;
	var fractionUtils = require('./fractionUtils.js').fractionUtils;
	var parser = require('./parser.js').parser;
	var operator = require('./operator.js').operator;
	var operatorProperties = require('./operatorProperties.js').operatorProperties;
	var getPrecedence = operatorProperties.getPrecedence;
	var expression = require('./expression.js').expression;
}

var equalityType = {
	'verbatim' : 0,
	'commuteCo' : 1,
	'commute' : 2,
	'full' : 3,
	'fullEq' : 4,
	'none' : 5
};

var evaluate = (function() {
	var debug = false;
	var debugArith = false;
	var debugMult = false;
	var debugSolve = false;
	var debugSort = false;
	var debugSynth = false;
	var debugGroup = false;
	var debugSimp = false;
	var debugCombine = false;
	var debugCompare = false;
	var debugCoeff = false;
	var debugFullEq = false;

	function printDebug(preMessage, node, debugType) {
		if (debugType) {
			console.log(preMessage +
				display.displayExpression(node, outputType.text, parenMode.full));
		}
	}
	function invertNodes(nodeArray) {
		for (var i = 0; i < nodeArray.length; i++) {
			nodeArray[i] = operator.invertNode(nodeArray[i]);
		}
		return nodeArray;
	}
	function makeCommutativeIfNecessaryRec(node, op) {
		if ((node.type === 'compound' || node.type === 'ternary') &&
			getPrecedence(node.op) === getPrecedence(op)) {
			var newNode = node;
			if (node.type === 'compound') {
				newNode = expression.createCompoundExpression(
					makeCommutativeIfNecessaryRec(node.lhs, op),
					makeCommutativeIfNecessaryRec(node.rhs, op),
					node.op);
			}
			if (node.type === 'ternary') {
				newNode = expression.createTernaryExpression(
					makeCommutativeIfNecessaryRec(node.left, op),
					makeCommutativeIfNecessaryRec(node.middle, op),
					makeCommutativeIfNecessaryRec(node.right, op),
					node.op1,
					node.op2);
			}
			return expr.makeCommutativeIfNecessary(newNode);
		}
		return node;
	}

	function getMultipliedIdentfiers(node) {
		if (node.type === 'identifier') { return [node]; }
		if (utils.isUnaryNegative(node)) { return getMultipliedIdentfiers(node.child); }
		if (node.type === 'number' && fractionUtils.isValueRadical(node.value)) {
			return [node];
		}
		if (node.type !== 'compound') { return []; }
		if (getPrecedence(node.op) < 2) { return [node]; }
		var identifiers = getMultipliedIdentfiers(node.lhs);
		if (node.op === '^') {
			// If it's something like x^2 or x^(3/2), we can pull out an x, so in those cases we
			// ignore whatever is in the exponent and let that be dealt with when we try to pull out
			// the base.
			// But, if it's anything else, say a fraction smaller than 1 like x^(1/2), or it's a
			// radical like x^{\sqrt{2}}, or the exponent contains an identifier like x^y, we can
			// only pull it out as a unit so we want to look for repetitions of the full expression,
			// not just the base (or bases, in the case of something like (xy)^2).
			// So, if it is not a number, or it is a number that's a fraction < 1
			if (node.rhs.type !== 'number' || !(fractionUtils.isInteger(node.rhs.value) ||
				(fractionUtils.isNodeFraction(node.rhs) &&
					Math.abs(node.rhs.value.top) > Math.abs(node.rhs.value.bottom)))) {
				var resultIdentifiers = [];
				for (var i = 0; i < identifiers.length; i++) {
					resultIdentifiers.push(expression.createCompoundExpression(identifiers[i],
						node.rhs, '^'));
				}
				return resultIdentifiers;
			} else if (fractionUtils.isNodeNegative(node.rhs)) {
				return invertNodes(identifiers);
			}
		}
		var rhsIdentifiers = getMultipliedIdentfiers(node.rhs);
		if (node.op === '*') { identifiers = identifiers.concat(rhsIdentifiers); }
		if (node.op === '/') { identifiers = identifiers.concat(invertNodes(rhsIdentifiers)); }
		return identifiers;
	}
	function containsIdentifier(node, identifierNode) {
		if (node.syntacticEquals(identifierNode)) { return true; }
		if (node.type === 'unary') { return containsIdentifier(node.child, identifierNode); }
		if (node.type === 'compound') {
			return containsIdentifier(node.lhs, identifierNode) ||
				containsIdentifier(node.rhs, identifierNode);
		}
		return false;
	}
	function findRepeats(array) {
		array = array.sort();
		var repeats = [];
		while (array.length > 0) {
			var current = array.shift();
			var found = false;
			for (var i = 0; i < array.length; i++) {
				if (current.type === 'number' && array[i].type === 'number' ||
					current.syntacticEquals(array[i])) {
					found = true;
					array.splice(i--, 1);
				}
			}
			if (found) { repeats.push(current); }
		}
		return repeats;
	}
	// includes those that are inverted
	function getAllIdentifiersInMulitplication(node) {
		if (node.type === 'identifier') { return [node]; }
		if (node.type === 'number') { return [node]; }
		if (utils.isUnaryNegative(node)) { return getAllIdentifiersInMulitplication(node.child); }
		if (node.type !== 'compound' || getPrecedence(node.op) < 2) { return []; }
		var lhsIdenifiers = getAllIdentifiersInMulitplication(node.lhs);
		if (node.op === '^') { return lhsIdenifiers; }
		var rhsIdentifiers = getAllIdentifiersInMulitplication(node.rhs);
		return lhsIdenifiers.concat(rhsIdentifiers);
	}
	// maybe also pass identifierNode and make sure the base is correct identifier?
	function getPower(node) {
		if (node.type === 'compound' && node.op === '^') { return node.rhs; }
		return expression.createSimpleExpression('number', 1);
	}
	function combineGroupedIdentifiers(nodeOne, nodeTwo, op, identifierNode) {
		printDebug('in combineGroupedIdentifiers, nodeOne: ', nodeOne, debugCombine);
		printDebug('                              nodeTwo: ', nodeTwo, debugCombine);
		if (nodeTwo === null) { return nodeOne; }
		if (nodeOne === null) {
			if (op !== '/') { return nodeTwo; }
			var power = getPower(nodeTwo);
			return expression.createCompoundExpression(identifierNode,
				arithmeticEvaluation(expression.createCompoundExpression(
					expression.createSimpleExpression('number', -1), power, '*')),
				'^');
		}
		if (nodeOne.type === 'identifier' && nodeTwo.type === 'identifier' &&
			nodeOne.value === nodeTwo.value) {
			if (op === '*') {
				return expression.createCompoundExpression(nodeOne,
					expression.createSimpleExpression('number', 2), '^');
			}
			if (op === '/') { return expression.createSimpleExpression('number', 1); }
		}
		var onePower = getPower(nodeOne);
		var twoPower = getPower(nodeTwo);
		if (op === '/') { twoPower = arithmeticEvaluation(expression.createCompoundExpression(
			expression.createSimpleExpression('number', -1), twoPower, '*')); }
		return arithmeticEvaluation(
			expression.createCompoundExpression(
				identifierNode,
				evaluate.evaluateRec(expression.createCompoundExpression(onePower, twoPower, '+')),
				'^')
			);
	}
	function multiplyNodesAndEvaluate(nodeOne, nodeTwo, op) {
		var result = operator.multiplyNodesMaybeNull(nodeOne, nodeTwo, op);
		return evaluate.evaluateRec(result);
	}
	function isIdentifierGroup(node, identifierNode) {
		return (identifierNode.syntacticEquals(node) || (node.type === 'compound' &&
			node.op === '^' && identifierNode.syntacticEquals(node.lhs)));
	}
	function separateGroupedIdentifierAndRestOfExpression(node, identifierNode) {
		var identifierGroup = node;
		var restOfExpression = null;
		if (!isIdentifierGroup(node, identifierNode)) {
			if (node.type === 'compound' && isIdentifierGroup(node.lhs, identifierNode) &&
				getPrecedence(node.op) > 1) {
				identifierGroup = node.lhs;
				restOfExpression = node.rhs;
			} else {
				identifierGroup = null;
				restOfExpression = node;
			}
		}
		return {
			'identifierGroup' : identifierGroup,
			'restOfExpression' : restOfExpression
		};
	}
	function groupRepeatedIdentifierOnLhs(node, identifierNode) {
		printDebug('pulling ', identifierNode, debugGroup);
		printDebug(' to left of ', node, debugGroup);
		if (isIdentifierGroup(node, identifierNode)) { return node; }
		if (utils.isUnaryNegative(node)) {
			return expression.createCompoundExpression(node.child,
				expression.createSimpleExpression('number', -1), '*');
		}
		if (node.type !== 'compound' || getPrecedence(node.op) < 2) { return node; }
		if (node.op === '^' && !identifierNode.syntacticEquals(node.lhs)) {
			var newBase = groupRepeatedIdentifierOnLhs(
				expr.makeCommutativeIfNecessary(node.lhs), identifierNode);

			// should not ever happen, i think?
			if (identifierNode.syntacticEquals(newBase)) {
				return groupRepeatedIdentifierOnLhs(
					expression.createCompoundExpression(newBase, node.rhs, '^'), identifierNode);
			}
			if (newBase.type === 'compound' && newBase.op === '*') {
				var components =
					separateGroupedIdentifierAndRestOfExpression(newBase, identifierNode);
				if (components.identifierGroup !== null) {
					var newLhs = expression.createCompoundExpression(
						components.identifierGroup, node.rhs, '^');
					if (components.identifierGroup.type === 'compound' &&
						components.identifierGroup.op === '^') {
						newLhs = expression.createCompoundExpression(components.identifierGroup.lhs,
							expression.createCompoundExpression(
								components.identifierGroup.rhs, node.rhs, '*'),
							'^');
					}
					return expression.createCompoundExpression(newLhs,
						expression.createCompoundExpression(
							components.restOfExpression, node.rhs, '^'), '*');
				}
			}
			return node;
		}
		var leftGrouped = node.lhs;
		var rightGrouped = node.rhs;
		if (!isIdentifierGroup(leftGrouped, identifierNode)) {
			leftGrouped = groupRepeatedIdentifierOnLhs(node.lhs, identifierNode);
		}
		if (!isIdentifierGroup(rightGrouped, identifierNode)) {
			rightGrouped = groupRepeatedIdentifierOnLhs(node.rhs, identifierNode);
		}
		var leftComponents = separateGroupedIdentifierAndRestOfExpression(
			leftGrouped, identifierNode);
		var rightComponents = separateGroupedIdentifierAndRestOfExpression(
			rightGrouped, identifierNode);

		var identifierGroup = combineGroupedIdentifiers(leftComponents.identifierGroup,
			rightComponents.identifierGroup, node.op, identifierNode);
		var restOfExpression = operator.multiplyNodesMaybeNull(
			leftComponents.restOfExpression, rightComponents.restOfExpression, node.op);
		return operator.multiplyNodesMaybeNull(identifierGroup, restOfExpression, '*');
	}
	function getCommonCoefficient(node) {
		printDebug('in getCommonCoefficient: ', node, debugCoeff);
		if (node.type === 'number') { return node; }
		if (utils.isUnaryNegative(node)) {
			return equality.additiveInverseOfNumber(getCommonCoefficient(node.child));
		}
		if (node.type !== 'compound') { return null; }
		// TODO(sdspikes) revisit this
		if (node.op === '^') { return null; }
		var left = getCommonCoefficient(node.lhs);
		var right = getCommonCoefficient(node.rhs);
		if (getPrecedence(node.op) === 2) {
			if (left !== null && right !== null) {
				return arithmeticEvaluation(
					expression.createCompoundExpression(left, right, node.op), false);
			}
			if (left !== null) { return left; }
			if (right !== null) { return (node.op === '/') ? operator.invertNode(right) : right; }

			return null;
		}
		if (getPrecedence(node.op) === 1) {
			if (left !== null && right !== null &&
				left.type === 'number' && right.type === 'number') {
				return expression.createSimpleExpression('number',
					fractionUtils.fractionGCD(left.value, right.value));
			}
			return null;
		}
		return null;
	}

	function groupRepeats(node, multiplyDivision) {
		// If we could not factor anything out, we have our final answer (at
		// least until node becomes part of a larger expression), so pretty it
		// up by combining identifiers in multiplied terms.
		printDebug('grouping ', node, debugGroup);
		node = operator.rationalizeDenominator(node);
		if (node.type === 'compound' && node.op === '/') {
			var topFactor = getCommonCoefficient(node.lhs);
			var bottomFactor = getCommonCoefficient(node.rhs);
			if (topFactor !== null && bottomFactor !== null && topFactor.type === 'number' &&
				bottomFactor.type === 'number') {
				var gcd = fractionUtils.fractionGCD(topFactor.value, bottomFactor.value);
				if (gcd !== 1) {
					gcd = operator.invertNode(expression.createSimpleExpression('number', gcd));
					node = expression.createCompoundExpression(
						fullMultiply(expression.createCompoundExpression(gcd, node.lhs, '*'),
							multiplyDivision),
						fullMultiply(expression.createCompoundExpression(gcd, node.rhs, '*'),
							multiplyDivision),
						node.op);
				}
			}
		}
		if (node.type === 'compound' && getPrecedence(node.op) === 2) {
			node = expr.makeCommutativeIfNecessary(node);
			var repeatedIdentifiers = findRepeats(getAllIdentifiersInMulitplication(node));
			while (repeatedIdentifiers.length > 0) {
				node = groupRepeatedIdentifierOnLhs(node, repeatedIdentifiers.shift());
				// TODO(sdspikes): there has to be a better way than special casing this...
				if (node.type === 'compound' && node.op === '^' && node.lhs.type === 'number' &&
					getCoefficient(node.rhs) !== 1) {
					node = evaluate.evaluateRec(node);
				}
			}
		}
		return node;
	}

	// Later in the list means it should be sorted earlier.  Callers of compareStrings (or
	// comparison functions that use it) should fill this list before calling, then clear it
	// afterward if they want non-alphabetic ordering for some or all identifiers.
	var identifierPriority = [];
	function compareStrings(a, b, mult) {
		var priorityA = (a.type === 'identifier') ? identifierPriority.indexOf(a.value) : -1;
		var priorityB = (b.type === 'identifier') ? identifierPriority.indexOf(b.value) : -1;
		if (priorityA !== -1 || priorityB !== -1) {
			return mult * (priorityB - priorityA);
		}
		return mult * ((a.value === b.value) ? 0 : ((a.value < b.value) ? -1 : 1));
	}
	function comparePairs(a, b, altA, altB, mult) {
		var comp = compareTerms(a, b, mult);
		return (comp === 0) ? compareTerms(altA, altB, mult) : comp;
	}
	function containsDivTerm(node, divTerm) {
		if (node.type === 'compound') {
			if (node.op === '/') {
				return equality.equivalentModuloCommutativity(divTerm, node.rhs, false);
			}
			if (node.op === '*') {
				return containsDivTerm(node.lhs, divTerm) || containsDivTerm(node.rhs, divTerm);
			}
		}
		return false;
	}
	function compareNumbers(a, b) {
		var sub = operator['-'].evaluateValues(a, b);
		if (sub.value === 0) { return 0; }
		return (fractionUtils.isNegative(sub.value)) ? -1 : 1;
	}
	function compareTerms(a, b, mult) {
		printDebug('checking ', a, debugCompare);
		printDebug(' against ', b, debugCompare);
		if (equality.equivalentModuloCommutativity(a, b, false)) { return 0; }
		if (a.type === 'compound' && b.type === 'compound' && a.op === '/' && b.op === '/') {
			return compareTerms(a.rhs, b.rhs, mult);
		}
		if (a.type === 'compound' && a.op === '/') { return (containsDivTerm(b, a.rhs)) ? 0 : -1; }
		if (b.type === 'compound' && b.op === '/') { return (containsDivTerm(a, b.rhs)) ? 0 : 1; }
		if (a.type === 'number' && b.type === 'number') {
			if (fractionUtils.isValueRadical(a.value) && fractionUtils.isValueRadical(b.value)) {
				if (fractionUtils.compareFractions(a.value, b.value, true)) { return 0; }
				var diff = compareNumbers(a.value.radicand, b.value.radicand);
				return (diff === 0) ? compareNumbers(a.value.power, b.value.power) : diff;
			}
			if (fractionUtils.isValueRadical(a.value)) { return 1; }
			if (fractionUtils.isValueRadical(b.value)) { return -1; }
			return compareNumbers(a.value, b.value);
		}
		// If one of them is a straight up number (int or frac), it's smaller than whatever the
		// other thing thing is (probably compound or identifier).  However, we want to treat
		// radicals essentially like variables at this point, so don't dismiss them, keep checking.
		if (a.type === 'number' && !fractionUtils.isValueRadical(a.value) ||
			b.type === 'number' && !fractionUtils.isValueRadical(b.value)) {
			return (a.type === 'number') ? -1 : 1;
		}
		if (a.type === 'identifier' && b.type === 'identifier') {
			return compareStrings(a, b, mult);
		}
		if ((a.type === 'number' && b.type === 'identifier') ||
			(b.type === 'number' && a.type === 'identifier')) {
			return (a.type === 'number') ? -1 : 1;
		}
		if (utils.isUnaryNegative(a)) { return compareTerms(a.child, b, mult); }
		if (utils.isUnaryNegative(b)) { return compareTerms(a, b.child, mult); }
		if (a.type === 'unary' && b.type === 'unary') {
			if (a.op === b.op) { return compareTerms(a.child, b.child); }
			return (a.op === '|') ? -1 : 1;
		}
		if (a.type === 'unary') { return -1; }
		if (b.type === 'unary') { return 1; }
		if (a.type === 'compound' && b.type === 'compound') {
			if (getPrecedence(a.op) !== 2 && getPrecedence(b.op) !== 2) {
				if (a.op === '^' && b.op === '^') {
					return comparePairs(a.lhs, b.lhs, a.rhs, b.rhs, mult);
				}
				if (getPrecedence(a.op) === 1 && getPrecedence(b.op) === 1) {
					a = sortTerms(a, false);
					b = sortTerms(b, false);
					return comparePairs(a.lhs, b.lhs, a.rhs, b.rhs, mult);
				}
				if (getPrecedence(a.op) === 1) { return 1; }
				if (getPrecedence(b.op) === 1) { return -1; }
				if (a.op === '\\sqrt' && b.op === '\\sqrt') {
					return comparePairs(a.rhs, b.rhs, a.lhs, b.lhs, mult);
				}
				if (a.op === '\\sqrt') { return -1; }
				if (b.op === '\\sqrt') { return 1; }
			}
		}
		var termsA = getAllTermsAtLevelNoSort(a, '*', false);
		var termsB = getAllTermsAtLevelNoSort(b, '*', false);
		termsA = termsA.sort(compareMultiplication);
		termsB = termsB.sort(compareMultiplication);
		while (termsA.length > 0 && termsB.length > 0) {
			a = termsA.shift();
			b = termsB.shift();
			printDebug('checking nth terms ', a, debugCompare);
			printDebug(' against ', b, debugCompare);
			if (a.type === 'identifier' && b.type === 'compound') {
				var compRight = (b.op === '^') ? compareTerms(a, b.lhs, mult) : 1 * mult;
				return (compRight !== 0) ? compRight : 1 * mult;
			}
			if (a.type === 'compound' && b.type === 'identifier') {
				var compLeft = (a.op === '^') ? compareTerms(a.lhs, b, mult) : -1 * mult;
				return (compLeft !== 0) ? compLeft : -1 * mult;
			}
			var comp = compareTerms(a, b, mult);
			if (comp !== 0) { return comp; }
			if (a.type === 'compound' && a.op === '/') { return 0; }
		}
		return (termsA.length > 0) ? mult : (termsB.length > 0) ? -1 * mult : 0;
	}

	function compareMultiplicationDivFirst(a, b) {
		if (utils.isOneOverSomething(a) && utils.isOneOverSomething(b)) {
			return compareMultiplication(a, b);
		}
		if (utils.isOneOverSomething(a)) {
			return -1;
		}
		if (utils.isOneOverSomething(b)) {
			return 1;
		}
		return compareMultiplication(a, b);
	}

	function compareMultiplication(a, b) {
		return compareTerms(a, b, 1);
	}
	function compareAddition(a, b) {
		return -1 * compareTerms(a, b, -1);
	}

	function makeUnaryOfEach(nodeArray, op) {
		for (var i = 0; i < nodeArray.length; i++) {
			nodeArray[i] = expression.createUnaryExpression(nodeArray[i], op);
		}
		return nodeArray;
	}

	function getAllTermsAtLevelNoSort(node, op, coefficients) {
		if (node.type === 'number') {
			if (fractionUtils.isValueRadical(node.value)) { return [node]; }
			return (coefficients) ? [node] : [];
		}
		if (node.type === 'identifier') { return [node]; }
		if (node.type === 'unary') {
			if (node.op !== '-' && node.op !== op) { return [node]; }
			var allUnary = getAllTermsAtLevelNoSort(node.child, op, coefficients);
			return (coefficients && node.op !== op) ? makeUnaryOfEach(allUnary, node.op) : allUnary;
		}
		if (getPrecedence(node.op) !== getPrecedence(op)) { return [node]; }
		var identifiers = getAllTermsAtLevelNoSort(node.lhs, op, coefficients);
		var rhsIdentifiers = getAllTermsAtLevelNoSort(node.rhs, op, coefficients);
		if (node.op === '/') {
			if (node.op !== op && utils.isOneOverSomething(node)) {
				return invertNodes(rhsIdentifiers);
			}
			return identifiers.concat(invertNodes(rhsIdentifiers));
		}
		return identifiers.concat(rhsIdentifiers);
	}

	function getAllTermsAtLevelSorted(node, op, associateRight) {
		printDebug('getAllTermsAtLevelSorted:', node, debugSort);
		if (utils.isSimpleExpression(node)) { return [node]; }
		if (utils.isUnaryNegative(node)) {
			if (getPrecedence(op) !== 2) {
				return [expression.createUnaryExpression(
					sortTerms(node.child, associateRight), '-')];
			} else {
				return getAllTermsAtLevelSorted(node.child, associateRight).concat(
					[expression.createSimpleExpression('number', -1)]);
			}
		}
		if (node.type === 'unary') {
			var sorted = sortTerms(node.child, associateRight);
			if (node.op === '|') {
				// If the leading term is negative (or has a negative coefficient), invert the whole
				// term inside the absolute value by making it unary negative and multiplying it out.
				// This allows for seeing |a-b| and |b-a| as the same thing.
				// Only need to do this once, so check that associateRight is true before checking.
				if (associateRight &&
					fractionUtils.isNodeNegative(leftMostTermInMult(leftMostTerm(sorted)))) {
					sorted = fullMultiply(expression.createUnaryExpression(sorted, '-'), true);
				}
			}
			return [expression.createUnaryExpression(sorted, node.op)];
		}
		if (node.op !== op) { return [sortTerms(node, associateRight)]; }
		return getAllTermsAtLevelSorted(node.lhs, op, associateRight).concat(
			getAllTermsAtLevelSorted(node.rhs, op, associateRight));
	}


	/**
	 * Should only be called with minus always converted to + -, but not / to * 1/.
	 */
	function sortTerms(node, associateRight) {
		printDebug('sorting:', node, debugSort);
		if (node === errorNode || utils.isSimpleExpression(node)) {
			return node;
		}
		if (node.type === 'ternary') {
			return expression.createTernaryExpression(sortTerms(node.left, associateRight),
				sortTerms(node.middle, associateRight), sortTerms(node.right, associateRight),
				node.op1, node.op2);
		}

		var precedence = getPrecedence(node.op);
		var terms = getAllTermsAtLevelSorted(node, node.op, associateRight);
		/*
		for (var q = 0; q < terms.length; q++) {
			printDebug(q + 'th ', terms[q], debugSort);
		}*/
		if (terms.length === 1) { return terms[0]; }
		var compareFunc = null;
		if (precedence === 1) { compareFunc = compareAddition; }
		if (precedence === 2 && node.op !== '/') { compareFunc = compareMultiplication; }
		if (compareFunc !== null) { terms = terms.sort(compareFunc); }
		var result = terms[0];
		if (associateRight) {
			result = terms[terms.length-1];
			if (terms.length > 1) {
				var currentLevel = terms[terms.length-2];
				var currentLeft = currentLevel;
				for (var i = terms.length - 3; i >= 0; i--) {
					if (compareFunc !== null && compareFunc(currentLeft, terms[i]) === 0) {
						currentLeft = terms[i];
						currentLevel = expression.createCompoundExpression(
							terms[i], currentLevel, node.op);
					} else {
						result = expression.createCompoundExpression(currentLevel, result, node.op);
						currentLeft = terms[i];
						currentLevel = terms[i];
					}
				}
				result = expression.createCompoundExpression(currentLevel, result, node.op);
			}
		} else {
			for (var j = 1; j < terms.length; j++) {
				if (getPrecedence(node.op) !== 0) {
					terms[j] = equality.normalizeTermSign(terms[j]);
				}
				terms[j] = convertFractionalExponentToSqrt(terms[j]);
				if (node.op === '+' && utils.isUnaryPlusMinusOrNegative(terms[j])) {
					result = expression.createCompoundExpression(
						result, terms[j].child, terms[j].op);
				} else if (node.op === '+' && terms[j].type === 'number' &&
					fractionUtils.isNegative(terms[j].value)) {
					result = expression.createCompoundExpression(
						result, equality.additiveInverseOfNumber(terms[j]), '-');
				} else if (getPrecedence(node.op) === 2 && utils.isNegativeOne(terms[j])) {
					result = expression.createUnaryExpression(result, '-');
				} else if (node.op === '*' && utils.isNegativeOne(result)) {
					result = expression.createUnaryExpression(terms[j], '-');
				} else if (node.op === '*' && utils.isUnaryPlusMinusOrNegative(result)) {
					result = expression.createUnaryExpression(expression.createCompoundExpression(
						result.child, terms[j], node.op), result.op);
				} else if (node.op === '*' && utils.isUnaryPlusMinusOrNegative(terms[j])) {
					result = expression.createUnaryExpression(expression.createCompoundExpression(
						result, terms[j].child, node.op), terms[j].op);
				} else if (node.op === '*' && result.type === 'compound' && result.op === '/') {
					return expression.createCompoundExpression(arithmeticEvaluation(
						expression.createCompoundExpression(result.lhs, terms[j], '*')),
					result.rhs, '/');
				} else if (node.op === '/' && result.type === 'compound' && result.op === '/') {
					result = expression.createCompoundExpression(result.lhs,
						sortTerms(expression.createCompoundExpression(result.rhs, terms[j], '*')),
						'/');
				} else if (node.op === '/' && terms[j].type === 'compound' &&
					terms[j].op === '/') {
					result = arithmeticEvaluation(
						expression.createCompoundExpression(result, terms[j].rhs, '*'));
					if (terms[j].lhs !== 'number' || terms[j].lhs !== 1) {
						result = expression.createCompoundExpression(result, terms[j].lhs, '/');
					}
				} else if (terms[j].type === 'compound'  && terms[j].op === '^' &&
					(utils.isUnaryNegative(terms[j].rhs) || (terms[j].rhs.type === 'number' &&
						fractionUtils.isNegative(terms[j].rhs.value)))) {
					if (node.op === '/') {
						result = expression.createCompoundExpression(result,
							invertExponent(terms[j]), '*');
					} else {
						result = expression.createCompoundExpression(result,
							flipNegativePowerIntoOneOver(terms[j]), node.op);
					}
				} else { result = expression.createCompoundExpression(result, terms[j], node.op); }
				result = flipNegativePowerIntoOneOver(result);
				printDebug('after flip: ', result, debugSort);
				result = convertFractionalExponentToSqrt(result);
				printDebug('after sqrt: ', result, debugSort);
				result = arithmeticEvaluation(result, false);
				printDebug('after arith: ', result, debugSort);
			}
		}
		return result;
	}

	/**
	 * Takes in a node that's had all the subexpressions multiplied out as far as possible, then
	 * sorted, so that the like terms are near one another.  It looks for common identifiers
	 */
	function simplifyNode(node) {
		printDebug('simplifying node: ', node, debugSimp);
		if (node.type === 'identifier' || node.type === 'number') { return node; }
		if (node.type === 'compound') {
			node = expr.makeCommutativeIfNecessary(node);
			var lhsNode = simplifyNode(node.lhs);
			var rhsNode = simplifyNode(node.rhs);
			if (lhsNode === errorNode || rhsNode === errorNode) { return errorNode; }
			node = expression.createCompoundExpression(lhsNode, rhsNode, node.op);
			var newNode = self.evaluateArithmetic(node, true);
			if (newNode !== null) { return newNode; }

			// If there was no simple arithmetic operation, see if you can find any identifiers
			// on both sides, and if there is one, move it over so that you can factor it out.
			if (node.op === '+') {
				var leftIdentifiers = getMultipliedIdentfiers(node.lhs);
				var rightIdentifiers = getMultipliedIdentfiers(node.rhs);
				var commonIdentifier = null;
				for (var i = 0; i < leftIdentifiers.length; i++) {
					for(var j = 0; j < rightIdentifiers.length; j++) {
						if (leftIdentifiers[i].syntacticEquals(rightIdentifiers[j])) {
							commonIdentifier = leftIdentifiers[i];
							break;
						}
					}
				}
				if (commonIdentifier !== null) {
					printDebug('commonIdentifier: ', commonIdentifier, debugSimp);
					var newLhs = node.lhs;
					// In case we had a-(-x), we'd like to have the two sides be
					// a and x, not a and -(-x)
					var newRhs = arithmeticEvaluation(node.rhs);
					printDebug('commLhs: ', newLhs, debugSimp);
					printDebug('commRhs: ', newRhs, debugSimp);
					newLhs = self.pullIdentfierToLhs(newLhs, commonIdentifier);
					if (newLhs.type === 'compound' && newLhs.op === '^') { return node; }
					newRhs = self.pullIdentfierToLhs(newRhs, commonIdentifier);
					if (newRhs.type === 'compound' && newRhs.op === '^') { return node; }
					newRhs = expression.createCompoundExpression(newLhs.rhs, newRhs.rhs, node.op);
					printDebug('newLhs: ', newLhs, debugSimp);
					printDebug('newRhs: ', newRhs, debugSimp);

					node = simplifyNode(expression.createCompoundExpression(
						newLhs.lhs, evaluate.evaluateRec(newRhs), '*'));
				} else if (node.op === '+' && utils.isUnaryNegative(node.rhs)) {
					node = expression.createCompoundExpression(node.lhs, node.rhs.child, '-');
				} else if (node.op === '+' && node.rhs.type === 'number' &&
					fractionUtils.isNegative(node.rhs.value)) {
					node = expression.createCompoundExpression(
						node.lhs, equality.additiveInverseOfNumber(node.rhs), '-');
				}
			}
			printDebug('returning simplified: ', node, debugSimp);
			return node;
		}
		if (node.type === 'unary') {
			var childNode = simplifyNode(node.child);
			if (childNode === errorNode) { return errorNode; }
			node = expression.createUnaryExpression(childNode, node.op);
			return arithmeticEvaluation(node, true);
		}
		if (node.type === 'ternary') {
			var left = simplifyNode(node.left);
			var middle = simplifyNode(node.middle);
			var right = simplifyNode(node.right);
			return expression.createTernaryExpression(left, middle, right, node.op1, node.op2);
		}
		console.error('about to return errorNode');
		return errorNode;
	}

	function expandFraction(node) {
		return expression.createCompoundExpression(
			expression.createSimpleExpression('number', node.value.top),
			expression.createSimpleExpression('number', node.value.bottom),
			'/');
	}

	function combineWithCoefficients(node, op) {
		printDebug('combining ', node, debugCoeff);
		if (node.type === 'compound' && op === '/' && node.rhs.type === 'number') {
			return combineWithCoefficients(expression.createCompoundExpression(
				operator.invertNode(node.rhs), node.lhs, '*'), '*');
		}
		if (node.type === 'compound' && getPrecedence(op) === 2 &&
			!utils.isOneOverSomething(node)) {
			var lhsSplit = equality.splitCoefficientFromRest(node.lhs);
			var rhsNode = equality.splitCoefficientFromRest(node.rhs);
			if (lhsSplit === null || rhsNode === null) { return node; }
			var coefficient = operator.multiplyNodesMaybeNull(lhsSplit.co, rhsNode.co, op);
			if (coefficient !== null) {
				coefficient = arithmeticEvaluation(coefficient);
				printDebug('coefficient: ', coefficient, debugCoeff);
			}
			if (op === '/' && lhsSplit.rest === null && rhsNode.rest !== null) {
				return operator.multiplyNodesMaybeNull(coefficient, rhsNode.rest, '/');
			}
			var rest = operator.multiplyNodesMaybeNull(lhsSplit.rest, rhsNode.rest, op);
			printDebug('rest: ', rest, debugCoeff);
			return operator.multiplyNodesMaybeNull(coefficient, rest, '*');
		}
		return node;
	}
	function fullMultiply(node, multiplyDivision) {
		printDebug('fullMultiplying: ', node, debugMult);
		if (node.type === 'identifier' || node.type === 'number') {
			return arithmeticEvaluation(node);
		}
		if (node.type === 'ternary') {
			return expression.createTernaryExpression(fullMultiply(node.left, multiplyDivision),
				fullMultiply(node.middle, multiplyDivision),
				fullMultiply(node.right, multiplyDivision), node.op1, node.op2);
		}
		if (node.type === 'compound') {
			node = expr.makeAdditionCommutative(node);
			var lhsNode = fullMultiply(node.lhs, multiplyDivision);
			var rhsNode = fullMultiply(node.rhs, multiplyDivision);
			printDebug('left: ', lhsNode, debugMult);
			printDebug('right: ', rhsNode, debugMult);
			if (lhsNode === errorNode || rhsNode === errorNode) { return errorNode; }
			node = expression.createCompoundExpression(lhsNode, rhsNode, node.op);
			var newNode = self.evaluateArithmetic(node, true);
			printDebug('arithmetic: ', newNode, debugMult);
			if (newNode !== null) { return newNode; }
			if (multiplyDivision) {
				node = combineWithCoefficients(node, node.op);
				printDebug('combineWithCoefficients: ', node, debugMult);
			}
			if (node.op === '/') { return groupRepeats(node, multiplyDivision); }
			if (node.op === '\\sqrt') {
				return fullMultiply(expression.createCompoundExpression(
					node.rhs, operator.invertNode(node.lhs), '^'), multiplyDivision);
			}
			//*
			if (multiplyDivision && node.op === '^' && node.rhs.type === 'compound') {
				if (getPrecedence(node.rhs.op) === 1) {
					var rightExp = node.rhs.rhs;
					if (node.rhs.op === '-') {
						rightExp = equality.additiveInverseOfNumber(rightExp);
					}
					return expression.createCompoundExpression(
						fullMultiply(
							expression.createCompoundExpression(node.lhs, node.rhs.lhs, '^'),
							multiplyDivision),
						fullMultiply(expression.createCompoundExpression(node.lhs, rightExp, '^'),
							multiplyDivision), '*');
				} else if (node.rhs.op === '*' && node.lhs.type === 'number' &&
					getCoefficient(node.rhs) !== 1) {
					//TODO(sdspikes) : think about /
					return fullMultiply(expression.createCompoundExpression(fullMultiply(
						expression.createCompoundExpression(node.lhs, node.rhs.lhs, '^'),
						multiplyDivision), node.rhs.rhs, '^'), multiplyDivision);

				}
			}//*/
			if (node.op === '^' && node.lhs.type === 'compound') {
				if (node.lhs.op === '^') {
					return fullMultiply(expression.createCompoundExpression(node.lhs.lhs,
						fullMultiply(
							expression.createCompoundExpression(node.lhs.rhs, node.rhs, '*'),
							multiplyDivision), '^'), multiplyDivision);
				} else if (getPrecedence(node.lhs.op) === 2) {
					return fullMultiply(expression.createCompoundExpression(
						fullMultiply(
							expression.createCompoundExpression(node.lhs.lhs, node.rhs, '^'),
							multiplyDivision),
						fullMultiply(
							expression.createCompoundExpression(node.lhs.rhs, node.rhs, '^'),
							multiplyDivision),
						node.lhs.op), multiplyDivision);
				} else if (getPrecedence(node.lhs.op) === 1 &&
					node.rhs.type === 'number' && !fractionUtils.isValueFraction(node.rhs.value) &&
					!fractionUtils.isValueRadical(node.rhs.value)) {
					return fullMultiply(evaluate.splitPowerNumberExponent(node), multiplyDivision);
				}
			}
			// Allows for x^(-2)-x^(-2) evaluating to 0
			if (node.op === '^') {
				if (fractionUtils.isNodeNegative(node.rhs)) {
					return operator.invertNode(fullMultiply(
						expression.createCompoundExpression(node.lhs,
							fullMultiply(expression.createUnaryExpression(node.rhs, '-'),
								multiplyDivision), '^'),
						multiplyDivision));
				}
			}
			if (node.op === '*') {
				if (!multiplyDivision) {
					if (node.lhs.type === 'compound' && node.lhs.op === '/' &&
						fractionUtils.isNodeFraction(node.rhs)) {
						return groupRepeats(expression.createCompoundExpression(fullMultiply(
							expression.createCompoundExpression(node.lhs.lhs,
								expression.createSimpleExpression('number', node.rhs.value.top),
								'*'),
							multiplyDivision),
							fullMultiply(expression.createCompoundExpression(node.lhs.rhs,
								expression.createSimpleExpression('number', node.rhs.value.bottom),
								'*'),
							multiplyDivision),
							'/'), multiplyDivision);
					}
					if (node.rhs.type === 'compound' && node.rhs.op === '/' &&
						fractionUtils.isNodeFraction(node.lhs)) {
						return groupRepeats(expression.createCompoundExpression(fullMultiply(
							expression.createCompoundExpression(node.rhs.lhs,
								expression.createSimpleExpression('number', node.lhs.value.top),
								'*'),
							multiplyDivision),
							fullMultiply(expression.createCompoundExpression(node.rhs.rhs,
								expression.createSimpleExpression('number', node.lhs.value.bottom),
								'*'),
							multiplyDivision),
							'/'), multiplyDivision);
					}
					/*
					if (fractionUtils.isNodeFraction(node.lhs)) {
						node.lhs = expandFraction(node.lhs);
					}*/
					if (node.lhs.type === 'compound' && node.lhs.op === '/') {
						var leftLeft = node.rhs;
						if (!(node.lhs.lhs.type === 'number' && node.lhs.lhs.value === 1)) {
							leftLeft = fullMultiply(expression.createCompoundExpression(
								node.lhs.lhs, node.rhs, '*'), multiplyDivision);
						}
						return expression.createCompoundExpression(leftLeft, node.lhs.rhs, '/');
					}
					if (node.rhs.type === 'compound' && node.rhs.op === '/') {
						var rightLeft = node.lhs;
						if (!(node.rhs.lhs.type === 'number' && node.rhs.lhs.value === 1)) {
							rightLeft = fullMultiply(expression.createCompoundExpression(
								node.lhs, node.rhs.lhs, '*'), multiplyDivision);
						}
						return expression.createCompoundExpression(rightLeft, node.rhs.rhs, '/');
					}
				}
				if (node.lhs.type === 'compound' && getPrecedence(node.lhs.op) === 1) {
					node.lhs = expr.makeCommutativeIfNecessary(node.lhs);
					return fullMultiply(expression.createCompoundExpression(
						fullMultiply(
							expression.createCompoundExpression(node.lhs.lhs, node.rhs, '*'),
							multiplyDivision),
						fullMultiply(
							expression.createCompoundExpression(node.lhs.rhs, node.rhs, '*'),
							multiplyDivision),
						node.lhs.op), multiplyDivision);
				}
				if (node.rhs.type === 'compound' && getPrecedence(node.rhs.op) === 1) {
					node.rhs = expr.makeCommutativeIfNecessary(node.rhs);
					return fullMultiply(expression.createCompoundExpression(
						fullMultiply(
							expression.createCompoundExpression(node.lhs, node.rhs.lhs, '*'),
							multiplyDivision),
						fullMultiply(
							expression.createCompoundExpression(node.lhs, node.rhs.rhs, '*'),
							multiplyDivision),
						node.rhs.op), multiplyDivision);
				}
			}
			node = groupRepeats(node, multiplyDivision);
		}
		if (node.type === 'unary') {
			var childNode = fullMultiply(node.child, multiplyDivision);
			if (childNode === errorNode) { return errorNode; }
			if (node.op === '\\sqrt') {
				var power = expression.createSimpleExpression('number',
					fractionUtils.createFractionValue(1, 2));
				return fullMultiply(
					expression.createCompoundExpression(childNode, power, '^'), multiplyDivision);
			}
			node = expression.createUnaryExpression(childNode, node.op);
			if (node.op !== '-') { return node;}
			var arith = self.evaluateArithmetic(node, multiplyDivision);
			return (arith !== null) ? fullMultiply(arith, multiplyDivision) : node;
		}
		printDebug('returning: ', node, debugMult);
		return node;
	}

	function leftMostTerm(node) {
		if (node.type !== 'compound' || getPrecedence(node.op) !== 1) {
			return node;
		}
		return leftMostTerm(node.lhs);
	}

	function getExponentiation(node) {
		var exponentiation = { base : node, power : 1 };
		if (node.type === 'compound' && node.op === '^' && node.rhs.type === 'number') {
			exponentiation.base = node.lhs;
			exponentiation.power = node.rhs.value;
		}
		return exponentiation;
	}
	function divideTermsIfCommonBase(dividend, divisor) {
		printDebug('checking: ', dividend, debugSynth);
		printDebug(' and ', divisor, debugSynth);
		var divisorExponentiation = getExponentiation(divisor);
		var dividendExponentiation = getExponentiation(dividend);

		if (divisorExponentiation.base.syntacticEquals(dividendExponentiation.base)) {
			if (debugSynth) {
				console.log('bases are the same! powers: ' + dividendExponentiation.power +
					' and ' + divisorExponentiation.power);
			}
			if (dividendExponentiation.power >= divisorExponentiation.power) {
				if (debugSynth) { console.log('powers right!'); }
				return arithmeticEvaluation(
					expression.createCompoundExpression(divisorExponentiation.base,
					expression.createSimpleExpression('number',
						(dividendExponentiation.power - divisorExponentiation.power)),
					'^'), false);
			}
		}
		return null;
	}
	function leftTerm(node) {
		if (utils.isSimpleExpression(node) || node.type === 'unary') { return node; }
		if (node.type === 'compound') { return node.lhs; }
	}
	function isExpandedFraction(node) {
		return (node.type === 'compound' && node.op === '/' &&
			node.lhs.type === 'number' && node.rhs.type === 'number');
	}
	function leftMostTermInMult(node) {
		if (node.numeric) { return node; }
		if (node.type !== 'compound') { return node; }
		if (isExpandedFraction(node)) { return operator['/'].evaluate(node.lhs, node.rhs); }
		if (getPrecedence(node.op) !== 2) { return node; }
		return leftMostTermInMult(node.lhs);
	}
	/**
	 * Take a numeric expression and evaluates it, ignoring any radicals that may exist.  Only does
	 * anything for all-multiplication or unary nodes, since there's no simple way to evaluate
	 * something like 1+\sqrt{2}
	 * @param  {Expression} node The numeric expression which may contain a radical.
	 * @return {Expression}      The value of all
	 */
	function allButRadical(node) {
		if (node.type === 'number') {
			if (fractionUtils.isValueRadical(node.value)) {
				return expression.createSimpleExpression('number', 1);
			}
		} else if (utils.isUnaryNegative(node)) {
			return arithmeticEvaluation(
				expression.createUnaryExpression(allButRadical(node.child), '-'));
		} else if (node.type === 'compound' && getPrecedence(node.op) === 2) {
			return arithmeticEvaluation(expression.createCompoundExpression(
				allButRadical(node.lhs), allButRadical(node.rhs), node.op));
		}
		return node;
	}
	function getCoefficient(node, includeRadical) {
		if (typeof includeRadical !== 'boolean') { includeRadical = true; }
		if (node.numeric) { return (includeRadical) ? node : allButRadical(node); }
		if (node.type === 'compound' && node.op === '/') {
			return operator['/'].evaluate(
				getCoefficient(node.lhs, includeRadical), getCoefficient(node.rhs, includeRadical));
		}
		var left = leftMostTermInMult(node);
		if (utils.isUnaryNegative(left)) {
			return equality.additiveInverseOfNumber(getCoefficient(left.child, includeRadical));
		}
		return (left.type === 'number' &&
			(includeRadical || !fractionUtils.isValueRadical(left.value))) ?
			left : expression.createSimpleExpression('number', 1);
	}

	function compareDegree(a, b) {
		var powerA = getPower(a);
		var powerB = getPower(b);
		return compareMultiplication(powerA, powerB);
	}

	function syntheticDivision(dividend, divisor, remainder) {
		printDebug('dividend: ', dividend, debugSynth);
		printDebug(' divisor: ', divisor, debugSynth);
		var leadingTermBottom = leftMostTerm(divisor);
		var termsBottom = getAllTermsAtLevelNoSort(leadingTermBottom, '*', false);

		var identifiersInLeadingTermBottom = getAllTermsAtLevelNoSort(leadingTermBottom, '*', false);
		identifierPriority = identifiersInLeadingTermBottom.sort(compareDegree).reverse()
			.map(function(node) {
				var base = getExponentiation(node).base;
				if (base.type === 'identifier') { return base.value; }
				return '';
			});

		var topTerms = getAllTermsAtLevelNoSort(
			makeCommutativeIfNecessaryRec(dividend, dividend.op), '+', true);
		topTerms.sort(compareAddition);
		identifierPriority = [];

		var leadingTermTop = topTerms[0];
		var identifiersInLeadingTermTop = getAllIdentifiersInMulitplication(leadingTermTop)
			.map(function(elem) { return elem.value; });

		var termsTop = getAllTermsAtLevelNoSort(leadingTermTop, '*', false);
		var dividedTerms = [];
		while (termsBottom.length > 0) {
			var bottomTerm = termsBottom.shift();
			var found = false;
			for (var i = 0; i < termsTop.length; i++) {
				var quo = divideTermsIfCommonBase(termsTop[i], bottomTerm);
				if (quo !== null) {
					printDebug('quo: ', quo, debugSynth);
					dividedTerms.push(quo);
					termsTop.splice(i, 1);
					found = true;
					break;
				}
			}
			if (!found) { return { result: null, success: false }; }
		}
		// We found all the terms that were in the divisor's leading term in the dividend's leading
		// term, so now we just need to put the leftover terms from dividend's leading term, and
		// divide the coeffients (if any) to get the multiplier for the divisor.
		dividedTerms = dividedTerms.concat(termsTop);
		var multTerm = operator['/'].evaluate(
				getCoefficient(leadingTermTop, false), getCoefficient(leadingTermBottom, false));
		if (debugSynth) {
			printDebug('leadingTermTop: ', leadingTermTop, debugSynth);
			printDebug('leadingTermBottom: ', leadingTermBottom, debugSynth);
			console.log('top: ' + getCoefficient(leadingTermTop, false).value + ' bottom: ' +
				getCoefficient(leadingTermBottom, false).value);
			printDebug('coefficients: ', multTerm, debugSynth);
		}
		while (dividedTerms.length > 0) {
			multTerm = expression.createCompoundExpression(multTerm, dividedTerms.shift(), '*');
			multTerm = arithmeticEvaluation(multTerm, false);
		}
		printDebug('multTerm: ', multTerm, debugSynth);
		var mult = evaluate.evaluateRec(
			expression.createCompoundExpression(multTerm, divisor, '*'), false);
		printDebug('mult: ', mult, debugSynth);
		var sub = evaluate.evaluateRec(expression.createCompoundExpression(dividend, mult, '-'));
		printDebug('sub: ', sub, debugSynth);
		if (sub.type === 'number' && sub.value === 0) {
			return { result: multTerm, success: true };
		}
		if (sub.syntacticEquals(dividend)) {
			console.log('somehow got back to myself?');
			return { result: null, success: false };
		}
		// If it didn't go in, try again with what we have left.
		var attempt = syntheticDivision(sub, divisor, remainder);
		var newTerm = attempt.result;
		if (!attempt.success && remainder) {
			if (debugSynth) { console.log('there was a remainder'); }
			newTerm = evaluate.evaluateRec(expression.createCompoundExpression(sub, divisor, '/'));
			attempt.success = true;
		}
		if (attempt.success) {
			if (debugSynth) { console.log('no remainder'); }
			attempt.result = expression.createCompoundExpression(multTerm, newTerm, '+');
		}
		return attempt;
	}
	function isFractionRhsNotNumber(node) {
		return (node.type === 'compound' && node.op === '/' && node.rhs.type !== 'number');
	}
	function pullAllTermsOverCommonDivisor(node) {
		if (node.type === 'unary') {
			var gathered = pullAllTermsOverCommonDivisor(node.child);
			if (isFractionRhsNotNumber(gathered)) {
				return expression.createCompoundExpression(
					expression.createUnaryExpression(gathered.lhs, node.op), gathered.rhs, '/');
			}
		}
		if (node.type !== 'compound' || getPrecedence(node.op) === 0 || node.op === '^') {
			return node;
		}
		var gatheredLhs = pullAllTermsOverCommonDivisor(node.lhs);
		var gatheredRhs = pullAllTermsOverCommonDivisor(node.rhs);
		if (!isFractionRhsNotNumber(gatheredLhs) && !isFractionRhsNotNumber(gatheredRhs)) {
			return node;
		}
		var topLeft = gatheredLhs;
		var topRight = gatheredRhs;
		var bottomLeft = null;
		var bottomRight = null;
		if (isFractionRhsNotNumber(gatheredLhs)) {
			topLeft = gatheredLhs.lhs;
			bottomLeft = gatheredLhs.rhs;
		}
		if (isFractionRhsNotNumber(gatheredRhs)) {
			topRight = gatheredRhs.lhs;
			bottomRight = gatheredRhs.rhs;
		}
		if (getPrecedence(node.op) === 2) {
			if (node.op === '/') {
				var temp = topRight;
				topRight = bottomRight;
				bottomRight = temp;
			}
			return expression.createCompoundExpression(
				multiplyNodesAndEvaluate(topLeft, topRight, '*'),
				multiplyNodesAndEvaluate(bottomLeft, bottomRight, '*'), '/');
		}
		if (getPrecedence(node.op) === 1) {
			var bottom = multiplyNodesAndEvaluate(bottomLeft, bottomRight, '*');
			topLeft = operator.multiplyNodesMaybeNull(topLeft, bottomRight, '*');
			topRight = operator.multiplyNodesMaybeNull(bottomLeft, topRight, '*');
			var top = evaluate.evaluateRec(
				expression.createCompoundExpression(topLeft, topRight, node.op));
			printDebug('top: ', top, debugSynth);
			printDebug('bottom: ', bottom, debugSynth);
			return expression.createCompoundExpression(top, bottom, '/');
		}
	}

	function hasNonMultiplication(node) {
		if (node.type !== 'compound') { return false; }
		if (getPrecedence(node.op) < 2) { return true; }
		var left = hasNonMultiplication(node.lhs);
		if (node.op === '^') { return left; }
		return left && hasNonMultiplication(node.rhs);
	}


	function trySyntheticDivision(node, remainder) {
		if (node.type === 'ternary') { return node; }
		var origNode = node;
		node = expr.copyNode(node);
		var before = 0;
		if (debugSynth) {
			before = 'before: ' + display.displayExpression(node, 0, 2);
		}
		node = pullAllTermsOverCommonDivisor(node);
		if (debugSynth && !origNode.syntacticEquals(node)) {
			console.log(before);
			printDebug('after: ', node, debugSynth);
		}

		if (node === errorNode || utils.isSimpleExpression(node)) { return node; }
		if (node.type === 'unary') {
			return expression.createUnaryExpression(
				trySyntheticDivision(node.child, remainder), node.op);
		}
		if (node.op === '/') {
			if (node.rhs.type === 'number') { return node; }
			var result = syntheticDivision(node.lhs, node.rhs, remainder);
			if (result.success === true) {
				if (debugSynth) { console.log('dividing whole thing worked!'); }
				result.result = sortTerms(result.result);
				printDebug('result: ', result.result, debugSynth);
				return result.result;
			}
			if (debugSynth) { console.log('dividing whole thing did not work :('); }
			return origNode;
		}
		var leftOrig = expr.copyNode(node.lhs);
		var rightOrig = expr.copyNode(node.rhs);
		var left = trySyntheticDivision(node.lhs, remainder);
		var right = trySyntheticDivision(node.rhs, remainder);
		if (!left.syntacticEquals(leftOrig) || !right.syntacticEquals(rightOrig)) {
			return self.evaluateRec(expression.createCompoundExpression(left, right, node.op));
		}
		return node;
	}

	function invertExponent(node) {
		var result = node;
		if (node.type === 'compound' && node.op === '^' && node.rhs.type === 'number' &&
			fractionUtils.isNegative(node.rhs.value)) {
			result = expression.createCompoundExpression(
				node.lhs, equality.additiveInverseOfNumber(node.rhs), '^');
		}
		if (node.type === 'compound' && node.op === '^' && utils.isUnaryNegative(node.rhs)) {
			result = expression.createCompoundExpression(node.lhs, node.rhs.child, '^');
		}
		return arithmeticEvaluation(result);
	}
	function flipNegativePowerIntoOneOver(node) {
		if (node.type === 'compound' && node.op === '^' && (utils.isUnaryNegative(node.rhs) ||
			(node.rhs.type === 'number' && fractionUtils.isNegative(node.rhs.value)))) {
			return expression.createCompoundExpression(
				expression.createSimpleExpression('number', 1),
				invertExponent(node), '/');
		}
		return node;
	}
	function convertFractionalExponentToSqrt(node) {
		if (node.type === 'compound' && node.op === '^') {
			if (node.rhs.type === 'number' && fractionUtils.isValueFraction(node.rhs.value)) {
				if (node.rhs.value === fractionUtils.createFractionValue(1, 2)) {
					return expression.createUnaryExpression(node.lhs, '\\sqrt');
				}
				return expression.createCompoundExpression(
					expression.createSimpleExpression('number', node.rhs.value.bottom),
					arithmeticEvaluation(expression.createCompoundExpression(node.lhs,
						expression.createSimpleExpression('number', node.rhs.value.top), '^')),
					'\\sqrt');
			}
			if (node.rhs.type === 'compound' && node.rhs.op === '/') {
				return expression.createCompoundExpression(node.rhs.rhs,
					arithmeticEvaluation(expression.createCompoundExpression(node.lhs,
						node.rhs.lhs, '^')),
					'\\sqrt');
			}
		}
		return node;
	}
	/**
	 *  Assumes node and other have already been run through evaluateRec.
	 */
	function equivalentEquationOrInequalityHelper(node, other) {
		printDebug('node:',  node, debugFullEq);
		printDebug('other:',  node, debugFullEq);
		if (node.type === 'compound' && other.type === 'compound') {
			var originalOp = '+';
			if (getPrecedence(node.op) === 0 && getPrecedence(other.op) === 0) {
				if (node.op !== other.op &&
					operatorProperties.getInverseInequality(node.op) !== other.op) {
					return false;
				}
				originalOp = node.op;
				var otherLeft = other.lhs;
				var otherRight = other.rhs;
				if (operatorProperties.getInverseInequality(node.op) === other.op) {
					otherLeft = other.rhs;
					otherRight = other.lhs;
				}

				node = evaluate.evaluateRec(
					expression.createCompoundExpression(node.lhs, node.rhs, '-'));
				other = evaluate.evaluateRec(
					expression.createCompoundExpression(otherLeft, otherRight, '-'));
				if (equality.equivalentModuloCommutativity(node, other, false)) { return true; }
			}
			var quotient = self.doSyntheticDivision(node, other, true);
			return (quotient.type === 'number' && (originalOp === '=' ||
				(getPrecedence(originalOp) !== 0 && quotient.value === 1) ||
				(getPrecedence(originalOp) === 0 && !fractionUtils.isNegative(quotient.value))));
		}
		if (node.type === 'ternary' && other.type === 'ternary') {
			return equivalentEquationOrInequalityHelper(
				expression.createCompoundExpression(node.left, node.middle, node.op1),
				expression.createCompoundExpression(other.left, other.middle, other.op1)) &&
			equivalentEquationOrInequalityHelper(
				expression.createCompoundExpression(node.middle, node.right, node.op2),
				expression.createCompoundExpression(other.middle, other.right, other.op2));
		}
		return false;
	}

	function putTermsWithoutIdentifierOnRhs(node, identifierNode) {
		if (node.lhs.syntacticEquals(identifierNode)) { return [node]; }
		if (node.lhs.type === 'unary') {
			if (node.lhs.op === '-') {
				// Flip direction of inequality, and make the other side negative instead
				return putTermsWithoutIdentifierOnRhs(
					expression.createCompoundExpression(node.lhs.child,
						expression.createUnaryExpression(node.rhs, '-'),
						operatorProperties.getInverseInequality(node.op)),
					identifierNode);
			}
		}
		if (node.lhs.op === '\\sqrt' || node.lhs.op === '^') {
			var newLhs = convertFractionalExponentToSqrt(node.lhs);
			if (newLhs.op === '\\sqrt') {
				var power = newLhs.lhs;
				var base = newLhs.rhs;
				if (newLhs.type === 'unary') {
					base = node.child;
					power = expression.createSimpleExpression('number', 2);
				}
				return putTermsWithoutIdentifierOnRhs(expression.createCompoundExpression(base,
					expression.createCompoundExpression(node.rhs, power, '^'), node.op),
					identifierNode);
			}
			// So it's a straight up exponent.
			if (node.lhs.rhs.type === 'number') {
				var newRhs = expression.createCompoundExpression(node.lhs.rhs, node.rhs, '\\sqrt');
				if (fractionUtils.isEven(node.lhs.rhs.value)) {
					return putTermsWithoutIdentifierOnRhs(expression.createCompoundExpression(
							node.lhs.lhs, newRhs, node.op), identifierNode).concat(
						putTermsWithoutIdentifierOnRhs(expression.createCompoundExpression(
							node.lhs.lhs, expression.createUnaryExpression(newRhs, '-'), node.op),
						identifierNode));
				} else {
					return putTermsWithoutIdentifierOnRhs(expression.createCompoundExpression(
						node.lhs.lhs, newRhs, node.op), identifierNode);
				}
			}
			// If the exponent is not a number, not sure what to do... just leave it.
			return [node];
		}
		if (node.lhs.type === 'compound') {
			if (!containsIdentifier(node.lhs.rhs, identifierNode)) {
				return putTermsWithoutIdentifierOnRhs(
					expression.createCompoundExpression(node.lhs.lhs,
					expression.createCompoundExpression(
						node.rhs, node.lhs.rhs, operatorProperties.inverseOp(node.lhs.op)),
					node.op), identifierNode);
			}
			if (!containsIdentifier(node.lhs.lhs, identifierNode)) {
				if (utils.isOneOverSomething(node.lhs)) {
					return putTermsWithoutIdentifierOnRhs(
						expression.createCompoundExpression(node.lhs.rhs, operator.invertNode(node.rhs),
							operatorProperties.getInverseInequality(node.op)),
						identifierNode);
				}
				var newLeft = expr.makeCommutativeIfNecessary(node.lhs);
				return putTermsWithoutIdentifierOnRhs(
					expression.createCompoundExpression(newLeft.rhs,
						expression.createCompoundExpression(
							node.rhs, newLeft.lhs, operatorProperties.inverseOp(node.lhs.op)),
					node.op), identifierNode);
			}
		}
		return [node];
	}

	function arithmeticEvaluation(node, expand) {
		printDebug('arithmeticEvaluation: ', node, debugArith);
		if (node.simplified) { return node; }
		var newNode = operator.rationalizeDenominator(node);
		if (!node.syntacticEquals(newNode)) { return newNode; }
		if (node.type === 'number' && fractionUtils.isValueFraction(node.value)) {
			newNode = expression.createSimpleExpression('number',
				fractionUtils.simplifyFraction(node.value));
			newNode.simplified = true;
			return newNode;
		}
		if (node.type === 'number' && fractionUtils.isValueRadical(node.value)) {
			return operator.simplifyRadicalIntoNode(node.value);
		}
		if (node.type === 'unary') {
			if (node.child.type === 'number') {
				if (fractionUtils.isValueRadical(node.child.value)) { return newNode; }
				else { newNode = operator[node.op].unaryEvaluate(node.child); }
			} else {
				var newChild = arithmeticEvaluation(node.child);
				if (utils.isUnaryNegative(node) && utils.isUnaryNegative(newChild)) {
					return newChild.child;
				}
				if (expand && utils.isUnaryNegative(node)) {
					newNode = expression.createCompoundExpression(
						expression.createSimpleExpression('number', -1),
						newChild, '*');
				} else {
					newNode = expression.createUnaryExpression(newChild, node.op);
				}
			}
			return newNode;
		}
		if (node.type !== 'compound' || getPrecedence(node.op) === 0) { return node; }
		node.lhs = expr.makeIntoFractionNodeIfApplicable(node.lhs);
		node.rhs = expr.makeIntoFractionNodeIfApplicable(node.rhs);
		if (node.lhs === errorNode || node.rhs === errorNode) { return errorNode; }
		newNode = expression.createCompoundExpression(node.lhs, node.rhs, node.op);
		if (node.lhs.numeric && node.rhs.numeric) {
			if ((node.lhs.type !== 'number' && node.rhs.type !== 'number') &&
				(!node.lhs.simplified || !node.rhs.simplified)) { return newNode; }
			newNode = operator[node.op].evaluate(node.lhs, node.rhs);
			if (!newNode.syntacticEquals(node)) {
				newNode.simplified = true;
				return newNode;
			}
		}
		if (node.rhs.type === 'number' &&
			node.rhs.value === operatorProperties[node.op].identity) {
			newNode = node.lhs;
		} else if (node.rhs.type === 'number' && node.rhs.value === 0) {
			if (node.op === '*') { newNode = node.rhs; }
			if (node.op === '^') { newNode = expression.createSimpleExpression('number', 1); }
			if (node.op === '/') { return errorNode; }
		} else if (getPrecedence(node.op) === 2 && utils.isNegativeOne(node.rhs) && !expand) {
			newNode = expression.createUnaryExpression(node.lhs, '-');
		} else if (node.op === '*' && utils.isNegativeOne(node.lhs) && !expand) {
			newNode = expression.createUnaryExpression(node.rhs, '-');
		} else if (node.op === '^' && utils.isNegativeOne(node.rhs) && !expand) {
			newNode = expression.createCompoundExpression(
				expression.createSimpleExpression('number', 1), node.lhs, '/');
		} else {
			node = expr.makeCommutativeIfNecessary(node);
			if (node.lhs.type === 'number') {
				if (operatorProperties[node.op].commutative &&
					node.lhs.value === operatorProperties[node.op].identity) {
					newNode = node.rhs;
				} else if (node.lhs.value === 0 && getPrecedence(node.op) > 1) {
					newNode = node.lhs;
				}
			}
		}
		return newNode;
	}

	var self = {
		evaluateArithmetic: function(exp, expand) {
			if (exp.simplified) { return null; }
			var parent = exp.parent;
			var side = utils.getSide(exp);
			var copy = expr.copyNode(exp);
			var node = arithmeticEvaluation(exp, expand);
			if (node === errorNode) { return errorNode; }
			if (node.syntacticEquals(copy)) { return null; }
			utils.setChild(node, parent, side);
			return node;
		},
		/**
		 * This function is used by simplify to factor out a known common identifier using the
		 * distributive property of multiplication over addition.
		 *
		 * Public for testing purposes only.
		 */
		pullIdentfierToLhs: function(node, identifierNode) {
			printDebug('Pulling ', identifierNode, debugSimp);
			printDebug(' to the left of ', node, debugSimp);
			if (node.syntacticEquals(identifierNode)) {
				node = expression.createCompoundExpression(
					node, expression.createSimpleExpression('number', 1), '*');
			} else if (utils.isUnaryNegative(node)) {
				var newChild = self.pullIdentfierToLhs(node.child, identifierNode);
				node = expression.createCompoundExpression(
					newChild, expression.createSimpleExpression('number', -1), '*');
				return self.pullIdentfierToLhs(node, identifierNode);
			} else if (node.type !== 'compound') {
				return node;
			} else if (node.op === '^' && node.rhs.type === 'number' &&
				!fractionUtils.isValueRadical(node.rhs.value)) {
				var base = node.lhs;
				if (base.syntacticEquals(identifierNode)) {
					return self.splitPowerNumberExponent(node);
				}
				base = self.pullIdentfierToLhs(node.lhs, identifierNode);
				if (base.type === 'compound' && identifierNode.syntacticEquals(base.lhs)) {
					return expression.createCompoundExpression(
						self.splitPowerNumberExponent(expression.createCompoundExpression(
							base.lhs, node.rhs, '^')),
						expression.createCompoundExpression(base.rhs, node.rhs, '^'),
						'*');
				}
				return node;
			} else if (identifierNode.syntacticEquals(node.lhs)) {
				return node;
			} else if (node.op === '*' && identifierNode.syntacticEquals(node.rhs)) {
				return self.pullIdentfierToLhs(expr.commute(node), identifierNode);
			} else {
				var newLhs = self.pullIdentfierToLhs(node.lhs, identifierNode);
				if (newLhs.type === 'compound' && identifierNode.syntacticEquals(newLhs.lhs)) {
					newLhs = expr.makeCommutativeIfNecessary(newLhs);
					node = expr.makeCommutativeIfNecessary(node);
					node = expression.createCompoundExpression(newLhs.lhs,
						expression.createCompoundExpression(newLhs.rhs, node.rhs, '*'), '*');
				} else { // pulling left failed, must be in the right
					if (node.op === '/') { identifierNode = operator.invertNode(identifierNode); }
					var newRhs = self.pullIdentfierToLhs(node.rhs, identifierNode);
					if (newRhs.type === 'compound' && identifierNode.syntacticEquals(newRhs.lhs) &&
						(node.op !== '^' || (!fractionUtils.isNodeFraction(newRhs.rhs) &&
						!fractionUtils.isValueRadical(newRhs.rhs)))) {
						printDebug('we have found it and newRhs is: ', newRhs, debugSimp);
						printDebug('at this point, node is: ', node, debugSimp);
						newLhs = newRhs.lhs;
						newRhs = expr.makeCommutativeIfNecessary(newRhs);
						if (node.op === '/') { newLhs = operator.invertNode(newLhs); }
						node = expression.createCompoundExpression(newLhs,
							expression.createCompoundExpression(node.lhs, newRhs.rhs, node.op),
							'*');
					}
				}
			}
			return node;
		},

		// Public methods
		splitPowerNumberExponent: function(node) {
			if (node.op !== '^' || node.rhs.type !== 'number' ||
				fractionUtils.isValueRadical(node.rhs.value)) { return node; }
			if (node.rhs.value === -1) {
				return expression.createCompoundExpression(
					expression.createSimpleExpression('number', 1), node.lhs, '/');
			}
			if (node.rhs.value < -1) {
				return flipNegativePowerIntoOneOver(node);
			}
			if (node.rhs.value === 0) { return expression.createSimpleExpression('number', 1); }
			if (node.rhs.value === 1) { return node.lhs; }
			if (node.rhs.value === 2) {
				return expression.createCompoundExpression(node.lhs, node.lhs, '*');
			}
			var newExponent = operator['-'].evaluateValues(node.rhs.value, 1);
			if (fractionUtils.isNodeNegative(newExponent)) { return node; }
			return expression.createCompoundExpression(node.lhs,
				expression.createCompoundExpression(node.lhs,
					newExponent, '^'), '*');
		},

		fullEquality : function(node, other) {
			return equality.equivalentModuloCommutativity(self.evaluateRec(expr.copyNode(node)),
				self.evaluateRec(expr.copyNode(other), false));
		},
		// The following functions destructively modify the expression tree.
		doSyntheticDivision: function(dividend, divisor, remainder) {
			var node = expression.createCompoundExpression(
				dividend, divisor, '/');
			var newNode = trySyntheticDivision(expression.createCompoundExpression(
				dividend, divisor, '/'), remainder);
			if (node.syntacticEquals(newNode)) {
				// if it didn't work, just evaluate it like normal, but skip the synth step since
				// we already tried it once.
				return evaluate.evaluateRec(node, false);
			}
			return newNode;
		},
		evaluateRec: function(node, doSynth) {
			if (node === errorNode) { return errorNode; }
			printDebug('init:', node, debug);
			node = fullMultiply(node, true);
			printDebug('mult1:', node, debug);
			node = sortTerms(node, true);
			printDebug('sort1:', node, debug);
			node = simplifyNode(node);
			printDebug('simp:', node, debug);
			node = fullMultiply(node, false);
			printDebug('mult2:', node, debug);
			node = sortTerms(node, false);
			printDebug('sort2:', node, debug);
			if (typeof doSynth === 'undefined' || doSynth === true) {
				node = trySyntheticDivision(node, false);
				printDebug('synth:', node, debug);
			}
			return node;
		},
		evaluateFull : function(node) {
			var parent = node.parent;
			var side = utils.getSide(node);
			var newNode = self.evaluateRec(node);
			if (newNode === errorNode) { return newNode; }
			if (newNode.syntacticEquals(node)) { return null; }
			utils.setChild(newNode, parent, side);
			return newNode;
		},
		equivalentEquationOrInequality: function(node, other) {
			node = self.evaluateRec(node);
			other = self.evaluateRec(other);
			if (equality.equivalentModuloCommutativity(node, other)) { return true; }
			return equivalentEquationOrInequalityHelper(node, other);
		},
		strictestEqualityType: function(node, other, fullEq) {
			if (node === errorNode || other === errorNode) { return equalityType.none; }
			if (node.syntacticEquals(other)) { return equalityType.verbatim; }
			if (equality.equivalentModuloCommutativity(node, other, true)) {
				return equalityType.commuteCo;
			}
			if (equality.equivalentModuloCommutativity(node, other, false)) {
				return equalityType.commute;
			}
			node = self.evaluateRec(node);
			other = self.evaluateRec(other);

			if (equality.equivalentModuloCommutativity(node, other, false)) {
				return equalityType.full;
			}
			if (fullEq && equivalentEquationOrInequalityHelper(node, other)) {
				return equalityType.fullEq;
			}
			return equalityType.none;
		},
		// Does not destructively modify anything, returns a totally new expression with no shared
		// nodes.
		substituteNodeForOtherNode: function(expression, node, other) {
			if (expression === errorNode || node === errorNode || other === errorNode) {
				return errorNode;
			}
			if (expression.syntacticEquals(node)) {
				return expr.copyNode(other);
			}
			if (utils.isSimpleExpression(expression)) { return expr.copyNode(expression); }
			if (expression.type === 'unary') {
				return expression.createUnaryExpression(
					self.substituteNodeForOtherNode(expression.child, node, other), expression.op);
			}
			if (expression.type === 'compound') {
				return expression.createCompoundExpression(
					self.substituteNodeForOtherNode(expression.lhs, node, other),
					self.substituteNodeForOtherNode(expression.rhs, node, other),
					expression.op);
			}
		},
		expandExponent: function(node) {
			var newNode = null;
			if (node !== errorNode && node.op === '^' && node.rhs.type === 'number') {
				var parent = node.parent;
				var side = utils.getSide(node);
				newNode = self.splitPowerNumberExponent(node);
				utils.setChild(newNode, parent, side);
			}
			return newNode;
		},
		solveForIdentifier: function(exp, identifier) {
			if (debugSolve) {
				console.log(display.displayExpression(exp, 0, 2));
				console.log(display.displayExpression(identifier, 0, 2));
			}
			// Only try if it's an equation (no double ineqality solving for now).
			if (exp.type !== 'compound' || getPrecedence(exp.op) !== 0 ||
				!containsIdentifier(exp, identifier)) {
				return [exp];
			}
			// First put everything on one side
			var result = expr.copyNode(exp);
			result = evaluate.evaluateRec(expression.createCompoundExpression(
				expression.createCompoundExpression(result.lhs, result.rhs, '-'),
				expression.createSimpleExpression('number', 0),
				exp.op));

			if (debugSolve) { console.log(display.displayExpression(result, 0, 2)); }
			// Now put terms containing identifier on the left and those that don't on the right,
			// as much as possible.  Returns an array in case there are multiple solutions.
			var resultArray = putTermsWithoutIdentifierOnRhs(result, identifier);
			result = [];
			for (var i = 0; i < resultArray.length; i++) {
				if (debugSolve) { console.log(display.displayExpression(resultArray[i], 0, 2)); }
				result.push(evaluate.evaluateRec(resultArray[i]));
			}
			return result;
		},
		getAllTermsAtLevel: function(node, sort) {
			node = makeCommutativeIfNecessaryRec(node, node.op);
			var terms = getAllTermsAtLevelNoSort(node, node.op, true);
			if (sort) { terms = terms.sort(compareAddition); }
			return {
				operator : node.op,
				terms : terms
			};
		},
		getAllIdentifiers: function(node) {
			if (node.type === 'identifier') { return [node.value]; }
			if (node.type === 'unary') { return self.getAllIdentifiers(node.child); }
			if (node.type === 'compound') {
				return self.getAllIdentifiers(node.lhs).concat(self.getAllIdentifiers(node.rhs));
			}
			if (node.type === 'ternary') {
				return self.getAllIdentifiers(node.left).concat(
					self.getAllIdentifiers(node.middle).concat(self.getAllIdentifiers(node.right)));
			}
			return [];
		}
	};

	return self;
}());

if (typeof module !== 'undefined' && typeof exports !== 'undefined') {
	exports.evaluate = evaluate;
	exports.equalityType = equalityType;
}