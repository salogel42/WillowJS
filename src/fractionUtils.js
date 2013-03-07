/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var utils = require('./utils.js').utils;
	var errorNode = require('./utils.js').errorNode;
}

var fractionUtils = (function() {
	var primeList = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
		73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
		173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269,
		271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379,
		383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487,
		491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607,
		613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727,
		733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853,
		857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977,
		983, 991, 997];
	function factorInteger(n) {
		if (!self.isInteger(n)) { return []; }
		n = Math.abs(n);
		var factorList = [];
		for (var i = 0; i < primeList.length; i++) {
			var quo = n/primeList[i];
			var newEntry = [primeList[i], 0];
			while (self.isInteger(quo)) {
				newEntry[1]++;
				n = quo;
				quo = n/primeList[i];
			}
			if (newEntry[1] !== 0) {
				factorList.push(newEntry);
			}
			if (n === 1) {
				return factorList;
			}
		}
		if (n !== 1) {
			console.error('The number was too big to factor');
		}
		return factorList;
	}



	var self = {
		isInteger: function(num) { return num === Math.floor(num); },
		isEven : function(num) { return Math.floor(num/2) === num/2; },
		getGcd: function(a, b) {
			if (a % b === 0) { return b; }
			return self.getGcd(b, a % b);
		},
		getLcm: function(a, b) { return a * b / self.getGcd(a, b); },

		numericalRoot: function(radicand, power) {
			if (typeof power === 'undefined') { power = 1/2; }
			if (self.isValueFraction(power)) { power = power.top/power.bottom; }
			if (!fractionUtils.isValueFraction(radicand)) {
				return Math.pow(radicand, power);
			} else {
				return fractionUtils.simplifyFraction(self.createFractionValue(
					Math.pow(radicand.top, power), Math.pow(radicand.bottom, power)));
			}
		},

		combineRadicals: function(left, right) {
			if (fractionUtils.isValueRadical(left) && fractionUtils.isValueRadical(right)) {
				if (!fractionUtils.compareFractions(left.power, right.power)) {
					if (fractionUtils.isValueRadical(left.power) ||
						fractionUtils.isValueRadical(right.power)) {
						return errorNode;
					}
					// Assume that all radicals have fractional powers because why would they have
					// integer powers? then they wouldn't be radicals.
					left.power = fractionUtils.simplifyFraction(left.power);
					right.power = fractionUtils.simplifyFraction(right.power);
					if (left.power.bottom !== right.power.bottom) {
						var lcm = fractionUtils.getLcm(left.power.bottom, right.power.bottom);
						left.power = fractionUtils.createFractionValue(
							left.power.top * lcm/left.power.bottom, lcm);
						right.power = fractionUtils.createFractionValue(
							right.power.top * lcm/right.power.bottom, lcm);
					}
					if (left.power.top !== 1) {
						left.radicand = Math.pow(left.radicand, left.power.top);
						left.power.top = 1;
					}
					if (right.power.top !== 1) {
						right.radicand = Math.pow(right.radicand, right.power.top);
						right.power.top = 1;
					}
				}
				return self.createRadicalValue(
					self.multiplyFractions(left.radicand, right.radicand), left.power);
			}
			return errorNode;
		},

		simplifyRadical: function(radicalValue) {
			if (radicalValue.simplified) { return radicalValue; }
			radicalValue.simplified = true;
			if (self.isValueRadical(radicalValue.power)) { return radicalValue; }
			var comp = self.numericalRoot(radicalValue.radicand, radicalValue.power);
			if (self.isInteger(comp) ||
				(self.isValueFraction(comp) && self.isInteger(comp.top) && self.isInteger(comp.bottom))) {
				return comp;
			}
			// don't think this should ever happen...
			if (!self.isValueFraction(radicalValue.power)) { return radicalValue; }
			var root = radicalValue.power.bottom;
			if (!self.isInteger(root)) { return radicalValue; }

			// Either it didn't come out even due to rounding or it's got a surd in it.
			// Either way, we now look for bits to pull out.
			var newValue = self.createRadicalValue(
				self.numericalRoot(radicalValue.radicand, radicalValue.power.top),
				self.createFractionValue(1, root), true);


			var mixed = self.createMixedValue(1, newValue);

			var rootFactors = factorInteger(root);
			if (rootFactors.length > 1) {
				for (var j = 0; j < rootFactors.length; j++) {
					while (rootFactors[j][1] > 0) {
						var testValue = self.createRadicalValue(mixed.radical.radicand,
							self.createFractionValue(1, rootFactors[j][0]));
						var testResult = self.simplifyRadical(testValue);
						if (fractionUtils.isValueFraction(testResult) || self.isInteger(testResult)) {
							mixed.rational = self.multiplyFractions(mixed.rational, testValue);
							mixed.radical.power = self.createFractionValue(1,
								mixed.radical.power/rootFactors[j][0]);
							rootFactors[j][1]--;
						} else {
							break;
						}
					}
				}
			}


			var factors = factorInteger(newValue.radicand);

			for (var i = 0; i < factors.length; i++) {
				while (factors[i][1] >= root) {
					mixed.rational *= factors[i][0];
					mixed.radical.radicand /= Math.pow(factors[i][0], root);
					factors[i][1] -= root;
				}
			}
			if (mixed.radical.radicand === 1) { return mixed.rational; }
			if (mixed.rational === 1) { return mixed.radical; }
			return mixed;
		},
		abs: function(val) {
			if (!self.isValueFraction(val)) {
				return Math.abs(val);
			}
			return self.createFractionValue(Math.abs(val.top), Math.abs(val.bottom));
		},
		fractionGCD: function(a, b) {
			if (self.isValueRadical(a) || self.isValueRadical(b)) { return 1; }
			a = self.simplifyFraction(a);
			b = self.simplifyFraction(b);
			if (self.compareFractions(a, b, true)) { return a; }
			if (self.isValueFraction(a) && self.isValueFraction(b)) {
				if (a.bottom === b.bottom) {
					return self.createFractionValue(self.getGcd(a.top, b.top), a.bottom);
				}
				return 1;
			}
			if (self.isValueFraction(a)) {
				return self.getGcd(a.top, b);
			}
			if (self.isValueFraction(b)) {
				return self.getGcd(a, b.top);
			}
			return self.getGcd(a, b);
		},
		multiplyFractions: function(lhsValue, rhsValue) {
			if (fractionUtils.isValueFraction(rhsValue) &&
				fractionUtils.isValueFraction(lhsValue)) {
				return fractionUtils.simplifyFraction(fractionUtils.createFractionValue(
					lhsValue.top * rhsValue.top, lhsValue.bottom * rhsValue.bottom));
			} else if (fractionUtils.isValueFraction(lhsValue)) {
				return self.multiplyFractions(rhsValue, lhsValue);
			} else if (fractionUtils.isValueFraction(rhsValue)) {
				return fractionUtils.simplifyFraction(
					fractionUtils.createFractionValue(lhsValue * rhsValue.top, rhsValue.bottom));
			}
			return lhsValue * rhsValue;
		},
		// Used only by operator
		addFractions: function(lhsValue, rhsValue) {
			if (lhsValue.bottom !== rhsValue.bottom) {
				var lcm = self.getLcm(lhsValue.bottom, rhsValue.bottom);
				lhsValue = self.createFractionValue(lhsValue.top * lcm / lhsValue.bottom, lcm);
				rhsValue = self.createFractionValue(rhsValue.top * lcm / rhsValue.bottom, lcm);
			}
			var top = lhsValue.top + rhsValue.top;
			if (top === 0) { return 0; }
			return self.simplifyFraction(self.createFractionValue(top, lhsValue.bottom));
		},

		// Also used by evaluate.
		invertFraction: function(fractionValue) {
			if (!self.isValueFraction(fractionValue)) {
				return self.createFractionValue(1, fractionValue);
			}
			if (fractionValue.top === 1) {
				return fractionValue.bottom;
			}
			return self.createFractionValue(fractionValue.bottom, fractionValue.top);
		},

		// Used widely
		isNegative: function(maybeFractionValue) {
			if (self.isValueRadical(maybeFractionValue)) { return false; }
			var simplified = self.simplifyFraction(maybeFractionValue);
			if (!self.isValueFraction(simplified)) { return simplified < 0; }
			return simplified.top < 0;
		},
		compareFractions: function(left, right, decimal) {
			if (self.isValueFraction(left) && self.isValueFraction(right)) {
				return self.compareFractions(left.top, right.top) ?
					self.compareFractions(left.bottom, right.bottom) : false;
			} else if (self.isValueRadical(left) && self.isValueRadical(right)) {
				return self.compareFractions(left.radicand, right.radicand) ?
					self.compareFractions(left.power, right.power) : false;
			} else if (decimal) {
				if (self.isValueRadical(left) || self.isValueRadical(right)) {
					return false;
				} else if (self.isValueFraction(right)) {
					return self.compareFractions(right, left, true);
				} else if (self.isValueFraction(left)) {
					var simplified = self.simplifyFraction(left);
					if (!self.compareFractions(simplified, left, false)) { return false; }
					return left.top/left.bottom === right;
				}
			}
			return left === right;
		},
		computeApproxNumericValue: function(value) {
			if (self.isValueMixed(value)) {
				return self.computeApproxNumericValue(value.radical) *
					self.computeApproxNumericValue(value.rational);
			} else if (self.isValueRadical(value)) {
				return self.numericalRoot(value.radicand, value.power);
			} else if (self.isValueFraction(value)) {
				return value.top/value.bottom;
			}
			return value;

		},
		isValueFraction: function(value) {
			return (typeof value.top !== 'undefined' && typeof value.bottom  !== 'undefined');
		},
		isValueRadical: function(value) {
			return (typeof value.radicand !== 'undefined' && typeof value.power  !== 'undefined');
		},
		isValueMixed: function(value) {
			return (typeof value.rational !== 'undefined' &&
				typeof value.radical  !== 'undefined');
		},
		createFractionValue: function(top, bottom) { return { 'top' : top, 'bottom': bottom }; },
		createRadicalValue: function(radicand, power, simplified) {
			return { 'radicand' : radicand, 'power': power, 'simplified': simplified };
		},
		createMixedValue: function(rational, radical) {
			return { 'rational' : rational, 'radical': radical };
		},
		simplifyFraction: function(fractionValue) {
			if (!self.isValueFraction(fractionValue)) { return fractionValue; }
			if (self.isValueRadical(fractionValue.top) ||
				self.isValueRadical(fractionValue.bottom)) {
				return fractionValue;
			}
			if (fractionValue.top === 0) { return 0; }
			if (fractionValue.bottom === 0) {
				console.error('Attempted to divide by 0');
				return errorNode;
			}
			if (!self.isInteger(fractionValue.top) || !self.isInteger(fractionValue.bottom)) {
				return fractionValue.top/fractionValue.bottom;
			}
			var newValue = self.createFractionValue(fractionValue.top, fractionValue.bottom);
			var gcd = self.getGcd(newValue.top, newValue.bottom);
			newValue.top /= gcd;
			newValue.bottom /= gcd;
			if (newValue.bottom < 0) {
				newValue.top *= -1;
				newValue.bottom *= -1;
			}
			if (newValue.bottom === 1) { return newValue.top; }
			return newValue;
		},
		isNegativeNumber: function(node) {
			return (node !== undefined && node.type === 'number' &&
				self.isNegative(node.value));
		},
		isNodeFraction: function(node) {
			return (node.type === 'number' && self.isValueFraction(node.value));
		},
		isNodeNegative: function(node) {
			return utils.isUnaryNegative(node) || self.isNegativeNumber(node);
		},
		/**
		 * This should only ever be called if fractionUtils.isNegative(value) is true,
		 * so it should never be called on a radical.
		 *
		 * This function should work on unprocessed values (i.e. not simplified), and should
		 * not simplify the values.
		 *
		 * @param  {number|fractionValue} value A value that is known to be negative.
		 * @return {number|fractionValue}       The additive inverse of the value.
		 */
		additiveInverseOfFraction: function(value) {
			if (self.isValueRadical(value)) { return errorNode; }
			if (self.isValueFraction(value)) {
				if (self.isNegative(value.top)) {
					return self.createFractionValue(self.additiveInverseOfFraction(value.top),
						value.bottom);
				}
				if (self.isNegative(value.bottom)) {
					return self.createFractionValue(value.top,
						self.additiveInverseOfFraction(value.bottom));
				}
			}
			return -1 * value;
		}
	};

	return self;
}());

if (typeof exports !== 'undefined') { exports.fractionUtils = fractionUtils; }