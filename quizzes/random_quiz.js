/*global require:true exports:true */

if (typeof module !== 'undefined' && typeof require !== 'undefined') {
	var quizUtils = require('./quizUtils.js').quizUtils;
}

var quizInteraction = (function() {

	/**
	 * Constructor for the object that store all the relevant info.  Use it to fill in the display.
	 * @param {Number} total                        How many problems the user has gotten right.
	 * @param {Number} streak                       How many problems the user has done in a row
	 *                                              without making any mistakes.
	 * @param {function} problemGenerator           No-arg function which returns a
	 *                                              ProblemDefinition.
	 * @param {ProblemDefinition} problemDefinition Contains all the relevant info about this
	 *                                              problem.
	 */
	function QuizProperties(total, streak, problemGenerator, problemDefinition) {
		this.total = total;
		this.streak = streak;
		this.problemGenerator = problemGenerator;
		this.problemDefinition = problemDefinition;
	}
	var quizProperties = null;

	function updateHistoryWithComment(latex, comment) {
		quizProperties.problemDefinition.history.push(latex);
		$('#show-history').show();
		var outerDiv = $(document.createElement('div'));
		var expressionSpan = $('<span class="expression"></span>');
		$('#show-history').append(outerDiv);
		outerDiv.append(
			$('<img src="/media/img/site/icons/udacity/math_practice_checkmark.png" />'));
		outerDiv.append(expressionSpan);
		expressionSpan.mathquill().mathquill('latex', latex);
		var textSpan = $('<span class="response right" id="comment"></span>');
		textSpan.text(comment);
		outerDiv.append(textSpan);
	}

	function displayStats() {
		$('#total').text(quizProperties.total);
		$('#streak').text(quizProperties.streak);
	}

	function hideAllAndDisplayIndex() {
		console.error('quiz type does not exist.');
		$('#mathquill-input').hide();
		$('#check-things').hide();
		$('#continue').hide();
		$('#skip').hide();
		$('#reset').hide();
		$('#simple-prompt').hide();

		$('#custom-prompt').show();
		$('#custom-prompt').text('The type specified in the url is not defined.');
		$('#custom-prompt').append(' Please see the ');
		$('#custom-prompt').append($('<a href="/quiz?type=index">quiz index</a>'));
		$('#custom-prompt').append(' for a list of valid quizzes.');
	}

	function setupProblem() {
		$('#show-history').empty();
		$('#mathquill-input').show();
		$('#check-things').show();
		$('#continue').hide();
		$('#skip').show();
		$('#reset').show();

		if (quizProperties.problemDefinition.multiStep) {
			$('#single-answer').hide();
			$('#intermediate-answers').show();
		} else {
			$('#reset').hide();
			$('#single-answer').show();
			$('#intermediate-answers').hide();
		}

		$('#mathquill-input').mathquill('latex',
			quizProperties.problemDefinition.getLastAttempt());
		$('#mathquill-input').focus();

		if (quizProperties.problemDefinition.customPrompt !== null) {
			$('#simple-prompt').hide();
			var promptDiv = $('#custom-prompt');
			promptDiv.show();
			promptDiv.empty();
			for (var i = 0; i < quizProperties.problemDefinition.customPrompt.length; i++) {
				if (quizProperties.problemDefinition.customPrompt[i].math) {
					var mathquillSpan = $(document.createElement('span'));
					mathquillSpan.mathquill().mathquill('latex',
						quizProperties.problemDefinition.customPrompt[i].value);
					promptDiv.append(mathquillSpan);
				} else {
					promptDiv.append(quizProperties.problemDefinition.customPrompt[i].value);
				}
			}
		} else {
			// Fill in the div that shows the question, and get MathQuill to render it.
			$('#question').mathquill().mathquill('latex',
				quizProperties.problemDefinition.questionString);
		}

		for (var j = 0; j < quizProperties.problemDefinition.history.length; j++) {
			updateHistoryWithComment(quizProperties.problemDefinition.history[j], '');
			$('#comment').remove();
		}
	}

	function submitOnEnterKey(e) {
		if (e.keyCode === 13) {
			e.preventDefault();
			self.checkExpressions();
		}
	}

	var self = {
		getQuizProperties: function() { return quizProperties; },

		// This takes the url query param type and stores it so we know what kind of quiz to
		// serve. The default, if the query param is not present, is positive_integer.
		firstTimeSetup : function(quizType, quizProperties) {
			$('#mathquill-input').keydown(submitOnEnterKey);
			$('#mathquill-input').focus();

			if (typeof quizProperties === 'undefined') {
				quizProperties = new QuizProperties(
					0, 0, quizUtils.getQuizGenerator(quizType), null);
			}

			// This should be triggered if the url contains type=(something that is not defined).
			if (typeof quizProperties.problemGenerator === 'undefined') {
				quizProperties.problemGenerator = function() { return null; };
				hideAllAndDisplayIndex();
			}
			// Otherwise it is a known type, so just set up the problem, making a new one if
			// none is present.
			else {
				if (quizProperties.problemDefinition === null) {
					quizProperties.problemDefinition = quizProperties.problemGenerator();
				}
				setupProblem();
			}
			displayStats();
			return quizProperties;
		},

		setupNewProblem : function() {
			quizProperties.problemDefinition = quizProperties.problemGenerator();
			setupProblem();
		},

		checkExpressions : function() {
			$('#comment').remove();
			$('#right-response').empty();
			$('#wrong-response').empty();

			var correct = true;
			// This gets the text the student wrote into the mathquill box, in latex format.
			var studentResponse = $('#mathquill-input').mathquill('latex');
			var answerAndComment = quizUtils.checkExpressions(studentResponse,
				quizProperties.problemDefinition);
			if (answerAndComment.correct === quizUtils.answerCategory.incorrect) {
				correct = false;
				$('#wrong-response').text(answerAndComment.comment);
				quizProperties.streak = 0;
			} else if (answerAndComment.correct === quizUtils.answerCategory.unchanged) {
				$('#right-response').text(answerAndComment.comment);
			} else if (answerAndComment.correct === quizUtils.answerCategory.intermediate) {
				updateHistoryWithComment(studentResponse, answerAndComment.comment);
			} else if (answerAndComment.correct === quizUtils.answerCategory.complete) {
				quizProperties.streak++;
				quizProperties.total++;
				if (quizProperties.problemDefinition.useContinue) {
					updateHistoryWithComment(studentResponse, answerAndComment.comment);
					$('#mathquill-input').hide();
					$('#check-things').hide();
					$('#continue').show();
					$('#continue').focus();
					$('#skip').hide();
					$('#reset').hide();
				} else {
					$('#mathquill-input').mathquill('latex','');
					$('#right-response').text('Great job! See above for new problem.');
					$('#show-history').empty();
					self.setupNewProblem();
				}
			}
			displayStats();
			return {
				'correct' : correct,
				'comment' : answerAndComment.comment,
				'state' : quizProperties
			};
		},

		reset : function() {
			$('#mathquill-input').mathquill('latex',
				quizProperties.problemDefinition.getLastAttempt());
			$('#comment').remove();
			$('#wrong-response').empty();
		},

		skip : function() {
			quizProperties.streak = 0;
			$('#streak').text(quizProperties.streak);
			$('#wrong-response').empty();
			$('#right-response').empty();
			$('#show-history').empty();
			self.setupNewProblem();
		}
	};
	return self;
}());

if (typeof exports !== 'undefined') {
	exports.quizInteraction = quizInteraction;
}