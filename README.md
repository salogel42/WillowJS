WillowJS
========

Math expression parser/evaluator/display engine used by Udacity quizzes.

If you want to be able to run commands, tests, or build the libraries (basically anything other than just using
them unminified in a browser), you'll need to install [node.js](http://nodejs.org/).  You'll probably also want to run

	npm install

Then, to run the tests (from the base directory), do

	jasmine-node willowjs/tests

If you want to create uglified (minified) versions of the js files in `build/` (i.e. to use on a website)
run the `grunt` command.

If you want to use the library from command line, run something like

	node src/nodeWrapper.js simplify newline '2-x^2-3x-5x^2y+\frac{4x}{3}-6'

See the `nodeWrapper.js` file itself for docs on the available commands.
