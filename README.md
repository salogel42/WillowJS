WillowJS
========

Math expression parser/evaluator/display engine used by Udacity quizzes.

If you want to be able to run commands, tests, or build the libraries (basically anything other than just using
them unminified in a browser), you'll need to install [node.js](http://nodejs.org/).  You'll probably also want to run

	npm install

Then, to run the tests (from the base directory), do

	jasmine-node tests

If you want to create uglified (minified) versions of the js files in `build/` (i.e. to use on a website)
run the `grunt` command.

If you want to use the library from command line, run something like

	node src/nodeWrapper.js simplify newline '2-x^2-3x-5x^2y+\frac{4x}{3}-6'

See the `nodeWrapper.js` file itself for docs on the available commands.


Note: this is my first project in js and probably uses awful js style, and definitely needs to be restructured
and better documented.  That's why we aren't open sourcing it yet, but if you guys can help me make it presentable,
more robust, and actually useable, then I'll be able to open it up.  Even if all you do is play with it until you find
a few bugs to report to me, you're still helping :)  Thanks! ~Sarah
