WillowJS
========

Math expression parser/evaluator/display engine used by Udacity quizzes.

If you want to be able to run commands, tests, or build the libraries (basically anything other than just using
them unminified in a browser), you'll need to install [node.js](http://nodejs.org/).  You'll probably also want to run

	sudo npm install

###Running commands

If you want to use the library from command line, run something like

	node src/nodeWrapper.js simplify newline '2-x^2-3x-5x^2y+\frac{4x}{3}-6'

See the [`nodeWrapper.js`](https://github.com/udacity/WillowJS/blob/master/src/nodeWrapper.js) file itself for
docs on the available commands.

###Uglify (minify/obfuscate) the code

To create uglified (minified) versions of the js files (i.e. to use on a website), run the following:
	grunt

This will dump the uglified version of the files into the `build/` folder.

###Tests

####Tests run with node.js
To run the tests (from the base directory), do

	jasmine-node tests


####Tests run in the browser
To run tests continuously in a browser (currently configured for Chrome), run

	karma start

Note that if you have not run `sudo npm install` since May 29, 2013, you will need to run it again.  If the above
command still doesn't work, try doing `rm -rf node_modules` then doing `npm install` again.

I also have karma configured to determine test coverage.  To see the test coverage, use a browser to look at the
generated coverage file.  For me, this is at

	file:///Users/sdspikes/code/willowjs/tests/coverage/Chrome%2027.0%20(Mac)/src/index.html

(you will need to change the base path based on where the willowjs repo lives on your computer)

###Call to action

Note: this is my first project in js and probably uses awful js style, and definitely needs to be restructured
and better documented.  That's why we aren't open sourcing it yet, but if you guys can help me make it presentable,
more robust, and actually useable, then I'll be able to open it up.  Even if all you do is play with it until you find
a few bugs to report to me, you're still helping :)
