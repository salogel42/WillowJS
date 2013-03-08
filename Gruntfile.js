module.exports = function(grunt) {
  var u = require('underscore');
  var uglifyObj = {
    options: {
      banner: '/*! <%= pkg.name %> <%= grunt.template.today("yyyy-mm-dd") %> */\n'
    }
  };
  u.each(grunt.file.expandMapping('*.js', 'build', {cwd: 'src/'}), function (fileMap) {
    uglifyObj[fileMap.src] = fileMap;
  });


  // Project configuration.
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    uglify: uglifyObj
  });

  // Load the plugin that provides the "uglify" task.
  grunt.loadNpmTasks('grunt-contrib-uglify');

  // Default task(s).
  grunt.registerTask('default', ['uglify']);

};