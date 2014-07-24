module.exports = function(grunt) {
  grunt.initConfig({
    compass: {
      dist: {
        options: {
          config: 'config.rb'
        }
      }
    },

    cssmin: {
      def: {
        src: 'static/assets/main.css',
        dest: 'static/assets/styles.min.css'
      }
    },

    uglify: {
      app: {
        src:  'static/assets/app.js',
        dest: 'static/assets/app.min.js'
      }
    },

    concat: {
      vendor: {
        src: [
          'bower_components/jquery/dist/jquery.min.js',
          'bower_components/angular/angular.min.js',
          'bower_components/angular-route/angular-route.min.js',
          'bower_components/angular-cookies/angular-cookies.min.js',
          'bower_components/angular-truncate/dist/angular-truncate.min.js',
          'bower_components/momentjs/min/moment.min.js',
        ],
        dest: 'static/assets/vendor.min.js'
      },
      app: {
        src: [
          'javascripts/landing.js',
          'javascripts/app.js'
        ],
        dest: 'static/assets/app.js'
      },
      dev: {
        src: [
          'static/assets/vendor.min.js',
          'static/assets/app.js'
        ],
        dest: 'pstatic/scripts.js'
      },
      prod: {
        src: [
          'static/assets/vendor.min.js',
          'static/assets/app.min.js'
        ],
        dest: 'static/assets/scripts.min.js'
      }
    },

    clean: {
      vendor: [ 'static/assets/vendor.min.js' ],
      css:    [ 'static/assets/**/*.css' ],
      js:     [
        'static/assets/app.js',
        'static/assets/app.min.js',
        'static/assets/scripts.js',
        'static/assets/scripts.min.js'
      ],
      extra:  [
        'static/assets/main.css',
        'static/assets/app.js',
        'static/assets/app.min.js'
      ]
    },

    watch: {
      dev: {
        files: [
          'sass/**/*.scss',
          'javascripts/**/*.js'
        ],
        tasks: [
          'clean:css', 'compass', 'cssmin',
          'clean:js', 'concat:app', 'concat:dev', 'clean:extra'
        ]
      },
      prod: {
        files: [
          'sass/**/*.scss',
          'javascripts/**/*.js'
        ],
        tasks: [
          'clean:css', 'compass', 'cssmin',
          'clean:js', 'concat:app', 'uglify:app', 'concat:prod', 'clean:extra'
        ]
      }
    },

    imageoptim: {
      dist: {
        src: ['static/images']
      }
    },
  });

  grunt.loadNpmTasks('grunt-imageoptim');
  grunt.loadNpmTasks('grunt-contrib-clean');
  grunt.loadNpmTasks('grunt-contrib-compass');
  grunt.loadNpmTasks('grunt-contrib-concat');
  grunt.loadNpmTasks('grunt-contrib-cssmin');
  grunt.loadNpmTasks('grunt-contrib-uglify');
  grunt.loadNpmTasks('grunt-contrib-watch');


  grunt.registerTask('default',    ['clean', 'concat:vendor', 'watch:dev']);
  grunt.registerTask('production', ['clean', 'concat:vendor', 'watch:prod']);
  grunt.registerTask('release', ['clean:css', 'compass', 'cssmin', 'clean:js', 'concat:app', 'uglify:app', 'concat:prod', 'clean:extra']);
};