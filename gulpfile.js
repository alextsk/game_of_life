var path = require('path');
var gulp = require('gulp');
var concat = require('gulp-concat');
var del	= require('del');
var webpack = require("webpack");
var webpack$ = require('webpack-stream');
var gutil = require('gulp-util');
var sass = require('gulp-sass');
var browserSync = require('browser-sync');

var rebaseUrls = require('gulp-css-rebase-urls');
var changed = require('gulp-changed');
var ghPages = require('gulp-gh-pages');
 
const DEST = './dist',
			SRC = './src/**/*'
gulp.task('webpack', function() {
	gulp.src(['./src/index.js'])
		.pipe(changed('./dist/'))
		.pipe(webpack$({
			 module: {
				rules : [
					{
			      test:    /\.elm$/,
			      exclude: [/elm-stuff/, /node_modules/],
			      loader:  'elm-webpack-loader?verbose=true&warn=true',
			    }
		    ],
	      noParse: /\.elm$/
			} 
		}, webpack))
		.pipe(concat('app.js'))
		.pipe(gulp.dest('./dist/'))
		.pipe(browserSync.reload({stream: true}))
});

gulp.task('sass', function() {
	return gulp.src('./src/**/*.scss')
		.pipe(sass().on('error', sass.logError))
		.pipe(rebaseUrls())
		.pipe(gulp.dest('./dist'))
		.pipe(browserSync.reload({stream:true}))
});

gulp.task('fonts', function() {
	gulp.src('./assets/**/*')
	.pipe(gulp.dest('./dist/assets'))
});

gulp.task('html', function() {
	gulp.src('./src/**/*.html')
	.pipe(gulp.dest('dist/'))
	.pipe(browserSync.reload({stream: true}))
})

gulp.task('clear', function() {
	del(['dist/**', '!dist']).then(paths => {
    console.log('Deleted files and folders:\n', paths.join('\n'));
	});
});

gulp.task('browser-sync', function() {
	browserSync({
		server: {
			baseDir: 'dist'
		},
		notify: false,
		// tunnel: true,
		// tunnel: "projectmane", //Demonstration page: http://projectmane.localtunnel.me
	});
});

gulp.task('watch', ['sass', 'webpack', 'html', 'fonts', 'browser-sync'], function() {
	gulp.watch('src/**/*.scss', ['sass'] );
	gulp.watch(['src/**/*.elm'], ['webpack']);
	gulp.watch('src/*.html', ['html']);
});

gulp.task('deploy', function() {
  return gulp.src('./dist/**/*')
    .pipe(ghPages())
});

gulp.task('default', ['watch']);