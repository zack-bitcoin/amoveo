"use strict";

const {
	series
} = require('gulp');
const gulp = require('gulp'),
	del = require('del'),
	bulkSass = require('gulp-sass-bulk-import'),
	postcss = require('gulp-postcss'),
	sass = require('gulp-sass'),
	autoprefixer = require('autoprefixer'),
	mqpacker = require('css-mqpacker'),
	csso = require('gulp-csso'),
	extend = require('extend'),
	cache = require('gulp-cache'),
	imagemin = require('gulp-imagemin'),
	pngquant = require('imagemin-pngquant'),
	svgstore = require('gulp-svgstore'),
	svgmin = require('gulp-svgmin'),
	concat = require('gulp-concat'),
	uglify = require('gulp-uglify-es').default,
	rename = require('gulp-rename');
//concat       = require('gulp-concat');

//var config = extend(true, require('./gulp-config.js'));
var config = require('./gulp-config.js');

// The `clean` function is not exported so it can be considered a private task.
// It can still be used within the `series()` composition.
function clean(done) {
	del(config.basedir);
	done();
}

function styles(done) {
	return gulp.src(config.paths.styles.src)
		.pipe(bulkSass())
		.pipe(sass())
		.pipe(postcss([
			require('css-mqpacker')({
				sort: true
			}),
			require('autoprefixer')({
				browsers: ['last 2 versions', '> 1%', 'ie 9']
			})
		]))
		.pipe(csso())
		.pipe(rename({
			suffix: '.min'
		}))
		.pipe(gulp.dest(config.paths.styles.dest))
	done();
}

function script(done) {
	return gulp.src(config.paths.js.src)
		.pipe(concat('common.js'))
		//.pipe(uglify({
			//outSourceMap: true
		//}))
		//.pipe(rename({suffix: '.min'}))
		.pipe(gulp.dest(config.paths.js.dest))
	done();
}

function images(done) {
	return gulp.src(config.paths.img.src)
		.pipe(cache(imagemin({
			interlaced: true,
			progressive: true,
			svgoPlugins: [{
				removeViewBox: false
			}],
			use: [pngquant()]
		})))
		.pipe(gulp.dest(config.paths.img.dest))

	done();
}

function svg(done) {
	return gulp.src(config.paths.svg.src)
		.pipe(svgmin(function(file) {
			return {
				plugins: [{
					cleanupIDs: {
						minify: true
					}
				}]
			}
		}))
		.pipe(svgstore())
		.pipe(rename({
			basename: "sprite",
			prefix: "svg-",
			//suffix: "-",
			//extname: ".svg"
		}))
		.pipe(gulp.dest(config.paths.svg.dest))

	done();
}

// Watch
gulp.task('watch:styles', function() {
	gulp.watch(config.paths.watch.styles, gulp.series('styles'));
});
gulp.task('watch:script', function() {
	gulp.watch(config.paths.js.src, gulp.series('script'));
});

// The `build` function is exported so it is public and can be run with the `gulp` command.
// It can also be used within the `series()` composition.
function build(done) {
	// body omitted
	done();
}

exports.clean = clean;
exports.styles = styles;
exports.script = script;
exports.images = images;
exports.svg = svg;
exports.build = series(clean, styles, script, images, svg);
exports.default = series(styles);
