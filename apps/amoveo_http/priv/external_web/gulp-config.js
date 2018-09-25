"use strict";

const config = {};

config.basedir = 'dest/';
config.srcdir = 'src/';

config.paths = {
    styles: {
		src: config.srcdir+'scss/style.scss',
		dest: config.basedir+'styles/'
    },
    js: {
		src: [
            config.srcdir+'js/common.js',
        ],
		dest: config.basedir+'js/'
    },
	img: {
		src: config.srcdir+'img/**/*.+(png|jpg|gif|svg)',
		dest: config.basedir+'img/'
    },
	svg: {
		src: config.srcdir+'svg/**/sprite-*.svg',
		dest: config.basedir+'svg/'
    },
    watch: {
		styles: config.srcdir+'scss/**/_*.scss',
		scripts: config.srcdir+'js/**/*.js',
		svg: config.srcdir+'svg/**/*.svg',
    }
};

module.exports = config;
