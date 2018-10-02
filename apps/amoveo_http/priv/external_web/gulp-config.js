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
            'BigInteger.js',
            'sjcl.js',
            'sha256.js',
            'elliptic.min.js',
            'format.js',
            'rpc.js',
            'files.js',
            'codecBytes.js',
            'sha256.js',
            'crypto.js',
            'merkle_proofs.js',
            'encryption_library.js',
            'keys.js',
            'server.js',
            'headers.js',
            'signing.js',
            'spend_tx.js',
            'sign_tx.js',
            'combine_cancel_assets.js',
            'market.js',
            'chalang.js',
            'spk.js',
            'bets.js',
            'lightning.js',
            'encryption.js',
            'encryption_interface.js',
            'oracles.js',
            'channels.js',
            'tabs.js',
            'spoiler.js',
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
