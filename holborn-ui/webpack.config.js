// webpack.config.js
'use strict';
var webpack = require("webpack");

var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var src = ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'];

var ffi = ['bower_components/purescript-*/src/**/*.js'];

var modulesDirectories = [
  'node_modules',
  'bower_components'
];

var config = {
  entry: {
    app: './src/entry',
    vendor: ["react", "react-dom"]
  },
  output: { path: __dirname
            , pathinfo: true
            , filename: 'bundle.js'
          },
  module: { loaders: [ { test: /\.purs$/
                         , loader: 'purs-loader'
                       } ] },
  resolve: { modulesDirectories: modulesDirectories, extensions: [ '', '.js', '.purs'] },
  plugins: [
    new PurescriptWebpackPlugin({src: src, ffi: ffi}),
    new webpack.optimize.CommonsChunkPlugin("vendor", "vendor.bundle.js"),
  ]
};


module.exports = config;
