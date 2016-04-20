// webpack.config.js
'use strict';
var webpack = require("webpack");
var path = require("path");

var PurescriptWebpackPlugin = require('purescript-webpack-plugin');

var src = ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'];

// TOOD "pulp server" writes a src/.webpack.js so we need to adjust
// the ffi files to ignore that.
var ffi = ['bower_components/purescript-*/src/**/*.js', 'src/**/FFI*.js'];

var modulesDirectories = [
  'node_modules',
  'bower_components'
];


var purescriptWebpackPlugin = new PurescriptWebpackPlugin({
  src: src,
  ffi: ffi,
  // TODO currently running on language-javascript 0.5 with missing
  // `a` escape.
  bundle: true,

  // PSA not working for unknown reasons. Use psc for now.
  //psc: 'psa',
});


var config = {
  entry: {
    app: './entry',
    vendor: ["react", "react-dom"]
  },
  output: { path: __dirname
            , pathinfo: true
            , filename: 'bundle.js'
          },
  module: { loaders: [
    { test: /\.purs$/,
      loader: 'purs-loader',
    },
    {
      test: /\.scss$/,
      loaders: ["style", "css", "sass"],
    },
  ]},

  sassLoader: {
    includePaths: ["./scss"],
  },

  resolve: { modulesDirectories: modulesDirectories, extensions: [ '', '.js', '.purs'] },
  plugins: [
    purescriptWebpackPlugin,
    new webpack.optimize.CommonsChunkPlugin("vendor", "vendor.bundle.js"),
  ],

  devServer: {
    port: 1337,
    // Route all 404s to index.html
    historyApiFallback: true
  },
};


module.exports = config;
