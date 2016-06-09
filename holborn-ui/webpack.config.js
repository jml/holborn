// webpack.config.js
'use strict';
var webpack = require("webpack");
var path = require("path");


var modulesDirectories = [
  'node_modules',
  'bower_components'
];


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
      exclude: /node_modules/,
      query: {
        src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
      },
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
    new webpack.optimize.CommonsChunkPlugin("vendor", "vendor.bundle.js"),
  ],

  devServer: {
    port: 1337,
    // Route all 404s to index.html
    historyApiFallback: {
      verbose: true,

      // bizarrely the historyApiFallback code decides that a dot in
      // the path should stop rewriting:
      // https://github.com/bripkens/connect-history-api-fallback/blob/master/lib/index.js#L59
      // We fix this in the dev server by catching dot paths unless they are a bundle
      rewrites: [{
        from: /\./,
        to: 'index.html',
      }],
    },
  },
};


module.exports = config;
