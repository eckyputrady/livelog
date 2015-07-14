'use strict';

// import Webpack plugins
var webpack = require('webpack')
var cleanPlugin = require('clean-webpack-plugin');
var htmlWebpackPlugin = require('html-webpack-plugin')

// define Webpack configuration object to be exported
var config = {
    context: __dirname + '/app',
    entry: './index.js',
    output: {
      path: __dirname + '/dist',
      filename: 'bundle.js',
      hash: true
    },
    resolve: {
      alias: {
        'npm': __dirname + '/node_modules'
      }
    },    
    module: {
      loaders: [
        { 
          test: /\.css$/, 
          loader: 'style!css'
        },
        { 
          test: /\.(woff|woff2)$/,   
          loader: 'url?limit=10000&mimetype=application/font-woff' 
        },
        { 
          test: /\.(eot|svg|ttf)$/,    
          loader: 'file' 
        },
        {
          test: /\.js?$/,
          exclude: /node_modules/,
          loader: 'babel?stage=4!jshint'
        }
      ]
    },
    plugins: [
      new cleanPlugin(['dist']),
      new htmlWebpackPlugin({
        title: 'Livelog',
        template: __dirname + '/app/index.html'
      }),
      new webpack.ProvidePlugin({
        $: 'jquery',
        jQuery: 'jquery',
        'window.jQuery': 'jquery',
        Hammer: 'hammerjs'
      })
    ],
    devServer: {
      contentBase: __dirname + '/dist',
      colors: true,
      host: '0.0.0.0'
    }
};

module.exports = config;