/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const webpack = require('webpack');
const path = require('path');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const TerserPlugin = require("terser-webpack-plugin");

// https://webpack.js.org/configuration/
module.exports = {
  entry: {
    main: [path.join(__dirname, '_webpack', 'main.js'), path.join(__dirname, '_webpack', 'main.scss')],
    editor: path.join(__dirname, '_webpack', 'editor'),
    install: {
      import: path.join(__dirname, '_webpack', 'install'),
      dependOn: 'main',
    },
    search: path.join(__dirname, '_webpack', 'search'),
    tryFlow: path.join(__dirname, '_webpack', 'tryFlow'),
    tryFlowWorker: path.join(__dirname, '_webpack', 'tryFlowWorker'),
  },
  output: {
    path: path.resolve(__dirname, 'assets'),
    filename: '[name]-bundle.js',
  },
  devtool: 'source-map',
  resolve: {
    extensions: ['.json', '.js', '.jsx'],
    modules: ['node_modules'],
  },
  optimization: {
    minimize: true,
    minimizer: [new TerserPlugin({
      terserOptions: {
        format: {
          comments: /Copyright/i,
        },
      },
      extractComments: false,
    })],
  },
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, loader: 'babel-loader' },
      {
        test: /\.scss$/,
        use: [
          MiniCssExtractPlugin.loader,
          'css-loader',
          {
            loader: 'postcss-loader',
            options: {
              postcssOptions: {
                plugins: ['autoprefixer'],
              }
            }
          },
          'sass-loader',
        ],
      },
      {
        test: /\.wasm$/,
        loader: "file-loader",
        type: "javascript/auto",
      }
    ],
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: '[name].css',
    }),
    new webpack.DefinePlugin({
      // for docsearch.js 2.x; 3.x doesn't use `process`
      // https://github.com/algolia/algoliasearch-client-javascript/issues/764
      'process.env.RESET_APP_DATA_TIMER': JSON.stringify(undefined),
    }),
  ],
};
