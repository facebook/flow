/* @flow */
const Module = require('module');

const module: Module = new Module('foo');

module.load('bar');
module.load(1); // error