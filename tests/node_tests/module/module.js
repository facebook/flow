/* @flow */

const Module = require('module');

const myModule = new Module('foo');
const invalidModule = new Module(1); // error

// Instance methods
myModule.globalPaths = ['/xyz', '/abc'];
myModule.globalPaths = [1]; // error

myModule.load('bar.js');
(myModule.load('a.js'): Module); // error

// Static methods
Module._cache =  {a: myModule, b: new Module('baz')};
Module._cache = {a: 123}; // error

Module._findPath('a', ['./bcd', '../xyz']);
Module._findPath('a', '/bcd'); // error

let paths = Module._resolveLookupPaths('foo');

if (paths) {
  (paths: string[]);
} else {
  (paths: null);
};

(Module._load('file.js'): {});
(Module._load('file.js'): {[export: string]: () => {}});

Module._resolveFilename('foo.js', 'bar'); // error
let filename = Module._resolveFilename('foo.js', myModule);

(filename: boolean); // error
