/* This is basically the ast-types main.js, except without the conflicting
 * babel definitions */
var types = require("ast-types/lib/types");

// This core module of AST types captures ES5 as it is parsed today by
// git://github.com/ariya/esprima.git#master.
require("ast-types/def/core");

// Feel free to add to or remove from this list of extension modules to
// configure the precise type hierarchy that you need.
require("ast-types/def/e4x");
require("ast-types/def/es6");
require("ast-types/def/es7");
require("ast-types/def/esprima");
require("ast-types/def/flow");
require("ast-types/def/jsx");
require("ast-types/def/mozilla");
require('./custom_ast_types');

types.finalize();

exports.namedTypes = types.namedTypes;
