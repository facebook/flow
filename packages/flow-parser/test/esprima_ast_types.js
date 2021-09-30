/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* This is based off of ast-types/main.ts */

const fork = require("ast-types/fork");
const { namedTypes } = require("ast-types/gen/namedTypes");

const jsxDef = require("ast-types/def/jsx");
const flowDef = require("ast-types/def/flow");
const esprimaDef = require("ast-types/def/esprima");
const customDef = require('./custom_ast_types');

const {
  namedTypes: n,
  ...rest
} = fork([
  jsxDef,
  flowDef,
  esprimaDef,
  customDef,
]);

module.exports = {
  namedTypes: Object.assign(namedTypes, n),
  ...rest
};
