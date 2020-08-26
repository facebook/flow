/* This is based off of ast-types/main.ts */

import fork from "ast-types/fork";
import { namedTypes } from "ast-types/gen/namedTypes";

import jsxDef from "ast-types/def/jsx";
import flowDef from "ast-types/def/flow";
import esprimaDef from "ast-types/def/esprima";
import customDef from './custom_ast_types';

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
