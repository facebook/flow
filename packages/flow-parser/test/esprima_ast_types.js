/* This is based off of ast-types/main.ts */

import fork from "ast-types/fork";
import { namedTypes } from "ast-types/gen/namedTypes";

import coreDef from "ast-types/def/core";
import es6Def from "ast-types/def/es6";
import es7Def from "ast-types/def/es7";
import es2020Def from "ast-types/def/es2020";
import jsxDef from "ast-types/def/jsx";
import typeAnnotationsDef from "ast-types/def/type-annotations";
import flowDef from "ast-types/def/flow";
import esprimaDef from "ast-types/def/esprima";
import esProposalsDef from "ast-types/def/es-proposals";
import customDef from './custom_ast_types';

const {
  namedTypes: n,
  ...rest
} = fork([
  coreDef,
  es6Def,
  es7Def,
  es2020Def,
  jsxDef,
  flowDef,
  typeAnnotationsDef,
  esprimaDef,
  esProposalsDef,
  customDef,
]);

module.exports = {
  namedTypes: Object.assign(namedTypes, n),
  ...rest
};
