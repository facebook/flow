/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {Program} from 'flow-estree-oxidized';
import type {VisitorKeysType} from 'flow-parser-oxidized';
import type {PartialAnalyzeOptions, ScopeManager} from './scope-manager';

import * as HermesParser from 'flow-parser-oxidized';
import {analyze} from './scope-manager';

type ParseForESLintOptions = $ReadOnly<{
  ...PartialAnalyzeOptions,
}>;

function parse(code: string, options?: ParseForESLintOptions): Program {
  const parserOptions = {
    allowReturnOutsideFunction: true,
    flow: ('all': 'all'),
    sourceType: options?.sourceType ?? ('module': 'module'),
    tokens: true,
    enableExperimentalComponentSyntax:
      options?.enableExperimentalComponentSyntax ?? true,
    enableExperimentalFlowMatchSyntax:
      options?.enableExperimentalFlowMatchSyntax ?? true,
    enableExperimentalFlowRecordSyntax:
      options?.enableExperimentalFlowRecordSyntax ?? true,
  };

  try {
    const ast = HermesParser.parse(code, parserOptions);
    return ast;
  } catch (e) {
    // Format error location for ESLint
    if (e instanceof SyntaxError) {
      // $FlowFixMe[prop-missing]
      e.lineNumber = e.loc.line;
      // $FlowFixMe[prop-missing]
      e.column = e.loc.column;
    }

    throw e;
  }
}

const VisitorKeys = HermesParser.FlowVisitorKeys;

type ParseForESLintReturn = {
  ast: Program,
  scopeManager: ScopeManager,
  visitorKeys: VisitorKeysType,
};
function parseForESLint(
  code: string,
  options?: ParseForESLintOptions,
): ParseForESLintReturn {
  const ast = parse(code, options);

  // set the parent pointers
  HermesParser.SimpleTraverser.traverse(ast, {
    enter(node, parent) {
      // $FlowExpectedError[cannot-write]
      node.parent = parent;
    },
    leave() {},
  });

  const scopeManager = analyze(ast, options);

  return {
    ast,
    scopeManager,
    visitorKeys: VisitorKeys,
  };
}

export type * from './scope-manager';
export type {ParseForESLintOptions, ParseForESLintReturn};
export {ScopeType, DefinitionType} from './scope-manager';
export {parse, parseForESLint, VisitorKeys};
