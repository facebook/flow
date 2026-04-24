/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {Program} from 'flow-estree-oxidized';

import {FlowVisitorKeys} from 'flow-parser-oxidized';
import {Referencer} from './referencer';
import {ScopeManager} from './ScopeManager';

type AnalyzeOptions = $ReadOnly<{
  /**
   * Whether the whole script is executed under node.js environment.
   * When enabled, the scope manager adds a function scope immediately following the global scope.
   * Defaults to `false`.
   */
  globalReturn: boolean,

  /**
   * The identifier that's used for JSX Element creation (after transpilation).
   * This should not be a member expression - just the root identifier (i.e. use "React" instead of "React.createElement").
   *
   * To use the new global JSX transform function, you can explicitly set this to `null`.
   *
   * Defaults to `"React"`.
   */
  jsxPragma: string | null,

  /**
   * The identifier that's used for JSX fragment elements (after transpilation).
   * If `null`, assumes transpilation will always use a member on `jsxFactory` (i.e. React.Fragment).
   * This should not be a member expression - just the root identifier (i.e. use "h" instead of "h.Fragment").
   * Defaults to `null`.
   */
  jsxFragmentName: string | null,

  /**
   * The source type of the script.
   */
  sourceType: 'script' | 'module',

  /**
   * Ignore <fbt /> JSX elements when adding references to the module-level `React` variable.
   * FBT is JSX that's transformed to non-JSX and thus references differently
   *
   * https://facebook.github.io/fbt/
   */
  fbt: boolean,

  /**
   * Support experimental component syntax
   *
   * Defaults to `true`.
   */
  enableExperimentalComponentSyntax?: boolean,

  /**
   * Support experimental Flow match syntax
   *
   * Defaults to `true`.
   */
  enableExperimentalFlowMatchSyntax?: boolean,

  /**
   * Support experimental Flow record syntax
   *
   * Defaults to `true`.
   */
  enableExperimentalFlowRecordSyntax?: boolean,
}>;
type PartialAnalyzeOptions = $ReadOnly<Partial<AnalyzeOptions>>;

const DEFAULT_OPTIONS: AnalyzeOptions = {
  globalReturn: false,
  jsxPragma: 'React',
  jsxFragmentName: null,
  sourceType: 'module',
  fbt: true,
};

const JS_IDENTIFIER_REGEX = /^[_$a-zA-Z][_$a-zA-Z0-9]*$/;
function extractIdentifier(directive: ?$ReadOnlyArray<string>): ?string {
  // handle `@\jsx Foo.bar` -> we want to extract `Foo`, not `Foo.bar`
  const foundPragma = directive?.[0].split('.')[0];
  if (foundPragma != null && JS_IDENTIFIER_REGEX.test(foundPragma)) {
    return foundPragma;
  }

  return null;
}

function getJsxPragma(
  ast: Program,
  providedOptions?: PartialAnalyzeOptions,
): string | null {
  // search for a pragma comment in the docblock only
  // we do this for speed and simplicity
  if (ast.docblock) {
    const foundPragma = extractIdentifier(ast.docblock.directives.jsx);
    if (foundPragma != null) {
      return foundPragma;
    }
  }

  if (
    providedOptions &&
    // intentionally differentiate between null and undefined
    // -- null      = don't reference JSX pragma
    // -- undefined = not set, use default
    // eslint-disable-next-line eqeqeq
    providedOptions.jsxPragma === null
  ) {
    return null;
  }

  return providedOptions?.jsxPragma ?? DEFAULT_OPTIONS.jsxPragma;
}
function getJsxFragmentPragma(
  ast: Program,
  providedOptions?: PartialAnalyzeOptions,
): string | null {
  // search for a pragma comment in the docblock only
  // we do this for speed and simplicity
  if (ast.docblock) {
    const foundPragma = extractIdentifier(ast.docblock.directives.jsxFrag);
    if (foundPragma != null) {
      return foundPragma;
    }
  }

  return providedOptions?.jsxFragmentName ?? DEFAULT_OPTIONS.jsxFragmentName;
}

/**
 * Takes an AST and returns the analyzed scopes.
 */
function analyze(
  ast: Program,
  providedOptions?: PartialAnalyzeOptions,
): ScopeManager {
  const scopeManager = new ScopeManager({
    globalReturn: providedOptions?.globalReturn ?? DEFAULT_OPTIONS.globalReturn,
    sourceType: providedOptions?.sourceType ?? DEFAULT_OPTIONS.sourceType,
  });
  const referencer = new Referencer(
    {
      childVisitorKeys: FlowVisitorKeys,
      fbtSupport: providedOptions?.fbt ?? DEFAULT_OPTIONS.fbt,
      jsxPragma: getJsxPragma(ast, providedOptions),
      jsxFragmentName: getJsxFragmentPragma(ast, providedOptions),
    },
    scopeManager,
  );

  referencer.visit(ast);

  return scopeManager;
}

export type {AnalyzeOptions, PartialAnalyzeOptions};
export {analyze};
