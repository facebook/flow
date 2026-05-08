/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

declare module '@babel/parser' {
  declare type ParserPlugin =
    | 'asyncGenerators'
    | 'bigInt'
    | 'classPrivateMethods'
    | 'classPrivateProperties'
    | 'classProperties'
    | 'decorators'
    | 'decorators-legacy'
    | 'doExpressions'
    | 'dynamicImport'
    | 'estree'
    | 'exportDefaultFrom'
    | 'exportNamespaceFrom' // deprecated
    | 'flow'
    | 'flowComments'
    | 'functionBind'
    | 'functionSent'
    | 'importMeta'
    | 'jsx'
    | 'logicalAssignment'
    | 'moduleAttributes'
    | 'nullishCoalescingOperator'
    | 'numericSeparator'
    | 'objectRestSpread'
    | 'optionalCatchBinding'
    | 'optionalChaining'
    | 'partialApplication'
    | 'pipelineOperator'
    | 'placeholders'
    | 'privateIn'
    | 'throwExpressions'
    | 'topLevelAwait'
    | 'typescript'
    | 'v8intrinsic'
    | ParserPluginWithOptions;
  declare type ParserPluginWithOptions =
    | ['decorators', DecoratorsPluginOptions]
    | ['pipelineOperator', PipelineOperatorPluginOptions]
    | ['flow', FlowPluginOptions];
  declare type DecoratorsPluginOptions = {
    decoratorsBeforeExport?: boolean,
  };
  declare type PipelineOperatorPluginOptions = {
    proposal: 'minimal' | 'smart',
  };
  declare type FlowPluginOptions = {
    all?: boolean,
    enums?: boolean,
  };

  declare export function parse(
    input: string,
    // https://github.com/babel/babel/blob/1daded57126ba172eef3664c08829b83a1112cc9/packages/babel-parser/src/options.js
    options?: $ReadOnly<{
      /**
       * Source type ("script" or "module") for different semantics
       */
      sourceType?: 'script' | 'module' | 'unambiguous',
      /**
       * Source filename.
       */
      sourceFilename?: string,
      /** Column (0-based) from which to start counting source. Useful for
       * integration with other tools.
       */
      startColumn?: number,
      /**
       * Line (1-based) from which to start counting source. Useful for
       * integration with other tools.
       */
      startLine?: number,
      /**
       * When enabled, await at the top level is not considered an
       * error.
       */
      allowAwaitOutsideFunction?: boolean,
      /**
       * When enabled, a return at the top level is not considered an
       * error.
       */
      allowReturnOutsideFunction?: boolean,
      /**
       * When enabled, import/export statements are not constrained to
       * appearing at the top of the program.
       */
      allowImportExportEverywhere?: boolean,
      allowSuperOutsideMethod?: boolean,
      /**
       * When enabled, export statements can reference undeclared variables.
       */
      allowUndeclaredExports?: boolean,
      /**
       * An array of plugins to enable
       */
      plugins?: $ReadOnlyArray<ParserPlugin>,
      strictMode?: ?boolean,
      /**
       * Nodes have their start and end characters offsets recorded in
       * `start` and `end` properties (directly on the node, rather than
       * the `loc` object, which holds line/column data. To also add a
       * [semi-standardized][range] `range` property holding a `[start,
       * end]` array with the same numbers, set the `ranges` option to
       * `true`.
       *
       * [range]: https://bugzilla.mozilla.org/show_bug.cgi?id=745678
       */
      ranges?: boolean,
      /**
       * Adds all parsed tokens to a `tokens` property on the `File` node
       */
      tokens?: boolean,
      /**
       * Whether to create ParenthesizedExpression AST nodes (if false
       * the parser sets extra.parenthesized on the expression nodes instead).
       */
      createParenthesizedExpressions?: boolean,
      /**
       * When enabled, errors are attached to the AST instead of being directly thrown.
       * Some errors will still throw, because @babel/parser can't always recover.
       */
      errorRecovery?: boolean,
      /**
       * When enabled, comments will be attached to adjacent AST nodes as one of
       * `leadingComments`, `trailingComments` and `innerComments`. The comment attachment
       * is vital to preserve comments after transform. If you don't print AST back,
       * consider set this option to `false` for performance
       */
      attachComment?: boolean,
    }>,
  ): mixed /* AST */;
}
