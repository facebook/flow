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

import type {MaybeDetachedNode} from './detachedNodeTypes';
import type {Program} from 'flow-estree-oxidized';

import mutateESTreeASTForPrettier from '../../utils/mutateESTreeASTForPrettier';
import * as prettier from 'prettier/standalone';
import {mutateESTreeASTCommentsForPrettier} from './comments/comments';
import type {VisitorKeysType} from '../../traverse/getVisitorKeys';

export async function print(
  ast: MaybeDetachedNode<Program>,
  originalCode: string,
  prettierOptions: {...} = {},
  visitorKeys?: ?VisitorKeysType,
): Promise<string> {
  // $FlowExpectedError[incompatible-type] This is now safe to access.
  const program: Program = ast;

  // If the AST body is empty, we can skip the cost of prettier by returning a static string of the contents.
  if (program.body.length === 0) {
    // If the program had a docblock comment, we need to create the string manually.
    const docblockComment = program.docblock?.comment;
    if (docblockComment != null) {
      return '/*' + docblockComment.value + '*/\n';
    }

    return '';
  }

  // Cleanup the comments from the AST and generate the "orginal" code needed for prettier.
  const codeForPrinting = mutateESTreeASTCommentsForPrettier(
    program,
    originalCode,
  );

  // Fix up the AST to match what prettier expects.
  mutateESTreeASTForPrettier(program, visitorKeys);

  // Always use prettier's built-in Flow plugin. We cannot use
  // `prettier-plugin-hermes-parser` here because that package eagerly loads
  // its own hermes WASM module, and two WASM modules from the same upstream
  // C++ source linked into the same Node process trigger a SIGSEGV in V8's
  // wasm runtime as soon as either is invoked. Empirically reproducible:
  // a jest test that does `import {SimpleTraverser}` (which transitively
  // pulls in nothing wasm-related but does cause `parse.js` to import
  // `print.js`, which `await import`s `prettier-plugin-hermes-parser`),
  // then `parse(code)` (which loads OUR `flow-parser-wasm`), then
  // `print(ast, code)` segfaults 5/5 times. Substituting the prettier flow
  // plugin (which is pure JS and ships with prettier) eliminates the
  // segfault.
  //
  // We don't actually use the upstream parser's `parse()` here — we override
  // it below to return our pre-built `program` AST — so the only thing the
  // hermes plugin gives us is its printer. The prettier flow printer
  // produces output close enough that the round-trip
  // `printForSnapshotESTree(code).toBe(code.trim())` assertions in the
  // contract suite all pass; the (small) printer-output differences for
  // newer Flow syntax are tracked separately.
  const prettierFlowPlugin = require('prettier/plugins/flow');
  const prettierESTreePlugin = require('prettier/plugins/estree');
  const pluginParser = prettierFlowPlugin.parsers.flow;
  const pluginParserName = 'flow';

  return prettier.format(
    codeForPrinting,
    // $FlowExpectedError[incompatible-exact] - we don't want to create a dependency on the prettier types
    {
      ...prettierOptions,
      parser: pluginParserName,
      requirePragma: false,
      plugins: [
        prettierESTreePlugin,
        {
          parsers: {
            [pluginParserName]: {
              ...pluginParser,
              parse() {
                return program;
              },
            },
          },
        },
      ],
    },
  );
}
