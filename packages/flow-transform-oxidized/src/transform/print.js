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

import type {MaybeDetachedNode} from '../detachedNode';
import type {Program} from 'flow-estree-oxidized';

import {mutateESTreeASTForPrettier} from 'flow-parser-oxidized';
import * as prettier from 'prettier/standalone';
import {mutateESTreeASTCommentsForPrettier} from './comments/comments';
import type {VisitorKeysType} from 'flow-parser-oxidized';

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

  let pluginParserName = 'flow';
  let pluginParser;
  let pluginPrinter;
  let additionalPlugins = [];
  try {
    // Use prettier-plugin-flow-parser-oxidized if we can. It has latest Flow syntax support.
    // $FlowExpectedError[untyped-import]
    const prettierHermesPlugin = await import(
      'prettier-plugin-flow-parser-oxidized'
    );
    pluginParser = prettierHermesPlugin.parsers.hermes;
    if (prettierHermesPlugin.__flowTransformUseFlowParserName === true) {
      const prettierESTreePlugin = require('prettier/plugins/estree');
      additionalPlugins = [prettierESTreePlugin];
    } else {
      pluginPrinter = prettierHermesPlugin.printers;
      pluginParserName = 'hermes';
    }
  } catch {
    const prettierFlowPlugin = require('prettier/plugins/flow');
    const prettierESTreePlugin = require('prettier/plugins/estree');
    pluginParser = prettierFlowPlugin.parsers.flow;
    additionalPlugins = [prettierESTreePlugin];
  }

  return prettier.format(
    codeForPrinting,
    // $FlowExpectedError[incompatible-exact] - we don't want to create a dependency on the prettier types
    {
      ...prettierOptions,
      parser: pluginParserName,
      requirePragma: false,
      plugins: [
        ...additionalPlugins,
        {
          parsers: {
            [pluginParserName]: {
              ...pluginParser,
              parse() {
                return program;
              },
            },
          },
          printers: pluginPrinter,
        },
      ],
    },
  );
}
