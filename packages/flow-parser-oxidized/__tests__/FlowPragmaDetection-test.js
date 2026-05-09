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

import {parse} from '../__test_utils__/parse';
import {parse as parseRaw} from '../src/FlowParser';

test('Flow pragma detection', () => {
  const parsedAsFlow = {
    type: 'File',
    program: {
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'CallExpression',
          },
        },
      ],
    },
  };

  const notParsedAsFlow = {
    type: 'File',
    program: {
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'BinaryExpression',
          },
        },
      ],
    },
  };

  // In the presence of a Flow pragma, ambiguous expressions are parsed as Flow
  const withFlowPragma = `
    // @flow
    foo<T>(x);
  `;
  expect(parse(withFlowPragma, {babel: true, flow: 'detect'})).toMatchObject(
    parsedAsFlow,
  );
  expect(parse(withFlowPragma, {babel: true, flow: 'all'})).toMatchObject(
    parsedAsFlow,
  );

  // Without a Flow pragma present, ambiguous expressions are not parsed as Flow
  // by default, but are parsed as flow in `flow: all` mode.
  const withoutFlowPragma = `
    foo<T>(x);
  `;
  expect(parse(withoutFlowPragma, {babel: true, flow: 'detect'})).toMatchObject(
    notParsedAsFlow,
  );
  expect(parse(withoutFlowPragma, {babel: true, flow: 'all'})).toMatchObject(
    parsedAsFlow,
  );

  // Without a Flow pragma, unambiguous Flow syntax is still parsed as Flow,
  // matching upstream Hermes' ParseFlowSetting::UNAMBIGUOUS mode.
  const unambiguousFlowWithoutFlowPragma = `
    // @noflow
    import type {Foo} from 'Foo';
    type Bar = Foo;
    function f(value: Bar): Bar {
      return value;
    }
    const x = (f('x'): Bar);
  `;
  expect(
    parse(unambiguousFlowWithoutFlowPragma, {babel: true, flow: 'detect'}),
  ).toMatchObject({
    type: 'File',
    program: {
      body: [
        {
          type: 'ImportDeclaration',
          importKind: 'type',
        },
        {
          type: 'TypeAlias',
        },
        {
          type: 'FunctionDeclaration',
        },
        {
          type: 'VariableDeclaration',
        },
      ],
    },
  });

  // Special Flow comment syntax is not parsed as Flow. This matches upstream
  // Hermes: comment syntax remains a JavaScript comment regardless of
  // `flow: 'detect'`, `flow: 'all'`, a file-level Flow pragma, or direct
  // raw parser calls.
  const commentSyntax = `
    module.exports = (require("dompurify-2.5.8")()/*:: as any */);
    function f(x/*: number */) { return x; }
  `;
  const commentSyntaxCases: Array<{
    source: string,
    flow: 'detect' | 'all',
  }> = [
    {source: commentSyntax, flow: 'detect'},
    {source: '/** @flow */\n' + commentSyntax, flow: 'detect'},
    {source: commentSyntax, flow: 'all'},
  ];
  for (const {source, flow} of commentSyntaxCases) {
    const commentSyntaxAST = parse(source, {babel: true, flow});
    expect(commentSyntaxAST.program.body[0]).toMatchObject({
      type: 'ExpressionStatement',
      expression: {
        type: 'AssignmentExpression',
        right: {
          type: 'CallExpression',
        },
      },
    });
    expect(commentSyntaxAST.program.body[1]).toMatchObject({
      type: 'FunctionDeclaration',
      params: [
        {
          type: 'Identifier',
        },
      ],
    });
    expect(commentSyntaxAST.program.body[1].params[0]).not.toHaveProperty(
      'typeAnnotation',
    );
  }
  const rawCommentSyntaxAST = parseRaw(commentSyntax, {enableTypes: true});
  expect(rawCommentSyntaxAST.body[0]).toMatchObject({
    type: 'ExpressionStatement',
    expression: {
      type: 'AssignmentExpression',
      right: {
        type: 'CallExpression',
      },
    },
  });
  expect(rawCommentSyntaxAST.body[1]).toMatchObject({
    type: 'FunctionDeclaration',
    params: [
      {
        type: 'Identifier',
      },
    ],
  });
  expect(rawCommentSyntaxAST.body[1].params[0]).not.toHaveProperty(
    'typeAnnotation',
  );

  // Flow pragma can appear after directives
  const flowPragmaAfterDirective = `
    'use strict';
    /* @flow */
    foo<T>(x);
  `;
  expect(
    parse(flowPragmaAfterDirective, {babel: true, flow: 'detect'}),
  ).toMatchObject(parsedAsFlow);

  // Flow pragma must appear before the first statement, so ambiguous expression
  // is not parsed as Flow type syntax.
  const flowPragmaAfterStatement = `
    foo<T>(x);
    // @flow
  `;
  expect(
    parse(flowPragmaAfterStatement, {babel: true, flow: 'detect'}),
  ).toMatchObject(notParsedAsFlow);

  // @flow must be followed by a word boundary for it to count as a Flow pragma
  const malformedFlowPragmas = `
    // @flo
    // @floww
    // @flow1
    // @flow_strict
    foo<T>(x);
  `;
  expect(
    parse(malformedFlowPragmas, {babel: true, flow: 'detect'}),
  ).toMatchObject(notParsedAsFlow);
});
