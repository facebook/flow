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
