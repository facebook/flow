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
import {loc} from '../__test_utils__/loc';

test('Tokens', () => {
  const source = `var foo = 'str'; 1 === true`;
  expect(parse(source, {tokens: true})).toMatchObject({
    type: 'Program',
    tokens: [
      {
        type: 'Keyword',
        loc: loc(1, 0, 1, 3),
        value: 'var',
      },
      {
        type: 'Identifier',
        loc: loc(1, 4, 1, 7),
        value: 'foo',
      },
      {
        type: 'Punctuator',
        loc: loc(1, 8, 1, 9),
        value: '=',
      },
      {
        type: 'String',
        loc: loc(1, 10, 1, 15),
        value: "'str'",
      },
      {
        type: 'Punctuator',
        loc: loc(1, 15, 1, 16),
        value: ';',
      },
      {
        type: 'Numeric',
        loc: loc(1, 17, 1, 18),
        value: '1',
      },
      {
        type: 'Punctuator',
        loc: loc(1, 19, 1, 22),
        value: '===',
      },
      {
        type: 'Boolean',
        loc: loc(1, 23, 1, 27),
        value: 'true',
      },
    ],
  });
});
