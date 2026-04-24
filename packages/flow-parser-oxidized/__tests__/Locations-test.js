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

describe('Locations', () => {
  test('Source locations', () => {
    // ESTree source locations include range
    expect(parse('Foo')).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          loc: {
            start: {
              line: 1,
              column: 0,
            },
            end: {
              line: 1,
              column: 3,
            },
          },
          range: [0, 3],
        },
      ],
    });

    // Babel source locations include start/end properties
    expect(parse('Foo', {babel: true})).toMatchObject({
      type: 'File',
      program: {
        type: 'Program',
        body: [
          {
            type: 'ExpressionStatement',
            loc: {
              start: {
                line: 1,
                column: 0,
              },
              end: {
                line: 1,
                column: 3,
              },
            },
            start: 0,
            end: 3,
          },
        ],
      },
    });

    // Filename is included in source location if provided
    expect(parse('Foo', {sourceFilename: 'FooTest.js'})).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          loc: {
            source: 'FooTest.js',
            start: {
              line: 1,
              column: 0,
            },
            end: {
              line: 1,
              column: 3,
            },
          },
        },
      ],
    });

    // Code points that will be encoded as 1, 2, 3, and 4 byte UTF-8 characters
    // within Hermes are translated back to UTF-16 code unit source locations.
    const unicode = `\
'foo1';
'foo\u00a7';
'foo\u2014';
'foo\ud83d\udea6';
`;
    expect(parse(unicode)).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'Literal',
            loc: loc(1, 0, 1, 6),
            range: [0, 6],
            value: 'foo1',
          },
        },
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'Literal',
            loc: loc(2, 0, 2, 6),
            range: [8, 14],
            value: 'foo\u00a7',
          },
        },
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'Literal',
            loc: loc(3, 0, 3, 6),
            range: [16, 22],
            value: 'foo\u2014',
          },
        },
        {
          type: 'ExpressionStatement',
          expression: {
            type: 'Literal',
            loc: loc(4, 0, 4, 7),
            range: [24, 31],
            value: 'foo\ud83d\udea6',
          },
        },
      ],
    });
  });

  test('Decorator locations on class properties and methods', () => {
    // Decorators on both properties and methods should have range
    expect(
      parse(`class C {
  @d1 prop;
  @d2 method() {}
}`),
    ).toMatchObject({
      type: 'Program',
      body: [
        {
          type: 'ClassDeclaration',
          body: {
            type: 'ClassBody',
            body: [
              {
                type: 'PropertyDefinition',
                decorators: [
                  {
                    type: 'Decorator',
                    range: [12, 15],
                    expression: {
                      type: 'Identifier',
                      name: 'd1',
                      range: [13, 15],
                    },
                  },
                ],
              },
              {
                type: 'MethodDefinition',
                decorators: [
                  {
                    type: 'Decorator',
                    range: [24, 27],
                    expression: {
                      type: 'Identifier',
                      name: 'd2',
                      range: [25, 27],
                    },
                  },
                ],
              },
            ],
          },
        },
      ],
    });
  });

  test('Program source locations', () => {
    const source = `
A;
/*comment*/`;

    // ESTree program location only includes program body
    expect(parse(source)).toMatchObject({
      type: 'Program',
      loc: loc(2, 0, 2, 2),
      range: [1, 3],
    });

    // Babel program location starts at beginning of source and includes trailing comments
    expect(parse(source, {babel: true})).toMatchObject({
      type: 'File',
      loc: loc(1, 0, 3, 11),
      start: 0,
      end: 15,
      program: {
        type: 'Program',
        loc: loc(1, 0, 3, 11),
        start: 0,
        end: 15,
      },
    });
  });
});
