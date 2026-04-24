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

describe('Comments', () => {
  test('Parsing comments', () => {
    const source = '/*Block comment*/ 1; // Line comment';

    // ESTree comments in AST
    expect(parse(source)).toMatchObject({
      type: 'Program',
      comments: [
        {
          type: 'Block',
          value: 'Block comment',
          loc: {
            start: {
              line: 1,
              column: 0,
            },
            end: {
              line: 1,
              column: 17,
            },
          },
          range: [0, 17],
        },
        {
          type: 'Line',
          value: ' Line comment',
          loc: {
            start: {
              line: 1,
              column: 21,
            },
            end: {
              line: 1,
              column: 36,
            },
          },
          range: [21, 36],
        },
      ],
    });

    // Babel comments in AST
    expect(parse(source, {babel: true})).toMatchObject({
      type: 'File',
      program: {
        type: 'Program',
        body: [
          {
            type: 'ExpressionStatement',
          },
        ],
      },
      comments: [
        {
          type: 'CommentBlock',
          value: 'Block comment',
        },
        {
          type: 'CommentLine',
          value: ' Line comment',
        },
      ],
    });
  });

  test('Interpreter directives', () => {
    // Parsing interpreter directive
    const sourceWithDirective = `#! interpreter comment
      1; /*block comment*/
    `;

    const interpreter = {
      type: 'InterpreterDirective',
      loc: loc(1, 0, 1, 22),
      value: ' interpreter comment',
    };

    expect(parse(sourceWithDirective)).toMatchObject({
      type: 'Program',
      interpreter,
      comments: [{type: 'Block', value: 'block comment'}],
    });
    expect(parse(sourceWithDirective, {babel: true})).toMatchObject({
      type: 'File',
      program: {
        type: 'Program',
        interpreter,
      },
      comments: [{type: 'CommentBlock', value: 'block comment'}],
    });

    // No interpreter directive
    const sourceWithoutDirective = '// Line comment';
    expect(parse(sourceWithoutDirective)).toMatchObject({
      type: 'Program',
      interpreter: null,
      comments: [{type: 'Line', value: ' Line comment'}],
    });
    expect(parse(sourceWithoutDirective, {babel: true})).toMatchObject({
      type: 'File',
      program: {
        type: 'Program',
        interpreter: null,
      },
      comments: [{type: 'CommentLine', value: ' Line comment'}],
    });
  });

  describe('ESTree > Extracts the docblock onto the program node', () => {
    const DOCBLOCK_COMMENT = {
      type: 'Block',
      value: 'Block comment',
      loc: {
        start: {
          line: 1,
          column: 0,
        },
        end: {
          line: 1,
          column: 17,
        },
      },
      range: [0, 17],
    };

    test('with docblock and body node', () => {
      const source = '/*Block comment*/ 1; // Line comment';
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [{type: 'ExpressionStatement'}],
        comments: [
          DOCBLOCK_COMMENT,
          {
            type: 'Line',
            value: ' Line comment',
          },
        ],
        docblock: {
          directives: {},
          comment: DOCBLOCK_COMMENT,
        },
      });
    });

    test('with dockblock and no body node', () => {
      const source = '/*Block comment*/ // Line comment';
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [],
        comments: [
          DOCBLOCK_COMMENT,
          {
            type: 'Line',
            value: ' Line comment',
          },
        ],
        docblock: {
          directives: {},
          comment: DOCBLOCK_COMMENT,
        },
      });
    });

    test('with no dockblock', () => {
      const source = '1; // Line comment';
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [{type: 'ExpressionStatement'}],
        comments: [
          {
            type: 'Line',
            value: ' Line comment',
          },
        ],
        docblock: null,
      });
    });

    test('with block comment AFTER the body node', () => {
      const source = '1; /*Block comment*/';
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [{type: 'ExpressionStatement'}],
        comments: [
          {
            type: 'Block',
            value: 'Block comment',
          },
        ],
        docblock: null,
      });
    });

    test('with docblock, body node, and interpreter directive', () => {
      const source = `#! interpreter comment
/*Block comment*/ 1; // Line comment`;
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [{type: 'ExpressionStatement'}],
        comments: [
          {
            type: 'Block',
            value: 'Block comment',
          },
          {
            type: 'Line',
            value: ' Line comment',
          },
        ],
        docblock: {
          directives: {},
          comment: {
            type: 'Block',
            value: 'Block comment',
          },
        },
      });
    });

    test('it extracts directives from the docblock', () => {
      const source = `
        /**
         * @foo foo-value
         * @bar bar-value
         * @multiple one
         * @multiple two
         * @empty
         */
      `;
      expect(parse(source).docblock?.directives).toMatchInlineSnapshot(`
        {
          "bar": [
            "bar-value",
          ],
          "empty": [
            "",
          ],
          "foo": [
            "foo-value",
          ],
          "multiple": [
            "one",
            "two",
          ],
        }
      `);
    });

    test('docblock - directives with "reserved" names shouldn\'t crash the parser', () => {
      const source = `
        /**
         * @constructor
         * @toString
         * @toLocaleString
         * @hasOwnProperty
         * @valueOf
         * @__proto__
         */
      `;
      expect(parse(source).docblock?.directives).toMatchInlineSnapshot(`
        {
          "__proto__": [
            "",
          ],
          "constructor": [
            "",
          ],
          "hasOwnProperty": [
            "",
          ],
          "toLocaleString": [
            "",
          ],
          "toString": [
            "",
          ],
          "valueOf": [
            "",
          ],
        }
      `);
    });

    it('handles the case where eslint converts a shebang to a line comment', () => {
      const source = `\
#!scripts/third-party/node
/*Block comment*/`.replace(
        // https://github.com/eslint/eslint/blob/dd58cd4afa6ced9016c091fc99a702c97a3e44f0/lib/shared/ast-utils.js#L13
        /^#!([^\r\n]+)/,
        // https://github.com/eslint/eslint/blob/21d647904dc30f9484b22acdd9243a6d0ecfba38/lib/linter/linter.js#L779
        (_, captured) => `//${captured}`,
      );
      expect(parse(source)).toMatchObject({
        type: 'Program',
        body: [],
        comments: [
          {
            type: 'Line',
            value: 'scripts/third-party/node',
          },
          {
            type: 'Block',
            value: 'Block comment',
          },
        ],
        docblock: {
          directives: {},
          comment: {
            type: 'Block',
            value: 'Block comment',
          },
        },
      });
    });
  });
});
