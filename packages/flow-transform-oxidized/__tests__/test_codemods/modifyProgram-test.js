/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import type {Program} from 'flow-estree-oxidized';

import {t, transform} from './test-utils';

describe('modifyProgram', () => {
  describe('Modify docblock with body', () => {
    function modifyDocblockWithBody(code: string) {
      return transform(code, context => ({
        Program(node: Program) {
          context.modifyNodeInPlace(node, {
            docblock: {
              directives: {},
              comment: t.BlockComment({value: ' DOCBLOCK! '}),
            },
            body: [
              t.ExpressionStatement({
                expression: t.StringLiteral({value: 'hello'}),
              }),
            ],
          });
        },
      }));
    }

    it('with existing content', async () => {
      const result = await modifyDocblockWithBody(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

const empty = 1;
`);

      expect(result).toMatchInlineSnapshot(`
              "/* DOCBLOCK! */
              ('hello');
              "
          `);
    });
    it('with only a statement', async () => {
      const result = await modifyDocblockWithBody('1;');

      expect(result).toMatchInlineSnapshot(`
              "/* DOCBLOCK! */ ('hello');
              "
          `);
    });
    it('with only a docblock', async () => {
      const result = await modifyDocblockWithBody('/* Hi! */');

      expect(result).toMatchInlineSnapshot(`
              "/* DOCBLOCK! */ ('hello');
              "
          `);
    });
  });

  describe('Modify docblock', () => {
    function modifyDocblockWithBody(code: string) {
      return transform(code, context => ({
        Program(node: Program) {
          context.modifyNodeInPlace(node, {
            docblock: {
              directives: {},
              comment: t.BlockComment({value: ' DOCBLOCK! '}),
            },
          });
        },
      }));
    }

    it('with existing content', async () => {
      const result = await modifyDocblockWithBody(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

const empty = 1;
`);

      expect(result).toMatchInlineSnapshot(`
        "/* DOCBLOCK! */
        const empty = 1;
        "
      `);
    });
    it('with only a statement', async () => {
      const result = await modifyDocblockWithBody('1;');

      expect(result).toMatchInlineSnapshot(`
        "/* DOCBLOCK! */ 1;
        "
      `);
    });

    it('with only a docblock', async () => {
      const result = await modifyDocblockWithBody('/* Hi! */');

      expect(result).toMatchInlineSnapshot(`
        "/* DOCBLOCK! */
        "
      `);
    });
  });

  describe('Modify body', () => {
    function modifyDocblockWithBody(code: string) {
      return transform(code, context => ({
        Program(node: Program) {
          context.modifyNodeInPlace(node, {
            body: [
              t.ExpressionStatement({
                expression: t.StringLiteral({value: 'hello'}),
              }),
            ],
          });
        },
      }));
    }

    it('with existing content', async () => {
      const result = await modifyDocblockWithBody(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

const empty = 1;
`);

      expect(result).toMatchInlineSnapshot(`
        "/**
         * LICENCE GOES HERE
         *
         * @flow strict-local
         * @format
         */

        ('hello');
        "
      `);
    });
    it('with only a statement', async () => {
      const result = await modifyDocblockWithBody('1;');

      expect(result).toMatchInlineSnapshot(`
        "('hello');
        "
      `);
    });

    it('with only a docblock', async () => {
      const result = await modifyDocblockWithBody('/* Hi! */');

      expect(result).toMatchInlineSnapshot(`
        "/* Hi! */ ('hello');
        "
      `);
    });
  });

  describe('Remove body', () => {
    function modifyDocblockWithBody(code: string) {
      return transform(code, context => ({
        Program(node: Program) {
          context.modifyNodeInPlace(node, {
            body: [],
          });
        },
      }));
    }

    it('With docblock', async () => {
      const result = await modifyDocblockWithBody(`\
/**
 * LICENCE GOES HERE
 *
 * @flow strict-local
 * @format
 */

const empty = 1;
`);

      expect(result).toMatchInlineSnapshot(`
        "/**
         * LICENCE GOES HERE
         *
         * @flow strict-local
         * @format
         */
        "
      `);
    });

    it('Without docblock', async () => {
      const result = await modifyDocblockWithBody(`\
const empty = 1;
`);

      expect(result).toMatchInlineSnapshot(`""`);
    });
  });
});
