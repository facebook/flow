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

import type {Variance} from 'flow-estree';

import {
  parse,
  parseForSnapshot,
  printForSnapshotESTree,
} from '../__test_utils__/parse';
import {SimpleTraverser} from '../oxidized-src/traverse/SimpleTraverser';

// Directional variance preserves each public kind, including the combined form.
const directionalVariance: ReadonlyArray<Variance['kind']> = [
  'in',
  'out',
  'in out',
];
test.each(directionalVariance)('type parameter variance: %s', kind => {
  const declaration = parse(`type T<${kind} A> = A;`).body[0];
  if (declaration.type !== 'TypeAlias' || declaration.typeParameters == null) {
    throw new Error('Expected a parameterized type alias');
  }
  const variance = declaration.typeParameters.params[0].variance;
  expect(variance).toMatchObject({type: 'Variance', kind}); // OK
});

describe('TypeParameter', () => {
  const boundSyntaxes = [
    {name: 'extends', syntax: 'extends'},
    {name: 'deprecated colon', syntax: ':'},
  ];

  test.each(boundSyntaxes)('ESTree $name bound is a bare type', ({syntax}) => {
    const code = `type T<A ${syntax} string> = A;`;
    const start = code.indexOf('string');
    expect(parseForSnapshot(code, {preserveRange: true})).toMatchObject({
      body: [
        {
          typeParameters: {
            params: [
              {
                usesExtendsBound: syntax === 'extends',
                bound: {
                  type: 'StringTypeAnnotation',
                  range: [start, start + 'string'.length],
                },
              },
            ],
          },
        },
      ],
    });
  });

  test.each(boundSyntaxes)('Babel $name bound stays wrapped', ({syntax}) => {
    expect(
      parseForSnapshot(`type T<A ${syntax} string> = A;`, {babel: true}),
    ).toMatchObject({
      body: [
        {
          typeParameters: {
            params: [
              {
                usesExtendsBound: syntax === 'extends',
                bound: {
                  type: 'TypeAnnotation',
                  typeAnnotation: {type: 'StringTypeAnnotation'},
                },
              },
            ],
          },
        },
      ],
    });
  });

  test.each([false, true])('missing bound stays null (babel: %s)', babel => {
    expect(parseForSnapshot('type T<A> = A;', {babel})).toMatchObject({
      body: [{typeParameters: {params: [{bound: null}]}}],
    });
  });

  test.each(boundSyntaxes)('prints $name bounds', async ({syntax}) => {
    expect(
      await printForSnapshotESTree(`type T<A ${syntax} string> = A;`),
    ).toBe(`type T<A${syntax === ':' ? ': ' : ' extends '}string> = A;`);
  });

  test('traverses nested bounds without a TypeAnnotation wrapper', () => {
    const types = [];
    const names = [];
    SimpleTraverser.traverse(parse('type T<A extends Foo<Bar>> = A;', {}), {
      enter(node) {
        types.push(node.type);
        if (node.type === 'Identifier') {
          names.push(node.name);
        }
      },
      leave() {},
    });
    expect(types).not.toContain('TypeAnnotation');
    expect(names).toEqual(['T', 'Foo', 'Bar', 'A']);
  });
});
