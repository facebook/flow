/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import * as t from '../src/generated/node-types';

describe('generated node builders', () => {
  test('preserves export kinds', () => {
    const id = t.Identifier({name: 'Foo'});

    expect(
      t.ExportSpecifier({
        exported: id,
        local: id,
        exportKind: 'type',
      }),
    ).toMatchObject({exportKind: 'type'});
  });

  test('preserves optional object type indexers', () => {
    expect(
      t.ObjectTypeIndexer({
        id: null,
        key: t.StringTypeAnnotation(),
        value: t.NumberTypeAnnotation(),
        static: false,
        variance: null,
        optional: true,
      }),
    ).toMatchObject({optional: true});
  });

  test('builds declare-class extends calls', () => {
    expect(
      t.DeclareClassExtendsCall({
        callee: t.GenericTypeAnnotation({
          id: t.Identifier({name: 'Base'}),
        }),
        argument: t.InterfaceExtends({
          id: t.Identifier({name: 'Mixin'}),
        }),
      }),
    ).toMatchObject({
      type: 'DeclareClassExtendsCall',
      callee: {type: 'GenericTypeAnnotation'},
      argument: {type: 'InterfaceExtends'},
    });
  });

  test('builds namespace export declarations', () => {
    expect(
      t.NamespaceExportDeclaration({
        id: t.Identifier({name: 'Namespace'}),
      }),
    ).toMatchObject({
      type: 'NamespaceExportDeclaration',
      id: {type: 'Identifier', name: 'Namespace'},
    });
  });

  test('builds private object type fields', () => {
    expect(
      t.ObjectTypePrivateField({
        key: t.PrivateIdentifier({name: 'field'}),
      }),
    ).toMatchObject({
      type: 'ObjectTypePrivateField',
      key: {type: 'PrivateIdentifier', name: 'field'},
    });
  });

  test('builds optional tuple type elements', () => {
    expect(
      t.TupleTypeElement({
        elementType: t.StringTypeAnnotation(),
        optional: true,
      }),
    ).toMatchObject({
      type: 'TupleTypeElement',
      elementType: {type: 'StringTypeAnnotation'},
      optional: true,
    });
  });
});
