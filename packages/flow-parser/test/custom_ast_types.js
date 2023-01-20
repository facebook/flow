/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const {default: typesPlugin} = require('ast-types/lib/types');
const sharedPlugin = require('ast-types/lib/shared');

function custom_ast_types(fork) {
  const {
    Type: {def, or},
  } = fork.use(typesPlugin);
  const shared = fork.use(sharedPlugin);
  const defaults = shared.defaults;

  /////////
  // flow
  /////////
  def('ObjectTypeIndexer').field('id', or(def('Identifier'), null));

  def('TupleTypeLabeledElement')
    .build('elementType', 'label', 'optional')
    .field('elementType', def('FlowType'))
    .field('label', def('Identifier'))
    .field('optional', Boolean);

  def('TupleTypeSpreadElement')
    .build('typeAnnotation', 'label')
    .field('typeAnnotation', def('FlowType'))
    .field('label', or(def('Identifier'), null));

  def('TupleTypeAnnotation')
    .bases('FlowType')
    .build('elementTypes')
    .field('elementTypes', [
      or(
        def('FlowType'),
        def('TupleTypeLabeledElement'),
        def('TupleTypeSpreadElement'),
      ),
    ])
    .field('types', undefined);

  def('TypeofTypeAnnotation')
    .bases('FlowType')
    .build('argument')
    .field('argument', or(def('Identifier'), def('QualifiedTypeofIdentifier')));

  def('QualifiedTypeofIdentifier')
    .bases('Node')
    .build('qualification', 'id')
    .field(
      'qualification',
      or(def('Identifier'), def('QualifiedTypeofIdentifier')),
    )
    .field('id', def('Identifier'));

  // See https://github.com/benjamn/ast-types/issues/180
  def('ExportNamedDeclaration')
    // TODO: this is non-standard, upstream is standard
    .field(
      'specifiers',
      [or(def('ExportSpecifier'), def('ExportNamespaceSpecifier'))],
      defaults.emptyArray,
    )
    .field('exportKind', or('type', 'value'));

  def('ExportAllDeclaration').field('exportKind', or('type', 'value'));

  // See https://github.com/benjamn/ast-types/issues/180
  def('ExportNamespaceSpecifier')
    .bases('Specifier')
    .build('exported')
    .field('exported', def('Identifier'));

  def('DeclareEnum')
    .bases('Declaration')
    .build('id', 'body')
    .field('id', def('Identifier'))
    .field(
      'body',
      or(
        def('EnumBooleanBody'),
        def('EnumNumberBody'),
        def('EnumStringBody'),
        def('EnumSymbolBody'),
      ),
    );

  def('DeclareExportDeclaration').field(
    'declaration',
    or(
      def('DeclareVariable'),
      def('DeclareFunction'),
      def('DeclareClass'),
      def('DeclareEnum'), // <-- ADDITION
      def('FlowType'), // Implies default.
      def('TypeAlias'), // Implies named type
      def('DeclareOpaqueType'), // Implies named opaque type
      def('InterfaceDeclaration'),
      null,
    ),
  );

  /////////
  // es2018
  /////////
  def('ObjectPattern').field('properties', [
    or(def('Property'), def('PropertyPattern'), def('RestElement')),
  ]);

  /////////
  // es2020
  /////////
  def('BigIntLiteral').bases('Literal').build('bigint').field('bigint', String);

  /////////
  // es2022
  /////////
  def('PrivateIdentifier').bases('Expression', 'Pattern').field('name', String);

  def('PropertyDefinition').bases('ClassProperty');

  def('ClassBody').field('body', [
    or(def('MethodDefinition'), def('PropertyDefinition')),
  ]);

  ////////////
  // ts syntax
  ////////////
  def('UnknownTypeAnnotation').bases('FlowType');
  def('NeverTypeAnnotation').bases('FlowType');
  def('UndefinedTypeAnnotation').bases('FlowType');
  def('KeyofTypeAnnotation')
    .bases('FlowType')
    .build('argument')
    .field('argument', def('FlowType'));
  def('ReadOnlyTypeAnnotation')
    .bases('FlowType')
    .build('argument')
    .field('argument', def('FlowType'));
  def('TypeParameter').field('usesExtendsBound', or(undefined, Boolean));
  def('Variance').field(
    'kind',
    or('plus', 'minus', 'readonly', 'in', 'out', 'in-out'),
  );
  def('AsExpression')
    .bases('Expression')
    .build('expression', 'typeAnnotation')
    .field('expression', def('Expression'))
    .field('typeAnnotation', def('FlowType'));
  def('SatisfiesExpression')
    .bases('Expression')
    .build('expression', 'typeAnnotation')
    .field('expression', def('Expression'))
    .field('typeAnnotation', def('FlowType'));
  def('AsConstExpression')
    .bases('Expression')
    .build('expression')
    .field('expression', def('Expression'));
}

module.exports = custom_ast_types;
