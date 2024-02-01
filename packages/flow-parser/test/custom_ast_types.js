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
  def('TypeOperator')
    .bases('FlowType')
    .build('operator', 'typeAnnotation')
    .field('operator', String)
    .field('typeAnnotation', def('FlowType'));

  def('ObjectTypeAnnotation').field('properties', [
    or(
      def('ObjectTypeProperty'),
      def('ObjectTypeSpreadProperty'),
      def('ObjectTypeMappedTypeProperty'),
    ),
  ]);

  def('ObjectTypeMappedTypeProperty')
    .bases('Node')
    .build('keyTparam', 'propType', 'sourceType', 'variance', 'optional')
    .field('keyTparam', def('TypeParameter'))
    .field('propType', def('FlowType'))
    .field('sourceType', def('FlowType'))
    .field('variance', or(def('Variance'), null))
    .field('optional', or('PlusOptional', 'MinusOptional', 'Optional', null));

  def('ConditionalTypeAnnotation')
    .bases('FlowType')
    .build('checkType', 'extendsType', 'trueType', 'falseType')
    .field('checkType', def('FlowType'))
    .field('extendsType', def('FlowType'))
    .field('trueType', def('FlowType'))
    .field('falseType', def('FlowType'));

  def('InferTypeAnnotation')
    .bases('FlowType')
    .build('typeParameter')
    .field('typeParameter', def('TypeParameter'));

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

  def('TypePredicate')
    .bases('TypeAnnotation', 'FlowType')
    .build('parameterName', 'typeAnnotation', 'asserts')
    .field('parameterName', def('Identifier'))
    .field('typeAnnotation', or(def('FlowType'), null), defaults['null'])
    .field('asserts', Boolean, defaults['false']);

  def('TypeofTypeAnnotation')
    .bases('FlowType')
    .build('argument', 'typeArguments')
    .field('argument', or(def('Identifier'), def('QualifiedTypeofIdentifier')))
    .field('typeArguments', or(undefined, def('TypeParameterInstantiation')));

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

  def('DeclareNamespace')
    .bases('Declaration')
    .build('id', 'body')
    .field('id', def('Identifier'))
    .field('body', def('BlockStatement'));

  def('DeclareComponent')
    .bases('Declaration')
    .build('id', 'tparams', 'params', 'rest', 'return')
    .field('id', def('Identifier'))
    .field('typeParameters', or(def('TypeParameterDeclaration'), null))
    .field('params', [def('ComponentTypeParameter')])
    .field('rest', or(def('ComponentTypeParameter'), null))
    .field('rendersType', or(def('TypeOperator'), null));

  def('ComponentTypeAnnotation')
    .bases('FlowType')
    .build('tparams', 'params', 'rest', 'return')
    .field('typeParameters', or(def('TypeParameterDeclaration'), null))
    .field('params', [def('ComponentTypeParameter')])
    .field('rest', or(def('ComponentTypeParameter'), null))
    .field('rendersType', or(def('TypeOperator'), null));

  def('ComponentTypeParameter')
    .build('name', 'annotation', 'optional')
    .field('name', or(def('Identifier'), def('Literal'), null))
    .field('typeAnnotation', def('FlowType'))
    .field('optional', Boolean);

  def('DeclareExportDeclaration').field(
    'declaration',
    or(
      def('DeclareVariable'),
      def('DeclareFunction'),
      def('DeclareClass'),
      def('DeclareEnum'), // <-- ADDITION
      def('DeclareComponent'),
      def('FlowType'), // Implies default.
      def('TypeAlias'), // Implies named type
      def('DeclareOpaqueType'), // Implies named opaque type
      def('InterfaceDeclaration'),
      null,
    ),
  );
  def('DeclareVariable').field('kind', or('var', 'let', 'const'));

  def('ComponentDeclaration')
    .bases('Declaration')
    .build('id', 'tparams', 'params', 'return', 'body')
    .field('id', def('Identifier'))
    .field('typeParameters', or(def('TypeParameterDeclaration'), null))
    .field('params', [or(def('ComponentParameter'), def('RestElement'))])
    .field('rendersType', or(def('TypeOperator'), null))
    .field('body', def('BlockStatement'));

  def('HookDeclaration').bases('FunctionDeclaration');

  def('HookTypeAnnotation').bases('FunctionTypeAnnotation');

  def('DeclareHook').bases('DeclareFunction');

  def('ComponentParameter')
    .build('name', 'local', 'shorthand')
    .field('name', or(def('Identifier'), def('Literal')))
    .field('local', def('Pattern'))
    .field('shorthand', Boolean);

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

  ////////
  // babel
  ////////
  def('InterpreterDirective')
    .bases('Node')
    .build('value')
    .field('value', String);

  def('Program').field(
    'interpreter',
    or(def('InterpreterDirective'), null),
    defaults['null'],
  );

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
  def('JSXOpeningElement').field(
    'typeArguments',
    or(undefined, def('TypeParameterInstantiation')),
  );
}

module.exports = custom_ast_types;
