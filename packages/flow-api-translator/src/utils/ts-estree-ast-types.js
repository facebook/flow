/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

/**
 * The following types have been adapted by hand from
 * https://unpkg.com/browse/@typescript-eslint/types@5.41.0/dist/generated/ast-spec.d.ts
 *
 * Changes:
 * - remove and inline `ValueOf` type
 * - `undefined` -> `void`
 * - remove all `declare` keywords
 * - comment out `bigint` type
 *     -> flow doesn't support it yet
 * - remove `range` and `loc` from `NodeOrTokenData`
 *     -> during conversion our locations will be all off, so we'll rely on prettier to print later
 * - make all properties readonly and all arrays $ReadOnlyArray
 *     -> unlike TS - flow enforces subtype constraints strictly!
 * - add `type` to interfaces that previously relied upon inheriting the `type`
 *     -> this is because flow sentinel refinement does not check inherited members
 * - create "Ambiguous" versions for some nodes that have unions (like PropertyDefinition, MemberDefinition)
 *     -> makes it easier to construct them from other nodes that have unions
 */

'use strict';

interface NodeOrTokenData {
  loc: SourceLocation;
}
interface BaseNode extends NodeOrTokenData {}
interface BaseToken extends NodeOrTokenData {
  readonly value: string;
}

export type Accessibility = 'private' | 'protected' | 'public';
export interface ArrayExpression extends BaseNode {
  readonly type: 'ArrayExpression';
  readonly elements: ReadonlyArray<Expression | SpreadElement>;
}
export interface ArrayPattern extends BaseNode {
  readonly type: 'ArrayPattern';
  readonly elements: ReadonlyArray<DestructuringPattern | null>;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly optional?: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export interface ArrowFunctionExpression extends BaseNode {
  readonly type: 'ArrowFunctionExpression';
  readonly generator: boolean;
  readonly id: null;
  readonly params: ReadonlyArray<Parameter>;
  readonly body: BlockStatement | Expression;
  readonly async: boolean;
  readonly expression: boolean;
  readonly returnType?: TSTypeAnnotation;
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface AssignmentExpression extends BaseNode {
  readonly type: 'AssignmentExpression';
  readonly operator:
    | '='
    | '+='
    | '-='
    | '*='
    | '**='
    | '/='
    | '%='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '|='
    | '||='
    | '&&='
    | '??='
    | '^=';
  readonly left: Expression;
  readonly right: Expression;
}
export interface AssignmentPattern extends BaseNode {
  readonly type: 'AssignmentPattern';
  readonly left: BindingName;
  readonly right: Expression;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly optional?: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export interface AwaitExpression extends BaseNode {
  readonly type: 'AwaitExpression';
  readonly argument: Expression;
}
export interface BigIntLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: bigint;
  readonly bigint: string;
}
export interface BinaryExpression extends BaseNode {
  readonly type: 'BinaryExpression';
  readonly operator: string;
  readonly left: Expression | PrivateIdentifier;
  readonly right: Expression;
}
export type BindingName = BindingPattern | Identifier;
export type BindingPattern = ArrayPattern | ObjectPattern;
export interface BlockComment extends BaseToken {
  readonly type: 'Block';
}
export interface BlockStatement extends BaseNode {
  readonly type: 'BlockStatement';
  readonly body: ReadonlyArray<Statement>;
}
export interface BooleanLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: boolean;
  readonly raw: 'false' | 'true';
}
export interface BooleanToken extends BaseToken {
  readonly type: 'Boolean';
}
export interface BreakStatement extends BaseNode {
  readonly type: 'BreakStatement';
  readonly label: Identifier | null;
}
export interface CallExpression extends BaseNode {
  readonly type: 'CallExpression';
  readonly callee: LeftHandSideExpression;
  readonly arguments: ReadonlyArray<CallExpressionArgument>;
  readonly typeArguments?: TSTypeParameterInstantiation;
  readonly optional: boolean;
}
export type CallExpressionArgument = Expression | SpreadElement;
export interface CatchClause extends BaseNode {
  readonly type: 'CatchClause';
  readonly param: BindingName | null;
  readonly body: BlockStatement;
}
export type ChainElement =
  CallExpression | MemberExpression | TSNonNullExpression;
export interface ChainExpression extends BaseNode {
  readonly type: 'ChainExpression';
  readonly expression: ChainElement;
}
interface ClassBase extends BaseNode {
  /**
   * Whether the class is an abstract class.
   * ```
   * abstract class Foo {...}
   * ```
   * This is always `undefined` for `ClassExpression`.
   */
  readonly abstract?: boolean;
  /**
   * The class body.
   */
  readonly body: ClassBody;
  /**
   * Whether the class has been `declare`d:
   * ```
   * declare class Foo {...}
   * ```
   * This is always `undefined` for `ClassExpression`.
   */
  readonly declare?: boolean;
  /**
   * The decorators declared for the class.
   * This is `undefined` if there are no decorators.
   * ```
   * @deco
   * class Foo {...}
   * ```
   * This is always `undefined` for `ClassExpression`.
   */
  readonly decorators?: ReadonlyArray<Decorator>;
  /**
   * The class's name.
   * - For a `ClassExpression` this may be `null` if the name is omitted.
   * - For a `ClassDeclaration` this may be `null` if and only if the parent is
   *   an `ExportDefaultDeclaration`.
   */
  readonly id: Identifier | null;
  /**
   * The implemented interfaces for the class.
   * This is `undefined` if there are no implemented interfaces.
   */
  readonly implements?: ReadonlyArray<TSClassImplements>;
  /**
   * The super class this class extends.
   */
  readonly superClass: LeftHandSideExpression | null;
  /**
   * The generic type parameters passed to the superClass.
   * This is `undefined` if there are no generic type parameters passed.
   */
  readonly superTypeArguments?: TSTypeParameterInstantiation;
  /**
   * The generic type parameters declared for the class.
   * This is `undefined` if there are no generic type parameters declared.
   */
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface ClassBody extends BaseNode {
  readonly type: 'ClassBody';
  readonly body: ReadonlyArray<ClassElement>;
}
export type ClassDeclaration =
  ClassDeclarationWithName | ClassDeclarationWithOptionalName;
interface ClassDeclarationBase extends ClassBase {
  readonly type: 'ClassDeclaration';
}
export interface ClassDeclarationWithName extends ClassDeclarationBase {
  readonly type: 'ClassDeclaration';
  readonly id: Identifier;
}
export interface ClassDeclarationWithOptionalName extends ClassDeclarationBase {
  readonly type: 'ClassDeclaration';
  readonly id: Identifier | null;
}
export type ClassElement =
  | MethodDefinition
  | PropertyDefinition
  | MethodDefinitionAmbiguous
  | PropertyDefinitionAmbiguous
  | StaticBlock
  | TSAbstractMethodDefinition
  | TSAbstractPropertyDefinition
  | TSIndexSignature;
export interface ClassExpression extends ClassBase {
  readonly type: 'ClassExpression';
  readonly abstract?: void;
  readonly declare?: void;
  readonly decorators?: void;
}
interface ClassMethodDefinitionNonComputedNameBase extends MethodDefinitionBase {
  readonly type: 'MethodDefinition';
  readonly key: ClassPropertyNameNonComputed;
  readonly computed: false;
}
interface ClassPropertyDefinitionNonComputedNameBase extends PropertyDefinitionBase {
  readonly type: 'PropertyDefinition';
  readonly key: ClassPropertyNameNonComputed;
  readonly computed: false;
}
export type ClassPropertyNameNonComputed =
  PrivateIdentifier | PropertyNameNonComputed;
export type Comment = BlockComment | LineComment;
export interface ConditionalExpression extends BaseNode {
  readonly type: 'ConditionalExpression';
  readonly test: Expression;
  readonly consequent: Expression;
  readonly alternate: Expression;
}
export interface ContinueStatement extends BaseNode {
  readonly type: 'ContinueStatement';
  readonly label: Identifier | null;
}
export interface DebuggerStatement extends BaseNode {
  readonly type: 'DebuggerStatement';
}
export type DeclarationStatement =
  | ClassDeclaration
  | ClassExpression
  | ExportAllDeclaration
  | ExportDefaultDeclaration
  | ExportNamedDeclaration
  | FunctionDeclaration
  | TSDeclareFunction
  | TSEnumDeclaration
  | TSImportEqualsDeclaration
  | TSInterfaceDeclaration
  | TSModuleDeclaration
  | TSNamespaceExportDeclaration
  | TSTypeAliasDeclaration;
export interface Decorator extends BaseNode {
  readonly type: 'Decorator';
  readonly expression: LeftHandSideExpression;
}
export type DefaultExportDeclarations =
  | ClassDeclarationWithOptionalName
  | Expression
  | FunctionDeclarationWithName
  | FunctionDeclarationWithOptionalName
  | TSDeclareFunction
  | TSEnumDeclaration
  | TSInterfaceDeclaration
  | TSModuleDeclaration
  | TSTypeAliasDeclaration
  | VariableDeclaration;
export type DestructuringPattern =
  | ArrayPattern
  | AssignmentPattern
  | Identifier
  | MemberExpression
  | ObjectPattern
  | RestElement;
export interface DoWhileStatement extends BaseNode {
  readonly type: 'DoWhileStatement';
  readonly test: Expression;
  readonly body: Statement;
}
export interface EmptyStatement extends BaseNode {
  readonly type: 'EmptyStatement';
}
export type EntityName = Identifier | ThisExpression | TSQualifiedName;
export interface ExportAllDeclaration extends BaseNode {
  readonly type: 'ExportAllDeclaration';
  /**
   * The assertions declared for the export.
   * ```
   * export * from 'mod' assert { type: 'json' };
   * ```
   */
  readonly assertions: ReadonlyArray<ImportAttribute>;
  /**
   * The name for the exported items. `null` if no name is assigned.
   */
  readonly exported: Identifier | null;
  /**
   * The kind of the export.
   */
  readonly exportKind: ExportKind;
  /**
   * The source module being exported from.
   */
  readonly source: StringLiteral;
}
type ExportAndImportKind = 'type' | 'value';
export type ExportDeclaration =
  DefaultExportDeclarations | NamedExportDeclarations;
export interface ExportDefaultDeclaration extends BaseNode {
  readonly type: 'ExportDefaultDeclaration';
  /**
   * The declaration being exported.
   */
  readonly declaration: DefaultExportDeclarations;
  /**
   * The kind of the export.
   */
  readonly exportKind: ExportKind;
}
export type ExportKind = ExportAndImportKind;
export type ExportNamedDeclaration =
  | ExportNamedDeclarationWithoutSourceWithMultiple
  | ExportNamedDeclarationWithoutSourceWithSingle
  | ExportNamedDeclarationWithSource;
interface ExportNamedDeclarationBase extends BaseNode {
  readonly type: 'ExportNamedDeclaration';
  /**
   * The assertions declared for the export.
   * ```
   * export { foo } from 'mod' assert { type: 'json' };
   * ```
   * This will be an empty array if `source` is `null`
   */
  readonly assertions: ReadonlyArray<ImportAttribute>;
  /**
   * The exported declaration.
   * ```
   * export const x = 1;
   * ```
   * This will be `null` if `source` is not `null`, or if there are `specifiers`
   */
  readonly declaration: NamedExportDeclarations | null;
  /**
   * The kind of the export.
   */
  readonly exportKind: ExportKind;
  /**
   * The source module being exported from.
   */
  readonly source: StringLiteral | null;
  /**
   * The specifiers being exported.
   * ```
   * export { a, b };
   * ```
   * This will be an empty array if `declaration` is not `null`
   */
  readonly specifiers: ReadonlyArray<ExportSpecifier>;
}
export interface ExportNamedDeclarationAmbiguous extends ExportNamedDeclarationBase {
  readonly type: 'ExportNamedDeclaration';
}
export interface ExportNamedDeclarationWithoutSourceWithSingle extends ExportNamedDeclarationBase {
  readonly type: 'ExportNamedDeclaration';
  readonly assertions: ReadonlyArray<ImportAttribute>;
  readonly declaration: NamedExportDeclarations;
  readonly source: null;
  readonly specifiers: [];
}
export interface ExportNamedDeclarationWithoutSourceWithMultiple extends ExportNamedDeclarationBase {
  readonly type: 'ExportNamedDeclaration';
  readonly assertions: ReadonlyArray<ImportAttribute>;
  readonly declaration: null;
  readonly source: null;
  readonly specifiers: ReadonlyArray<ExportSpecifier>;
}
export interface ExportNamedDeclarationWithSource extends ExportNamedDeclarationBase {
  readonly type: 'ExportNamedDeclaration';
  readonly assertions: ReadonlyArray<ImportAttribute>;
  readonly declaration: null;
  readonly source: StringLiteral;
  readonly specifiers: ReadonlyArray<ExportSpecifier>;
}
export interface ExportSpecifier extends BaseNode {
  readonly type: 'ExportSpecifier';
  readonly local: Identifier;
  readonly exported: Identifier;
  readonly exportKind: ExportKind;
}
export type Expression =
  | ArrayExpression
  | ArrayPattern
  | ArrowFunctionExpression
  | AssignmentExpression
  | AwaitExpression
  | BinaryExpression
  | CallExpression
  | ChainExpression
  | ClassExpression
  | ConditionalExpression
  | FunctionExpression
  | Identifier
  | ImportExpression
  | JSXElement
  | JSXFragment
  | LiteralExpression
  | LogicalExpression
  | MemberExpression
  | MetaProperty
  | NewExpression
  | ObjectExpression
  | ObjectPattern
  | SequenceExpression
  | Super
  | TaggedTemplateExpression
  | TemplateLiteral
  | ThisExpression
  | TSAsExpression
  | TSInstantiationExpression
  | TSNonNullExpression
  | TSQualifiedName
  | TSTypeAssertion
  | UnaryExpression
  | UpdateExpression
  | YieldExpression;
export interface ExpressionStatement extends BaseNode {
  readonly type: 'ExpressionStatement';
  readonly expression: Expression;
  readonly directive?: string;
}
export type ForInitialiser = Expression | VariableDeclaration;
export interface ForInStatement extends BaseNode {
  readonly type: 'ForInStatement';
  readonly left: ForInitialiser;
  readonly right: Expression;
  readonly body: Statement;
}
export interface ForOfStatement extends BaseNode {
  readonly type: 'ForOfStatement';
  readonly left: ForInitialiser;
  readonly right: Expression;
  readonly body: Statement;
  readonly await: boolean;
}
export interface ForStatement extends BaseNode {
  readonly type: 'ForStatement';
  readonly init: Expression | ForInitialiser | null;
  readonly test: Expression | null;
  readonly update: Expression | null;
  readonly body: Statement;
}
interface FunctionBase extends BaseNode {
  /**
   * Whether the function is async:
   * ```
   * async function foo(...) {...}
   * const x = async function (...) {...}
   * const x = async (...) => {...}
   * ```
   */
  readonly async: boolean;
  /**
   * The body of the function.
   * - For an `ArrowFunctionExpression` this may be an `Expression` or `BlockStatement`.
   * - For a `FunctionDeclaration` or `FunctionExpression` this is always a `BlockStatement.
   * - For a `TSDeclareFunction` this is always `undefined`.
   * - For a `TSEmptyBodyFunctionExpression` this is always `null`.
   */
  readonly body?: BlockStatement | Expression | null;
  /**
   * This is only `true` if and only if the node is a `TSDeclareFunction` and it has `declare`:
   * ```
   * declare function foo(...) {...}
   * ```
   */
  readonly declare?: boolean;
  /**
   * This is only ever `true` if and only the node is an `ArrowFunctionExpression` and the body
   * is an expression:
   * ```
   * (() => 1)
   * ```
   */
  readonly expression: boolean;
  /**
   * Whether the function is a generator function:
   * ```
   * function *foo(...) {...}
   * const x = function *(...) {...}
   * ```
   * This is always `false` for arrow functions as they cannot be generators.
   */
  readonly generator: boolean;
  /**
   * The function's name.
   * - For an `ArrowFunctionExpression` this is always `null`.
   * - For a `FunctionExpression` this may be `null` if the name is omitted.
   * - For a `FunctionDeclaration` or `TSDeclareFunction` this may be `null` if
   *   and only if the parent is an `ExportDefaultDeclaration`.
   */
  readonly id: Identifier | null;
  /**
   * The list of parameters declared for the function.
   */
  readonly params: ReadonlyArray<Parameter>;
  /**
   * The return type annotation for the function.
   * This is `undefined` if there is no return type declared.
   */
  readonly returnType?: TSTypeAnnotation;
  /**
   * The generic type parameter declaration for the function.
   * This is `undefined` if there are no generic type parameters declared.
   */
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export type FunctionDeclaration =
  FunctionDeclarationWithName | FunctionDeclarationWithOptionalName;
interface FunctionDeclarationBase extends FunctionBase {
  readonly type: 'FunctionDeclaration';
  readonly body: BlockStatement;
  readonly declare?: false;
  readonly expression: false;
}
export interface FunctionDeclarationWithName extends FunctionDeclarationBase {
  readonly type: 'FunctionDeclaration';
  readonly id: Identifier;
}
export interface FunctionDeclarationWithOptionalName extends FunctionDeclarationBase {
  readonly type: 'FunctionDeclaration';
  readonly id: Identifier | null;
}
export interface FunctionExpression extends FunctionBase {
  readonly type: 'FunctionExpression';
  readonly body: BlockStatement;
  readonly expression: false;
}
export type FunctionLike =
  | ArrowFunctionExpression
  | FunctionDeclaration
  | FunctionExpression
  | TSDeclareFunction
  | TSEmptyBodyFunctionExpression;
export interface Identifier extends BaseNode {
  readonly type: 'Identifier';
  readonly name: string;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly optional?: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export interface IdentifierToken extends BaseToken {
  readonly type: 'Identifier';
}
export interface IfStatement extends BaseNode {
  readonly type: 'IfStatement';
  readonly test: Expression;
  readonly consequent: Statement;
  readonly alternate: Statement | null;
}
export interface ImportAttribute extends BaseNode {
  readonly type: 'ImportAttribute';
  readonly key: Identifier | Literal;
  readonly value: Literal;
}
export type ImportClause =
  ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier;
export interface ImportDeclaration extends BaseNode {
  readonly type: 'ImportDeclaration';
  /**
   * The assertions declared for the export.
   * ```
   * import * from 'mod' assert { type: 'json' };
   * ```
   */
  readonly assertions: ReadonlyArray<ImportAttribute>;
  /**
   * The kind of the import.
   */
  readonly importKind: ImportKind;
  /**
   * The source module being imported from.
   */
  readonly source: StringLiteral;
  /**
   * The specifiers being imported.
   * If this is an empty array then either there are no specifiers:
   * ```
   * import {} from 'mod';
   * ```
   * Or it is a side-effect import:
   * ```
   * import 'mod';
   * ```
   */
  readonly specifiers: ReadonlyArray<ImportClause>;
}
export interface ImportDefaultSpecifier extends BaseNode {
  readonly type: 'ImportDefaultSpecifier';
  readonly local: Identifier;
}
export interface ImportExpression extends BaseNode {
  readonly type: 'ImportExpression';
  readonly source: Expression;
  readonly attributes: Expression | null;
}
type ImportKind = ExportAndImportKind;
export interface ImportNamespaceSpecifier extends BaseNode {
  readonly type: 'ImportNamespaceSpecifier';
  readonly local: Identifier;
}
export interface ImportSpecifier extends BaseNode {
  readonly type: 'ImportSpecifier';
  readonly local: Identifier;
  readonly imported: Identifier;
  readonly importKind: ?ImportKind;
}
export type IterationStatement =
  | DoWhileStatement
  | ForInStatement
  | ForOfStatement
  | ForStatement
  | WhileStatement;
export interface JSXAttribute extends BaseNode {
  readonly type: 'JSXAttribute';
  readonly name: JSXIdentifier | JSXNamespacedName;
  readonly value: JSXExpression | Literal | null;
}
export type JSXChild = JSXElement | JSXExpression | JSXFragment | JSXText;
export interface JSXClosingElement extends BaseNode {
  readonly type: 'JSXClosingElement';
  readonly name: JSXTagNameExpression;
}
export interface JSXClosingFragment extends BaseNode {
  readonly type: 'JSXClosingFragment';
}
export interface JSXElement extends BaseNode {
  readonly type: 'JSXElement';
  readonly openingElement: JSXOpeningElement;
  readonly closingElement: JSXClosingElement | null;
  readonly children: ReadonlyArray<JSXChild>;
}
export interface JSXEmptyExpression extends BaseNode {
  readonly type: 'JSXEmptyExpression';
}
export type JSXExpression =
  JSXEmptyExpression | JSXExpressionContainer | JSXSpreadChild;
export interface JSXExpressionContainer extends BaseNode {
  readonly type: 'JSXExpressionContainer';
  readonly expression: Expression | JSXEmptyExpression;
}
export interface JSXFragment extends BaseNode {
  readonly type: 'JSXFragment';
  readonly openingFragment: JSXOpeningFragment;
  readonly closingFragment: JSXClosingFragment;
  readonly children: ReadonlyArray<JSXChild>;
}
export interface JSXIdentifier extends BaseNode {
  readonly type: 'JSXIdentifier';
  readonly name: string;
}
export interface JSXIdentifierToken extends BaseToken {
  readonly type: 'JSXIdentifier';
}
export interface JSXMemberExpression extends BaseNode {
  readonly type: 'JSXMemberExpression';
  readonly object: JSXTagNameExpression;
  readonly property: JSXIdentifier;
}
export interface JSXNamespacedName extends BaseNode {
  readonly type: 'JSXNamespacedName';
  readonly namespace: JSXIdentifier;
  readonly name: JSXIdentifier;
}
export interface JSXOpeningElement extends BaseNode {
  readonly type: 'JSXOpeningElement';
  readonly typeParameters?: TSTypeParameterInstantiation;
  readonly selfClosing: boolean;
  readonly name: JSXTagNameExpression;
  readonly attributes: ReadonlyArray<JSXAttribute | JSXSpreadAttribute>;
}
export interface JSXOpeningFragment extends BaseNode {
  readonly type: 'JSXOpeningFragment';
}
export interface JSXSpreadAttribute extends BaseNode {
  readonly type: 'JSXSpreadAttribute';
  readonly argument: Expression;
}
export interface JSXSpreadChild extends BaseNode {
  readonly type: 'JSXSpreadChild';
  readonly expression: Expression | JSXEmptyExpression;
}
export type JSXTagNameExpression =
  JSXIdentifier | JSXMemberExpression | JSXNamespacedName;
export interface JSXText extends BaseNode {
  readonly type: 'JSXText';
  readonly value: string;
  readonly raw: string;
}
export interface JSXTextToken extends BaseToken {
  readonly type: 'JSXText';
}
export interface KeywordToken extends BaseToken {
  readonly type: 'Keyword';
}
export interface LabeledStatement extends BaseNode {
  readonly type: 'LabeledStatement';
  readonly label: Identifier;
  readonly body: Statement;
}
export type LeftHandSideExpression =
  | ArrayExpression
  | ArrayPattern
  | ArrowFunctionExpression
  | CallExpression
  | ClassExpression
  | FunctionExpression
  | Identifier
  | JSXElement
  | JSXFragment
  | LiteralExpression
  | MemberExpression
  | MetaProperty
  | ObjectExpression
  | ObjectPattern
  | SequenceExpression
  | Super
  | TaggedTemplateExpression
  | ThisExpression
  | TSAsExpression
  | TSNonNullExpression
  | TSQualifiedName
  | TSTypeAssertion;
export interface LineComment extends BaseToken {
  readonly type: 'Line';
}
export type Literal =
  | BigIntLiteral
  | BooleanLiteral
  | NullLiteral
  | NumberLiteral
  | RegExpLiteral
  | StringLiteral;
interface LiteralBase extends BaseNode {
  readonly type: 'Literal';
  readonly raw: string;
  readonly value: RegExp | bigint | boolean | number | string | null;
}
export type LiteralExpression = Literal | TemplateLiteral;
export interface LogicalExpression extends BaseNode {
  readonly type: 'LogicalExpression';
  readonly operator: '??' | '&&' | '||';
  readonly left: Expression;
  readonly right: Expression;
}
export type MemberExpression =
  MemberExpressionComputedName | MemberExpressionNonComputedName;
interface MemberExpressionBase extends BaseNode {
  readonly object: LeftHandSideExpression;
  readonly property: Expression | Identifier | PrivateIdentifier;
  readonly computed: boolean;
  readonly optional: boolean;
}
export interface MemberExpressionComputedName extends MemberExpressionBase {
  readonly type: 'MemberExpression';
  readonly property: Expression;
  readonly computed: true;
}
export interface MemberExpressionNonComputedName extends MemberExpressionBase {
  readonly type: 'MemberExpression';
  readonly property: Identifier | PrivateIdentifier;
  readonly computed: false;
}
export interface MetaProperty extends BaseNode {
  readonly type: 'MetaProperty';
  readonly meta: Identifier;
  readonly property: Identifier;
}
export type MethodDefinition =
  MethodDefinitionComputedName | MethodDefinitionNonComputedName;
/** this should not be directly used - instead use MethodDefinitionComputedNameBase or MethodDefinitionNonComputedNameBase */
interface MethodDefinitionBase extends BaseNode {
  readonly accessibility?: Accessibility;
  readonly computed: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
  readonly key: PropertyName;
  readonly kind: 'constructor' | 'get' | 'method' | 'set';
  readonly optional?: boolean;
  readonly override?: boolean;
  readonly static: boolean;
  readonly typeParameters?: TSTypeParameterDeclaration;
  readonly value: FunctionExpression | TSEmptyBodyFunctionExpression;
}
export interface MethodDefinitionAmbiguous extends MethodDefinitionBase {
  type: 'MethodDefinition';
}
export interface MethodDefinitionComputedName extends MethodDefinitionComputedNameBase {
  readonly type: 'MethodDefinition';
  readonly computed: true;
}
interface MethodDefinitionComputedNameBase extends MethodDefinitionBase {
  readonly key: PropertyNameComputed;
  readonly computed: true;
}
export interface MethodDefinitionNonComputedName extends ClassMethodDefinitionNonComputedNameBase {
  readonly type: 'MethodDefinition';
  readonly computed: false;
}
interface MethodDefinitionNonComputedNameBase extends MethodDefinitionBase {
  readonly key: PropertyNameNonComputed;
  readonly computed: false;
}
export type Modifier =
  | TSAbstractKeyword
  | TSAsyncKeyword
  | TSPrivateKeyword
  | TSProtectedKeyword
  | TSPublicKeyword
  | TSReadonlyKeyword
  | TSStaticKeyword;
export type NamedExportDeclarations =
  | ClassDeclarationWithName
  | ClassDeclarationWithOptionalName
  | FunctionDeclarationWithName
  | FunctionDeclarationWithOptionalName
  | TSDeclareFunction
  | TSEnumDeclaration
  | TSInterfaceDeclaration
  | TSModuleDeclaration
  | TSTypeAliasDeclaration
  | VariableDeclaration;
export interface NewExpression extends BaseNode {
  readonly type: 'NewExpression';
  readonly callee: LeftHandSideExpression;
  readonly arguments: ReadonlyArray<CallExpressionArgument>;
  readonly typeParameters?: TSTypeParameterInstantiation;
}
export type Node =
  | ArrayExpression
  | ArrayPattern
  | ArrowFunctionExpression
  | AssignmentExpression
  | AssignmentPattern
  | AwaitExpression
  | BinaryExpression
  | BlockStatement
  | BreakStatement
  | CallExpression
  | CatchClause
  | ChainExpression
  | ClassBody
  | ClassDeclaration
  | ClassExpression
  | ConditionalExpression
  | ContinueStatement
  | DebuggerStatement
  | Decorator
  | DoWhileStatement
  | EmptyStatement
  | ExportAllDeclaration
  | ExportDefaultDeclaration
  | ExportNamedDeclaration
  | ExportSpecifier
  | ExpressionStatement
  | ForInStatement
  | ForOfStatement
  | ForStatement
  | FunctionDeclaration
  | FunctionExpression
  | Identifier
  | IfStatement
  | ImportAttribute
  | ImportDeclaration
  | ImportDefaultSpecifier
  | ImportExpression
  | ImportNamespaceSpecifier
  | ImportSpecifier
  | JSXAttribute
  | JSXClosingElement
  | JSXClosingFragment
  | JSXElement
  | JSXEmptyExpression
  | JSXExpressionContainer
  | JSXFragment
  | JSXIdentifier
  | JSXMemberExpression
  | JSXNamespacedName
  | JSXOpeningElement
  | JSXOpeningFragment
  | JSXSpreadAttribute
  | JSXSpreadChild
  | JSXText
  | LabeledStatement
  | Literal
  | LogicalExpression
  | MemberExpression
  | MetaProperty
  | MethodDefinition
  | NewExpression
  | ObjectExpression
  | ObjectPattern
  | PrivateIdentifier
  | Program
  | Property
  | PropertyDefinition
  | RestElement
  | ReturnStatement
  | SequenceExpression
  | SpreadElement
  | StaticBlock
  | Super
  | SwitchCase
  | SwitchStatement
  | TaggedTemplateExpression
  | TemplateElement
  | TemplateLiteral
  | ThisExpression
  | ThrowStatement
  | TryStatement
  | TSAbstractKeyword
  | TSAbstractMethodDefinition
  | TSAbstractPropertyDefinition
  | TSAnyKeyword
  | TSArrayType
  | TSAsExpression
  | TSAsyncKeyword
  | TSBigIntKeyword
  | TSBooleanKeyword
  | TSCallSignatureDeclaration
  | TSClassImplements
  | TSConditionalType
  | TSConstructorType
  | TSConstructSignatureDeclaration
  | TSDeclareFunction
  | TSDeclareKeyword
  | TSEmptyBodyFunctionExpression
  | TSEnumDeclaration
  | TSEnumMember
  | TSExportAssignment
  | TSExportKeyword
  | TSExternalModuleReference
  | TSFunctionType
  | TSImportEqualsDeclaration
  | TSImportType
  | TSIndexedAccessType
  | TSIndexSignature
  | TSInferType
  | TSInstantiationExpression
  | TSInterfaceBody
  | TSInterfaceDeclaration
  | TSInterfaceHeritage
  | TSIntersectionType
  | TSIntrinsicKeyword
  | TSLiteralType
  | TSMappedType
  | TSMethodSignature
  | TSModuleBlock
  | TSModuleDeclaration
  | TSNamedTupleMember
  | TSNamespaceExportDeclaration
  | TSNeverKeyword
  | TSNonNullExpression
  | TSNullKeyword
  | TSNumberKeyword
  | TSObjectKeyword
  | TSOptionalType
  | TSParameterProperty
  | TSPrivateKeyword
  | TSPropertySignature
  | TSProtectedKeyword
  | TSPublicKeyword
  | TSQualifiedName
  | TSReadonlyKeyword
  | TSRestType
  | TSStaticKeyword
  | TSStringKeyword
  | TSSymbolKeyword
  | TSTemplateLiteralType
  | TSThisType
  | TSTupleType
  | TSTypeAliasDeclaration
  | TSTypeAnnotation
  | TSTypeAssertion
  | TSTypeLiteral
  | TSTypeOperator
  | TSTypeParameter
  | TSTypeParameterDeclaration
  | TSTypeParameterInstantiation
  | TSTypePredicate
  | TSTypeQuery
  | TSTypeReference
  | TSUndefinedKeyword
  | TSUnionType
  | TSUnknownKeyword
  | TSVoidKeyword
  | UnaryExpression
  | UpdateExpression
  | VariableDeclaration
  | VariableDeclarator
  | WhileStatement
  | WithStatement
  | YieldExpression
  // new "ambiguous" nodes
  | ExportNamedDeclarationAmbiguous;
export interface NullLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: null;
  readonly raw: 'null';
}
export interface NullToken extends BaseToken {
  readonly type: 'Null';
}
export interface NumberLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: number;
}
export interface NumericToken extends BaseToken {
  readonly type: 'Numeric';
}
export interface ObjectExpression extends BaseNode {
  readonly type: 'ObjectExpression';
  readonly properties: ReadonlyArray<ObjectLiteralElement>;
}
export type ObjectLiteralElement = MethodDefinition | Property | SpreadElement;
export type ObjectLiteralElementLike = ObjectLiteralElement;
export interface ObjectPattern extends BaseNode {
  readonly type: 'ObjectPattern';
  readonly properties: ReadonlyArray<Property | RestElement>;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly optional?: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export type Parameter =
  | ArrayPattern
  | AssignmentPattern
  | Identifier
  | ObjectPattern
  | RestElement
  | TSParameterProperty;
export interface Position {
  /**
   * Line number (1-indexed)
   */
  readonly line: number;
  /**
   * Column number on the line (0-indexed)
   */
  readonly column: number;
}
export type PrimaryExpression =
  | ArrayExpression
  | ArrayPattern
  | ClassExpression
  | FunctionExpression
  | Identifier
  | JSXElement
  | JSXFragment
  | JSXOpeningElement
  | LiteralExpression
  | MetaProperty
  | ObjectExpression
  | ObjectPattern
  | Super
  | TemplateLiteral
  | ThisExpression
  | TSNullKeyword;
export interface PrivateIdentifier extends BaseNode {
  readonly type: 'PrivateIdentifier';
  readonly name: string;
}
export interface Program extends BaseNode {
  readonly type: 'Program';
  readonly body: ReadonlyArray<ProgramStatement>;
  readonly sourceType: 'module' | 'script';
  readonly comments?: ReadonlyArray<Comment>;
  readonly tokens?: ReadonlyArray<Token>;
}
export type ProgramStatement =
  | ExportAllDeclaration
  | ExportDefaultDeclaration
  | ExportNamedDeclaration
  | ImportDeclaration
  | Statement
  | TSImportEqualsDeclaration
  | TSNamespaceExportDeclaration;
export type Property = PropertyComputedName | PropertyNonComputedName;
interface PropertyBase extends BaseNode {
  readonly type: 'Property';
  readonly key: PropertyName;
  readonly value:
    | AssignmentPattern
    | BindingName
    | Expression
    | TSEmptyBodyFunctionExpression;
  readonly computed: boolean;
  readonly method: boolean;
  readonly shorthand: boolean;
  readonly optional?: boolean;
  readonly kind: 'get' | 'init' | 'set';
}
export interface PropertyComputedName extends PropertyBase {
  readonly type: 'Property';
  readonly key: PropertyNameComputed;
  readonly computed: true;
}
export type PropertyDefinition =
  PropertyDefinitionComputedName | PropertyDefinitionNonComputedName;
interface PropertyDefinitionBase extends BaseNode {
  readonly accessibility?: Accessibility;
  readonly computed: boolean;
  readonly declare: boolean;
  readonly decorators?: ReadonlyArray<Decorator>;
  readonly definite?: boolean;
  readonly key: PropertyName;
  readonly optional?: boolean;
  readonly override?: boolean;
  readonly readonly?: boolean;
  readonly static: boolean;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly value: Expression | null;
}
export interface PropertyDefinitionAmbiguous extends PropertyDefinitionBase {
  type: 'PropertyDefinition';
}
export interface PropertyDefinitionComputedName extends PropertyDefinitionComputedNameBase {
  readonly type: 'PropertyDefinition';
  readonly computed: true;
}
interface PropertyDefinitionComputedNameBase extends PropertyDefinitionBase {
  readonly key: PropertyNameComputed;
  readonly computed: true;
}
export interface PropertyDefinitionNonComputedName extends ClassPropertyDefinitionNonComputedNameBase {
  readonly type: 'PropertyDefinition';
  readonly computed: false;
}
interface PropertyDefinitionNonComputedNameBase extends PropertyDefinitionBase {
  readonly key: PropertyNameNonComputed;
  readonly computed: false;
}
export type PropertyName =
  ClassPropertyNameNonComputed | PropertyNameComputed | PropertyNameNonComputed;
export type PropertyNameComputed = Expression;
export type PropertyNameNonComputed =
  Identifier | NumberLiteral | StringLiteral;
export interface PropertyNonComputedName extends PropertyBase {
  readonly type: 'Property';
  readonly key: PropertyNameNonComputed;
  readonly computed: false;
}
export interface PunctuatorToken extends BaseToken {
  readonly type: 'Punctuator';
  readonly value:
    | '{'
    | '}'
    | '('
    | ')'
    | '['
    | ']'
    | '.'
    | '...'
    | ';'
    | ','
    | '?.'
    | '<'
    | '</'
    | '>'
    | '<='
    | '>='
    | '=='
    | '!='
    | '==='
    | '!=='
    | '=>'
    | '+'
    | '-'
    | '*'
    | '**'
    | '/'
    | '%'
    | '++'
    | '--'
    | '<<'
    | '>>'
    | '>>>'
    | '&'
    | '|'
    | '^'
    | '!'
    | '~'
    | '&&'
    | '||'
    | '?'
    | ':'
    | '@'
    | '??'
    | '`'
    | '#';
}
/**
 * An array of two numbers.
 * Both numbers are a 0-based index which is the position in the array of source code characters.
 * The first is the start position of the node, the second is the end position of the node.
 */
export type Range = [number, number];
export interface RegExpLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: RegExp | null;
  readonly regex: {
    readonly pattern: string,
    readonly flags: string,
  };
}
export interface RegularExpressionToken extends BaseToken {
  readonly type: 'RegularExpression';
  readonly regex: {
    readonly pattern: string,
    readonly flags: string,
  };
}
export interface RestElement extends BaseNode {
  readonly type: 'RestElement';
  readonly argument: DestructuringPattern;
  readonly typeAnnotation?: TSTypeAnnotation;
  readonly optional?: boolean;
  readonly value?: AssignmentPattern;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export interface ReturnStatement extends BaseNode {
  readonly type: 'ReturnStatement';
  readonly argument: Expression | null;
}
export interface SequenceExpression extends BaseNode {
  readonly type: 'SequenceExpression';
  readonly expressions: ReadonlyArray<Expression>;
}
export interface SourceLocation {
  /**
   * The position of the first character of the parsed source region
   */
  readonly start: Position;
  /**
   * The position of the first character after the parsed source region
   */
  readonly end: Position;
}
export interface SpreadElement extends BaseNode {
  readonly type: 'SpreadElement';
  readonly argument: Expression;
}
export type Statement =
  | BlockStatement
  | BreakStatement
  | ClassDeclarationWithName
  | ContinueStatement
  | DebuggerStatement
  | DoWhileStatement
  | ExportAllDeclaration
  | ExportDefaultDeclaration
  | ExportNamedDeclaration
  | ExpressionStatement
  | ForInStatement
  | ForOfStatement
  | ForStatement
  | FunctionDeclarationWithName
  | IfStatement
  | ImportDeclaration
  | LabeledStatement
  | ReturnStatement
  | SwitchStatement
  | ThrowStatement
  | TryStatement
  | TSDeclareFunction
  | TSEnumDeclaration
  | TSExportAssignment
  | TSImportEqualsDeclaration
  | TSInterfaceDeclaration
  | TSModuleDeclaration
  | TSNamespaceExportDeclaration
  | TSTypeAliasDeclaration
  | VariableDeclaration
  | WhileStatement
  | WithStatement;
export interface StaticBlock extends BaseNode {
  readonly type: 'StaticBlock';
  readonly body: ReadonlyArray<Statement>;
}
export interface StringLiteral extends LiteralBase {
  readonly type: 'Literal';
  readonly value: string;
}
export interface StringToken extends BaseToken {
  readonly type: 'String';
}
export interface Super extends BaseNode {
  readonly type: 'Super';
}
export interface SwitchCase extends BaseNode {
  readonly type: 'SwitchCase';
  readonly test: Expression | null;
  readonly consequent: ReadonlyArray<Statement>;
}
export interface SwitchStatement extends BaseNode {
  readonly type: 'SwitchStatement';
  readonly discriminant: Expression;
  readonly cases: ReadonlyArray<SwitchCase>;
}
export interface TaggedTemplateExpression extends BaseNode {
  readonly type: 'TaggedTemplateExpression';
  readonly typeParameters?: TSTypeParameterInstantiation;
  readonly tag: LeftHandSideExpression;
  readonly quasi: TemplateLiteral;
}
export interface TemplateElement extends BaseNode {
  readonly type: 'TemplateElement';
  readonly value: {
    raw: string,
    cooked: string,
  };
  readonly tail: boolean;
}
export interface TemplateLiteral extends BaseNode {
  readonly type: 'TemplateLiteral';
  readonly quasis: ReadonlyArray<TemplateElement>;
  readonly expressions: ReadonlyArray<Expression>;
}
export interface TemplateToken extends BaseToken {
  readonly type: 'Template';
}
export interface ThisExpression extends BaseNode {
  readonly type: 'ThisExpression';
}
export interface ThrowStatement extends BaseNode {
  readonly type: 'ThrowStatement';
  readonly argument: Statement | TSAsExpression | null;
}
export type Token =
  | BooleanToken
  | Comment
  | IdentifierToken
  | JSXIdentifierToken
  | JSXTextToken
  | KeywordToken
  | NullToken
  | NumericToken
  | PunctuatorToken
  | RegularExpressionToken
  | StringToken
  | TemplateToken;
export interface TryStatement extends BaseNode {
  readonly type: 'TryStatement';
  readonly block: BlockStatement;
  readonly handler: CatchClause | null;
  readonly finalizer: BlockStatement | null;
}
export interface TSAbstractKeyword extends BaseNode {
  readonly type: 'TSAbstractKeyword';
}
export type TSAbstractMethodDefinition =
  | TSAbstractMethodDefinitionComputedName
  | TSAbstractMethodDefinitionNonComputedName;
export interface TSAbstractMethodDefinitionComputedName extends MethodDefinitionComputedNameBase {
  readonly type: 'TSAbstractMethodDefinition';
  readonly computed: true;
}
export interface TSAbstractMethodDefinitionNonComputedName extends MethodDefinitionNonComputedNameBase {
  readonly type: 'TSAbstractMethodDefinition';
  readonly computed: false;
}
export type TSAbstractPropertyDefinition =
  | TSAbstractPropertyDefinitionComputedName
  | TSAbstractPropertyDefinitionNonComputedName;
export interface TSAbstractPropertyDefinitionComputedName extends PropertyDefinitionComputedNameBase {
  readonly computed: true;
  readonly type: 'TSAbstractPropertyDefinition';
  readonly value: null;
}
export interface TSAbstractPropertyDefinitionNonComputedName extends PropertyDefinitionNonComputedNameBase {
  readonly type: 'TSAbstractPropertyDefinition';
  readonly computed: false;
  readonly value: null;
}
export interface TSAnyKeyword extends BaseNode {
  readonly type: 'TSAnyKeyword';
}
export interface TSArrayType extends BaseNode {
  readonly type: 'TSArrayType';
  readonly elementType: TypeNode;
}
export interface TSAsExpression extends BaseNode {
  readonly type: 'TSAsExpression';
  readonly expression: Expression;
  readonly typeAnnotation: TypeNode;
}
export interface TSAsyncKeyword extends BaseNode {
  readonly type: 'TSAsyncKeyword';
}
export interface TSBigIntKeyword extends BaseNode {
  readonly type: 'TSBigIntKeyword';
}
export interface TSBooleanKeyword extends BaseNode {
  readonly type: 'TSBooleanKeyword';
}
export interface TSCallSignatureDeclaration extends TSFunctionSignatureBase {
  readonly type: 'TSCallSignatureDeclaration';
}
export interface TSClassImplements extends TSHeritageBase {
  readonly type: 'TSClassImplements';
}
export interface TSConditionalType extends BaseNode {
  readonly type: 'TSConditionalType';
  readonly checkType: TypeNode;
  readonly extendsType: TypeNode;
  readonly trueType: TypeNode;
  readonly falseType: TypeNode;
}
export interface TSConstructorType extends TSFunctionSignatureBase {
  readonly type: 'TSConstructorType';
  readonly abstract: boolean;
}
export interface TSConstructSignatureDeclaration extends TSFunctionSignatureBase {
  readonly type: 'TSConstructSignatureDeclaration';
}
export interface TSDeclareFunction extends FunctionBase {
  readonly type: 'TSDeclareFunction';
  readonly body?: BlockStatement;
  readonly declare?: boolean;
  readonly expression: false;
}
export interface TSDeclareKeyword extends BaseNode {
  readonly type: 'TSDeclareKeyword';
}
export interface TSEmptyBodyFunctionExpression extends FunctionBase {
  readonly type: 'TSEmptyBodyFunctionExpression';
  readonly body: null;
  readonly id: null;
}
export interface TSEnumDeclaration extends BaseNode {
  readonly type: 'TSEnumDeclaration';
  /**
   * Whether this is a `const` enum.
   * ```
   * const enum Foo {...}
   * ```
   */
  readonly const?: boolean;
  /**
   * Whether this is a `declare`d enum.
   * ```
   * declare enum Foo {...}
   * ```
   */
  readonly declare?: boolean;
  /**
   * The enum name.
   */
  readonly id: Identifier;
  /**
   * The enum members.
   */
  readonly members: ReadonlyArray<TSEnumMember>;
  readonly modifiers?: ReadonlyArray<Modifier>;
}
export type TSEnumMember =
  TSEnumMemberComputedName | TSEnumMemberNonComputedName;
interface TSEnumMemberBase extends BaseNode {
  readonly type: 'TSEnumMember';
  readonly id: PropertyNameComputed | PropertyNameNonComputed;
  readonly initializer?: Expression;
  readonly computed?: boolean;
}
/**
 * this should only really happen in semantically invalid code (errors 1164 and 2452)
 *
 * VALID:
 * enum Foo { ['a'] }
 *
 * INVALID:
 * const x = 'a';
 * enum Foo { [x] }
 * enum Bar { ['a' + 'b'] }
 */
export interface TSEnumMemberComputedName extends TSEnumMemberBase {
  readonly type: 'TSEnumMember';
  readonly id: PropertyNameComputed;
  readonly computed: true;
}
export interface TSEnumMemberNonComputedName extends TSEnumMemberBase {
  readonly type: 'TSEnumMember';
  readonly id: PropertyNameNonComputed;
  readonly computed?: false;
}
export interface TSExportAssignment extends BaseNode {
  readonly type: 'TSExportAssignment';
  readonly expression: Expression;
}
export interface TSExportKeyword extends BaseNode {
  readonly type: 'TSExportKeyword';
}
export interface TSExternalModuleReference extends BaseNode {
  readonly type: 'TSExternalModuleReference';
  readonly expression: Expression;
}
interface TSFunctionSignatureBase extends BaseNode {
  readonly params: ReadonlyArray<Parameter>;
  readonly returnType?: TSTypeAnnotation;
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface TSFunctionType extends TSFunctionSignatureBase {
  readonly type: 'TSFunctionType';
}
interface TSHeritageBase extends BaseNode {
  readonly expression: Expression;
  readonly typeArguments?: TSTypeParameterInstantiation;
}
export interface TSImportEqualsDeclaration extends BaseNode {
  readonly type: 'TSImportEqualsDeclaration';
  /**
   * The locally imported name
   */
  readonly id: Identifier;
  /**
   * The value being aliased.
   * ```
   * import F1 = A;
   * import F2 = A.B.C;
   * import F3 = require('mod');
   * ```
   */
  readonly moduleReference: EntityName | TSExternalModuleReference;
  readonly importKind: ImportKind;
  /**
   * Whether this is immediately exported
   * ```
   * export import F = A;
   * ```
   */
  readonly isExport: boolean;
}
export interface TSImportType extends BaseNode {
  readonly type: 'TSImportType';
  readonly argument?: TypeNode;
  readonly options: ObjectExpression | null;
  readonly qualifier: EntityName | null;
  readonly source?: StringLiteral;
  readonly typeArguments: TSTypeParameterInstantiation | null;
}
export interface TSIndexedAccessType extends BaseNode {
  readonly type: 'TSIndexedAccessType';
  readonly objectType: TypeNode;
  readonly indexType: TypeNode;
}
export interface TSIndexSignature extends BaseNode {
  readonly type: 'TSIndexSignature';
  readonly accessibility?: Accessibility;
  readonly export?: boolean;
  readonly parameters: ReadonlyArray<Parameter>;
  readonly readonly?: boolean;
  readonly static?: boolean;
  readonly typeAnnotation?: TSTypeAnnotation;
}
export interface TSInferType extends BaseNode {
  readonly type: 'TSInferType';
  readonly typeParameter: TSTypeParameter;
}
export interface TSInstantiationExpression extends BaseNode {
  readonly type: 'TSInstantiationExpression';
  readonly expression: Expression;
  readonly typeArguments: TSTypeParameterInstantiation;
}
export interface TSInterfaceBody extends BaseNode {
  readonly type: 'TSInterfaceBody';
  readonly body: ReadonlyArray<TypeElement>;
}
export interface TSInterfaceDeclaration extends BaseNode {
  readonly type: 'TSInterfaceDeclaration';
  readonly abstract?: boolean;
  /**
   * The body of the interface
   */
  readonly body: TSInterfaceBody;
  /**
   * Whether the interface was `declare`d, `undefined` otherwise
   */
  readonly declare?: boolean;
  /**
   * The types this interface `extends`
   */
  readonly extends?: ReadonlyArray<TSInterfaceHeritage>;
  /**
   * The name of this interface
   */
  readonly id: Identifier;
  readonly implements?: ReadonlyArray<TSInterfaceHeritage>;
  /**
   * The generic type parameters declared for the interface.
   * This is `undefined` if there are no generic type parameters declared.
   */
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface TSInterfaceHeritage extends TSHeritageBase {
  readonly type: 'TSInterfaceHeritage';
}
export interface TSIntersectionType extends BaseNode {
  readonly type: 'TSIntersectionType';
  readonly types: ReadonlyArray<TypeNode>;
}
export interface TSIntrinsicKeyword extends BaseNode {
  readonly type: 'TSIntrinsicKeyword';
}
export interface TSLiteralType extends BaseNode {
  readonly type: 'TSLiteralType';
  readonly literal: LiteralExpression | UnaryExpression | UpdateExpression;
}
export interface TSMappedType extends BaseNode {
  readonly type: 'TSMappedType';
  readonly typeParameter: TSTypeParameter;
  readonly readonly?: boolean | '-' | '+';
  readonly optional?: boolean | '-' | '+';
  readonly typeAnnotation?: TypeNode;
  readonly nameType: TypeNode | null;
}
export type TSMethodSignature =
  TSMethodSignatureComputedName | TSMethodSignatureNonComputedName;
interface TSMethodSignatureBase extends BaseNode {
  readonly type: 'TSMethodSignature';
  readonly accessibility?: Accessibility;
  readonly computed: boolean;
  readonly export?: boolean;
  readonly key: PropertyName;
  readonly kind: 'get' | 'method' | 'set';
  readonly optional?: boolean;
  readonly params: ReadonlyArray<Parameter>;
  readonly readonly?: boolean;
  readonly returnType?: TSTypeAnnotation;
  readonly static?: boolean;
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface TSMethodSignatureComputedName extends TSMethodSignatureBase {
  readonly type: 'TSMethodSignature';
  readonly key: PropertyNameComputed;
  readonly computed: true;
}
export interface TSMethodSignatureNonComputedName extends TSMethodSignatureBase {
  readonly type: 'TSMethodSignature';
  readonly key: PropertyNameNonComputed;
  readonly computed: false;
}
export interface TSModuleBlock extends BaseNode {
  readonly type: 'TSModuleBlock';
  readonly body: ReadonlyArray<ProgramStatement>;
}
export interface TSModuleDeclaration extends BaseNode {
  readonly type: 'TSModuleDeclaration';
  /**
   * The name of the module
   * ```
   * namespace A {}
   * namespace A.B.C {}
   * module 'a' {}
   * ```
   */
  readonly id: Identifier | Literal;
  /**
   * The body of the module.
   * This can only be `undefined` for the code `declare module 'mod';`
   * This will be a `TSModuleDeclaration` if the name is "nested" (`Foo.Bar`).
   */
  readonly body?: TSModuleBlock | TSModuleDeclaration;
  /**
   * Whether this is a global declaration
   * ```
   * declare global {}
   * ```
   */
  readonly global?: boolean;
  /**
   * Whether the module is `declare`d
   * ```
   * declare namespace F {}
   * ```
   */
  readonly declare?: boolean;
  readonly modifiers?: ReadonlyArray<Modifier>;
}
export interface TSNamedTupleMember extends BaseNode {
  readonly type: 'TSNamedTupleMember';
  readonly elementType: TypeNode;
  readonly label: Identifier;
  readonly optional: boolean;
}
export interface TSNamespaceExportDeclaration extends BaseNode {
  readonly type: 'TSNamespaceExportDeclaration';
  /**
   * The name the global variable being exported to
   */
  readonly id: Identifier;
}
export interface TSNeverKeyword extends BaseNode {
  readonly type: 'TSNeverKeyword';
}
export interface TSNonNullExpression extends BaseNode {
  readonly type: 'TSNonNullExpression';
  readonly expression: Expression;
}
export interface TSNullKeyword extends BaseNode {
  readonly type: 'TSNullKeyword';
}
export interface TSNumberKeyword extends BaseNode {
  readonly type: 'TSNumberKeyword';
}
export interface TSObjectKeyword extends BaseNode {
  readonly type: 'TSObjectKeyword';
}
export interface TSOptionalType extends BaseNode {
  readonly type: 'TSOptionalType';
  readonly typeAnnotation: TypeNode;
}
export interface TSParameterProperty extends BaseNode {
  readonly type: 'TSParameterProperty';
  readonly accessibility?: Accessibility;
  readonly readonly?: boolean;
  readonly static?: boolean;
  readonly export?: boolean;
  readonly override?: boolean;
  readonly parameter: AssignmentPattern | BindingName | RestElement;
  readonly decorators?: ReadonlyArray<Decorator>;
}
export interface TSPrivateKeyword extends BaseNode {
  readonly type: 'TSPrivateKeyword';
}
export type TSPropertySignature =
  TSPropertySignatureComputedName | TSPropertySignatureNonComputedName;
interface TSPropertySignatureBase extends BaseNode {
  readonly type: 'TSPropertySignature';
  readonly accessibility?: Accessibility;
  readonly computed: boolean;
  readonly export?: boolean;
  readonly initializer?: Expression;
  readonly key: PropertyName;
  readonly optional?: boolean;
  readonly readonly?: boolean;
  readonly static?: boolean;
  readonly typeAnnotation?: TSTypeAnnotation;
}
export interface TSPropertySignatureComputedName extends TSPropertySignatureBase {
  readonly type: 'TSPropertySignature';
  readonly key: PropertyNameComputed;
  readonly computed: true;
}
export interface TSPropertySignatureNonComputedName extends TSPropertySignatureBase {
  readonly type: 'TSPropertySignature';
  readonly key: PropertyNameNonComputed;
  readonly computed: false;
}
export interface TSProtectedKeyword extends BaseNode {
  readonly type: 'TSProtectedKeyword';
}
export interface TSPublicKeyword extends BaseNode {
  readonly type: 'TSPublicKeyword';
}
export interface TSQualifiedName extends BaseNode {
  readonly type: 'TSQualifiedName';
  readonly left: EntityName;
  readonly right: Identifier;
}
export interface TSReadonlyKeyword extends BaseNode {
  readonly type: 'TSReadonlyKeyword';
}
export interface TSRestType extends BaseNode {
  readonly type: 'TSRestType';
  readonly typeAnnotation: TypeNode;
}
export interface TSStaticKeyword extends BaseNode {
  readonly type: 'TSStaticKeyword';
}
export interface TSStringKeyword extends BaseNode {
  readonly type: 'TSStringKeyword';
}
export interface TSSymbolKeyword extends BaseNode {
  readonly type: 'TSSymbolKeyword';
}
export interface TSTemplateLiteralType extends BaseNode {
  readonly type: 'TSTemplateLiteralType';
  readonly quasis: ReadonlyArray<TemplateElement>;
  readonly types: ReadonlyArray<TypeNode>;
}
export interface TSThisType extends BaseNode {
  readonly type: 'TSThisType';
}
export interface TSTupleType extends BaseNode {
  readonly type: 'TSTupleType';
  readonly elementTypes: ReadonlyArray<TypeNode>;
}
export interface TSTypeAliasDeclaration extends BaseNode {
  readonly type: 'TSTypeAliasDeclaration';
  /**
   * Whether the type was `declare`d.
   * ```
   * declare type T = 1;
   * ```
   */
  readonly declare?: boolean;
  /**
   * The name of the type.
   */
  readonly id: Identifier;
  /**
   * The "value" (type) of the declaration
   */
  readonly typeAnnotation: TypeNode;
  /**
   * The generic type parameters declared for the type.
   * This is `undefined` if there are no generic type parameters declared.
   */
  readonly typeParameters?: TSTypeParameterDeclaration;
}
export interface TSTypeAnnotation extends BaseNode {
  readonly type: 'TSTypeAnnotation';
  readonly typeAnnotation: TypeNode;
}
export interface TSTypeAssertion extends BaseNode {
  readonly type: 'TSTypeAssertion';
  readonly typeAnnotation: TypeNode;
  readonly expression: Expression;
}
export interface TSTypeLiteral extends BaseNode {
  readonly type: 'TSTypeLiteral';
  readonly members: ReadonlyArray<TypeElement>;
}
export interface TSTypeOperator extends BaseNode {
  readonly type: 'TSTypeOperator';
  readonly operator: 'keyof' | 'readonly' | 'unique';
  readonly typeAnnotation?: TypeNode;
}
export interface TSTypeParameter extends BaseNode {
  readonly type: 'TSTypeParameter';
  readonly name: Identifier;
  readonly constraint?: TypeNode;
  readonly default?: TypeNode;
  readonly in: boolean;
  readonly out: boolean;
}
export interface TSTypeParameterDeclaration extends BaseNode {
  readonly type: 'TSTypeParameterDeclaration';
  readonly params: ReadonlyArray<TSTypeParameter>;
}
export interface TSTypeParameterInstantiation extends BaseNode {
  readonly type: 'TSTypeParameterInstantiation';
  readonly params: ReadonlyArray<TypeNode>;
}
export interface TSTypePredicate extends BaseNode {
  readonly type: 'TSTypePredicate';
  readonly asserts: boolean;
  readonly parameterName: Identifier | TSThisType;
  readonly typeAnnotation: TSTypeAnnotation | null;
}
export interface TSTypeQuery extends BaseNode {
  readonly type: 'TSTypeQuery';
  readonly exprName: EntityName | TSImportType;
  readonly typeArguments?: TSTypeParameterInstantiation;
}
export interface TSTypeReference extends BaseNode {
  readonly type: 'TSTypeReference';
  readonly typeName: EntityName;
  readonly typeArguments?: TSTypeParameterInstantiation;
}
export type TSUnaryExpression =
  AwaitExpression | LeftHandSideExpression | UnaryExpression | UpdateExpression;
export interface TSUndefinedKeyword extends BaseNode {
  readonly type: 'TSUndefinedKeyword';
}
export interface TSUnionType extends BaseNode {
  readonly type: 'TSUnionType';
  readonly types: ReadonlyArray<TypeNode>;
}
export interface TSUnknownKeyword extends BaseNode {
  readonly type: 'TSUnknownKeyword';
}
export interface TSVoidKeyword extends BaseNode {
  readonly type: 'TSVoidKeyword';
}
export type TypeElement =
  | TSCallSignatureDeclaration
  | TSConstructSignatureDeclaration
  | TSIndexSignature
  | TSMethodSignature
  | TSPropertySignature;
export type TypeNode =
  | TSAbstractKeyword
  | TSAnyKeyword
  | TSArrayType
  | TSAsyncKeyword
  | TSBigIntKeyword
  | TSBooleanKeyword
  | TSConditionalType
  | TSConstructorType
  | TSDeclareKeyword
  | TSExportKeyword
  | TSFunctionType
  | TSImportType
  | TSIndexedAccessType
  | TSInferType
  | TSIntersectionType
  | TSIntrinsicKeyword
  | TSLiteralType
  | TSMappedType
  | TSNamedTupleMember
  | TSNeverKeyword
  | TSNullKeyword
  | TSNumberKeyword
  | TSObjectKeyword
  | TSOptionalType
  | TSQualifiedName
  | TSPrivateKeyword
  | TSProtectedKeyword
  | TSPublicKeyword
  | TSReadonlyKeyword
  | TSRestType
  | TSStaticKeyword
  | TSStringKeyword
  | TSSymbolKeyword
  | TSTemplateLiteralType
  | TSThisType
  | TSTupleType
  | TSTypeLiteral
  | TSTypeOperator
  | TSTypePredicate
  | TSTypeQuery
  | TSTypeReference
  | TSUndefinedKeyword
  | TSUnionType
  | TSUnknownKeyword
  | TSVoidKeyword;
export interface UnaryExpression extends UnaryExpressionBase {
  readonly type: 'UnaryExpression';
  readonly operator: '-' | '!' | '+' | '~' | 'delete' | 'typeof' | 'void';
}
interface UnaryExpressionBase extends BaseNode {
  readonly operator: string;
  readonly prefix: boolean;
  readonly argument: LeftHandSideExpression | Literal | UnaryExpression;
}
export interface UpdateExpression extends UnaryExpressionBase {
  readonly type: 'UpdateExpression';
  readonly operator: '--' | '++';
}
export interface VariableDeclaration extends BaseNode {
  readonly type: 'VariableDeclaration';
  /**
   * The variables declared by this declaration.
   * Note that there may be 0 declarations (i.e. `const;`).
   * ```
   * let x;
   * let y, z;
   * ```
   */
  readonly declarations: ReadonlyArray<VariableDeclarator>;
  /**
   * Whether the declaration is `declare`d
   * ```
   * declare const x = 1;
   * ```
   */
  readonly declare?: boolean;
  /**
   * The keyword used to declare the variable(s)
   * ```
   * const x = 1;
   * let y = 2;
   * var z = 3;
   * ```
   */
  readonly kind: 'const' | 'let' | 'var';
}
export interface VariableDeclarator extends BaseNode {
  readonly type: 'VariableDeclarator';
  readonly id: BindingName;
  readonly init: Expression | null;
  readonly definite?: boolean;
}
export interface WhileStatement extends BaseNode {
  readonly type: 'WhileStatement';
  readonly test: Expression;
  readonly body: Statement;
}
export interface WithStatement extends BaseNode {
  readonly type: 'WithStatement';
  readonly object: Expression;
  readonly body: Statement;
}
export interface YieldExpression extends BaseNode {
  readonly type: 'YieldExpression';
  readonly delegate: boolean;
  readonly argument?: Expression;
}
