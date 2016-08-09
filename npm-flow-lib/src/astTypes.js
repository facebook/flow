// @flow

/**
 * Flow-specific (non-ESTree) nodes
 */

type linecol = {line: number, column: number};

export type TFlowComment =
  | TFlowCommentBlock
  | TFlowCommentLine
;

export type TFlowCommentBlock = {
  type: "Block",
  loc: TFlowLoc,
  range: TFlowRange,
  value: string,
};

export type TFlowCommentLine = {
  type: "Line",
  loc: TFlowLoc,
  range: TFlowRange,
  value: string,
};

export type TFlowLoc = {
  source: ?string,
  start: linecol,
  end: linecol,
};

export type TFlowParseError = {
  loc: TFlowLoc,
  message: string,
}

export type TFlowRange = [number, number];

// TODO: Type annotations

/**
 * ESTree nodes
 */

export type TArrayExpression = {
  type: "ArrayExpression",
  elements: [] | Array<TExpression>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TArrayPattern = {
  type: "ArrayPattern",
  elements: Array<TPattern | null>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TArrowFunctionExpression = {
  type: "ArrowFunctionExpression",
  body: TBlockStatement | TExpression,
  expression: boolean,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TAssignmentExpression = {
  type: "AssignmentExpression",
  operator: "=" | "+=" | "-=" | "*=" | "/=" | "%=" | "<<=" | ">>=" | ">>>=" |
            "|=" | "^=" | "&=",
  left: TPattern | TExpression,
  right: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TBinaryExpression = {
  type: "BinaryExpression",
  operator: "==" | "!=" | "===" | "!==" | "<" | "<=" | ">" | ">=" | "<<" |
            ">>" | ">>>" | "+" | "-" | "*" | "/" | "%" | "|" | "^" | "&" |
            "in" | "instanceof",
  left: TExpression,
  right: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TBlockStatement = {
  type: "BlockStatement",
  body: Array<TStatement>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TBreakStatement = {
  type: "BreakStatement",
  label: TIdentifier | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TCallExpression = {
  type: "CallExpression",
  callee: TExpression,
  arguments: [] | Array<TExpression>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TCatchClause = {
  type: "CatchClause",
  param: TPattern,
  body: TBlockStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TClassBody = {
  type: "ClassBody",
  body: Array<TMethodDefinition>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TClassDeclaration = {
  type: "ClassDeclaration",
  id: TIdentifier,
  superClass: TExpression | null,
  body: TClassBody,
  typeParameters: null, // TODO
  superTypeParameters: null, // TODO
  implements: Array<string>, // TODO
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TClassExpression = {
  type: "ClassExpression",
  id: TIdentifier | null,
  superClass: TExpression | null,
  typeParameters: Object, /* TODO */
  body: TClassBody,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TConditionalExpression = {
  type: "ConditionalExpression",
  test: TExpression,
  alternate: TExpression,
  consequence: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TContinueStatement = {
  type: "ContinueStatement",
  label: TIdentifier | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TDebuggerStatement = {
  type: "DebuggerStatement",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TDoWhileStatement = {
  type: "DoWhileStatement",
  body: TStatement,
  test: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TEmptyStatement = {
  type: "EmptyStatement",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TExpression =
  | TArrayExpression
  | TArrowFunctionExpression
  | TAssignmentExpression
  | TBinaryExpression
  | TCallExpression
  | TClassExpression
  | TConditionalExpression
  | TFunctionExpression
  | TIdentifier
  | TLiteral
  | TLogicalExpression
  | TMemberExpression
  | TMetaProperty
  | TNewExpression
  | TObjectExpression
  | TSequenceExpression
  | TTaggedTemplateExpression
  | TTemplateElement
  | TTemplateLiteral
  | TThisExpression
  | TUnaryExpression
  | TUpdateExpression
  | TYieldExpression
;

export type TExpressionStatement = {
  type: "ExpressionStatement",
  expression: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TForInStatement = {
  type: "ForInStatement",
  left: TVariableDeclaration | TPattern,
  right: TExpression,
  body: TStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TForOfStatement = {
  type: "ForOfStatement",
  left: TVariableDeclaration | TPattern,
  right: TExpression,
  body: TStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TForStatement = {
  type: "ForStatement",
  init: TVariableDeclaration | TExpression | null,
  test: TExpression | null,
  update: TExpression | null,
  body: TStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TFunctionDeclaration = {
  type: "FunctionDeclaration",
  id: TIdentifier,
  params: Array<TPattern>,
  defaults: Array<TExpression>,
  body: TBlockStatement,
  async: boolean,
  generator: boolean,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TFunctionExpression = {
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TIdentifier = {
  type: "Identifier",
  name: string,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TIfStatement = {
  type: "IfStatement",
  test: TExpression,
  consequent: TStatement,
  alternate: TStatement | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TImportDeclaration = {
  type: "ImportDeclaration",
  specifiers: Array<
    TImportSpecifier | TImportDefaultSpecifier | TImportNamespaceSpecifier
  >,
  source: TLiteral,
  importKind: "value" | "type",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TImportDefaultSpecifier = {
  type: "ImportDefaultSpecifier",
  id: TIdentifier, // TODO: This key should be called 'local'
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TImportNamespaceSpecifier = {
  type: "ImportNamespaceSpecifier",
  id: TIdentifier, // TODO: This key should be called 'local'
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TImportSpecifier = {
  type: "ImportSpecifier",
  id: TIdentifier, // TODO: This key should be called 'imported'
  name: TIdentifier | null, // TODO: This key should be called 'local'
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TLabeledStatement = {
  type: "LabeledStatement",
  label: TIdentifier,
  body: TStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TLiteral = {
  type: "Literal",
  value: mixed,
  raw: string,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TLogicalExpression = {
  type: "LogicalExpression",
  operator: "||" | "&&",
  left: TExpression,
  right: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TMemberExpression = {
  type: "MemberExpression",
  object: TExpression | TSuper,
  property: TExpression,
  computed: boolean,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TMetaProperty = {
  type: "MetaProperty",
  meta: TIdentifier,
  property: TIdentifier,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TMethodDefinition = {
  type: "MethodDefinition",
  key: TExpression,
  value: TFunctionExpression,
  kind: "constructor" | "method" | "get" | "set",
  computed: boolean,
  static: boolean,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TNewExpression = {
  type: "NewExpression",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TObjectExpression = {
  type: "ObjectExpression",
  properties: Array<TProperty>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TObjectPattern = {
  type: "ObjectPattern",
  properties: Array<{
    // TODO: This seems to be underspecified in ESTree
    //       (missing `key` prop, `kind: "init"` is unnecessary, `method: false`
    //        unnecessary)
    type: "PropertyPattern", // TODO: ESTree specifies this as "Property"
    key: TIdentifier,
    pattern: TPattern,
    //kind: "init",  // TODO: ESTree specifies this, but it's missing from Flow
    //method: false, // TODO: ESTree specifies this, but it's missing from Flow
    computed: false, // TODO: Remove this -- it's unnecessary for a pattern
  }>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TPattern =
    TIdentifier
  | TMemberExpression
  | TArrayPattern
  | TObjectPattern
  | TRestElement

export type TProgram = {
  type: "Program",
  errors: Array<TFlowParseError>,
  loc: TFlowLoc,
  range: TFlowRange,
  body: Array<TStatement>,
  comments: Array<TFlowComment>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TProperty = {
  type: "Property",
  key: TLiteral | TIdentifier,
  value: TExpression,
  kind: "init" | "get" | "set",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TRestElement = {
  type: "RestElement",
  argument: TPattern,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TReturnStatement = {
  type: "ReturnStatement",
  argument: TExpression | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TSequenceExpression = {
  type: "SequenceExpression",
  expressions: Array<TExpression>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TStatement =
  | TBlockStatement
  | TBreakStatement
  | TClassDeclaration
  | TContinueStatement
  | TDebuggerStatement
  | TDoWhileStatement
  | TEmptyStatement
  | TExpressionStatement
  | TForInStatement
  | TForOfStatement
  | TForStatement
  | TFunctionDeclaration
  | TIfStatement
  | TImportDeclaration
  | TLabeledStatement
  | TReturnStatement
  | TSwitchStatement
  | TThrowStatement
  | TTryStatement
  | TVariableDeclaration
  | TWhileStatement
  | TWithStatement
;

export type TSuper = {
  type: "Super",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TSwitchStatement = {
  type: "SwitchStatement",
  discriminant: TExpression,
  cases: Array<{
    type: "SwitchCase",
    test: TExpression | null,
    consequent: Array<TStatement>,
    loc: TFlowLoc,
    range: TFlowRange,
  }>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TTaggedTemplateExpression = {
  type: "TaggedTemplateExpression",
  tag: TExpression,
  quasi: TTemplateLiteral,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TTemplateElement = {
  type: "TemplateElement",
  tail: boolean,
  value: {
    cooked: string,
    raw: string,
  },
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TTemplateLiteral = {
  type: "TemplateLiteral",
  quasis: Array<TTemplateElement>,
  expressions: Array<TExpression>,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TThisExpression = {
  type: "ThisExpression",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TThrowStatement = {
  type: "ThrowStatement",
  argument: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TTryStatement = {
  type: "TryStatement",
  block: TBlockStatement,
  handler: TCatchClause | null,
  finalizer: TBlockStatement | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TUnaryExpression = {
  type: "UnaryExpression",
  operator: "-" | "+" | "!" | "~" | "typeof" | "void" | "delete",
  prefix: boolean,
  argument: TExpression,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TUpdateExpression = {
  type: "UpdateExpression",
  operator: "++" | "--",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TVariableDeclaration = {
  type: "VariableDeclaration",
  declarations: Array<TVariableDeclarator>,
  kind: "var" | "let" | "const",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TVariableDeclarator = {
  type: "VariableDeclarator",
  id: TPattern,
  init: TExpression | null,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TWhileStatement = {
  type: "WhileStatement",
  test: TExpression,
  body: TStatement,
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TWithStatement = {
  type: "WithStatement",
  loc: TFlowLoc,
  range: TFlowRange,
};

export type TYieldExpression = {
  type: "YieldExpression",
  argument: TExpression | null,
  delegate: boolean,
  loc: TFlowLoc,
  range: TFlowRange,
};
