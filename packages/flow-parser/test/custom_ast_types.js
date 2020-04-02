import typesPlugin from "ast-types/lib/types";
import sharedPlugin from "ast-types/lib/shared";

export default function (fork) {
  const {Type: {def, or}} = fork.use(typesPlugin);
  const shared = fork.use(sharedPlugin);
  const defaults = shared.defaults;

//////////////////////
  /////////
  // flow
  /////////
  def("ImportDeclaration")
    .field("importKind", or("value", "type", "typeof"), () => "value");

  def("OpaqueType")
    .field("supertype", or(def("FlowType"), null));

  def("DeclareOpaqueType")
    .bases("OpaqueType")
    .field("impltype", or(def("FlowType"), null))
    .field("right", undefined); // Hack to remove incorrect field in upstream

  def("SymbolTypeAnnotation")
    .bases("FlowType")
    .build();

  def("BigIntLiteralTypeAnnotation")
    .bases("FlowType")
    .build("value", "raw")
    .field("value", null)
    .field("raw", String);

  def("BigIntTypeAnnotation")
    .bases("FlowType")
    .build();

  def("DeclareFunction")
    .field("predicate", or(def("FlowPredicate"), null), defaults["null"])

  def("Function")
    .field("predicate", or(def("FlowPredicate"), null), defaults["null"])

  def("TypeParameter")
    .build("name", "variance", "bound", "default")
    .field("default",
      or(def("FlowType"), null),
      defaults["null"]);

  def("FunctionTypeParam")
    .field("name", or(def("Identifier"), null));

  def("ObjectTypeIndexer")
    .field("id", or(def("Identifier"), null))
    .field("static", Boolean);

  def("DeclareExportDeclaration")
    .field("declaration", or(
      def("DeclareVariable"),
      def("DeclareFunction"),
      def("DeclareClass"),
      def("FlowType"), // Implies default type
      def("TypeAlias"), // Implies named type
      def("DeclareOpaqueType"), // Implies named opaque type
      def("InterfaceDeclaration"),
      null
    ));

  // See https://github.com/benjamn/ast-types/issues/180
  def("ExportDefaultDeclaration")
      .bases("Declaration")
      .build("declaration", "exportKind")
      .field("declaration", or(
        def("ClassDeclaration"),
        def("FunctionDeclaration"),
        def("VariableDeclaration"),
        def("InterfaceDeclaration"),
        def("EnumDeclaration"),
        def("TypeAlias"),
        def("OpaqueType"),
        def("Expression")))
      .field("exportKind", or("type", "value"));

  // See https://github.com/benjamn/ast-types/issues/180
  def("ExportNamedDeclaration")
      .bases("Declaration")
      .build("declaration", "specifiers", "source", "exportKind")
      .field("declaration", or(def("Declaration"), null))
      // TODO: this is non-standard. should be this:
      // .field("specifiers", [def("ExportSpecifier")], defaults.emptyArray)
      .field("specifiers", [or(
          def("ExportSpecifier"),
          def("ExportNamespaceSpecifier")
        )], defaults.emptyArray)
      .field("source", or(def("Literal"), null), defaults["null"])
      .field("exportKind", or("type", "value"));

  // See https://github.com/benjamn/ast-types/issues/180
  def("ExportAllDeclaration")
      .bases("Declaration")
      .build("source", "exportKind")
      .field("source", def("Literal"))
      .field("exportKind", or("type", "value"));

  // See https://github.com/benjamn/ast-types/issues/180
  def("ExportNamespaceSpecifier")
      .bases("Specifier")
      .build("exported")
      .field("exported", def("Identifier"));

  // Enums
  def("EnumDeclaration")
    .bases("Declaration")
    .build("id", "body")
    .field("id", def("Identifier"))
    .field("body", or(
      def("EnumBooleanBody"),
      def("EnumNumberBody"),
      def("EnumStringBody"),
      def("EnumSymbolBody")))

  def("EnumBooleanBody")
    .build("members", "explicitType")
    .field("members", [def("EnumBooleanMember")])
    .field("explicitType", Boolean)

  def("EnumNumberBody")
    .build("members", "explicitType")
    .field("members", [def("EnumNumberMember")])
    .field("explicitType", Boolean)

  def("EnumStringBody")
    .build("members", "explicitType")
    .field("members", [or(def("EnumStringMember"), def("EnumDefaultedMember"))])
    .field("explicitType", Boolean)

  def("EnumSymbolBody")
    .build("members")
    .field("members", [def("EnumDefaultedMember")])

  def("EnumBooleanMember")
    .build("id", "init")
    .field("id", def("Identifier"))
    .field("init", Boolean)

  def("EnumNumberMember")
    .build("id", "init")
    .field("id", def("Identifier"))
    .field("init", def("Literal"))

  def("EnumStringMember")
    .build("id", "init")
    .field("id", def("Identifier"))
    .field("init", def("Literal"))

  def("EnumDefaultedMember")
    .build("id")
    .field("id", def("Identifier"))

  /////////
  // jsx
  /////////
  def("JSXSpreadChild")
    .bases("Expression")
    .build("expression")
    .field("expression", def("Expression"));

  def("JSXFragment")
    .bases("Expression")
    .build("openingFragment", "closingFragment", "children")
    .field("openingFragment", def("JSXOpeningFragment"))
    .field("closingFragment", def("JSXClosingFragment"))
    .field("children", [or(
      def("JSXText"),
      def("JSXExpressionContainer"),
      def("JSXSpreadChild"), // <- Added
      def("JSXElement"),
      def("JSXFragment"),
      def("Literal"), // Remove?
    )], defaults.emptyArray)
    .field("openingElement", undefined) // Hack to remove incorrect field in upstream
    .field("closingElement", undefined); // Hack to remove incorrect field in upstream

  def("JSXElement")
    .field("children", [or(
      def("JSXText"),
      def("JSXExpressionContainer"),
      def("JSXSpreadChild"), // <- Added
      def("JSXElement"),
      def("JSXFragment"),
      def("Literal"), // Remove?
    )], defaults.emptyArray);

  /////////
  // es2015 (es6)
  /////////
  // in 'babel' defs, move
  def("Super")
    .bases("Expression")
    .build();

  // in `babel` defs, move
  def("MetaProperty")
      .bases("Expression")
      .build("meta", "property")
      .field("meta", def("Identifier"))
      .field("property", def("Identifier"));

  // estree allows a nameless decl inside an `export default`
  // (https://github.com/estree/estree/issues/98),
  // but ast-types uses the `TypeName` in `def("TypeName")` as the expected `type`
  // property, which isn't the case here. So, we incorrectly have to loosen all
  // FunctionDeclarations for now.
  def("FunctionDeclaration")
      .field("id", or(def("Identifier"), null))

  /////////
  // es2016 (es7)
  /////////
  var AssignmentOperator = or(
      "=", "+=", "-=", "*=", "/=", "%=",
      "<<=", ">>=", ">>>=",
      "|=", "^=", "&=",
      "**=");

  def("AssignmentExpression")
      .field("operator", AssignmentOperator)

  /////////
  // es2018
  /////////
  def("ObjectPattern")
    .field("properties", [or(def("Property"), def("PropertyPattern"), def("RestElement"))]);

  /////////
  // es2020
  /////////
  def("BigIntLiteral")
    .bases("Literal")
    .build("bigint")
    .field("bigint", String);

  /////////
  // es-proposal
  /////////
  // in 'babel' defs, move
  def("Decorator")
    .bases("Node")
    .build("expression")
    .field("expression", def("Expression"));

  // in 'babel' defs, move
  def("PrivateName")
    .bases("Expression")
    .field("id", def("Identifier"))
  def("ClassPrivateProperty")
    .bases("ClassProperty")
    .field("key", def("Identifier"))

  /////////
  // legacy
  /////////
  // ESTree specifies `ImportExpression` instead, but Babel Parser
  // continues to output a `CallExpression`, so keep this for now
  def('Import')
    .bases("Node")
  def('CallExpression')
    .field('callee', or(def('Expression'), def('Import')));
}
