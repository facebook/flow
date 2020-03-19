var types = require("ast-types/lib/types");
var defaults = require("ast-types/lib/shared").defaults;
var def = types.Type.def;
var or = types.Type.or;

def("Variance")
  .bases("Node")
  .build("kind")
  .field("kind", or("plus", "minus"));

def("TypeParameter")
  .bases("Type")
  .build("name", "variance", "bound", "default")
  .field("name", String)
  .field("variance",
    or(def("Variance"), null),
    defaults["null"])
  .field("bound",
    or(def("TypeAnnotation"), null),
    defaults["null"])
  .field("default",
    or(def("Type"), null),
    defaults["null"]);

def("DeclareModuleExports")
  .bases("Statement")
  .build("typeAnnotation")
  .field("typeAnnotation", def("TypeAnnotation"));

def("DeclareExportDeclaration")
  .field("declaration", or(
    def("DeclareVariable"),
    def("DeclareFunction"),
    def("DeclareClass"),
    def("Type"), // Implies default type
    def("TypeAlias"), // Implies named type
    def("DeclareOpaqueType"), // Implies named opaque type
    def("InterfaceDeclaration"),
    null
  ))

def("DeclareExportAllDeclaration")
    .bases("Declaration")
    .build("source")
    .field("source", def("Literal"));

def("Decorator")
  .bases("Node")
  .build("expression")
  .field("expression", def("Expression"));

def("OpaqueType")
  .bases("Declaration")
  .build("id", "typeParameters", "impltype", "supertype")
  .field("id", def("Identifier"))
  .field("typeParameters", or(def("TypeParameterDeclaration"), null))
  .field("impltype", def("Type"))
  .field("supertype", or(def("Type"), null));

def("DeclareOpaqueType")
  .bases("OpaqueType")
  .build("id", "typeParameters", "impltype", "supertype")
  .field("impltype", or(def("Type"), null));

def("PrivateName")
  .bases("Expression")
  .field("id", def("Identifier"))

def("ClassPrivateProperty")
  .bases("ClassProperty")
  .field("key", def("Identifier"))


// TODO: should be named NullableClassDeclaration. estree allows a nameless
// decl inside an `export default` (https://github.com/estree/estree/issues/98),
// but ast-types uses the `TypeName` in `def("TypeName")` as the expected `type`
// property, which isn't the case here. So, we incorrectly have to loosen all
// ClassDeclarations for now.
def("ClassDeclaration")
    .field("id", or(def("Identifier"), null))

// TODO: should be named NullableFunctionDeclaration. estree allows a nameless
// decl inside an `export default` (https://github.com/estree/estree/issues/98),
// but ast-types uses the `TypeName` in `def("TypeName")` as the expected `type`
// property, which isn't the case here. So, we incorrectly have to loosen all
// FunctionDeclarations for now.
def("FunctionDeclaration")
    .field("id", or(def("Identifier"), null))

// See https://github.com/benjamn/ast-types/issues/180
def("ExportDefaultDeclaration")
    .bases("Declaration")
    .build("declaration", "exportKind")
    .field("declaration", or(
      def("ClassDeclaration"), // TODO: should be NullableClassDeclaration
      def("FunctionDeclaration"), // TODO: should be NullableFunctionDeclaration
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

var BinaryOperator = or(
    "==", "!=", "===", "!==",
    "<", "<=", ">", ">=",
    "<<", ">>", ">>>",
    "+", "-", "*", "/", "%",
    "&", // TODO Missing from the Parser API.
    "|", "^", "in",
    "instanceof", "..",
    "**");

def("BinaryExpression")
    .bases("Expression")
    .build("operator", "left", "right")
    .field("operator", BinaryOperator)
    .field("left", def("Expression"))
    .field("right", def("Expression"));

var AssignmentOperator = or(
    "=", "+=", "-=", "*=", "/=", "%=",
    "<<=", ">>=", ">>>=",
    "|=", "^=", "&=",
    "**=");

def("AssignmentExpression")
    .bases("Expression")
    .build("operator", "left", "right")
    .field("operator", AssignmentOperator)
    .field("left", def("Pattern"))
    .field("right", def("Expression"));

def("Predicate")
    .bases("Node")

def("InferredPredicate")
    .bases("Predicate")
    .build()

def("DeclaredPredicate")
    .bases("Predicate")
    .build("value")
    .field("value", def("Expression"))

def("DeclareFunction")
    .field("predicate", or(def("Predicate"), null), defaults["null"])

def("Function")
    .field("predicate", or(def("Predicate"), null), defaults["null"])

def("ObjectTypeAnnotation")
    .field("exact", Boolean)
    .field("inexact", or(Boolean, void 0), defaults["undefined"])
    .field("properties", [or(
      def("ObjectTypeProperty"),
      def("ObjectTypeSpreadProperty"))])
    .field("internalSlots", [def("ObjectTypeInternalSlot")]);

def("InterfaceTypeAnnotation")
    .bases("Type")
    .build("body", "extends")
    .field("body", def("ObjectTypeAnnotation"))
    .field("extends", or([def("InterfaceExtends")], null), defaults["null"]);

def("MetaProperty")
    .bases("Expression")
    .build("meta", "property")
    .field("meta", def("Identifier"))
    .field("property", def("Identifier"));

// https://github.com/benjamn/ast-types/pull/162
def("ObjectPattern")
  .bases("Pattern")
  .build("properties")
  .field("properties", [or(def("RestProperty"), def("Property"))]);
def("RestProperty")
  .bases("Node")
  .build("argument")
  .field("argument", def("Expression"));

// https://github.com/benjamn/ast-types/issues/183
def("Super")
  .bases("Expression")
  .build();

def("FunctionTypeParam")
  .bases("Node")
  .build("typeAnnotation", "optional")
  .field("name", or(def("Identifier"), null))
  .field("typeAnnotation", def("Type"))
  .field("optional", Boolean);

def("ObjectTypeIndexer")
  .bases("Node")
  .build("id", "key", "value")
  .field("id", or(def("Identifier"), null))
  .field("key", def("Type"))
  .field("value", def("Type"));

def("ObjectTypeSpreadProperty")
  .bases("Node")
  .build("argument")
  .field("argument", def("Type"));

def("ObjectTypeInternalSlot")
  .bases("Node")
  .build("id", "static", "method")
  .field("id", def("Identifier"))
  .field("static", Boolean)
  .field("method", Boolean)
  .field("value", def("Type"));

// https://github.com/benjamn/ast-types/issues/186
def("ForAwaitStatement")
  .bases("Statement")
  .build("left", "right", "body")
  .field("left", or(
    def("VariableDeclaration"),
    def("Expression")))
  .field("right", def("Expression"))
  .field("body", def("Statement"));

def('Import')
  .bases("Node")

def('CallExpression')
  .field('callee', or(def('Expression'), def('Import')));

def('OptionalMemberExpression')
  .bases("MemberExpression")
  .build("optional")
  .field("optional", Boolean)

def('OptionalCallExpression')
  .bases("CallExpression")
  .build("optional")
  .field("optional", Boolean)

def('LogicalExpression')
  .field('operator', or("||", "&&", "??"));

def("CatchClause")
    .bases("Node")
    .build("param", "guard", "body")
    // https://github.com/tc39/proposal-optional-catch-binding
    .field("param", or(def("Pattern"), null), defaults["null"])
    .field("guard", or(def("Expression"), null), defaults["null"])
    .field("body", def("BlockStatement"));

def("BigIntLiteral")
  .bases("Literal")
  .build("value", "bigint")
  .field("value", or(def("BigInt"), null))
  .field("bigint", String);

def("BigIntLiteralTypeAnnotation")
  .bases("Type")
  .build("value", "raw")
  .field("value", or(def("BigInt"), null))
  .field("raw", String);

def("SymbolTypeAnnotation")
  .bases("Type");

def("EmptyTypeAnnotation")
  .bases("Type")
  .build();

def("BigIntTypeAnnotation")
  .bases("Type")
  .build();

def("JSXElement")
  .field("children", [or(
    def("JSXElement"),
    def("JSXExpressionContainer"),
    def("JSXFragment"),
    def("JSXText"),
    def("Literal"),
    def("JSXSpreadChild")
  )], defaults.emptyArray);

def("JSXFragment")
  .bases("Expression")
  .build("openingFragment", "closingFragment", "children")
  .field("openingFragment", def("JSXOpeningFragment"))
  .field("closingFragment", def("JSXClosingFragment"))
  .field("children", [or(
    def("JSXElement"),
    def("JSXExpressionContainer"),
    def("JSXFragment"),
    def("JSXText"),
    def("Literal"),
    def("JSXSpreadChild")
  )], defaults.emptyArray)

def("JSXOpeningFragment")
  .bases("Node")
  .build();

def("JSXClosingFragment")
  .bases("Node")
  .build();

def("JSXSpreadChild")
  .bases("Expression")
  .build("expression")
  .field("expression", def("Expression"));

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
