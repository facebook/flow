var types = require("ast-types/lib/types");
var defaults = require("ast-types/lib/shared").defaults;
var def = types.Type.def;
var or = types.Type.or;

def("TypeParameter")
  .bases("Type")
  .build("name", "variance", "bound", "default")
  .field("name", String)
  .field("variance",
    or("plus", "minus", null),
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
    def("InterfaceDeclaration"),
    null
  ))

def("DeclareExportAllDeclaration")
    .bases("Declaration")
    .build("source")
    .field("source", def("Literal"));


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
      def("TypeAlias"),
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
    .field("properties", [or(
      def("ObjectTypeProperty"),
      def("ObjectTypeSpreadProperty"))]);

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
  .field("argument", def("GenericTypeAnnotation"));

// https://github.com/benjamn/ast-types/issues/186
def("ForAwaitStatement")
  .bases("Statement")
  .build("left", "right", "body")
  .field("left", or(
    def("VariableDeclaration"),
    def("Expression")))
  .field("right", def("Expression"))
  .field("body", def("Statement"));
