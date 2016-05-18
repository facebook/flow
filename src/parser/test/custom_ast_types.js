var types = require("ast-types/lib/types");
var defaults = require("ast-types/lib/shared").defaults;
var def = types.Type.def;
var or = types.Type.or;

def("TypeParameter")
.bases("Identifier")
.build("variance", "default")
.field("variance", or(String, null))
.field("default", or(def("Type"), null));

def("DeclareModuleExports")
  .bases("Statement")
  .build("typeAnnotation")
  .field("typeAnnotation", def("TypeAnnotation"));

def("DeclareModule")
  .bases("Statement")
  .build("id", "body", "kind")
  .field("id", or(def("Identifier"), def("Literal")))
  .field("body", def("BlockStatement"))
  .field("kind", or("CommonJS", "ES"));

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

def("ExistsTypeAnnotation")
  .bases("Type")
  .build();

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
