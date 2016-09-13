var types = require("ast-types/lib/types");
var defaults = require("ast-types/lib/shared").defaults;
var def = types.Type.def;
var or = types.Type.or;

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

def("MetaProperty")
    .bases("Expression")
    .build("meta", "property")
    .field("meta", def("Identifier"))
    .field("property", def("Identifier"));
