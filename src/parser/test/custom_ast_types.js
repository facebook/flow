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

def("ExistsTypeAnnotation")
  .bases("Type")
  .build();
