//Provides: flow_ecma_string_of_float
//Requires: caml_js_to_string
function flow_ecma_string_of_float(float) {
  var ocaml_string = caml_js_to_string(float.toString());
  return ocaml_string;
}
