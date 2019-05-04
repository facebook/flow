// Stdlib overrides until js_of_ocaml fixes it

//Provides: caml_parse_sign_and_base
//Requires: caml_string_unsafe_get, caml_ml_string_length
function caml_parse_sign_and_base(s) {
  var i = 0, len = caml_ml_string_length(s), base = 10, sign = 1;
  if (len > 0) {
    switch (caml_string_unsafe_get(s, i)) {
      case 45: i++; sign = -1; break;
      case 43: i++; sign = 1; break;
    }
  }
  if (i + 1 < len && caml_string_unsafe_get(s, i) == 48)
    switch (caml_string_unsafe_get(s, i + 1)) {
      case 120: case 88: base = 16; i += 2; break;
      case 111: case 79: base = 8; i += 2; break;
      case 98: case 66: base = 2; i += 2; break;
      case 117: case 85: sign = 0; i += 2; break;
    }
  return [i, sign, base];
}