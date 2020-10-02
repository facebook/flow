/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//Provides: hh_get_build_revision const
//Requires: caml_js_to_string
function hh_get_build_revision() {
  return caml_js_to_string("js_of_ocaml_build");
}


//Provides: hh_get_build_commit_time_string const
//Requires: caml_js_to_string
function hh_get_build_commit_time_string() {
  return caml_js_to_string("build_time");
}

//Provides: hh_get_build_commit_time const
function hh_get_build_commit_time() {
  return 0;
}
