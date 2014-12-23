/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
#include <caml/memory.h>
#include <caml/alloc.h>
#include <string.h>

extern const char* const BuildInfo_kRevision;
const char* const build_time = __DATE__ " " __TIME__;

/**
 * Export the constants provided by Facebook's build system to ocaml-land, since
 * their FFI only allows you to call functions, not reference variables. Doing
 * it this way makes sense for Facebook internally since our build system has
 * machinery for providing these two constants automatically (and no machinery
 * for doing codegen in a consistent way to build an ocaml file with them) but
 * is very roundabout for external users who have to have CMake codegen these
 * constants anyways. Sorry about that.
 */
value hh_get_build_id(void) {
  CAMLparam0();
  CAMLlocal1(result);

  size_t revlen = strlen(BuildInfo_kRevision);
  size_t timelen = strlen(build_time);
  result = caml_alloc_string(revlen + timelen + 1);

  memcpy(String_val(result), BuildInfo_kRevision, revlen);
  String_val(result)[revlen] = ' ';
  memcpy(String_val(result) + revlen + 1, build_time, timelen);

  CAMLreturn(result);
}
