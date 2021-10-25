/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/**
 * Export the constants provided by Facebook's build system to ocaml-land, since
 * their FFI only allows you to call functions, not reference variables. Doing
 * it this way makes sense for Facebook internally since our build system has
 * machinery for providing these two constants automatically (and no machinery
 * for doing codegen in a consistent way to build an ocaml file with them) but
 * is very roundabout for external users who have to have CMake codegen these
 * constants anyways. Sorry about that.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>

extern const char* const BuildInfo_kRevision;

value hh_get_build_revision(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_string(BuildInfo_kRevision));
}
