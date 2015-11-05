/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
#define _XOPEN_SOURCE

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>

#include <assert.h>
#include <string.h>

#include <time.h>

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
value hh_get_build_revision(void) {
  CAMLparam0();
  CAMLlocal1(result);

  size_t len = strlen(BuildInfo_kRevision);
  result = caml_alloc_string(len);

  memcpy(String_val(result), BuildInfo_kRevision, len);
  CAMLreturn(result);
}

value hh_get_build_time_string(void) {
  CAMLparam0();
  CAMLlocal1(result);

  size_t len = strlen(build_time);
  result = caml_alloc_string(len);

  memcpy(String_val(result), build_time, len);
  CAMLreturn(result);
}

value hh_get_build_time(void) {
#ifdef _WIN32
  return Val_long(0);
#else
  struct tm tm;
  char* success = strptime(build_time, "%b %d %Y %H:%M:%S", &tm);
  assert(success != NULL && "Failed to parse build time");
  return Val_long(mktime(&tm));
#endif
}
