/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */
#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/alloc.h>

#include <assert.h>
#include <stdint.h>
#include <string.h>

#include <time.h>

#ifdef HH_BUILD_ID
#include "hphp/runtime/version.h"
#endif

extern const char* const BuildInfo_kRevision;
extern const uint64_t BuildInfo_kRevisionCommitTimeUnix;

#define STRINGIFY_HELPER(x) #x
#define STRINGIFY_VALUE(x) STRINGIFY_HELPER(x)

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

#ifdef HH_BUILD_ID
  const char* const buf =
    STRINGIFY_VALUE(HH_BUILD_ID) "-" HHVM_VERSION_C_STRING_LITERALS;
#else
  const char* const buf = BuildInfo_kRevision;
#endif
  const size_t len = strlen(buf);
  result = caml_alloc_string(len);

  memcpy(String_val(result), buf, len);

  CAMLreturn(result);
}

value hh_get_build_commit_time_string(void) {
  CAMLparam0();
  CAMLlocal1(result);

  char s[25];
  unsigned long timestamp = BuildInfo_kRevisionCommitTimeUnix;
#ifdef HH_BUILD_TIMESTAMP
  if (timestamp == 0) {
    timestamp = HH_BUILD_TIMESTAMP;
  }
#endif

  // A previous version used localtime_r, which is not available on Windows
  struct tm *p = localtime((time_t*)&timestamp);
  strftime(s, sizeof(s), "%c", p);

  result = caml_copy_string(s);
  CAMLreturn(result);
}

value hh_get_build_commit_time(void) {
  return Val_long(BuildInfo_kRevisionCommitTimeUnix);
}
