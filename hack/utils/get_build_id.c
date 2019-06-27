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
#include <stdio.h>
#include <string.h>

#include <time.h>

#ifdef NO_HHVM
#define HHVM_VERSION_MAJOR 0
#define HHVM_VERSION_MINOR 0
#else
#include "hphp/runtime/version.h"
#endif

extern const char* const BuildInfo_kRevision;
extern const uint64_t BuildInfo_kRevisionCommitTimeUnix;
extern const char* const BuildInfo_kBuildMode;

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

  const char* const buf = BuildInfo_kRevision;
  const size_t len = strlen(buf);
  result = caml_alloc_string(len);

  memcpy(String_val(result), buf, len);

  CAMLreturn(result);
}

static struct tm *get_built_timestamp(void) {
  unsigned long timestamp = BuildInfo_kRevisionCommitTimeUnix;
#ifdef HH_BUILD_TIMESTAMP
  if (timestamp == 0) {
    timestamp = HH_BUILD_TIMESTAMP;
  }
#endif
  // A previous version used localtime_r, which is not available on Windows
  return localtime((time_t*)&timestamp);
}

value hh_get_build_commit_time_string(void) {
  CAMLparam0();
  CAMLlocal1(result);

  char timestamp_string[25];
  struct tm *timestamp = get_built_timestamp();
  strftime(timestamp_string, sizeof(timestamp_string), "%c", timestamp);

  result = caml_copy_string(timestamp_string);
  CAMLreturn(result);
}

value hh_get_build_major(void) {
  return Val_long(HHVM_VERSION_MAJOR);
}

value hh_get_build_minor(void) {
  return Val_long(HHVM_VERSION_MINOR);
}

value hh_get_build_commit_time(void) {
  return Val_long(BuildInfo_kRevisionCommitTimeUnix);
}

value hh_get_build_mode(void) {
  CAMLparam0();
  CAMLlocal1(result);

  const size_t len = strlen(BuildInfo_kBuildMode);
  result = caml_alloc_string(len);

  memcpy(String_val(result), BuildInfo_kBuildMode, len);

  CAMLreturn(result);
}

value hh_get_build_banner(void) {
  CAMLparam0();
  CAMLlocal1(result);

  char timestamp_string[25] = { 0 };
#ifdef HH_BUILD_BANNER
  const char* const buf =
    STRINGIFY_VALUE(HH_BUILD_BANNER) "-" HHVM_VERSION_C_STRING_LITERALS;
  const size_t len = strlen(buf);
  const size_t timestamp_string_len = 0;
#else
  const char* const buf = BuildInfo_kRevision;
  const size_t len = strlen(buf);
  struct tm *timestamp = get_built_timestamp();
  strftime(timestamp_string, sizeof(timestamp_string), "%c", timestamp);
  const size_t timestamp_string_len = strlen(timestamp_string);
#endif
  result = caml_alloc_string(len + timestamp_string_len + 1);

  sprintf(String_val(result), "%s %s", buf, timestamp_string);

  CAMLreturn(result);
}
