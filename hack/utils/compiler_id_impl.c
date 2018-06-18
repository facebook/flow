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
#include <string.h>

#include "hphp/util/embedded-data.h"

#define BUF_SIZE 64

static const char section_name[] = "build_id";
static const char default_id[] = "hackc-unknown-version";

#define STRINGIFY_HELPER(x) #x
#define STRINGIFY_VALUE(x) STRINGIFY_HELPER(x)

value hh_get_compiler_id(void) {
  CAMLparam0();
#ifdef HACKC_COMPILER_ID
  const char* const buf = STRINGIFY_VALUE(HACKC_COMPILER_ID);
  const ssize_t len = strlen(buf);
#else
  char buf[BUF_SIZE];
  const ssize_t len = hphp_read_embedded_data(section_name, buf, BUF_SIZE);
#endif
  value result;

  if (len < 0) {
    result = caml_alloc_string(strlen(default_id));
    memcpy(String_val(result), default_id, strlen(default_id));
    CAMLreturn(result);
  } else {
    result = caml_alloc_string(len);
    memcpy(String_val(result), buf, len);
    CAMLreturn(result);
  }
}
