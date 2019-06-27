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

extern const char* const build_id;

value hh_get_compiler_id(void) {
  CAMLparam0();
  const char* const buf = build_id;
  const ssize_t len = strlen(buf);
  value result;

  result = caml_alloc_string(len);
  memcpy(String_val(result), buf, len);
  CAMLreturn(result);
}
