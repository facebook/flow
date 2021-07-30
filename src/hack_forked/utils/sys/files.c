/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/fail.h>
#include <caml/memory.h>

#ifndef _WIN32
#include <sys/time.h>
#endif

void hh_lutimes(value filename_v) {
  CAMLparam1(filename_v);
#ifdef _WIN32
  /* Not implemented */
  CAMLreturn0;
#else
  const char* filename = String_val(filename_v);
  int success = lutimes(filename, NULL);
  if (success != 0) {
    caml_failwith("lutimes failed");
  }
#endif
  CAMLreturn0;
}
