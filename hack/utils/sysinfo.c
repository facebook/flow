/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#define CAML_NAME_SPACE
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <assert.h>
#ifndef _WIN32
#include <sys/sysinfo.h>
#endif

value hh_sysinfo_totalram(void) {
  CAMLparam0();
#ifdef _WIN32
  /* Not implemented */
  CAMLreturn(Val_long(0));
#else
  struct sysinfo info;
  int success = sysinfo(&info);
  assert(success == 0 && "sysinfo() failed");
  CAMLreturn(Val_long(info.totalram));
#endif
}
