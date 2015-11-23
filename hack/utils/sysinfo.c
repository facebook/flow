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
#ifndef __APPLE__
#include <sys/sysinfo.h>
#endif
#endif

value hh_sysinfo_totalram(void) {
  CAMLparam0();
#ifdef __linux__
  struct sysinfo info;
  int success = sysinfo(&info);
  assert(success == 0 && "sysinfo() failed");
  CAMLreturn(Val_long(info.totalram));
#else
  /* Not implemented */
  CAMLreturn(Val_long(0));
#endif
}

value hh_sysinfo_uptime(void) {
  CAMLparam0();
#ifdef __linux__
  struct sysinfo info;
  int success = sysinfo(&info);
  assert(success == 0 && "sysinfo() failed");
  CAMLreturn(Val_long(info.uptime));
#else
  /* Not implemented */
  CAMLreturn(Val_long(0));
#endif
}
