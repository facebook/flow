/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <caml/memory.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

value nproc(void) {
  CAMLparam0();
  CAMLlocal1(result);
#ifdef _WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  result = Val_long(sysinfo.dwNumberOfProcessors);
#else
  result = Val_long(sysconf(_SC_NPROCESSORS_ONLN));
#endif
  CAMLreturn(result);
}
