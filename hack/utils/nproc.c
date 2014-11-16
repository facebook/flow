/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */
#include <caml/memory.h>
#include <unistd.h>

value nproc(void) {
  CAMLparam0();
  CAMLlocal1(result);

  result = Val_long(sysconf(_SC_NPROCESSORS_ONLN));
  CAMLreturn(result);
}
