/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#include <caml/mlvalues.h>

#ifdef _WIN32
#include <Windows.h>
#endif

// Can be removed once support for ocaml-4.01 is dropped
value win_terminate_process(value v_pid)
{
#ifdef _WIN32
  return (Val_bool(TerminateProcess((HANDLE) Long_val(v_pid), 0)));
#else
  return Val_int(0); // False
#endif
}
