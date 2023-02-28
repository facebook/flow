/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/memory.h>
#ifdef _WIN32
#include <windows.h>
#endif

CAMLprim value flow_win32_get_last_error(value unit) {
  CAMLparam1(unit);
#ifdef _WIN32
  CAMLreturn(Val_long(GetLastError()));
#else
  CAMLreturn(Val_long(0));
#endif
}
