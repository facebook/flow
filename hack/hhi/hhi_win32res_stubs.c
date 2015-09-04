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
#include <caml/unixsupport.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <string.h>
#endif

#define UNUSED(x) (void)(x)

value caml_hh_win32res_blit_to_string(value val_buf1, value val_ofs1,
                                      value val_buf2, value val_ofs2,
                                      value val_len)
{
  memcpy(String_val(val_buf2) + Long_val(val_ofs2),
         (char*)Caml_ba_data_val(val_buf1) + Long_val(val_ofs1),
         Long_val(val_len));
  return Val_unit;
}

#ifdef _WIN32

value caml_hh_win32res_load_resource(value name, value type)
{
  CAMLparam2(name, type);
  CAMLlocal2(ba, ba_opt);
  intnat dim[1];
  HGLOBAL rcData;
  HRSRC rc;
  rc = FindResource(
         NULL,
         MAKEINTRESOURCE(Long_val(name)),
         MAKEINTRESOURCE(Long_val(type)));
  if (!rc) {
    win32_maperr(GetLastError());
    CAMLreturn(Val_long(0)); // None
  }
  rcData = LoadResource(NULL, rc);
  if (!rcData) {
    win32_maperr(GetLastError());
    uerror("LoadResource", Nothing);
  }
  dim[0] = SizeofResource(NULL, rc);
  ba = caml_ba_alloc(
         CAML_BA_UINT8 /* CAML_BA_CHAR */ | CAML_BA_C_LAYOUT,
         1,
         LockResource(rcData),
         dim);
  ba_opt = caml_alloc_small(1, 0);
  Field(ba_opt, 0) = ba;
  CAMLreturn(ba_opt);
}

#else

value caml_hh_win32res_load_resource(value name, value type)
{
  UNUSED(name);
  UNUSED(type);
  return Val_long(0); // None
}

#endif
