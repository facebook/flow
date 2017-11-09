/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <limits.h>
#include <stdlib.h>

#define Val_none Val_int(0)

static value
Val_some( value v )
{
  CAMLparam1( v );
  CAMLlocal1( some );
  some = caml_alloc(1, 0);
  Store_field( some, 0, v );
  CAMLreturn( some );
}

CAMLprim value
hh_realpath(value v) {
  char *input;
#ifndef _WIN32
  char output[PATH_MAX];
#else
  char output[_MAX_PATH];
#endif
  char *result;

  CAMLparam1(v);

  input = String_val(v);
#ifndef _WIN32
  result = realpath(input, output);
#else
  result = _fullpath(output, input, _MAX_PATH);
#endif
  if (result == NULL) {
    CAMLreturn(Val_none);
  } else {
    CAMLreturn(Val_some(caml_copy_string(output)));
  }
}
