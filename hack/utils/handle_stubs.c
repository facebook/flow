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
#include <caml/intext.h>
#include <caml/custom.h>
#include <stdio.h>

#include "handle.h"

value caml_hh_worker_get_handle(value x) {
    return Val_long(Handle_val(x));
}

value caml_hh_worker_create_handle(value x) {
#ifdef _WIN32
  return Val_handle((HANDLE)Long_val(x));
#else
  return Val_handle(Long_val(x));
#endif
}

#ifdef _WIN32
static void win_handle_serialize(value h, uintnat *wsize_32, uintnat *wsize_64) {
  serialize_int_8((int64)Handle_val(h));
  serialize_int_1(Descr_kind_val(h));
  serialize_int_1(CRT_fd_val(h));
  serialize_int_1(Flags_fd_val(h));
  *wsize_32 = sizeof(struct filedescr);
  *wsize_64 = sizeof(struct filedescr);
}

static uintnat win_handle_deserialize(void * dst) {
  struct filedescr *h= (struct filedescr *)dst;
  h->fd.handle = (HANDLE)caml_deserialize_sint_8();
  h->kind = caml_deserialize_uint_1();
  h->crt_fd = caml_deserialize_sint_1();
  h->flags_fd = caml_deserialize_uint_1();
  return sizeof(struct filedescr);
}
#endif

value win_setup_handle_serialization(value unit) {
#ifdef _WIN32
  value handle = win_alloc_handle((HANDLE)0); // Dummy handle
  struct custom_operations *win_handle_ops = (struct custom_operations *)Field(handle, 0);
  win_handle_ops->serialize = win_handle_serialize;
  win_handle_ops->deserialize = win_handle_deserialize;
  caml_register_custom_operations(win_handle_ops);
#endif
  return Val_unit;
}

