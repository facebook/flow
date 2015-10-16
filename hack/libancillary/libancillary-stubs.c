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
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#undef CAML_NAME_SPACE

#include "hphp/hack/src/third-party/libancillary/ancillary.h"

CAMLprim value stub_ancil_send_fd(value int_val_socket, value int_val_fd) {
  CAMLparam2(int_val_socket, int_val_fd);
  int socket = Int_val(int_val_socket);
  int fd = Int_val(int_val_fd);
  CAMLreturn(Val_int(ancil_send_fd(socket, fd)));
}

/** Returns -1 on failure, or non-negative file descriptor on success. */
CAMLprim value stub_ancil_recv_fd(value int_val_socket) {
  CAMLparam1(int_val_socket);
  int fd = 0;
  int result;
  int socket = Int_val(int_val_socket);
  result = ancil_recv_fd(socket, &fd);
  if (result >= 0) {
    CAMLreturn(Val_int(fd));
  } else {
    CAMLreturn(Val_int(result));
  }
}
