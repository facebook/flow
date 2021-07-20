/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/fail.h>
#include <caml/memory.h>

#ifndef _WIN32
#include <sys/time.h>
#endif

#ifdef __linux__
#include <linux/magic.h>
#include <sys/vfs.h>
#endif

void hh_lutimes(value filename_v) {
  CAMLparam1(filename_v);
#ifdef _WIN32
  /* Not implemented */
  CAMLreturn0;
#else
  const char* filename = String_val(filename_v);
  int success = lutimes(filename, NULL);
  if (success != 0) {
    caml_failwith("lutimes failed");
  }
#endif
  CAMLreturn0;
}

value hh_is_nfs(value filename_v) {
  CAMLparam1(filename_v);
#ifdef __linux__
  struct statfs buf;
  const char* filename = String_val(filename_v);
  int success = statfs(filename, &buf);
  if (success != 0) {
    caml_failwith("statfs failed");
  }
  switch (buf.f_type) {
#ifdef CIFS_MAGIC_NUMBER
    case CIFS_MAGIC_NUMBER:
#endif
    case NFS_SUPER_MAGIC:
    case SMB_SUPER_MAGIC:
      CAMLreturn(Val_bool(1));
    default:
      CAMLreturn(Val_bool(0));
  }
#endif
  CAMLreturn(Val_bool(0));
}
