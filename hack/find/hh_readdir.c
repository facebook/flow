/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */

#include "hh_readdir.h"

#include <dirent.h>
#include <errno.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/unixsupport.h>

CAMLprim value hh_readdir(value path) {
  CAMLparam1(path);
  CAMLlocal3(head, tail, list);

  if (Tag_val(path) != String_tag) {
    caml_invalid_argument(
      "Path must be a string"
    );
  }

  DIR* dir = opendir(String_val(path));
  if (!dir) {
    /* from caml/unixsupport.h */
    unix_error(errno, "opendir", path);
  }

  list = Val_emptylist;
  struct dirent* ent;
  while (1) {
    errno = 0;
    ent = readdir(dir);
    if (!ent) {
      if (errno) {
        unix_error(errno, "readdir", path);
      }
      break;
    }
    head = caml_alloc_tuple(2);
#ifdef __APPLE__
    Store_field(head, 0, caml_alloc_initialized_string(ent->d_namlen, ent->d_name));
#else
    Store_field(head, 0, caml_copy_string(ent->d_name));
#endif
    Store_field(head, 1, Val_int(ent->d_type));

    tail = list;
    list = caml_alloc(2, 0);
    Store_field(list, 0, head);
    Store_field(list, 1, tail);
  }
  closedir(dir);
  CAMLreturn(list);
}
