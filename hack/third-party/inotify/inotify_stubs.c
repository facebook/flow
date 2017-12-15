/*
 * Copyright (C) 2006-2008 Vincent Hanquez <vincent@snarc.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * Inotify Ocaml binding - C glue
 */

#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>

#include <sys/inotify.h>

static int inotify_flag_table[] = {
  IN_ACCESS, IN_ATTRIB, IN_CLOSE_WRITE, IN_CLOSE_NOWRITE,
  IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MODIFY,
  IN_MOVE_SELF, IN_MOVED_FROM, IN_MOVED_TO, IN_OPEN,
  IN_DONT_FOLLOW, IN_MASK_ADD, IN_ONESHOT, IN_ONLYDIR,
  IN_MOVE, IN_CLOSE, IN_ALL_EVENTS, 0
};

static int inotify_return_table[] = {
  IN_ACCESS, IN_ATTRIB, IN_CLOSE_WRITE, IN_CLOSE_NOWRITE,
  IN_CREATE, IN_DELETE, IN_DELETE_SELF, IN_MODIFY,
  IN_MOVE_SELF, IN_MOVED_FROM, IN_MOVED_TO, IN_OPEN,
  IN_IGNORED, IN_ISDIR, IN_Q_OVERFLOW, IN_UNMOUNT, 0
};

value caml_inotify_init(value unit) {
  CAMLparam1(unit);

  int fd = inotify_init();
  if (fd == -1) uerror("inotify_init", Nothing);

  CAMLreturn(Val_int(fd));
}

value caml_inotify_add_watch(value fd, value path, value selector_flags) {
  CAMLparam3(fd, path, selector_flags);

  int selector = caml_convert_flag_list(selector_flags, inotify_flag_table);

  int watch = inotify_add_watch(Int_val(fd), String_val(path), selector);
  if (watch == -1) uerror("inotify_add_watch", path);

  CAMLreturn(Val_int(watch));
}

value caml_inotify_rm_watch(value fd, value watch) {
  CAMLparam2(fd, watch);

  int ret = inotify_rm_watch(Int_val(fd), Int_val(watch));
  if (ret == -1) uerror("inotify_rm_watch", Nothing);

  CAMLreturn(Val_unit);
}

value caml_inotify_struct_size(void) {
  CAMLparam0();
  CAMLreturn(Val_int(sizeof(struct inotify_event)));
}

value caml_inotify_name_max(void) {
  CAMLparam0();
  CAMLreturn(Val_int(NAME_MAX));
}

value caml_inotify_convert(value buf) {
  CAMLparam1(buf);
  CAMLlocal3(event, list, next);

  list = next = Val_emptylist;

  struct inotify_event ievent;
  memcpy(&ievent, String_val(buf), sizeof(struct inotify_event));

  int flag;
  for (flag = 0; inotify_return_table[flag]; flag++) {
    if (!(ievent.mask & inotify_return_table[flag]))
      continue;

    next = caml_alloc_small(2, Tag_cons);
    Field(next, 0) = Val_int(flag);
    Field(next, 1) = list;
    list = next;
  }

  event = caml_alloc_tuple(4);
  Store_field(event, 0, Val_int(ievent.wd));
  Store_field(event, 1, list);
  Store_field(event, 2, caml_copy_int32(ievent.cookie));
  Store_field(event, 3, Val_int(ievent.len));

  CAMLreturn(event);
}
