/*
 *     Copyright (C) 2006-2008 Vincent Hanquez <vincent@snarc.org>
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
#include <sys/ioctl.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/callback.h>

#include <features.h>

#if __GLIBC__ >= 2 && __GLIBC_MINOR__ >= 4
#define GLIBC_SUPPORT_INOTIFY 1
#else
#define GLIBC_SUPPORT_INOTIFY 0
#endif

#if GLIBC_SUPPORT_INOTIFY
#include <sys/inotify.h>
#else
#include "inotify_compat.h"
#endif

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

static void raise_inotify_error(char const *msg)
{
        static value *inotify_err = NULL;
        value args[2];

        if (!inotify_err)
                inotify_err = caml_named_value("inotify.error");
        args[0] = caml_copy_string(msg);
        args[1] = Val_int(errno);

        caml_raise_with_args(*inotify_err, 2, args);
}

value stub_inotify_init(value unit)
{
        CAMLparam1(unit);
        int fd;

        fd = inotify_init();
        CAMLreturn(Val_int(fd));
}

value stub_inotify_ioctl_fionread(value fd)
{
        CAMLparam1(fd);
        int rc, bytes;

        rc = ioctl(Int_val(fd), FIONREAD, &bytes);
        if (rc == -1)
                raise_inotify_error("ioctl fionread");

        CAMLreturn(Val_int(bytes));
}

value stub_inotify_add_watch(value fd, value path, value mask)
{
        CAMLparam3(fd, path, mask);
        int cv_mask, wd;

        cv_mask = caml_convert_flag_list(mask, inotify_flag_table);
        wd = inotify_add_watch(Int_val(fd), String_val(path), cv_mask);
        if (wd < 0)
                raise_inotify_error("add_watch");
        CAMLreturn(Val_int(wd));
}

value stub_inotify_rm_watch(value fd, value wd)
{
        CAMLparam2(fd, wd);
        int ret;

        ret = inotify_rm_watch(Int_val(fd), Int_val(wd));
        if (ret == -1)
                raise_inotify_error("rm_watch");
        CAMLreturn(Val_unit);
}

value stub_inotify_struct_size(void)
{
        CAMLparam0();
        CAMLreturn(Val_int(sizeof(struct inotify_event)));
}

value stub_inotify_convert(value buf)
{
        CAMLparam1(buf);
        CAMLlocal3(event, l, tmpl);
        struct inotify_event ev;
        int i;

        l = Val_emptylist;
        tmpl = Val_emptylist;

        memcpy(&ev, String_val(buf), sizeof(struct inotify_event));

        for (i = 0; inotify_return_table[i]; i++) {
                if (!(ev.mask & inotify_return_table[i]))
                        continue;
                tmpl = caml_alloc_small(2, Tag_cons);
                Field(tmpl, 0) = Val_int(i);
                Field(tmpl, 1) = l;
                l = tmpl;
        }

        event = caml_alloc_tuple(4);
        Store_field(event, 0, Val_int(ev.wd));
        Store_field(event, 1, l);
        Store_field(event, 2, caml_copy_int32(ev.cookie));
        Store_field(event, 3, Val_int(ev.len));

        CAMLreturn(event);
}
