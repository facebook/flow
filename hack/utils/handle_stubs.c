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

#ifdef __WIN32__

value caml_hh_worker_get_handle(value x) {
    HANDLE h = Handle_val(x);
    return Val_long(h);
}

value caml_hh_worker_create_handle(value x) {
    HANDLE h1 = (HANDLE)Long_val(x);
    return win_alloc_handle(h1);
}

#else

value caml_hh_worker_get_handle(value x) {
    return x;
}

value caml_hh_worker_create_handle(value x) {
    return x;
}

#endif
