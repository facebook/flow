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

#ifdef _WIN32
#define Val_handle(fd) (win_alloc_handle(fd))
#else
#define Handle_val(fd) (Long_val(fd))
#define Val_handle(fd) (Val_long(fd))
#endif
