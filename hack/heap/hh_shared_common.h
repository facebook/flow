/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#pragma once

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

/* Too lazy to use getconf */
#define CACHE_LINE_SIZE (1 << 6)
#define CACHE_MASK      (~(CACHE_LINE_SIZE - 1))
#define ALIGNED(x)      ((x + CACHE_LINE_SIZE - 1) & CACHE_MASK)

char* hh_store_ocaml(char** heap, value data);
