/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

/* Like `Sys.readdir`, but returns name * type list instead of name array */
CAMLprim value hh_readdir(value path);
