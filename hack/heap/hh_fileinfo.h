/**
 * Copyright (c) 2015-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 */

#include <stdint.h>

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void hhfi_insert_row(
  /*sqlite3*/ void *db,
  int64_t hash,
  const char *name,
  int64_t nkind,
  const char *filespec
);

// returns either NULL or a heap-allocatd pointer to string
// freeing it is your responsibility
char *hhfi_get_filespec(
  /*sqlite3*/ void *db,
  int64_t hash
);

#ifdef __cplusplus
}
#endif
