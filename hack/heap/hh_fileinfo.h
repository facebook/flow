#include <stdint.h>

#pragma once

#ifdef __cplusplus
extern "C" {
#endif

void hhfi_insert_row(
  sqlite3 *db,
  int64_t hash,
  const char *name,
  int64_t nkind,
  const char *filespec
);

// returns either NULL or a heap-allocatd pointer to string
// freeing it is your responsibility
char *hhfi_get_filespec(
  sqlite3 *db,
  int64_t hash
);

char *hhfi_get_filespec_debug(
  sqlite3 *db,
  int64_t hash,
  const char *name,
  int64_t n_kind
);

#ifdef __cplusplus
}
#endif
