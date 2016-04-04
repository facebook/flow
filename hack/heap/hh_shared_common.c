/**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/alloc.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <string.h>
#include <unistd.h>
#endif

#include "hh_shared_common.h"

#ifdef _WIN32
static int win32_getpagesize(void)
{
  SYSTEM_INFO siSysInfo;
  GetSystemInfo(&siSysInfo);
  return siSysInfo.dwPageSize;
}
#define getpagesize win32_getpagesize
#endif

/*****************************************************************************/
/* Allocates in the shared heap.
 * The chunks are cache aligned.
 * The word before the chunk address contains the size of the chunk in bytes.
 * The function returns a pointer to the data (the size can be accessed by
 * looking at the address: chunk - sizeof(size_t)).
 */
/*****************************************************************************/

static char* hh_alloc(char** heap, size_t size) {
  size_t slot_size  = ALIGNED(size + sizeof(size_t));
  char* chunk       = __sync_fetch_and_add(heap, (char*)slot_size);
#ifdef _WIN32
  if (!VirtualAlloc(chunk, slot_size, MEM_COMMIT, PAGE_READWRITE)) {
    win32_maperr(GetLastError());
    uerror("VirtualAlloc1", Nothing);
  }
#endif
  // XXX: should add a bounds check
  *((size_t*)chunk) = size;
  return (chunk + sizeof(size_t));
}

/*****************************************************************************/
/* Allocates an ocaml value in the shared heap.
 * The values can only be ocaml strings. It returns the address of the
 * allocated chunk.
 */
/*****************************************************************************/
char* hh_store_ocaml(char** heap, value data) {
  size_t data_size = caml_string_length(data);
  char* addr = hh_alloc(heap, data_size);
  memcpy(addr, String_val(data), data_size);
  return addr;
}
