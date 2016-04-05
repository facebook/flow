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
#include <caml/unixsupport.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include "hh_shared_common.h"

#ifndef _WIN32
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/errno.h>
#include <sys/mman.h>
#endif

/*
 * bytes_val: max size of the list in bytes
 */
CAMLprim value hh_shared_list_create(value size_val) {
  CAMLparam1(size_val);
  CAMLlocal1(list_val);
  long size = Long_val(size_val);

  int flags = MAP_SHARED | MAP_ANON | MAP_NORESERVE;
  int prot  = PROT_READ  | PROT_WRITE;

  char* mem = (char*)mmap(NULL, size, prot, flags, 0, 0);
  if (mem == MAP_FAILED) {
    printf("Error initializing: %s\n", strerror(errno));
    exit(2);
  }
  char* list_start = mem + getpagesize();
  ((uintptr_t*)mem)[0] = (uintptr_t)list_start;
  list_val = caml_alloc(1, Abstract_tag);
  Field(list_val, 0) = (uintptr_t)mem;
  CAMLreturn(list_val);
}

CAMLprim value hh_shared_list_append(value list_val, value v) {
  CAMLparam2(list_val, v);
  char** mem = (char**)Field(list_val, 0);
  hh_store_ocaml(mem, v);
  CAMLreturn(Val_unit);
}

CAMLprim value hh_shared_list_reset(value list_val) {
  CAMLparam1(list_val);
  char* mem = (char*)Field(list_val, 0);
  mem[0] = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value hh_shared_list_get(value list_val) {
  CAMLparam1(list_val);
  CAMLlocal3(xs, str, cons);
  xs = Val_emptylist;
  char* mem = (char*)Field(list_val, 0);
  char* list_ptr = mem + getpagesize();
  char* list_end = ((char**)mem)[0];
  while (list_ptr < list_end) {
    size_t size = *(size_t*)(list_ptr);
    str = caml_alloc_string(size);
    memcpy(String_val(str), list_ptr + sizeof(size_t), size);
    size_t slot_size = ALIGNED(size + sizeof(size_t));
    list_ptr += slot_size;
    cons = caml_alloc(2, 0);
    Store_field(cons, 0, str);
    Store_field(cons, 1, xs);
    xs = cons;
  }
  CAMLreturn(xs);
}
