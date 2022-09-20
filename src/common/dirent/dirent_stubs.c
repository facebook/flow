/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>

/**
 * Maps C d_type to the ocaml d_type constructor
 */
static int ctor_of_d_type(int d_type) {
  switch (d_type) {
    case 0:
      return 0; // DT_UNKNOWN
    case 1:
      return 1; // DT_FIFO
    case 2:
      return 2; // DT_CHR
    case 4:
      return 3; // DT_DIR
    case 6:
      return 4; // DT_BLK
    case 8:
      return 5; // DT_REG
    case 10:
      return 6; // DT_LNK
    case 12:
      return 7; // DT_SOCK
    default:
      return 0; // DT_UNKNOWN
  }
}

static value flow_alloc_dirent(char* d_name, int d_type) {
  CAMLparam0();
  CAMLlocal1(result);
  result = caml_alloc(2, 0);
  Store_field(result, 0, caml_copy_string(d_name));
  Store_field(result, 1, Val_int(ctor_of_d_type(d_type)));
  CAMLreturn(result);
}

static int d_type_of_dirent(struct dirent* e) {
#ifdef _DIRENT_HAVE_D_TYPE
  return e->d_type;
#else
  return 0;
#endif
}

CAMLprim value flow_dirent_readdir(value vd) {
  CAMLparam1(vd);
#ifdef _WIN32
  caml_raise_sys_error(
      caml_copy_string("flow_dirent_readdir: not implemented on Windows"));
#else
  CAMLlocal1(result);
  DIR* d;
  struct dirent* e;
  d = DIR_Val(vd);
  if (d == NULL) {
    unix_error(EBADF, "readdir", Nothing);
  }
  caml_enter_blocking_section();
  errno = 0;
  e = readdir(d);
  caml_leave_blocking_section();
  if (e == NULL) {
    if (errno) {
      uerror("readdir", Nothing);
    }
    caml_raise_end_of_file();
  }
  result = flow_alloc_dirent(e->d_name, d_type_of_dirent(e));
  CAMLreturn(result);
#endif // _WIN32
}

struct flow_dirent {
  int d_type;
  char d_name[];
};

/**
 * Adds all of the dirents in p to tbl. Runs without the ocaml runtime
 * lock, so don't touch the OCaml heap or call back into OCaml.
 * Returns 0 on success. Returns -1 and sets errno if opendir errors,
 * -2 and sets errno if readdir errors.
 */
static int flow_dirent_read_directory(const char* p, struct ext_table* tbl) {
  DIR* d;
  struct dirent* e;
  struct flow_dirent* tbl_item;
  d = opendir(p);
  if (d == NULL) {
    return -1;
  }
  while (1) {
    errno = 0;
    e = readdir(d);
    if (e == NULL) {
      if (errno) {
        return -2;
      }
      break;
    }
    if (strcmp(e->d_name, ".") == 0 || strcmp(e->d_name, "..") == 0) {
      continue;
    }
    size_t name_len;
#ifdef _D_ALLOC_NAMLEN
    name_len = _D_ALLOC_NAMLEN(e);
#else
    name_len = 1 + strlen(e->d_name);
#endif
    tbl_item = (struct flow_dirent*)caml_stat_alloc(
        sizeof(struct flow_dirent) + name_len);
    tbl_item->d_type = d_type_of_dirent(e);
    strcpy(tbl_item->d_name, e->d_name);
    caml_ext_table_add(tbl, tbl_item);
  }
  closedir(d);
  return 0;
}

/**
 * Given a directory path, returns a Dirent.t array for
 * all of the entries in that directory, except "." and "..".
 * This is similar to calling Dirent.readdir in a loop.
 *
 * Raises Unix_error(ENOENT, "opendir", _) if the path is invalid,
 * plus any of the errors from opendir(3).
 */
CAMLprim value flow_dirent_entries(value path) {
  CAMLparam1(path);
  CAMLlocal2(result, item);
  struct ext_table tbl;
  struct flow_dirent* e;
  char* p;
  int ret;

  caml_unix_check_path(path, "opendir");

  p = caml_stat_strdup(String_val(path));
  caml_ext_table_init(&tbl, 50);

  caml_enter_blocking_section();
  ret = flow_dirent_read_directory(p, &tbl);
  caml_leave_blocking_section();

  caml_stat_free(p);
  if (ret < 0) {
    caml_ext_table_free(&tbl, 1);
    uerror(ret == -1 ? "opendir" : "readdir", Nothing);
  }

  result = caml_alloc(tbl.size, 0);
  mlsize_t i;
  for (i = 0; i < tbl.size; i++) {
    e = (struct flow_dirent*)tbl.contents[i];
    item = flow_alloc_dirent(e->d_name, e->d_type);
    Store_field(result, i, item);
  }

  caml_ext_table_free(&tbl, 1);

  CAMLreturn(result);
}
