/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/intext.h>
#include <caml/memory.h>

#define XXH_STATIC_LINKING_ONLY
#include <xxhash.h>

#include <string.h>

/* Blit string into bigarray buffer at given position */
CAMLexport value type_sig_bin_write_string(
    value ba,
    value pos_val,
    value len_val,
    value str_val) {
  CAMLparam4(ba, pos_val, len_val, str_val);
  intnat pos = Long_val(pos_val);
  intnat len = Long_val(len_val);
  char* buf = Caml_ba_data_val(ba);
  const char* str = String_val(str_val);
  memcpy(buf + pos, str, len);
  CAMLreturn(Val_unit);
}

/* Read string from bigarray at given position with given length */
CAMLexport value
type_sig_bin_read_string(value ba, value pos_val, value len_val) {
  CAMLparam3(ba, pos_val, len_val);
  CAMLlocal1(str_val);
  intnat pos = Long_val(pos_val);
  intnat len = Long_val(len_val);
  char* buf = Caml_ba_data_val(ba);
  str_val = caml_alloc_initialized_string(len, buf + pos);
  CAMLreturn(str_val);
}

/* Read serialized value from bigarray at given position */
CAMLexport value type_sig_bin_read_serialized(value ba, value pos_val) {
  CAMLparam2(ba, pos_val);
  CAMLlocal1(v);
  intnat pos = Long_val(pos_val);
  intnat len = Caml_ba_array_val(ba)->dim[0];
  char* buf = Caml_ba_data_val(ba);
  v = caml_input_value_from_block(buf + pos, len - pos);
  CAMLreturn(v);
}

/* OCaml serialized values have a header. The header starts with a big-endian
 * 4-byte magic value, which distinguishes between "small" and "big" encodings.
 *
 * Danger! This is technically an internal detail of the encoding and subject to
 * change, although it seems pretty unlikely. */
#define Intext_magic_number_small 0x8495A6BE
#define Intext_magic_number_big 0x8495A6BF

/* Read big-endian uint32. GCC/clang optimize this to a load+bswap. */
static uint32_t read32u(unsigned char* buf) {
  uint32_t x = 0;
  for (int i = 0; i < 4; i++)
    x = (x << 8) + *buf++;
  return x;
}

/* Read big-endian uint64. GCC/clang optimize this to a load+bswap. */
static uint64_t read64u(unsigned char* buf) {
  uint64_t x = 0;
  for (int i = 0; i < 8; i++)
    x = (x << 8) + *buf++;
  return x;
}

/* Calculate hash of serialized OCaml values by hashing the serialized bytes
 * directly. We read the header to find the byte size and offset of the data. */
CAMLexport value type_sig_bin_hash_serialized(value ba, value pos_val) {
  CAMLparam2(ba, pos_val);
  CAMLlocal1(hash);
  intnat pos = Long_val(pos_val);
  unsigned char* buf = Caml_ba_data_val(ba);
  uint32_t magic = read32u(buf + pos);
  int header_len;
  uint64_t data_len;
  switch (magic) {
    case Intext_magic_number_small:
      header_len = 20;
      data_len = read32u(buf + pos + 4);
      break;
    case Intext_magic_number_big:
      header_len = 32;
      data_len = read64u(buf + pos + 8);
      break;
    default:
      caml_failwith("type_sig_bin_hash_serialized: bad object");
  }
  hash = caml_copy_int64(XXH64(buf + pos + header_len, data_len, 0));
  CAMLreturn(hash);
}
