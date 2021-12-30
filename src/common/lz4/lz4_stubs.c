/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <lz4.h>
#define CAML_NAME_SPACE
#include <caml/bigarray.h>
#include <caml/mlvalues.h>

CAMLprim value caml_lz4_compress_default(value src, value dst) {
  int src_size = caml_string_length(src);
  int dst_capacity = caml_string_length(dst);
  int compressed_size = LZ4_compress_default(
      String_val(src), (char*)Bytes_val(dst), src_size, dst_capacity);
  return Val_long(compressed_size);
}

CAMLprim value caml_lz4_decompress_safe(value src, value dst) {
  struct caml_ba_array* ba = Caml_ba_array_val(src);
  int src_size = ba->dim[0];
  int dst_capacity = caml_string_length(dst);
  int decompressed_size = LZ4_decompress_safe(
      ba->data, (char*)Bytes_val(dst), src_size, dst_capacity);
  return Val_long(decompressed_size);
}
