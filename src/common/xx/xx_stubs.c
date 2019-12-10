/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define XXH_STATIC_LINKING_ONLY
#include <xxhash.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#define State_val(v) (*((XXH64_state_t *) Data_custom_val(v)))

static struct custom_operations xx_state_ops = {
  "org.flow.xx_state",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

static value alloc_xx_state(XXH64_state_t state) {
  value v = caml_alloc_custom(&xx_state_ops, sizeof(XXH64_state_t), 0, 1);
  State_val(v) = state;
  return v;
}

CAMLexport value caml_xx_init(value unit) {
  CAMLparam1(unit);
  XXH64_state_t state;
  XXH64_reset(&state, 0);
  CAMLreturn(alloc_xx_state(state));
}

CAMLexport value caml_xx_update(value state, value v) {
  assert(Tag_val(v) == String_tag);
  XXH64_update(&State_val(state), String_val(v), caml_string_length(v));
  return Val_unit;
}

CAMLexport value caml_xx_update_int(value state, value v) {
  assert(Is_long(v));
  XXH64_update(&State_val(state), &v, sizeof(value));
  return Val_unit;
}

CAMLexport value caml_xx_update_int64_unboxed(value state, int64_t v) {
  XXH64_update(&State_val(state), &v, sizeof(int64_t));
  return Val_unit;
}

CAMLexport value caml_xx_update_int64(value state, value v) {
  return caml_xx_update_int64_unboxed(state, Int64_val(v));
}

CAMLexport XXH64_hash_t caml_xx_digest_unboxed(value state) {
  return XXH64_digest(&State_val(state));
}

CAMLexport value caml_xx_digest(value state) {
  return caml_copy_int64(caml_xx_digest_unboxed(state));
}

CAMLexport value caml_xx_to_string_unboxed(XXH64_hash_t hash) {
  CAMLparam0();
  CAMLlocal1(str);
  /* Max unsigned long long is 7FFFFFFFFFFFFFFF which is 16 bytes.
   *
   * Note that OCaml strings are not null-terminated, but rather use a somewhat
   * clever encoding that combines the wosize from the header and the last byte
   * of the data segment. When we allocate a string to hold 16 bytes (2 words),
   * OCaml will actually allocate 24 bytes (3 words), where the final byte
   * stores an offset which is 7 in our case.
   *
   * When calculating the length of a string, OCaml combines the byte size of
   * the value (23) and the offset (7) to arrive at the actual size 16 = 23 - 7.
   *
   * The caml_alloc_string API hides this detail for us, so we don't need to
   * worry about it at all, except to know that it's safe to write into the
   * first 16 bytes. We could even write a null character into those bytes if we
   * wanted.
   */
  str = caml_alloc_string(16);
  snprintf(String_val(str), 16, "%016llx", (unsigned long long)hash);
  CAMLreturn(str);
}

CAMLexport value caml_xx_to_string(value hash) {
  CAMLparam1(hash);
  CAMLlocal1(str);
  str = caml_xx_to_string_unboxed(Int64_val(hash));
  CAMLreturn(str);
}
