/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#define XXH_STATIC_LINKING_ONLY
#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <xxhash.h>
#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define State_val(v) (*((XXH64_state_t*)Data_custom_val(v)))

static struct custom_operations xx_state_ops = {"org.flow.xx_state",
                                                custom_finalize_default,
                                                custom_compare_default,
                                                custom_hash_default,
                                                custom_serialize_default,
                                                custom_deserialize_default,
                                                custom_compare_ext_default};

static value alloc_xx_state(XXH64_state_t state) {
  value v = caml_alloc_custom(&xx_state_ops, sizeof(XXH64_state_t), 0, 1);
  State_val(v) = state;
  return v;
}

CAMLexport value caml_xx_init_unboxed(unsigned long long seed) {
  CAMLparam0();
  XXH64_state_t state;
  XXH64_reset(&state, seed);
  CAMLreturn(alloc_xx_state(state));
}

CAMLexport value caml_xx_init(value seed) {
  CAMLparam1(seed);
  CAMLreturn(caml_xx_init_unboxed(Int64_val(seed)));
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

CAMLexport XXH64_hash_t caml_xx_hash_unboxed(value v, unsigned long long seed) {
  return XXH64(String_val(v), caml_string_length(v), seed);
}

CAMLexport value caml_xx_hash(value v, value seed) {
  return caml_copy_int64(caml_xx_hash_unboxed(v, Int64_val(seed)));
}
