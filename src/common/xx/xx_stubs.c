#define XXH_STATIC_LINKING_ONLY
#include <xxhash.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>
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
  value v = alloc_custom(&xx_state_ops, sizeof(XXH64_state_t), 0, 1);
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

CAMLexport value caml_xx_digest(value state) {
  CAMLparam1(state);
  CAMLlocal1(v);
  XXH64_hash_t hash = XXH64_digest(&State_val(state));
  v = caml_alloc_string(sizeof(XXH64_hash_t));
  memcpy(String_val(v), &hash, sizeof(XXH64_hash_t));
  CAMLreturn(v);
}

/*
 * XXH64_hash_t is an unsigned 64 bit integer. This is too big for an OCaml
 * int, so we just copy it into a string and pass that around abstractly. But to
 * actually print as a readable string, we need to convert it back to an int
 * and sprintf it into a new string
 */
CAMLexport value caml_xx_to_string(value hash) {
  CAMLparam1(hash);
  CAMLlocal1(str);
  // Max unsigned long long is 7FFFFFFFFFFFFFFF which is 16 characters. It
  // doesn't seem like you need to ask for extra space for the null terminator
  str = caml_alloc_string(16);
  XXH64_hash_t hash_as_int;
  memcpy(&hash_as_int, String_val(hash), sizeof(XXH64_hash_t));
  // 17 is 16 hex characters plus a null terminator
  snprintf(String_val(str), 17, "%016llx", hash_as_int);
  CAMLreturn(str);
}
