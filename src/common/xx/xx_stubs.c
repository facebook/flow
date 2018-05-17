#define XXH_STATIC_LINKING_ONLY
#include <xxhash.h>
#include <assert.h>
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
  XXH64_hash_t hash = XXH64_digest(&State_val(state));
  value v = caml_alloc_string(sizeof(XXH64_hash_t));
  memcpy(String_val(v), &hash, sizeof(XXH64_hash_t));
  CAMLreturn(v);
}
