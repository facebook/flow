#include <xxhash.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/intext.h>

#define State_val(v) (*((XXH64_state_t **) Data_custom_val(v)))

static void caml_finalize_xx_state(value v) {
  XXH64_freeState(State_val(v));
}

static struct custom_operations xx_state_ops = {
  "org.flow.xx_state",
  caml_finalize_xx_state,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

static value alloc_xx_state(XXH64_state_t *state) {
  value v = alloc_custom(&xx_state_ops, sizeof(XXH64_state_t *), 0, 1);
  State_val(v) = state;
  return v;
}

CAMLexport value caml_xx_init(value unit) {
  CAMLparam1(unit);
  XXH64_state_t *state = XXH64_createState();
  XXH64_reset(state, 0);
  CAMLreturn(alloc_xx_state(state));
}

CAMLexport value caml_xx_update(value state, value v) {
  XXH64_update(State_val(state), String_val(v), caml_string_length(v));
  return Val_unit;
}

CAMLexport value caml_xx_update_int(value state, value v) {
  // TODO: we know this is an int, so just hash its bytes
  static char data[40]; // 32 for the header
  intnat len = caml_output_value_to_block(v, Val_int(0), data, 40);
  XXH64_update(State_val(state), data, (size_t)len);
  return Val_unit;
}

CAMLexport value caml_xx_digest(value state) {
  CAMLparam1(state);
  XXH64_hash_t hash = XXH64_digest(State_val(state));
  value v = caml_alloc_string(sizeof(XXH64_canonical_t));
  XXH64_canonicalFromHash((XXH64_canonical_t *)String_val(v), hash);
  CAMLreturn(v);
}
