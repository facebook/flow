/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "fuzzy_path_wrapper.h"

#define CAML_NAME_SPACE
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/custom.h>
#include <caml/fail.h>

static struct custom_operations matcher_ops = {
  "org.flow.fuzzy_path_matcher",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
#ifdef custom_compare_ext_default
  , custom_compare_ext_default
#endif
#ifdef custom_fixed_length_default
  , custom_fixed_length_default
#endif
};

#define Matcher_val(v) (*((matcher_t **) Data_custom_val(v)))

static value alloc_matcher(matcher_t *m) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc_custom(&matcher_ops, sizeof(matcher_t *), 0, 1);
  Matcher_val(v) = m;
  CAMLreturn(v);
}

static value alloc_result(match_result_t *r) {
  CAMLparam0();
  CAMLlocal1(v);
  v = caml_alloc(2, 0);
  Store_field(v, 0, caml_copy_string(r->value));
  Store_field(v, 1, caml_copy_double(r->score));
  CAMLreturn(v);
}

value fuzzy_create(value candidates) {
  CAMLparam1(candidates);
  matcher_t *matcher = matcher_create();
  if (matcher == NULL) {
    caml_raise_out_of_memory();
  }
  CAMLreturn(alloc_matcher(matcher));
}

value fuzzy_add_candidate(value matcher, value candidate) {
  CAMLparam2(matcher, candidate);
  matcher_add_candidate(Matcher_val(matcher), String_val(candidate));
  CAMLreturn (Val_unit);
}

value fuzzy_add_candidates(value matcher, value candidates) {
  CAMLparam2(matcher, candidates);
  CAMLlocal1(head);
  matcher_t *m = Matcher_val(matcher);
  size_t count = 0;
  head = candidates;
  while (Is_block(head)) {
    count++;
    head = Field(head, 1);
  }
  matcher_reserve(m, matcher_size(m) + count);
  head = candidates;
  while (Is_block(head)) {
    matcher_add_candidate(m, String_val(Field(head, 0)));
    head = Field(head, 1);
  }
  CAMLreturn (Val_unit);
}

value fuzzy_remove_candidate(value matcher, value candidate) {
  CAMLparam2(matcher, candidate);
  matcher_remove_candidate(Matcher_val(matcher), String_val(candidate));
  CAMLreturn (Val_unit);
}

value fuzzy_match(value matcher_val, value query_val, value options) {
  CAMLparam3(matcher_val, query_val, options);
  CAMLlocal2 (head, rest);

  matcher_t *matcher = Matcher_val(matcher_val);
  const char *query = String_val(query_val);
  matcher_options_t opts = {
    (bool)Bool_val(Field(options, 0)), // case_sensitive
    (bool)Bool_val(Field(options, 1)), // smart_case
    Int_val(Field(options, 2)), // num_threads
    Int_val(Field(options, 3)), // max_results
    Int_val(Field(options, 4)), // max_gap
    false, // record_match_indexes
    String_val(Field(options, 5)) // root_path
  };

  match_results_t results = matcher_find(matcher, query, opts);
  rest = Val_int(0); // end of list
  for (int i = results.size - 1; i >= 0; i--) {
    match_result_t *result = &results.results[i];
    head = caml_alloc(2, 0); // list item
    Store_field(head, 0, alloc_result(result));
    Store_field(head, 1, rest);
    rest = head;
  }

  CAMLreturn (head);
}
