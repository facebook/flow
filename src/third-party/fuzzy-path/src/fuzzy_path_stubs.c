/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  Store_field(v, 1, Val_long(r->score));
  // we don't currently need r->weight
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

value fuzzy_add_candidate(value matcher, value candidate, value weight) {
  CAMLparam3(matcher, candidate, weight);
  matcher_add_candidate(Matcher_val(matcher), String_val(candidate), Int_val(weight));
  CAMLreturn (Val_unit);
}

value fuzzy_add_candidates(value matcher, value candidates) {
  CAMLparam2(matcher, candidates);
  CAMLlocal2(head, pair);
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
    pair = Field(head, 0);
    head = Field(head, 1);
    matcher_add_candidate(m, String_val(Field(pair, 0)), Int_val(Field(pair, 1)));
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
    (bool)Bool_val(Field(options, 0)), // first_match_can_be_weak
    Int_val(Field(options, 1)), // num_threads
    Int_val(Field(options, 2)), // max_results
    (bool)Bool_val(Field(options, 3)), // weighted
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

value fuzzy_score(value haystack_v, value needle_v, value boost_full_match_v, value first_match_can_be_weak_v) {
  CAMLparam4(haystack_v, needle_v, boost_full_match_v, first_match_can_be_weak_v);
  CAMLlocal1(result);

  const char *haystack = String_val(haystack_v);
  const char *needle = String_val(needle_v);
  bool boost_full_match = Bool_val(boost_full_match_v);
  bool first_match_can_be_weak = Bool_val(first_match_can_be_weak);

  long score = 0;
  bool has_score = fuzzy_score_c(haystack, needle, boost_full_match, first_match_can_be_weak, &score);

  if (has_score) {
    result = caml_alloc_some(Val_long(score));
  } else {
    result = Val_none;
  }

  CAMLreturn(result);
}
