/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifndef __FUZZY_PATH_WRAPPER_H__
#define __FUZZY_PATH_WRAPPER_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdlib.h>
#include <stdbool.h>

struct matcher;
typedef struct matcher matcher_t;

typedef struct matcher_options {
  bool case_sensitive;
  bool smart_case;
  size_t num_threads;
  size_t max_results;
  size_t max_gap;
  bool record_match_indexes;
  char *root_path;
} matcher_options_t;

typedef struct match_result {
  float score;
  // We can't afford to copy strings around while we're ranking them.
  // These are not guaranteed to last very long and should be copied out ASAP.
  const char *value;
  // Only computed if `record_match_indexes` was set to true.
  int *matchIndexes;
  int score_based_root_path;
} match_result_t;

typedef struct match_results {
  size_t size;
  match_result_t *results;
} match_results_t;

matcher_t *matcher_create();
match_results_t matcher_find(matcher_t *m, char *query, matcher_options_t options);
void matcher_add_candidate(matcher_t *m, char *candidate);
void matcher_remove_candidate(matcher_t *m, char *candidate);
void matcher_clear(matcher_t *m);
void matcher_reserve(matcher_t *m, size_t n);
size_t matcher_size(matcher_t *m);

#ifdef __cplusplus
}
#endif

#endif /* __FUZZY_PATH_WRAPPER_H__ */
