/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string>

#include "fuzzy_path_wrapper.h"

#include "../vendor/MatcherBase.h"

struct matcher {
  void *obj;
};

matcher_t *matcher_create()
{
  void *p;
  matcher_t *m;
  MatcherBase *obj;

  p = malloc(sizeof(*m));
  if (p == NULL) {
    return NULL;
  }

  obj = new MatcherBase();

  m = (matcher_t *)p;
  m->obj = obj;

  return m;
}

match_results_t matcher_find(matcher_t *m, char *query, matcher_options_t options) {
  MatcherBase *obj;
  if (m == NULL) {
  return {
      0,
      0
    };
  }
  obj = static_cast<MatcherBase *>(m->obj);

  MatcherOptions cpp_options;
  cpp_options.case_sensitive = options.case_sensitive;
  cpp_options.smart_case = options.smart_case;
  cpp_options.num_threads = options.num_threads;
  cpp_options.max_results = options.max_results;
  cpp_options.max_gap = options.max_gap;
  cpp_options.record_match_indexes = options.record_match_indexes;
  std::string root_path_str(options.root_path);
  cpp_options.root_path = root_path_str;

  std::string q(query);
  std::vector<MatchResult> matches = obj->findMatches(q, cpp_options);

  match_result_t *results = new match_result[matches.size()];
  for (size_t i = 0; i < matches.size(); i++) {
    results[i] = {
      matches[i].score,
      matches[i].value->c_str(),
      matches[i].matchIndexes != NULL ? matches[i].matchIndexes->data() : 0,
      matches[i].score_based_root_path
    };
  }

  return {
    matches.size(),
    results
  };
}

void matcher_add_candidate(matcher_t *m, char *candidate) {
  MatcherBase *obj;
  if (m == NULL) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);

  std::string s(candidate);
  obj->addCandidate(s);
}

void matcher_remove_candidate(matcher_t *m, char *candidate) {
  MatcherBase *obj;
  if (m == NULL) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);

  std::string s(candidate);
  obj->removeCandidate(s);
}

void matcher_clear(matcher_t *m) {
  MatcherBase *obj;
  if (m == NULL) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  obj->clear();
}

void matcher_reserve(matcher_t *m, size_t n) {
  MatcherBase *obj;
  if (m == NULL) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  obj->reserve(n);
}

size_t matcher_size(matcher_t *m) {
  MatcherBase *obj;
  if (m == NULL) {
  return 0;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  return obj->size();
}
