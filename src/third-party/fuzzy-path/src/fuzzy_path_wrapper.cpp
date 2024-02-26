/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <stdlib.h>
#include <string>

#include "fuzzy_path_wrapper.h"

#include "../vendor/MatcherBase.h"
#include "../vendor/score_match.h"

struct matcher {
  void *obj;
};

matcher_t *matcher_create()
{
  void *p;
  matcher_t *m;
  MatcherBase *obj;

  p = malloc(sizeof(*m));
  if (p == nullptr) {
    return nullptr;
  }

  obj = new MatcherBase();

  m = (matcher_t *)p;
  m->obj = obj;

  return m;
}

match_results_t matcher_find(matcher_t *m, const char *query, matcher_options_t options) {
  MatcherBase *obj;
  if (m == nullptr) {
  return {
      0,
      nullptr
    };
  }
  obj = static_cast<MatcherBase *>(m->obj);

  MatcherOptions cpp_options;
  cpp_options.first_match_can_be_weak = options.first_match_can_be_weak;
  cpp_options.num_threads = options.num_threads;
  cpp_options.max_results = options.max_results;
  cpp_options.weighted = options.weighted;

  std::string q(query);
  std::vector<MatchResult> matches = obj->findMatches(q, cpp_options);

  match_result_t *results = new match_result[matches.size()];
  for (size_t i = 0; i < matches.size(); i++) {
    results[i] = {
      matches[i].weight,
      matches[i].score,
      matches[i].value->c_str()
    };
  }

  return {
    matches.size(),
    results
  };
}

void matcher_add_candidate(matcher_t *m, const char *candidate, int weight) {
  MatcherBase *obj;
  if (m == nullptr) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);

  std::string s(candidate);
  obj->addCandidate(s, weight);
}

void matcher_remove_candidate(matcher_t *m, const char *candidate) {
  MatcherBase *obj;
  if (m == nullptr) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);

  std::string s(candidate);
  obj->removeCandidate(s);
}

void matcher_clear(matcher_t *m) {
  MatcherBase *obj;
  if (m == nullptr) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  obj->clear();
}

void matcher_reserve(matcher_t *m, size_t n) {
  MatcherBase *obj;
  if (m == nullptr) {
  return;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  obj->reserve(n);
}

size_t matcher_size(matcher_t *m) {
  MatcherBase *obj;
  if (m == nullptr) {
  return 0;
  }
  obj = static_cast<MatcherBase *>(m->obj);
  return obj->size();
}

inline std::string str_to_lower(const std::string &s) {
  std::string lower(s);
  for (auto& c : lower) {
    if (c >= 'A' && c <= 'Z') {
      c += 'a' - 'A';
    }
  }
  return lower;
}

bool fuzzy_score_c(
    const char* haystack,
    const char* needle,
    bool boost_full_match,
    bool first_match_can_be_weak,
    int64_t* result) {
  std::string haystack_str(haystack);
  std::string haystack_lower = str_to_lower(haystack_str);

  std::string needle_str(needle);
  std::string needle_lower = str_to_lower(needle_str);

  MatchOptions options;
  options.boost_full_match = boost_full_match;
  options.first_match_can_be_weak = first_match_can_be_weak;

  return fuzzy_score(
      haystack,
      haystack_lower.c_str(),
      needle,
      needle_lower.c_str(),
      options,
      result);
}
