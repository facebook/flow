#pragma once

#include <cstddef>
#include <string>

struct MatchOptions {
  bool case_sensitive;
  bool smart_case;
  size_t max_gap;
  std::string root_path;
};

/**
 * Returns a matching score between 0-1.
 * 0 represents no match at all, while 1 is a perfect match.
 * See implementation for scoring details.
 *
 * If options.case_sensitive is false, haystack_lower and
 * needle_lower must be provided.
 */
float score_match(const char *haystack,
                  const char *haystack_lower,
                  const char *needle,
                  const char *needle_lower,
                  const MatchOptions &options,
                  const float min_score);
