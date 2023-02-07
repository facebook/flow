#pragma once

#include <cstddef>
#include <string>

struct MatchOptions {
  bool smart_case;
  /* If false, the first character of the needle must be a "strong" match:
     it must be the first character of the haystack, or immediately following
     a word boundary. */
  bool first_match_can_be_weak;
};

/**
 * Returns a matching score between 0-1.
 * 0 represents no match at all, while 1 is a perfect match.
 * See implementation for scoring details.
 */
float score_match(const char *haystack,
                  const char *haystack_lower,
                  const char *needle,
                  const char *needle_lower,
                  const MatchOptions &options,
                  const float min_score);
