#pragma once

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

struct MatcherOptions {
  bool case_sensitive = false;
  bool smart_case = false;
  size_t num_threads = 0;
  size_t max_results = 0;
  size_t max_gap = 0;
  bool record_match_indexes = false;
  std::string root_path;
};

struct MatchResult {
  float score;
  // We can't afford to copy strings around while we're ranking them.
  // These are not guaranteed to last very long and should be copied out ASAP.
  const std::string *value;
  // Only computed if `record_match_indexes` was set to true.
  mutable std::shared_ptr<std::vector<int>> matchIndexes = nullptr;
  int score_based_root_path;

  MatchResult(float score,
              int score_based_root_path,
              const std::string *value)
    : score(score), value(value), score_based_root_path(score_based_root_path) {}

  // Order small scores to the top of any priority queue.
  // We need a min-heap to maintain the top-N results.
  bool operator<(const MatchResult& other) const {
    if (score == other.score) {
      // In case of a tie, favour shorter strings.
      if (score_based_root_path == other.score_based_root_path) {
        return value->length() < other.value->length();
      }
      return score_based_root_path > other.score_based_root_path;
    }
    return score > other.score;
  }
};

class MatcherBase {
public:
  struct CandidateData {
    std::string value;
    std::string lowercase;
    int num_dirs;
    /**
     * A bitmask representing the counts of letters a-z contained in the string.
     * Bits i*2 and i*2 + 1 represent a count of the i-th letter:
     * 00 = 0
     * 01 = 1
     * 11 = 2
     * With this scheme, if (bitmask(X) & bitmask(Y)) == bitmask(X) then we
     * instantly know that Y contains at least all the letters in X.
     */
    uint64_t bitmask;
    /**
     * True if this was a match against lastQuery_.
     * Since the most common use case for this library is for typeaheads,
     * we can often avoid a ton of work by skiping past negatives.
     * We'll use this only if the new query strictly extends lastQuery_.
     */
    bool last_match;
  };

  std::vector<MatchResult> findMatches(const std::string &query,
                                       const MatcherOptions &options);
  void addCandidate(const std::string &candidate);
  void removeCandidate(const std::string &candidate);
  void clear();
  void reserve(size_t n);
  size_t size() const;

private:
  // Storing candidate data in an array makes table scans significantly faster.
  // This makes add/remove slightly more expensive, but in our case queries
  // are significantly more frequent.
  std::vector<CandidateData> candidates_;
  std::unordered_map<std::string, size_t> lookup_;
  std::string lastQuery_;
};
