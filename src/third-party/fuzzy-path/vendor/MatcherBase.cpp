// Copyright 2004-present Facebook. All Rights Reserved.

#include "MatcherBase.h"
#include "score_match.h"

#include <algorithm>
#include <atomic>
#include <queue>
#include <thread>

using namespace std;

typedef priority_queue<MatchResult> ResultHeap;

inline uint64_t letter_bitmask(const std::string &str) {
  uint64_t result = 0;
  for (char c : str) {
    if (c >= 'a' && c <= 'z') {
      int index = c - 'a';
      uint64_t count_bit = (result >> (index * 2));
      // "Increment" the count_bit:
      // 00 -> 01
      // 01 -> 11
      // 11 -> 11
      count_bit = ((count_bit << 1) | 1) & 3;
      result |= count_bit << (index * 2);
    } else if (c == '-') {
      result |= (1ULL << 52);
    } else if (c == '_') {
      result |= (1ULL << 53);
    } else if (c >= '0' && c <= '9') {
      result |= (1ULL << (c - '0' + 54));
    }
  }
  return result;
}

inline string str_to_lower(const std::string &s) {
  string lower(s);
  for (auto& c : lower) {
    if (c >= 'A' && c <= 'Z') {
      c += 'a' - 'A';
    }
  }
  return lower;
}

bool is_slash(char c) {
  return c == '/' || c == '\\';
}

int num_dirs(const std::string &path) {
  int num = 0;
  for (size_t i = 0; i < path.length(); i++) {
    if (is_slash(path[i])) {
      num++;
    }
  }
  return num;
}

int score_based_root_path(const MatchOptions &options,
                          const MatcherBase::CandidateData &candidate) {
  const std::string &root = options.root_path;
  if (root.length() == 0) {
    return 0;
  }

  const std::string &value = candidate.value;

  size_t num_common_dirs = 0;
  size_t i = 0;

  // Count number of common directories
  for (; i < root.length() && i < value.length(); i++) {
    if (root[i] != value[i]) {
      break;
    }
    if (is_slash(root[i])) {
      num_common_dirs++;
    }
  }

  if (i == root.length() && i < value.length() && is_slash(value[i])) {
    num_common_dirs++;
  }

  return 1000 * num_common_dirs - candidate.num_dirs;
}

// Push a new entry on the heap while ensuring size <= max_results.
void push_heap(ResultHeap &heap,
               float score,
               int score_based_root_path,
               const std::string *value,
               size_t max_results) {
  MatchResult result(score, score_based_root_path, value);
  if (heap.size() < max_results || result < heap.top()) {
    heap.push(std::move(result));
    if (heap.size() > max_results) {
      heap.pop();
    }
  }
}

vector<MatchResult> finalize(const string &query,
                             const string &query_case,
                             const MatchOptions &options,
                             bool record_match_indexes,
                             ResultHeap &&heap) {
  vector<MatchResult> vec;
  while (heap.size()) {
    const MatchResult &result = heap.top();
    if (record_match_indexes) {
      result.matchIndexes.reset(new vector<int>(query.size()));
      string lower = str_to_lower(*result.value);
      score_match(
        result.value->c_str(),
        lower.c_str(),
        query.c_str(),
        query_case.c_str(),
        options,
        0.0,
        result.matchIndexes.get()
      );
    }
    vec.push_back(result);
    heap.pop();
  }
  reverse(vec.begin(), vec.end());
  return vec;
}

void thread_worker(
  const string &query,
  const string &query_case,
  const MatchOptions &options,
  bool use_last_match,
  std::atomic<float>* min_score,
  size_t max_results,
  vector<MatcherBase::CandidateData> &candidates,
  size_t start,
  size_t end,
  ResultHeap &result
) {
  uint64_t bitmask = letter_bitmask(query_case.c_str());
  for (size_t i = start; i < end; i++) {
    auto &candidate = candidates[i];
    if (use_last_match && !candidate.last_match) {
      continue;
    }
    if ((bitmask & candidate.bitmask) == bitmask) {
      float score = score_match(
        candidate.value.c_str(),
        candidate.lowercase.c_str(),
        query.c_str(),
        query_case.c_str(),
        options,
        min_score->load()
      );
      if (score > 0) {
        push_heap(
          result,
          score,
          score_based_root_path(options, candidate),
          &candidate.value,
          max_results
        );
        if (result.size() == max_results) {
          float current_max = result.top().score;
          float min_score_value = min_score->load();
          // Unfortunately there's no thread-safe "max"...
          // When running compare_exchange_weak it's possible that another
          // thread wrote to it in the meantime, in which case we have to check
          // again. Since it's always increasing this is guaranteed to converge.
          while (current_max > min_score_value) {
            min_score->compare_exchange_weak(min_score_value, current_max);
          }
        }
        candidate.last_match = true;
      } else {
        candidate.last_match = false;
      }
    }
  }
}

vector<MatchResult> MatcherBase::findMatches(const std::string &query,
                                             const MatcherOptions &options) {
  size_t max_results = options.max_results;
  size_t num_threads = options.num_threads;
  if (max_results == 0) {
    max_results = numeric_limits<size_t>::max();
  }
  MatchOptions matchOptions;
  matchOptions.case_sensitive = options.case_sensitive;
  matchOptions.smart_case = false;
  matchOptions.max_gap = options.max_gap;
  matchOptions.root_path = options.root_path;

  string new_query;
  // Ignore all whitespace in the query.
  for (auto c : query) {
    if (!isspace(c)) {
      new_query += c;
    }
    if (options.smart_case && isupper(c) && !matchOptions.case_sensitive) {
      matchOptions.smart_case = true;
    }
  }

  string query_case;
  if (!options.case_sensitive) {
    query_case = str_to_lower(new_query);
  } else {
    query_case = query;
  }

  // If our current query is just an extension of the last query,
  // quickly ignore all previous non-matches as an optimization.
  bool use_last_match = query_case.substr(0, lastQuery_.size()) == lastQuery_;
  lastQuery_ = query_case;

  ResultHeap combined;
  std::atomic<float> min_score(0);
  if (num_threads == 0 || candidates_.size() < 10000) {
    thread_worker(new_query, query_case, matchOptions, use_last_match, &min_score,
                  max_results, candidates_, 0, candidates_.size(), combined);
  } else {
    vector<ResultHeap> thread_results(num_threads);
    vector<thread> threads;
    size_t cur_start = 0;
    for (size_t i = 0; i < num_threads; i++) {
      size_t chunk_size = candidates_.size() / num_threads;
      // Distribute remainder among the chunks.
      if (i < candidates_.size() % num_threads) {
        chunk_size++;
      }
      threads.emplace_back(
        thread_worker,
        ref(new_query),
        ref(query_case),
        ref(matchOptions),
        use_last_match,
        &min_score,
        max_results,
        ref(candidates_),
        cur_start,
        cur_start + chunk_size,
        ref(thread_results[i])
      );
      cur_start += chunk_size;
    }

    for (size_t i = 0; i < num_threads; i++) {
      threads[i].join();
      while (thread_results[i].size()) {
        auto &top = thread_results[i].top();
        push_heap(
          combined,
          top.score,
          top.score_based_root_path,
          top.value,
          max_results
        );
        thread_results[i].pop();
      }
    }
  }

  return finalize(
    new_query,
    query_case,
    matchOptions,
    options.record_match_indexes,
    move(combined)
  );
}

void MatcherBase::addCandidate(const string &candidate) {
  auto it = lookup_.find(candidate);
  if (it == lookup_.end()) {
    string lowercase = str_to_lower(candidate);
    lookup_[candidate] = candidates_.size();
    CandidateData data;
    data.value = candidate;
    data.bitmask = letter_bitmask(lowercase.c_str());
    data.lowercase = move(lowercase);
    data.last_match = true;
    data.num_dirs = num_dirs(candidate);
    candidates_.emplace_back(move(data));
  }
}

void MatcherBase::removeCandidate(const string &candidate) {
  auto it = lookup_.find(candidate);
  if (it != lookup_.end()) {
    if (it->second + 1 != candidates_.size()) {
      swap(candidates_[it->second], candidates_.back());
      lookup_[candidates_[it->second].value] = it->second;
    }
    candidates_.pop_back();
    lookup_.erase(candidate);
  }
}

void MatcherBase::clear() {
  candidates_.clear();
  lookup_.clear();
}

void MatcherBase::reserve(size_t n) {
  candidates_.reserve(n);
  lookup_.reserve(n);
}

size_t MatcherBase::size() const {
  return candidates_.size();
}
