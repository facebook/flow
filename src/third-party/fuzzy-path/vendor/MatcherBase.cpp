// Copyright 2004-present Facebook. All Rights Reserved.

#include "MatcherBase.h"
#include "score_match.h"

#include <algorithm>
#include <atomic>
#include <queue>
#include <thread>

typedef std::priority_queue<MatchResult> ResultHeap;

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

inline std::string str_to_lower(const std::string &s) {
  std::string lower(s);
  for (auto& c : lower) {
    if (c >= 'A' && c <= 'Z') {
      c += 'a' - 'A';
    }
  }
  return lower;
}

// Push a new entry on the heap while ensuring size <= max_results.
void push_heap(ResultHeap &heap,
               float score,
               const std::string *value,
               size_t max_results) {
  MatchResult result(score, value);
  if (heap.size() < max_results || result < heap.top()) {
    heap.push(std::move(result));
    if (heap.size() > max_results) {
      heap.pop();
    }
  }
}

std::vector<MatchResult> finalize(const std::string &query,
                             const std::string &query_case,
                             const MatchOptions &options,
                             ResultHeap &&heap) {
  std::vector<MatchResult> vec;
  while (heap.size()) {
    const MatchResult &result = heap.top();
    vec.push_back(result);
    heap.pop();
  }
  reverse(vec.begin(), vec.end());
  return vec;
}

void thread_worker(
  const std::string &query,
  const std::string &query_case,
  const MatchOptions &options,
  bool use_last_match,
  std::atomic<float>* min_score,
  size_t max_results,
  std::vector<MatcherBase::CandidateData> &candidates,
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

std::vector<MatchResult> MatcherBase::findMatches(const std::string &query,
                                             const MatcherOptions &options) {
  size_t max_results = options.max_results;
  size_t num_threads = options.num_threads;
  if (max_results == 0) {
    max_results = std::numeric_limits<size_t>::max();
  }

  bool first_match_can_be_weak = options.first_match_can_be_weak;

  MatchOptions matchOptions;
  matchOptions.smart_case = false;
  matchOptions.first_match_can_be_weak = first_match_can_be_weak;

  std::string new_query;
  // Ignore all whitespace in the query.
  for (auto c : query) {
    if (!isspace(c)) {
      new_query += c;
    }
    if (options.smart_case && isupper(c)) {
      matchOptions.smart_case = true;
    }
  }

  std::string query_case = str_to_lower(new_query);

  // If our current query is just an extension of the last query,
  // quickly ignore all previous non-matches as an optimization.
  bool use_last_match = query_case.substr(0, lastQuery_.size()) == lastQuery_ &&
      first_match_can_be_weak == lastQueryFirstMatchCanBeWeak_;
  lastQuery_ = query_case;
  lastQueryFirstMatchCanBeWeak_ = first_match_can_be_weak;

  ResultHeap combined;
  std::atomic<float> min_score(0);
  if (num_threads == 0 || candidates_.size() < 10000) {
    thread_worker(new_query, query_case, matchOptions, use_last_match, &min_score,
                  max_results, candidates_, 0, candidates_.size(), combined);
  } else {
    std::vector<ResultHeap> thread_results(num_threads);
    std::vector<std::thread> threads;
    size_t cur_start = 0;
    for (size_t i = 0; i < num_threads; i++) {
      size_t chunk_size = candidates_.size() / num_threads;
      // Distribute remainder among the chunks.
      if (i < candidates_.size() % num_threads) {
        chunk_size++;
      }
      threads.emplace_back(
        thread_worker,
        std::ref(new_query),
        std::ref(query_case),
        std::ref(matchOptions),
        use_last_match,
        &min_score,
        max_results,
        std::ref(candidates_),
        cur_start,
        cur_start + chunk_size,
        std::ref(thread_results[i])
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
    std::move(combined)
  );
}

void MatcherBase::addCandidate(const std::string &candidate) {
  auto it = lookup_.find(candidate);
  if (it == lookup_.end()) {
    std::string lowercase = str_to_lower(candidate);
    lookup_[candidate] = candidates_.size();
    CandidateData data;
    data.value = candidate;
    data.bitmask = letter_bitmask(lowercase.c_str());
    data.lowercase = std::move(lowercase);
    data.last_match = true;
    candidates_.emplace_back(std::move(data));
  }
}

void MatcherBase::removeCandidate(const std::string &candidate) {
  auto it = lookup_.find(candidate);
  if (it != lookup_.end()) {
    if (it->second + 1 != candidates_.size()) {
      std::swap(candidates_[it->second], candidates_.back());
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
