/*
 * Portions Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Based on the TypeScript implementation from VSCode:
 * https://github.com/microsoft/vscode/blob/e79a401ba5f6bc8eff07bfffaf4544e96f394837/src/vs/base/common/filters.ts#L574
 * That implementation is...
 *
 *   Copyright (c) Microsoft Corporation. All rights reserved.
 */

#include "score_match.h"

#include <algorithm>
#include <cstring>

const int MAX_LEN = 128;

thread_local int _minWordMatchPos[2 * MAX_LEN] {0};

thread_local int _maxWordMatchPos[2 * MAX_LEN] {0};

enum Arrow { LeftLeft, Left, Diag };

//  row 0 and column 0 are base cases, so these tables must be MAX_LEN + 1
thread_local Arrow _arrows[MAX_LEN + 1][MAX_LEN + 1]{Diag};
thread_local long _diag[MAX_LEN + 1][MAX_LEN + 1]{0};
thread_local long _table[MAX_LEN + 1][MAX_LEN + 1]{0};

// Convenience structure for passing around during recursion.
struct MatchInfo {
  const char *haystack;
  const char *haystack_case;
  size_t haystack_len;
  const char *needle;
  const char *needle_case;
  size_t needle_len;
  bool boost_full_match;
  bool first_match_can_be_weak;
};

const long MAX_SAFE_INTEGER = 9007199254740991;

bool is_separator_at_pos(const char* value, size_t value_len, size_t index) {
  if (index < 0 || index >= value_len) {
    return false;
  }
  switch (value[index]) {
    case '_':
    case '-':
    case '.':
    case ' ':
    case '/':
    case '\\':
    case '\'':
    case '"':
    case ':':
    case '$':
    case '<':
    case '>':
    case '(':
    case ')':
    case '[':
    case ']':
    case '{':
    case '}':
      return true;
    default:
      return false;
  }
}

bool is_whitespace_at_pos(const char* value, size_t value_len, size_t index) {
  if (index < 0 || index >= value_len) {
    return false;
  }
  return value[index] == ' ' || value[index] == '\t';
}

bool is_uppercase_at_pos(size_t pos, const char* word, const char* word_lower) {
  return word[pos] != word_lower[pos];
}

// needle is the pattern being searched for
// haystack is the word being matched against
long do_score(
    const MatchInfo& m,
    const size_t haystack_idx,
    const size_t needle_idx,
    bool new_match_start,
    bool& has_strong_first_match) {
  const char* word = m.haystack;
  const char* word_lower = m.haystack_case;
  size_t word_len = m.haystack_len;
  const size_t word_pos = haystack_idx;

  const char* pattern = m.needle;
  const char* pattern_lower = m.needle_case;
  const size_t pattern_pos = needle_idx;

  if (pattern_lower[pattern_pos] != word_lower[word_pos]) {
    return MIN_SCORE;
  }

  long score;
  bool is_gap_location;

  if (word_pos == pattern_pos) {
    // common prefix: `foobar <-> foobaz`
    //                            ^^^^^
    score = pattern[pattern_pos] == word[word_pos] ? 7 : 5;
    is_gap_location = false;
  } else if (
      is_uppercase_at_pos(word_pos, word, word_lower) &&
      (word_pos == 0 || !is_uppercase_at_pos(word_pos - 1, word, word_lower))) {
    // hitting upper-case: `foo <-> forOthers`
    //                              ^^ ^
    score = pattern[pattern_pos] == word[word_pos] ? 7 : 5;
    is_gap_location = true;
  } else if (
      is_separator_at_pos(word, word_len, word_pos) &&
      (word_pos == 0 || !is_separator_at_pos(word, word_len, word_pos - 1))) {
    // hitting a separator: `. <-> foo.bar`
    //                                ^
    score = 5;
    is_gap_location = false;
  } else if (
      is_separator_at_pos(word, word_len, word_pos - 1) ||
      is_whitespace_at_pos(word, word_len, word_pos - 1)) {
    // post separator: `foo <-> bar_foo`
    //                              ^^^
    score = 5;
    is_gap_location = true;
  } else {
    score = 1;
    is_gap_location = is_uppercase_at_pos(word_pos, word, word_lower);
  }

  if (score > 1 && pattern_pos == 0) {
    has_strong_first_match = true;
  }

  if (pattern_pos == 0) {
    // first character in pattern
    if (word_pos > 0) {
      // the first pattern character would match a word character that is not at
      // the word start so introduce a penalty to account for the gap preceding
      // this match
      score -= is_gap_location ? 3 : 5;
    }
  } else if (new_match_start) {
    // this would be the beginning of a new match (i.e. there would be a gap
    // before this location)
    score += is_gap_location ? 2 : 0;
  } else {
    // this is part of a contiguous match, so give it a slight bonus, but do so
    // only if it would not be a preferred gap location
    score += is_gap_location ? 0 : 1;
  }

  if (word_pos + 1 == word_len) {
    // we always penalize gaps, but this gives unfair advantages to a match that
    // would match the last character in the word so pretend there is a gap
    // after the last character in the word to normalize things
    score -= is_gap_location ? 3 : 5;
  }

  return score;
}

bool isPatternInWord(const char* patternLow, size_t patternPos, size_t patternLen, const char* wordLow, size_t wordPos, size_t wordLen) {
  while (patternPos < patternLen && wordPos < wordLen) {
    if (patternLow[patternPos] == wordLow[wordPos]) {
      // Remember the min word position for each pattern position
      _minWordMatchPos[patternPos] = wordPos;
      patternPos += 1;
    }
    wordPos += 1;
  }
  return patternPos == patternLen; // pattern must be exhausted
}

void _fillInMaxWordMatchPos(size_t patternLen, size_t wordLen, const char* patternLow, const char* wordLow) {
  size_t patternPos = patternLen - 1;
  size_t wordPos = wordLen - 1;
  while (patternPos >= 0 && wordPos >= 0) {
    if (patternLow[patternPos] == wordLow[wordPos]) {
      _maxWordMatchPos[patternPos] = wordPos;
      if (patternPos == 0) {
        break;
      }
      patternPos--;
    }
    if (wordPos == 0) {
      break;
    }
    wordPos--;
  }
}

bool do_fuzzy_score(const MatchInfo& m, long* result) {
  bool first_match_can_be_weak = m.first_match_can_be_weak;

  // const char* pattern = m.needle;
  const char* patternLow = m.needle_case;
  const char* word = m.haystack;
  const char* wordLow = m.haystack_case;

  int patternLen = m.needle_len > MAX_LEN ? MAX_LEN : m.needle_len;
  int wordLen = m.haystack_len > MAX_LEN ? MAX_LEN : m.haystack_len;

  if (0 >= patternLen || 0 >= wordLen || patternLen > wordLen) {
    return false;
  }

  // Run a simple check if the characters of pattern occur
  // (in order) at all in word. If that isn't the case we
  // stop because no match will be possible
  if (!isPatternInWord(patternLow, 0, patternLen, wordLow, 0, wordLen)) {
    return false;
  }

  // Find the max matching word position for each pattern position
  // NOTE: the min matching word position was filled in above, in the `isPatternInWord` call
  _fillInMaxWordMatchPos(patternLen, wordLen, patternLow, wordLow);

  int row = 1;
  int column = 1;
  int patternPos = 0;
  int wordPos = 0;

  bool hasStrongFirstMatch = false;

  // There will be a match, fill in tables
  for (row = 1, patternPos = 0; patternPos < patternLen; row++, patternPos++) {

    // Reduce search space to possible matching word positions and to possible access from next row
    int minWordMatchPos = _minWordMatchPos[patternPos];
    int maxWordMatchPos = _maxWordMatchPos[patternPos];
    int nextMaxWordMatchPos = (patternPos + 1 < patternLen ? _maxWordMatchPos[patternPos + 1] : wordLen);

    for (column = minWordMatchPos + 1, wordPos = minWordMatchPos; wordPos < nextMaxWordMatchPos; column++, wordPos++) {
      long score = MIN_SCORE;
      bool canComeDiag = false;

      if (wordPos <= maxWordMatchPos) {
        score = do_score(
          m,
          wordPos,
          patternPos,
          _diag[row - 1][column - 1] == 0,
          hasStrongFirstMatch
        );
      }

      long diagScore = 0;
      if (score != MAX_SAFE_INTEGER) {
        canComeDiag = true;
        diagScore = score + _table[row - 1][column - 1];
      }

      bool canComeLeft = wordPos > minWordMatchPos;
      long leftScore = canComeLeft ? _table[row][column - 1] + (_diag[row][column - 1] > 0 ? -5 : 0) : 0; // penalty for a gap start

      bool canComeLeftLeft = wordPos > minWordMatchPos + 1 && _diag[row][column - 1] > 0;
      long leftLeftScore = canComeLeftLeft ? _table[row][column - 2] + (_diag[row][column - 2] > 0 ? -5 : 0) : 0; // penalty for a gap start

      if (canComeLeftLeft && (!canComeLeft || leftLeftScore >= leftScore) && (!canComeDiag || leftLeftScore >= diagScore)) {
        // always prefer choosing left left to jump over a diagonal because that means a match is earlier in the word
        _table[row][column] = leftLeftScore;
        _arrows[row][column] = LeftLeft;
        _diag[row][column] = 0;
      } else if (canComeLeft && (!canComeDiag || leftScore >= diagScore)) {
        // always prefer choosing left since that means a match is earlier in the word
        _table[row][column] = leftScore;
        _arrows[row][column] = Left;
        _diag[row][column] = 0;
      } else if (canComeDiag) {
        _table[row][column] = diagScore;
        _arrows[row][column] = Diag;
        _diag[row][column] = _diag[row - 1][column - 1] + 1;
      } else {
        // not possible
      }
    }
  }

  if (!hasStrongFirstMatch && !first_match_can_be_weak) {
    return false;
  }

  row--;
  column--;

  *result = _table[row][column];

  int backwardsDiagLength = 0;
  int maxMatchColumn = 0;

  while (row >= 1) {
    // Find the column where we go diagonally up
    int diagColumn = column;
    do {
      long arrow = _arrows[row][diagColumn];
      if (arrow == LeftLeft) {
        diagColumn = diagColumn - 2;
      } else if (arrow == Left) {
        diagColumn = diagColumn - 1;
      } else {
        // found the diagonal
        break;
      }
    } while (diagColumn >= 1);

    // Overturn the "forwards" decision if keeping the "backwards" diagonal would give a better match
    if (
      backwardsDiagLength > 1 // only if we would have a contiguous match of 3 characters
      && patternLow[row - 1] == wordLow[column - 1] // only if we can do a contiguous match diagonally
      && !is_uppercase_at_pos(diagColumn - 1, word, wordLow) // only if the forwards chose diagonal is not an uppercase
      && backwardsDiagLength + 1 > _diag[row][diagColumn] // only if our contiguous match would be longer than the "forwards" contiguous match
    ) {
      diagColumn = column;
    }

    if (diagColumn == column) {
      // this is a contiguous match
      backwardsDiagLength++;
    } else {
      backwardsDiagLength = 1;
    }

    if (!maxMatchColumn) {
      // remember the last matched column
      maxMatchColumn = diagColumn;
    }

    row--;
    column = diagColumn - 1;
  }

  if (wordLen == patternLen && m.boost_full_match) {
    // the word matches the pattern with all characters!
    // giving the score a total match boost (to come up ahead other words)
    *result += 2;
  }

  // Add 1 penalty for each skipped character in the word
  int skippedCharsCount = maxMatchColumn - patternLen;
  *result -= skippedCharsCount;

  return true;
}

bool fuzzy_score(
    const char* haystack,
    const char* haystack_lower,
    const char* needle,
    const char* needle_lower,
    const MatchOptions& options,
    long* result) {
  if (!*needle) {
    return 1.0;
  }

  MatchInfo m;
  m.haystack_len = strlen(haystack);
  m.needle_len = strlen(needle);
  m.haystack = haystack;
  m.needle = needle;
  m.haystack_case = haystack_lower;
  m.needle_case = needle_lower;
  m.boost_full_match = options.boost_full_match;
  m.first_match_can_be_weak = options.first_match_can_be_weak;

  return do_fuzzy_score(m, result);
}
