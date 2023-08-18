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

#pragma once

#include <cstddef>
#include <string>

const int64_t MIN_SCORE = -9007199254740991;

struct MatchOptions {
  bool boost_full_match;

  /* If false, the first character of the needle must be a "strong" match:
     it must be the first character of the haystack, or immediately following
     a word boundary. */
  bool first_match_can_be_weak;
};

bool fuzzy_score(
    const char* haystack,
    const char* haystack_lower,
    const char* needle,
    const char* needle_lower,
    const MatchOptions& options,
    int64_t* result);
