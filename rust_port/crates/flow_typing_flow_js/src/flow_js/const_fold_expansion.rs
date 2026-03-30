/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! The main problem with constant folding is infinite recursion. Consider a loop
// that keeps adding 1 to a variable x, which is initialized to 0. If we
//! constant fold x naively, we'll recurse forever, inferring that x has the type
//! (0 | 1 | 2 | 3 | 4 | etc). What we need to do is recognize loops and stop
//! doing constant folding.
//!
//! One solution is for constant-folding-location to keep count of how many times
//! we have seen a reason at a given position in the array.
//! Then, when we've seen it multiple times in the same place, we can decide
//! to stop doing constant folding.
use flow_common::reason::Reason;
use flow_typing_context::Context;

pub(crate) fn guard<'cx, T, F>(
    cx: &Context<'cx>,
    id: i32,
    reason_with_pos: (Reason, i32),
    f: F,
) -> T
where
    F: FnOnce(i32) -> T,
{
    let key = (id, reason_with_pos.0, reason_with_pos.1);
    let count = cx.const_fold_cache().get(&key).copied().unwrap_or(0);
    cx.const_fold_cache_mut().insert(key, count + 1);
    f(count)
}
