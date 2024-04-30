(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let trace_depth n = n

let dummy_trace = 0

let unit_trace = 1

let rec_trace parent =
  let parent_depth = trace_depth parent in
  parent_depth + 1

let concat_trace ts =
  let d = List.fold_left (fun acc d -> Base.Int.max acc d) 0 ts in
  d
