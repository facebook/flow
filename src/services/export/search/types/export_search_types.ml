(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type search_result = {
  name: string;
  source: Export_index.source;
  kind: Export_index.kind;
}
[@@deriving show]

type search_result_scored = {
  search_result: search_result;
  score: int;
  weight: int;
}
[@@deriving show]

type search_results = {
  results: search_result_scored list;
  is_incomplete: bool;
}
[@@deriving show]
