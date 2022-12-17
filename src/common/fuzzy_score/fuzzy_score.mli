(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val fuzzy_score :
  ?boost_full_match:bool ->
  ?first_match_can_be_weak:bool ->
  ?pattern_start:int ->
  ?word_start:int ->
  pattern:string ->
  string ->
  int option
