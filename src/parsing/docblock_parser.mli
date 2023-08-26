(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type docblock_error_kind =
  | MultipleFlowAttributes
  | InvalidFlowMode of string
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option
  | MultipleJSXRuntimeAttributes
  | InvalidJSXRuntimeAttribute

type docblock_error = Loc.t * docblock_error_kind

val docblock_max_tokens : int

val parse_docblock :
  max_tokens:int ->
  (* how many tokens to check in the beginning of the file *)
  File_key.t ->
  string ->
  docblock_error list * Docblock.t
