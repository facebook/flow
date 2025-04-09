(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val better_quote : prefer_single_quotes:bool -> string -> string

val property_key_quotes_needed : string -> bool

val string_of_elt :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.elt ->
  string

val string_of_elt_single_line :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  ?exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.elt ->
  string

val string_of_decl_single_line :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  ?exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.decl ->
  string

val string_of_t :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  ?exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.t ->
  string

val string_of_t_single_line :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  ?exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.t ->
  string

val string_of_type_at_pos_result :
  ?prefer_single_quotes:bool ->
  ?with_comments:bool ->
  ?exact_by_default:bool ->
  ts_syntax:bool ->
  Ty.type_at_pos_result ->
  string * (string * Loc.t) list option

val utf8_escape : quote:string -> string -> string
