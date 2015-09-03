(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * This module helps find the code extent (character position ranges in
 * the source file) of each node in the AST output by the hack parser.
 *)
open Ast

(* utility functions*)
val parse_file : Relative_path.t -> Parser_hack.parser_return
val lexing_slice_to_string :(Lexing.position * Lexing.position) ->
                            string -> string
val format_lexing_pos : Lexing.position -> string

(* source extent finding functions *)
val source_extent_catch : Relative_path.t -> string -> catch ->
                          (Lexing.position * Lexing.position)
val source_extent_do : Relative_path.t -> string -> block -> expr ->
                       (Lexing.position * Lexing.position)
val source_extent_efun : Relative_path.t -> string ->fun_ -> (id * bool) list ->
                         (Lexing.position * Lexing.position)
val source_extent_eif : Relative_path.t ->
                        string ->
                        expr ->
                        expr option ->
                        expr ->
                        (Lexing.position * Lexing.position)
val source_extent_expr : Relative_path.t -> string -> expr ->
                         (Lexing.position * Lexing.position)
val source_extent_for : Relative_path.t ->
                        string ->
                        expr ->
                        expr ->
                        expr ->
                        block ->
                        (Lexing.position * Lexing.position)
val source_extent_foreach : Relative_path.t -> string ->
                            expr -> Pos.t option -> as_expr -> block ->
                            (Lexing.position * Lexing.position)
val source_extent_hint : Relative_path.t -> string -> hint ->
                         (Lexing.position * Lexing.position)
val source_extent_if : Relative_path.t -> string -> expr -> block -> block ->
                       (Lexing.position * Lexing.position)
val source_extent_lfun: Relative_path.t -> string -> fun_ ->
                        (Lexing.position * Lexing.position)
val source_extent_stmt : Relative_path.t -> string -> stmt ->
                         (Lexing.position * Lexing.position)
val source_extent_switch : Relative_path.t -> string -> expr -> case list ->
                           (Lexing.position * Lexing.position)
val source_extent_throw : Relative_path.t -> string -> expr ->
                          (Lexing.position * Lexing.position)
val source_extent_try : Relative_path.t ->
                        string ->
                        block ->
                        catch list ->
                        block ->
                        (Lexing.position * Lexing.position)
val source_extent_while : Relative_path.t -> string -> expr -> block ->
                          (Lexing.position * Lexing.position)
val source_extent_class_ : Relative_path.t -> string -> class_ ->
                           (Lexing.position * Lexing.position)
val source_extent_def : Relative_path.t -> string -> def ->
                        (Lexing.position * Lexing.position)
val source_extent_fun_ : Relative_path.t -> string -> fun_ ->
                         (Lexing.position * Lexing.position)
val source_extent_method_ : Relative_path.t -> string -> method_ ->
                            (Lexing.position * Lexing.position)
val source_extent_program : Relative_path.t -> string -> program ->
                            (Lexing.position * Lexing.position)
