(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_match_utils
(* NOTE: Will only work if the list of patches is constructed off of the
   exact same AST that is passed to the matcher *)

(* Range adjustment functions *)
(* Expands to trailing semicolon and newline, if it expands to a newline,
   will also expand to any leading spaces on the same line *)
val adjust_range_stmt : string -> int -> int -> int * int
(* Expands to the trailing comma
   TODO handle leading commas and spaces *)
val adjust_range_expr : string -> int -> int -> int * int
(* Doesnt do anything *)
val dummy_adjuster : string -> int -> int -> int * int

(* Args: file, source, parsed source for pattern and target, returns the
   map of transformations that need to be done *)
(* TODO add ability to make additions or replacements *)
val preprocess_patch_file :
  Relative_path.t -> string -> Parser_hack.parser_return ->
  Relative_path.t -> string -> Parser_hack.parser_return -> patch_maps

(* Given a list of transformations, pretty print them into human readable
   format to show what things in the pattern will become what in the target.
   Returns a pair of string lists, each string is a transformation, first
   list is statements, second is expressions. *)
val to_string_patch_maps :
  patch_maps -> Relative_path.t -> string -> string list * string list


(* For creating patches from node types that are not stmt, expr.
   Get the extent of the source node by calling the appropriate
   Ast_code_extent.source_extent_* function and
   Get the target string by calling the appropriate unparsing function -
   Unparser.Unparse.u_* then calling Unparsed.to_string on the result.

   Returns None if passed an invalid extent
   (one that includes Lexing.dummy_pos) *)
val create_any_patch :
  extent : File_pos.t * File_pos.t ->
  target_string : string -> patch option

(* Given a source file, a node in that source file's AST and a target node,
   create a patch that will transform the source to the target in the source
   file.

   The adjust_ranges argument allows you to set whether or not the patch
   will try to adjust the range that is replaced in the source:
   (e.g. to include leading whitespace and trailing semicolon + newline for stmt
    and to include leading/trailing commas if necessary on exprs).

   example of range adjustment:
          "    stmt;\n"       "    stmt;\n"
   Before:     ^   ^    After: ^         ^

   It is reccommended to have adjust_ranges be false if src_node, tgt_node
   are not of type stmt or expr or if the patch will be formatted when it is
   applied (apply_patches ~format_result:true).

   NOTE: Currently only supports expr, stmt. For other types, create the patch
   yourself (Hh_match_utils.patch) or use create_any_patch.

   Pre: src_node from ast of src, tgt_node and src_node same type (e.g. both
   exprs or both stmts)
   Post: returns Some patch if one could be constructed, None otherwise *)
val create_patch :
  adjust_ranges : bool -> src : string -> src_node : ast_node ->
  tgt_node : ast_node -> patch option

(* Given a source file and a list of patches to apply, apply them and format
   the result if format_result is true then return the new source *)
val apply_patches :
  format_result : bool -> src : string -> patches : patch list -> string
