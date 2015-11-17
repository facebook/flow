(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * This module allows syntactic matching over hack files. It uses matching
 * specially formatted hack files for patterns and looks for these patterns
 * in normal hack files.
 *)

open Ast
open Hh_match_utils

(* ================== ID NAMING INSTRUCITONS ==================== *)

(* Constants for regular expression-type commands in identifiers;
   should be separated by '_', can appear in any order.

   TODO: make validator for expression files that checks if identifiers
   are valid combinations of types

   Prefix any regex commands with the magic_prefix, "__"

   Wildcard = "ANY"
   Equivalent of grep '.' for an AST node
   For larger constructs such as classes or functions, this means that anything
   of that type will match ignoring the body.

   WildcardName "SOMENAME"
   Will match an arbitrary name (for classes, functions). Will still require
   the body of the construct to match unlike Wildcard.
   e.g. "class __SOMENAME {}" will match any empty classes.

   SkipAny = "SOMENODE"
   For stmt only (and maybe expr later): skip any number of AST constructs
   possibly going up and down in the tree.
   Difference from grep '.*' match:
   ".* stmt1 .*" will match any block that has a direct child equal to stmt1
   "... stmt1 ..." will match any block that has a descendant equal to stmt1
   TODO find a better name
   TODO implement matching for this

   Not = "NOT"
   matches when no instances of the following ast_node exist

   KStar = "KSTAR"
   equivalent of grep '.*'

   Or = "OR"
   equivalent of grep '|'

   DoNotCare = "SOMENODE"
   constant for when we want to disregard an identifier (we're just
   putting it in so the parser doesn't complain)

   Metavariable = "META"
   Metavariables similar to Pfff
   TODO: finish implementing this later *)

(* find matches of a pattern over a text
   Args: text program, text filepath, text code, pattern program
   Return: list of matches *)
val find_matches : program -> Relative_path.t -> string ->
                   Parser_hack.parser_return ->
                   (ast_node * File_pos.t) list

(* does the same thing as find_matches except that it will also apply
   any patches passed in (provided that the patches come from the
   pattern AST that was passed in *)
val match_and_patch: program -> Relative_path.t -> string ->
                     Parser_hack.parser_return -> patch_maps ->
                     use_hh_format:bool -> string option

(* these do the same thing as the above two methods except they only work on
   patterns/targets that consist of a single statement that is an expression
   (to enable the -e option in the hh_matcher) or a single statement block
   (to enable the -s option in the hh_matcher) *)
val find_matches_expr : program -> Relative_path.t -> string ->
                        Parser_hack.parser_return ->
                        (ast_node * File_pos.t) list

val patch_expr: program -> Relative_path.t ->
                string -> Parser_hack.parser_return ->
                patch_maps -> use_hh_format:bool -> string option

val find_matches_stmt : program -> Relative_path.t -> string ->
                        Parser_hack.parser_return ->
                        (ast_node * File_pos.t) list

val patch_stmt : program -> Relative_path.t -> string ->
                 Parser_hack.parser_return -> patch_maps ->
                 use_hh_format:bool -> string option

(* gives nice formatted output of the list of matches as a string that
   has all the lines containing a match in order in the form:
   line num: line
   given a list of matches and the source file of the text *)
val format_matches : (ast_node * File_pos.t) list -> string -> string
