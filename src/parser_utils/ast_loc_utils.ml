(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Provides utilities for converting AST locations between ALoc.t and Loc.t *)

let loc_to_aloc_mapper : (Loc.t, Loc.t, ALoc.t, ALoc.t) Flow_polymorphic_ast_mapper.mapper =
  object
    inherit [Loc.t, Loc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot = ALoc.of_loc

    method on_type_annot = ALoc.of_loc
  end

class keyifier filename =
  object (this)
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    val table = ALoc.make_table filename

    val rev_table = ALoc.make_empty_reverse_table ()

    method get_table = table

    method keyify aloc = ALoc.keyify table rev_table aloc

    method on_loc_annot = this#keyify

    method on_type_annot = this#keyify

    (* We don't need the comment locations to be keyed *)
    method! comment (cmt : ALoc.t Flow_ast.Comment.t) : ALoc.t Flow_ast.Comment.t = cmt
  end

let keyify_alocs filename ast =
  let mapper = new keyifier filename in
  let ast' = mapper#program ast in
  let table = mapper#get_table in
  ALoc.shrink_table table;
  (table, ast')
