(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* returns all of the comments that start before `loc`, and discards the rest *)
let comments_before_loc loc comments =
  let rec helper loc acc = function
    | ((c_loc, _) as comment) :: rest when Loc.compare c_loc loc < 0 ->
      helper loc (comment :: acc) rest
    | _ -> List.rev acc
  in
  helper loc [] comments

class ['loc] inline_comments_stripper =
  object
    inherit ['loc] Flow_ast_mapper.mapper

    method! syntax_opt
        : 'internal.
          ('loc, 'internal) Flow_ast.Syntax.t option -> ('loc, 'internal) Flow_ast.Syntax.t option =
      (fun _ -> None)
  end

let strip_inlined_comments p = (new inline_comments_stripper)#program p

let strip_inlined_comments_expression expr = (new inline_comments_stripper)#expression expr

let strip_comments_list
    ?(preserve_docblock = false) ((loc, program) : ('loc, 'loc) Flow_ast.Program.t) =
  let { Flow_ast.Program.all_comments; _ } = program in
  ( loc,
    {
      program with
      Flow_ast.Program.all_comments =
        (if preserve_docblock then
          comments_before_loc loc all_comments
        else
          []);
    } )

let strip_all_comments ?(preserve_docblock = false) p =
  p |> strip_comments_list ~preserve_docblock |> strip_inlined_comments
