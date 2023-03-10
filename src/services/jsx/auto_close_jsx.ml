(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of (Loc.t, Loc.t) Flow_ast.JSX.Opening.t'

class finder target_pos =
  object
    (* Note that this cannot be a Flow_ast_contains_mapper.mapper, because the ending locs of
       jsx elements in error-recovered parses are not correct. For example, if the user types:

       <div>
         <foo>|
       <div>

       The end position of the `div` JSXElement is the end position of the opening tag, not the
       closing one, so it doesn't contain the opening `foo` element.
    *)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! jsx_opening_element ((loc, elem') as elem) =
      if loc.Loc._end = target_pos then raise (Found elem');
      super#jsx_opening_element elem
  end

let get_snippet ast target_pos =
  try
    ignore ((new finder target_pos)#program ast);
    None
  with
  | Found { Flow_ast.JSX.Opening.name; self_closing; _ } ->
    if self_closing then
      None
    else
      let text =
        Ast_builder.JSXs.closing name
        |> Js_layout_generator.jsx_closing
        |> Pretty_printer.print ~source_maps:None ~skip_endline:true
        |> Source.contents
      in
      Some ("$0" ^ text)
