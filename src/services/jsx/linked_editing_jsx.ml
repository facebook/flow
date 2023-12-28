(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of Loc.t list

let loc_of_jsx_name =
  let open Flow_ast.JSX in
  function
  | Identifier (loc, _)
  | NamespacedName (loc, _)
  | MemberExpression (loc, _) ->
    loc

let name_loc_of_jsx_fragment_element_loc frag_elem_loc =
  let open Loc in
  let _end = { frag_elem_loc._end with column = frag_elem_loc._end.column - 1 } in
  { frag_elem_loc with start = _end; _end }

class mapper target =
  object
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! jsx_element elem_loc elem =
      let open Flow_ast.JSX in
      match elem with
      | {
       opening_element = (_, { Opening.name = opening_name; _ });
       closing_element = Some (_, { Closing.name = closing_name; _ });
       _;
      } ->
        let opening_loc = loc_of_jsx_name opening_name in
        let closing_loc = loc_of_jsx_name closing_name in
        if Loc.contains opening_loc target then
          raise (Found [opening_loc; closing_loc])
        else
          super#jsx_element elem_loc elem
      | _ -> super#jsx_element elem_loc elem

    method! jsx_fragment frag_loc frag =
      let open Flow_ast.JSX in
      let { frag_opening_element; frag_closing_element; _ } = frag in
      let opening_loc = name_loc_of_jsx_fragment_element_loc frag_opening_element in
      let closing_loc = name_loc_of_jsx_fragment_element_loc frag_closing_element in
      if Loc.contains opening_loc target then
        raise (Found [opening_loc; closing_loc])
      else
        super#jsx_fragment frag_loc frag
  end

let get_linked_locs ast target_loc =
  try
    let mapper = new mapper target_loc in
    ignore (mapper#program ast);
    None
  with
  | Found locs -> Some locs
