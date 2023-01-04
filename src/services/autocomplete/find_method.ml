(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type node =
  | Method of (Loc.t, Loc.t) Flow_ast.Class.Method.t'
  | Property of (Loc.t, Loc.t) Flow_ast.Type.Object.Property.t'

exception Found of node

class finder target_loc =
  object
    inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

    method! class_element (elem : (Loc.t, Loc.t) Flow_ast.Class.Body.element) =
      ignore
        (match elem with
        | Flow_ast.Class.Body.Method
            ( _,
              ( {
                  Flow_ast.Class.Method.key = Flow_ast.Expression.Object.Property.Identifier (loc, _);
                  _;
                } as meth
              )
            )
          when Loc.equal target_loc loc ->
          raise (Found (Method meth))
        | _ -> ());
      super#class_element elem

    method! object_property_type (opt : (Loc.t, Loc.t) Flow_ast.Type.Object.Property.t) =
      ignore
        (match opt with
        | ( _,
            ( {
                Flow_ast.Type.Object.Property.key =
                  Flow_ast.Expression.Object.Property.Identifier (loc, _);
                _;
              } as prop
            )
          )
          when Loc.equal target_loc loc ->
          raise (Found (Property prop))
        | _ -> ());
      super#object_property_type opt
  end

let empty_method_of_property_type prop =
  let open Flow_ast.Type.Object.Property in
  let { key; value; static; _ } = prop in
  match value with
  | Init (_, Flow_ast.Type.Function f)
  | Get (_, f)
  | Set (_, f) ->
    Some
      ( Loc.none,
        {
          Flow_ast.Class.Method.kind = Flow_ast.Class.Method.Method;
          key;
          value = (Loc.none, Ast_builder.Functions.of_type f);
          static;
          decorators = [];
          comments = None;
        }
      )
  | _ -> None

let find reader target_loc =
  let open Base.Option.Let_syntax in
  let%bind source = Loc.source target_loc in
  let%bind ast = Parsing_heaps.Reader.get_ast ~reader source in
  try
    ignore ((new finder target_loc)#program ast);
    None
  with
  | Found (Method m) -> Some (Loc.none, m)
  | Found (Property p) -> empty_method_of_property_type p
