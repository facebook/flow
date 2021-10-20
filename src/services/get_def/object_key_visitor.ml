(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The default visitor does not provide all of the context we need when visiting an object key. In
 * particular, we need the location of the enclosing object literal. *)
class ['acc] visitor ~init =
  object (this)
    inherit ['acc, Loc.t] Flow_ast_visitor.visitor ~init as super

    method! expression (exp : (Loc.t, Loc.t) Flow_ast.Expression.t) =
      let open Flow_ast.Expression in
      begin
        match exp with
        | (loc, Object x) -> this#visit_object_literal loc x
        | _ -> ()
      end;
      super#expression exp

    method private visit_object_literal
        (loc : Loc.t) (obj : (Loc.t, Loc.t) Flow_ast.Expression.Object.t) =
      let open Flow_ast.Expression.Object in
      let get_prop_key =
        Property.(
          function
          | Init { key; _ }
          | Method { key; _ }
          | Get { key; _ }
          | Set { key; _ } ->
            key
        )
      in
      let { properties; comments = _ } = obj in
      properties
      |> List.iter (function
             | SpreadProperty _ -> ()
             | Property (_, prop) -> prop |> get_prop_key |> this#visit_object_key loc
             )

    method private visit_object_key
        (_literal_loc : Loc.t) (_key : (Loc.t, Loc.t) Flow_ast.Expression.Object.Property.key) =
      ()
  end
