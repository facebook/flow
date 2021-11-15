(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_mapper

module Make (L : Loc_sig.S) = struct
  module L = L

  type root =
    | Annotation of (L.t, L.t) Ast.Type.annotation
    | Value of (L.t, L.t) Ast.Expression.t
    | Contextual of L.t

  type selector =
    | Elem of int
    | Prop of {
        prop: string;
        has_default: bool;
      }
    | Computed of (L.t, L.t) Ast.Expression.t
    | ObjRest of {
        used_props: string list;
        after_computed: bool;
      }
    | ArrRest of int
    | Default of (L.t, L.t) Ast.Expression.t

  type binding =
    | Root of root
    | Select of selector * binding

  type def =
    | Binding of binding
    | Function of (L.t, L.t) Ast.Function.t
    | TypeAlias of (L.t, L.t) Ast.Statement.TypeAlias.t
    | TypeParam of (L.t, L.t) Ast.Type.TypeParam.t

  type map = def L.LMap.t

  module Destructure = struct
    open Ast.Pattern

    let type_of_pattern (_, p) =
      let open Ast.Pattern in
      match p with
      | Array { Array.annot = Ast.Type.Available t; _ }
      | Object { Object.annot = Ast.Type.Available t; _ }
      | Identifier { Identifier.annot = Ast.Type.Available t; _ } ->
        Some t
      | _ -> None

    let pattern_default acc = function
      | None -> acc
      | Some e -> Select (Default e, acc)

    let array_element acc i = Select (Elem i, acc)

    let array_rest_element acc i = Select (ArrRest i, acc)

    let object_named_property acc x ~has_default = Select (Prop { prop = x; has_default }, acc)

    let object_computed_property acc e = Select (Computed e, acc)

    let object_rest_property acc xs has_computed =
      Select (ObjRest { used_props = xs; after_computed = has_computed }, acc)

    let object_property acc xs key ~has_default =
      let open Ast.Pattern.Object in
      match key with
      | Property.Identifier (_, { Ast.Identifier.name = x; comments = _ }) ->
        let acc = object_named_property acc x ~has_default in
        (acc, x :: xs, false)
      | Property.Literal (_, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
        let acc = object_named_property acc x ~has_default in
        (acc, x :: xs, false)
      | Property.Computed (_, { Ast.ComputedKey.expression; comments = _ }) ->
        let acc = object_computed_property acc expression in
        (acc, xs, true)
      | Property.Literal (_, _) -> (acc, xs, false)

    let identifier ~f acc name_loc = f name_loc (Binding acc)

    let rec pattern ~f acc (_, p) =
      match p with
      | Array { Array.elements; annot = _; comments = _ } -> array_elements ~f acc elements
      | Object { Object.properties; annot = _; comments = _ } -> object_properties ~f acc properties
      | Identifier { Identifier.name = id; optional = _; annot = _ } ->
        let (id_loc, _) = id in
        identifier ~f acc id_loc
      | Expression _ -> ()

    and array_elements ~f acc =
      let open Ast.Pattern.Array in
      Base.List.iteri ~f:(fun i -> function
        | Hole _ -> ()
        | Element (_, { Element.argument = p; default = d }) ->
          let acc = array_element acc i in
          let acc = pattern_default acc d in
          pattern ~f acc p
        | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
          let acc = array_rest_element acc i in
          pattern ~f acc p
      )

    and object_properties =
      let open Ast.Pattern.Object in
      let prop ~f acc xs has_computed p =
        match p with
        | Property (_, { Property.key; pattern = p; default = d; shorthand = _ }) ->
          let has_default = d <> None in
          let (acc, xs, has_computed') = object_property acc xs key ~has_default in
          let acc = pattern_default acc d in
          pattern ~f acc p;
          (xs, has_computed || has_computed')
        | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
          let acc = object_rest_property acc xs has_computed in
          pattern ~f acc p;
          (xs, false)
      in
      let rec loop ~f acc xs has_computed = function
        | [] -> ()
        | p :: ps ->
          let (xs, has_computed) = prop ~f acc xs has_computed p in
          loop ~f acc xs has_computed ps
      in
      (fun ~f acc ps -> loop ~f acc [] false ps)
  end

  class def_finder =
    object (this)
      inherit [map, L.t] Flow_ast_visitor.visitor ~init:L.LMap.empty as super

      method add_binding loc src = this#update_acc (L.LMap.add loc src)

      method! variable_declarator ~kind decl =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_, { id; init }) = decl in
        let source =
          match (Destructure.type_of_pattern id, init) with
          | (Some annot, _) -> Some (Annotation annot)
          | (None, Some init) -> Some (Value init)
          | (None, None) -> None
        in
        Base.Option.iter
          ~f:(fun acc -> Destructure.pattern ~f:this#add_binding (Root acc) id)
          source;
        super#variable_declarator ~kind decl

      method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
        let open Ast.Statement.DeclareVariable in
        let { id = (id_loc, _); annot; comments = _ } = decl in
        this#add_binding id_loc (Binding (Root (Annotation annot)));
        super#declare_variable loc decl

      method! function_param (param : ('loc, 'loc) Ast.Function.Param.t) =
        let open Ast.Function.Param in
        let (loc, { argument; default }) = param in
        let source =
          match Destructure.type_of_pattern argument with
          | Some annot -> Annotation annot
          | None -> Contextual loc
        in
        let source = Destructure.pattern_default (Root source) default in
        Destructure.pattern ~f:this#add_binding source argument;
        super#function_param param

      method! function_rest_param (expr : ('loc, 'loc) Ast.Function.RestParam.t) =
        let open Ast.Function.RestParam in
        let (loc, { argument; _ }) = expr in
        let source =
          match Destructure.type_of_pattern argument with
          | Some annot -> Annotation annot
          | None -> Contextual loc
        in
        Destructure.pattern ~f:this#add_binding (Root source) argument;
        super#function_rest_param expr

      method! function_ loc expr =
        let open Ast.Function in
        let { id; _ } = expr in
        begin
          match id with
          | Some (id_loc, _) -> this#add_binding id_loc (Function expr)
          | None -> ()
        end;
        super#function_ loc expr

      method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
        let open Ast.Statement.DeclareFunction in
        let { id = (id_loc, _); annot; predicate = _; comments = _ } = decl in
        this#add_binding id_loc (Binding (Root (Annotation annot)));
        super#declare_function loc decl

      method! assignment loc (expr : ('loc, 'loc) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left = (_, lhs_node) as left; right; comments = _ } = expr in
        let () =
          match (operator, lhs_node) with
          | (None, _) -> Destructure.pattern ~f:this#add_binding (Root (Value right)) left
          | (Some _operator, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name = _name; _ }) ->
            (* Add an OpAssignment def *) ()
          | _ -> ()
        in
        super#assignment loc expr

      method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
        let open Ast.Statement.TypeAlias in
        let { id = (id_loc, _); _ } = alias in
        this#add_binding id_loc (TypeAlias alias);
        super#type_alias loc alias

      method! type_param (tparam : ('loc, 'loc) Ast.Type.TypeParam.t) =
        let open Ast.Type.TypeParam in
        let (_, { name = (name_loc, _); _ }) = tparam in
        this#add_binding name_loc (TypeParam tparam);
        super#type_param tparam
    end

  let find_defs ast =
    let finder = new def_finder in
    finder#eval finder#program ast
end

module With_Loc = Make (Loc_sig.LocS)
module With_ALoc = Make (Loc_sig.ALocS)
