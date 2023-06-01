(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Ast.Pattern

type selector =
  | Elem of {
      index: int;
      has_default: bool;
    }
  | Prop of {
      prop: string;
      prop_loc: ALoc.t;
      has_default: bool;
    }
  | Computed of {
      expression: (ALoc.t, ALoc.t) Ast.Expression.t;
      has_default: bool;
    }
  | ObjRest of {
      used_props: string list;
      after_computed: bool;
    }
  | ArrRest of int
  | Default

type binding =
  | Root
  | Rest
  | Select of {
      selector: selector;
      parent: ALoc.t * binding;
    }

let array_element (parent_loc, bind) index direct_default =
  let selector = Elem { index; has_default = direct_default <> None } in
  Select { selector; parent = (parent_loc, bind) }

let array_rest_element (parent_loc, bind) i =
  let selector = ArrRest i in
  Select { selector; parent = (parent_loc, bind) }

let object_named_property (parent_loc, bind) prop_loc x direct_default =
  let has_default = direct_default <> None in
  let selector = Prop { prop = x; prop_loc; has_default } in
  Select { selector; parent = (parent_loc, bind) }

let object_computed_property (parent_loc, bind) e direct_default =
  let selector = Computed { expression = e; has_default = direct_default <> None } in
  Select { selector; parent = (parent_loc, bind) }

let object_rest_property (parent_loc, bind) xs has_computed =
  let selector = ObjRest { used_props = xs; after_computed = has_computed } in
  Select { selector; parent = (parent_loc, bind) }

let object_property (parent_loc, bind) xs key direct_default =
  let open Ast.Pattern.Object in
  match key with
  | Property.Identifier (loc, { Ast.Identifier.name = x; comments = _ }) ->
    let bind = object_named_property (parent_loc, bind) loc x direct_default in
    (bind, x :: xs, false)
  | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
    let bind = object_named_property (parent_loc, bind) loc x direct_default in
    (bind, x :: xs, false)
  | Property.Computed (_, { Ast.ComputedKey.expression; comments = _ }) ->
    let bind = object_computed_property (parent_loc, bind) expression direct_default in
    (bind, xs, true)
  | Property.Literal (_, _) -> (bind, xs, false)

let identifier acc bind (name_loc, { Ast.Identifier.name; _ }) = SMap.add name (name_loc, bind) acc

let rec fold_pattern acc bind (ploc, p) =
  match p with
  | Array { Array.elements; annot = _; comments = _ } -> array_elements acc (ploc, bind) elements
  | Object { Object.properties; annot = _; comments = _ } ->
    object_properties acc (ploc, bind) properties
  | Identifier { Identifier.name = id; optional = _; annot = _ } -> identifier acc bind id
  | Expression _ -> acc

and array_elements acc (parent_loc, bind) elts =
  let open Ast.Pattern.Array in
  Base.List.fold
    ~init:(0, acc)
    ~f:(fun (i, acc) elt ->
      let acc =
        match elt with
        | Hole _ -> acc
        | Element (_, { Element.argument = p; default = d }) ->
          let bind = array_element (parent_loc, bind) i d in
          fold_pattern acc bind p
        | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
          let bind = array_rest_element (parent_loc, bind) i in
          fold_pattern acc bind p
      in
      (i + 1, acc))
    elts
  |> snd

and object_properties =
  let open Ast.Pattern.Object in
  let prop acc (parent_loc, bind) xs has_computed p =
    match p with
    | Property (_, { Property.key; pattern = p; default = d; shorthand = _ }) ->
      let (bind, xs, has_computed') = object_property (parent_loc, bind) xs key d in
      (fold_pattern acc bind p, xs, has_computed || has_computed')
    | RestElement (_, { Ast.Pattern.RestElement.argument = p; comments = _ }) ->
      let bind = object_rest_property (parent_loc, bind) xs has_computed in
      (fold_pattern acc bind p, xs, false)
  in
  let rec loop acc bind xs has_computed = function
    | [] -> acc
    | p :: ps ->
      let (acc, xs, has_computed) = prop acc bind xs has_computed p in
      loop acc bind xs has_computed ps
  in
  (fun acc bind ps -> loop acc bind [] false ps)

let bindings_of_params (_, { Ast.Function.Params.params; rest; _ }) =
  let acc =
    Base.List.fold params ~init:SMap.empty ~f:(fun acc (_, { Ast.Function.Param.argument; _ }) ->
        fold_pattern acc Root argument
    )
  in
  Base.Option.fold rest ~init:acc ~f:(fun acc (_, { Ast.Function.RestParam.argument; _ }) ->
      fold_pattern acc Rest argument
  )

let bindings_of_pattern pattern = fold_pattern SMap.empty Root pattern
