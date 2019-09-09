(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(* Main module for generating code *)
module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils
module Config = Flowtestgen_config

(* Set for types *)
module TypeSet' = Set.Make (struct
  type t = T.t'

  let compare = Pervasives.compare
end)

module TypeSet = struct
  include TypeSet'

  let choose choose_func set = set |> TypeSet'.elements |> Array.of_list |> choose_func
end

(* This is a set of supported types so far *)
let primitive_types = TypeSet.of_list [T.Number; T.String; T.Boolean]

let literal_types =
  TypeSet.of_list
    [ T.StringLiteral T.StringLiteral.{ value = "strlit"; raw = "\"strlit\"" };
      T.NumberLiteral T.NumberLiteral.{ value = 2.2; raw = "2.2" };
      T.BooleanLiteral T.BooleanLiteral.{ value = true; raw = "true" } ]

(* Make a union type out of an array of types *)
let mk_union_type (tarray : T.t' array) : T.t' =
  Array.(
    match length tarray with
    | 0
    | 1 ->
      failwith "Must provide at least two types"
    | _ ->
      T.Union
        ( (Loc.none, get tarray 0),
          (Loc.none, get tarray 1),
          List.map (fun s -> (Loc.none, s)) (to_list (sub tarray 2 (length tarray - 2))) ))

(* Make an object type output of a list of properties *)
let mk_obj_type (props : (string * T.t') list) : T.t' =
  T.Object.(
    let plist =
      Core_list.map
        ~f:(fun (p, t) ->
          let key = E.Object.Property.(Identifier (Loc.none, p)) in
          let value = T.Object.Property.(Init (Loc.none, t)) in
          T.Object.Property.(
            let variance =
              match Random.int 3 with
              | 0 -> None
              | 1 -> Some (Loc.none, Ast.Variance.Plus)
              | _ -> Some (Loc.none, Ast.Variance.Minus)
            in
            Property
              ( Loc.none,
                { key; value; optional = false; static = false; _method = false; variance } )))
        props
    in
    T.Object { exact = Random.bool (); properties = plist })

let mk_tuple_type (tlist : T.t' list) : T.t' =
  T.Tuple (Core_list.map ~f:(fun t -> (Loc.none, t)) tlist)

(* Return a string literal of a given type *)
let strlit_of_type (t : T.t') : E.t' =
  let mk_type_lit (tstring : string) =
    Ast.Literal.{ value = String tstring; raw = "\"" ^ tstring ^ "\"" }
  in
  E.Literal
    (match t with
    | T.String -> mk_type_lit "string"
    | T.Number -> mk_type_lit "number"
    | T.Boolean -> mk_type_lit "boolean"
    | _ -> mk_type_lit "null")

(* Make a literal expression.*)
let rec mk_literal_expr (t : T.t') : Code.t' =
  Code.(
    match t with
    | T.Number ->
      let lit = Ast.Literal.{ value = Number 1.1; raw = "1.1" } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.String ->
      let lit = Ast.Literal.{ value = String "foo"; raw = "\"foo\"" } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.Boolean ->
      let lit = Ast.Literal.{ value = Boolean false; raw = "false" } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.Union (t1, t2, rest) ->
      let all_types = t1 :: t2 :: rest |> Core_list.map ~f:snd in
      let t = Utils.random_choice (Array.of_list all_types) in
      mk_literal_expr t
    | T.Object obj_t -> mk_obj_literal_expr obj_t
    | T.StringLiteral lit ->
      let value = T.StringLiteral.(lit.value) in
      let raw = T.StringLiteral.(lit.raw) in
      let lit = Ast.Literal.{ value = String value; raw } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.NumberLiteral lit ->
      let value = T.NumberLiteral.(lit.value) in
      let raw = T.NumberLiteral.(lit.raw) in
      let lit = Ast.Literal.{ value = Number value; raw } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.BooleanLiteral lit ->
      let value = T.BooleanLiteral.(lit.value) in
      let raw = T.BooleanLiteral.(lit.raw) in
      let lit = Ast.Literal.{ value = Boolean value; raw } in
      { expr = E.Literal lit; expr_deps = [] }
    | T.Tuple tlist ->
      let elements =
        Core_list.map
          ~f:(fun (_, tt) ->
            let e = mk_literal_expr tt in
            Some (E.Expression (Loc.none, e.expr)))
          tlist
      in
      { expr = E.Array.(E.Array { elements }); expr_deps = [] }
    | T.Array t -> mk_literal_expr (T.Tuple [t; t; t; t; t])
    | _ ->
      let lit = Ast.Literal.{ value = Null; raw = "null" } in
      { expr = E.Literal lit; expr_deps = [] })

(* Make an object literal based on its type *)
and mk_obj_literal_expr (t : T.Object.t) : Code.t' =
  Code.(
    let prop_init_list =
      Core_list.map
        ~f:(fun p ->
          T.Object.Property.(
            match p with
            | T.Object.Property
                ( _,
                  {
                    key = k;
                    value = Init (_, ptype);
                    optional = o;
                    static = _;
                    _method = _;
                    variance = _;
                  } ) ->
              (k, o, mk_literal_expr ptype)
            | _ -> failwith "Unsupported property"))
        T.Object.(t.properties)
      (* Randomly remove some optional properties *)
      |> List.filter (fun (_, o, _) -> (not o) || Random.bool ())
      |> Core_list.map ~f:(fun (key, _, expr_t) ->
             E.Object.Property.(
               E.Object.Property
                 ( Loc.none,
                   {
                     key;
                     value = Init (Loc.none, expr_t.expr);
                     _method = false;
                     shorthand = false;
                   } )))
    in
    E.Object.{ expr = E.Object { properties = prop_init_list }; expr_deps = [] })
