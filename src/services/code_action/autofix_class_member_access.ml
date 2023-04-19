(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Found of Type.t option

class enclosing_class_finder target =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable enclosing_classes : Type.t list = []

    method with_enclosing_class_t : 'a. Type.t -> 'a Lazy.t -> 'a =
      fun class_t f ->
        let previously_enclosing = enclosing_classes in
        enclosing_classes <- class_t :: enclosing_classes;
        let result = Lazy.force f in
        enclosing_classes <- previously_enclosing;
        result

    method on_loc_annot loc =
      if Loc.equal (ALoc.to_loc_exn loc) target then
        raise (Found (Base.List.hd enclosing_classes))
      else
        loc

    method on_type_annot x =
      if Loc.equal (ALoc.to_loc_exn (fst x)) target then
        raise (Found (Base.List.hd enclosing_classes))
      else
        x

    method! statement stmt =
      match stmt with
      | (_, Flow_ast.Statement.ClassDeclaration { Flow_ast.Class.id = Some ((_, t), _); _ }) ->
        this#with_enclosing_class_t t (lazy (super#statement stmt))
      | _ -> super#statement stmt

    method! expression expr =
      match expr with
      | ((_, class_t), Flow_ast.Expression.Class _) ->
        this#with_enclosing_class_t class_t (lazy (super#expression expr))
      | _ -> super#expression expr
  end

let find_enclosing_class typed_ast target =
  try
    let finder = new enclosing_class_finder target in
    ignore (finder#program typed_ast);
    None
  with
  | Found t -> t

class mapper target =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

    method! expression expr =
      let open Flow_ast.Expression in
      match expr with
      | (_, Identifier ((loc, _) as id)) when this#is_target loc ->
        let open Ast_builder in
        Expressions.this () |> Expressions.Members.identifier ~property:id |> Expressions.member
      | _ -> super#expression expr
  end

let prefix_with_this ast target =
  let mapper = new mapper target in
  mapper#program ast

let is_member cx typed_ast file_sig type_ name =
  match
    Ty_members.extract
      ~force_instance:true
      ~cx
      ~typed_ast
      ~file_sig
      Type.TypeScheme.{ tparams_rev = []; type_ }
  with
  | Error _ -> false
  | Ok Ty_members.{ members; _ } ->
    NameUtils.Map.keys members
    |> Base.List.exists ~f:(function
           | Reason.OrdinaryName n -> n = name
           | _ -> false
           )

let fix ~cx ~file_sig ~ast ~typed_ast ~member_name target =
  let open Base.Option.Let_syntax in
  let%bind enclosing_class_t = find_enclosing_class typed_ast target in
  if is_member cx typed_ast file_sig enclosing_class_t member_name then
    let ast = prefix_with_this ast target in
    Some ast
  else
    None
