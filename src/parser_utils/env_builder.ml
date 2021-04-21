(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type refinement =
  | And of refinement * refinement
  | Or of refinement * refinement
  | Not of refinement
  | Truthy
[@@deriving show { with_path = false }]

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L) =
struct
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)

  class env_builder =
    object (this)
      inherit Ssa_builder.ssa_builder as super

      val mutable expression_refinements = IMap.empty

      val mutable refined_reads = L.LMap.empty

      method refined_reads : refinement L.LMap.t = refined_reads

      method private find_refinement name =
        let writes = SMap.find name this#ssa_env in
        IMap.find_opt (Ssa_builder.Val.id_of_val writes) expression_refinements

      method private add_refinement name refinement =
        let writes_to_loc = SMap.find name this#ssa_env in
        expression_refinements <-
          IMap.add (Ssa_builder.Val.id_of_val writes_to_loc) refinement expression_refinements

      method identifier_refinement ((_loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        this#add_refinement name Truthy

      method expression_refinement ((_loc, expr) as expression) =
        let open Flow_ast.Expression in
        match expr with
        | Identifier ident ->
          this#identifier_refinement ident;
          expression
        | Array _
        | ArrowFunction _
        | Assignment _
        | Binary _
        | Call _
        | Class _
        | Comprehension _
        | Conditional _
        | Function _
        | Generator _
        | Import _
        | JSXElement _
        | JSXFragment _
        | Literal _
        | Logical _
        | MetaProperty _
        | Member _
        | New _
        | Object _
        | OptionalCall _
        | OptionalMember _
        | Sequence _
        | Super _
        | TaggedTemplate _
        | TemplateLiteral _
        | TypeCast _
        | This _
        | Unary _
        | Update _
        | Yield _ ->
          this#expression expression

      method! logical _loc (expr : (L.t, L.t) Flow_ast.Expression.Logical.t) =
        let open Flow_ast.Expression.Logical in
        let { operator = _; left; right; comments = _ } = expr in
        let outer_refs = expression_refinements in
        ignore @@ this#expression_refinement left;
        let env1 = this#ssa_env in
        ignore @@ this#expression right;
        expression_refinements <- outer_refs;
        this#merge_self_ssa_env env1;
        expr

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read *)
      method! any_identifier loc name =
        super#any_identifier loc name;
        match this#find_refinement name with
        | None -> ()
        | Some refinement -> refined_reads <- L.LMap.add loc refinement refined_reads
    end

  let program_with_scope ?(ignore_toplevel = false) program =
    let open Hoister in
    let (loc, _) = program in
    let ssa_walk = new env_builder in
    let bindings =
      if ignore_toplevel then
        Bindings.empty
      else
        let hoist = new hoister ~with_types:true in
        hoist#eval hoist#program program
    in
    ignore @@ ssa_walk#with_bindings loc bindings ssa_walk#program program;
    (ssa_walk#acc, ssa_walk#values, ssa_walk#refined_reads)

  let program program =
    let (_, _, refined_reads) = program_with_scope ~ignore_toplevel:true program in
    refined_reads
end

module With_Loc = Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_builder.With_Loc)
