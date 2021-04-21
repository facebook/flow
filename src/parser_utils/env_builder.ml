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

      val mutable expression_refinement_scopes = []

      val mutable refined_reads = L.LMap.empty

      method refined_reads : refinement L.LMap.t = refined_reads

      method private push_refinement_scope () =
        expression_refinement_scopes <- IMap.empty :: expression_refinement_scopes

      method private pop_refinement_scope () =
        expression_refinement_scopes <- List.tl expression_refinement_scopes

      method private negate_new_refinements () =
        let head = List.hd expression_refinement_scopes in
        let head' = IMap.map (fun v -> Not v) head in
        expression_refinement_scopes <- head' :: List.tl expression_refinement_scopes

      method private find_refinement name =
        let writes = SMap.find name this#ssa_env in
        let key = Ssa_builder.Val.id_of_val writes in
        List.fold_left
          (fun refinement refinement_scope ->
            match (IMap.find_opt key refinement_scope, refinement) with
            | (None, _) -> refinement
            | (Some refinement, None) -> Some refinement
            | (Some refinement, Some refinement') -> Some (And (refinement, refinement')))
          None
          expression_refinement_scopes

      method private add_refinement name refinement =
        let writes_to_loc = SMap.find name this#ssa_env in
        match expression_refinement_scopes with
        | scope :: scopes ->
          let scope' =
            IMap.update
              (Ssa_builder.Val.id_of_val writes_to_loc)
              (function
                | None -> Some refinement
                | Some r' -> Some (And (r', refinement)))
              scope
          in
          expression_refinement_scopes <- scope' :: scopes
        | _ -> failwith "Tried to add a refinement when no scope was on the stack"

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
        let { operator; left; right; comments = _ } = expr in
        this#push_refinement_scope ();
        ignore @@ this#expression_refinement left;
        let env1 = this#ssa_env in
        (match operator with
        | Flow_ast.Expression.Logical.Or -> this#negate_new_refinements ()
        | Flow_ast.Expression.Logical.And -> ()
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          failwith "nullish coalescing refinements are not yet implemented");
        ignore @@ this#expression right;
        this#pop_refinement_scope ();
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
