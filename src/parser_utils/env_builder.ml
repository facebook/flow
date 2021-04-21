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

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_builder : Scope_builder_sig.S with module L = L) =
struct
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)

  class env_builder =
    object (this)
      inherit Ssa_builder.ssa_builder as _super

      val mutable expression_refinements = IMap.empty

      method private add_refinement _name _refinement = ()

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
    (ssa_walk#acc, ssa_walk#values)

  let program program =
    let (_, values) = program_with_scope ~ignore_toplevel:true program in
    values
end
