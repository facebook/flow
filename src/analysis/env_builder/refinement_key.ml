(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module type REFINEMENT_KEY = sig
  module L : Loc_sig.S

  type proj

  type lookup = {
    base: string;
    projections: proj list;
  }

  type t = {
    loc: L.t;
    lookup: lookup;
  }

  val debug_string_of_t : t -> string

  val of_optional_chain : ('a, L.t) Flow_ast.Expression.t -> t option

  val of_expression : ('a, L.t) Flow_ast.Expression.t -> t option

  val of_argument : ('a, L.t) Flow_ast.Expression.expression_or_spread -> t option

  val of_name : string -> L.t -> t

  val lookup_of_name : string -> lookup

  val lookup_of_name_with_projections : string -> proj list -> lookup

  val lookup_of_member :
    allow_optional:bool -> ('a, L.t) Flow_ast.Expression.Member.t -> lookup option

  val lookup_of_expression :
    ?allow_optional:bool -> ('a, L.t) Flow_ast.Expression.t -> lookup option

  val proj_uses_propname : private_:bool -> string -> proj list -> bool
end

module Make (L : Loc_sig.S) : REFINEMENT_KEY with module L = L = struct
  module L = L

  type proj =
    | Prop of string
    | Elem of lookup
    | PrivateField of string

  and lookup = {
    base: string;
    projections: proj list;
  }

  and t = {
    loc: L.t;
    lookup: lookup;
  }

  (* These functions are all either slightly modified or directly copied from src/typing/refinement.ml.
   * Eventually this module will replace that one. *)

  let rec debug_string_of_lookup { base; projections } =
    base
    ^ String.concat
        ""
        (List.rev projections
        |> Base.List.map ~f:(function
               | Prop name -> spf ".%s" name
               | PrivateField name -> spf "private.%s" name
               | Elem expr -> spf "[%s]" (debug_string_of_lookup expr)
               )
        )

  let debug_string_of_t { loc; lookup } =
    spf "{loc = %s; lookup = %s}" (L.debug_to_string loc) (debug_string_of_lookup lookup)

  (* true if the given lookup uses the given property name *)
  let rec uses_propname propname ~private_ { projections; base = _ } =
    proj_uses_propname ~private_ propname projections

  (* true if the given projection list uses the given property name *)
  and proj_uses_propname ~private_ propname = function
    | Prop name :: tail ->
      (name = propname && not private_) || proj_uses_propname ~private_ propname tail
    | PrivateField name :: tail ->
      (name = propname && private_) || proj_uses_propname ~private_ propname tail
    | Elem lookup :: tail ->
      uses_propname ~private_ propname lookup || proj_uses_propname ~private_ propname tail
    | [] -> false

  (* These functions are adapted from typing/refinement.ml. Eventually, this will be the only place
   * where refinement logic lives, so jmbrown is ok with this temporary duplication while he is
   * fleshing out the refinement features of EnvBuilder
   *
   * The purpose of these functions is to extract _what_ is being refined when we have something like
   * expr != null. What in expr does this refine? *)
  let rec lookup_of_expression ?(allow_optional = true) =
    let open Flow_ast.Expression in
    function
    | (_, Identifier id) -> Some (lookup_of_identifier id)
    | (_, OptionalMember { OptionalMember.member; _ }) when allow_optional ->
      lookup_of_member ~allow_optional member
    | (_, Member member) -> lookup_of_member ~allow_optional member
    | _ ->
      (* other LHSes unsupported currently/here *)
      None

  and lookup_of_member ~allow_optional { Flow_ast.Expression.Member._object; property; _ } =
    let open Flow_ast.Expression.Member in
    match property with
    | PropertyIdentifier (_, { Flow_ast.Identifier.name; comments = _ })
    | PropertyExpression
        (_, Flow_ast.Expression.Literal { Flow_ast.Literal.value = Flow_ast.Literal.String name; _ })
    | PropertyExpression
        ( _,
          Flow_ast.Expression.Literal
            { Flow_ast.Literal.value = Flow_ast.Literal.Number _; raw = name; comments = _ }
        ) ->
      (match lookup_of_expression ~allow_optional _object with
      | Some { base; projections } -> Some { base; projections = Prop name :: projections }
      | None -> None)
    | PropertyPrivateName (_, { Flow_ast.PrivateName.name; comments = _ }) ->
      (match lookup_of_expression ~allow_optional _object with
      | Some { base; projections } -> Some { base; projections = PrivateField name :: projections }
      | None -> None)
    | PropertyExpression index ->
      (* foo.bar[baz] -> Chain [Index baz; Id bar; Id foo] *)
      (match lookup_of_expression ~allow_optional _object with
      | Some { base; projections } ->
        (match lookup_of_expression ~allow_optional index with
        | Some lookup -> Some { base; projections = Elem lookup :: projections }
        | None -> None)
      | None -> None)

  and lookup_of_identifier (_, { Flow_ast.Identifier.name; comments = _ }) =
    { base = name; projections = [] }

  let of_expression expr =
    let loc = fst expr in
    match lookup_of_expression expr with
    | None -> None
    | Some lookup -> Some { loc; lookup }

  let of_argument arg =
    match arg with
    | Flow_ast.Expression.Spread _ -> None
    | Flow_ast.Expression.Expression e -> of_expression e

  let lookup_of_name name = { base = name; projections = [] }

  let lookup_of_name_with_projections base projections = { base; projections }

  let of_name name loc = { loc; lookup = lookup_of_name name }

  let rec lookup_of_optional_chain expr =
    let open Flow_ast.Expression in
    match expr with
    | (_, Call _) -> None
    | (_, Member _) -> None
    | ( _,
        OptionalMember
          {
            OptionalMember.member =
              { Member._object = (_, Identifier (_, { Flow_ast.Identifier.name; _ })); _ };
            _;
          }
      ) ->
      Some (lookup_of_name name)
    | (_, OptionalMember { OptionalMember.member = { Member._object = subject; _ }; _ })
    | (_, OptionalCall { OptionalCall.call = { Call.callee = subject; _ }; _ }) ->
      lookup_of_optional_chain subject
    | _ -> None

  let of_optional_chain expr =
    let loc = fst expr in
    match lookup_of_optional_chain expr with
    | None -> None
    | Some lookup -> Some { loc; lookup }
end
