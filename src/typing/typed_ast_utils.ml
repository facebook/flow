(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap

(* TODO(nmote) come up with a consistent story for abstract/concrete locations in this module *)

let mk_bound_t loc name =
  let reason = Reason.(mk_annot_reason (RType name) loc) in
  Type.BoundT (reason, name)

class type_parameter_mapper =
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    (* Since the mapper wasn't originally written to pass an accumulator value
       through the calls, we're maintaining this accumulator imperatively. *)
    val mutable bound_tparams : (ALoc.t * string) list = []

    method annot_with_tparams : 'a. ((ALoc.t * string) list -> 'a) -> 'a = (fun f -> f bound_tparams)

    (* Imperatively adds type parameter to bound_tparams environment. *)
    method! type_param tparam =
      let res = super#type_param tparam in
      let (_, { Ast.Type.TypeParam.name; _ }) = tparam in
      let (id_loc, { Ast.Identifier.name; comments = _ }) = name in
      bound_tparams <- (id_loc, name) :: bound_tparams;
      res

    (* Record and restore the parameter environment around nodes that might
       update it. *)
    method! type_params_opt pd f =
      let originally_bound_tparams = bound_tparams in
      let res = super#type_params_opt pd f in
      bound_tparams <- originally_bound_tparams;
      res

    (* Classes assume an additional "this" type parameter, which needs to be
       explicitly added to bound_tparams *)
    method! class_ cls =
      let this_tparam =
        let open Ast.Class in
        let { body = (body_loc, _); id; _ } = cls in
        let id_loc = Base.Option.value_map ~f:(fun ((loc, _), _) -> loc) id ~default:body_loc in
        (id_loc, "this")
      in
      let originally_bound_tparams = bound_tparams in
      bound_tparams <- this_tparam :: bound_tparams;
      let cls = super#class_ cls in
      bound_tparams <- originally_bound_tparams;
      cls
  end

(* Find exact location match *)
module ExactMatchQuery = struct
  exception Found of Type.TypeScheme.t

  let found t tparams = raise (Found { Type.TypeScheme.tparams; type_ = t })

  class exact_match_searcher (target_loc : ALoc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method! type_param_identifier id =
        let (loc, { Ast.Identifier.name; comments = _ }) = id in
        if target_loc = loc then
          self#annot_with_tparams (found (mk_bound_t loc name))
        else
          super#type_param_identifier id

      method! on_type_annot annot =
        let (loc, t) = annot in
        if target_loc = loc then
          self#annot_with_tparams (found t)
        else
          super#on_type_annot annot
    end

  let find typed_ast aloc =
    let searcher = new exact_match_searcher aloc in
    try
      ignore (searcher#program typed_ast);
      None
    with Found scheme -> Some scheme
end

let find_exact_match_annotation = ExactMatchQuery.find

(* Find identifier under location *)
module Type_at_pos = struct
  exception Found of ALoc.t * Type.TypeScheme.t

  (* Kinds of nodes that "type-at-pos" is interested in:
   * - identifiers              (handled in t_identifier)
   * - type parameters          (handled in type_param_identifier)
   * - literal object keys      (handled in object_key)
   * - `this`, `super`          (handled in expression)
   * - private property names   (handled in expression)
   *)
  class type_at_pos_searcher (target_loc : Loc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method find_loc : 'a. ALoc.t -> Type.t -> (ALoc.t * string) list -> 'a =
        (fun loc t tparams -> raise (Found (loc, { Type.TypeScheme.tparams; type_ = t })))

      method! t_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#t_identifier id

      method! jsx_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#jsx_identifier id

      method! type_param_identifier id =
        let (loc, { Ast.Identifier.name; comments = _ }) = id in
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc (mk_bound_t loc name))
        else
          super#type_param_identifier id

      method! object_key key =
        let open Ast.Expression.Object.Property in
        match key with
        | Literal ((loc, t), _) when self#covers_target loc ->
          self#annot_with_tparams (self#find_loc loc t)
        | _ -> super#object_key key

      method! expression expr =
        let open Ast.Expression in
        match expr with
        | ((loc, t), (This _ | Super _))
        | ((_, t), Member { Member.property = Member.PropertyPrivateName (loc, _); _ })
        | ( (_, t),
            OptionalMember
              {
                OptionalMember.member = { Member.property = Member.PropertyPrivateName (loc, _); _ };
                _;
              } )
          when self#covers_target loc ->
          self#annot_with_tparams (fun tparams -> self#find_loc loc t tparams)
        | _ -> super#expression expr

      method! implicit (((loc, t), _) as impl) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#implicit impl
    end

  let find typed_ast loc =
    let searcher = new type_at_pos_searcher loc in
    try
      ignore (searcher#program typed_ast);
      None
    with Found (loc, scheme) -> Some (ALoc.to_loc_exn loc, scheme)
end

let find_type_at_pos_annotation = Type_at_pos.find

class type_at_aloc_map_folder =
  object
    inherit type_parameter_mapper

    val mutable map = ALocMap.empty

    method! on_type_annot x =
      let (loc, type_) = x in
      let scheme = Type.TypeScheme.{ type_; tparams = bound_tparams } in
      map <- ALocMap.add loc scheme map;
      x

    method to_map = map
  end

class type_at_aloc_list_folder =
  object
    inherit type_parameter_mapper

    val mutable l = []

    method! on_type_annot x =
      let (loc, type_) = x in
      l <- (loc, Type.TypeScheme.{ type_; tparams = bound_tparams }) :: l;
      x

    method to_list = l
  end

let typed_ast_to_map typed_ast : Type.TypeScheme.t ALocMap.t =
  let folder = new type_at_aloc_map_folder in
  ignore (folder#program typed_ast);
  folder#to_map

let typed_ast_to_list typed_ast : (ALoc.t * Type.TypeScheme.t) list =
  let folder = new type_at_aloc_list_folder in
  ignore (folder#program typed_ast);
  folder#to_list

(** Mappers
 *  Used to construct error nodes during type checking.
 *)

(* Error nodes are typed at `any`. Do not change this type as it might change
 * current behavior. *)
let error_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.AnyT.at (Type.AnyError None) loc)
  end

(* Used in unimplemented cases or unsupported nodes *)
let unimplemented_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unimplemented)) loc)
  end

(* Code is not checked at all *)
let unchecked_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unchecked)) loc)
  end

let unreachable_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(EmptyT.at loc |> with_trust bogus_trust))
  end
