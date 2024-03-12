(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocMap = Loc_collections.ALocMap
open Typed_ast_finder

(* Find identifier under location *)
module Type_at_pos = struct
  exception Found of ALoc.t * bool * Type.t

  (* Kinds of nodes that "type-at-pos" is interested in:
   * - identifiers              (handled in t_identifier)
   * - type parameters          (handled in type_param_identifier)
   * - literal object keys      (handled in object_key)
   * - `this`, `super`          (handled in expression)
   * - private property names   (handled in expression)
   *)
  class type_at_pos_searcher cx (target_loc : Loc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method find_loc
          : 'a. ALoc.t -> Type.t -> is_type_identifier:bool -> tparams_rev:Type.typeparam list -> 'a
          =
        (fun loc t ~is_type_identifier ~tparams_rev:_ -> raise (Found (loc, is_type_identifier, t)))

      method! t_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#t_identifier id

      method! type_identifier_reference (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:true)
        else
          super#t_identifier id

      method! jsx_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#jsx_identifier id

      method! type_param ((_, { Ast.Type.TypeParam.name = (loc, _); _ }) as tparam) =
        if self#covers_target loc then (
          let tparam = self#make_typeparam tparam in
          rev_bound_tparams <- tparam :: rev_bound_tparams;
          self#annot_with_tparams
            (self#find_loc loc (mk_bound_t cx tparam) ~is_type_identifier:false)
        ) else
          super#type_param tparam

      method! object_key key =
        let open Ast.Expression.Object.Property in
        match key with
        | StringLiteral ((loc, t), _)
        | NumberLiteral ((loc, t), _)
        | BigIntLiteral ((loc, t), _)
          when self#covers_target loc ->
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
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
              }
          )
          when self#covers_target loc ->
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        | _ -> super#expression expr

      method! implicit (((loc, t), _) as impl) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#implicit impl

      method! jsx_attribute_name_identifier (((loc, _), _) as id) =
        if self#covers_target loc then
          let reason = Reason.mk_reason (Reason.RCustom "jsx attr") loc in
          let (_, lazy_hint) = Type_env.get_hint cx loc in
          lazy_hint reason
          |> Type_hint.with_hint_result
               ~ok:(fun t ->
                 self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false))
               ~error:(fun () -> super#jsx_attribute_name_identifier id)
        else
          super#jsx_attribute_name_identifier id
    end

  let find cx typed_ast loc =
    let searcher = new type_at_pos_searcher cx loc in
    try
      ignore (searcher#program typed_ast);
      None
    with
    | Found (loc, is_type_id, scheme) -> Some (ALoc.to_loc_exn loc, is_type_id, scheme)
end

let find_type_at_pos_annotation = Type_at_pos.find

class type_at_aloc_map_folder =
  object
    inherit type_parameter_mapper

    val mutable map = ALocMap.empty

    method! on_type_annot x =
      let (loc, t) = x in
      map <- ALocMap.add loc t map;
      x

    method to_map = map
  end

class type_at_aloc_list_folder =
  object
    inherit type_parameter_mapper

    val mutable l = []

    method! on_type_annot x =
      let (loc, t) = x in
      l <- (loc, t) :: l;
      x

    method to_list = l
  end

let typed_ast_to_map typed_ast : Type.t ALocMap.t =
  let folder = new type_at_aloc_map_folder in
  ignore (folder#program typed_ast);
  folder#to_map

let typed_ast_to_list typed_ast : (ALoc.t * Type.t) list =
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

(* Used in skipped body due to placeholder function types. *)
let placeholder_mapper cx =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Context.mk_placeholder cx Reason.(mk_reason RAnyImplicit loc))
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

let untyped_ast_mapper =
  object
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot (loc, _) = loc
  end
