(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap

(* TODO(nmote) come up with a consistent story for abstract/concrete locations in this module *)

class type_parameter_mapper =
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    (* Since the mapper wasn't originally written to pass an accumulator value
     through the calls, we're maintaining this accumulator imperatively. *)
    val mutable bound_tparams : Type.typeparam list = []

    method annot_with_tparams : 'a. (Type.typeparam list -> 'a) -> 'a = (fun f -> f bound_tparams)

    (* Imperatively adds type parameter to bound_tparams environment. *)
    method! type_param tparam =
      let res = super#type_param tparam in
      (* Recover the Type.typeparams corresponding to AST type parameters *)
      let tparam =
        let ( _,
              {
                Ast.Type.TypeParam.name = ((_, t), { Ast.Identifier.name; comments = _ });
                bound;
                variance;
                default;
              } ) =
          tparam
        in
        let reason = Type.reason_of_t t in
        let bound =
          match bound with
          | Ast.Type.Missing _ -> Type.MixedT.make reason |> Type.with_trust Trust.bogus_trust
          | Ast.Type.Available (_, ((_, t), _)) -> t
        in
        let polarity =
          Ast.Variance.(
            match variance with
            | Some (_, Plus) -> Polarity.Positive
            | Some (_, Minus) -> Polarity.Negative
            | None -> Polarity.Neutral)
        in
        let default = Option.map default ~f:(fun ((_, t), _) -> t) in
        { Type.reason; name; bound; polarity; default }
      in
      bound_tparams <- tparam :: bound_tparams;
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
        Ast.Class.(
          let { body = ((body_loc, self_t), _); id; _ } = cls in
          let name =
            Option.value_map ~f:Flow_ast_utils.name_of_ident id ~default:"<<anonymous class>>"
          in
          let name_loc = Option.value_map ~f:(fun ((loc, _), _) -> loc) id ~default:body_loc in
          {
            Type.name = "this";
            reason = Reason.mk_reason (Reason.RType name) name_loc;
            bound = self_t;
            polarity = Polarity.Positive;
            default = None;
          })
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
   * - literal object keys      (handled in object_key)
   * - `this`, `super`          (handled in expression)
   * - private property names   (handled in expression)
   *)
  class type_at_pos_searcher (target_loc : Loc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method find_loc : 'a. ALoc.t -> Type.t -> Type.typeparam list -> 'a =
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

      method! object_key key =
        Ast.Expression.Object.Property.(
          match key with
          | Literal ((loc, t), _) when self#covers_target loc ->
            self#annot_with_tparams (self#find_loc loc t)
          | _ -> super#object_key key)

      method! expression expr =
        Ast.Expression.(
          match expr with
          | ((loc, t), (This | Super))
          | ((_, t), Member { Member.property = Member.PropertyPrivateName (loc, _); _ })
          | ( (_, t),
              OptionalMember
                {
                  OptionalMember.member =
                    { Member.property = Member.PropertyPrivateName (loc, _); _ };
                  _;
                } )
            when self#covers_target loc ->
            self#annot_with_tparams (fun tparams -> self#find_loc loc t tparams)
          | _ -> super#expression expr)

      method! implicit (loc, t) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#implicit (loc, t)
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

    method on_type_annot loc = (loc, Type.AnyT.at Type.AnyError loc)
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
