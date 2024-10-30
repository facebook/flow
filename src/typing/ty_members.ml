(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a member_info = {
  ty: 'a;
  optional: bool;
  def_locs: ALoc.t list;
      (** The location where the member is defined. There may be multiple in cases
          involving unions (e.g., `{ foo: string } | { foo: number }`). *)
  inherited: bool;
      (** [inherited] indicates whether the member was inherited either from a super
          class, higher in the prototype chain, or from an interface. *)
  source: Ty.prop_source;
      (** [source] indicates whether the member was defined in a "primitive prototype",
          an interface, or some other object/class. *)
  from_nullable: bool;
      (** If a member came from a possibly-null/undefined object, autocomplete may suggest
          that the user use optional chaining to access it.
          [from_nullable] indicates that the member is from a possibly-null/undefined object. *)
}

let map_member_info f info = { info with ty = f info.ty }

(** Special cases we'll consider when recursively getting the members of a constituent type
    in a union/intersection *)
type membership_behavior =
  | EmptyOrAny
      (** the given type's member set is the universal set, where each member's type is the given type. *)
  | Nullish  (** the given type is Void or Null *)
  | Normal  (** a type which falls into neither of these special cases *)

let membership_behavior =
  let open Ty in
  function
  | Bot _
  | Any _ ->
    EmptyOrAny
  | Void
  | Null ->
    Nullish
  | _ -> Normal

let merge_sources src1 src2 =
  let open Ty in
  match (src1, src2) with
  (* Object.prototype has the lowest priority. If a prop comes from anywhere
     else, take that instead. *)
  | (src, PrimitiveProto "Object")
  | (PrimitiveProto "Object", src) ->
    src
  (* All other primitive protos have the next lowest priority. *)
  | (src, PrimitiveProto _)
  | (PrimitiveProto _, src) ->
    src
  (* "Other" has the highest priority. *)
  | (Other, _)
  | (_, Other) ->
    Other
  (* A prop is treated as from an interface only if both constituent props are. *)
  | (Interface, Interface) -> Interface

let rec members_of_ty : Ty.t -> Ty.t member_info NameUtils.Map.t * string list =
  let open Ty in
  let ty_of_named_prop = function
    | Field { t; optional = false; _ }
    | Get t
    | Set t ->
      (t, false)
    | Field { t; optional = true; _ } -> (Union (false, t, Void, []), true)
    | Method ft -> (Fun ft, false)
  in
  let members_of_obj obj_props =
    obj_props
    |> Base.List.fold_left ~init:(NameUtils.Map.empty, []) ~f:(fun (mems1, errs1) prop ->
           let (mems2, errs2) =
             match prop with
             | NamedProp { name; prop; inherited; source; def_locs } ->
               let (ty, optional) = ty_of_named_prop prop in
               let prop = { ty; optional; inherited; source; from_nullable = false; def_locs } in
               (NameUtils.Map.singleton name prop, [])
             | SpreadProp ty -> members_of_ty ty
             (* TODO(jmbrown): Mapped Type autocomplete *)
             | MappedTypeProp _ -> (NameUtils.Map.empty, [])
             | CallProp _ -> (NameUtils.Map.empty, [])
           in
           (NameUtils.Map.union ~combine:(fun _ _ snd -> Some snd) mems1 mems2, errs1 @ errs2)
       )
  in
  (* given a list of objects, collates the members that exist in all of them *)
  let intersection_of_members ((t1_members, ts_members) : 'a member_info NameUtils.Map.t Nel.t) =
    NameUtils.Map.map (map_member_info Nel.one) t1_members
    |> List.fold_right
         (NameUtils.Map.merge (fun _ ty_opt tys_opt ->
              match (ty_opt, tys_opt) with
              | ( Some
                    {
                      ty;
                      optional = opt;
                      inherited = id;
                      source = src;
                      from_nullable = fn;
                      def_locs = dl;
                    },
                  Some
                    {
                      ty = tys;
                      optional = opts;
                      inherited = ids;
                      source = srcs;
                      from_nullable = fns;
                      def_locs = dls;
                    }
                ) ->
                (* We say that a member formed by unioning other members should be treated:
                 * - as inherited if all of its constituent members are.
                 * - as from a nullable object if any of its constituent members are.
                 * - as defined at the definition locations of any of its constituent members. *)
                Some
                  {
                    ty = Nel.cons ty tys;
                    optional = opt || opts;
                    inherited = id && ids;
                    source = merge_sources src srcs;
                    from_nullable = fn || fns;
                    def_locs = Base.List.unordered_append dl dls;
                  }
              | (None, _)
              | (_, None) ->
                None
          )
         )
         ts_members
  in
  (* given a list of objects, collates the members that exist in any of them *)
  let union_of_members ((t1_members, ts_members) : 'a member_info NameUtils.Map.t Nel.t) =
    NameUtils.Map.map (map_member_info Nel.one) t1_members
    |> List.fold_right
         (NameUtils.Map.merge (fun _ ty_opt tys_opt ->
              match (ty_opt, tys_opt) with
              | ( Some
                    {
                      ty;
                      optional = opt;
                      inherited = id;
                      source = src;
                      from_nullable = fn;
                      def_locs = dl;
                    },
                  Some
                    {
                      ty = tys;
                      optional = opts;
                      inherited = ids;
                      source = srcs;
                      from_nullable = fns;
                      def_locs = dls;
                    }
                ) ->
                (* We say that a member formed by intersecting other members should be treated:
                 * - as inherited if all of its constituent members are.
                 * - as from a nullable object only if all its constituent members are.
                 * - as defined at the definition locations of any of its constituent members. *)
                Some
                  {
                    ty = Nel.cons ty tys;
                    optional = opt || opts;
                    inherited = id && ids;
                    source = merge_sources src srcs;
                    from_nullable = fn && fns;
                    def_locs = Base.List.unordered_append dl dls;
                  }
              | (Some info, None) -> Some (map_member_info Nel.one info)
              | (None, Some info) -> Some info
              | (None, None) -> None
          )
         )
         ts_members
  in
  let intersection_of_member_types =
    NameUtils.Map.map
      (map_member_info (function
          | (t, []) -> t
          | (t1, t2 :: ts) -> Ty_utils.simplify_type ~merge_kinds:true (Inter (t1, t2, ts))
          )
          )
  in
  let union_of_member_types ~from_bounds =
    NameUtils.Map.map
      (map_member_info (function
          | (t, []) -> t
          | (t1, t2 :: ts) ->
            Ty_utils.simplify_type ~merge_kinds:true (Union (from_bounds, t1, t2, ts))
          )
          )
  in
  let add_special_cases special_cases =
    NameUtils.Map.map (map_member_info (List.fold_right Nel.cons special_cases))
  in
  let members_of_union ~from_bounds (t1, t2, ts) =
    let ((t1_members, errs1), (t2_members, errs2), (ts_members, errss)) =
      (members_of_ty t1, members_of_ty t2, Base.List.map ~f:members_of_ty ts |> List.split)
    in
    let errs = Base.List.concat (errs1 :: errs2 :: errss) in
    let universe =
      (* set union of all child members *)
      List.fold_right
        (NameUtils.Map.merge (fun _ _ _ -> Some ()))
        (t1_members :: t2_members :: ts_members)
        NameUtils.Map.empty
    in
    (* empty and any have all possible members *)
    let (t1_members, t2_members, ts_members) =
      let f ty ty_members =
        match membership_behavior ty with
        | EmptyOrAny ->
          NameUtils.Map.map
            (Fun.const
               {
                 ty;
                 optional = false;
                 inherited = true;
                 source = PrimitiveProto "Object";
                 from_nullable = false;
                 def_locs = [];
               }
            )
            universe
        | Nullish ->
          NameUtils.Map.map
            (* Bot is the identity of type union *)
            (Fun.const
               {
                 ty = Bot EmptyType;
                 optional = false;
                 inherited = true;
                 source = PrimitiveProto "Object";
                 from_nullable = true;
                 def_locs = [];
               }
            )
            universe
        | Normal -> ty_members
      in
      (f t1 t1_members, f t2 t2_members, List.map2 f ts ts_members)
    in
    let mems =
      (* set intersection of members; type union upon overlaps *)
      (t1_members, t2_members :: ts_members)
      |> intersection_of_members
      |> union_of_member_types ~from_bounds
    in
    (mems, errs)
  in
  let members_of_intersection (t1, t2, ts) =
    let ((t1_members, errs1), (t2_members, errs2), (ts_members, errss)) =
      (members_of_ty t1, members_of_ty t2, Base.List.map ~f:members_of_ty ts |> List.split)
    in
    let errs = Base.List.concat (errs1 :: errs2 :: errss) in
    let special_cases =
      Base.List.filter_map
        ~f:(fun ty ->
          match membership_behavior ty with
          | EmptyOrAny -> Some ty
          | Nullish
          | Normal ->
            None)
        (t1 :: t2 :: ts)
    in
    let mems =
      (* set union of members; type intersection upon overlaps *)
      union_of_members (t1_members, t2_members :: ts_members)
      |> add_special_cases special_cases
      |> intersection_of_member_types
    in
    (mems, errs)
  in
  function
  | Obj { obj_props; _ } -> members_of_obj obj_props
  | Fun { fun_static; _ } -> members_of_ty fun_static
  | Union (from_bounds, t1, t2, ts) -> members_of_union ~from_bounds (t1, t2, ts)
  | Inter (t1, t2, ts) -> members_of_intersection (t1, t2, ts)
  | ( Bound _ | Generic _ | Symbol | Num _ | Str _ | Bool _ | BigInt _ | NumLit _ | StrLit _
    | BoolLit _ | BigIntLit _ | Arr _ | Tup _ ) as t ->
    ( NameUtils.Map.empty,
      [
        Printf.sprintf
          "members_of_ty unexpectedly applied to (%s)"
          (Ty_debug.dump_t_EXPOSES_ABSTRACT_LOCS t);
      ]
    )
  | Any _
  | Top
  | Bot _
  | Void
  | Null
  | InlineInterface _
  | TypeOf _
  | Utility _
  | IndexedAccess _
  | Conditional _
  | Component _
  | Infer _
  | Renders _ ->
    (NameUtils.Map.empty, [])

type ty_members = {
  members: Ty.t member_info NameUtils.Map.t;
  errors: string list;
}

let extract ?(force_instance = false) ?max_size ~cx ~typed_ast_opt ~file_sig scheme =
  let options =
    {
      Ty_normalizer_env.expand_internal_types = true;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = Ty_normalizer_env.EvaluateNone;
      optimize_types = true;
      omit_targ_defaults_option = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 40;
      max_size;
      toplevel_is_type_identifier_reference = false;
    }
  in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~typed_ast_opt ~file_sig in
  match Ty_normalizer_flow.expand_members ~force_instance genv scheme with
  | Error error -> Error (Ty_normalizer.error_to_string error)
  | Ok (Ty.Any _) -> Error "not enough type information to extract members"
  | Ok this_ty ->
    let (members, errors) = members_of_ty this_ty in
    Ok { members; errors }
