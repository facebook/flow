(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a member_info = {
  ty: 'a;
  def_loc: ALoc.t option;
  from_proto: bool;
      (** Autocomplete ranks members from primitive prototypes below user-defined members.
          [from_proto] indicates that the member is from a primitive prototype. *)
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

let rec members_of_ty : Ty.t -> Ty.t member_info NameUtils.Map.t * string list =
  let open Ty in
  let ty_of_named_prop = function
    | Field { t; optional = false; _ }
    | Get t
    | Set t ->
      t
    | Field { t; optional = true; _ } -> Union (t, Void, [])
    | Method ft -> Fun ft
  in
  let members_of_obj obj_props =
    obj_props
    |> Base.List.fold_left ~init:(NameUtils.Map.empty, []) ~f:(fun (mems1, errs1) prop ->
           let (mems2, errs2) =
             match prop with
             | NamedProp { name; prop; from_proto; def_loc } ->
               ( NameUtils.Map.singleton
                   name
                   { ty = ty_of_named_prop prop; from_proto; from_nullable = false; def_loc },
                 [] )
             | SpreadProp ty -> members_of_ty ty
             | CallProp _ -> (NameUtils.Map.empty, [])
           in
           (NameUtils.Map.union ~combine:(fun _ _ snd -> Some snd) mems1 mems2, errs1 @ errs2))
  in
  let members_of_union (t1, t2, ts) =
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
            (Fn.const { ty; from_proto = true; from_nullable = false; def_loc = None })
            universe
        | Nullish ->
          NameUtils.Map.map
            (* Bot is the identity of type union *)
            (Fn.const
               { ty = Bot EmptyType; from_proto = true; from_nullable = true; def_loc = None })
            universe
        | Normal -> ty_members
      in
      (f t1 t1_members, f t2 t2_members, List.map2 f ts ts_members)
    in
    let mems =
      (* set intersection of members; type union upon overlaps *)
      NameUtils.Map.map (map_member_info Nel.one) t1_members
      |> List.fold_right
           (NameUtils.Map.merge (fun _ ty_opt tys_opt ->
                match (ty_opt, tys_opt) with
                | ( Some { ty; from_proto = fp; from_nullable = fn; def_loc = dl },
                    Some { ty = tys; from_proto = fps; from_nullable = fns; def_loc = dls } ) ->
                  (* We say that a member formed by unioning other members should be treated:
                   * - as from a prototype only if all its constituent members are.
                   * - as from a nullable object if any of its constituent members are.
                   * we say its def_loc is that of an arbitrary constituent member. *)
                  Some
                    {
                      ty = Nel.cons ty tys;
                      from_proto = fp && fps;
                      from_nullable = fn || fns;
                      def_loc = Base.Option.first_some dl dls;
                    }
                | (None, _)
                | (_, None) ->
                  None))
           (t2_members :: ts_members)
      |> NameUtils.Map.map
           (map_member_info (function
               | (t, []) -> t
               | (t1, t2 :: ts) -> Ty_utils.simplify_type ~merge_kinds:true (Union (t1, t2, ts))))
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
      NameUtils.Map.map (map_member_info Nel.one) t1_members
      |> List.fold_right
           (NameUtils.Map.merge (fun _ ty_opt tys_opt ->
                match (ty_opt, tys_opt) with
                | ( Some { ty; from_proto = fp; from_nullable = fn; def_loc = dl },
                    Some { ty = tys; from_proto = fps; from_nullable = fns; def_loc = dls } ) ->
                  (* We say that a member formed by intersecting other members should be treated:
                   * - as from a prototype only if all its constituent members are.
                   * - as from a nullable object only if all its constituent members are.
                   * we say its def_loc is that of an arbitrary constituent member. *)
                  Some
                    {
                      ty = Nel.cons ty tys;
                      from_proto = fp && fps;
                      from_nullable = fn && fns;
                      def_loc = Base.Option.first_some dl dls;
                    }
                | (Some info, None) -> Some (map_member_info Nel.one info)
                | (None, Some info) -> Some info
                | (None, None) -> None))
           (t2_members :: ts_members)
      |> NameUtils.Map.map (map_member_info (List.fold_right Nel.cons special_cases))
      |> NameUtils.Map.map
           (map_member_info (function
               | (t, []) -> t
               | (t1, t2 :: ts) -> Ty_utils.simplify_type ~merge_kinds:true (Inter (t1, t2, ts))))
    in
    (mems, errs)
  in
  function
  | Obj { obj_props; _ } -> members_of_obj obj_props
  | Fun { fun_static; _ } -> members_of_ty fun_static
  | Union (t1, t2, ts) -> members_of_union (t1, t2, ts)
  | Inter (t1, t2, ts) -> members_of_intersection (t1, t2, ts)
  | ( TVar _ | Bound _ | Generic _ | Symbol | Num _ | Str _ | Bool _ | NumLit _ | StrLit _
    | BoolLit _ | Arr _ | Tup _ ) as t ->
    ( NameUtils.Map.empty,
      [Printf.sprintf "members_of_ty unexpectedly applied to (%s)" (Ty_debug.dump_t t)] )
  | Any _
  | Top
  | Bot _
  | Void
  | Null
  | InlineInterface _
  | TypeOf _
  | Utility _
  | IndexedAccess _
  | Mu _
  | CharSet _ ->
    (NameUtils.Map.empty, [])

type ty_members = {
  members: Ty.t member_info NameUtils.Map.t;
  errors: string list;
  in_idx: bool;
}

let ty_normalizer_options =
  Ty_normalizer_env.
    {
      expand_internal_types = true;
      expand_type_aliases = false;
      flag_shadowed_type_params = true;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = false;
      optimize_types = true;
      omit_targ_defaults = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 50;
    }

let extract ~include_proto_members ~cx ~typed_ast ~file_sig scheme =
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig in
  let in_idx_ref = ref false in
  let idx_hook () = in_idx_ref := true in
  match
    Ty_normalizer.expand_members
      ~include_proto_members
      ~idx_hook
      ~options:ty_normalizer_options
      ~genv
      scheme
  with
  | exception Flow_js.Not_expect_bound x -> Error x
  | Error error -> Error (Ty_normalizer.error_to_string error)
  | Ok (Ty.Any _) -> Error "not enough type information to extract members"
  | Ok this_ty ->
    let (members, errors) = members_of_ty this_ty in
    let in_idx = !in_idx_ref in
    Ok { members; errors; in_idx }
