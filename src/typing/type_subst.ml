(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil
open Reason

(*****************)
(* substitutions *)
(*****************)

type replacement =
  | TypeSubst of Type.t * (* Free vars in type *) Subst_name.Set.t
  | AlphaRename of Subst_name.t

type fv_acc = {
  bound: Subst_name.Set.t;
  free: Subst_name.Set.t;
}

let visitor =
  object (self)
    inherit [fv_acc] Type_visitor.t as super

    method! type_ cx pole { bound; free } t =
      match t with
      | GenericT { name; _ } when not (Subst_name.Set.mem name bound) ->
        super#type_ cx pole { free = Subst_name.Set.add name free; bound } t
      | DefT (_, PolyT { tparams_loc = _; tparams = xs; t_out = inner; _ }) ->
        let orig_bound = bound in
        let { free; bound } =
          Nel.fold_left
            (fun { free; bound } tp ->
              self#type_param cx pole { free; bound = Subst_name.Set.add tp.name bound } tp)
            { free; bound }
            xs
        in
        let { free; _ } = self#type_ cx pole { free; bound } inner in
        { free; bound = orig_bound }
      | ThisInstanceT (_, t, _, this_name) ->
        let { free; _ } =
          self#instance_type cx pole { free; bound = Subst_name.Set.add this_name bound } t
        in
        { free; bound }
      | _ -> super#type_ cx pole { bound; free } t

    method private with_distributive_tparam_name { bound; free } name f =
      let orig_bound = bound in
      let bound =
        match name with
        | None -> bound
        | Some n -> Subst_name.Set.add n bound
      in
      let { bound = _; free } = f { bound; free } in
      { bound = orig_bound; free }

    method! destructor cx { bound; free } t =
      match t with
      | ConditionalType { distributive_tparam_name; infer_tparams; extends_t; true_t; false_t } ->
        self#with_distributive_tparam_name
          { bound; free }
          distributive_tparam_name
          (fun { free; bound } ->
            let pole = Polarity.Neutral in
            let { bound; free } = self#type_ cx pole { bound; free } false_t in
            let { bound; free } =
              Base.List.fold infer_tparams ~init:{ bound; free } ~f:(fun { bound; free } tp ->
                  self#type_param cx pole { free; bound = Subst_name.Set.add tp.name bound } tp
              )
            in
            let { bound; free } = self#type_ cx pole { bound; free } extends_t in
            self#type_ cx pole { bound; free } true_t
        )
      | MappedType { distributive_tparam_name; property_type; mapped_type_flags = _; homomorphic }
        ->
        self#with_distributive_tparam_name { bound; free } distributive_tparam_name (fun acc ->
            let pole_TODO = Polarity.Neutral in
            let acc = self#type_ cx pole_TODO acc property_type in
            match homomorphic with
            | SemiHomomorphic t -> self#type_ cx pole_TODO acc t
            | Homomorphic
            | Unspecialized ->
              acc
        )
      | _ -> super#destructor cx { bound; free } t
  end

let free_var_finder cx ?(bound = Subst_name.Set.empty) t =
  let { free; _ } = visitor#type_ cx Polarity.Positive { free = Subst_name.Set.empty; bound } t in
  free

(** Substitute bound type variables with associated types in a type. **)

let new_name name fvs =
  let (ct, n) =
    match name with
    | Subst_name.Synthetic { name = n; _ } ->
      failwith (Utils_js.spf "Cannot rename synthetic name %s" n)
    | Subst_name.Name n -> (0, n)
    | Subst_name.Id (ct, n) -> (ct, n)
  in
  let rec loop ct =
    let name = Subst_name.Id (ct, n) in
    if not @@ Subst_name.Set.mem name fvs then
      name
    else
      loop (ct + 1)
  in
  loop (ct + 1)

let fvs_of_map map =
  Subst_name.Map.fold
    (fun _ t acc ->
      match t with
      | TypeSubst (_, fvs) -> Subst_name.Set.union fvs acc
      | AlphaRename _ -> acc)
    map
    Subst_name.Set.empty

let avoid_capture map name =
  let fvs = fvs_of_map map in
  if Subst_name.Set.mem name fvs then
    let new_name =
      new_name name (Subst_name.Map.fold (fun n _ acc -> Subst_name.Set.add n acc) map fvs)
    in
    (new_name, Subst_name.Map.add name (AlphaRename new_name) map)
  else
    (name, Subst_name.Map.remove name map)

let union_ident_map_and_dedup =
  let rec union_flatten cx seen ts = Base.List.concat_map ~f:(flatten cx seen) ts
  and flatten cx seen = function
    | UnionT (_, rep) -> union_flatten cx seen (UnionRep.members rep)
    | MaybeT (r, t) -> DefT (r, NullT) :: DefT (r, VoidT) :: flatten cx seen t
    | OptionalT { reason = r; type_ = t; use_desc } ->
      let void_t = VoidT.why_with_use_desc ~use_desc r in
      void_t :: flatten cx seen t
    | t -> [t]
  in
  fun cx f t r rep ->
    let (t0, (t1, ts)) = UnionRep.members_nel rep in
    let t0_ = f t0 in
    let t1_ = f t1 in
    let ts_ = ListUtils.ident_map f ts in
    if t0_ == t0 && t1_ == t1 && ts_ == ts then
      t
    else
      let ts = union_flatten cx ISet.empty (t0_ :: t1_ :: ts_) in
      let (ts, _) =
        Base.List.fold_left ts ~init:([], TypeSet.empty) ~f:(fun (acc, seen) t ->
            if TypeSet.mem t seen then
              (acc, seen)
            else
              (t :: acc, TypeSet.add t seen)
        )
      in
      TypeUtil.union_of_ts r (List.rev ts)

let substituter =
  object (self)
    inherit [replacement Subst_name.Map.t * bool * bool * use_op option] Type_mapper.t as super

    val mutable change_id = false

    (* Objects store a list of reachable targs and their polarity so that we
     * can do any-propagation without exploring the full structure of the
     * type. *)
    val mutable obj_reachable_targs = None

    method tvar _cx _map_cx _r id = id

    method call_prop cx map_cx id =
      let t = Context.find_call cx id in
      let t' = self#type_ cx map_cx t in
      if t == t' then
        id
      else
        Context.make_call_prop cx t'

    method props cx map_cx id =
      let props_map = Context.find_props cx id in
      let props_map' =
        NameUtils.Map.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props_map
      in
      let id' =
        if props_map == props_map' then
          id
        (* When substitution results in a new property map, we have to use a
           generated id, rather than a location from source. The substituted
           object will have the same location as the generic version, meaning
           that this location will not serve as a unique identifier. *)
        else
          Context.generate_property_map cx props_map'
      in
      id'

    method exports cx map_cx id =
      let exps = Context.find_exports cx id in
      let map_named_symbol ({ name_loc; preferred_def_locs; type_ } as orig) =
        let type_' = self#type_ cx map_cx type_ in
        if type_ == type_' then
          orig
        else
          { name_loc; preferred_def_locs; type_ = type_' }
      in
      let exps' = NameUtils.Map.ident_map map_named_symbol exps in
      if exps == exps' then
        id
      else
        Context.make_export_map cx exps'

    method! type_ cx map_cx t =
      let (map, force, placeholder_no_infer, use_op) = map_cx in
      if Subst_name.Map.is_empty map then
        t
      else
        let t_out =
          match t with
          | GenericT { name = Subst_name.Synthetic { name; op_kind = _; ts = ids }; reason; _ } ->
            if Base.List.exists ~f:(fun name -> Subst_name.Map.mem name map) ids then
              failwith
                (Utils_js.spf
                   "Cannot substitute through synthetic name %s at %s."
                   name
                   (Debug_js.dump_reason cx reason)
                )
            else
              super#type_ cx map_cx t
          | GenericT ({ reason = tp_reason; name; no_infer; _ } as gen) ->
            let annot_loc = loc_of_reason tp_reason in
            begin
              match Subst_name.Map.find_opt name map with
              | None -> super#type_ cx map_cx t
              | Some (TypeSubst (param_t, _)) ->
                change_id <- true;
                if placeholder_no_infer && no_infer then
                  Context.mk_placeholder cx (TypeUtil.reason_of_t param_t)
                else (
                  match (obj_reachable_targs, param_t) with
                  | (_, GenericT _) ->
                    mod_reason_of_t
                      (fun param_reason ->
                        annot_reason ~annot_loc @@ repos_reason annot_loc param_reason)
                      param_t
                  | (Some targs, OpenT _) ->
                    obj_reachable_targs <- Some ((param_t, Polarity.Neutral) :: targs);
                    param_t
                  | _ -> param_t
                )
              | Some (AlphaRename name') ->
                let t = GenericT { gen with name = name' } in
                super#type_ cx map_cx t
            end
          | DefT (reason, PolyT { tparams_loc; tparams = xs; t_out = inner; id }) ->
            let prev_change_id = change_id in
            change_id <- false;
            let (xs, map, changed) =
              Nel.fold_left
                (fun (xs, map, changed) typeparam ->
                  let bound =
                    self#type_ cx (map, force, placeholder_no_infer, use_op) typeparam.Type.bound
                  in
                  let default =
                    match typeparam.default with
                    | None -> None
                    | Some default ->
                      let default_ =
                        self#type_ cx (map, force, placeholder_no_infer, use_op) default
                      in
                      if default_ == default then
                        typeparam.default
                      else
                        Some default_
                  in
                  let (name, map) = avoid_capture map typeparam.name in
                  ( { typeparam with bound; default; name } :: xs,
                    map,
                    changed || bound != typeparam.bound || default != typeparam.default
                  ))
                ([], map, false)
                xs
            in
            let xs = xs |> List.rev |> Nel.of_list in
            let xs = Base.Option.value_exn xs in
            let inner_ = self#type_ cx (map, false, placeholder_no_infer, None) inner in
            let changed = changed || inner_ != inner in
            let id =
              if change_id then
                Type.Poly.generate_id ()
              else
                id
            in
            change_id <- prev_change_id || change_id;
            if changed then
              DefT (reason, PolyT { tparams_loc; tparams = xs; t_out = inner_; id })
            else
              t
          | ThisInstanceT (r, this, i, this_name) ->
            let (name, map) = avoid_capture map this_name in
            let this_ = self#instance_type cx (map, force, placeholder_no_infer, use_op) this in
            if this_ == this && name == this_name then
              t
            else
              ThisInstanceT (r, this_, i, name)
          | TypeAppT { reason; use_op = op; type_; targs; from_value; use_desc } ->
            let type_' = self#type_ cx map_cx type_ in
            let targs' = ListUtils.ident_map (self#type_ cx map_cx) targs in
            if type_' == type_ && targs' == targs then
              t
            else
              (* If the TypeAppT changed then one of the type arguments had a
               * GenericT that was substituted. In this case, also change the use_op
               * so we can point at the op which instantiated the types that
               * were substituted. *)
              let use_op = Base.Option.value use_op ~default:op in
              TypeAppT { reason; use_op; type_ = type_'; targs = targs'; from_value; use_desc }
          | EvalT (x, TypeDestructorT (op, r, d), _) ->
            let x' = self#type_ cx map_cx x in
            let d' = self#destructor cx map_cx d in
            if x == x' && d == d' then
              t
            else
              (* If the EvalT changed then either the target or destructor had a
               * GenericT that was substituted. In this case, also change the use_op
               * so we can point at the op which instantiated the types that
               * were substituted. *)
              let use_op = Base.Option.value use_op ~default:op in
              Flow_cache.Eval.id cx x' (TypeDestructorT (use_op, r, d'))
          | ModuleT _
          | InternalT (ExtendsT _) ->
            failwith (Utils_js.spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)
          | UnionT (r, urep) -> union_ident_map_and_dedup cx (self#type_ cx map_cx) t r urep
          | t -> super#type_ cx map_cx t
        in
        if t == t_out then
          t
        else
          match Reason.desc_of_reason ~unwrap:false (reason_of_t t_out) with
          | Reason.RTypeAlias (name, Some _, d) ->
            let desc = Reason.RTypeAlias (name, None, d) in
            mod_reason_of_t (Reason.replace_desc_reason desc) t_out
          | _ -> t_out

    method! predicate cx (map, force, placeholder_no_infer, use_op) p =
      match p with
      | LatentP _ -> super#predicate cx (map, force, placeholder_no_infer, use_op) p
      | p -> p

    method! obj_type cx map_cx t =
      let old_obj_reachable_targs = obj_reachable_targs in
      obj_reachable_targs <- Some [];
      let t' = super#obj_type cx map_cx t in
      let reachable_targs =
        match obj_reachable_targs with
        | Some ts -> ts
        | _ -> failwith "Invariant violation: reachable targs are None after visiting an object"
      in
      obj_reachable_targs <-
        (match old_obj_reachable_targs with
        | Some targs -> Some (targs @ reachable_targs)
        | None -> None);

      if reachable_targs == t'.reachable_targs then
        t'
      else
        { t' with reachable_targs }

    method private distributive_tparam_name name map =
      match name with
      | None -> (name, map)
      | Some name ->
        let (name, map) = avoid_capture map name in
        (Some name, map)

    method! destructor cx map_cx t =
      let (map, _, placeholder_no_infer, _) = map_cx in
      match t with
      | ConditionalType { distributive_tparam_name; infer_tparams; extends_t; true_t; false_t } ->
        let (distributive_tparam_name, map) =
          self#distributive_tparam_name distributive_tparam_name map
        in
        let false_t' = self#type_ cx (map, false, placeholder_no_infer, None) false_t in
        let (tparams_rev, map, changed) =
          Base.List.fold
            infer_tparams
            ~init:([], map, false)
            ~f:(fun (xs, map, changed) typeparam ->
              let bound =
                self#type_ cx (map, false, placeholder_no_infer, None) typeparam.Type.bound
              in
              let (name, map) = avoid_capture map typeparam.name in
              ({ typeparam with bound; name } :: xs, map, changed || bound != typeparam.bound)
          )
        in
        let extends_t' = self#type_ cx (map, false, placeholder_no_infer, None) extends_t in
        let true_t' = self#type_ cx (map, false, placeholder_no_infer, None) true_t in
        if changed || extends_t != extends_t' || true_t != true_t' || false_t != false_t' then
          ConditionalType
            {
              distributive_tparam_name;
              infer_tparams = List.rev tparams_rev;
              extends_t = extends_t';
              true_t = true_t';
              false_t = false_t';
            }
        else
          t
      | MappedType { distributive_tparam_name; property_type; mapped_type_flags; homomorphic } ->
        let (distributive_tparam_name, map) =
          self#distributive_tparam_name distributive_tparam_name map
        in
        let property_type' = self#type_ cx (map, false, placeholder_no_infer, None) property_type in
        let homomorphic' =
          match homomorphic with
          | SemiHomomorphic t ->
            let t' = self#type_ cx (map, false, placeholder_no_infer, None) t in
            if t' == t then
              homomorphic
            else
              SemiHomomorphic t'
          | Homomorphic
          | Unspecialized ->
            homomorphic
        in
        if property_type' == property_type && homomorphic' == homomorphic then
          t
        else
          MappedType
            {
              distributive_tparam_name;
              property_type = property_type';
              mapped_type_flags;
              homomorphic = homomorphic';
            }
      | _ -> super#destructor cx map_cx t

    (* The EvalT case is the only case that calls this function. We've explicitly overrided it
     * in all cases, so this should never be called *)
    method eval_id _cx _map_cx _id = assert false
  end

let subst cx ?use_op ?(force = true) ?(placeholder_no_infer = false) map ty =
  let map = Subst_name.Map.map (fun t -> TypeSubst (t, free_var_finder cx t)) map in
  substituter#type_ cx (map, force, placeholder_no_infer, use_op) ty

let subst_destructor cx ?use_op ?(force = true) ?(placeholder_no_infer = false) map des =
  let map = Subst_name.Map.map (fun t -> TypeSubst (t, free_var_finder cx t)) map in
  substituter#destructor cx (map, force, placeholder_no_infer, use_op) des

let subst_instance_type cx ?use_op ?(force = true) ?(placeholder_no_infer = false) map instance_t =
  let map = Subst_name.Map.map (fun t -> TypeSubst (t, free_var_finder cx t)) map in
  substituter#instance_type cx (map, force, placeholder_no_infer, use_op) instance_t
