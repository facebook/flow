(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
(*****************)
(* substitutions *)
(*****************)

(** Substitute bound type variables with associated types in a type. Do not
    force substitution under polymorphic types. This ensures that existential
    type variables under a polymorphic type remain unevaluated until the
    polymorphic type is applied. **)
let substituter = object(self)
  inherit [Type.t SMap.t * bool * use_op option] Type_mapper.t_with_uses as super

  method tvar _cx _map_cx _r id = id

  method call_prop cx map_cx id =
    let t = Context.find_call cx id in
    let t' = self#type_ cx map_cx t in
    if t == t' then id else Context.make_call_prop cx t'

  method props cx map_cx id =
    let props_map = Context.find_props cx id in
    let props_map' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props_map in
    let id' = if props_map == props_map' then id
      (* When substitution results in a new property map, we have to use a
         generated id, rather than a location from source. The substituted
         object will have the same location as the generic version, meaning
         that this location will not serve as a unique identifier. *)
      else Context.generate_property_map cx props_map' in
    id'

  method exports cx map_cx id =
    let exps = Context.find_exports cx id in
    let map_loc_type_pair ((loc, t) as orig) =
      let t' = self#type_ cx map_cx t in
      if t == t' then orig else (loc, t')
    in
    let exps' = SMap.ident_map map_loc_type_pair exps in
    if exps == exps' then id
    else Context.make_export_map cx exps'

  method! type_ cx map_cx t =
    let (map, force, use_op) = map_cx in
    if SMap.is_empty map then t
    else
    let t_out = match t with
      | BoundT (tp_reason, name, _) ->
        begin match SMap.get name map with
        | None -> t
        | Some (ReposT (_, param_t)) when name = "this" ->
          ReposT (annot_reason tp_reason, param_t)
        | Some param_t when name = "this" ->
          ReposT (annot_reason tp_reason, param_t)
        | Some param_t ->
          (match desc_of_reason ~unwrap:false (reason_of_t param_t) with
          | RPolyTest _ ->
            mod_reason_of_t (fun reason ->
              annot_reason (repos_reason (aloc_of_reason tp_reason) reason)
            ) param_t
          | _ ->
            param_t
          )
        end

      | ExistsT reason ->
        if force then Tvar.mk cx reason
        else t

      | DefT (reason, trust, PolyT (tparams_loc, xs, inner, _)) ->
        let xs, map, changed = Nel.fold_left (fun (xs, map, changed) typeparam ->
          let bound = self#type_ cx (map, force, use_op) typeparam.bound in
          let default = match typeparam.default with
          | None -> None
          | Some default ->
            let default_ = self#type_ cx (map, force, use_op) default in
            if default_ == default then typeparam.default else Some default_
          in
          { typeparam with bound; default; }::xs,
          SMap.remove typeparam.name map,
          changed || bound != typeparam.bound || default != typeparam.default
        ) ([], map, false) xs in
        let xs = xs |> List.rev |> Nel.of_list in
        (* The constructed list will always be nonempty because we fold over a nonempty list and add
         * an element to the resulting list for every element in the original list. It's just a bit
         * tricky to show this by construction while preserving the exact semantics of the above code.
         *)
        let xs = Option.value_exn xs in
        let inner_ = self#type_ cx (map, false, None) inner in
        let changed = changed || inner_ != inner in
        if changed then DefT (reason, trust, PolyT (tparams_loc, xs, inner_, Context.make_nominal cx)) else t

      | ThisClassT (reason, this) ->
        let map = SMap.remove "this" map in
        let this_ = self#type_ cx (map, force, use_op) this in
        if this_ == this then t else ThisClassT (reason, this_)

      | TypeAppT (r, op, c, ts) ->
        let c' = self#type_ cx map_cx c in
        let ts' = ListUtils.ident_map (self#type_ cx map_cx) ts in
        if c == c' && ts == ts' then t else (
          (* If the TypeAppT changed then one of the type arguments had a
           * BoundT that was substituted. In this case, also change the use_op
           * so we can point at the op which instantiated the types that
           * were substituted. *)
          let use_op = Option.value use_op ~default:op in
          TypeAppT (r, use_op, c', ts')
        )

      | EvalT (x, TypeDestructorT (op, r, d), _) ->
        let x' = self#type_ cx map_cx x in
        let d' = self#destructor cx map_cx d in
        if x == x' && d == d' then t
        else (
          (* If the EvalT changed then either the target or destructor had a
           * BoundT that was substituted. In this case, also change the use_op
           * so we can point at the op which instantiated the types that
           * were substituted. *)
          let use_op = Option.value use_op ~default:op in
          EvalT (x', TypeDestructorT (use_op, r, d'), Reason.mk_id ())
        )

      (* We only want to change the EvalT id if the rest of the EvalT actually changed *)
      | EvalT (t', dt, _id) ->
          let t'' = self#type_ cx map_cx t' in
          let dt' = self#defer_use_type cx map_cx dt in
          if t' == t'' && dt == dt' then t
          else EvalT (t'', dt', Reason.mk_id ())

      | ModuleT _
      | InternalT (ExtendsT _)
        ->
          failwith (Utils_js.spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)

      | t -> super#type_ cx map_cx t
    in
    if t == t_out then t else
    match Reason.desc_of_reason ~unwrap:false (reason_of_t t_out) with
    | Reason.RTypeAlias (name, true, d) ->
      let desc = Reason.RTypeAlias (name, false, d) in
      mod_reason_of_t (Reason.replace_desc_reason desc) t_out
    | _ -> t_out

  method! predicate cx (map, force, use_op) p = match p with
  | LatentP (t, i) ->
    let t' = self#type_ cx (map, force, use_op) t in
    if t == t' then p else LatentP (t', i)
  | p -> p

  (* The EvalT case is the only case that calls this function. We've explicitly overrided it
   * in all cases, so this should never be called *)
  method eval_id _cx _map_cx _id = assert false
end

let subst cx ?use_op ?(force=true) map =
    substituter#type_ cx (map, force, use_op)
