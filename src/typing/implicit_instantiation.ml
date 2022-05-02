(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Polarity
module TypeParamMarked = Marked.Make (Subst_name)
module Marked = TypeParamMarked
module Check = Context.Implicit_instantiation_check

let reduce_implicit_instantiation_check reducer cx pole check =
  let { Check.lhs; poly_t = (loc, tparams, t); operation = (use_op, reason_op, op) } = check in
  let lhs' = reducer#type_ cx pole lhs in
  let tparams' = Nel.ident_map (reducer#type_param cx pole) tparams in
  let t' = reducer#type_ cx pole t in
  let op' =
    match op with
    | Check.Call calltype -> Check.Call (reducer#fun_call_type cx pole calltype)
    | Check.Constructor args ->
      Check.Constructor (ListUtils.ident_map (reducer#call_arg cx pole) args)
  in
  { Check.lhs = lhs'; poly_t = (loc, tparams', t'); operation = (use_op, reason_op, op') }

module type OBSERVER = sig
  type output

  val on_constant_tparam : Context.t -> Subst_name.t -> output

  val on_pinned_tparam : Context.t -> Subst_name.t -> Type.t -> output

  val on_missing_bounds :
    Context.t ->
    Subst_name.t ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output

  val on_upper_non_t :
    Context.t ->
    Subst_name.t ->
    Type.use_t ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output
end

module type KIT = sig
  type output

  val fold :
    Context.t ->
    Context.master_context ->
    f:(Context.t -> 'acc -> Check.t -> output Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(init_cx:Context.t -> cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (Observer : OBSERVER) : KIT with type output = Observer.output = struct
  type output = Observer.output

  let get_t cx =
    let no_lowers _cx r = Type.Unsoundness.merged_any r in
    function
    | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
    | t -> t

  (* This visitor records the polarities at which BoundTs are found. We follow the bounds of each
   * type parameter as well, since some type params are only used in the bounds of another.
   *)
  class implicit_instantiation_visitor ~bounds_map =
    object (self)
      inherit [Marked.t * Subst_name.Set.t] Type_visitor.t as super

      method! type_ cx pole ((marked, tparam_names) as acc) =
        function
        | GenericT { name = s; _ } as t ->
          if Subst_name.Set.mem s tparam_names then
            match Marked.add s pole marked with
            | None -> acc
            | Some (_, marked) ->
              (match Subst_name.Map.find_opt s bounds_map with
              | None -> (marked, tparam_names)
              | Some t -> self#type_ cx pole (marked, tparam_names) t)
          else
            super#type_ cx pole acc t
        (* We remove any tparam names from the map when entering a PolyT to avoid naming conflicts. *)
        | DefT (_, _, PolyT { tparams; t_out = t; _ }) ->
          let tparam_names' =
            Nel.fold_left (fun names x -> Subst_name.Set.remove x.name names) tparam_names tparams
          in
          let (marked, _) = self#type_ cx pole (marked, tparam_names') t in
          (* TODO(jmbrown): Handle defaults on type parameters *)
          (marked, tparam_names)
        | TypeAppT (_, _, c, ts) -> self#typeapp ts cx pole acc c
        (* ThisTypeAppT is created from a new expression, which cannot
         * be used as an annotation, so we do not special case it like
         * we do with TypeAppT
         *)
        | t -> super#type_ cx pole acc t

      method private typeapp =
        let rec loop cx pole seen = function
          (* Any arity erors are already handled in Flow_js *)
          | (_, []) -> seen
          | (Some [], _) -> seen
          | (None, targ :: targs) ->
            (* In the absence of tparams we will just visit the args with a
             * neutral polarity. *)
            let param_polarity = Polarity.Neutral in
            let seen = self#type_ cx param_polarity seen targ in
            loop cx pole seen (None, targs)
          | (Some (tparam :: tparams), targ :: targs) ->
            let param_polarity = Polarity.mult (pole, tparam.polarity) in
            let seen = self#type_ cx param_polarity seen targ in
            loop cx pole seen (Some tparams, targs)
        in
        fun targs cx pole acc t ->
          match get_t cx t with
          | AnnotT (_, t, _) -> self#typeapp targs cx pole acc t
          | DefT (_, _, PolyT { tparams; _ }) -> loop cx pole acc (Some (Nel.to_list tparams), targs)
          | DefT (_, _, EmptyT)
          | AnyT _ ->
            loop cx pole acc (None, targs)
          | t ->
            failwith
            @@ "Encountered a "
            ^ string_of_ctor t
            ^ " in typeapp case of fully constrained analysis"
    end

  type use_t_result =
    | UpperEmpty
    | UpperNonT of Type.use_t
    | UpperT of Type.t

  (* We never want to use the bound of the type variable in its inferred type. Instead, we will pin
   * the type and then check it against the bound. This prevents us from adding trivial `& bound` to
   * instantiations, and also prevents us from pinning to the bound when no actual upper bounds are added *)
  let t_not_bound t bound =
    if t = bound then
      UpperEmpty
    else
      UpperT t

  let t_of_use_t bound = function
    | UseT (_, t) -> t_not_bound t bound
    | u -> UpperNonT u

  let merge_upper_bounds upper_r bound cx = function
    | OpenT (_, id) ->
      let constraints = Context.find_graph cx id in
      (match constraints with
      | Constraint.FullyResolved (_, (lazy t))
      | Constraint.Resolved (_, t) ->
        t_not_bound t bound
      | Constraint.Unresolved bounds ->
        let uppers = Constraint.UseTypeMap.keys bounds.Constraint.upper in
        (match uppers with
        | [] -> UpperEmpty
        | [(t, _)] -> t_of_use_t bound t
        | ts ->
          ts
          |> List.fold_left
               (fun acc (t, _) ->
                 match (acc, t_of_use_t bound t) with
                 | (UpperNonT u, _) -> UpperNonT u
                 | (_, UpperNonT u) -> UpperNonT u
                 | (UpperEmpty, UpperT t) -> UpperT t
                 | (UpperT t', UpperT t) ->
                   (match (t', t) with
                   | (IntersectionT (_, rep1), IntersectionT (_, rep2)) ->
                     UpperT (IntersectionT (upper_r, InterRep.append (InterRep.members rep2) rep1))
                   | (_, IntersectionT (_, rep)) ->
                     UpperT (IntersectionT (upper_r, InterRep.append [t'] rep))
                   | (IntersectionT (_, rep), _) ->
                     UpperT (IntersectionT (upper_r, InterRep.append [t] rep))
                   | (t', t) -> UpperT (IntersectionT (upper_r, InterRep.make t' t [])))
                 | (UpperT _, UpperEmpty) -> acc
                 | (UpperEmpty, UpperEmpty) -> acc)
               UpperEmpty))
    | _ -> failwith "Implicit instantiation is not an OpenT"

  let merge_lower_bounds cx t =
    match t with
    | OpenT (_, id) ->
      let constraints = Context.find_graph cx id in
      (match constraints with
      | Constraint.FullyResolved (_, (lazy t))
      | Constraint.Resolved (_, t) ->
        Some t
      | Constraint.Unresolved bounds ->
        let lowers = bounds.Constraint.lower in
        if TypeMap.cardinal lowers = 0 then
          None
        else
          Some (get_t cx t))
    | _ -> failwith "Implicit instantiation is not an OpenT"

  let check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation =
    let { Check.lhs; operation = (use_op, reason_op, op); _ } = implicit_instantiation in
    let (call_targs, tparam_map) =
      List.fold_right
        (fun tparam (targs, map) ->
          let reason_tapp = TypeUtil.reason_of_t lhs in
          let targ =
            Instantiation_utils.ImplicitTypeArgument.mk_targ cx tparam reason_op reason_tapp
          in
          (ExplicitArg targ :: targs, Subst_name.Map.add tparam.name targ map))
        tparams
        ([], Subst_name.Map.empty)
    in
    let () =
      match op with
      | Check.Call calltype ->
        let new_tout = Tvar.mk_no_wrap cx reason_op in
        let call_t =
          CallT
            ( use_op,
              reason_op,
              { calltype with call_targs = Some call_targs; call_tout = (reason_op, new_tout) }
            )
        in
        Flow_js.flow cx (lhs, call_t)
      | Check.Constructor call_args ->
        let new_tout = Tvar.mk cx reason_op in
        let constructor_t =
          ConstructorT (use_op, reason_op, Some call_targs, call_args, new_tout)
        in
        Flow_js.flow cx (lhs, constructor_t)
    in
    (tparam_map, marked_tparams)

  let pin_types cx tparam_map marked_tparams bounds_map implicit_instantiation =
    let { Check.operation = (_, instantiation_reason, _); _ } = implicit_instantiation in
    let use_upper_bounds cx name tvar tparam_binder_reason instantiation_reason =
      let upper_t =
        merge_upper_bounds tparam_binder_reason (Subst_name.Map.find name bounds_map) cx tvar
      in
      match upper_t with
      | UpperEmpty -> Observer.on_missing_bounds cx name ~tparam_binder_reason ~instantiation_reason
      | UpperNonT u -> Observer.on_upper_non_t cx name ~tparam_binder_reason ~instantiation_reason u
      | UpperT t -> Observer.on_pinned_tparam cx name t
    in
    tparam_map
    |> Subst_name.Map.mapi (fun name t ->
           let tparam_binder_reason = TypeUtil.reason_of_t t in
           match Marked.get name marked_tparams with
           | None -> Observer.on_constant_tparam cx name
           | Some Neutral ->
             (* TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
              * to avoid cluttering the output we are actually interested in from this module,
              * I'm not going to start doing that until we need error diff information for
              * switching to Pierce's algorithm for implicit instantiation *)
             let lower_t = merge_lower_bounds cx t in
             (match lower_t with
             | None -> use_upper_bounds cx name t tparam_binder_reason instantiation_reason
             | Some _ -> Observer.on_pinned_tparam cx name t)
           | Some Positive ->
             let t = merge_lower_bounds cx t in
             (match t with
             | None ->
               Observer.on_missing_bounds cx name ~tparam_binder_reason ~instantiation_reason
             | Some t -> Observer.on_pinned_tparam cx name t)
           | Some Negative -> use_upper_bounds cx name t tparam_binder_reason instantiation_reason
       )

  let check_fun cx ~tparams ~bounds_map ~return_t ~implicit_instantiation =
    (* Visit the return type *)
    let visitor = new implicit_instantiation_visitor ~bounds_map in
    let tparam_names =
      tparams
      |> List.fold_left (fun set tparam -> Subst_name.Set.add tparam.name set) Subst_name.Set.empty
    in
    let (marked_tparams, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
    check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation

  let check_instance cx ~tparams ~implicit_instantiation =
    let marked_tparams =
      tparams
      |> List.fold_left
           (fun marked tparam ->
             match Marked.add tparam.name tparam.polarity marked with
             | None -> marked
             | Some (_, marked) -> marked)
           Marked.empty
    in
    check_instantiation cx ~tparams ~marked_tparams ~implicit_instantiation

  let implicitly_instantiate cx implicit_instantiation =
    let { Check.poly_t = (_, tparams, t); operation; _ } = implicit_instantiation in
    let tparams = Nel.to_list tparams in
    let bounds_map =
      List.fold_left
        (fun map x -> Subst_name.Map.add x.name x.bound map)
        Subst_name.Map.empty
        tparams
    in
    let (tparams_map, marked_tparams) =
      match get_t cx t with
      | DefT (_, _, FunT (_, funtype)) ->
        check_fun cx ~tparams ~bounds_map ~return_t:funtype.return_t ~implicit_instantiation
      | ThisClassT (_, DefT (_, _, InstanceT (_, _, _, _insttype)), _, _) ->
        (match operation with
        | (_, _, Check.Call _) ->
          (* This case is hit when calling a static function. We will implicitly
           * instantiate the type variables on the class, but using an instance's
           * type params in a static method does not make sense. We ignore this case
           * intentionally *)
          (Subst_name.Map.empty, Marked.empty)
        | (_, _, Check.Constructor _) -> check_instance cx ~tparams ~implicit_instantiation)
      | _ -> failwith "No other possible lower bounds"
    in
    (tparams_map, marked_tparams, bounds_map)

  let fold init_cx master_cx ~f ~init ~post implicit_instantiation_checks =
    let file = Context.file init_cx in
    let metadata = Context.metadata init_cx in
    let aloc_table = Utils_js.FilenameMap.find file (Context.aloc_tables init_cx) in
    let module_ref = Files.module_ref file in
    let ccx = Context.make_ccx master_cx in
    let cx =
      Context.make
        ccx
        metadata
        file
        aloc_table
        (Reason.OrdinaryName module_ref)
        Context.PostInference
    in
    let reducer =
      new Context_optimizer.context_optimizer ~no_lowers:(fun _ -> Unsoundness.merged_any)
    in
    let implicit_instantiation_checks =
      Base.List.map
        ~f:(reduce_implicit_instantiation_check reducer init_cx Polarity.Neutral)
        implicit_instantiation_checks
    in
    Context.merge_into
      ccx
      {
        Type.TypeContext.graph = reducer#get_reduced_graph;
        trust_graph = reducer#get_reduced_trust_graph;
        property_maps = reducer#get_reduced_property_maps;
        call_props = reducer#get_reduced_call_props;
        export_maps = reducer#get_reduced_export_maps;
        evaluated = reducer#get_reduced_evaluated;
      };

    let r =
      Base.List.fold_left
        ~f:(fun acc check ->
          let (tparams_map, marked_tparams, bounds_map) = implicitly_instantiate cx check in
          let pinned = pin_types cx tparams_map marked_tparams bounds_map check in
          f cx acc check pinned)
        ~init
        implicit_instantiation_checks
    in
    post ~init_cx ~cx;
    r
end
