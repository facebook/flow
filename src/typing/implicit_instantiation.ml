(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Polarity
module IdMarked = Marked.IdMarked
module TypeParamMarked = Marked.Make (StringKey)
module Marked = TypeParamMarked

let get_t cx =
  let no_lowers _cx r = Type.Unsoundness.merged_any r in
  function
  | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
  | t -> t

let print_type cx typed_ast file_sig =
  let options = { Ty_normalizer_env.default_options with Ty_normalizer_env.max_depth = Some 10 } in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig in
  fun t ->
    let ty = Ty_normalizer.from_type ~options ~genv t in
    match ty with
    | Error _ -> "ERROR PRINTING TYPE"
    | Ok ty -> Ty_printer.string_of_elt ~with_comments:false ty ~exact_by_default:true

(* This visitor records the polarities at which BoundTs are found. We follow the bounds of each
 * type parameter as well, since some type params are only used in the bounds of another.
 *)
class implicit_instantiation_visitor ~bounds_map =
  object (self)
    inherit [Marked.t * SSet.t] Type_visitor.t as super

    method! type_ cx pole ((marked, tparam_names) as acc) =
      function
      | BoundT (_, s) ->
        if SSet.mem s tparam_names then
          match Marked.add s pole marked with
          | None -> acc
          | Some (_, marked) ->
            (match SMap.find_opt s bounds_map with
            | None -> (marked, tparam_names)
            | Some t -> self#type_ cx pole (marked, tparam_names) t)
        else
          acc
      (* We remove any tparam names from the map when entering a PolyT to avoid naming conflicts. *)
      | DefT (_, _, PolyT { tparams; t_out = t; _ }) ->
        let tparam_names' =
          Nel.fold_left (fun names x -> SSet.remove x.name names) tparam_names tparams
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
    let (_, constraints) = Context.find_constraints cx id in
    (match constraints with
    | Constraint.FullyResolved (_, t)
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
    let (_, constraints) = Context.find_constraints cx id in
    (match constraints with
    | Constraint.FullyResolved (_, t)
    | Constraint.Resolved (_, t) ->
      Some t
    | Constraint.Unresolved bounds ->
      let lowers = bounds.Constraint.lower in
      if TypeMap.cardinal lowers = 0 then
        None
      else
        Some (get_t cx t))
  | _ -> failwith "Implicit instantiation is not an OpenT"

let mk_not_enough_info_msg tparam_name reason_call reason_l =
  Error_message.EImplicitInstantiationUnderconstrainedError
    { bound = tparam_name; reason_call; reason_l }

let mk_has_type_msg ~print_type tparam_name tparam_reason t =
  let msg = tparam_name ^ " is pinned to type " ^ print_type t in
  Error_message.EImplicitInstantiationTemporaryError (Reason.aloc_of_reason tparam_reason, msg)

let mk_non_upper_msg tparam_name tparam_reason u =
  let msg = tparam_name ^ " contains a non-Type.t upper bound " ^ string_of_use_ctor u in
  Error_message.EImplicitInstantiationTemporaryError (Reason.aloc_of_reason tparam_reason, msg)

let mk_polarity_log_msg tparam pole position =
  let pole_msg pole =
    match pole with
    | None -> "does not appear in the "
    | Some Positive -> "appears positively in the "
    | Some Neutral -> "appears neutrally in the "
    | Some Negative -> "appears negatively in the "
  in
  let msg = tparam.name ^ " " ^ pole_msg pole ^ position in
  Error_message.EImplicitInstantiationTemporaryError (Reason.aloc_of_reason tparam.reason, msg)

let check_instantiation
    cx ~bounds_map ~tparams ~marked_tparams ~implicit_instantiation ~print_type:_ =
  let (call_targs, tparam_map) =
    List.fold_right
      (fun tparam (targs, map) ->
        let reason_tapp = TypeUtil.reason_of_t implicit_instantiation.Context.fun_or_class in
        let targ =
          Instantiation_utils.ImplicitTypeArgument.mk_targ
            cx
            tparam
            (TypeUtil.reason_of_use_t implicit_instantiation.Context.call_or_constructor)
            reason_tapp
        in
        (ExplicitArg targ :: targs, SMap.add tparam.name targ map))
      tparams
      ([], SMap.empty)
  in
  (match implicit_instantiation.Context.call_or_constructor with
  | CallT (use_op, reason_op, calltype) ->
    let new_tout = Tvar.mk_no_wrap cx reason_op in
    let call_t =
      CallT
        ( use_op,
          reason_op,
          { calltype with call_targs = Some call_targs; call_tout = (reason_op, new_tout) } )
    in
    Flow_js.flow cx (implicit_instantiation.Context.fun_or_class, call_t)
  | ConstructorT (use_op, reason_op, _, call_args, _) ->
    let new_tout = Tvar.mk cx reason_op in
    let constructor_t = ConstructorT (use_op, reason_op, Some call_targs, call_args, new_tout) in
    Flow_js.flow cx (implicit_instantiation.Context.fun_or_class, constructor_t)
  | u -> failwith ("FunT ~> " ^ string_of_use_ctor u));
  let use_upper_bounds cx name tvar reason reason_u =
    let upper_t = merge_upper_bounds reason (SMap.find name bounds_map) cx tvar in
    match upper_t with
    | UpperEmpty -> Some (mk_not_enough_info_msg name reason_u reason)
    | UpperNonT u -> Some (mk_non_upper_msg name reason u)
    | UpperT _ -> None
  in
  tparam_map
  |> SMap.iter (fun name t ->
         let reason = TypeUtil.reason_of_t t in
         let reason_u =
           TypeUtil.reason_of_use_t implicit_instantiation.Context.call_or_constructor
         in
         let msg =
           match Marked.get name marked_tparams with
           | None ->
             (* TODO(jmbrown): For now, we don't need to infer implicit instantiations if the
              * type param doesn't appear in the return type. A future extension here
              * will still pin down types when that type is needed to check the body of an
              * unannotated lambda passed into a polymorphic function call *)
             None
           | Some Neutral ->
             (* TODO(jmbrown): The neutral case should also unify upper/lower bounds. In order
              * to avoid cluttering the output we are actually interested in from this module,
              * I'm not going to start doing that until we need error diff information for
              * switching to Pierce's algorithm for implicit instantiation *)
             let lower_t = merge_lower_bounds cx t in
             (match lower_t with
             | None -> use_upper_bounds cx name t reason reason_u
             | Some _ -> None)
           | Some Positive ->
             let t = merge_lower_bounds cx t in
             (match t with
             | None -> Some (mk_not_enough_info_msg name reason_u reason)
             | Some _ -> None)
           | Some Negative -> use_upper_bounds cx name t reason reason_u
         in
         match msg with
         | None -> ()
         | Some msg -> Flow_js.add_output cx msg)

let check_fun cx ~tparams ~bounds_map ~return_t ~implicit_instantiation ~print_type =
  (* Visit the return type *)
  let visitor = new implicit_instantiation_visitor ~bounds_map in
  let tparam_names =
    tparams |> List.fold_left (fun set tparam -> SSet.add tparam.name set) SSet.empty
  in
  let (marked_tparams, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
  check_instantiation cx ~bounds_map ~tparams ~marked_tparams ~implicit_instantiation ~print_type

let check_instance cx ~tparams ~bounds_map ~implicit_instantiation ~print_type =
  let marked_tparams =
    tparams
    |> List.fold_left
         (fun marked tparam ->
           match Marked.add tparam.name tparam.polarity marked with
           | None -> marked
           | Some (_, marked) -> marked)
         Marked.empty
  in
  check_instantiation cx ~bounds_map ~tparams ~marked_tparams ~implicit_instantiation ~print_type

let check_implicit_instantiation cx tast file_sig implicit_instantiation =
  let print_type = print_type cx tast file_sig in
  let t = implicit_instantiation.Context.fun_or_class in
  match t with
  | DefT (_, _, PolyT { t_out = t; tparams; _ }) ->
    let tparams = Nel.to_list tparams in
    let bounds_map = List.fold_left (fun map x -> SMap.add x.name x.bound map) SMap.empty tparams in
    (match get_t cx t with
    | DefT (_, _, FunT (_, _, funtype)) ->
      check_fun
        cx
        ~tparams
        ~bounds_map
        ~return_t:funtype.return_t
        ~implicit_instantiation
        ~print_type
    | ThisClassT (_, DefT (_, _, InstanceT (_, _, _, _insttype)), _) ->
      (match implicit_instantiation.Context.call_or_constructor with
      | CallT _ ->
        (* This case is hit when calling a static function. We will implicitly
         * instantiate the type variables on the class, but using an instance's
         * type params in a static method does not make sense. We ignore this case
         * intentionally *)
        ()
      | ConstructorT _ -> check_instance cx ~tparams ~bounds_map ~implicit_instantiation ~print_type
      | _ -> failwith "Not possible")
    | _ -> failwith "No other possible lower bounds")
  | _t ->
    failwith "Implicit instantiation checks should always have a polymorphic class or function"
