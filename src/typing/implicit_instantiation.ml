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
        | AnyT _
        | MergedT _ ->
          loop cx pole acc (None, targs)
        | t ->
          failwith
          @@ "Encountered a "
          ^ string_of_ctor t
          ^ " in typepapp case of fully constrained analysis"
  end

let check_fun_call cx ~tparams ~params ?rest_param ~return_t ~f_params ~f_return =
  let tparams = Nel.to_list tparams in
  let (tparams_map, tparam_names) =
    List.fold_left
      (fun (map, names) x -> (SMap.add x.name x.bound map, SSet.add x.name names))
      (SMap.empty, SSet.empty)
      tparams
  in
  let visitor = new implicit_instantiation_visitor ~bounds_map:tparams_map in

  (* Visit params *)
  let (marked_params, _) =
    List.fold_left
      (fun acc (_, t) -> visitor#type_ cx Negative acc t)
      (Marked.empty, tparam_names)
      params
  in

  (* Visit rest param *)
  let (marked_params, _) =
    Base.Option.fold
      ~init:(marked_params, tparam_names)
      ~f:(fun map_cx (_, _, t) -> visitor#type_ cx Negative map_cx t)
      rest_param
  in

  (* Visit the return type *)
  let (marked_return, _) = visitor#type_ cx Positive (Marked.empty, tparam_names) return_t in
  tparams
  |> List.iter (fun tparam ->
         f_params tparam (Marked.get tparam.name marked_params);
         f_return tparam (Marked.get tparam.name marked_return))

let check_implicit_instantiation cx implicit_instantiation =
  let t = implicit_instantiation.Context.fun_or_class in
  let mk_error_msg tparam pole position =
    let pole_msg pole =
      match pole with
      | None -> "does not appear in the "
      | Some Positive -> "appears positively in the "
      | Some Neutral -> "appears neutrally in the "
      | Some Negative -> "appears negatively in the "
    in
    let msg = tparam.name ^ " " ^ pole_msg pole ^ position in
    Error_message.EImplicitInstantiationTemporaryError (Reason.aloc_of_reason tparam.reason, msg)
  in
  match t with
  | DefT (_, _, PolyT { t_out = t; tparams; _ }) ->
    (match get_t cx t with
    | DefT (_, _, FunT (_, _, funtype)) ->
      check_fun_call
        cx
        ~tparams
        ~params:funtype.params
        ?rest_param:funtype.rest_param
        ~return_t:funtype.return_t
        ~f_params:(fun tparam pole -> Flow_js.add_output cx (mk_error_msg tparam pole "params"))
        ~f_return:(fun tparam pole -> Flow_js.add_output cx (mk_error_msg tparam pole "return"))
    | _t -> ())
  | _t ->
    failwith "Implicit instantiation checks should always have a polymorphic class or function"
