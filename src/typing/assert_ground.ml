(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Constraint
open Reason
open Utils_js
open Type
module Marked = Marked.IdMarked
module FlowError = Flow_error

class type_finder t =
  object (_self)
    inherit [bool] Type_visitor.t as super

    method! type_ cx pole found =
      function
      | t' -> t = t' || super#type_ cx pole found t
  end

(* Given a type, report missing annotation errors if

   - the given type is a tvar whose id isn't explicitly specified in the given
   skip set, or isn't explicitly marked as derivable, or if

   - the tvar appears in a negative position

   Type variables that are in the skip set are marked in assume_ground as
   depending on `require`d modules. Thus, e.g., when the superclass of an
   exported class is `require`d, we should not insist on an annotation for the
   superclass.
*)
(* need to consider only "def" types *)

module Kit (Flow : Flow_common.S) : Flow_common.ASSERT_GROUND = struct
  include Flow

  class assert_ground_visitor r ~max_reasons ~should_munge_underscores =
    object (self)
      inherit [Marked.t] Type_visitor.t as super

      (* Track prop maps which correspond to object literals. We don't ask for
       annotations for object literals which reach exports. Instead, we walk the
       properties covariantly. *)
      val mutable objlits : int Properties.Map.t = Properties.Map.empty

      (* Track prop maps which correspond to instance fields and methods, indicating
       any fields which are initialized. We don't ask for annotations for (a)
       munged property names, which are private and thus not inputs, and (b)
       initialized field names. *)
      val mutable insts : (int * SSet.t) Properties.Map.t = Properties.Map.empty

      val depth = ref 0

      val reason_stack = ref (Nel.one r)

      method private push_frame r =
        incr depth;
        if max_reasons > 0 && Nel.length !reason_stack < max_reasons then (
          reason_stack := Nel.cons r !reason_stack;
          true
        ) else
          false

      method private pop_frame did_add =
        decr depth;
        if max_reasons > 0 && did_add then
          (* We start with a Nel and always add in push_frame, so the tail should always
           * be non-empty *)
          reason_stack := Nel.of_list_exn (Nel.tl !reason_stack)

      method private with_frame r f =
        let did_add = self#push_frame r in
        let result = f () in
        self#pop_frame did_add;
        result

      (* Tvars with reasons that match should not be missing annotation errors. *)
      method private skip_reason r =
        match desc_of_reason r with
        (* No possible annotation for `this` type. *)
        | RThis -> true
        (* Treat * as an annotation, even though it is inferred, because the
         resulting errors are confusing and have unpredictable locations. *-types
         are already deprecated, and this wart will go away entirely when we
         finally remove support. *)
        | RExistential -> true
        | _ -> false

      method private derivable_reason r =
        match desc_of_reason r with
        | RShadowProperty _ -> true
        | _ -> is_derivable_reason r

      method! tvar cx pole seen r id =
        let (root_id, constraints) = Context.find_constraints cx id in
        if id != root_id then
          self#tvar cx pole seen r root_id
        else if self#skip_reason r then
          seen
        else
          let pole =
            if self#derivable_reason r then
              Polarity.Positive
            else
              pole
          in
          (* TODO: clean up the match pole below. Visiting a tvar with a negative
           polarity will add an error and resolve the tvar to any. We don't need
           to also walk the positive edge of the tvar. This behavior is a bit
           different from what the Marked module provides, but treating negative
           as neutral gives the correct behavior. *)
          let marked_pole =
            match pole with
            | Polarity.Negative -> Polarity.Neutral
            | _ -> pole
          in
          match Marked.add id marked_pole seen with
          | None -> seen
          | Some (pole, seen) ->
            (match pole with
            | Polarity.Neutral
            | Polarity.Negative ->
              AnyT.locationless AnyError |> unify_opt cx ~unify_any:true (OpenT (r, id));
              let trace_reasons =
                if max_reasons = 0 then
                  []
                else
                  Base.List.map
                    ~f:(fun reason -> repos_reason (def_aloc_of_reason reason) reason)
                    (Nel.to_list !reason_stack)
              in
              add_output cx (Error_message.EMissingAnnotation (r, trace_reasons));
              seen
            | Polarity.Positive ->
              (match constraints with
              | FullyResolved _ ->
                (* A fully resolved node corresponds to either (a) a tvar imported
                 from a dependency, which has already gone through assert_ground
                 or (b) a tvar corresponding to an annotation in this file which
                 certainly does not contain unresolved tvars.

                 In either case, it is not necessary to visit the structure of the
                 resolved type, as we will not find anything to complain about. *)
                seen
              | Resolved (_, t) -> self#type_ cx Polarity.Positive seen t
              | Unresolved { lower; _ } ->
                TypeMap.fold (fun t _ seen -> self#type_ cx Polarity.Positive seen t) lower seen))

      method! type_ cx pole seen t =
        Option.iter
          ~f:(fun { Verbose.depth = verbose_depth; indent; enabled_during_flowlib = _ } ->
            let pid = Context.pid_prefix cx in
            let indent = String.make (!depth * indent) ' ' in
            prerr_endlinef
              "\n%s%sassert_ground (%s): %s"
              indent
              pid
              (Polarity.string pole)
              (Debug_js.dump_t cx ~depth:verbose_depth t))
          (Context.verbose cx);
        self#with_frame (reason_of_t t) (fun () ->
            let seen =
              match t with
              | BoundT _ -> seen
              | MergedT _ ->
                (* The base class implementation will walk uses here, but there's no
             reasonable way to complain about missing annotations for MergedT,
             which was added to avoid missing annotations. *)
                seen
              | ReposT (r, _) ->
                (* It's possible that we might encounter a substituted this type in a
                 * negative position. This is normally an error, but might be
                 * suppresesed or otherwise still present in the exports. If we
                 * encounter this, we should just ignore it. *)
                if desc_of_reason r = RThisType then
                  seen
                else
                  super#type_ cx pole seen t
              | EvalT (_, TypeDestructorT _, _) ->
                (* Type destructors are annotations, so we should never complain about
             missing annotations due them. The default visitor _should_ never
             visit a tvar in an input position, but do to some wacky stuff in
             eval, it's possible today. *)
                seen
              | KeysT _ ->
                (* Same idea as type destructors. *)
                seen
              | TypeAppT (_, _, c, ts) -> self#typeapp ts cx pole seen c
              | DefT (r, _, ArrT (ArrayAT (t, ts))) when is_literal_array_reason r ->
                self#arrlit cx pole seen t ts
              | DefT (r, _, ObjT o) when is_literal_object_reason r ->
                let refcnt = (try Properties.Map.find o.props_tmap objlits with Not_found -> 0) in
                objlits <- Properties.Map.add o.props_tmap (refcnt + 1) objlits;
                let seen = super#type_ cx pole seen t in
                objlits <-
                  ( if refcnt = 0 then
                    Properties.Map.remove o.props_tmap objlits
                  else
                    Properties.Map.add o.props_tmap refcnt objlits );
                seen
              | DefT (_, _, InstanceT (static, _, _, i)) ->
                let static_props_id =
                  match static with
                  | DefT (_, _, ObjT o) -> Some o.props_tmap
                  | _ -> None
                in
                let own_refcnt =
                  (try fst (Properties.Map.find i.own_props insts) with Not_found -> 0)
                in
                let proto_refcnt =
                  (try fst (Properties.Map.find i.proto_props insts) with Not_found -> 0)
                in
                let static_refcnt =
                  Option.value_map static_props_id ~default:0 ~f:(fun id ->
                      (try fst (Properties.Map.find id insts) with Not_found -> 0))
                in
                insts <- Properties.Map.add i.own_props (own_refcnt + 1, i.initialized_fields) insts;
                insts <- Properties.Map.add i.proto_props (proto_refcnt + 1, SSet.empty) insts;
                Option.iter static_props_id (fun id ->
                    insts <-
                      Properties.Map.add id (static_refcnt + 1, i.initialized_static_fields) insts);
                let seen = super#type_ cx pole seen t in
                insts <-
                  ( if own_refcnt = 0 then
                    Properties.Map.remove i.own_props insts
                  else
                    Properties.Map.add i.own_props (own_refcnt, i.initialized_fields) insts );
                insts <-
                  ( if proto_refcnt = 0 then
                    Properties.Map.remove i.proto_props insts
                  else
                    Properties.Map.add i.proto_props (own_refcnt, SSet.empty) insts );
                Option.iter static_props_id (fun id ->
                    insts <-
                      ( if static_refcnt = 0 then
                        Properties.Map.remove id insts
                      else
                        Properties.Map.add id (static_refcnt, i.initialized_static_fields) insts ));
                seen
              | DefT (r, _, FunT (static, prototype, ft)) ->
                (* This won't propagate to any other types because this happens post-merge *)
                let any kind = AnyT.locationless (Unsound kind) in
                any DummyStatic |> unify_opt cx ~unify_any:true static;
                any FunctionPrototype |> unify_opt cx ~unify_any:true prototype;
                any BoundFunctionThis |> unify_opt cx ~unify_any:true ft.this_t;
                super#type_
                  cx
                  pole
                  seen
                  (DefT
                     ( r,
                       bogus_trust (),
                       FunT
                         ( any DummyStatic,
                           any FunctionPrototype,
                           { ft with this_t = any BoundFunctionThis } ) ))
              | _ -> super#type_ cx pole seen t
            in
            seen)

      method! props cx pole seen id =
        if Properties.Map.mem id objlits then
          self#objlit_props cx pole seen id
        else
          match Properties.Map.find_opt id insts with
          | Some (_, init) -> self#inst_props cx pole seen id init
          | _ -> super#props cx pole seen id

      method private arrlit cx pole seen t ts =
        let seen = self#type_ cx pole seen t in
        let seen = Option.fold ts ~init:seen ~f:(List.fold_left (self#type_ cx pole)) in
        seen

      method private objlit_props cx pole seen id =
        let props = Context.find_props cx id in
        SMap.fold
          (fun _ p acc -> Property.read_t p |> Option.fold ~f:(self#type_ cx pole) ~init:acc)
          props
          seen

      method private inst_props cx pole seen id init =
        let props = Context.find_props cx id in
        SMap.fold
          (fun x p acc ->
            if is_munged_prop_name_with_munge x ~should_munge_underscores then
              acc
            else if SSet.mem x init then
              Property.read_t p |> Option.fold ~f:(self#type_ cx pole) ~init:acc
            else
              self#prop cx pole acc p)
          props
          seen

      method private typeapp =
        let rec loop ?constant_polarity_param cx pole seen = function
          | (_, []) -> seen
          | ([], _) -> seen
          | (tparam :: tparams, targ :: targs) ->
            let param_polarity =
              match constant_polarity_param with
              | Some (s, p) when tparam.name = s -> p
              | _ -> Polarity.mult (pole, tparam.polarity)
            in
            let seen = self#type_ cx param_polarity seen targ in
            loop cx pole seen (tparams, targs)
        in
        fun targs cx pole seen -> function
          | OpenT (r, id) ->
            let seen = self#tvar cx Polarity.Positive seen r id in
            (match Context.find_graph cx id with
            | Resolved (_, t)
            | FullyResolved (_, t) ->
              self#typeapp targs cx pole seen t
            | Unresolved { lower; _ } ->
              TypeMap.fold (fun t _ acc -> self#typeapp targs cx pole acc t) lower seen)
          | AnnotT (_, t, _) -> self#typeapp targs cx pole seen t
          (* Shallowly check to see if it is an EvalT. If the EvalT's first
           * value is a BoundT, we can visit that parameter with a constant
           * positive polarity if it does not appear in the defer_use_t.
           *)
          | DefT
              ( _,
                _,
                PolyT
                  {
                    tparams;
                    t_out =
                      DefT
                        ( _,
                          _,
                          TypeT
                            (_, EvalT ((BoundT (_, s) as t), TypeDestructorT (_, _, destructor), _))
                        );
                    _;
                  } ) ->
            if (new type_finder t)#destructor cx false destructor then
              loop cx pole seen (Nel.to_list tparams, targs)
            else
              loop
                cx
                pole
                seen
                (Nel.to_list tparams, targs)
                ~constant_polarity_param:(s, Polarity.Positive)
          | DefT (_, _, PolyT { tparams; _ }) -> loop cx pole seen (Nel.to_list tparams, targs)
          | DefT (_, _, EmptyT _) -> seen
          | AnyT _ -> seen
          | _ ->
            (* We don't error here on an unexpected typeapp because we would have already
             * caught that this type is not polymorphic earlier *)
            seen
    end

  let enforce_strict cx t ~should_munge_underscores =
    let visitor =
      new assert_ground_visitor
        (reason_of_t t)
        ~max_reasons:(Context.max_trace_depth cx)
        ~should_munge_underscores
    in
    let seen = visitor#type_ cx Polarity.Positive Marked.empty t in
    ignore (seen : Marked.t)
end
