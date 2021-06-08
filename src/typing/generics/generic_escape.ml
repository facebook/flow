(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

type acc = {
  seen: ISet.t;
  tvar_ids: reason option IMap.t;
  top_level_reason: reason;
  annot_reason: reason option;
  scope_id: Scope_id.t;
  use_op: use_op;
}

class escape_finder ~gcx ~(add_output : Context.t -> ?trace:Type.trace -> Error_message.t -> unit) =
  object (self)
    inherit [acc] Type_visitor.t as super

    method scan_for_generics cx tvar_ids ~top_level_reason ~annot_reason scope_id use_op seen ty =
      let { seen; _ } =
        self#type_
          cx
          Polarity.Positive
          { seen; tvar_ids; top_level_reason; annot_reason; scope_id; use_op }
          ty
      in
      seen

    method! type_
        cx
        pole
        ({ seen = _; top_level_reason; annot_reason; scope_id; use_op; tvar_ids = _ } as acc)
        ty =
      match (ty, ALoc.source (def_aloc_of_reason (reason_of_t ty))) with
      | (GenericT { id; _ }, _) ->
        let err_fn id name acc =
          let is_escaped = not @@ Generic_cx.in_scope gcx scope_id id in
          if is_escaped then
            add_output
              cx
              (Error_message.EEscapedGeneric
                 {
                   reason = top_level_reason;
                   blame_reason = reason_of_t ty;
                   annot_reason;
                   use_op;
                   bound_name = name;
                   bound_loc = (id :> ALoc.t);
                   is_this = name = "this";
                 });
          is_escaped || acc
        in
        if Generic.fold_ids ~f:err_fn ~acc:false id then
          acc
        else
          super#type_ cx pole acc ty
      | (DefT (_, _, InstanceT (_, _, _, { type_args; _ })), Some key) when key <> Context.file cx
        ->
        (* It's really expensive to explore imported instances, and it's usually not necessary to do
               so. A nominal type like an instance, defined in a different module, should not be able to
               both contain, and have accessible, a type variable native to the importing module
               (at least under types-first). The exception is of course type parameters--an external class
               can have its type parameters instantiated (implicitly or explicitly) by a local generic.
               To catch such cases, we search the type args of non-local instances, but nothing else. *)
        let acc =
          List.fold_left
            (fun acc (_, _, t, pole') -> self#type_ cx (Polarity.mult (pole, pole')) acc t)
            acc
            type_args
        in
        acc
      | _ -> super#type_ cx pole acc ty

    method! use_type_ _cx acc _ty = acc

    (* Don't check evaluated types: the evaluated type in an EvalT theoretically "leaks"
       generic information that shouldn't actually cause issues *)
    method! eval_id _ _ acc _ = acc

    method! props cx pole acc id =
      NameUtils.Map.fold
        (fun name t acc ->
          if Signature_utils.is_munged_property_name name && Context.should_munge_underscores cx
          then
            acc
          else
            self#prop cx pole acc t)
        (Context.find_props cx id)
        acc

    method! fun_type cx pole acc ft =
      let { this_t = (this, subtyping); _ } = ft in
      (* Don't check this_t. It's hard to see how a generic `this_t` escaping could lead to problems,
         and without the ability to annotate the `this_t` type of a function, any errors that arose would
         be difficult to fix *)
      super#fun_type
        cx
        pole
        acc
        {
          ft with
          this_t = (VoidT.at (aloc_of_reason @@ reason_of_t this) (literal_trust ()), subtyping);
        }

    method! tvar cx pole ({ seen; tvar_ids; annot_reason; use_op; top_level_reason; _ } as acc) r id
        =
      let open Constraint in
      let (root_id, (lazy constraints)) = Context.find_constraints cx id in
      if id != root_id then
        self#tvar cx pole acc r root_id
      else if IMap.mem id tvar_ids then
        acc
      else if ISet.mem id seen then
        acc
      else
        let visit t (_, use_op') seen =
          let acc =
            (* Show the use_op, reason, and annotation site most local to the actual escape *)
            let (use_op', top_level_reason) =
              match root_of_use_op use_op' with
              | UnknownUse -> (use_op, top_level_reason)
              | _ -> (use_op', reason_of_t t)
            in
            let annot_reason' =
              match Generic_cx.get_id_annotation_reason gcx id with
              | Some _ as annot_reason' -> annot_reason'
              | _ -> annot_reason
            in
            if annot_reason' == annot_reason && use_op' == use_op then
              { acc with seen }
            else
              { acc with annot_reason = annot_reason'; use_op = use_op'; top_level_reason; seen }
          in
          (* only accumulate the seen ids, not other parts of the accumulator *)
          let { seen; _ } = self#type_ cx pole acc t in
          seen
        in
        let seen = ISet.add id seen in
        let seen =
          match constraints with
          | FullyResolved _ -> seen
          | Resolved (use_op, t) -> visit t (Trace.dummy_trace, use_op) seen
          | Unresolved { lower; _ } -> TypeMap.fold visit lower seen
        in
        { acc with seen }
  end

let scan_for_escapes
    cx ~(add_output : Context.t -> ?trace:Type.trace -> Error_message.t -> unit) ast =
  let scan_for_escapes_in_scope escape_finder scope_id scoped_tvars =
    let scan_tvar var_id annot_reason seen =
      let scan_type ty (_, use_op) seen =
        escape_finder#scan_for_generics
          cx
          scoped_tvars
          ~top_level_reason:(reason_of_t ty)
          ~annot_reason
          scope_id
          use_op
          seen
          ty
      in
      let open Constraint in
      if ISet.mem var_id seen then
        seen
      else
        let seen = ISet.add var_id seen in
        match Lazy.force (Context.find_graph cx var_id) with
        | Unresolved bounds -> Type.TypeMap.fold scan_type bounds.lower seen
        | Resolved (use_op, ty) -> scan_type ty (Trace.dummy_trace, use_op) seen
        | FullyResolved _ -> seen
    in
    ignore (IMap.fold scan_tvar scoped_tvars ISet.empty)
  in
  let finder = new Scoped_tvar_finder.finder cx in
  let gcx = finder#exec ast in
  let escape_finder = new escape_finder ~add_output ~gcx in
  Scope_id.ScopeMap.iter (scan_for_escapes_in_scope escape_finder) (Generic_cx.get_marked_ids gcx)
