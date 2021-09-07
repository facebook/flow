(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Type.AConstraint

let warn fmt =
  let f s = Utils_js.prerr_endlinef "WARNING: %s" s in
  Printf.ksprintf f fmt

(* TODO lookup aloc in tables, e.g.
 * ALoc.to_loc_with_tables (Context.aloc_tables cx) (Reason.aloc_of_reason r) *)
let string_of_reason_loc _cx r = Reason.string_of_aloc (Reason.aloc_of_reason r)

let unresolved_tvar cx reason = Avar.unresolved cx reason

let get_fully_resolved_type cx id =
  let (_, node) = Context.find_root cx id in
  match Lazy.force node.Constraint.constraints with
  | Constraint.FullyResolved (_, (lazy t)) -> t
  | Constraint.Resolved _
  | Constraint.Unresolved _ ->
    failwith "unexpected unresolved constraint in annotation inference"

(** [ensure_annot_resolved cx reason id] ensures that the annotation constraint
 *  associated with [id] has been resolved. If the respective constraint is already
 *  resolved then it returns immediately. Otherwise, it resolves [id] immediately
 *  to the 'any' type. In the case of an [Anno_op (_, _, dep_id)] constraint we also
 *  update the "dependents" set of [dep_id], so that we don't attempt to resolve
 *  [id] once again when [dep_id] gets resolved.
 *)
let rec ensure_annot_resolved cx reason id =
  let module A = Type.AConstraint in
  match Context.find_avar cx id with
  | (_, { A.constraints = (lazy A.Annot_resolved); _ }) -> ()
  | (_, { A.constraints = (lazy (A.Annot_unresolved _)); _ }) ->
    warn "returning any on %s" (string_of_reason_loc cx reason);
    resolve_id cx id (Unsoundness.merged_any reason)
  | (root_id, { A.constraints = (lazy (A.Annot_op { id = dep_id; _ })); _ }) ->
    warn "returning any on %s" (string_of_reason_loc cx reason);
    let (_, { A.constraints = (lazy dep_constraint); _ }) = Context.find_avar cx dep_id in
    A.update_deps_of_constraint dep_constraint ~f:(fun deps ->
        ISet.filter
          (fun id2 ->
            let (root_id2, _) = Context.find_avar cx id2 in
            root_id <> root_id2)
          deps);
    resolve_id cx id (Unsoundness.merged_any reason)

and mk_lazy_tvar cx reason f =
  let id = Reason.mk_id () in
  let tvar = OpenT (reason, id) in
  let constraints =
    lazy
      (Avar.unresolved_with_id cx id reason;
       f id;
       (* Before forcing the type constraint of [id] we need to make sure the
        * respective annotation constraint has been processed. If not we infer
        * the empty type. *)
       ensure_annot_resolved cx reason id;
       Lazy.force (Context.find_graph cx id))
  in
  Context.add_tvar cx id (Constraint.Root { Constraint.rank = 0; constraints });
  tvar

and mk_sig_tvar cx reason (resolved : Type.t Lazy.t) =
  let f id =
    let t = Lazy.force resolved in
    resolve_id cx id t
  in
  mk_lazy_tvar cx reason f

(** [resolve_id cx id1 t] resolves an annotation tvar [id1] to a type [t] *
 *  - If [t] is a concrete type, we mark [id1] as a resolved annotation tvar and
 *    record it as fully resolved in the type graph. *
 *  - If [t] is an OpenT (_, id2), then we unify [id1] and [id2]. (See merge_ids.)
 *)
and resolve_id cx id t =
  let module A = Type.AConstraint in
  let module C = Type.Constraint in
  match t with
  | Type.OpenT (_, id2) -> merge_ids cx id id2
  | _ ->
    let (root_id1, root1) = Context.find_avar cx id in
    Context.add_avar cx root_id1 A.fully_resolved_node;
    Context.add_tvar cx root_id1 (C.fully_resolved_node t);
    let dependents1 = deps_of_constraint (Lazy.force root1.A.constraints) in
    resolve_dependent_set cx dependents1 t

(** Makes id1 a goto node to id2. It also appends depndents of id1 to those of id2.
 *  If id2 is a resolved node, then dependents can be immediately resolved using
 *  the resolved type of id2. *)
and goto cx id1 dependents1 (id2, root2) =
  let module A = Type.AConstraint in
  let module T = Type.Constraint in
  Context.add_tvar cx id1 (T.Goto id2);
  Context.add_avar cx id1 (A.Goto id2);
  match Lazy.force root2.A.constraints with
  | (A.Annot_op _ | A.Annot_unresolved _) as constraint_ ->
    update_deps_of_constraint ~f:(ISet.union dependents1) constraint_
  | A.Annot_resolved ->
    let t = get_fully_resolved_type cx id2 in
    resolve_dependent_set cx dependents1 t

(** Similar to Flow_js.merge_ids. Uses rank information to determine which one
 *  of [id1] and [id2] will become the goto node and which one the root. *)
and merge_ids cx id1 id2 =
  let module A = Type.AConstraint in
  let ((id1, root1), (id2, root2)) = (Context.find_avar cx id1, Context.find_avar cx id2) in
  if id1 = id2 then
    ()
  else if root1.A.rank < root2.A.rank then
    let deps1 = deps_of_constraint (Lazy.force root1.A.constraints) in
    goto cx id1 deps1 (id2, root2)
  else if root2.A.rank < root1.A.rank then
    let deps2 = deps_of_constraint (Lazy.force root2.A.constraints) in
    goto cx id2 deps2 (id1, root1)
  else (
    Context.add_avar cx id2 (A.Root { root2 with A.rank = root1.A.rank + 1 });
    let deps1 = deps_of_constraint (Lazy.force root1.A.constraints) in
    goto cx id1 deps1 (id2, root2)
  )

and resolve_dependent_set cx dependents t =
  Context.iter_annot_dependent_set cx (fun id op -> resolve_id cx id (elab_t cx t op)) dependents

and reposition _cx _loc _t = failwith "TODO Annotation_inference.reposition"

and elab_t _cx _t _op = failwith "TODO Annotation_inference.elab_t"

and specialize _cx _t _use_op _reason_op _reason_tapp _cache _ts =
  failwith "TODO Annotation_inference.specialize"

and mk_instance _cx _reason _t = failwith "TODO Annotation_inference.mk_instance"

and get_prop_internal _cx _use_op _loc _reason_op _propref _l =
  failwith "TODO Annotation_inference.get_prop"

and get_prop cx use_op loc reason name t = get_prop_internal cx use_op loc reason (reason, name) t

and get_elem _cx _use_op _reason ~key:_ _t = failwith "TODO Annotation_inference.get_elem"

and qualify_type cx use_op loc reason propref t = get_prop_internal cx use_op loc reason propref t

and mk_typeof_annotation _cx ?trace:_ _reason ?use_desc:_ ?internal:_ _t =
  failwith "TODO Annotation_inference.mk_typeof_annotation"

and assert_export_is_type _cx _reason _name _l =
  failwith "TODO Annotation_inference.assert_export_is_type"

and cjs_require _cx _l _reason _is_strict = failwith "TODO Annotation_inference.cjs_require"

and export_named _cx _reason _kind _named _t = failwith "TODO Annotation_inference.export_named"

and cjs_extract_named_exports _cx _reason _local_module _t =
  failwith "TODO Annotation_inference.cjs_extract_named_exports"

and import_typeof _cx _reason _export_t _export_name =
  failwith "TODO Annotation_inference.mk_import_type_of"

and import_default _cx _reason _import_kind _local_name _module_name _is_strict _l =
  failwith "TODO Annotation_inference.import_default"

and import_named _cx _reason _import_kind _export_name _module_name _is_strict _l =
  failwith "TODO Annotation_inference.import_named"

and import_ns _cx _reason _is_strict _l = failwith "TODO Annotation_inference.import_ns"

and copy_named_exports _cx ~from_ns:_ _reason ~module_t:_ =
  failwith "TODO Annotation_inference.copy_named_exports"

and copy_type_exports _cx ~from_ns:_ _reason ~module_t:_ =
  failwith "TODO Annotation_inference.copy_type_exports"

and unary_minus _cx _reason_op _l = failwith "TODO Annotation_inference.unary_minus"

and unary_not _cx _reason_op _l = failwith "TODO Annotation_inference.unary_not"

and mixin _cx _reason _l = failwith "TODO Annotation_inference.mixin"

and obj_rest _cx _reason _xs _t = failwith "TODO Annotation_inference.obj_rest"

and arr_rest _cx _use_op _reason _i _t = failwith "TODO Annotation_inference.arr_rest"

and object_spread _cx _use_op _reason _target _state _t =
  failwith "TODO Annotation_inference.object_spread"

and obj_test_proto _cx _reason_op _l = failwith "TODO Annotation_inference.obj_test_proto"

and existential _cx ~force:_ _reason = failwith "TODO Annotation_inference.existential"
