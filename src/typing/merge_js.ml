(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type get_ast_return = Loc.t Flow_ast.Comment.t list * (ALoc.t, ALoc.t) Flow_ast.Program.t

module RequireMap = WrappedMap.Make (struct
  (* If file A.js imports module 'Foo', this will be ('Foo' * A.js) *)
  type t = string * File_key.t

  let compare (r1, f1) (r2, f2) =
    match String.compare r1 r2 with
    | 0 -> File_key.compare f1 f2
    | n -> n
end)

module FilenameMap = Utils_js.FilenameMap
module ALocSet = Loc_collections.ALocSet

module Reqs = struct
  type impl = ALocSet.t

  type dep_impl = Context.sig_t * ALocSet.t

  type unchecked = ALocSet.t

  type res = ALocSet.t

  type decl = ALocSet.t * Modulename.t

  type t = {
    impls: impl RequireMap.t;
    dep_impls: dep_impl RequireMap.t;
    unchecked: unchecked RequireMap.t;
    res: res RequireMap.t;
    decls: decl RequireMap.t;
  }

  let empty =
    {
      impls = RequireMap.empty;
      dep_impls = RequireMap.empty;
      unchecked = RequireMap.empty;
      res = RequireMap.empty;
      decls = RequireMap.empty;
    }

  let add_impl require requirer require_locs reqs =
    let impls = RequireMap.add ~combine:ALocSet.union (require, requirer) require_locs reqs.impls in
    { reqs with impls }

  let add_dep_impl =
    let combine (from_cx, locs1) (_, locs2) = (from_cx, ALocSet.union locs1 locs2) in
    fun require requirer (from_cx, require_locs) reqs ->
      let dep_impls =
        RequireMap.add ~combine (require, requirer) (from_cx, require_locs) reqs.dep_impls
      in
      { reqs with dep_impls }

  let add_unchecked require requirer require_locs reqs =
    let unchecked =
      RequireMap.add ~combine:ALocSet.union (require, requirer) require_locs reqs.unchecked
    in
    { reqs with unchecked }

  let add_res require requirer require_locs reqs =
    let res = RequireMap.add ~combine:ALocSet.union (require, requirer) require_locs reqs.res in
    { reqs with res }

  let add_decl =
    let combine (locs1, modulename) (locs2, _) = (ALocSet.union locs1 locs2, modulename) in
    fun require requirer (require_locs, modulename) reqs ->
      let decls =
        RequireMap.add ~combine (require, requirer) (require_locs, modulename) reqs.decls
      in
      { reqs with decls }
end

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require cx master_cx cx_to =
  let from_t = Context.find_module_sig master_cx Files.lib_module_ref in
  let to_t = Context.find_module cx_to Files.lib_module_ref in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require cx (cx_from, m, loc, cx_to) =
  let from_t = Context.find_module_sig cx_from m in
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

(* Create the export of a resource file on the fly and connect it to its import
   in cxs_to. This happens in some arbitrary cx, so cx_to should have already
   been copied to cx. *)
let explicit_res_require cx (loc, f, cx_to) =
  (* Recall that a resource file is not parsed, so its export doesn't depend on
     its contents, just its extension. So, we create the export of a resource
     file on the fly by looking at its extension. The general alternative of
     writing / reading these exports to / from separate contexts that live in a
     shared heap takes more time / space (linear in the number of resource
     files), so we avoid it. This optimization is analogous to what we do for
     unchecked files: we create the export (`any`) on the fly instead of writing
     / reading it to / from the context of each unchecked file. *)
  let from_t = Import_export.mk_resource_module_t cx loc f in
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect a export of a declared module to its import in cxs_to. This happens
   in some arbitrary cx, so cx_to should have already been copied to cx. *)
let explicit_decl_require cx (m, loc, resolved_m, cx_to) =
  let reason = Reason.(mk_reason (RCustom m) loc) in
  (* lookup module declaration from builtin context *)
  let m_name = resolved_m |> Modulename.to_string |> Reason.internal_module_name in
  let from_t =
    Tvar.mk_no_wrap_where cx reason (fun from_t ->
        Flow_js.lookup_builtin cx m_name reason (Type.Strict reason) from_t)
  in

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect exports of an unchecked module to its import in cx_to. Note that we
   still lookup the module instead of returning `any` directly. This is because
   a resolved-unchecked dependency is superceded by a possibly-checked libdef.
   See unchecked_*_module_vs_lib tests for examples. *)
let explicit_unchecked_require cx (m, loc, cx_to) =
  (* Use a special reason so we can tell the difference between an any-typed type import
   * from an untyped module and an any-typed type import from a nonexistent module. *)
  let reason = Reason.(mk_reason (RUntypedModule m) loc) in
  let m_name = Reason.internal_module_name m in
  let from_t =
    Tvar.mk_no_wrap_where cx reason (fun from_t ->
        Flow_js.lookup_builtin
          cx
          m_name
          reason
          (Type.NonstrictReturning (Some (Type.AnyT (reason, Type.Untyped), Type.OpenT from_t), None))
          from_t)
  in

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

let detect_sketchy_null_checks cx =
  let add_error ~loc ~null_loc kind falsy_loc =
    Error_message.ESketchyNullLint { kind; loc; null_loc; falsy_loc } |> Flow_js.add_output cx
  in
  let detect_function exists_excuses loc exists_check =
    ExistsCheck.(
      let exists_excuse =
        Loc_collections.ALocMap.find_opt loc exists_excuses |> Base.Option.value ~default:empty
      in
      match exists_check.null_loc with
      | None -> ()
      | Some null_loc ->
        let add_error = add_error ~loc ~null_loc in
        if Base.Option.is_none exists_excuse.bool_loc then
          Base.Option.iter exists_check.bool_loc ~f:(add_error Lints.SketchyNullBool);
        if Base.Option.is_none exists_excuse.number_loc then
          Base.Option.iter exists_check.number_loc ~f:(add_error Lints.SketchyNullNumber);
        if Base.Option.is_none exists_excuse.string_loc then
          Base.Option.iter exists_check.string_loc ~f:(add_error Lints.SketchyNullString);
        if Base.Option.is_none exists_excuse.mixed_loc then
          Base.Option.iter exists_check.mixed_loc ~f:(add_error Lints.SketchyNullMixed);
        if Base.Option.is_none exists_excuse.enum_bool_loc then
          Base.Option.iter exists_check.enum_bool_loc ~f:(add_error Lints.SketchyNullEnumBool);
        if Base.Option.is_none exists_excuse.enum_number_loc then
          Base.Option.iter exists_check.enum_number_loc ~f:(add_error Lints.SketchyNullEnumNumber);
        if Base.Option.is_none exists_excuse.enum_string_loc then
          Base.Option.iter exists_check.enum_string_loc ~f:(add_error Lints.SketchyNullEnumString);
        ())
  in
  Loc_collections.ALocMap.iter
    (detect_function (Context.exists_excuses cx))
    (Context.exists_checks cx)

let detect_test_prop_misses cx =
  let misses = Context.test_prop_get_never_hit cx in
  Base.List.iter
    ~f:(fun (prop_name, (reason_prop, reason_obj), use_op) ->
      Flow_js.add_output
        cx
        (Error_message.EPropNotFound
           { prop_name; reason_prop; reason_obj; use_op; suggestion = None }))
    misses

let detect_unnecessary_optional_chains cx =
  Base.List.iter
    ~f:(fun (loc, lhs_reason) ->
      Flow_js.add_output cx (Error_message.EUnnecessaryOptionalChain (loc, lhs_reason)))
    (Context.unnecessary_optional_chains cx)

let detect_unnecessary_invariants cx =
  Base.List.iter
    ~f:(fun (loc, reason) ->
      Flow_js.add_output cx (Error_message.EUnnecessaryInvariant (loc, reason)))
    (Context.unnecessary_invariants cx)

let detect_invalid_type_assert_calls cx file_sigs cxs tasts =
  if Context.type_asserts cx then Type_asserts.detect_invalid_calls ~full_cx:cx file_sigs cxs tasts

let force_annotations ~arch leader_cx other_cxs =
  if arch = Options.Classic then
    Base.List.iter
      ~f:(fun cx ->
        let should_munge_underscores = Context.should_munge_underscores cx in
        Context.module_ref cx
        |> Flow_js.lookup_module leader_cx
        |> Flow_js.enforce_strict leader_cx ~should_munge_underscores)
      (leader_cx :: other_cxs)

let detect_escaped_generics results =
  Base.List.iter
    ~f:(fun (cx, _, (_, { Flow_ast.Program.statements; _ })) ->
      Generic_escape.scan_for_escapes cx ~add_output:Flow_js.add_output statements)
    results

let detect_non_voidable_properties cx =
  (* This function approximately checks whether VoidT can flow to the provided
   * type without actually creating the flow so as not to disturb type inference.
   * Even though this is happening post-merge, it is possible to encounter an
   * unresolved tvar, in which case it conservatively returns false.
   *)
  let rec is_voidable seen_ids =
    Type.(
      function
      | OpenT (_, id) ->
        (* tvar is recursive: conservatively assume it is non-voidable *)
        if ISet.mem id seen_ids then
          false
        else (
          match Flow_js.possible_types cx id with
          (* tvar has no lower bounds: we conservatively assume it's non-voidable
           * except in the special case when it also has no upper bounds
           *)
          | [] -> Flow_js.possible_uses cx id = []
          (* tvar is resolved: look at voidability of the resolved type *)
          | [t] -> is_voidable (ISet.add id seen_ids) t
          (* tvar is unresolved: conservatively assume it is non-voidable *)
          | _ -> false
        )
      (* a union is voidable if any of its members are voidable *)
      | UnionT (_, rep) -> UnionRep.members rep |> List.exists (is_voidable seen_ids)
      (* an intersection is voidable if all of its members are voidable *)
      | IntersectionT (_, rep) -> InterRep.members rep |> List.for_all (is_voidable seen_ids)
      (* trivially voidable *)
      | MaybeT _
      | DefT (_, _, (VoidT | MixedT (Mixed_everything | Mixed_non_null)))
      | OptionalT _
      | AnyT _ ->
        true
      (* conservatively assume all other types are non-voidable *)
      | _ -> false)
  in
  let check_properties (property_map : Type.Properties.id) :
      ALoc.t Property_assignment.error list SMap.t -> unit =
    let pmap = Context.find_props cx property_map in
    SMap.iter (fun name errors ->
        let should_error =
          match SMap.find_opt name pmap with
          | Some (Type.Field (_, t, _)) -> not @@ is_voidable ISet.empty t
          | _ -> true
        in
        if should_error then
          List.iter
            (fun { Property_assignment.loc; desc } ->
              Flow_js.add_output cx (Error_message.EUninitializedInstanceProperty (loc, desc)))
            errors)
  in
  List.iter
    (fun {
           Context.public_property_map;
           private_property_map;
           errors = { Property_assignment.public_property_errors; private_property_errors };
         } ->
      check_properties public_property_map public_property_errors;
      check_properties private_property_map private_property_errors)
    (Context.voidable_checks cx)

let merge_tvar =
  Type.(
    let possible_types = Flow_js.possible_types in
    let rec collect_lowers ~filter_empty cx seen acc = function
      | [] -> Base.List.rev acc
      | t :: ts ->
        (match t with
        (* Recursively unwrap unseen tvars *)
        | OpenT (_, id) ->
          if ISet.mem id seen then
            collect_lowers ~filter_empty cx seen acc ts
          (* already unwrapped *)
          else
            let seen = ISet.add id seen in
            collect_lowers ~filter_empty cx seen acc (possible_types cx id @ ts)
        (* Ignore empty in existentials. This behavior is sketchy, but the error
           behavior without this filtering is worse. If an existential accumulates
           an empty, we error but it's very non-obvious how the empty arose. *)
        | DefT (_, _, EmptyT flavor) when filter_empty flavor ->
          collect_lowers ~filter_empty cx seen acc ts
        (* Everything else becomes part of the merge typed *)
        | _ -> collect_lowers ~filter_empty cx seen (t :: acc) ts)
    in
    fun ?filter_empty cx r id ->
      (* Because the behavior of existentials are so difficult to predict, they
         enjoy some special casing here. When existential types are finally
         removed, this logic can be removed. *)
      let existential =
        Reason.(
          match desc_of_reason r with
          | RExistential -> true
          | _ -> false)
      in
      let filter_empty flavor =
        existential
        ||
        match filter_empty with
        | Some filter_empty -> filter_empty flavor
        | None -> false
      in
      let lowers =
        let seen = ISet.singleton id in
        collect_lowers cx seen [] (possible_types cx id) ~filter_empty
      in
      match lowers with
      | [t] -> t
      | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)
      | [] ->
        let uses = Flow_js.possible_uses cx id in
        if uses = [] || existential then
          AnyT.locationless Unsoundness.existential
        else
          MergedT (r, uses))

let merge_trust_var constr =
  Trust_constraint.(
    match constr with
    | TrustResolved t -> t
    | TrustUnresolved bound -> get_trust bound |> Trust.fix)

class resolver_visitor =
  (* Filter out EmptyT types from unions, when they're not the result of generate_tests.
     (The flavor in this case will be `Zeroed`.) This helps keep types clean from
     EmptyTs that have been left over from refinement. *)
  let filter_empty flavor =
    match flavor with
    | Type.Zeroed -> false
    | Type.Bottom -> true
  in
  object (self)
    inherit [unit] Type_mapper.t_with_uses as super

    method! type_ cx map_cx t =
      let open Type in
      match t with
      | OpenT (r, id) -> merge_tvar ~filter_empty cx r id
      | EvalT (t', dt, _id) ->
        let t'' = self#type_ cx map_cx t' in
        let dt' = self#defer_use_type cx map_cx dt in
        if t' == t'' && dt == dt' then
          t
        else
          Flow_cache.Eval.id cx t'' dt'
      | _ -> super#type_ cx map_cx t

    (* Only called from type_ and the CreateObjWithComputedPropT use case *)
    method tvar _cx _seen _r id = id

    (* overridden in type_ *)
    method eval_id _cx _map_cx _id = assert false

    method props cx map_cx id =
      let props_map = Context.find_props cx id in
      let props_map' =
        SMap.ident_map (Type.Property.ident_map_t (self#type_ cx map_cx)) props_map
      in
      let id' =
        if props_map == props_map' then
          id
        (* When mapping results in a new property map, we have to use a
           generated id, rather than a location from source. *)
        else
          Context.generate_property_map cx props_map'
      in
      id'

    (* These should already be fully-resolved. *)
    method exports _cx _map_cx id = id

    method call_prop cx map_cx id =
      let t = Context.find_call cx id in
      let t' = self#type_ cx map_cx t in
      if t == t' then
        id
      else
        Context.make_call_prop cx t'
  end

let detect_matching_props_violations cx =
  let open Type in
  let resolver = new resolver_visitor in
  let step (reason, key, sentinel, obj) =
    let obj = resolver#type_ cx () obj in
    let sentinel = resolver#type_ cx () sentinel in
    match drop_generic sentinel with
    (* TODO: it should not be possible to create a MatchingPropT with a non-tvar tout *)
    | DefT (_, _, (BoolT (Some _) | StrT (Literal _) | NumT (Literal _))) ->
      (* Limit the check to promitive literal sentinels *)
      let use_op =
        Op
          (MatchingProp
             {
               op = reason;
               obj = TypeUtil.reason_of_t obj;
               key;
               sentinel_reason = TypeUtil.reason_of_t sentinel;
             })
      in
      (* If `obj` is a GenericT, we replace it with it's upper bound, since ultimately it will flow into
         `sentinel` rather than the other way around. *)
      Flow_js.flow cx (MatchingPropT (reason, key, sentinel), UseT (use_op, drop_generic obj))
    | _ -> ()
  in
  let matching_props = Context.matching_props cx in
  List.iter step matching_props

let detect_literal_subtypes =
  let lb_visitor = new resolver_visitor in
  let ub_visitor =
    object (self)
      inherit resolver_visitor as super

      method! type_ cx map_cx t =
        let open Type in
        match t with
        | DefT (r, _, EmptyT Zeroed) -> AnyT.why Untyped r
        | GenericT { bound; _ } -> self#type_ cx map_cx bound
        | t -> super#type_ cx map_cx t
    end
  in
  fun cx ->
    let checks = Context.literal_subtypes cx in
    List.iter
      (fun (t, u) ->
        let t = lb_visitor#type_ cx () t in
        let u = ub_visitor#use_type cx () u in
        Flow_js.flow cx (t, u))
      checks

(* Merge a component with its "implicit requires" and "explicit requires." The
   implicit requires are those defined in libraries. For the explicit
   requires, we need to merge only those parts of the dependency graph that the
   component immediately depends on. (We assume that this merging is part of a
   recursive process that has already handled recursive dependencies.)

   Now, by definition, files in a component can bidirectionally depend only on
   other files in the component. All other dependencies are unidirectional.

   Let dep_cxs contain the (optimized) contexts of all dependencies that are
   unidirectional, and let component_cxs contain the contexts of the files in
   the component. Let master_cx be the (optimized) context of libraries.

   Let implementations contain the dependency edges between contexts in
   component_cxs and dep_cxs, resources contain the dependency edges between
   contexts in component_cxs and resource files, and declarations contain the
   dependency edges from component_cxs to master_cx.

   We assume that the first context in component_cxs is that of the leader (cx):
   this serves as the "host" for the merging. Let the remaining contexts in
   component_cxs be other_cxs.

   1. Copy dep_cxs, other_cxs, and master_cx to the host cx.

   2. Link the edges in implementations.

   3. Link the edges in resources.

   4. Link the edges in declarations.

   5. Link the local references to libraries in master_cx and component_cxs.
*)
let merge_component
    ~arch
    ~metadata
    ~lint_severities
    ~strict_mode
    ~file_sigs
    ~get_ast_unsafe
    ~get_aloc_table_unsafe
    ~get_docblock_unsafe
    ~phase
    component
    reqs
    dep_cxs
    (master_cx : Context.sig_t) =
  let aloc_tables =
    Nel.fold_left
      (fun tables filename ->
        let table = lazy (get_aloc_table_unsafe filename) in
        FilenameMap.add filename table tables)
      FilenameMap.empty
      component
  in
  let sig_cx = Context.make_sig () in
  let ccx = Context.make_ccx sig_cx aloc_tables in
  let need_merge_master_cx = ref true in
  let (rev_results, impl_cxs) =
    Nel.fold_left
      (fun (results, impl_cxs) filename ->
        (* create cx *)
        let info = get_docblock_unsafe filename in
        let metadata = Context.docblock_overrides info metadata in
        let module_ref = Files.module_ref filename in
        let rev_table =
          let table = FilenameMap.find filename aloc_tables in
          lazy
            (try Lazy.force table |> ALoc.reverse_table
             with
             (* If we aren't in abstract locations mode, or are in a libdef, we
                won't have an aloc table, so we just create an empty reverse
                table. We handle this exception here rather than explicitly
                making an optional version of the get_aloc_table function for
                simplicity. *)
             | Parsing_heaps_exceptions.Sig_ast_ALoc_table_not_found _ ->
               ALoc.make_empty_reverse_table ())
        in
        let cx = Context.make ccx metadata filename rev_table module_ref phase in
        (* create builtins *)
        if !need_merge_master_cx then (
          need_merge_master_cx := false;
          Flow_js.mk_builtins cx;
          Context.merge_into sig_cx master_cx;
          implicit_require cx master_cx cx
        );

        (* local inference *)
        let (comments, ast) = get_ast_unsafe filename in
        let lint_severities =
          if metadata.Context.strict || metadata.Context.strict_local then
            StrictModeSettings.fold
              (fun lint_kind lint_severities ->
                LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
              strict_mode
              lint_severities
          else
            lint_severities
        in
        let file_sig = FilenameMap.find filename file_sigs in
        let tast =
          Type_inference_js.infer_ast cx filename comments ast ~lint_severities ~file_sig
        in
        ((cx, ast, tast) :: results, FilenameMap.add filename cx impl_cxs))
      ([], FilenameMap.empty)
      component
  in
  let results = Base.List.rev rev_results in
  let (cxs, _, tasts) = Base.List.unzip3 results in
  let (cx, other_cxs) = (Base.List.hd_exn cxs, Base.List.tl_exn cxs) in

  dep_cxs |> Base.List.iter ~f:(Context.merge_into sig_cx);

  Reqs.(
    reqs.impls
    |> RequireMap.iter (fun (m, fn_to) locs ->
           let cx_to = FilenameMap.find fn_to impl_cxs in
           ALocSet.iter (fun loc -> explicit_impl_require cx (sig_cx, m, loc, cx_to)) locs);

    reqs.dep_impls
    |> RequireMap.iter (fun (m, fn_to) (cx_from, locs) ->
           let cx_to = FilenameMap.find fn_to impl_cxs in
           ALocSet.iter (fun loc -> explicit_impl_require cx (cx_from, m, loc, cx_to)) locs);

    reqs.res
    |> RequireMap.iter (fun (f, fn_to) locs ->
           let cx_to = FilenameMap.find fn_to impl_cxs in
           ALocSet.iter (fun loc -> explicit_res_require cx (loc, f, cx_to)) locs);

    reqs.decls
    |> RequireMap.iter (fun (m, fn_to) (locs, resolved_m) ->
           let cx_to = FilenameMap.find fn_to impl_cxs in
           ALocSet.iter (fun loc -> explicit_decl_require cx (m, loc, resolved_m, cx_to)) locs);

    reqs.unchecked
    |> RequireMap.iter (fun (m, fn_to) locs ->
           let cx_to = FilenameMap.find fn_to impl_cxs in
           ALocSet.iter (fun loc -> explicit_unchecked_require cx (m, loc, cx_to)) locs);

    (* Post-merge errors.
     *
     * At this point, all dependencies have been merged and the component has been
     * linked together. Any constraints should have already been evaluated, which
     * means we can complain about things that either haven't happened yet, or
     * which require complete knowledge of tvar bounds.
     *)
    detect_sketchy_null_checks cx;
    detect_non_voidable_properties cx;
    detect_test_prop_misses cx;
    detect_unnecessary_optional_chains cx;
    detect_unnecessary_invariants cx;
    detect_invalid_type_assert_calls cx file_sigs cxs tasts;
    Strict_es6_import_export.detect_errors ~metadata ~phase cx results;
    if phase = Context.Checking then detect_escaped_generics results;

    (* Well-formed conditionals *)
    detect_matching_props_violations cx;
    detect_literal_subtypes cx;

    force_annotations ~arch cx other_cxs;

    match results with
    | [] -> failwith "there is at least one cx"
    | x :: xs -> (x, xs))

(****************** signature contexts *********************)

(* Once a context is merged with other contexts it depends on, it is ready to be
   optimized for use by contexts that in turn depend on it. The only interesting
   part of the context for such use is its exports. Thus, we call a context
   optimized for such use a "signature context."

   A signature context should contain only descriptions of its exports. Anything
   that does not contribute to those descriptions are redundant and should be
   discarded: otherwise, we would end up paying significant hidden costs, in
   terms of memory (storing in the heap) and time (serializing / deserializing
   from the heap, garbage collection pauses).

   As it turns out, it is not always possible or even desirable to describe the
   exports in "closed form" as types: instead, the descriptions take the form of
   a (mutually) recursive system of "equations" relating type variables to
   "solved" types, and the types themselves may contain these type variables
   inside. As indicated above, such descriptions are both necessary (due to
   recursive types) and desirable (due to sharing, and for cyclically depending
   contexts, due to the need to deal with the exports of all such contexts at
   once). This form is quite adequate for type checking (indeed, it can be
   thought of as merely a condensed form of usual contexts, which already
   exhibit this "interlinked" behavior).

   Unsurprisingly, since types also make references to some other maps in a
   context (property maps, envs), these references need to be carried around
   as well for the descriptions to be complete.

   We can collect compact and complete descriptions of exports by using a custom
   type visitor that marks relevant things that need to be carried over as it
   walks the context from its exports. Once the walk is done, we can replace the
   corresponding parts of the context (graph, property maps, envs) by their
   reduced forms.

   NOTE: Changing ids of entities stored in the Context is dangerous here,
   since it would require changing every occurence of that id. As such, the mapper
   currently only traverses ids for side-effects, but returns the original id.

   There are also several places where we add something to a map, recurse, then
   readd. The original add makes sure we don't visit the same entity twice, the
   second actually changes the entity in the exports.
*)
module ContextOptimizer = struct
  open Constraint
  open Type
  open TypeUtil

  class context_optimizer =
    object (self)
      inherit [Polarity.t] Type_mapper.t_with_uses as super

      val sig_hash = Xx.init 0L

      method sig_hash () = Xx.digest sig_hash

      val mutable next_stable_id = 0

      method fresh_stable_id =
        let stable_id = next_stable_id in
        next_stable_id <- next_stable_id + 1;
        stable_id

      val mutable stable_tvar_ids = IMap.empty

      val mutable stable_trust_var_ids = IMap.empty

      val mutable stable_eval_ids = IMap.empty

      val mutable stable_poly_ids = IMap.empty

      val mutable stable_props_ids = IMap.empty

      val mutable stable_call_prop_ids = IMap.empty

      val mutable reduced_module_map = SMap.empty

      val mutable reduced_graph = IMap.empty

      val mutable reduced_trust_graph = IMap.empty

      val mutable reduced_property_maps = Properties.Map.empty

      val mutable reduced_call_props = IMap.empty

      val mutable reduced_export_maps = Exports.Map.empty

      val mutable reduced_evaluated = Eval.Map.empty

      val mutable export_reason = None

      val mutable export_file = None

      method reduce cx module_ref =
        let export = Context.find_module cx module_ref in
        export_file <- reason_of_t export |> Reason.aloc_of_reason |> ALoc.source;
        let export' = self#type_ cx Polarity.Neutral export in
        reduced_module_map <- SMap.add module_ref export' reduced_module_map

      method tvar cx pole r id =
        let (root_id, _) = Context.find_constraints cx id in
        if id == root_id then (
          if IMap.mem id reduced_graph then (
            let stable_id = IMap.find root_id stable_tvar_ids in
            SigHash.add_int sig_hash stable_id;
            id
          ) else
            let t = merge_tvar cx r id in
            let node = Root { rank = 0; constraints = FullyResolved (unknown_use, t) } in
            reduced_graph <- IMap.add id node reduced_graph;
            let () =
              let stable_id = self#fresh_stable_id in
              stable_tvar_ids <- IMap.add id stable_id stable_tvar_ids
            in
            let t = self#type_ cx pole t in
            let node = Root { rank = 0; constraints = FullyResolved (unknown_use, t) } in
            reduced_graph <- IMap.add id node reduced_graph;
            id
        ) else (
          ignore (self#tvar cx pole r root_id);
          let node = Goto root_id in
          reduced_graph <- IMap.add id node reduced_graph;
          id
        )

      method trust_var cx pole id =
        let (root_id, constr) = Context.find_trust_constraints cx id in
        if id == root_id then (
          if IMap.mem id reduced_trust_graph then (
            let stable_id = IMap.find root_id stable_trust_var_ids in
            SigHash.add_int sig_hash stable_id;
            id
          ) else
            let t = merge_trust_var constr in
            let node = Trust_constraint.new_resolved_root t in
            reduced_trust_graph <- IMap.add id node reduced_trust_graph;
            let () =
              let stable_id = self#fresh_stable_id in
              stable_trust_var_ids <- IMap.add id stable_id stable_trust_var_ids
            in
            id
        ) else (
          ignore (self#trust_var cx pole root_id);
          let node = Trust_constraint.TrustGoto root_id in
          reduced_trust_graph <- IMap.add id node reduced_trust_graph;
          id
        )

      method props cx pole id =
        if Properties.Map.mem id reduced_property_maps then
          let () =
            Base.Option.iter
              ~f:(fun id_int ->
                let stable_id =
                  if Context.mem_nominal_prop_id cx id_int then
                    IMap.find id_int stable_props_ids
                  else
                    id_int
                in
                SigHash.add_int sig_hash stable_id)
              (Properties.id_as_int id)
          in
          id
        else
          let () =
            Base.Option.iter
              ~f:(fun id_int ->
                let stable_id =
                  if Context.mem_nominal_prop_id cx id_int then (
                    let stable_id = self#fresh_stable_id in
                    stable_props_ids <- IMap.add id_int stable_id stable_props_ids;
                    stable_id
                  ) else
                    id_int
                in
                SigHash.add_int sig_hash stable_id)
              (Properties.id_as_int id)
          in
          let pmap = Context.find_props cx id in
          let () = SigHash.add_props_map sig_hash pmap in
          reduced_property_maps <- Properties.Map.add id pmap reduced_property_maps;
          let pmap' = SMap.ident_map (self#prop cx pole) pmap in
          reduced_property_maps <- Properties.Map.add id pmap' reduced_property_maps;
          id

      method call_prop cx pole id =
        if IMap.mem id reduced_call_props then
          let stable_id = IMap.find id stable_call_prop_ids in
          let () = SigHash.add_int sig_hash stable_id in
          id
        else
          let () =
            let stable_id = self#fresh_stable_id in
            stable_call_prop_ids <- IMap.add id stable_id stable_call_prop_ids
          in
          let t = Context.find_call cx id in
          reduced_call_props <- IMap.add id t reduced_call_props;
          let t' = self#type_ cx pole t in
          reduced_call_props <- IMap.add id t' reduced_call_props;
          id

      method! export_types cx map_cx e =
        let { exports_tmap; cjs_export; has_every_named_export } = e in
        let exports_tmap' = self#exports cx map_cx exports_tmap in
        let cjs_export' =
          OptionUtils.ident_map
            (fun exp ->
              export_reason <- Some (reason_of_t exp);
              self#type_ cx map_cx exp)
            cjs_export
        in
        if exports_tmap == exports_tmap' && cjs_export == cjs_export' then
          e
        else
          { exports_tmap = exports_tmap'; cjs_export = cjs_export'; has_every_named_export }

      method exports cx pole id =
        if Exports.Map.mem id reduced_export_maps then
          id
        else
          let tmap = Context.find_exports cx id in
          let map_pair p =
            let (loc, t) = p in
            export_reason <- Some (reason_of_t t);
            let t' = self#type_ cx pole t in
            if t == t' then
              p
            else
              (loc, t')
          in
          reduced_export_maps <- Exports.Map.add id tmap reduced_export_maps;
          let tmap' = SMap.ident_map map_pair tmap in
          reduced_export_maps <- Exports.Map.add id tmap' reduced_export_maps;
          SigHash.add_exports_map sig_hash tmap';
          id

      method eval_id cx pole id =
        if Eval.Map.mem id reduced_evaluated then (
          Eval.id_as_int id
          |> Base.Option.iter ~f:(fun int_id ->
                 IMap.find int_id stable_eval_ids |> SigHash.add_int sig_hash);
          id
        ) else
          let stable_id = self#fresh_stable_id in
          Eval.id_as_int id
          |> Base.Option.iter ~f:(fun int_id ->
                 stable_eval_ids <- IMap.add int_id stable_id stable_eval_ids);
          match Eval.Map.find_opt id (Context.evaluated cx) with
          | None -> id
          | Some t ->
            reduced_evaluated <- Eval.Map.add id t reduced_evaluated;
            let t' = self#type_ cx pole t in
            reduced_evaluated <- Eval.Map.add id t' reduced_evaluated;
            id

      method! destructor cx map_cx t =
        SigHash.add_destructor sig_hash t;
        match t with
        | NonMaybeType -> t
        | PropertyType s ->
          SigHash.add sig_hash s;
          t
        | ElementType t' ->
          let t'' = self#type_ cx map_cx t' in
          if t'' == t' then
            t
          else
            ElementType t''
        | Bind t' ->
          let t'' = self#type_ cx map_cx t' in
          if t'' == t' then
            t
          else
            Bind t''
        | ReadOnlyType -> t
        | SpreadType (options, tlist, acc) ->
          let () =
            match options with
            | Object.Spread.Annot { make_exact } -> SigHash.add_bool sig_hash make_exact
            | _ -> ()
          in
          let tlist' = ListUtils.ident_map (self#object_kit_spread_operand cx map_cx) tlist in
          let acc' = OptionUtils.ident_map (self#object_kit_spread_operand_slice cx map_cx) acc in
          if tlist' == tlist && acc == acc' then
            t
          else
            SpreadType (options, tlist', acc')
        | RestType (options, x) ->
          let open Object.Rest in
          let () =
            match options with
            | Sound -> SigHash.add_int sig_hash 1
            | IgnoreExactAndOwn -> SigHash.add_int sig_hash 2
            | ReactConfigMerge p ->
              SigHash.add_int sig_hash 3;
              SigHash.add sig_hash (Polarity.sigil p)
          in
          let x' = self#type_ cx map_cx x in
          if x' == x then
            t
          else
            RestType (options, x')
        | ValuesType -> t
        | CallType args ->
          let args' = ListUtils.ident_map (self#type_ cx map_cx) args in
          if args' == args then
            t
          else
            CallType args'
        | TypeMap tmap ->
          let tmap' = self#type_map cx map_cx tmap in
          if tmap' == tmap then
            t
          else
            TypeMap tmap'
        | ReactConfigType default_props ->
          let default_props' = self#type_ cx map_cx default_props in
          if default_props' == default_props then
            t
          else
            ReactConfigType default_props'
        | ReactElementPropsType
        | ReactElementConfigType
        | ReactElementRefType ->
          t

      method! dict_type cx pole dicttype =
        let dicttype' = super#dict_type cx pole dicttype in
        SigHash.add_polarity sig_hash dicttype'.dict_polarity;
        dicttype'

      method! enum cx pole e =
        let { enum_id; members; _ } = e in
        let e' = super#enum cx pole e in
        SigHash.add_aloc sig_hash (enum_id :> ALoc.t);
        Base.List.iter
          ~f:(SigHash.add sig_hash)
          (SMap.keys members |> Base.List.stable_sort ~compare:String.compare);
        e'

      method! type_ cx pole t =
        SigHash.add_reason sig_hash (reason_of_t t);
        begin
          match t with
          | DefT (_, trust, _) when Context.trust_tracking cx && is_ident trust ->
            ignore (self#trust_var cx pole (as_ident trust))
          | _ -> ()
        end;
        match t with
        | InternalT _ -> Utils_js.assert_false "internal types should not appear in signatures"
        | OpenT _ -> super#type_ cx pole t
        | DefT (_, _, InstanceT (_, _, _, { class_id; _ })) ->
          SigHash.add_aloc sig_hash (class_id :> ALoc.t);
          super#type_ cx pole t
        | OpaqueT (_, { opaque_id; _ }) ->
          SigHash.add_aloc sig_hash (opaque_id :> ALoc.t);
          super#type_ cx pole t
        | DefT (_, _, PolyT { id = poly_id; _ }) ->
          if Context.mem_nominal_poly_id cx poly_id then
            Poly.id_as_int poly_id
            |> Base.Option.iter ~f:(fun poly_id ->
                   let id =
                     match IMap.find_opt poly_id stable_poly_ids with
                     | None ->
                       let id = self#fresh_stable_id in
                       stable_poly_ids <- IMap.add poly_id id stable_poly_ids;
                       id
                     | Some id -> id
                   in
                   SigHash.add_int sig_hash id)
          else
            Poly.id_as_int poly_id |> Base.Option.iter ~f:(SigHash.add_int sig_hash);
          super#type_ cx pole t
        | UnionT (_, rep) ->
          if not (UnionRep.is_optimized_finally rep) then
            UnionRep.optimize
              rep
              ~reasonless_eq:TypeUtil.reasonless_eq
              ~flatten:(Type_mapper.union_flatten cx)
              ~find_resolved:(Context.find_resolved cx)
              ~find_props:(Context.find_props cx);
          let t' = super#type_ cx pole t in
          SigHash.add_type sig_hash t';
          t'
        | _ ->
          let t' = super#type_ cx pole t in
          SigHash.add_type sig_hash t';
          t'

      method! use_type cx pole use =
        SigHash.add_reason sig_hash (reason_of_use_t use);
        match use with
        | UseT (u, t) ->
          let t' = self#type_ cx Polarity.Neutral t in
          if t' == t then
            use
          else
            UseT (u, t')
        | _ ->
          SigHash.add_use sig_hash use;
          super#use_type cx pole use

      method! choice_use_tool =
        (* Even with MergedT, any choice kit constraints should be fully
           discharged by this point. This preserves a key invariant, that type
           graphs are local to a single merge job. In other words, we will not see
           a FullyResolveType constraint that corresponds to a tvar from another
           context. This makes it possible to clear the type graph before storing
           in the heap. *)
        Utils_js.assert_false "choice kit uses should not appear in signatures"

      (* We need to make sure to hash the keys in any spread intermediate types! *)
      method! object_kit_spread_operand_slice cx map_cx slice =
        SMap.iter (fun k _ -> SigHash.add sig_hash k) slice.Object.Spread.prop_map;
        super#object_kit_spread_operand_slice cx map_cx slice

      method get_reduced_module_map = reduced_module_map

      method get_reduced_graph = reduced_graph

      method get_reduced_trust_graph = reduced_trust_graph

      method get_reduced_property_maps = reduced_property_maps

      method get_reduced_call_props = reduced_call_props

      method get_reduced_export_maps = reduced_export_maps

      method get_reduced_evaluated = reduced_evaluated
    end

  (* walk a context from a list of exports *)
  let reduce_context cx module_refs =
    let reducer = new context_optimizer in
    Base.List.iter ~f:(reducer#reduce cx) module_refs;
    (reducer#sig_hash (), reducer)

  (* reduce a context to a "signature context" *)
  let sig_context cx module_refs =
    let (sig_hash, reducer) = reduce_context cx module_refs in
    Context.set_module_map cx reducer#get_reduced_module_map;
    Context.set_graph cx reducer#get_reduced_graph;
    Context.set_trust_graph cx reducer#get_reduced_trust_graph;
    Context.set_property_maps cx reducer#get_reduced_property_maps;
    Context.set_call_props cx reducer#get_reduced_call_props;
    Context.set_export_maps cx reducer#get_reduced_export_maps;
    Context.set_evaluated cx reducer#get_reduced_evaluated;
    sig_hash
end
