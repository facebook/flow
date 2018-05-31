(**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module RequireMap = MyMap.Make (struct
  (* If file A.js imports module 'Foo', this will be ('Foo' * A.js) *)
  type t = string * File_key.t

  let compare (r1, f1) (r2, f2) =
    match String.compare r1 r2 with
    | 0 -> File_key.compare f1 f2
    | n -> n
end)

module FilenameMap = Utils_js.FilenameMap
module LocSet = Utils_js.LocSet

module Reqs = struct
  type impl = LocSet.t
  type dep_impl = Context.sig_t * LocSet.t
  type unchecked = LocSet.t
  type res = LocSet.t
  type decl = LocSet.t * Modulename.t
  type t = {
    impls: impl RequireMap.t;
    dep_impls: dep_impl RequireMap.t;
    unchecked: unchecked RequireMap.t;
    res: res RequireMap.t;
    decls: decl RequireMap.t;
  }

  let empty = {
    impls = RequireMap.empty;
    dep_impls = RequireMap.empty;
    unchecked = RequireMap.empty;
    res = RequireMap.empty;
    decls = RequireMap.empty;
  }

  let add_impl require requirer require_locs reqs =
    let impls = RequireMap.add ~combine:LocSet.union (require, requirer) require_locs reqs.impls in
    { reqs with impls }

  let add_dep_impl =
    let combine (from_cx, locs1) (_, locs2) = from_cx, LocSet.union locs1 locs2 in
    fun require requirer (from_cx, require_locs) reqs ->
      let dep_impls =
        RequireMap.add ~combine (require, requirer) (from_cx, require_locs) reqs.dep_impls
      in
      { reqs with dep_impls }

  let add_unchecked require requirer require_locs reqs =
    let unchecked =
      RequireMap.add ~combine:LocSet.union (require, requirer) require_locs reqs.unchecked
    in
    { reqs with unchecked }

  let add_res require requirer require_locs reqs =
    let res = RequireMap.add ~combine:LocSet.union (require, requirer) require_locs reqs.res in
    { reqs with res }

  let add_decl =
    let combine (locs1, modulename) (locs2, _) = LocSet.union locs1 locs2, modulename in
    fun require requirer (require_locs, modulename) reqs ->
    let decls = RequireMap.add ~combine (require, requirer) (require_locs, modulename) reqs.decls in
    { reqs with decls }
end

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx cx_to =
  let from_t = Context.find_module_sig master_cx Files.lib_module_ref in
  let to_t = Context.find_module cx_to Files.lib_module_ref in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, m, loc, cx_to) =
  let from_t = Context.find_module_sig cx_from m in
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

(* Create the export of a resource file on the fly and connect it to its import
   in cxs_to. This happens in some arbitrary cx, so cx_to should have already
   been copied to cx. *)
let explicit_res_require_strict cx (loc, f, cx_to) =
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
let explicit_decl_require_strict cx (m, loc, resolved_m, cx_to) =
  let reason = Reason.(mk_reason (RCustom m) loc) in

  (* lookup module declaration from builtin context *)
  let m_name =
    resolved_m
    |> Modulename.to_string
    |> Reason.internal_module_name
  in
  let from_t = Tvar.mk cx reason in
  Flow_js.lookup_builtin cx m_name reason
    (Type.Strict reason) from_t;

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect exports of an unchecked module to its import in cx_to. Note that we
   still lookup the module instead of returning `any` directly. This is because
   a resolved-unchecked dependency is superceded by a possibly-checked libdef.
   See unchecked_*_module_vs_lib tests for examples. *)
let explicit_unchecked_require_strict cx (m, loc, cx_to) =
  (* Use a special reason so we can tell the difference between an any-typed type import
   * from an untyped module and an any-typed type import from a nonexistent module. *)
  let reason = Reason.(mk_reason (RUntypedModule m) loc) in
  let m_name = Reason.internal_module_name m in
  let from_t = Tvar.mk cx reason in
  Flow_js.lookup_builtin cx m_name reason
    (Type.NonstrictReturning (Some (Type.DefT (reason, Type.AnyT), from_t), None)) from_t;

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to loc in
  Flow_js.flow_t cx (from_t, to_t)

let detect_sketchy_null_checks cx =
  let add_error ~loc ~null_loc kind falsy_loc =
    let msg = Flow_error.ESketchyNullLint { kind; loc; null_loc; falsy_loc } in
    Flow_error.error_of_msg ~trace_reasons:[] ~source_file:(Context.file cx) msg
    |> Context.add_error cx
  in

  let detect_function exists_excuses loc exists_check =
    let open ExistsCheck in

    let exists_excuse = Utils_js.LocMap.get loc exists_excuses
      |> Option.value ~default:empty in

    begin match exists_check.null_loc with
      | None -> ()
      | Some null_loc ->
        let add_error = add_error ~loc ~null_loc in
        if (Option.is_none exists_excuse.bool_loc) then
          Option.iter exists_check.bool_loc ~f:(add_error Lints.SketchyBool);
        if (Option.is_none exists_excuse.number_loc) then
          Option.iter exists_check.number_loc ~f:(add_error Lints.SketchyNumber);
        if (Option.is_none exists_excuse.string_loc) then
          Option.iter exists_check.string_loc ~f:(add_error Lints.SketchyString);
        if (Option.is_none exists_excuse.mixed_loc) then
          Option.iter exists_check.mixed_loc ~f:(add_error Lints.SketchyMixed);
        ()
    end
  in

  Utils_js.LocMap.iter (detect_function (Context.exists_excuses cx)) (Context.exists_checks cx)

let detect_test_prop_misses cx =
  let misses = Context.test_prop_get_never_hit cx in
  List.iter (fun (name, reasons, use_op) ->
    Flow_js.add_output cx (Flow_error.EPropNotFound (name, reasons, use_op))
  ) misses

let detect_unnecessary_optional_chains cx =
  List.iter (fun (loc, lhs_reason) ->
    Flow_js.add_output cx (Flow_error.EUnnecessaryOptionalChain (loc, lhs_reason))
  ) (Context.unnecessary_optional_chains cx)

let apply_docblock_overrides (metadata: Context.metadata) docblock_info =
  let open Context in

  let metadata =
    let jsx = match Docblock.jsx docblock_info with
    | Some (Docblock.Jsx_pragma (expr, jsx_expr)) -> Options.Jsx_pragma (expr, jsx_expr)
    | Some Docblock.Csx_pragma -> Options.Jsx_csx
    | None -> Options.Jsx_react
    in
    { metadata with jsx }
  in

  let metadata = match Docblock.flow docblock_info with
  | None -> metadata
  | Some Docblock.OptIn -> { metadata with checked = true; }
  | Some Docblock.OptInStrict -> { metadata with checked = true; strict = true; }
  | Some Docblock.OptInStrictLocal -> { metadata with checked = true; strict_local = true; }
  | Some Docblock.OptInWeak -> { metadata with checked = true; weak = true }

  (* --all (which sets metadata.checked = true) overrides @noflow, so there are
     currently no scenarios where we'd change checked = true to false. in the
     future, there may be a case where checked defaults to true (but is not
     forced to be true ala --all), but for now we do *not* want to force
     checked = false here. *)
  | Some Docblock.OptOut -> metadata
  in

  let metadata = match Docblock.preventMunge docblock_info with
  | Some value -> { metadata with munge_underscores = not value; }
  | None -> metadata
  in

  metadata


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
let merge_component_strict ~metadata ~lint_severities ~strict_mode ~file_sigs
  ~get_ast_unsafe ~get_docblock_unsafe ?(do_gc=false)
  component reqs dep_cxs (master_cx: Context.sig_t) =

  let sig_cx = Context.make_sig () in
  let need_merge_master_cx = ref true in

  let init_gc_state = if do_gc then Some (Gc_js.init ~master_cx) else None in

  let rev_cxs, impl_cxs, _ = Nel.fold_left (fun (cxs, impl_cxs, gc_state) filename ->
    (* create cx *)
    let info = get_docblock_unsafe filename in
    let metadata = apply_docblock_overrides metadata info in
    let module_ref = Files.module_ref filename in
    let cx = Context.make sig_cx metadata filename module_ref in

    (* create builtins *)
    if !need_merge_master_cx then (
      need_merge_master_cx := false;
      Flow_js.mk_builtins cx;
      Context.merge_into sig_cx master_cx;
      implicit_require_strict cx master_cx cx
    );

    (* local inference *)
    let ast = get_ast_unsafe filename in
    let lint_severities =
      if metadata.Context.strict || metadata.Context.strict_local
      then StrictModeSettings.fold
        (fun lint_kind lint_severities ->
          LintSettings.set_value lint_kind (Severity.Err, None) lint_severities
        ) strict_mode lint_severities
      else lint_severities in
    let file_sig = FilenameMap.find_unsafe filename file_sigs in
    Type_inference_js.infer_ast cx filename ast
      ~lint_severities ~file_sig;

    let gc_state = Option.map gc_state Gc_js.(fun gc_state ->
      let gc_state = mark cx gc_state in
      sweep ~master_cx cx gc_state;
      gc_state
    ) in

    cx::cxs, FilenameMap.add filename cx impl_cxs, gc_state
  ) ([], FilenameMap.empty, init_gc_state) component in
  let cxs = List.rev rev_cxs in

  let cx, other_cxs = List.hd cxs, List.tl cxs in

  Flow_js.Cache.clear();

  dep_cxs |> List.iter (Context.merge_into sig_cx);

  let open Reqs in

  reqs.impls
  |> RequireMap.iter (fun (m, fn_to) locs ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    LocSet.iter (fun loc ->
      explicit_impl_require_strict cx (sig_cx, m, loc, cx_to);
    ) locs;
  );

  reqs.dep_impls
  |> RequireMap.iter (fun (m, fn_to) (cx_from, locs) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    LocSet.iter (fun loc ->
      explicit_impl_require_strict cx (cx_from, m, loc, cx_to)
    ) locs
  );

  reqs.res
  |> RequireMap.iter (fun (f, fn_to) locs ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    LocSet.iter (fun loc ->
      explicit_res_require_strict cx (loc, f, cx_to)
    ) locs
  );

  reqs.decls
  |> RequireMap.iter (fun (m, fn_to) (locs, resolved_m) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    LocSet.iter (fun loc ->
      explicit_decl_require_strict cx (m, loc, resolved_m, cx_to)
    ) locs
  );

  reqs.unchecked
  |> RequireMap.iter (fun (m, fn_to) locs ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    LocSet.iter (fun loc ->
      explicit_unchecked_require_strict cx (m, loc, cx_to)
    ) locs
  );

  (* Post-merge errors.
   *
   * At this point, all dependencies have been merged and the component has been
   * linked together. Any constraints should have already been evaluated, which
   * means we can complain about things that either haven't happened yet, or
   * which require complete knowledge of tvar bounds.
   *)
  detect_sketchy_null_checks cx;
  detect_test_prop_misses cx;
  detect_unnecessary_optional_chains cx;

  cx, other_cxs

let merge_tvar =
  let open Type in
  let possible_types = Flow_js.possible_types in
  let rec collect_lowers cx seen acc = function
    | [] -> List.rev acc
    | t::ts ->
      match t with
      (* Recursively unwrap unions *)
      | DefT (_, UnionT rep) ->
        collect_lowers cx seen acc (UnionRep.members rep @ ts)
      (* Recursively unwrap unseen tvars *)
      | OpenT (_, id) ->
        if ISet.mem id seen
        then collect_lowers cx seen acc ts (* already unwrapped *)
        else collect_lowers cx (ISet.add id seen) acc (possible_types cx id @ ts)
      (* Ignore empty *)
      | DefT (_, EmptyT) -> collect_lowers cx seen acc ts
      (* Everything else becomes part of the merge typed *)
      | _ -> collect_lowers cx seen (t::acc) ts
  in
  fun cx r id ->
    let lowers = collect_lowers cx (ISet.singleton id) [] (possible_types cx id) in
    match lowers with
      | [t] -> t
      | t0::t1::ts -> DefT (r, UnionT (UnionRep.make t0 t1 ts))
      | [] ->
        let uses = Flow_js.possible_uses cx id in
        if uses = []
          then Locationless.AnyT.t
          else MergedT (r, uses)

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
*)
module ContextOptimizer = struct
  open Constraint
  open Type

  type quotient = {
    reduced_module_map : Type.t SMap.t;
    reduced_graph : node IMap.t;
    reduced_property_maps : Properties.map;
    reduced_export_maps : Exports.map;
    reduced_evaluated : Type.t IMap.t;
  }

  let empty = {
    reduced_module_map = SMap.empty;
    reduced_graph = IMap.empty;
    reduced_property_maps = Properties.Map.empty;
    reduced_export_maps = Exports.Map.empty;
    reduced_evaluated = IMap.empty;
  }

  class context_optimizer = object(self)
    inherit [quotient] Type_visitor.t as super

    val sig_hash = Xx.init ()
    method sig_hash () = Xx.digest sig_hash

    val mutable next_stable_id = 0
    method fresh_stable_id =
      let stable_id = next_stable_id in
      next_stable_id <- next_stable_id + 1;
      stable_id

    val mutable stable_tvar_ids = IMap.empty
    val mutable stable_nominal_ids = IMap.empty
    val mutable stable_eval_ids = IMap.empty
    val mutable stable_opaque_ids = IMap.empty
    val mutable stable_poly_ids = IMap.empty

    method reduce cx quotient module_ref =
      let { reduced_module_map; _ } = quotient in
      let export = Context.find_module cx module_ref in
      let reduced_module_map = SMap.add module_ref export reduced_module_map in
      self#type_ cx Neutral { quotient with reduced_module_map } export

    method! tvar cx pole quotient r id =
      let root_id, _ = Context.find_constraints cx id in
      if id == root_id then
        let { reduced_graph; _ } = quotient in
        if IMap.mem id reduced_graph then
          let stable_id = IMap.find_unsafe root_id stable_tvar_ids in
          SigHash.add_int sig_hash stable_id;
          quotient
        else
          let t = merge_tvar cx r id in
          let node = Root { rank = 0; constraints = Resolved t } in
          let reduced_graph = IMap.add id node reduced_graph in
          let () =
            let stable_id = self#fresh_stable_id in
            stable_tvar_ids <- IMap.add id stable_id stable_tvar_ids
          in
          self#type_ cx pole { quotient with reduced_graph } t
      else
        let quotient = self#tvar cx pole quotient r root_id in
        let node = Goto root_id in
        let reduced_graph = IMap.add id node quotient.reduced_graph in
        { quotient with reduced_graph }

    method! props cx pole quotient id =
      let { reduced_property_maps; _ } = quotient in
      if (Properties.Map.mem id reduced_property_maps)
      then
        let () = SigHash.add_int sig_hash (id :> int) in
        quotient
      else
        let pmap = Context.find_props cx id in
        let () = SigHash.add_props_map sig_hash pmap in
        let reduced_property_maps =
          Properties.Map.add id pmap reduced_property_maps in
        super#props cx pole { quotient with reduced_property_maps } id

    method! exports cx pole quotient id =
      let { reduced_export_maps; _ } = quotient in
      if (Exports.Map.mem id reduced_export_maps) then quotient
      else
        let tmap = Context.find_exports cx id in
        SigHash.add_exports_map sig_hash tmap;
        let reduced_export_maps =
          Exports.Map.add id tmap reduced_export_maps in
        super#exports cx pole { quotient with reduced_export_maps } id

    method! eval_id cx pole quotient id =
      let { reduced_evaluated; _ } = quotient in
      if IMap.mem id reduced_evaluated
      then
        let stable_id = IMap.find_unsafe id stable_eval_ids in
        SigHash.add_int sig_hash stable_id;
        quotient
      else
        let stable_id = self#fresh_stable_id in
        stable_eval_ids <- IMap.add id stable_id stable_eval_ids;
        match IMap.get id (Context.evaluated cx) with
        | None -> quotient
        | Some t ->
          let quotient = self#type_ cx pole quotient t in
          let reduced_evaluated = IMap.add id t reduced_evaluated in
          super#eval_id cx pole { quotient with reduced_evaluated } id

    method! dict_type cx pole quotient dicttype =
      SigHash.add_polarity sig_hash dicttype.dict_polarity;
      super#dict_type cx pole quotient dicttype

    method! type_ cx pole quotient t =
      SigHash.add_reason sig_hash (reason_of_t t);
      match t with
      | InternalT _ -> Utils_js.assert_false "internal types should not appear in signatures"
      | OpenT _ -> super#type_ cx pole quotient t
      | DefT (_, InstanceT (_, _, _, { class_id; _ })) ->
        let id =
          if Context.mem_nominal_id cx class_id
          then match IMap.get class_id stable_nominal_ids with
          | None ->
            let id = self#fresh_stable_id in
            stable_nominal_ids <- IMap.add class_id id stable_nominal_ids;
            id
          | Some id -> id
          else class_id in
        SigHash.add_int sig_hash id;
        super#type_ cx pole quotient t
      | OpaqueT (_, opaquetype) ->
        let id =
          let {opaque_id; _} = opaquetype in
          if Context.mem_nominal_id cx opaque_id
          then match IMap.get opaque_id stable_opaque_ids with
          | None ->
            let id = self#fresh_stable_id in
            stable_opaque_ids <- IMap.add opaque_id id stable_opaque_ids;
            id
          | Some id -> id
          else opaque_id
        in
        SigHash.add_int sig_hash id;
        super#type_ cx pole quotient t
      | DefT (_, PolyT (_, _, poly_id)) ->
        let id =
          if Context.mem_nominal_id cx poly_id
          then match IMap.get poly_id stable_poly_ids with
          | None ->
            let id = self#fresh_stable_id in
            stable_poly_ids <- IMap.add poly_id id stable_poly_ids;
            id
          | Some id -> id
          else poly_id
        in
        SigHash.add_int sig_hash id;
        super#type_ cx pole quotient t
      | _ ->
        SigHash.add_type sig_hash t;
        super#type_ cx pole quotient t

    method! use_type_ cx quotient use =
      SigHash.add_reason sig_hash (reason_of_use_t use);
      match use with
      | UseT (_, t) -> self#type_ cx Neutral quotient t
      | _ ->
        SigHash.add_use sig_hash use;
        super#use_type_ cx quotient use
  end

  (* walk a context from a list of exports *)
  let reduce_context cx module_refs =
    let reducer = new context_optimizer in
    let quotient = List.fold_left (reducer#reduce cx) empty module_refs in
    reducer#sig_hash (), quotient

  (* reduce a context to a "signature context" *)
  let sig_context cx module_refs =
    let sig_hash, quotient = reduce_context cx module_refs in
    Context.set_module_map cx quotient.reduced_module_map;
    Context.set_graph cx quotient.reduced_graph;
    Context.set_property_maps cx quotient.reduced_property_maps;
    Context.set_export_maps cx quotient.reduced_export_maps;
    Context.set_evaluated cx quotient.reduced_evaluated;
    Context.set_type_graph cx (
      Graph_explorer.new_graph
        (IMap.fold (fun k _ -> ISet.add k) quotient.reduced_graph ISet.empty)
    );
    sig_hash

end
