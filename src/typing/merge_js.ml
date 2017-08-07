(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module FilenameMap = Utils_js.FilenameMap

module Reqs = struct
  type impl = Loc.filename * string * string * Loc.filename
  type dep_impl = Context.t * string * string * Loc.filename
  type unchecked = string * Loc.t * Loc.filename
  type res = string * Loc.t * string * Loc.filename
  type decl = string * Loc.t * Modulename.t * Loc.filename
  type t = {
    impls: impl list;
    dep_impls: dep_impl list;
    unchecked: unchecked list;
    res: res list;
    decls: decl list;
  }

  let empty = {
    impls = [];
    dep_impls = [];
    unchecked = [];
    res = [];
    decls = [];
  }

  let add_impl impl reqs =
    { reqs with impls = impl::reqs.impls }

  let add_dep_impl dep_impl reqs =
    { reqs with dep_impls = dep_impl::reqs.dep_impls }

  let add_unchecked unchecked reqs =
    { reqs with unchecked = unchecked::reqs.unchecked }

  let add_res res reqs =
    { reqs with res = res::reqs.res }

  let add_decl decl reqs =
    { reqs with decls = decl::reqs.decls }
end

(* Connect the builtins object in master_cx to the builtins reference in some
   arbitrary cx. *)
let implicit_require_strict cx master_cx cx_to =
  let from_t = Flow_js.lookup_module master_cx Files.lib_module_ref in
  let to_t = Flow_js.lookup_module cx_to Files.lib_module_ref in
  Flow_js.flow_t cx (from_t, to_t)

(* Connect the export of cx_from to its import in cx_to. This happens in some
   arbitrary cx, so cx_from and cx_to should have already been copied to cx. *)
let explicit_impl_require_strict cx (cx_from, m, r, cx_to) =
  let from_t = Flow_js.lookup_module cx_from m in
  let to_t = Context.find_require cx_to r in
  Flow_js.flow_t cx (from_t, to_t)

(* Create the export of a resource file on the fly and connect it to its import
   in cxs_to. This happens in some arbitrary cx, so cx_to should have already
   been copied to cx. *)
let explicit_res_require_strict cx (r, loc, f, cx_to) =
  (* Recall that a resource file is not parsed, so its export doesn't depend on
     its contents, just its extension. So, we create the export of a resource
     file on the fly by looking at its extension. The general alternative of
     writing / reading these exports to / from separate contexts that live in a
     shared heap takes more time / space (linear in the number of resource
     files), so we avoid it. This optimization is analogous to what we do for
     unchecked files: we create the export (`any`) on the fly instead of writing
     / reading it to / from the context of each unchecked file. *)
  let from_t = Import_export.mk_resource_module_t cx loc f in
  let to_t = Context.find_require cx_to r in
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
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx m_name reason
    (Type.Strict reason) from_t;

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to m in
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
  let from_t = Flow_js.mk_tvar cx reason in
  Flow_js.lookup_builtin cx m_name reason
    (Type.NonstrictReturning (Some (Type.DefT (reason, Type.AnyT), from_t, false))) from_t;

  (* flow the declared module type to importing context *)
  let to_t = Context.find_require cx_to m in
  Flow_js.flow_t cx (from_t, to_t)

let detect_sketchy_null_checks cx =
  let add_error ~loc ~null_loc kind falsy_loc =
    let msg = Flow_error.ESketchyNullLint { kind; loc; null_loc; falsy_loc } in
    Flow_error.error_of_msg ~trace_reasons:[] ~op:None ~source_file:(Context.file cx) msg
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
          Option.iter exists_check.bool_loc ~f:(add_error LintSettings.SketchyBool);
        if (Option.is_none exists_excuse.number_loc) then
          Option.iter exists_check.number_loc ~f:(add_error LintSettings.SketchyNumber);
        if (Option.is_none exists_excuse.string_loc) then
          Option.iter exists_check.string_loc ~f:(add_error LintSettings.SketchyString);
        if (Option.is_none exists_excuse.mixed_loc) then
          Option.iter exists_check.mixed_loc ~f:(add_error LintSettings.SketchyMixed);
        ()
    end
  in

  Utils_js.LocMap.iter (detect_function (Context.exists_excuses cx)) (Context.exists_checks cx)

let apply_docblock_overrides (metadata: Context.metadata) docblock_info =
  let open Context in

  let local_metadata = metadata.local_metadata in

  let local_metadata = { local_metadata with jsx = Docblock.jsx docblock_info } in

  let local_metadata = match Docblock.flow docblock_info with
  | None -> local_metadata
  | Some Docblock.OptIn -> { local_metadata with checked = true; }
  | Some Docblock.OptInWeak -> { local_metadata with checked = true; weak = true }

  (* --all (which sets metadata.checked = true) overrides @noflow, so there are
     currently no scenarios where we'd change checked = true to false. in the
     future, there may be a case where checked defaults to true (but is not
     forced to be true ala --all), but for now we do *not* want to force
     checked = false here. *)
  | Some Docblock.OptOut -> local_metadata
  in

  let local_metadata = match Docblock.preventMunge docblock_info with
  | Some value -> { local_metadata with munge_underscores = not value; }
  | None -> local_metadata
  in

  { metadata with local_metadata }


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
let merge_component_strict ~metadata ~lint_settings ~require_loc_maps
  ~get_ast_unsafe ~get_docblock_unsafe
  component reqs dep_cxs master_cx =

  let rev_cxs, impl_cxs = List.fold_left (fun (cxs, impl_cxs) filename ->
    let ast = get_ast_unsafe filename in
    let info = get_docblock_unsafe filename in
    let metadata = apply_docblock_overrides metadata info in
    let require_loc_map = FilenameMap.find_unsafe filename require_loc_maps in
    let cx = Type_inference_js.infer_ast ast
      ~metadata ~filename ~lint_settings ~require_loc_map
    in
    cx::cxs, FilenameMap.add filename cx impl_cxs
  ) ([], FilenameMap.empty) component in
  let cxs = List.rev rev_cxs in

  let cx, other_cxs = List.hd cxs, List.tl cxs in

  Flow_js.Cache.clear();

  dep_cxs |> List.iter (Context.merge_into cx);
  other_cxs |> List.iter (Context.merge_into cx);
  Context.merge_into cx master_cx;

  let open Reqs in

  reqs.impls |> List.iter (fun (fn_from, m, r, fn_to) ->
    let cx_from = FilenameMap.find_unsafe fn_from impl_cxs in
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    explicit_impl_require_strict cx (cx_from, m, r, cx_to);
    Context.add_module cx m (Context.find_module cx_from m)
  );

  reqs.dep_impls |> List.iter (fun (cx_from, m, r, fn_to) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    explicit_impl_require_strict cx (cx_from, m, r, cx_to)
  );

  reqs.res |> List.iter (fun (r, loc, f, fn_to) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    explicit_res_require_strict cx (r, loc, f, cx_to)
  );

  reqs.decls |> List.iter (fun (m, loc, resolved_m, fn_to) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    explicit_decl_require_strict cx (m, loc, resolved_m, cx_to)
  );

  reqs.unchecked |> List.iter (fun (m, loc, fn_to) ->
    let cx_to = FilenameMap.find_unsafe fn_to impl_cxs in
    explicit_unchecked_require_strict cx (m, loc, cx_to)
  );

  other_cxs |> List.iter (implicit_require_strict cx master_cx);
  implicit_require_strict cx master_cx cx;

  detect_sketchy_null_checks cx;

  cx

(* After merging dependencies into a context (but before optimizing the
   context), it is important to restore the parts of the context that were
   copied from other, already optimized contexts (dep_cxs and master_cx, see
   above comment for details on what they mean). Indeed, merging is an
   imperative process, and there is no guarantee that those parts of the context
   would have remained unchanged.

   Restoration maintains consistency for "diamond-shaped" dependency relations:
   it forces two contexts B and C that depend on the same context A to agree on
   the meaning of the parts of A they share (and that meaning is dictated by A
   itself), and so some context D that depends on both B and C (and perhaps A
   too) is never confused when merging them.
*)
let restore cx dep_cxs master_cx =
  dep_cxs |> List.iter (Context.merge_into cx);
  Context.merge_into cx master_cx

(* Given a sig context, it makes sense to clear the parts that are shared with
   the master sig context. Why? The master sig context, which contains global
   declarations, is an implicit dependency for every file, and so will be
   "merged in" anyway, thus making those shared parts redundant to carry around
   in other sig contexts. This saves a lot of shared memory as well as
   deserialization time. *)
let clear_master_shared cx master_cx =
  Context.set_graph cx (Context.graph cx |> IMap.filter (fun id _ -> not
    (IMap.mem id (Context.graph master_cx))));
  Context.set_property_maps cx (Context.property_maps cx |> Type.Properties.Map.filter (fun id _ -> not
    (Type.Properties.Map.mem id (Context.property_maps master_cx))));
  Context.set_envs cx (Context.envs cx |> IMap.filter (fun id _ -> not
    (IMap.mem id (Context.envs master_cx))));
  Context.set_evaluated cx (Context.evaluated cx |> IMap.filter (fun id _ -> not
    (IMap.mem id (Context.evaluated master_cx))))

let merge_lib_file cx master_cx =
  Context.merge_into master_cx cx;
  implicit_require_strict master_cx master_cx cx;

  let errs = Context.errors cx in
  Context.remove_all_errors cx;

  errs, Context.error_suppressions cx, Context.lint_settings cx

let lowers_of_tvar =
  let open Type in
  let possible_types = Flow_js.possible_types in
  let finish r = function
    | [] -> Locationless.AnyT.t
    | [t] -> t
    | t0::t1::ts -> DefT (r, UnionT (UnionRep.make t0 t1 ts))
  in
  let rec merge cx r seen acc = function
    | [] -> finish r (List.rev acc)
    | t::ts ->
      match t with
      (* Recursively unwrap unions *)
      | DefT (_, UnionT rep) ->
        merge cx r seen acc (UnionRep.members rep @ ts)
      (* Recursively unwrap unseen tvars *)
      | OpenT (_, id) ->
        if ISet.mem id seen
        then merge cx r seen acc ts (* already unwrapped *)
        else merge cx r (ISet.add id seen) acc (possible_types cx id @ ts)
      (* Ignore empty *)
      | DefT (_, EmptyT) -> merge cx r seen acc ts
      (* Everything else becomes part of the merge typed *)
      | _ -> merge cx r seen (t::acc) ts
  in
  fun cx r id -> merge cx r (ISet.singleton id) [] (possible_types cx id)

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
    reduced_envs : Context.env IMap.t;
    reduced_evaluated : Type.t IMap.t;
    sig_hash : SigHash.t;
  }

  let empty = {
    reduced_module_map = SMap.empty;
    reduced_graph = IMap.empty;
    reduced_property_maps = Properties.Map.empty;
    reduced_export_maps = Exports.Map.empty;
    reduced_envs = IMap.empty;
    reduced_evaluated = IMap.empty;
    sig_hash = SigHash.empty;
  }

  class context_optimizer = object(self)
    inherit [quotient] Type_visitor.t as super

    val mutable next_stable_id = 0
    method fresh_stable_id =
      let stable_id = next_stable_id in
      next_stable_id <- next_stable_id + 1;
      stable_id

    val mutable stable_tvar_ids = IMap.empty
    val mutable stable_propmap_ids = Properties.Map.empty
    val mutable stable_nominal_ids = IMap.empty
    val mutable stable_eval_ids = IMap.empty

    method reduce cx quotient module_ref =
      let { reduced_module_map; _ } = quotient in
      let export = Flow_js.lookup_module cx module_ref in
      let reduced_module_map = SMap.add module_ref export reduced_module_map in
      self#type_ cx { quotient with reduced_module_map } export

    method! tvar cx quotient r id =
      let { reduced_graph; sig_hash; _ } = quotient in
      if (IMap.mem id reduced_graph)
      then
        let stable_id = IMap.find_unsafe id stable_tvar_ids in
        let sig_hash = SigHash.add stable_id sig_hash in
        { quotient with sig_hash }
      else
        let t = lowers_of_tvar cx r id in
        let node = Root { rank = 0; constraints = Resolved t } in
        let reduced_graph = IMap.add id node reduced_graph in
        let stable_id = self#fresh_stable_id in
        stable_tvar_ids <- IMap.add id stable_id stable_tvar_ids;
        self#type_ cx { quotient with reduced_graph } t

    method! props cx quotient id =
      let { reduced_property_maps; sig_hash; _ } = quotient in
      if (Properties.Map.mem id reduced_property_maps)
      then
        let stable_id = Properties.Map.find_unsafe id stable_propmap_ids in
        let sig_hash = SigHash.add stable_id sig_hash in
        { quotient with sig_hash }
      else
        let pmap = Context.find_props cx id in
        let sig_hash = SigHash.add_props_map pmap sig_hash in
        let reduced_property_maps =
          Properties.Map.add id pmap reduced_property_maps in
        let stable_id = self#fresh_stable_id in
        stable_propmap_ids <- Properties.Map.add id stable_id stable_propmap_ids;
        super#props cx { quotient with reduced_property_maps; sig_hash } id

    method! exports cx quotient id =
      let { reduced_export_maps; sig_hash; _ } = quotient in
      if (Exports.Map.mem id reduced_export_maps) then quotient
      else
        let tmap = Context.find_exports cx id in
        let sig_hash = SigHash.add_exports_map tmap sig_hash in
        let reduced_export_maps =
          Exports.Map.add id tmap reduced_export_maps in
        super#exports cx { quotient with reduced_export_maps; sig_hash } id

    method! eval_id cx quotient id =
      let { reduced_evaluated; sig_hash; _ } = quotient in
      if IMap.mem id reduced_evaluated
      then
        let stable_id = IMap.find_unsafe id stable_eval_ids in
        let sig_hash = SigHash.add stable_id sig_hash in
        { quotient with sig_hash }
      else
        let stable_id = self#fresh_stable_id in
        stable_eval_ids <- IMap.add id stable_id stable_eval_ids;
        match IMap.get id (Context.evaluated cx) with
        | None -> quotient
        | Some t ->
          let t = match t with
          | OpenT (r, id) -> lowers_of_tvar cx r id
          | t -> t
          in
          let reduced_evaluated = IMap.add id t reduced_evaluated in
          super#eval_id cx { quotient with reduced_evaluated } id

    method! fun_type cx quotient funtype =
      let id = funtype.closure_t in
      if id = 0 then super#fun_type cx quotient funtype
      else
        let { reduced_envs; _ } = quotient in
        let closure = IMap.find_unsafe id (Context.envs cx) in
        let reduced_envs = IMap.add id closure reduced_envs in
        super#fun_type cx { quotient with reduced_envs } funtype

    method! dict_type cx quotient dicttype =
      let { sig_hash; _ } = quotient in
      let sig_hash = SigHash.add dicttype.dict_polarity sig_hash in
      super#dict_type cx { quotient with sig_hash } dicttype

    method! type_ cx quotient t =
      let quotient = { quotient with
        sig_hash = SigHash.add (reason_of_t t) quotient.sig_hash
      } in
      match t with
      | OpenT _ -> super#type_ cx quotient t
      | DefT (_, InstanceT (_, _, _, { class_id; _ })) ->
        let { sig_hash; _ } = quotient in
        let id =
          if Context.mem_nominal_id cx class_id
          then match IMap.get class_id stable_nominal_ids with
          | None ->
            let id = self#fresh_stable_id in
            stable_nominal_ids <- IMap.add class_id id stable_nominal_ids;
            id
          | Some id -> id
          else class_id in
        let sig_hash = SigHash.add id sig_hash in
        super#type_ cx { quotient with sig_hash } t
      | _ ->
        let { sig_hash; _ } = quotient in
        let sig_hash = SigHash.add_type t sig_hash in
        super#type_ cx { quotient with sig_hash } t
  end

  (* walk a context from a list of exports *)
  let reduce_context cx module_refs =
    let reducer = new context_optimizer in
    List.fold_left (reducer#reduce cx) empty module_refs

  (* reduce a context to a "signature context" *)
  let sig_context cx module_refs =
    let quotient = reduce_context cx module_refs in
    Context.set_module_map cx quotient.reduced_module_map;
    Context.set_graph cx quotient.reduced_graph;
    Context.set_property_maps cx quotient.reduced_property_maps;
    Context.set_export_maps cx quotient.reduced_export_maps;
    Context.set_envs cx quotient.reduced_envs;
    Context.set_evaluated cx quotient.reduced_evaluated;
    Context.set_type_graph cx (
      Graph_explorer.new_graph
        (IMap.fold (fun k _ -> ISet.add k) quotient.reduced_graph ISet.empty)
    );
    quotient.sig_hash

end
