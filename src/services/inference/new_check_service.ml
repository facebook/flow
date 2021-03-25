(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type module_ref = string

type require = module_ref * ALoc.t Nel.t * Modulename.t

type check_file =
  File_key.t ->
  require list ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  Loc.t Flow_ast.Comment.t list ->
  File_sig.With_ALoc.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

(* Check will lazily create types for the checked file's dependencies. These
 * types are created in the dependency's context and need to be copied into the
 * checked file's context.
 *
 * This visitor walks a type in the dependency's context (src_cx) and copies
 * any tvars, property maps, evaluated types, etc. into the check file's context
 * (dst_cx).
 *
 * When calculating a direct dependency's types, we might also need to construct
 * types for a transitive dependency. These types are similarly created in the
 * transitive dependency's context, then copied into the dependency's context,
 * and so on.
 *
 * Finally, due to cycles, it's possile that src_cx and dst_cx share the same
 * component cx, and thus have the same tvar graph, property maps, etc. Happily,
 * this does not complicate the implementation, as the mem checks and early
 * returns on each method override are sufficient.
 *
 * Crucially, this copying process is shallow. We only copy what is necessary to
 * interpret a given type. *)
let copier =
  let open Type in
  let open Constraint in
  object (self)
    inherit [Context.t, infer_phase] Type_visitor.t as super

    (* Copying a tvar produces a FullyResolved tvar in the dst cx, which
     * contains an unevaluated thunk. The laziness here makes the copying
     * shallow. Note that the visitor stops at root tvars here and only resumes
     * if the thunk is forced.
     *
     * Copying a tvar also has the effect of optimizing the tvar in the src cx
     * if necessary. That is, Unresolved and Resolved tvars become FullyResolved.
     * *)
    method! tvar src_cx pole dst_cx r id =
      let dst_graph = Context.graph dst_cx in
      if IMap.mem id dst_graph then
        dst_cx
      else
        let (root_id, constraints) = Context.find_constraints src_cx id in
        if id == root_id then (
          let thunk =
            match constraints with
            | Unresolved { lower; _ } ->
              let ts = TypeMap.keys lower |> List.filter Type.is_proper_def in
              let t =
                match ts with
                | [t] -> t
                | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)
                | [] -> Unsoundness.merged_any r
              in
              let node = Root { rank = 0; constraints = FullyResolved (unknown_use, lazy t) } in
              Context.add_tvar src_cx id node;
              lazy
                (let (_ : Context.t) = self#type_ src_cx pole dst_cx t in
                 t)
            | Resolved (_, t) ->
              let node = Root { rank = 0; constraints = FullyResolved (unknown_use, lazy t) } in
              Context.add_tvar src_cx id node;
              lazy
                (let (_ : Context.t) = self#type_ src_cx pole dst_cx t in
                 t)
            | FullyResolved (_, t) ->
              lazy
                (let t = Lazy.force t in
                 let (_ : Context.t) = self#type_ src_cx pole dst_cx t in
                 t)
          in
          let node = Root { rank = 0; constraints = FullyResolved (unknown_use, thunk) } in
          Context.set_graph dst_cx (IMap.add id node dst_graph);
          dst_cx
        ) else (
          Context.set_graph dst_cx (IMap.add id (Goto root_id) dst_graph);
          self#tvar src_cx pole dst_cx r root_id
        )

    method! props src_cx pole dst_cx id =
      let dst_property_maps = Context.property_maps dst_cx in
      if Properties.Map.mem id dst_property_maps then
        dst_cx
      else
        let props = Context.find_props src_cx id in
        Context.set_property_maps dst_cx (Properties.Map.add id props dst_property_maps);
        super#props src_cx pole dst_cx id

    method! call_prop src_cx pole dst_cx id =
      let dst_call_props = Context.call_props dst_cx in
      if IMap.mem id dst_call_props then
        dst_cx
      else
        let t = Context.find_call src_cx id in
        Context.set_call_props dst_cx (IMap.add id t dst_call_props);
        super#call_prop src_cx pole dst_cx id

    method! exports src_cx pole dst_cx id =
      let dst_export_maps = Context.export_maps dst_cx in
      if Exports.Map.mem id dst_export_maps then
        dst_cx
      else
        let map = Context.find_exports src_cx id in
        Context.set_export_maps dst_cx (Exports.Map.add id map dst_export_maps);
        super#exports src_cx pole dst_cx id

    method! eval_id src_cx pole dst_cx id =
      match Eval.Map.find_opt id (Context.evaluated src_cx) with
      | None -> dst_cx
      | Some t ->
        let dst_evaluated = Context.evaluated dst_cx in
        if Eval.Map.mem id dst_evaluated then
          dst_cx
        else (
          Context.set_evaluated dst_cx (Eval.Map.add id t dst_evaluated);
          super#eval_id src_cx pole dst_cx id
        )
  end

let copy_into dst_cx src_cx t =
  let (_ : Context.t) = copier#type_ src_cx Polarity.Positive dst_cx t in
  ()

(* Helper to create a lazy type converted from a signature. The returned tvar
 * contains a lazy thunk which evaluates to a type. While the lazy thunk is
 * under evaluation, we swap out the tvar's constraint with a fresh unresolved
 * root. This deals with the recursive case where forcing `resolved` loops back
 * to the tvar being defined. *)
let mk_sig_tvar cx reason resolved =
  let open Type in
  let open Constraint in
  let id = Reason.mk_id () in
  let tvar = OpenT (reason, id) in
  let t =
    lazy
      ( Context.add_tvar cx id (new_unresolved_root ());
        let t = Lazy.force resolved in
        Flow_js.unify cx tvar t;
        t )
  in
  Context.add_tvar cx id (Root { rank = 0; constraints = FullyResolved (unknown_use, t) });
  tvar

let unknown_module_t cx mref provider =
  let react_server_module_err = provider = Modulename.String Type.react_server_module_ref in
  let desc = Reason.RCustom mref in
  let m_name =
    match provider with
    | Modulename.String ("react" | "React") when Context.in_react_server_component_file cx ->
      Type.react_server_module_ref
    | _ -> Modulename.to_string provider
  in
  let m_name = Reason.internal_module_name m_name in
  fun loc ->
    if react_server_module_err then
      Flow_js.add_output cx (Error_message.EImportInternalReactServerModule loc);
    let reason = Reason.mk_reason desc loc in
    Flow_js.lookup_builtin_strict cx m_name reason

let resource_module_t cx f loc = Import_export.mk_resource_module_t cx loc f

let unchecked_module_t cx mref =
  let desc = Reason.RUntypedModule mref in
  let m_name = Reason.internal_module_name mref in
  fun loc ->
    let reason = Reason.mk_reason desc loc in
    let default = Type.(AnyT (reason, Untyped)) in
    Flow_js.lookup_builtin_with_default cx m_name default

let get_lint_severities metadata options =
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  Merge_js.get_lint_severities metadata strict_mode lint_severities

(* This function is designed to be applied up to the unit argument and returns a
 * function which can be called repeatedly. The returned function closes over an
 * environment which defines caches that can be re-used when checking multiple
 * files. *)
let mk_check_file ~options ~reader () =
  let open Type_sig_collections in
  let module Merge = Type_sig_merge in
  let module Pack = Type_sig_pack in
  let audit = Expensive.ok in

  let get_file = Module_heaps.Reader_dispatcher.get_file ~reader ~audit in
  let get_docblock_unsafe = Parsing_heaps.Reader_dispatcher.get_docblock_unsafe ~reader in
  let get_type_sig_unsafe = Parsing_heaps.Reader_dispatcher.get_type_sig_unsafe ~reader in
  let get_info_unsafe = Module_heaps.Reader_dispatcher.get_info_unsafe ~reader ~audit in
  let get_aloc_table_unsafe = Parsing_heaps.Reader_dispatcher.get_aloc_table_unsafe ~reader in
  let find_leader = Context_heaps.Reader_dispatcher.find_leader ~reader in
  let find_resolved_module = Module_js.find_resolved_module ~reader ~audit in

  let master_cx = Context_heaps.Reader_dispatcher.find_master ~reader in
  let ccx_cache : (File_key.t, Context.component_t) Hashtbl.t = Hashtbl.create 0 in
  let file_cache : (File_key.t, Merge.file) Hashtbl.t = Hashtbl.create 0 in

  let base_metadata = Context.metadata_of_options options in

  let connect_builtins ccx cx =
    let open Context in
    merge_into ccx master_cx.master_sig_cx;
    set_builtins cx master_cx.builtins
  in

  (* Create a merging context for a dependency of a checked file. These contexts
   * are used when converting signatures to types. *)
  let create_dep_cx file_key =
    let leader = find_leader file_key in
    let docblock = get_docblock_unsafe file_key in
    let metadata = Context.docblock_overrides docblock base_metadata in
    let module_ref = Reason.OrdinaryName (Files.module_ref file_key) in
    let aloc_table = lazy (get_aloc_table_unsafe file_key) in
    match Hashtbl.find_opt ccx_cache leader with
    | Some ccx -> Context.make ccx metadata file_key aloc_table module_ref Context.Checking
    | None ->
      let ccx = Context.make_ccx () in
      Hashtbl.add ccx_cache leader ccx;
      let cx = Context.make ccx metadata file_key aloc_table module_ref Context.Checking in
      connect_builtins ccx cx;
      cx
  in

  (* Create a type representing the exports of a dependency. For checked
   * dependencies, we will create a "sig tvar" with a lazy thunk that evaluates
   * to a ModuleT type. *)
  let rec dep_module_t cx mref provider =
    match get_file provider with
    | None -> unknown_module_t cx mref provider
    | Some (File_key.ResourceFile f) -> resource_module_t cx f
    | Some dep_file ->
      let { Module_heaps.checked; parsed; _ } = get_info_unsafe dep_file in
      if checked && parsed then
        sig_module_t cx dep_file
      else
        unchecked_module_t cx mref
  and sig_module_t cx file_key _loc =
    let file =
      match Hashtbl.find_opt file_cache file_key with
      | Some file -> file
      | None ->
        let file = dep_file file_key in
        Hashtbl.add file_cache file_key file;
        file
    in
    let t = file.Merge.exports () in
    copy_into cx file.Merge.cx t;
    t
  (* Create a Type_sig_merge.file record for a dependency, which we use to
   * convert signatures into types. This function reads the signature for a file
   * from shared memory and creates thunks (either lazy tvars or lazy types)
   * that resolve to types. *)
  and dep_file file_key =
    let source = Some file_key in

    let aloc (loc : Locs.index) = ALoc.ALocRepresentationDoNotUse.make_keyed source (loc :> int) in

    let aloc =
      if Options.abstract_locations options then
        aloc
      else
        let aloc_table = lazy (get_aloc_table_unsafe file_key) in
        (fun loc -> aloc loc |> ALoc.to_loc aloc_table |> ALoc.of_loc)
    in

    let cx = create_dep_cx file_key in

    (* Currently, Flow stores type signatures as a single, serialized OCaml
     * value in shared memory. This means that we fetch and deserialize the
     * entire type signature for a file when we need any part of it. Once type
     * signatures are stored more granularly in the shared heap, we can change
     * this logic read signature data from the heap granularly as well. *)
    let {
      Packed_type_sig.Module.exports;
      export_def;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs;
      patterns;
    } =
      get_type_sig_unsafe file_key
    in

    let dependencies =
      let f mref =
        let provider = find_resolved_module file_key mref in
        (mref, dep_module_t cx mref provider)
      in
      Module_refs.map f module_refs
    in

    let exports file_rec =
      let reason =
        let file_loc = ALoc.of_loc { Loc.none with Loc.source } in
        Reason.(mk_reason RExports file_loc)
      in
      let resolved =
        lazy (Pack.map_exports aloc exports |> Merge.merge_exports (Lazy.force file_rec) reason)
      in
      let t = mk_sig_tvar cx reason resolved in
      (fun () -> t)
    in

    let export_def file_rec =
      lazy
        (match export_def with
        | None -> None
        | Some def -> Some (Pack.map_packed aloc def |> Merge.merge (Lazy.force file_rec)))
    in

    let local_def file_rec def =
      let def = Pack.map_packed_def aloc def in
      let reason = Merge.def_reason def in
      let resolved = lazy (Merge.merge_def (Lazy.force file_rec) reason def) in
      let t = mk_sig_tvar cx reason resolved in
      fun () ->
        let loc = Type_sig.def_id_loc def in
        let name = Type_sig.def_name def in
        (loc, name, t)
    in

    let remote_ref file_rec remote_ref =
      let remote_ref = Pack.map_remote_ref aloc remote_ref in
      let reason = Merge.remote_ref_reason remote_ref in
      let resolved = lazy (Merge.merge_remote_ref (Lazy.force file_rec) reason remote_ref) in
      let t = mk_sig_tvar cx reason resolved in
      fun () ->
        let loc = Pack.remote_ref_loc remote_ref in
        let name = Pack.remote_ref_name remote_ref in
        (loc, name, t)
    in

    let pattern_def file_rec def =
      lazy (Pack.map_packed aloc def |> Merge.merge (Lazy.force file_rec))
    in

    let pattern file_rec p =
      lazy (Pack.map_pattern aloc p |> Merge.merge_pattern (Lazy.force file_rec))
    in

    let rec file_rec =
      lazy
        {
          Merge.key = file_key;
          cx;
          dependencies;
          exports = exports file_rec;
          export_def = export_def file_rec;
          local_defs = Local_defs.map (local_def file_rec) local_defs;
          remote_refs = Remote_refs.map (remote_ref file_rec) remote_refs;
          pattern_defs = Pattern_defs.map (pattern_def file_rec) pattern_defs;
          patterns = Patterns.map (pattern file_rec) patterns;
        }
    in
    Lazy.force file_rec
  in

  let connect_require cx (mref, locs, provider) =
    let module_t = dep_module_t cx mref provider in
    let connect loc =
      let module_t = module_t loc in
      let require_t = Context.find_require cx loc in
      Flow_js.flow_t cx (module_t, require_t)
    in
    Nel.iter connect locs
  in

  fun file_key requires ast comments file_sig docblock aloc_table ->
    let ccx = Context.make_ccx () in
    let metadata = Context.docblock_overrides docblock base_metadata in
    let module_ref = Reason.OrdinaryName (Files.module_ref file_key) in
    let cx = Context.make ccx metadata file_key aloc_table module_ref Context.Checking in
    connect_builtins ccx cx;
    let lint_severities = get_lint_severities metadata options in
    Type_inference_js.add_require_tvars cx file_sig;
    Context.set_local_env cx file_sig.File_sig.With_ALoc.exported_locals;
    List.iter (connect_require cx) requires;
    let typed_ast = Type_inference_js.infer_ast cx file_key comments ast ~lint_severities in
    Merge_js.post_merge_checks cx master_cx ast typed_ast metadata file_sig;
    (cx, typed_ast)
