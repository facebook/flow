(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type module_ref = string

type require = module_ref * ALoc.t Nel.t * Parsing_heaps.resolved_module

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
    inherit [Context.t] Type_visitor.t as super

    (* Copying a tvar produces a FullyResolved tvar in the dst cx, which
     * contains an unevaluated thunk. The laziness here makes the copying
     * shallow. Note that the visitor stops at root tvars here and only resumes
     * if the thunk is forced. *)
    method! tvar src_cx pole dst_cx r id =
      let dst_graph = Context.graph dst_cx in
      if IMap.mem id dst_graph then
        dst_cx
      else
        let (root_id, constraints) = Context.find_constraints src_cx id in
        if id == root_id then (
          let t =
            match constraints with
            | Unresolved _
            | Resolved _ ->
              failwith "unexpected unresolved constraint"
            | FullyResolved (_, thunk) ->
              lazy
                (let (lazy t) = thunk in
                 let (_ : Context.t) = self#type_ src_cx pole dst_cx t in
                 t
                )
          in
          let constraints = FullyResolved (unknown_use, t) in
          let node = Root { rank = 0; constraints } in
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

let unknown_module_t cx mref m =
  let react_server_module_err = m = Modulename.String Type.react_server_module_ref in
  let desc = Reason.RCustom mref in
  let m_name =
    match m with
    | Modulename.String ("react" | "React") when Context.in_react_server_component_file cx ->
      Type.react_server_module_ref
    | _ -> Modulename.to_string m
  in
  let m_name = Reason.internal_module_name m_name in
  fun loc ->
    if react_server_module_err then
      Flow_js.add_output cx (Error_message.EImportInternalReactServerModule loc);
    let reason = Reason.mk_reason desc loc in
    Flow_js_utils.lookup_builtin_strict cx m_name reason

let unchecked_module_t cx mref =
  let desc = Reason.RUntypedModule mref in
  let m_name = Reason.internal_module_name mref in
  fun loc ->
    let reason = Reason.mk_reason desc loc in
    let default = Type.(AnyT (reason, Untyped)) in
    Flow_js_utils.lookup_builtin_with_default cx m_name default

let get_lint_severities metadata options =
  let lint_severities = Options.lint_severities options in
  let strict_mode = Options.strict_mode options in
  Merge_js.get_lint_severities metadata strict_mode lint_severities

let resolve_require_id = Flow_js.resolve_id

[@@@warning "-60"]

(* Don't use Flow_js directly from Check_service. Instead, encode the respective
 * logic in ConsGen. *)
module Flow_js = struct end

[@@@warning "+60"]

module type READER = sig
  type provider

  type typed_parse

  val get_master_cx : unit -> Context.master_context

  val get_provider : Modulename.t -> provider option

  val get_file_key : provider -> File_key.t

  val get_typed_parse : provider -> typed_parse option

  val get_leader_key : typed_parse -> File_key.t

  val get_aloc_table : typed_parse -> ALoc.table

  val get_docblock : typed_parse -> Docblock.t

  val get_type_sig_buf : typed_parse -> Type_sig_bin.buf

  val get_resolved_modules : typed_parse -> Parsing_heaps.resolved_module SMap.t
end

let mk_heap_reader reader =
  let module Reader = struct
    module Heap = SharedMem.NewAPI

    type provider = File_key.t * Parsing_heaps.file_addr

    type typed_parse = File_key.t * [ `typed ] Parsing_heaps.parse_addr

    let get_master_cx () = Context_heaps.Reader_dispatcher.find_master ~reader

    let get_provider m =
      match Parsing_heaps.Reader_dispatcher.get_provider ~reader m with
      | None -> None
      | Some dep_addr ->
        let file_key =
          try Parsing_heaps.read_file_key dep_addr with
          | SharedMem.Invalid_header _ as exn ->
            let exn = Exception.wrap exn in
            Exception.raise_with_backtrace
              (Failure
                 (Printf.sprintf
                    "Invalid provider for %s: %s"
                    (Modulename.to_string m)
                    (Exception.get_ctor_string exn)
                 )
              )
              exn
        in
        Some (file_key, dep_addr)

    let get_file_key (file_key, _) = file_key

    let get_typed_parse (file_key, dep_addr) =
      match Parsing_heaps.Reader_dispatcher.get_typed_parse ~reader dep_addr with
      | None -> None
      | Some parse -> Some (file_key, parse)

    let get_leader_key (file_key, parse) =
      Parsing_heaps.Reader_dispatcher.get_leader_unsafe ~reader file_key parse
      |> Parsing_heaps.read_file_key

    let get_aloc_table (file_key, parse) = Parsing_heaps.read_aloc_table_unsafe file_key parse

    let get_docblock (file_key, parse) = Parsing_heaps.read_docblock_unsafe file_key parse

    let get_type_sig_buf (_, parse) = Heap.type_sig_buf (Option.get (Heap.get_type_sig parse))

    let get_resolved_modules (file_key, parse) =
      Parsing_heaps.Reader_dispatcher.get_resolved_modules_unsafe ~reader file_key parse
  end in
  (module Reader : READER)

(* This function is designed to be applied up to the unit argument and returns a
 * function which can be called repeatedly. The returned function closes over an
 * environment which defines caches that can be re-used when checking multiple
 * files. *)
let mk_check_file (module Reader : READER) ~options ~cache () =
  let open Type_sig_collections in
  let module ConsGen = Annotation_inference.ConsGen in
  let module Merge = Type_sig_merge in
  let module Pack = Type_sig_pack in
  let module Bin = Type_sig_bin in
  let master_cx = Reader.get_master_cx () in

  let base_metadata = Context.metadata_of_options options in

  (* Create a type representing the exports of a dependency. For checked
   * dependencies, we will create a "sig tvar" with a lazy thunk that evaluates
   * to a ModuleT type. *)
  let rec dep_module_t cx mref = function
    | Error mapped_name ->
      let m = Option.value mapped_name ~default:mref in
      unknown_module_t cx mref (Modulename.String m)
    | Ok m ->
      (match Reader.get_provider m with
      | None -> unknown_module_t cx mref m
      | Some provider ->
        (match Reader.get_file_key provider with
        | File_key.ResourceFile f -> Merge.merge_resource_module_t cx f
        | dep_file ->
          (match Reader.get_typed_parse provider with
          | Some parse -> sig_module_t cx dep_file parse
          | None -> unchecked_module_t cx mref)))
  and sig_module_t cx file_key parse _loc =
    let create_file = dep_file file_key parse in
    let leader = lazy (Reader.get_leader_key parse) in
    let file = Check_cache.find_or_create cache ~leader ~master_cx ~create_file file_key in
    let t = file.Type_sig_merge.exports in
    copy_into cx file.Type_sig_merge.cx t;
    t
  (* Create a Type_sig_merge.file record for a dependency, which we use to
   * convert signatures into types. This function reads the signature for a file
   * from shared memory and creates thunks (either lazy tvars or lazy types)
   * that resolve to types. *)
  and dep_file file_key parse ccx =
    let source = Some file_key in

    let aloc_table = lazy (Reader.get_aloc_table parse) in

    let aloc (loc : Locs.index) = ALoc.ALocRepresentationDoNotUse.make_keyed source (loc :> int) in

    let aloc =
      if Options.abstract_locations options then
        aloc
      else
        fun loc ->
      aloc loc |> ALoc.to_loc aloc_table |> ALoc.of_loc
    in

    let buf = Reader.get_type_sig_buf parse in

    let cx =
      let docblock = Reader.get_docblock parse in
      let metadata = Context.docblock_overrides docblock base_metadata in
      Context.make ccx metadata file_key aloc_table Context.Merging
    in

    let dependencies =
      let resolved_modules = Reader.get_resolved_modules parse in
      let f buf pos =
        let mref = Bin.read_str buf pos in
        let m = SMap.find mref resolved_modules in
        (mref, dep_module_t cx mref m)
      in
      let pos = Bin.module_refs buf in
      Bin.read_tbl_generic f buf pos Module_refs.init
    in

    let exports file_rec =
      let file_loc = ALoc.of_loc { Loc.none with Loc.source = Some file_key } in
      let reason = Reason.(mk_reason RExports file_loc) in
      let type_export buf pos =
        lazy
          (Bin.read_hashed Bin.read_type_export buf pos
          |> Pack.map_type_export aloc
          |> Merge.merge_type_export (Lazy.force file_rec) reason
          )
      in
      let cjs_exports buf pos =
        lazy
          (Bin.read_hashed Bin.read_packed buf pos
          |> Pack.map_packed aloc
          |> Merge.merge SMap.empty (Lazy.force file_rec)
          )
      in
      let es_export buf pos =
        lazy
          (Bin.read_hashed Bin.read_es_export buf pos
          |> Pack.map_export aloc
          |> Merge.merge_export (Lazy.force file_rec)
          )
      in
      let cjs_module buf pos =
        let (Pack.CJSModuleInfo { type_export_keys; type_stars; strict }) =
          Bin.cjs_module_info buf pos
          |> Bin.read_hashed Bin.read_cjs_info buf
          |> Pack.map_cjs_module_info aloc
        in
        let type_exports = Bin.cjs_module_type_exports buf pos |> Bin.read_tbl type_export buf in
        let exports = Bin.cjs_module_exports buf pos |> Bin.read_opt cjs_exports buf in
        let type_exports =
          let f acc name export = SMap.add name export acc in
          Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
        in
        Type_sig_merge.CJSExports { type_exports; exports; type_stars; strict }
      in
      let es_module buf pos =
        let (Pack.ESModuleInfo { type_export_keys; export_keys; type_stars; stars; strict }) =
          Bin.es_module_info buf pos
          |> Bin.read_hashed Bin.read_es_info buf
          |> Pack.map_es_module_info aloc
        in
        let type_exports = Bin.es_module_type_exports buf pos |> Bin.read_tbl type_export buf in
        let exports = Bin.es_module_exports buf pos |> Bin.read_tbl es_export buf in
        let type_exports =
          let f acc name export = SMap.add name export acc in
          Base.Array.fold2_exn ~init:SMap.empty ~f type_export_keys type_exports
        in
        let exports =
          let f acc name export = SMap.add name export acc in
          Base.Array.fold2_exn ~init:SMap.empty ~f export_keys exports
        in
        Type_sig_merge.ESExports { type_exports; exports; type_stars; stars; strict }
      in
      let resolved =
        lazy
          (Bin.module_kind buf
          |> Bin.read_module_kind cjs_module es_module buf
          |> Merge.merge_exports (Lazy.force file_rec) reason
          )
      in
      ConsGen.mk_sig_tvar cx reason resolved
    in

    let local_def file_rec buf pos =
      lazy
        (let def = Pack.map_packed_def aloc (Bin.read_local_def buf pos) in
         let loc = Type_sig.def_id_loc def in
         let name = Type_sig.def_name def in
         let reason = Type_sig_merge.def_reason def in
         let resolved = lazy (Merge.merge_def (Lazy.force file_rec) reason def) in
         let t = ConsGen.mk_sig_tvar cx reason resolved in
         (loc, name, t)
        )
    in

    let remote_ref file_rec buf pos =
      lazy
        (let remote_ref = Pack.map_remote_ref aloc (Bin.read_remote_ref buf pos) in
         let loc = Pack.remote_ref_loc remote_ref in
         let name = Pack.remote_ref_name remote_ref in
         let reason = Type_sig_merge.remote_ref_reason remote_ref in
         let resolved = lazy (Merge.merge_remote_ref (Lazy.force file_rec) reason remote_ref) in
         let t = ConsGen.mk_sig_tvar cx reason resolved in
         (loc, name, t)
        )
    in

    let pattern_def file_rec buf pos =
      lazy
        (Bin.read_packed buf pos
        |> Pack.map_packed aloc
        |> Merge.merge SMap.empty (Lazy.force file_rec)
        )
    in

    let pattern file_rec buf pos =
      lazy
        (Bin.read_pattern buf pos
        |> Pack.map_pattern aloc
        |> Merge.merge_pattern (Lazy.force file_rec)
        )
    in

    let local_defs file_rec =
      let pos = Bin.local_defs buf in
      Bin.read_tbl_generic (local_def file_rec) buf pos Local_defs.init
    in

    let remote_refs file_rec =
      let pos = Bin.remote_refs buf in
      Bin.read_tbl_generic (remote_ref file_rec) buf pos Remote_refs.init
    in

    let pattern_defs file_rec =
      let pos = Bin.pattern_defs buf in
      Bin.read_tbl_generic (pattern_def file_rec) buf pos Pattern_defs.init
    in

    let patterns file_rec =
      let pos = Bin.patterns buf in
      Bin.read_tbl_generic (pattern file_rec) buf pos Patterns.init
    in

    let rec file_rec =
      lazy
        {
          Type_sig_merge.key = file_key;
          cx;
          dependencies;
          exports = exports file_rec;
          local_defs = local_defs file_rec;
          remote_refs = remote_refs file_rec;
          pattern_defs = pattern_defs file_rec;
          patterns = patterns file_rec;
        }
    in
    Lazy.force file_rec
  in

  let connect_require cx (mref, locs, m) =
    let module_t = dep_module_t cx mref m in
    let connect loc =
      let module_t = module_t loc in
      let (_, require_id) = Context.find_require cx loc in
      resolve_require_id cx require_id module_t
    in
    Nel.iter connect locs
  in

  fun file_key requires ast comments file_sig docblock aloc_table ->
    let ccx = Context.make_ccx master_cx in
    let metadata = Context.docblock_overrides docblock base_metadata in
    let cx = Context.make ccx metadata file_key aloc_table Context.Checking in
    ConsGen.set_dst_cx cx;
    let lint_severities = get_lint_severities metadata options in
    Type_inference_js.add_require_tvars cx file_sig;
    List.iter (connect_require cx) requires;
    let typed_ast = Type_inference_js.infer_ast cx file_key comments ast ~lint_severities in
    Merge_js.post_merge_checks cx master_cx ast typed_ast metadata file_sig;
    (cx, typed_ast)
