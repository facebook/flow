(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type_sig_collections
module Parse = Type_sig_parse
module Mark = Type_sig_mark
module Pack = Type_sig_pack

(* For builtins, each lib file is parsed separately into an unresolved global
   scope. The parsed lib files are then combined before resolving names. The
   proceses of combining deals with overridden definitions. Finally, the
   resolved builtins are merged. Declared modules can depend on each other, so
   they are treated like a cycle. *)
let parse_lib opts scope locs ast =
  let open Parse in
  let (_, { Flow_ast.Program.statements = stmts; _ }) = ast in
  List.iter (statement opts scope locs) stmts

let parse_libs opts ordered_asts =
  let open Parse in
  let scope = Scope.create_global () in
  let tbls = create_tables () in
  List.iter (parse_lib opts scope tbls) ordered_asts;
  (tbls, Scope.builtins_exn scope)

let pack_builtins (tbls, (global_values, global_types, global_modules)) =
  let { Parse.locs; module_refs; local_defs; remote_refs; pattern_defs; patterns } = tbls in
  (* mark *)
  SMap.iter (fun _ b -> Mark.mark_binding ~locs_to_dirtify:[] b) global_values;
  SMap.iter (fun _ b -> Mark.mark_binding ~locs_to_dirtify:[] b) global_types;
  SMap.iter (fun _ m -> Mark.mark_builtin_module m) global_modules;
  (* compact *)
  let locs = Locs.compact locs in
  let module_refs = Module_refs.Interned.compact module_refs in
  let local_defs = Local_defs.compact local_defs in
  let remote_refs = Remote_refs.compact remote_refs in
  let pattern_defs = Pattern_defs.compact pattern_defs in
  let patterns = Patterns.compact patterns in
  (* copy *)
  let cx = Pack.create_cx () in
  let (locs, _) = Locs.copy (fun x -> x) locs in
  let (module_refs, _) = Module_refs.copy (fun x -> x) module_refs in
  let (local_defs, _) = Local_defs.copy (Pack.pack_local_binding cx) local_defs in
  let (remote_refs, _) = Remote_refs.copy Pack.pack_remote_binding remote_refs in
  let (pattern_defs, _) = Pattern_defs.copy (Pack.pack_parsed cx) pattern_defs in
  let (patterns, _) = Patterns.copy Pack.pack_pattern patterns in
  let global_values = SMap.map Pack.pack_builtin global_values in
  let global_types = SMap.map Pack.pack_builtin global_types in
  let global_modules =
    SMap.mapi
      (fun name m ->
        let (loc, module_kind) = Pack.pack_builtin_module cx name m in
        { Packed_type_sig.Builtins.loc; module_kind })
      global_modules
  in
  ( cx.Pack.errs,
    locs,
    {
      Packed_type_sig.Builtins.module_refs;
      local_defs;
      remote_refs;
      pattern_defs;
      patterns;
      global_values;
      global_types;
      global_modules;
    }
  )

(* Modules are parsed and packed separately, then merged component wise
   according to the dependency DAG. *)
let parse_module ~strict ~platform_availability_set source opts ast =
  let open Parse in
  let (_, { Flow_ast.Program.statements = stmts; _ }) = ast in
  let scope = Scope.create_module ~strict ~platform_availability_set in
  let tbls = create_tables () in
  let file_loc = push_loc tbls { Loc.none with Loc.source } in
  List.iter (statement opts scope tbls) stmts;
  (tbls, file_loc, Scope.exports_exn scope)

let merge_locs loc0 loc1 =
  let k = Packed_locs.compare_locs loc0 loc1 in
  if k < 0 then
    None
  else if k = 0 then
    Some loc0
  else
    Printf.ksprintf
      failwith
      "out of order %s > %s"
      (Loc.debug_to_string ~include_source:true loc0)
      (Loc.debug_to_string ~include_source:true loc1)

let pack ~locs_to_dirtify source (tbls, file_loc, exports) =
  let { Parse.locs; module_refs; local_defs; remote_refs; pattern_defs; patterns } = tbls in
  (* mark *)
  Mark.mark_exports ~locs_to_dirtify file_loc exports;
  (* compact *)
  let locs = Locs.compact ~merge:merge_locs locs in
  let module_refs = Module_refs.Interned.compact module_refs in
  let local_defs = Local_defs.compact local_defs in
  let remote_refs = Remote_refs.compact remote_refs in
  let pattern_defs = Pattern_defs.compact pattern_defs in
  let patterns = Patterns.compact patterns in
  (* copy *)
  let cx = Pack.create_cx () in
  let (locs, _) = Locs.copy (fun x -> x) locs in
  let (module_refs, _) = Module_refs.copy (fun x -> x) module_refs in
  let (local_defs, dirty_local_defs) = Local_defs.copy (Pack.pack_local_binding cx) local_defs in
  let (remote_refs, _) = Remote_refs.copy Pack.pack_remote_binding remote_refs in
  let (pattern_defs, dirty_pattern_defs) = Pattern_defs.copy (Pack.pack_parsed cx) pattern_defs in
  let (patterns, _) = Patterns.copy Pack.pack_pattern patterns in
  let module_kind =
    Pack.pack_exports
      cx
      file_loc
      (Base.Option.value_map source ~default:"<unnamed>" ~f:File_key.to_string)
      exports
  in
  ( cx.Pack.errs,
    locs,
    {
      Packed_type_sig.Module.module_kind;
      module_refs;
      local_defs;
      dirty_local_defs;
      remote_refs;
      pattern_defs;
      dirty_pattern_defs;
      patterns;
    }
  )

let parse_and_pack_module ~strict ~platform_availability_set opts source ast =
  pack
    ~locs_to_dirtify:opts.Type_sig_options.locs_to_dirtify
    source
    (parse_module ~strict ~platform_availability_set source opts ast)

let parse_and_pack_builtins opts ordered_asts = pack_builtins (parse_libs opts ordered_asts)
