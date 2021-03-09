(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO: Run ocamlformat and fix all the detached comments. *)
[@@@ocamlformat "disable=true"]

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
  let locs = Locs.create () in
  List.iter (parse_lib opts scope locs) ordered_asts;
  locs, Scope.builtins_exn scope

let pack_builtins (locs, (tbls, globals, modules)) =
  let {Parse.module_refs; local_defs; remote_refs; _} = tbls in
  (* mark *)
  SMap.iter (fun _ b -> Mark.mark_binding b) globals;
  SMap.iter (fun _ m -> Mark.mark_builtin_module m) modules;
  (* compact *)
  let locs = Locs.compact locs in
  let module_refs = Module_refs.compact module_refs in
  let local_defs = Local_defs.compact local_defs in
  let remote_refs = Remote_refs.compact remote_refs in
  (* copy *)
  let cx = Pack.create_cx () in
  let locs = Locs.copy (fun x -> x) locs in
  let module_refs = Module_refs.copy (fun x -> x) module_refs in
  let local_defs = Local_defs.copy (Pack.pack_local_binding cx) local_defs in
  let remote_refs = Remote_refs.copy Pack.pack_remote_binding remote_refs in
  let globals = SMap.map Pack.pack_builtin globals in
  let modules =
    SMap.map
      (fun m ->
        let (loc, exports, export_def) = Pack.pack_builtin_module cx m in
        { Packed_type_sig.Builtins.loc; exports; export_def}
      )
      modules
  in
  cx.Pack.errs, locs, { Packed_type_sig.Builtins.module_refs; local_defs; remote_refs; globals; modules }

(* Modules are parsed and packed separately, then merged component wise
   according to the dependency DAG. *)
let parse_module ~strict source opts ast =
  let open Parse in
  let (_, { Flow_ast.Program.statements = stmts; _ }) = ast in
  let scope = Scope.create_module ~strict in
  let locs = Locs.create () in
  let file_loc = Locs.push locs ({Loc.none with Loc.source}) in
  List.iter (statement opts scope locs) stmts;
  locs, file_loc, Scope.exports_exn scope

let merge_locs loc0 loc1 =
  let k = Loc.(pos_cmp loc0.start loc1.start) in
  let k = if k = 0 then Loc.(pos_cmp loc1._end loc0._end) else k in
  if k < 0 then None
  else if k = 0 then Some loc0
  else
    Printf.ksprintf
      failwith
      "out of order %s > %s"
      (Loc.debug_to_string ~include_source:true loc0)
      (Loc.debug_to_string ~include_source:true loc1)

let pack (locs, file_loc, (tbls, exports)) =
  let {Parse.module_refs; local_defs; remote_refs; pattern_defs; patterns} = tbls in
  (* mark *)
  Mark.mark_exports file_loc exports;
  (* compact *)
  let locs = Locs.compact ~merge:merge_locs locs in
  let module_refs = Module_refs.compact module_refs in
  let local_defs = Local_defs.compact local_defs in
  let remote_refs = Remote_refs.compact remote_refs in
  let pattern_defs = Pattern_defs.compact pattern_defs in
  let patterns = Patterns.compact patterns in
  (* copy *)
  let cx = Pack.create_cx () in
  let locs = Locs.copy (fun x -> x) locs in
  let module_refs = Module_refs.copy (fun x -> x) module_refs in
  let local_defs = Local_defs.copy (Pack.pack_local_binding cx) local_defs in
  let remote_refs = Remote_refs.copy Pack.pack_remote_binding remote_refs in
  let pattern_defs = Pattern_defs.copy (Pack.pack_parsed cx) pattern_defs in
  let patterns = Patterns.copy Pack.pack_pattern patterns in
  let exports, export_def = Pack.pack_exports cx file_loc exports in
  cx.Pack.errs, locs, { Packed_type_sig.Module.exports; export_def; module_refs; local_defs; remote_refs; pattern_defs; patterns }

let parse_and_pack_module ~strict opts source ast =
  pack (parse_module ~strict source opts ast)

let parse_and_pack_builtins opts ordered_asts =
  pack_builtins (parse_libs opts ordered_asts)
