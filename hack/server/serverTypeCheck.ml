(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(*****************************************************************************)
open Utils
open ServerEnv

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

let print_defs prefix defs =
  List.iter begin fun (_, fname) ->
    Printf.printf "  %s %s\n" prefix fname;
  end defs

let print_fast_pos fast_pos =
  SMap.iter begin fun x (funs, classes) ->
    Printf.printf "File: %s\n" x;
    print_defs "Fun" funs;
    print_defs "Class" classes;
  end fast_pos;
  Printf.printf "\n";
  flush stdout;
  ()

let print_fast fast =
  SMap.iter begin fun x (funs, classes) ->
    Printf.printf "File: %s\n" x;
    SSet.iter (Printf.printf "  Fun %s\n") funs;
    SSet.iter (Printf.printf "  Class %s\n") classes;
  end fast;
  Printf.printf "\n";
  flush stdout;
  ()

(*****************************************************************************)
(* Given a set of Ast.id list produce a SSet.t (got rid of the positions)    *)
(*****************************************************************************)

let set_of_idl l =
  List.fold_left (fun acc (_, x) -> SSet.add x acc) SSet.empty l

(*****************************************************************************)
(* We want add all the declarations that were present in a file *before* the
 * current modification. The scenario:
 * File foo.php was defining the class A.
 * The user gets rid of class A (in foo.php)
 * In general, the type-checker determines what must be re-declared or
 * re-typechecked, by comparing the old and the new type-definitions.
 * That's why we are adding the 'old' definitions to the file.
 * In this case, the redecl phase (typing/typing_redecl_service.ml) is going
 * to compare the 'old' definition of A with the new one. It will realize that
 * the new one is missing, and go ahead and retype everything that depends
 * on A.
 * Without a call to add_old_decls, the class A wouldn't appear anywhere,
 * and we wouldn't realize that we have to re-check the types that depend
 * on A.
 *)
(*****************************************************************************)

let add_old_decls old_files_info fast =
  Relative_path.Map.fold begin fun filename info_names acc ->
    match Relative_path.Map.get filename old_files_info with
    | Some {FileInfo.consider_names_just_for_autoload = true; _}
    | None -> acc
    | Some old_info ->
      let old_info_names = FileInfo.simplify old_info in
      let info_names = FileInfo.merge_names old_info_names info_names in
      Relative_path.Map.add filename info_names acc
  end fast fast

(*****************************************************************************)
(* Reparsing helpers.
 * It's called reparse (as opposed to parse) because it retrieves the tree
 * from the datanodes where the Asts are stored in a serialized format.
 * Important: we never ever want to reparse a file that was already parsed.
 * If that was the case, an older version of the file would come and replace
 * a newer one (the one we just parsed). It would lead to very
 * subtle/terrible bugs.
 * This is why reparse takes a file->ast (fast). If we try to reparse a file
 * that we already parsed, the data is left unchanged.
 *)
(*****************************************************************************)

let reparse fast files_info additional_files =
  Relative_path.Set.fold begin fun x acc ->
    match Relative_path.Map.get x fast with
    | None ->
        (try
           let info = Relative_path.Map.find_unsafe x files_info in
           if info.FileInfo.consider_names_just_for_autoload then acc else
           let info_names = FileInfo.simplify info in
           Relative_path.Map.add x info_names acc
         with Not_found ->
           acc)
    | Some _ -> acc
  end additional_files fast

(*****************************************************************************)
(* Removes the names that were defined in the files *)
(*****************************************************************************)

let remove_decls env fast_parsed =
  let nenv = env.nenv in
  let nenv =
    Relative_path.Map.fold begin fun fn _ nenv ->
      match Relative_path.Map.get fn env.files_info with
      | Some {FileInfo.consider_names_just_for_autoload = true; _}
      | None -> nenv
      | Some {FileInfo.
              funs = funl;
              classes = classel;
              typedefs = typel;
              consts = constl;
              file_mode;
              comments;
              consider_names_just_for_autoload} ->
        let funs = set_of_idl funl in
        let classes = set_of_idl classel in
        let typedefs = set_of_idl typel in
        let consts = set_of_idl constl in
        let nenv = Naming.remove_decls nenv
            (funs, classes, typedefs, consts) in
        nenv
    end fast_parsed nenv
  in
  { env with nenv = nenv }

(*****************************************************************************)
(* Removes the files that failed *)
(*****************************************************************************)

let remove_failed fast failed =
  Relative_path.Set.fold Relative_path.Map.remove failed fast

(*****************************************************************************)
(* Parses the set of modified files *)
(*****************************************************************************)

let parsing genv env =
  Parser_heap.ParserHeap.remove_batch env.failed_parsing;
  Parser_heap.HH_FIXMES.remove_batch env.failed_parsing;
  HackSearchService.MasterApi.clear_shared_memory env.failed_parsing;
  SharedMem.collect();
  let get_next = Bucket.make (Relative_path.Set.elements env.failed_parsing) in
  Parsing_service.go genv.workers ~get_next

(*****************************************************************************)
(* At any given point in time, we want to know what each file defines.
 * The datastructure that maintains this information is called file_info.
 * This code updates the file information.
 *)
(*****************************************************************************)

let update_file_info env fast_parsed =
  Typing_deps.update_files fast_parsed;
  let files_info =
    Relative_path.Map.fold Relative_path.Map.add fast_parsed env.files_info in
  files_info

(*****************************************************************************)
(* Defining the global naming environment.
 * Defines an environment with the names of all the globals (classes/funs).
 *)
(*****************************************************************************)

let declare_names env files_info fast_parsed =
  let env = remove_decls env fast_parsed in
  let errorl, failed_naming, nenv =
    Relative_path.Map.fold
      Naming.ndecl_file fast_parsed ([], Relative_path.Set.empty, env.nenv) in
  let fast = remove_failed fast_parsed failed_naming in
  let fast = FileInfo.simplify_fast fast in
  let env = { env with nenv = nenv } in
  env, errorl, failed_naming, fast

(*****************************************************************************)
(* Function called after parsing, does nothing by default. *)
(*****************************************************************************)

let hook_after_parsing = ref (fun _ _ _ _ -> ())

(*****************************************************************************)
(* Where the action is! *)
(*****************************************************************************)

let type_check genv env =

  Printf.eprintf "******************************************\n";
  Hh_logger.log "Files to recompute: %d"
    (Relative_path.Set.cardinal env.failed_parsing);
  (* PARSING *)
  let start_t = Unix.gettimeofday() in
  let fast_parsed, errorl, failed_parsing =
    Hh_logger.measure "Parsing" begin fun () ->
      parsing genv env
    end in

  (* UPDATE FILE INFO *)
  let old_env = env in
  let updates = old_env.failed_parsing in
  let files_info = update_file_info env fast_parsed in

  (* BUILDING AUTOLOADMAP *)
  !hook_after_parsing genv old_env { env with files_info } updates;

  (* NAMING *)
  let env, errorl', failed_naming, fast =
    Hh_logger.measure "Naming" begin fun () ->
      let env, errorl', failed_naming, fast =
        declare_names env files_info fast_parsed in

      (* COMPUTES WHAT MUST BE REDECLARED  *)
      let fast = reparse fast files_info env.failed_decl in
      let fast = add_old_decls env.files_info fast in

      env, errorl', failed_naming, fast
    end in

  let errorl = List.rev_append errorl' errorl in

  let to_redecl_phase2, to_recheck1 =
    Hh_logger.measure "Determining changes" begin fun () ->
      let _, _, to_redecl_phase2, to_recheck1 =
        Typing_redecl_service.redo_type_decl genv.workers env.nenv fast
      in
      let to_redecl_phase2 = Typing_deps.get_files to_redecl_phase2 in
      let to_recheck1 = Typing_deps.get_files to_recheck1 in
      to_redecl_phase2, to_recheck1
    end in

  let fast_redecl_phase2 = reparse fast files_info to_redecl_phase2 in

  (* DECLARING TYPES: Phase2 *)
  let errorl', failed_decl, to_recheck2 =
    Hh_logger.measure "Type-decl" begin fun () ->
      let errorl', failed_decl, _to_redecl2, to_recheck2 =
        Typing_redecl_service.redo_type_decl
          genv.workers env.nenv fast_redecl_phase2 in
      let to_recheck2 = Typing_deps.get_files to_recheck2 in
      errorl', failed_decl, to_recheck2
    end in

  let errorl = List.rev_append errorl' errorl in

  (* DECLARING TYPES: merging results of the 2 phases *)
  let fast =
    Relative_path.Map.fold Relative_path.Map.add fast fast_redecl_phase2 in
  let to_recheck = Relative_path.Set.union env.failed_decl to_redecl_phase2 in
  let to_recheck = Relative_path.Set.union to_recheck1 to_recheck in
  let to_recheck = Relative_path.Set.union to_recheck2 to_recheck in

  (* TYPE CHECKING *)
  let errorl', failed_check = Hh_logger.measure "Type-check" begin fun () ->
    let to_recheck = Relative_path.Set.union to_recheck env.failed_check in
    let fast = reparse fast files_info to_recheck in
    ServerCheckpoint.process_updates fast;
    Typing_check_service.go genv.workers env.nenv fast
  end in

  let errorl = List.rev (List.rev_append errorl' errorl) in

  Hh_logger.log "Total: %f\n%!" ((Unix.gettimeofday ()) -. start_t);
  let total_rechecked_count = Relative_path.Set.cardinal to_recheck in
  HackEventLogger.recheck_once_end start_t total_rechecked_count;

  (* Done, that's the new environment *)
  { files_info = files_info;
    nenv = env.nenv;
    errorl = errorl;
    failed_parsing = Relative_path.Set.union failed_naming failed_parsing;
    failed_decl = failed_decl;
    failed_check = failed_check;
  }

(*****************************************************************************)
(* Checks that the working directory is clean *)
(*****************************************************************************)

let check genv env =
  if !debug then begin
    Printf.printf "****************************************\n";
    Printf.printf "Start Check\n";
    flush stdout;
  end;
  type_check genv env
