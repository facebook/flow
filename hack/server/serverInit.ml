(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open ServerEnv

(* Initialization of the server *)
let init_hack genv env get_next =

  let t = Unix.gettimeofday () in
  let files_info, errorl1, failed1 =
    Parsing_service.go genv.workers ~get_next in
  let hs = SharedMem.heap_size () in
  Hh_logger.log "Heap size: %d" hs;
  Stats.(stats.init_parsing_heap_size <- hs);
  let t = Hh_logger.log_duration "Parsing" t in

  let is_check_mode =
    ServerArgs.check_mode genv.options &&
    ServerArgs.convert genv.options = None &&
    (* Note: we need to run update_files to get an accurate saved state *)
    ServerArgs.save_filename genv.options = None
  in

  let ai_mode = ServerArgs.ai_mode genv.options in

  let t = if is_check_mode then t else begin
    Typing_deps.update_files files_info;
    Hh_logger.log_duration "Updating deps" t
  end in

  let errorl2, failed2, nenv =
    Relative_path.Map.fold
      Naming.ndecl_file files_info ([], Relative_path.Set.empty, env.nenv) in
  let t = Hh_logger.log_duration "Naming" t in

  let fast = FileInfo.simplify_fast files_info in
  let fast = Relative_path.Set.fold Relative_path.Map.remove failed2 fast in
  let errorl3, failed3 = Typing_decl_service.go genv.workers nenv fast in
  let hs = SharedMem.heap_size () in
  Hh_logger.log "Heap size: %d" hs;
  Stats.(stats.init_heap_size <- hs);
  let t = Hh_logger.log_duration "Type-decl" t in

  let errorl4, failed4, t =
    if ai_mode = None || not is_check_mode then
      let e, f = Typing_check_service.go genv.workers nenv fast in
      let t = Hh_logger.log_duration "Type-check" t in
      e, f, t
    else
      [],  Relative_path.Set.empty, t in

  let errorl5, failed5, t = match ai_mode with
    | Some optstr ->
        let files_info =
          Relative_path.Set.fold Relative_path.Map.remove failed2 files_info in
        let files_info =
          Relative_path.Set.fold Relative_path.Map.remove failed3 files_info in
        let e, f = Ai.go
          Typing_check_utils.check_defs genv.workers files_info nenv optstr in
        let t = Hh_logger.log_duration "Ai" t in
        e, f, t
    | None ->
        [], Relative_path.Set.empty, t in

  let failed =
    List.fold_right [failed1; failed2; failed3; failed4; failed5]
      ~f:Relative_path.Set.union
      ~init:Relative_path.Set.empty in
  let env = { env with files_info = files_info; nenv = nenv } in

  SharedMem.init_done();

  let {SharedMem.used_slots; slots} = SharedMem.dep_stats () in
  let load_factor = float_of_int used_slots /. float_of_int slots in
  Hh_logger.log "Dependency table load factor: %d / %d (%.02f)"
    used_slots slots load_factor;

  let {SharedMem.used_slots; slots} = SharedMem.hash_stats () in
  let load_factor = float_of_int used_slots /. float_of_int slots in
  Hh_logger.log "Hashtable load factor: %d / %d (%.02f)"
    used_slots slots load_factor;

  let errorl = List.fold_right [errorl1; errorl2; errorl3; errorl4; errorl5]
    ~f:List.rev_append
    ~init:[] in
  env, errorl, failed

(* entry point *)
let init genv env next_files =
  let env, errorl, failed = init_hack genv env next_files in
  let env = { env with errorl = errorl;
              failed_parsing = failed } in
  env
