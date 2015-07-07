(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ServerEnv

(* Initialization of the server *)
let init_hack genv env get_next =

  let files_info, errorl1, failed1 =
    Hh_logger.measure "Parsing" begin fun () ->
      Parsing_service.go genv.workers ~get_next
    end in

  Hh_logger.log "Heap size: %d" (SharedMem.heap_size ());

  let is_check_mode =
    ServerArgs.check_mode genv.options &&
    ServerArgs.convert genv.options = None &&
    (* Note: we need to run update_files to get an accurate saved state *)
    ServerArgs.save_filename genv.options = None
  in

  let is_ai_mode = ServerArgs.ai_mode genv.options in

  if not (is_check_mode || is_ai_mode) then begin
    Typing_deps.update_files files_info;
  end;

  let errorl2, failed2, nenv = Hh_logger.measure "Naming" begin fun () ->
    Relative_path.Map.fold
      Naming.ndecl_file files_info ([], Relative_path.Set.empty, env.nenv)
  end in

  let fast, errorl3, failed3 = Hh_logger.measure "Type-decl" begin fun () ->
    let fast = FileInfo.simplify_fast files_info in
    let fast = Relative_path.Set.fold Relative_path.Map.remove failed2 fast in
    let errorl3, failed3 = Typing_decl_service.go genv.workers nenv fast in
    fast, errorl3, failed3
  end in

  Hh_logger.log "Heap size: %d" (SharedMem.heap_size ());

  let errorl4, failed4 = if not is_ai_mode then
      Hh_logger.measure "Type-check" begin fun () ->
        Typing_check_service.go genv.workers nenv fast
      end
    else
      [],  Relative_path.Set.empty in

  let errorl5, failed5 = if is_ai_mode then
      Hh_logger.measure "Ai" begin fun () ->
        Ai.go Typing_check_utils.check_defs genv.workers files_info nenv
      end
    else
      [], Relative_path.Set.empty in

  let failed =
    List.fold_right Relative_path.Set.union
      [failed1; failed2; failed3; failed4; failed5]
      Relative_path.Set.empty in
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

  let errorl = List.fold_right List.rev_append
      [errorl1; errorl2; errorl3; errorl4; errorl5] [] in
  env, errorl, failed

(* entry point *)
let init genv env next_files =
  let env, errorl, failed = init_hack genv env next_files in
  let env = { env with errorl = errorl;
              failed_parsing = failed } in
  env
