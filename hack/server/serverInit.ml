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
  let start_t = Unix.gettimeofday() in
  let t = start_t in

  let files_info, errorl1, failed1 =
    Parsing_service.go genv.workers ~get_next in

  let t2 = Unix.gettimeofday() in
  Printf.printf "Parsing: %f\n%!" (t2 -. t);
  Printf.printf "Heap size: %d\n%!" (SharedMem.heap_size ());
  let t = t2 in

  let is_check_mode =
    ServerArgs.check_mode genv.options &&
    ServerArgs.convert genv.options = None &&
    ServerArgs.load_save_opt genv.options = None
  in

  if not is_check_mode then begin
    Typing_deps.update_files files_info;
  end;

  let nenv = env.nenv in

  let errorl2, failed2, nenv =
    Relative_path.Map.fold
      Naming.ndecl_file files_info ([], Relative_path.Set.empty, nenv) in

  let t2 = Unix.gettimeofday() in
  Printf.printf "Naming: %f\n%!" (t2 -. t);
  let t = t2 in

  let fast = FileInfo.simplify_fast files_info in
  let fast = Relative_path.Set.fold Relative_path.Map.remove failed2 fast in
  let errorl3, failed3 = Typing_decl_service.go genv.workers nenv fast in

  let t2 = Unix.gettimeofday() in
  Printf.printf "Type-decl: %f\n%!" (t2 -. t);
  Printf.printf "Heap size: %d\n%!" (SharedMem.heap_size ());
  let t = t2 in

  let errorl4, failed4 = Typing_check_service.go genv.workers fast in

  let t2 = Unix.gettimeofday() in
  Printf.printf "Type-check: %f\n%!" (t2 -. t);

  Printf.printf "Total: %f\n%!" (t2 -. start_t);

  let failed =
    List.fold_right Relative_path.Set.union
      [failed1; failed2; failed3; failed4]
      Relative_path.Set.empty in
  let env = { env with files_info = files_info; nenv = nenv } in

  SharedMem.init_done();

  let errorl = List.fold_right List.rev_append
      [errorl1; errorl2; errorl3; errorl4] [] in
  env, errorl, failed

(* entry point *)
let init genv env next_files =
  let env, errorl, failed = init_hack genv env next_files in
  let env = { env with errorl = errorl;
              failed_parsing = failed } in
  env
