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
open Utils

(* Return all the files that we need to typecheck *)
let make_next_files genv : Relative_path.t MultiWorker.nextlist =
  let next_files_root = compose
    (List.map ~f:(Relative_path.(create Root)))
    (genv.indexer ServerEnv.file_filter) in
  let hhi_root = Hhi.get_hhi_root () in
  let next_files_hhi = compose
    (List.map ~f:(Relative_path.(create Hhi)))
    (Find.make_next_files ~name:"hhi" FindUtils.is_php hhi_root) in
  fun () ->
    match next_files_hhi () with
    | [] -> next_files_root ()
    | x -> x

let is_check_mode options =
  ServerArgs.check_mode options &&
  ServerArgs.convert options = None &&
  (* Note: we need to run update_files to get an accurate saved state *)
  ServerArgs.save_filename options = None

let indexing genv =
  let t = Unix.gettimeofday () in
  let get_next = make_next_files genv in
  HackEventLogger.indexing_end t;
  let t = Hh_logger.log_duration "Indexing" t in
  get_next, t

let parsing genv env ~get_next t =
  let files_info, errorl, failed =
    Parsing_service.go genv.workers ~get_next in
  let hs = SharedMem.heap_size () in
  Hh_logger.log "Heap size: %d" hs;
  Stats.(stats.init_parsing_heap_size <- hs);
  (* TODO: log a count of the number of files parsed... 0 is a placeholder *)
  HackEventLogger.parsing_end t hs  ~parsed_count:0;
  let t = Hh_logger.log_duration "Parsing" t in
  {env with files_info; errorl; failed_parsing = failed}, t

let update_files genv files_info t =
  if is_check_mode genv.options then t else begin
    Typing_deps.update_files files_info;
    HackEventLogger.updating_deps_end t;
    Hh_logger.log_duration "Updating deps" t
  end

let naming env t =
  let env =
    Relative_path.Map.fold begin fun k v env ->
      let errorl, failed, nenv = Naming.ndecl_file k v env.nenv in
      { env with
        nenv;
        errorl = List.rev_append errorl env.errorl;
        failed_parsing = Relative_path.Set.union env.failed_parsing failed;
      }
    end env.files_info env
  in
  env, (Hh_logger.log_duration "Naming" t)

let type_decl genv env fast t =
  let errorl, failed_decl =
    Typing_decl_service.go genv.workers env.nenv fast in
  let hs = SharedMem.heap_size () in
  Hh_logger.log "Heap size: %d" hs;
  Stats.(stats.init_heap_size <- hs);
  HackEventLogger.type_decl_end t;
  let t = Hh_logger.log_duration "Type-decl" t in
  let env = {
    env with
    errorl = List.rev_append errorl env.errorl;
    failed_decl;
  } in
  env, t

let type_check genv env fast t =
  if ServerArgs.ai_mode genv.options = None || is_check_mode genv.options
  then begin
    let errorl, failed = Typing_check_service.go genv.workers env.nenv fast in
    HackEventLogger.type_check_end t;
    let env = { env with
      errorl = List.rev_append errorl env.errorl;
      failed_check = failed;
    } in
    env, (Hh_logger.log_duration "Type-check" t)
  end else env, t

let ai_check genv files_info env t =
  let all_passed = List.for_all
    [env.failed_parsing; env.failed_decl; env.failed_check]
    (fun m -> Relative_path.Set.is_empty m) in
  if not all_passed then env, t else
  match ServerArgs.ai_mode genv.options with
  | Some ai_opt ->
    let errorl, failed = Ai.go
      Typing_check_utils.check_defs genv.workers files_info env.nenv ai_opt in
    let env = { env with
      errorl = List.rev_append errorl env.errorl;
      failed_check = Relative_path.Set.union failed env.failed_check;
    } in
    env, (Hh_logger.log_duration "Ai" t)
  | None -> env, t

let print_hash_stats () =
  let {SharedMem.used_slots; slots} = SharedMem.dep_stats () in
  let load_factor = float_of_int used_slots /. float_of_int slots in
  Hh_logger.log "Dependency table load factor: %d / %d (%.02f)"
    used_slots slots load_factor;

  let {SharedMem.used_slots; slots} = SharedMem.hash_stats () in
  let load_factor = float_of_int used_slots /. float_of_int slots in
  Hh_logger.log "Hashtable load factor: %d / %d (%.02f)"
    used_slots slots load_factor;
  ()

(* entry point *)
let init ?wait_for_deps genv =
  let env = ServerEnvBuild.make_env genv.config in
  let get_next, t = indexing genv in
  let env, t = parsing genv env ~get_next t in

  Option.iter wait_for_deps begin fun f ->
    let start_t, end_t = f () in
    HackEventLogger.load_deps_end start_t end_t;
    let time_taken = end_t -. start_t in
    Hh_logger.log "Loading deps took %.2fs" time_taken;
  end;

  let t = update_files genv env.files_info t in
  let env, t = naming env t in
  let fast = FileInfo.simplify_fast env.files_info in
  let fast = Relative_path.(Set.fold Map.remove) env.failed_parsing fast in
  let env, t = type_decl genv env fast t in
  let env, t = type_check genv env fast t in
  let env, _t = ai_check genv env.files_info env t in

  SharedMem.init_done ();
  print_hash_stats ();

  env
