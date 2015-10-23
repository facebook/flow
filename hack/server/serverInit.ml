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
open ServerCheckUtils
open Utils

open Result.Export
open Result.Monad_infix

module DepSet = Typing_deps.DepSet
module Dep = Typing_deps.Dep
module SLC = ServerLocalConfig

exception No_loader
exception Loader_timeout

(*
 * hh_server can initialize either by typechecking the entire project (aka
 * starting from a "fresh state") or by loading from a saved state and
 * typechecking what has changed.
 *
 * If we start from a fresh state, we run the following phases:
 *
 *   Parsing -> Naming -> Type-decl -> Type-check
 *
 * If we are loading a state, we do
 *
 *   Run load script and parsing concurrently -> Naming -> Type-decl
 *
 * Then we typecheck only the files that have changed since the state was
 * saved.
 *
 * This is done in fairly similar manner to the incremental update
 * code in ServerTypeCheck. The key difference is that incremental mode
 * can compare the files that it has just parsed with their old versions,
 * thereby (in theory) recomputing the least amount possible. OTOH,
 * ServerInit only has the latest version of each file, so it has to make
 * the most conservative estimate about what to recheck.
 *)

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

let save_state env fn =
  let t = Unix.gettimeofday () in
  if env.errorl <> []
  then failwith "--save-mini only works if there are no type errors!";
  let chan = Sys_utils.open_out_no_fail fn in
  let names = FileInfo.simplify_fast env.files_info in
  Marshal.to_channel chan names [];
  Sys_utils.close_out_no_fail fn chan;
  SharedMem.save_dep_table (fn^".deptable");
  ignore @@ Hh_logger.log_duration "Saving" t

(* Expected output from script:
 * The first line indicates the path to the state file
 * The remaining lines indicate the files that have changed since that file
 * was built
 *)
let load_state_ root cmd =
  let cmd =
    Printf.sprintf
      "%s %s %s"
      (Filename.quote (Path.to_string cmd))
      (Filename.quote (Path.to_string root))
      (Filename.quote Build_id.build_id_ohai) in
  Hh_logger.log "Running load_mini script: %s\n%!" cmd;
  let ic = Unix.open_process_in cmd in
  let state_fn = input_line ic in
  let to_recheck = ref [] in
  (try
    while true do to_recheck := input_line ic :: !to_recheck done;
  with End_of_file -> ());
  assert (Unix.close_process_in ic = Unix.WEXITED 0);
  SharedMem.load_dep_table (state_fn^".deptable");
  let end_time = Unix.gettimeofday () in
  state_fn, !to_recheck, end_time

let load_state root cmd (_ic, oc) =
  let result = Result.try_with (fun () -> load_state_ root cmd) in
  Daemon.to_channel oc result

(* This runs the load script to download state and figure out which files have
 * changed, then loads the downloaded dependency table into shared memory. It
 * must not run concurrently with any operations that might write to the
 * deptable. *)
let mk_state_future timeout root cmd =
  let start_time = Unix.gettimeofday () in
  Result.try_with @@ fun () ->
  let {Daemon.channels = (ic, _oc); pid} =
    Daemon.fork ~log_file:(ServerFiles.load_log root) (load_state root cmd) in
  fun () ->
    Result.join @@ Result.try_with @@ fun () ->
    Sys_utils.with_timeout timeout
      ~on_timeout:(fun _ -> raise Loader_timeout)
      ~do_:begin fun () ->
        Daemon.from_channel ic
        >>| fun (fn, dirty_files, end_time) ->
        HackEventLogger.load_mini_worker_end start_time end_time;
        let time_taken = end_time -. start_time in
        Hh_logger.log "Mini-state loading worker took %.2fs" time_taken;
        let _, status = Unix.waitpid [] pid in
        assert (status = Unix.WEXITED 0);
        let chan = open_in fn in
        let old_fast = Marshal.from_channel chan in
        let dirty_files = List.map dirty_files Relative_path.(concat Root) in
        Relative_path.set_of_list dirty_files, old_fast
      end

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
  let files_info =
    Relative_path.Map.fold Relative_path.Map.add files_info env.files_info in
  let hs = SharedMem.heap_size () in
  Hh_logger.log "Heap size: %d" hs;
  Stats.(stats.init_parsing_heap_size <- hs);
  (* TODO: log a count of the number of files parsed... 0 is a placeholder *)
  HackEventLogger.parsing_end t hs  ~parsed_count:0;
  let env = { env with
    files_info;
    errorl = List.rev_append errorl env.errorl;
    failed_parsing = Relative_path.Set.union env.failed_parsing failed;
  } in
  env, (Hh_logger.log_duration "Parsing" t)

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
  let bucket_size = genv.local_config.SLC.type_decl_bucket_size in
  let errorl, failed_decl =
    Typing_decl_service.go ~bucket_size genv.workers env.nenv fast in
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
  if ServerArgs.ai_mode genv.options = None || not (is_check_mode genv.options)
  then begin
    let errorl, failed = Typing_check_service.go genv.workers env.nenv fast in
    HackEventLogger.type_check_end t;
    let env = { env with
      errorl = List.rev_append errorl env.errorl;
      failed_check = failed;
    } in
    env, (Hh_logger.log_duration "Type-check" t)
  end else env, t

let get_dirty_fast old_fast fast dirty =
  Relative_path.Set.fold begin fun fn acc ->
    let dirty_fast = Relative_path.Map.get fn fast in
    let dirty_old_fast = Relative_path.Map.get fn old_fast in
    let fast = Option.merge dirty_old_fast dirty_fast FileInfo.merge_names in
    match fast with
    | Some fast -> Relative_path.Map.add fn fast acc
    | None -> acc
  end dirty Relative_path.Map.empty

let get_all_deps {FileInfo.n_funs; n_classes; n_types; n_consts} =
  let add_deps_of_sset dep_ctor sset depset =
    SSet.fold begin fun n acc ->
      let dep = dep_ctor n in
      let deps = Typing_deps.get_bazooka dep in
      DepSet.union deps acc
    end sset depset
  in
  let deps = add_deps_of_sset (fun n -> Dep.Fun n) n_funs DepSet.empty in
  let deps = add_deps_of_sset (fun n -> Dep.FunName n) n_funs deps in
  let deps = add_deps_of_sset (fun n -> Dep.Class n) n_classes deps in
  let deps = add_deps_of_sset (fun n -> Dep.Class n) n_types deps in
  let deps = add_deps_of_sset (fun n -> Dep.GConst n) n_consts deps in
  let deps = add_deps_of_sset (fun n -> Dep.GConstName n) n_consts deps in
  deps

(* We start of with a list of files that have changed since the state was saved
 * (dirty_files), and two maps of the class / function declarations -- one made
 * when the state was saved (old_fast) and one made for the current files in
 * the repository (fast). We grab the declarations from both, to account for
 * both the declaratons that were deleted and those that are newly created.
 * Then we use the deptable to figure out the files that referred to them.
 * Finally we recheck the lot. *)
let type_check_dirty genv env old_fast fast dirty_files t =
  let fast = get_dirty_fast old_fast fast dirty_files in
  let names = Relative_path.Map.fold begin fun _k v acc ->
    FileInfo.merge_names v acc
  end fast FileInfo.empty_names in
  let deps = get_all_deps names in
  let to_recheck = Typing_deps.get_files deps in
  let fast = extend_fast fast env.files_info to_recheck in
  type_check genv env fast t

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

let get_build_targets () =
  let targets =
    List.map (BuildMain.get_all_targets ()) (Relative_path.(concat Root)) in
  Relative_path.set_of_list targets

(* entry point *)
let init ?load_mini_script genv =
  let env = ServerEnvBuild.make_env genv.config in
  let root = ServerArgs.root genv.options in

  (* Spawn this first so that it can run in the background while parsing is
   * going on. The script can fail in a variety of ways, but the resolution
   * is always the same -- we fall back to rechecking everything. Running it
   * in the Result monad provides a convenient way to locate the error
   * handling code in one place. *)
  let load_mini_script = Result.of_option load_mini_script ~error:No_loader in
  let timeout = genv.local_config.SLC.load_mini_script_timeout in
  let state_future = load_mini_script >>= (mk_state_future timeout root) in

  let get_next, t = indexing genv in
  let env, t = parsing genv env ~get_next t in

  let state = state_future >>= fun f ->
    f () >>= fun (dirty_files, old_fast) ->
    genv.wait_until_ready ();
    let root = Path.to_string root in
    let updates = genv.notifier () in
    let updates = SSet.filter (fun p ->
      str_starts_with p root && ServerEnv.file_filter p) updates in
    let changed_while_parsing = Relative_path.(relativize_set Root updates) in
    (* Build targets are untracked by version control, so we must always
     * recheck them. While we could query hg / git for the untracked files,
     * it's much slower. *)
    let dirty_files =
      Relative_path.Set.union dirty_files (get_build_targets ()) in
    Ok (dirty_files, changed_while_parsing, old_fast)
  in
  HackEventLogger.load_mini_state_end t;
  let t = Hh_logger.log_duration "Loading mini state" t in

  let t = update_files genv env.files_info t in
  let env, t = naming env t in
  let fast = FileInfo.simplify_fast env.files_info in
  let fast = Relative_path.(Set.fold Map.remove) env.failed_parsing fast in
  let env, t = type_decl genv env fast t in

  let env, t =
    match state with
    | Ok (dirty_files, changed_while_parsing, old_fast) ->
      (* If a file has changed while we were parsing, we may have parsed the
       * new version, so we must treat it as possibly creating new type
       * errors. *)
      let dirty_files =
        Relative_path.Set.union dirty_files changed_while_parsing in
      (* But we still want to keep it in the set of things that need to be
       * reparsed in the next round of incremental updates. *)
      let env = { env with
        failed_parsing =
          Relative_path.Set.union env.failed_parsing changed_while_parsing;
      } in
      type_check_dirty genv env old_fast fast dirty_files t
    | Error err ->
      (* Fall back to type-checking everything *)
      if err <> No_loader then begin
        HackEventLogger.load_mini_exn err;
        Hh_logger.exc ~prefix:"Could not load mini state: " err;
      end;
      type_check genv env fast t
  in

  let env, _t = ai_check genv env.files_info env t in

  SharedMem.init_done ();
  print_hash_stats ();
  env
