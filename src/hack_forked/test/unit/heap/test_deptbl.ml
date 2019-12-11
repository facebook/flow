(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_kernel

(* Functions to get and store data from deptable *)

external hh_add_dep : int -> unit = "hh_add_dep"

external hh_get_dep : int -> int list = "hh_get_dep"

external hh_get_dep_sqlite : int -> int list = "hh_get_dep_sqlite"

(* Taken from Typing_deps.Graph *)
let add x y = hh_add_dep ((x lsl 31) lor y)

let get = hh_get_dep

let get_sqlite = hh_get_dep_sqlite

(* Function to save and load deptable *)

external save_dep_table_sqlite : string -> string -> int = "hh_save_dep_table_sqlite"

external load_dep_table_sqlite : string -> bool -> int = "hh_load_dep_table_sqlite"

let expect ~msg bool =
  if bool then
    ()
  else (
    print_endline msg;
    exit 1
  )

let expect_equals key value expected =
  expect
    ~msg:(Printf.sprintf "Expected dependency %d to equal %d, got %d" key expected value)
    (value = expected)

let expect_equals_list key values expected_values =
  (* Because saving the dep table does not maintain order *)
  let values = List.sort compare values in
  let expected_values = List.sort compare expected_values in
  let len_values = List.length values in
  let len_exp_values = List.length expected_values in
  if len_values = len_exp_values then
    List.iter (List.zip_exn values expected_values) ~f:(fun (value, expected) ->
        expect_equals key value expected)
  else
    let msg = Printf.sprintf "Expected %d values, got %d values" len_exp_values len_values in
    print_endline msg;
    exit 1

let max_int_31bits = 2147483647 (* 0x7FFFFFFF *)

let data =
  [
    (0, [10; 11]);
    (1, [10]);
    (2, [20; 21; 22]);
    (3, [30; 31]);
    (4, [40]);
    (5, List.range 100 10000);
    (max_int_31bits - 1, List.range 1000000 1001000);
    (max_int_31bits, [max_int_31bits; max_int_31bits - 1]);
  ]

let data_empty = [(10, []); (11, []); (12, [])]

let populate_deptable () =
  List.iter data ~f:(fun (key, values) -> List.iter values ~f:(fun x -> add key x))

let init_shared_mem () =
  let num_workers = 0 in
  let handle =
    SharedMem.init
      ~num_workers
      {
        SharedMem.global_size = 16;
        heap_size = 1024;
        dep_table_pow = 16;
        hash_table_pow = 3;
        shm_dirs = [];
        shm_min_avail = 0;
        log_level = 0;
        sample_rate = 0.0;
      }
  in
  ignore (handle : SharedMem.handle)

let run_daemon fn =
  let handle = Daemon.fork ~channel_mode:`socket Unix.(stdout, stderr) fn () in
  let (_ : int * Unix.process_status) = Unix.waitpid [] handle.Daemon.pid in
  ()

let save_in_daemon ?buildRevision filename =
  run_daemon (fun () _ ->
      init_shared_mem ();
      populate_deptable ();
      Printf.printf "Saved as %s" filename;
      let revision =
        match buildRevision with
        | None -> Build_id.build_revision
        | Some revision -> revision
      in
      let (_ : int) = save_dep_table_sqlite filename revision in
      ())

let test_deps_in_memory () =
  init_shared_mem ();
  populate_deptable ();
  List.iter (List.append data data_empty) ~f:(fun (key, values) ->
      expect_equals_list key (get key) values)

let test_deptable_sql () =
  let deptable_name = Filename.temp_file "test_deptable" ".sql" in
  save_in_daemon deptable_name;
  init_shared_mem ();
  let (_ : int) = load_dep_table_sqlite deptable_name false in
  List.iter (List.append data data_empty) ~f:(fun (key, values) ->
      expect_equals_list key (get_sqlite key) values);
  Sys.remove deptable_name

let test_ignore_hh_version () =
  let deptable_name = Filename.temp_file "test_ignore_hh_version" ".sql" in
  save_in_daemon deptable_name ~buildRevision:"test_build_revision";
  init_shared_mem ();
  try
    let (_ : int) = load_dep_table_sqlite deptable_name false in
    print_endline "Should not have been able to load this deptable with hh version checking.";
    exit 1
  with _ ->
    let (_ : int) = load_dep_table_sqlite deptable_name true in
    List.iter (List.append data data_empty) ~f:(fun (key, values) ->
        expect_equals_list key (get_sqlite key) values);
    Sys.remove deptable_name

let tests () =
  let test_list =
    [
      ("test_in_memory", test_deps_in_memory);
      ("test_sql", test_deptable_sql);
      ("test_ignore_hh_version", test_ignore_hh_version);
    ]
  in
  let setup_test (name, test) =
    ( name,
      fun () ->
        test ();
        true )
  in
  List.map ~f:setup_test test_list

let () =
  Daemon.check_entry_point ();
  Unit_test.run_all (tests ())
