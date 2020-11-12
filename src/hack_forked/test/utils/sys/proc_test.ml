(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Asserter

(*
   1 (systemd) S 0 1 1
   118813 (java) R 1 420553 420553
   527674 (python3.6) R 118813 420553 420553
   527663 (python3.6) R 527663 420553 420553
   527674 (proc_test.opt) R 527663 420553 420553
*)

let systemd_pid = 1

let java_pid = 118813

let python_pid1 = 527111

let python_pid2 = 527663

let proc_test_pid = ref 527674

let systemd_cmdline =
  "/usr/lib/systemd/systemd\x00--switched-root\x00--system\x00--deserialize\x0030\x00"

(* This is a small subset of a Java tool's cmdline contents *)
let java_cmdline =
  "toold\x00-Xmx1000m\x00-Djava.awt.headless=true\x00-Djna.nosys=true\x00"
  ^ "-Djava.util.logging.config.class=com.company.tool.abc.bootstrapper.SomeConfig\x00-Dtool.test_util_no_tests_dir=true"

let python_cmdline1 =
  "[xarexec]\x00/usr/local/bin/threshold-monitor\x00-tt\x00/mnt/xarfuse/uid-0/123/__run_xar_main__.py"

let python_cmdline2 =
  "[xarexec]\x00/usr/local/bin/threshold-monitor\x00-tt\x00/mnt/xarfuse/uid-0/456/__run_xar_main__.py"

let proc_test_cmdline = "/data/users/unixname/proc_test/proc_test.opt\x00"

let proc_path_format = format_of_string "/proc/%d/"

let cmdline_path_format = format_of_string "%s/cmdline"

let stat_path_format = format_of_string "%s/stat"

let stat_format = format_of_string "%d (%s) R %d 123 456"

let create_proc_dir (pid : int) : string =
  let proc_dir = Printf.sprintf proc_path_format pid in
  Disk.mkdir_p proc_dir;
  proc_dir

let create_cmdline (pid : int) (cmdline : string) : unit =
  let proc_dir = create_proc_dir pid in
  Disk.write_file (Printf.sprintf cmdline_path_format proc_dir) cmdline

let create_stat (name : string) (pid : int) (ppid : int) : unit =
  let proc_dir = create_proc_dir pid in
  Disk.write_file
    (Printf.sprintf stat_path_format proc_dir)
    (Printf.sprintf stat_format pid name ppid)

let setup ~(use_test_stubbing : bool) : unit =
  if use_test_stubbing then (
    create_stat "systemd" systemd_pid 0;
    create_cmdline systemd_pid systemd_cmdline;

    create_stat "java" java_pid systemd_pid;
    create_cmdline java_pid java_cmdline;

    create_stat "python3.6" python_pid1 java_pid;
    create_cmdline python_pid1 python_cmdline1;

    create_stat "python3.6" python_pid2 python_pid1;
    create_cmdline python_pid2 python_cmdline2;

    create_stat "proc_test.opt" proc_test_pid.contents python_pid2;
    create_cmdline proc_test_pid.contents proc_test_cmdline
  ) else
    proc_test_pid := Unix.getpid ()

let ok_or_assert (r : ('a, string) result) : 'a =
  match r with
  | Error e ->
    Printf.eprintf "%s\n" e;
    assert false
  | Ok r -> r

let test_get_cmdline_self () : bool =
  let cmdline = Proc.get_cmdline proc_test_pid.contents in
  match cmdline with
  | Error e ->
    Printf.eprintf "%s" e;
    assert false
  | Ok c ->
    String_asserter.assert_equals
      "/data/users/unixname/proc_test/proc_test.opt "
      c
      "The process name should be correct!";
    true

let test_get_proc_stat_systemd () : bool =
  let proc_stat = ok_or_assert (Proc.get_proc_stat 1) in
  String_asserter.assert_equals
    "/usr/lib/systemd/systemd --switched-root --system --deserialize 30 "
    proc_stat.Proc.cmdline
    "The process cmdline should be correct!";
  Int_asserter.assert_equals 0 proc_stat.Proc.ppid "The process's parent PID should be correct!";
  true

let test_get_proc_stat_self () : bool =
  let proc_stat = ok_or_assert (Proc.get_proc_stat proc_test_pid.contents) in
  String_asserter.assert_equals
    "/data/users/unixname/proc_test/proc_test.opt "
    proc_stat.Proc.cmdline
    "The process cmdline should be correct!";
  true

let test_get_proc_stack_systemd () : bool =
  let proc_stack = ok_or_assert (Proc.get_proc_stack 1) in
  String_asserter.assert_list_equals
    ["/usr/lib/systemd/systemd --switched-root --system --deserialize 30"]
    proc_stack
    "The process cmdline stack should be correct!";
  true

let test_get_proc_stack_self_max_depth () =
  let proc_stack = ok_or_assert (Proc.get_proc_stack ~max_depth:2 proc_test_pid.contents) in
  String_asserter.assert_list_equals
    [
      "[xarexec] /usr/local/bin/threshold-monitor -tt /mnt/xarfuse/uid-0/456/__run_xar_main__.py";
      "/data/users/unixname/proc_test/proc_test.opt";
    ]
    proc_stack
    "The process name should be correct!";
  true

let test_get_proc_stack_self_max_length () =
  let proc_stack = ok_or_assert (Proc.get_proc_stack ~max_length:50 proc_test_pid.contents) in
  String_asserter.assert_list_equals
    [
      "/usr/lib/systemd/systemd --switched-root --system...";
      "toold -Xmx1000m -Djava.awt.headless=true -Djna.nos...";
      "[xarexec] /usr/local/bin/threshold-monitor -tt /mn...";
      "[xarexec] /usr/local/bin/threshold-monitor -tt /mn...";
      "/data/users/unixname/proc_test/proc_test.opt";
    ]
    proc_stack
    "The process name should be correct!";
  true

let test_get_proc_stack_self () =
  let proc_stack = ok_or_assert (Proc.get_proc_stack proc_test_pid.contents) in
  String_asserter.assert_list_equals
    [
      "/usr/lib/systemd/systemd --switched-root --system --deserialize 30";
      "toold -Xmx1000m -Djava.awt.headless=true -Djna.nosys=true -Djava.util.logging.config"
      ^ ".class=com.company.tool.abc.bootstrapper.SomeConfig -Dtool.test_util_no_tests_dir=true";
      "[xarexec] /usr/local/bin/threshold-monitor -tt /mnt/xarfuse/uid-0/123/__run_xar_main__.py";
      "[xarexec] /usr/local/bin/threshold-monitor -tt /mnt/xarfuse/uid-0/456/__run_xar_main__.py";
      "/data/users/unixname/proc_test/proc_test.opt";
    ]
    proc_stack
    "The process name should be correct!";
  true

let test_get_proc_stack_non_existent_PID () : bool =
  match Proc.get_proc_stack 9999999 with
  | Ok _ -> false
  | Error _ -> true

let tests =
  [
    ("Test get_cmdline on self", test_get_cmdline_self);
    ("Test get_proc_stat on systemd", test_get_proc_stat_systemd);
    ("Test get_proc_stat on self", test_get_proc_stat_self);
    ("Test get_proc_stack on self", test_get_proc_stack_self);
    ("Test get_proc_stack on systemd", test_get_proc_stack_systemd);
    ("Test get_proc_stack on self with max depth", test_get_proc_stack_self_max_depth);
    ("Test get_proc_stack on self with max length", test_get_proc_stack_self_max_length);
    ("Test get_proc_stack for a non-existent PID", test_get_proc_stack_non_existent_PID);
  ]

let () =
  (* Note: if you're running the tests with the default dependency injector,
     you need to remember to pass false to setup() because it's not
     possible to write to files in procfs. *)
  setup ~use_test_stubbing:true;
  Unit_test.run_all tests
