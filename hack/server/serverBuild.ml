(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type build_opts = {
  steps: string list option; (* steps for hack build to run.
                         None means 'all' *)
  no_steps: string list option; (* ...but don't run these steps *)
  run_scripts: bool; (* when true, run remaining arc build steps
                     that we haven't figured out how to port yet*)
  serial: bool; (* when true, don't use parallel workers *)
  test_dir: string option; (* test dir to generate into *)
  grade: bool; (* when true, diff test output against www and print
                  some stats *)
  check: bool; (* some sanity checking *)
  clean_before_build: bool; (* when true, do a clean build *)
  clean: bool; (* when true just clean all generated files *)
  is_push: bool; (* for push builds *)
  incremental: bool; (* for incremental build *)
  (* user the client is running as; if the server is running as a different user
   * we will probably have hard-to-debug permissions issues, so error out *)
  user: string;
  verbose: bool;
}

type build_progress =
  | BUILD_PROGRESS of string
  | BUILD_ERROR of string
  | BUILD_FINISHED
