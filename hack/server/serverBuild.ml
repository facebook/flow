(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let svnrev_path = "scripts/build/artifacts/SVN_REVISION"

type build_opts = {
  steps: string list option; (* steps for hack build to run.
                         None means 'all except kill-switched' *)
  ignore_killswitch: bool; (* when true, also build kill-switched steps *)
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
  id: string;
}

type build_progress =
  | BUILD_PROGRESS of string
  | BUILD_ERROR of string
  | BUILD_FINISHED

let build_type_of build_opts =
  let {steps; no_steps; is_push; incremental; _} = build_opts in
  if steps <> None || no_steps <> None then
    `Steps
  else if is_push then
    `Push
  else if incremental then
    `Incremental
  else
    `Full
