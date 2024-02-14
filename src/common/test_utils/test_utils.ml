(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let make_flowconfig_flags
    ?(ignores = [])
    ?(untyped = [])
    ?(declarations = [])
    ?(includes = [])
    ?(libs = [])
    ?(raw_lint_severities = [])
    () =
  { CommandUtils.ignores; untyped; declarations; includes; libs; raw_lint_severities }

let make_options_flags
    ?(all = false)
    ?(debug = false)
    ?(flowconfig_flags = make_flowconfig_flags ())
    ?(include_warnings = false)
    ?max_warnings
    ?max_workers
    ?merge_timeout
    ?(munge_underscore_members = false)
    ?(no_flowlib = false)
    ?(profile = false)
    ?(quiet = false)
    ?(strip_root = false)
    ?temp_dir
    ?traces
    ?verbose
    ?wait_for_recheck
    ?(include_suppressions = false)
    ?estimate_recheck_time
    ?long_lived_workers
    ?blocking_worker_communication
    ?(distributed = false)
    () =
  {
    CommandUtils.Options_flags.all;
    debug;
    flowconfig_flags;
    include_warnings;
    max_warnings;
    max_workers;
    merge_timeout;
    munge_underscore_members;
    no_flowlib;
    profile;
    quiet;
    strip_root;
    temp_dir;
    traces;
    verbose;
    slow_to_check_logging = Slow_to_check_logging.default;
    wait_for_recheck;
    include_suppressions;
    estimate_recheck_time;
    long_lived_workers;
    blocking_worker_communication;
    distributed;
  }

let make_saved_state_flags
    ?saved_state_allow_reinit
    ?saved_state_fetcher
    ?(saved_state_force_recheck = false)
    ?(saved_state_no_fallback = false)
    ?(saved_state_skip_version_check = false)
    ?(saved_state_verify = false)
    () =
  {
    CommandUtils.Saved_state_flags.saved_state_allow_reinit;
    saved_state_fetcher;
    saved_state_force_recheck;
    saved_state_no_fallback;
    saved_state_skip_version_check;
    saved_state_verify;
  }

let make_options
    ?(flowconfig_name = ".flowconfig")
    ?(flowconfig_hash = "")
    ?(flowconfig = FlowConfig.empty_config)
    ?lazy_mode
    ?(root = File_path.dummy_path)
    ?(options_flags = make_options_flags ())
    ?(saved_state_options_flags = make_saved_state_flags ())
    () =
  CommandUtils.make_options
    ~flowconfig_name
    ~flowconfig_hash
    ~flowconfig
    ~lazy_mode
    ~root
    ~options_flags
    ~saved_state_options_flags
