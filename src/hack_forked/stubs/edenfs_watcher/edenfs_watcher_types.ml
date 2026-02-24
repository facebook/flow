(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type translation_telemetry = {
  commit_transition_count: int;
  commit_transition_duration: int;
  directory_rename_count: int;
  directory_rename_duration: int;
  raw_changes_count: int;
  translated_files_count: int;
  duration: int;
}

let yojson_of_translation_telemetry _ = failwith "not implemented"

type async_telemetry = {
  worker_restart_count: int;
  notification_count: int;
  aggregated_translation_telemetry: translation_telemetry;
}

let yojson_of_async_telemetry _ = failwith "not implemented"

type instance_get_changes_sync_telemetry = {
  duration: int;
  eden_get_changes_since_duration: int;
  async_telemetry: async_telemetry;
  worker_reset_duration: int;
  translation_telemetry: translation_telemetry option;
}

let yojson_of_instance_get_changes_sync_telemetry _ = failwith "not implemented"

type instance_get_changes_async_telemetry = {
  duration: int;
  async_telemetry: async_telemetry;
}

let yojson_of_instance_get_changes_async_telemetry _ = failwith "not implemented"

type instance_get_all_files_telemetry = {
  duration: int;
  eden_glob_files_duration: int;
  post_processing_duration: int;
}

let yojson_of_instance_get_all_files_telemetry _ = failwith "not implemented"

type standalone_get_changes_since_telemetry = {
  duration: int;
  setup_duration: int;
  eden_get_changes_since_duration: int;
  translation_telemetry: translation_telemetry option;
}

let yojson_of_standalone_get_changes_since_telemetry _ = failwith "not implemented"

type watch_spec = {
  extensions: string list;
  file_names: string list;
  include_dirs: string list;
  exclude_dirs: string list;
}

type settings = {
  root: File_path.t;
  watch_spec: watch_spec;
  debug_logging: bool;
  timeout_secs: int;
  throttle_time_ms: int;
  report_telemetry: bool;
  state_tracking: bool;
  sync_queries_obey_deferral: bool;
  defer_states: string list;
}

type changes =
  | FileChanges of string list
  | CommitTransition of {
      from_commit: string;
      to_commit: string;
      file_changes: string list;
    }
  | StateEnter of string
  | StateLeave of string
[@@deriving show]

type edenfs_watcher_error =
  | EdenfsWatcherError of string
  | LostChanges of string
  | NonEdenMount
[@@deriving show]
