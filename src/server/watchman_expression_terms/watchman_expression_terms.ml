(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Builds Watchman expression terms for Flow's file watching.
    Uses shared helpers from File_watcher_spec for consistency with EdenFS watcher. *)

let make ~options =
  let module J = Hh_json_helpers.AdhocJsonHelpers in
  let file_options = Options.file_options options in
  let suffixes = File_watcher_spec.get_suffixes file_options in
  (* Unfortunately watchman can't deal with absolute paths. Its "wholename" feature only
   * works for relative paths to the watch root, and we don't know the watch root until we
   * init.
   *
   * Luckily, all we really need is to specify a superset of the files we care about. So
   * watching all .flowconfigs instead of just our .flowconfig is fine *)
  let basenames = File_watcher_spec.get_file_names options in
  [
    J.strlist ["type"; "f"];
    (* Watch for files *)
    J.pred "anyof" @@ [J.assoc_strlist "suffix" suffixes; J.assoc_strlist "name" basenames];
    J.pred "not"
    @@ [
         (* Ignore changes in source control dirs *)
         J.pred "anyof"
         @@ List.map (fun dir -> J.strlist ["dirname"; dir]) File_watcher_spec.exclude_dirs;
       ];
  ]
