(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Shared helpers for building file watcher specifications.
    Used by both Watchman and EdenFS watchers to ensure consistency. *)

(** Convert Flow's extension list (e.g., [".js"; ".jsx"; ".fb.js"]) to simple
    suffixes (e.g., ["js"; "jsx"; "js"]).

    Flow's extensions include leading dots and may be multi-part (like ".fb.js").
    File watchers (Watchman suffix matching, Rust Path::extension()) only match
    the last extension part, so we:
    1. Extract just the last extension (e.g., ".fb.js" -> ".js")
    2. Strip the leading dot (e.g., ".js" -> "js")

    Note: This may produce duplicate entries (e.g., ".js" and ".fb.js" both become "js")
    which is fine for both Watchman and EdenFS as they deduplicate internally. *)
let get_suffixes file_options =
  let exts = SSet.elements @@ Files.get_all_watched_extensions file_options in
  let exts = Files.flow_ext :: exts in
  exts
  (* Turn .foo.bar into .bar, since suffix can't deal with multi-part extensions *)
  |> List.map (fun ext -> Filename.extension ("foo" ^ ext))
  (* Strip off the leading '.' *)
  |> List.map (fun ext ->
         if ext <> "" && ext.[0] = '.' then
           String.sub ext 1 (String.length ext - 1)
         else
           ext
     )

(** Get the list of specific file names to watch (e.g., [".flowconfig"; "package.json"]) *)
let get_file_names options =
  let flowconfig_name = Options.flowconfig_name options in
  let flowconfig_path = Server_files_js.config_file flowconfig_name @@ Options.root options in
  ["package.json"; Filename.basename flowconfig_path]

(** Get the list of directories to watch as absolute paths.
    Used by both Watchman (for its roots) and EdenFS watcher (for filtering).
    This includes paths from .flowconfig [include] which may be outside the root. *)
let get_include_dirs_absolute options =
  let file_options = Options.file_options options in
  List.map File_path.to_string (Files.watched_paths file_options)

(** Standard source control directories to exclude from watching *)
let exclude_dirs = [".hg"; ".git"; ".svn"]
