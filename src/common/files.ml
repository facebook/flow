(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(************** file filter utils ***************)

let global_file_name = "(global)"
let flow_ext = ".flow"

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_prefix prefix =
  let prefix_with_sep = if String_utils.string_ends_with prefix Filename.dir_sep
    then prefix
    else prefix ^ Filename.dir_sep
  in fun path ->
    path = prefix || String_utils.string_starts_with path prefix_with_sep

let is_json_file filename =
  Utils_js.extension_of_filename filename = Some ".json"

let is_valid_path ~options =
  let file_exts = SSet.union
    (Options.module_file_exts options)
    (Options.module_resource_exts options) in
  let is_valid_path_helper path =
    not (is_dot_file path) &&
    (SSet.exists (Filename.check_suffix path) file_exts ||
      Filename.basename path = "package.json")

  in fun path ->
    if Filename.check_suffix path flow_ext
    (* foo.js.flow is valid if foo.js is valid *)
    then is_valid_path_helper (Filename.chop_suffix path flow_ext)
    else is_valid_path_helper path

let is_flow_file ~options path =
  is_valid_path ~options path && not (is_directory path)

let realpath path = match Sys_utils.realpath path with
| Some path -> path
| None -> path (* perhaps this should error? *)

let make_path_absolute root path =
  if Filename.is_relative path
  then Path.concat root path
  else Path.make path

type file_kind =
| Reg of string
| Dir of string * bool
| StatError of string
| Other

(* Determines whether a path is a regular file, a directory, or something else
   like a pipe, socket or device. If `path` is a symbolic link, then it returns
   the type of the target of the symlink, and the target's real path. *)
let kind_of_path path = Unix.(
  try match (Sys_utils.lstat path).st_kind with
  | S_REG -> Reg path
  | S_LNK ->
    (try begin match (stat path).st_kind with
    | S_REG -> Reg (realpath path)
    | S_DIR -> Dir (realpath path, true)
    | _ -> Other
    (* Don't spew errors on broken symlinks *)
    end with Unix_error (ENOENT, _, _) -> Other)
  | S_DIR -> Dir (path, false)
  | _ -> Other
  with
  | Unix_error (ENOENT, _, _) when Sys.win32 && String.length path >= 248 ->
    StatError (
      Utils_js.spf
        "On Windows, paths must be less than 248 characters for directories \
         and 260 characters for files. This path has %d characters. Skipping %s"
        (String.length path)
        path
    )
  | Unix_error (e, _, _) ->
    StatError (Utils_js.spf "Skipping %s: %s\n%!" path (Unix.error_message e))
)

let can_read path =
  try let () = Unix.access path [Unix.R_OK] in true
  with Unix.Unix_error (e, _, _) ->
    Printf.eprintf "Skipping %s: %s\n%!" path (Unix.error_message e);
    false

let try_readdir path =
  try Sys.readdir path
  with Sys_error msg ->
    Printf.eprintf "Skipping %s\n%!" msg;
    [||]

type stack =
  | S_Nil
  | S_Dir of string list * string * stack

let max_files = 1000

(* Calls out to `find <paths>` and immediately returns a closure. Running that
   closure will return a List of up to 1000 files whose paths match
   `path_filter`, and if the path is a symlink then whose real path matches
   `realpath_filter`; it also returns an SSet of all of the symlinks that
    point to _directories_ outside of `paths`.

    If kind_of_path fails, then we only emit a warning if error_filter passes *)
let make_next_files_and_symlinks
    ~path_filter ~realpath_filter ~error_filter paths =
  let prefix_checkers = List.map is_prefix paths in
  let rec process sz (acc, symlinks) files dir stack =
    if sz >= max_files then
      ((acc, symlinks), S_Dir (files, dir, stack))
    else
      match files with
      | [] -> process_stack sz (acc, symlinks) stack
      | file :: files ->
        let file = if dir = "" then file else Filename.concat dir file in
        match kind_of_path file with
        | Reg real ->
          if path_filter file && (file = real || realpath_filter real) && can_read real
          then process (sz+1) (real :: acc, symlinks) files dir stack
          else process sz (acc, symlinks) files dir stack
        | Dir (path, is_symlink) ->
          let dirfiles = Array.to_list @@ try_readdir path in
          let symlinks =
            (* accumulates all of the symlinks that point to
               directories outside of `paths`; symlinks that point to
               directories already covered by `paths` will be found on
               their own, so they are skipped. *)
            if not (List.exists (fun check -> check path) prefix_checkers) then
              SSet.add path symlinks
            else
              symlinks in
          if is_symlink then
            process sz (acc, symlinks) files dir stack
          else
            process sz (acc, symlinks) dirfiles file (S_Dir (files, dir, stack))
        | StatError msg ->
          if error_filter file then prerr_endline msg;
          process sz (acc, symlinks) files dir stack
        | Other ->
          process sz (acc, symlinks) files dir stack
  and process_stack sz accs = function
    | S_Nil -> (accs, S_Nil)
    | S_Dir (files, dir, stack) -> process sz accs files dir stack in
  let state = ref (S_Dir (paths, "", S_Nil)) in
  fun () ->
    let (res, symlinks), st = process_stack 0 ([], SSet.empty) !state in
    state := st;
    res, symlinks

(* Returns a closure that returns batches of files matching `path_filter` and/or
   `realpath_filter` (see `make_next_files_and_symlinks`), starting from `paths`
   and including any directories that are symlinked to even if they are outside
   of `paths`. *)
let make_next_files_following_symlinks
  ~path_filter
  ~realpath_filter
  ~error_filter
  paths =
  let paths = List.map Path.to_string paths in
  let cb = ref (make_next_files_and_symlinks
    ~path_filter ~realpath_filter ~error_filter paths
  ) in
  let symlinks = ref SSet.empty in
  let seen_symlinks = ref SSet.empty in
  let rec rec_cb () =
    let files, new_symlinks = !cb () in
    symlinks := SSet.fold (fun symlink accum ->
      if SSet.mem symlink !seen_symlinks then accum
      else SSet.add symlink accum
    ) new_symlinks !symlinks;
    seen_symlinks := SSet.union new_symlinks !seen_symlinks;
    let num_files = List.length files in
    if num_files > 0 then files
    else if (SSet.is_empty !symlinks) then []
    else begin
      let paths = SSet.elements !symlinks in
      symlinks := SSet.empty;
      (* since we're following a symlink, use realpath_filter for both *)
      cb := make_next_files_and_symlinks
        ~path_filter:realpath_filter ~realpath_filter ~error_filter paths;
      rec_cb ()
    end
  in
  rec_cb

(* Calls `next` repeatedly until it is resolved, returning a SSet of results *)
let get_all =
  let rec get_all_rec next accum =
    match next () with
    | [] -> accum
    | result ->
      let accum = List.fold_left (fun set x -> SSet.add x set) accum result in
      get_all_rec next accum
  in
  fun next -> get_all_rec next SSet.empty

let init options =
  let libs = Options.lib_paths options in
  let libs, filter = match Options.default_lib_dir options with
    | None -> libs, is_valid_path ~options
    | Some root ->
      let is_in_flowlib = is_prefix (Path.to_string root) in
      let filter path = is_in_flowlib path || is_valid_path ~options path in
      root::libs, filter
  in
  (* preserve enumeration order *)
  let libs = if libs = []
    then []
    else
      let get_next lib =
        let lib_str = Path.to_string lib in
        let filter' path = path = lib_str || filter path in
        make_next_files_following_symlinks
          ~path_filter:filter'
          ~realpath_filter:filter'
          ~error_filter:(fun _ -> true)
          [lib]
      in
      libs
      |> List.map (fun lib -> SSet.elements (get_all (get_next lib)))
      |> List.flatten
  in
  (libs, Utils_js.set_of_list libs)


let lib_module = ""

let dir_sep = Str.regexp "[/\\\\]"
let current_dir_name = Str.regexp_string Filename.current_dir_name
let parent_dir_name = Str.regexp_string Filename.parent_dir_name
let absolute_path = Str.regexp "^\\(/\\|[A-Za-z]:[/\\\\]\\)"

(* true if a file path matches an [ignore] entry in config *)
let is_ignored options =
  let list = List.map snd (Options.ignores options) in
  fun path ->
    (* On Windows, the path may use \ instead of /, but let's standardize the
     * ignore regex to use / *)
    let path = Sys_utils.normalize_filename_dir_sep path in
    List.exists (fun rx -> Str.string_match rx path 0) list

(* true if a file path matches an [include] path in config *)
let is_included options f = Path_matcher.matches (Options.includes options) f

let wanted ~options lib_fileset =
  let is_ignored_ = is_ignored options in
  fun path -> not (is_ignored_ path) && not (SSet.mem path lib_fileset)

(**
 * Creates a "next" function (see also: `get_all`) for finding the files in a
 * given FlowConfig root. Also takes an optional `subdir` argument to restrict
 * the set of files to things that sit under a given sub-directory of root. If
 * `subdir` is none, all JS files under the root will be returned.
 *)
let make_next_files ~subdir ~options ~libs =
  let root = Options.root options in
  let filter = wanted ~options libs in
  let others = Path_matcher.stems (Options.includes options) in
  let root_str= Path.to_string root in
  let realpath_filter path = is_valid_path ~options path && filter path in
  let path_filter =
    (**
     * This function is very hot on large codebases, so specialize it up front
     * to minimize work.
     *)
    match subdir with
    | None ->
      (fun path ->
        (String_utils.string_starts_with path root_str || is_included options path)
        && realpath_filter path
      )
    | Some subdir ->
      let subdir_str = Path.to_string subdir in
      (fun path ->
        (String_utils.string_starts_with path subdir_str)
        && realpath_filter path
      )
  in
  make_next_files_following_symlinks
    ~path_filter ~realpath_filter ~error_filter:filter (root::others)

let is_windows_root root =
  Sys.win32 &&
  String.length root = 2 &&
  root.[1] = ':' &&
  match root.[0] with
    | 'a'..'z' | 'A'..'Z' -> true
    | _ -> false

let rec normalize_path dir file =
  normalize_path_ dir (Str.split_delim dir_sep file)

and normalize_path_ dir names =
  match names with
  | dot::names when dot = Filename.current_dir_name ->
      (* ./<names> => dir/names *)
      normalize_path_ dir names
  | dots::names when dots = Filename.parent_dir_name ->
      (* ../<names> => parent(dir)/<names> *)
      normalize_path_ (Filename.dirname dir) names
  | ""::names when names <> [] ->
      (* /<names> => /<names> *)
      construct_path Filename.dir_sep names
  | root::names when is_windows_root root ->
      (* C:\<names> => C:\<names> *)
      construct_path (root ^ Filename.dir_sep) names
  | _ ->
      (* <names> => dir/<names> *)
      construct_path dir names

and construct_path = List.fold_left Filename.concat

(* helper: make relative path from root to file *)
let relative_path =
  let split_path = Str.split dir_sep in
  let rec make_relative = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) ->
        List.fold_left (fun path _ -> Filename.parent_dir_name::path) file root
  in
  fun root file ->
    (* This functions is only used for displaying error location.
       We use '/' as file separator even on Windows. This simplify
       the test-suite script... *)
    make_relative (split_path root, split_path file)
    |> String.concat "/"

(* helper to get the full path to the "flow-typed" library dir *)
let get_flowtyped_path root =
  make_path_absolute root "flow-typed"

(* helper: make different kinds of Loc.filename from a path string *)
let filename_from_string ~options p =
  let resource_file_exts = Options.module_resource_exts options in
  match Utils_js.extension_of_filename p with
  | Some ".json" -> Loc.JsonFile p
  | Some ext when SSet.mem ext resource_file_exts -> Loc.ResourceFile p
  | _ -> Loc.SourceFile p

let mkdirp path_str perm =
  let parts = Str.split dir_sep path_str in
  (* If path_str is absolute, then path_prefix will be something like C:\ on
   * Windows and / on Linux *)
  let path_prefix =
    if Str.string_match absolute_path path_str 0
    then Str.matched_string path_str
    else "" in

  (* On Windows, the Str.split above will mean the first part of an absolute
   * path will be something like C:, so let's remove that *)
  let parts = match parts with
  | first_part::rest when first_part ^ Filename.dir_sep = path_prefix -> rest
  | parts -> parts in

  ignore (List.fold_left (fun path_str part ->
    let new_path_str = Filename.concat path_str part in
    Unix.(
      try mkdir new_path_str perm
      with Unix_error (EEXIST, "mkdir", _) -> ()
    );
    new_path_str
  ) path_prefix parts);
