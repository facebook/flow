(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(************** file filter utils ***************)

open Utils

let global_file_name = "(global)"

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_prefix prefix =
  let prefix_with_sep = if str_ends_with prefix Filename.dir_sep
    then prefix
    else prefix ^ Filename.dir_sep
  in fun path -> path = prefix || str_starts_with path prefix_with_sep

let is_json_file path = Filename.check_suffix path ".json"

let is_valid_path ~options =
  let file_exts = Options.module_file_exts options in
  let is_valid_path_helper path =
    not (is_dot_file path) &&
    SSet.exists (Filename.check_suffix path) file_exts

  in fun path ->
    if Filename.check_suffix path FlowConfig.flow_ext
    (* foo.js.flow is valid if foo.js is valid *)
    then is_valid_path_helper (Filename.chop_suffix path FlowConfig.flow_ext)
    else is_valid_path_helper path

let is_flow_file ~options path =
  is_valid_path ~options path && not (is_directory path)

(* This is initialized early, before we work all the workers, so that each
 * worker isn't forced to find all the lib files themselves *)
let lib_files = ref None

let get_lib_files () = match !lib_files with
| None -> []
| Some (files, _) -> files

let get_lib_fileset () = match !lib_files with
| None -> SSet.empty
| Some (_, fileset) -> fileset

let realpath path = match Sys_utils.realpath path with
| Some path -> path
| None -> path (* perhaps this should error? *)

type file_kind =
| Reg of string
| Dir of string * bool
| Other

(* Determines whether a path is a regular file, a directory, or something else
   like a pipe, socket or device. If `path` is a symbolic link, then it returns
   the type of the target of the symlink, and the target's real path. *)
let kind_of_path path = Unix.(
  try match (Sys_utils.lstat path).st_kind with
  | S_REG -> Reg path
  | S_LNK ->
    (* TODO: can stat return a symlink? if yes, match S_LNK and recurse? *)
    (try begin match (stat path).st_kind with
    | S_REG -> Reg (realpath path)
    | S_DIR -> Dir (realpath path, true)
    | _ -> assert false
    (* Don't spew errors on broken symlinks *)
    end with Unix_error (ENOENT, _, _) -> Other)
  | S_DIR -> Dir (path, false)
  | _ -> Other
  with Unix_error (e, _, _) ->
    Printf.eprintf "%s %s\n%!" path (Unix.error_message e);
    Other
)

type stack =
  | S_Nil
  | S_Dir of string list * string * stack

let max_files = 1000
(* Calls out to `find <paths>` and immediately returns a closure. Running that
   closure will return a List of up to 1000 files whose paths match
   `path_filter`, and if the path is a symlink then whose real path matches
   `realpath_filter`; it also returns an SSet of all of the symlinks that
    point to _directories_ outside of `paths`. *)
let make_next_files_and_symlinks
    ~path_filter ~realpath_filter paths =
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
          if path_filter file && (file = real || realpath_filter real)
          then process (sz+1) (real :: acc, symlinks) files dir stack
          else process sz (acc, symlinks) files dir stack
        | Dir (path, is_symlink) ->
          let dirfiles = Array.to_list @@ Sys.readdir path in
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
        | _ ->
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
let make_next_files_following_symlinks ~path_filter ~realpath_filter paths =
  let paths = List.map Path.to_string paths in
  let cb = ref (make_next_files_and_symlinks
    ~path_filter ~realpath_filter paths
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
        ~path_filter:realpath_filter ~realpath_filter paths;
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
  match !lib_files with
  | Some _ -> ()
  | None -> (
    let tmp_dir = Options.temp_dir options in
    let include_default_libs = Options.include_default_libs options in
    let libs = Options.lib_paths options in
    let libs, filter = if not include_default_libs
      then libs, is_valid_path ~options
      else
        let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
        let is_in_flowlib = is_prefix (Path.to_string root) in
        let filter path = is_in_flowlib path || is_valid_path ~options path in
        if Flowlib.extract_flowlib root
        then root::libs, filter
        else begin
          let msg = "Could not locate flowlib files" in
          FlowExitStatus.(exit ~msg Could_not_find_flowconfig)
        end
    in
    (* preserve enumeration order *)
    let libs = if libs = []
      then []
      else
        let get_next = make_next_files_following_symlinks
          ~path_filter:filter
          ~realpath_filter:filter
        in
        let exp_list = libs |> List.map (fun lib ->
          let expanded = SSet.elements (get_all (get_next [lib])) in
          expanded
        ) in
        List.flatten exp_list
    in
    lib_files := Some (libs, set_of_list libs)
  )

let is_lib_file p =
  SSet.mem p (get_lib_fileset ())

let lib_module = ""

let dir_sep = Str.regexp "[/\\\\]"
let current_dir_name = Str.regexp_string Filename.current_dir_name
let parent_dir_name = Str.regexp_string Filename.parent_dir_name
let absolute_path = Str.regexp "^\\(/\\|[A-Za-z]:\\)"

let wanted config =
  let is_excluded = FlowConfig.is_excluded config in
  fun path -> not (is_excluded path) && not (is_lib_file path)

let make_next_files ~options =
  let root = Options.root options in
  let config = FlowConfig.get root in
  let filter = wanted config in
  let others = config.FlowConfig.include_stems in
  let sroot = Path.to_string root in
  let realpath_filter path = is_valid_path ~options path && filter path in
  let path_filter path =
    (str_starts_with path sroot || FlowConfig.is_included config path)
    && realpath_filter path
  in
  make_next_files_following_symlinks
    ~path_filter ~realpath_filter (root::others)

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
