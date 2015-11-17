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

let config_options : FlowConfig.Opts.t option ref = ref None

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_valid_path =
  let is_valid_path_helper path =
    let file_exts = FlowConfig.((get_unsafe ()).options.Opts.module_file_exts) in
    not (is_dot_file path) &&
    SSet.exists (Filename.check_suffix path) file_exts

  in fun path ->
    if Filename.check_suffix path FlowConfig.flow_ext
    (* foo.js.flow is valid if foo.js is valid *)
    then is_valid_path_helper (Filename.chop_suffix path FlowConfig.flow_ext)
    else is_valid_path_helper path

let is_flow_file path = is_valid_path path && not (is_directory path)

(* This is initialized early, before we work all the workers, so that each
 * worker isn't forced to find all the lib files themselves *)
let lib_files = ref None

let get_lib_files () = match !lib_files with
| None -> []
| Some (files, fileset) -> files

let get_lib_fileset () = match !lib_files with
| None -> SSet.empty
| Some (files, fileset) -> fileset

let restore_lib_files s =
  assert (!lib_files = None);
  lib_files := Some s

let realpath path = match Sys_utils.realpath path with
| Some path -> path
| None -> path (* perhaps this should error? *)

type file_kind =
| Reg of string
| Dir of string
| Other

(* Determines whether a path is a regular file, a directory, or something else
   like a pipe, socket or device. If `path` is a symbolic link, then it returns
   the type of the target of the symlink, and the target's real path. *)
let kind_of_path path = Unix.(
  try match (lstat path).st_kind with
  | S_REG -> Reg path
  | S_LNK ->
    (* TODO: can stat return a symlink? if yes, match S_LNK and recurse? *)
    begin match (stat path).st_kind with
    | S_REG -> Reg (realpath path)
    | S_DIR -> Dir (realpath path)
    | _ -> Other
    end
  | _ -> Other
  with Unix_error _ -> Other
)

let escape_spaces = Str.global_replace (Str.regexp " ") "\\ "

(* Calls out to `find <paths>` and immediately returns a closure. Running that
   closure will return a List of up to 1000 files whose paths match
   `path_filter`, and if the path is a symlink then whose real path matches
   `realpath_filter`; it also returns an SSet of all of the symlinks that
    point to _directories_ outside of `paths`. *)
let make_next_files_and_symlinks ~path_filter ~realpath_filter paths =
  let escaped_paths = List.map escape_spaces paths in
  let paths_str = String.concat " " escaped_paths in
  let ic = Unix.open_process_in ("find "^paths_str) in
  let done_ = ref false in
  (* This is subtle, but to optimize latency, we open the process and
   * then return a closure immediately. That way 'find' gets started
   * in parallel and will be ready when we need to get the list of
   * files (although it will be stopped very soon as the pipe buffer
   * will get full very quickly).
   *)
  fun () ->
    if !done_
    then [], SSet.empty
    else
      let result = ref [] in

      (* accumulates all of the symlinks that point to directories outside of
         `paths`; symlinks that point to directories already covered by `paths`
         will be found on their own, so they are skipped. *)
      let symlinks = ref SSet.empty in

      let i = ref 0 in
      try
        while !i < 1000 do
          let path = input_line ic in
          match kind_of_path path with
          | Reg real ->
            if path_filter path && (path = real || realpath_filter real)
            then begin
              result := real :: !result;
              incr i;
            end
          | Dir path ->
            let is_prefix p = Filename.(
              let p = if str_ends_with p dir_sep then p else p ^ dir_sep in
              str_starts_with path p
            ) in
            if not (List.exists is_prefix paths) then
              symlinks := SSet.add path !symlinks
          | Other -> ()
        done;
        List.rev !result, !symlinks
      with End_of_file ->
        done_ := true;
        (try ignore (Unix.close_process_in ic) with _ -> ());
        List.rev !result, !symlinks

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

let init ~include_default_libs ~tmp_dir libs =
  match !lib_files with
  | Some (files, fileset) -> ()
  | None -> (
    config_options := Some FlowConfig.((get_unsafe ()).options);
    let libs = if not include_default_libs
      then libs
      else
        let root = Path.make (Tmp.temp_dir tmp_dir "flowlib") in
        if Flowlib.extract_flowlib root
        then root::libs
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
          ~path_filter:is_valid_path
          ~realpath_filter:is_valid_path
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

let dir_sep = Str.regexp_string Filename.dir_sep
let current_dir_name = Str.regexp_string Filename.current_dir_name
let parent_dir_name = Str.regexp_string Filename.parent_dir_name

let wanted config =
  let is_excluded = FlowConfig.is_excluded config in
  fun path -> not (is_excluded path) && not (is_lib_file path)

let make_next_files root =
  let config = FlowConfig.get root in
  let filter = wanted config in
  let others = config.FlowConfig.include_stems in
  let sroot = Path.to_string root in
  let realpath_filter path = is_valid_path path && filter path in
  let path_filter path =
    (str_starts_with path sroot || FlowConfig.is_included config path)
    && realpath_filter path
  in
  make_next_files_following_symlinks
    ~path_filter ~realpath_filter (root::others)

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
  | _ ->
      (* <names> => dir/<names> *)
      construct_path dir names

and construct_path = List.fold_left Filename.concat

(* return a list of all file paths ending in 'package.json'
   under root or includes *)
let package_json root =
  let config = FlowConfig.get root in
  let sroot = Path.to_string root in
  let want = wanted config in
  let realpath_filter path =
    (Filename.basename path) = "package.json" && want path
  in
  let path_filter path =
    (str_starts_with path sroot || FlowConfig.is_included config path)
    && realpath_filter path
  in
  let others = config.FlowConfig.include_stems in
  let get_next = make_next_files_following_symlinks
    ~path_filter
    ~realpath_filter
    (root::others)
  in
  get_all get_next

(* helper: make relative path from root to file *)
let relative_path =
  let split_path = Str.split dir_sep in
  let rec make_relative = function
    | (dir1::root, dir2::file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) ->
        List.fold_left (fun path _ -> Filename.parent_dir_name::path) file root
  in
  fun root file ->
    make_relative (split_path root, split_path file)
    |> String.concat Filename.dir_sep
