(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(************** file filter utils ***************)

type options = {
  default_lib_dir: Path.t option;
  ignores: (string * Str.regexp) list;
  untyped: (string * Str.regexp) list;
  declarations: (string * Str.regexp) list;
  includes: Path_matcher.t;
  lib_paths: Path.t list;
  module_file_exts: SSet.t;
  module_resource_exts: SSet.t;
  node_resolver_dirnames: string list;
}

let default_lib_dir options = options.default_lib_dir

let ignores options = options.ignores

let untyped options = options.untyped

let declarations options = options.declarations

let includes options = options.includes

let lib_paths options = options.lib_paths

let module_file_exts options = options.module_file_exts

let module_resource_exts options = options.module_resource_exts

let node_resolver_dirnames options = options.node_resolver_dirnames

(* During node module resolution, we need to look for node_modules/ directories
 * as we walk up the path. But checking each directory for them would be expensive.
 * So instead we memorize which directories contain node_modules/ directories.
 *
 * This is complicated by the node_resolver_dirnames option, which means
 * node_modules/ directories might go by other names. So we need to keep track
 * of which directories contain node_modules/ directories and which aliases we've
 * seen *)
let node_modules_containers = ref SMap.empty

let global_file_name = "(global)"

let flow_ext = ".flow"

let has_flow_ext file = File_key.check_suffix file flow_ext

let chop_flow_ext file =
  if has_flow_ext file then
    Some (File_key.chop_suffix file flow_ext)
  else
    None

let is_directory path = (try Sys.is_directory path with Sys_error _ -> false)

let is_prefix prefix =
  let prefix_with_sep =
    if String_utils.string_ends_with prefix Filename.dir_sep then
      prefix
    else
      prefix ^ Filename.dir_sep
  in
  (fun path -> path = prefix || String_utils.string_starts_with path prefix_with_sep)

let is_json_file filename = Utils_js.extension_of_filename filename = Some ".json"

(* This is the set of file extensions which we watch for changes *)
let get_all_watched_extensions options =
  SSet.union options.module_file_exts options.module_resource_exts

let is_valid_path =
  (* Given a file foo.bar.baz.bat, checks the extensions .bat, .baz.bat, and .bar.baz.bat *)
  let check_ext =
    (*    helper file_exts "foo.bar.baz" "" ".bat"
     * -> helper file_exts "foo.bar" ".bat" ".baz"
     * -> helper file_exts "foo" ".baz.bat" ".bar"
     * -> helper file_exts "" ".bar.baz.bat" ""
     * -> false
     *)
    let rec helper file_exts basename acc ext =
      ext <> ""
      &&
      let acc = ext ^ acc in
      SSet.mem acc file_exts
      ||
      let basename = Filename.chop_suffix basename ext in
      let ext = Filename.extension basename in
      helper file_exts basename acc ext
    in
    fun file_exts basename ->
      let extension = Filename.extension basename in
      let acc = "" in
      if extension = flow_ext then
        (* We treat bar.foo.flow like bar.foo *)
        let basename = Filename.chop_suffix basename flow_ext in
        helper file_exts basename acc (Filename.extension basename)
      else
        helper file_exts basename acc extension
  in
  let is_dot_file basename = basename <> "" && basename.[0] = '.' in
  fun ~options ->
    let file_exts = get_all_watched_extensions options in
    fun path ->
      let basename = Filename.basename path in
      (not (is_dot_file basename)) && (check_ext file_exts basename || basename = "package.json")

let is_node_module options path = List.mem (Filename.basename path) options.node_resolver_dirnames

let is_flow_file ~options =
  let is_valid_path = is_valid_path ~options in
  (fun path -> is_valid_path path && not (is_directory path))

let realpath path =
  match Sys_utils.realpath path with
  | Some path -> path
  | None -> path

(* perhaps this should error? *)

let make_path_absolute root path =
  if Filename.is_relative path then
    Path.concat root path
  else
    Path.make path

type file_kind =
  | Reg of string
  | Dir of string * bool
  | StatError of string
  | Other

(* Determines whether a path is a regular file, a directory, or something else
   like a pipe, socket or device. If `path` is a symbolic link, then it returns
   the type of the target of the symlink, and the target's real path. *)
let kind_of_path path =
  Unix.(
    try
      match (Sys_utils.lstat path).st_kind with
      | S_REG -> Reg path
      | S_LNK ->
        (try
           match (stat path).st_kind with
           | S_REG -> Reg (realpath path)
           | S_DIR -> Dir (realpath path, true)
           | _ -> Other
           (* Don't spew errors on broken symlinks *)
         with Unix_error (ENOENT, _, _) -> Other)
      | S_DIR -> Dir (path, false)
      | _ -> Other
    with
    | Unix_error (ENOENT, _, _) when Sys.win32 && String.length path >= 248 ->
      StatError
        (Utils_js.spf
           "On Windows, paths must be less than 248 characters for directories and 260 characters for files. This path has %d characters. Skipping %s"
           (String.length path)
           path)
    | Unix_error (e, _, _) ->
      StatError (Utils_js.spf "Skipping %s: %s\n%!" path (Unix.error_message e)))

let can_read path =
  try
    let () = Unix.access path [Unix.R_OK] in
    true
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
    ~node_module_filter ~path_filter ~realpath_filter ~error_filter ~dir_filter paths =
  let prefix_checkers = Base.List.map ~f:is_prefix paths in
  let rec process sz (acc, symlinks) files dir stack =
    if sz >= max_files then
      ((acc, symlinks), S_Dir (files, dir, stack))
    else
      match files with
      | [] -> process_stack sz (acc, symlinks) stack
      | file :: files ->
        let file =
          if dir = "" then
            file
          else
            Filename.concat dir file
        in
        (match kind_of_path file with
        | Reg real ->
          if path_filter file && (file = real || realpath_filter real) && can_read real then
            process (sz + 1) (real :: acc, symlinks) files dir stack
          else
            process sz (acc, symlinks) files dir stack
        | Dir (path, is_symlink) ->
          if not (dir_filter file && (file = path || dir_filter path)) then
            (* ignore the entire directory *)
            process sz (acc, symlinks) files dir stack
          else (
            if node_module_filter file then
              node_modules_containers :=
                SMap.add
                  ~combine:SSet.union
                  (Filename.dirname file)
                  (file |> Filename.basename |> SSet.singleton)
                  !node_modules_containers;
            if is_symlink then
              let symlinks =
                (* accumulates all of the symlinks that point to
                   directories outside of `paths`; symlinks that point to
                   directories already covered by `paths` will be found on
                   their own, so they are skipped. *)
                if not (List.exists (fun check -> check path) prefix_checkers) then
                  SSet.add path symlinks
                else
                  symlinks
              in
              process sz (acc, symlinks) files dir stack
            else
              let dirfiles = Array.to_list @@ try_readdir path in
              process sz (acc, symlinks) dirfiles file (S_Dir (files, dir, stack))
          )
        | StatError msg ->
          if error_filter file then prerr_endline msg;
          process sz (acc, symlinks) files dir stack
        | Other -> process sz (acc, symlinks) files dir stack)
  and process_stack sz accs = function
    | S_Nil -> (accs, S_Nil)
    | S_Dir (files, dir, stack) -> process sz accs files dir stack
  in
  let state = ref (S_Dir (paths, "", S_Nil)) in
  fun () ->
    let ((res, symlinks), st) = process_stack 0 ([], SSet.empty) !state in
    state := st;
    (res, symlinks)

(* Returns a closure that returns batches of files matching `path_filter` and/or
   `realpath_filter` (see `make_next_files_and_symlinks`), starting from `paths`
   and including any directories that are symlinked to even if they are outside
   of `paths`. *)
let make_next_files_following_symlinks
    ~node_module_filter ~path_filter ~realpath_filter ~error_filter ~dir_filter paths =
  let paths = Base.List.map ~f:Path.to_string paths in
  let cb =
    ref
      (make_next_files_and_symlinks
         ~node_module_filter
         ~path_filter
         ~realpath_filter
         ~error_filter
         ~dir_filter
         paths)
  in
  let symlinks = ref SSet.empty in
  let seen_symlinks = ref SSet.empty in
  let rec rec_cb () =
    let (files, new_symlinks) = !cb () in
    symlinks :=
      SSet.fold
        (fun symlink accum ->
          if SSet.mem symlink !seen_symlinks then
            accum
          else
            SSet.add symlink accum)
        new_symlinks
        !symlinks;
    seen_symlinks := SSet.union new_symlinks !seen_symlinks;
    let num_files = List.length files in
    if num_files > 0 then
      files
    else if SSet.is_empty !symlinks then
      []
    else
      let paths = SSet.elements !symlinks in
      symlinks := SSet.empty;

      (* since we're following a symlink, use realpath_filter for both *)
      cb :=
        make_next_files_and_symlinks
          ~node_module_filter
          ~path_filter:realpath_filter
          ~realpath_filter
          ~error_filter
          ~dir_filter
          paths;
      rec_cb ()
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
  (fun next -> get_all_rec next SSet.empty)

(* Local reference to the module exported by a file. Like other local references
   to modules imported by the file, it is a member of Context.module_map. *)
let module_ref file = File_key.to_string file

let lib_module_ref = ""

let dir_sep = Str.regexp "[/\\\\]"

let current_dir_name = Str.regexp_string Filename.current_dir_name

let parent_dir_name = Str.regexp_string Filename.parent_dir_name

let absolute_path_regexp = Str.regexp "^\\(/\\|[A-Za-z]:[/\\\\]\\)"

let project_root_token = Str.regexp_string "<PROJECT_ROOT>"

let dir_filter_of_options (options : options) f =
  let can_prune =
    (* for now, we can prune if there are no negations, and if none of the ignores
       are matching specific files (by using $ to match the end of the filename).

       TODO: if all negated ignores are absolute, then we can check whether a
       directory is a prefix of the negation and so might contain a file that needs
       to be un-ignored. similarly, if an ignore ends in $, we could still prune if
       the current path isn't a prefix of it. *)
    Base.List.for_all
      ~f:(fun (pattern, _rx) ->
        (not (String_utils.string_starts_with pattern "!"))
        && not (String_utils.string_ends_with pattern "$"))
      options.ignores
  in
  if can_prune then
    f
  else
    fun _path ->
  true

let init ?(flowlibs_only = false) (options : options) =
  let node_module_filter = is_node_module options in
  let libs =
    if flowlibs_only then
      []
    else
      options.lib_paths
  in
  let (libs, filter) =
    match options.default_lib_dir with
    | None -> (libs, is_valid_path ~options)
    | Some root ->
      let is_in_flowlib = is_prefix (Path.to_string root) in
      let is_valid_path = is_valid_path ~options in
      let filter path = is_in_flowlib path || is_valid_path path in
      (root :: libs, filter)
  in
  let dir_filter _path = true in
  (* preserve enumeration order *)
  let libs =
    if libs = [] then
      []
    else
      let get_next lib =
        let lib_str = Path.to_string lib in
        (* TODO: better to parse json files, not ignore them *)
        let filter' path = (path = lib_str || filter path) && not (is_json_file path) in
        make_next_files_following_symlinks
          ~node_module_filter
          ~path_filter:filter'
          ~realpath_filter:filter'
          ~error_filter:(fun _ -> true)
          ~dir_filter
          [lib]
      in
      libs |> Base.List.map ~f:(fun lib -> SSet.elements (get_all (get_next lib))) |> List.flatten
  in
  (libs, SSet.of_list libs)

let is_matching path pattern_list =
  List.fold_left
    (fun current (pattern, rx) ->
      if String_utils.string_starts_with pattern "!" then
        current && not (Str.string_match rx path 0)
      else
        current || Str.string_match rx path 0)
    false
    pattern_list

(* true if a file path matches an [ignore] entry in config *)
let is_ignored (options : options) path =
  (* On Windows, the path may use \ instead of /, but let's standardize the
   * ignore regex to use / *)
  let path = Sys_utils.normalize_filename_dir_sep path in
  is_matching path options.ignores

(* true if a file path matches an [untyped] entry in config *)
let is_untyped (options : options) path =
  (* On Windows, the path may use \ instead of /, but let's standardize the
   * ignore regex to use / *)
  let path = Sys_utils.normalize_filename_dir_sep path in
  is_matching path options.untyped

(* true if a file path matches a [declarations] entry in config *)
let is_declaration (options : options) path =
  (* On Windows, the path may use \ instead of /, but let's standardize the
   * ignore regex to use / *)
  let path = Sys_utils.normalize_filename_dir_sep path in
  is_matching path options.declarations

(* true if a file path matches an [include] path in config *)
let is_included options f = Path_matcher.matches options.includes f

let wanted ~options lib_fileset =
  let is_ignored_ = is_ignored options in
  (fun path -> (not (is_ignored_ path)) && not (SSet.mem path lib_fileset))

let watched_paths options = Path_matcher.stems options.includes

(**
 * Creates a "next" function (see also: `get_all`) for finding the files in a
 * given FlowConfig root. This means all the files under the root and all the
 * included files, minus the ignored files and the libs.
 *
 * If `all` is true, ignored files and libs are also returned.
 * If subdir is set, then we return the subset of files under subdir
 *)
let make_next_files ~root ~all ~subdir ~options ~libs =
  let node_module_filter = is_node_module options in
  let filter =
    if all then
      fun _ ->
    true
    else
      wanted ~options libs
  in
  (* The directories from which we start our search *)
  let starting_points =
    match subdir with
    | None -> watched_paths options
    | Some subdir -> [subdir]
  in
  let root_str = Path.to_string root in
  let is_valid_path = is_valid_path ~options in
  let realpath_filter path = is_valid_path path && filter path in
  let path_filter =
    (*
     * This function is very hot on large codebases, so specialize it up front
     * to minimize work.
     *)
    match subdir with
    | None ->
      fun path ->
        (String_utils.string_starts_with path root_str || is_included options path)
        && realpath_filter path
    | Some subdir ->
      (* The subdir might contain symlinks outside of the subdir. To prevent
       * these files from being returned, we modify the path filter to check
       * that the realpath starts with the subdir *)
      let subdir_str = Path.to_string subdir in
      fun path ->
        String_utils.string_starts_with path subdir_str
        && (String_utils.string_starts_with path root_str || is_included options path)
        && realpath_filter path
  in
  let dir_filter = dir_filter_of_options options filter in
  make_next_files_following_symlinks
    ~node_module_filter
    ~path_filter
    ~realpath_filter
    ~error_filter:filter
    ~dir_filter
    starting_points

let is_windows_root root =
  Sys.win32
  && String.length root = 2
  && root.[1] = ':'
  &&
  match root.[0] with
  | 'a' .. 'z'
  | 'A' .. 'Z' ->
    true
  | _ -> false

let rec normalize_path dir file = normalize_path_ dir (Str.split_delim dir_sep file)

and normalize_path_ dir names =
  match names with
  | dot :: names when dot = Filename.current_dir_name ->
    (* ./<names> => dir/names *)
    normalize_path_ dir names
  | dots :: names when dots = Filename.parent_dir_name ->
    (* ../<names> => parent(dir)/<names> *)
    normalize_path_ (Filename.dirname dir) names
  | "" :: names when names <> [] ->
    (* /<names> => /<names> *)
    construct_path Filename.dir_sep names
  | root :: names when is_windows_root root ->
    (* C:\<names> => C:\<names> *)
    construct_path (root ^ Filename.dir_sep) names
  | _ ->
    (* <names> => dir/<names> *)
    construct_path dir names

and construct_path = List.fold_left Filename.concat

(* relative_path: (/path/to/foo, /path/to/bar/baz) -> ../bar/baz
 * absolute_path (/path/to/foo, ../bar/baz) -> /path/to/bar/baz
 *
 * Both of these are designed to avoid using Path and realpath so that we don't actually read the
 * file system *)
let (relative_path, absolute_path) =
  let split_path =
    let rec f acc rest =
      let dir = Filename.dirname rest in
      if rest = dir then
        if Filename.is_relative dir (* True for things like ".", false for "/", "C:/" *) then
          acc
        (* "path/to/foo.js" becomes ["path"; "to"; "foo.js"] *)
        else
          match acc with
          | [] -> [dir] (* "/" becomes ["/"] *)
          | last_dir :: rest ->
            (* "/path/to/foo.js" becomes ["/path"; "to"; "foo.js"] *)
            Filename.concat dir last_dir :: rest
      else
        f (Filename.basename rest :: acc) dir
    in
    (fun path -> f [] path)
  in
  let rec make_relative = function
    | (dir1 :: root, dir2 :: file) when dir1 = dir2 -> make_relative (root, file)
    | (root, file) -> List.fold_left (fun path _ -> Filename.parent_dir_name :: path) file root
  in
  let make_relative root file =
    (* This functions is only used for displaying error location or creating saved state.
       We use '/' as file separator even on Windows. This simplify the test-suite script... *)
    make_relative (split_path root, split_path file) |> String.concat "/"
  in
  let rec absolute_path = function
    | (_ :: root, dir2 :: file) when dir2 = Filename.parent_dir_name -> absolute_path (root, file)
    | (root, file) -> List.rev_append root file
  in
  let absolute_path root file =
    (* Let's avoid creating paths like "/path/to/foo/." *)
    if file = Filename.current_dir_name || file = "" then
      root
    else
      absolute_path (List.rev @@ split_path root, split_path file)
      (* We may actually use these paths, so use the correct directory sep *)
      |> String.concat Filename.dir_sep
  in
  (make_relative, absolute_path)

(* helper to get the full path to the "flow-typed" library dir *)
let get_flowtyped_path root = make_path_absolute root "flow-typed"

(* helper: make different kinds of File_key.t from a path string *)
let filename_from_string ~options p =
  let resource_file_exts = options.module_resource_exts in
  match Utils_js.extension_of_filename p with
  | Some ".json" -> File_key.JsonFile p
  | Some ext when SSet.mem ext resource_file_exts -> File_key.ResourceFile p
  | _ -> File_key.SourceFile p

let mkdirp path_str perm =
  let parts = Str.split dir_sep path_str in
  (* If path_str is absolute, then path_prefix will be something like C:\ on
   * Windows and / on Linux *)
  let path_prefix =
    if Str.string_match absolute_path_regexp path_str 0 then
      Str.matched_string path_str
    else
      ""
  in
  (* On Windows, the Str.split above will mean the first part of an absolute
   * path will be something like C:, so let's remove that *)
  let parts =
    match parts with
    | first_part :: rest when first_part ^ Filename.dir_sep = path_prefix -> rest
    | parts -> parts
  in
  ignore
    (List.fold_left
       (fun path_str part ->
         let new_path_str = Filename.concat path_str part in
         Unix.((try mkdir new_path_str perm with Unix_error (EEXIST, "mkdir", _) -> ()));
         new_path_str)
       path_prefix
       parts)

(* Given a path, we want to know if it's in a node_modules/ directory or not. *)
let is_within_node_modules ~root ~options path =
  (* We use paths that are relative to the root, so that we ignore ancestor directories *)
  let path = relative_path (Path.to_string root) path in
  let directories = Str.split dir_sep path |> SSet.of_list in
  let node_resolver_dirnames = node_resolver_dirnames options |> SSet.of_list in
  not (SSet.inter directories node_resolver_dirnames |> SSet.is_empty)

(* realpath doesn't work for non-existent paths. So let's find the longest existent prefix, run
 * realpath on that, and then append the rest to it
 *)
let imaginary_realpath =
  (* Realpath fails on non-existent paths. So let's find the longest prefix which exists. We
   * recurse using Sys.file_exists, which is just a single stat, as opposed to realpath which
   * stats /foo, then /foo/bar, then /foo/bar/baz, etc *)
  let rec find_real_prefix path rev_suffix =
    let rev_suffix = Filename.basename path :: rev_suffix in
    let prefix = Filename.dirname path in
    (* Sys.file_exists should always return true for / and for . so we should never get into
     * infinite recursion. Let's assert that *)
    assert (prefix <> path);
    if Sys.file_exists prefix then
      (prefix, rev_suffix)
    else
      find_real_prefix prefix rev_suffix
  in
  fun path ->
    let (real_prefix, rev_suffix) = find_real_prefix path [] in
    match Sys_utils.realpath real_prefix with
    | None -> failwith (Utils_js.spf "Realpath failed for existent path %s" real_prefix)
    | Some abs -> List.fold_left Filename.concat abs rev_suffix

let canonicalize_filenames ~cwd ~handle_imaginary filenames =
  Base.List.map
    ~f:(fun filename ->
      let filename = Sys_utils.expanduser filename in
      (* normalize ~ *)
      let filename = normalize_path cwd filename in
      (* normalize ./ and ../ *)
      match Sys_utils.realpath filename with
      (* normalize symlinks *)
      | Some abs -> abs
      | None -> handle_imaginary filename)
    filenames

let expand_project_root_token ~root =
  let root = Path.to_string root |> Sys_utils.normalize_filename_dir_sep in
  (fun str -> str |> Str.split_delim project_root_token |> String.concat root)
