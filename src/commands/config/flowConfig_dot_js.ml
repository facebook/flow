open Js_of_ocaml
open FlowConfig

let (%%) f g x = f (g x)

let to_js_string_array (list: string list) =
  Js.array (Array.of_list (List.map Js.string list))

let array_of_list f lst =
  Array.of_list (List.map f lst)

let set_to_js
  (type t elt)
  (module S: Set.S with type t = t and type elt = elt)
  (t: t)
  (f: elt -> 'b)
  : 'b Js.js_array Js.t
=
  let els = S.elements t in
  let set = Js.array @@ Array.of_list @@ List.map f els in
  set

let sset_to_js t =
  set_to_js (module SSet) t Js.string

let strict_mode_settings_to_js (t: StrictModeSettings.t) =
  let els = StrictModeSettings.elements t in
  let set = Js.array @@ Array.of_list @@ List.map (Js.string %% Lints.string_of_kind) els in
  set

let lintmap_to_js (t: 'a Lints.LintMap.t) (f: 'a -> 'b) =
  let module M = Lints.LintMap in
  let props = Array.of_list (
    M.fold (fun k v acc ->
      (Lints.string_of_kind k, Js.Unsafe.inject (f v)) :: acc) t []
  ) in
  let map = Js.Unsafe.inject (Js.Unsafe.obj props) in
  map

let js_opt t f = match t with
  | Some t -> Js.some (f t)
  | None -> Js.null

let opts_to_js t = FlowConfig.(object%js
  val all = Js.bool (all t)
  val emoji = Js.bool (emoji t)
  val enableConstParams = Js.bool (enable_const_params t)
  val enforceStrictCallArity = Js.bool (enforce_strict_call_arity t)
  val enforceWellFormedExports = Js.bool (enforce_well_formed_exports t)
  val enforceWellFormedExportsWhitelist = to_js_string_array (enforce_well_formed_exports_whitelist t)
  val esproposalClassInstanceFields = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_class_instance_fields t))
  val esproposalClassStaticFields = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_class_static_fields t))
  val esproposalDecorators = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_decorators t))
  val esproposalExportStarAs = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_export_star_as t))
  val esproposalNullishCoalescing = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_nullish_coalescing t))
  val esproposalOptionalChaining = Js.string (Options.esproposal_feature_mode_to_string  (esproposal_optional_chaining t))
  val facebookFbs = js_opt (facebook_fbs t) Js.string
  val facebookFbt = js_opt (facebook_fbt t) Js.string
  val fileWatcher = js_opt (file_watcher t) (Js.string %% Options.file_watcher_to_string)
  val hasteModuleRefPrefix = js_opt (haste_module_ref_prefix t) Js.string
  val hasteNameReducers = Js.null (* TODO: Regexp *)
  val hastePathsBlacklist = to_js_string_array (haste_paths_blacklist t)
  val hastePathsWhitelist = to_js_string_array (haste_paths_whitelist t)
  val hasteUseNameReducers = Js.bool (haste_use_name_reducers t)
  val ignoreNonLiteralRequires = Js.bool (ignore_non_literal_requires t)
  val includeWarnings = Js.bool (include_warnings t)
  val lazyMode = js_opt (lazy_mode t) (Js.string %% Options.lazy_mode_to_string)
  val logFile = js_opt (log_file t) (fun t -> Js.string (Path.to_string t))
  val maxHeaderTokens = max_header_tokens t
  val maxLiteralLength = max_literal_length t
  val maxWorkers = max_workers t
  val mergeTimeout = js_opt (merge_timeout t) (fun t -> t)
  val moduleFileExts = sset_to_js (module_file_exts t)
  val moduleNameMappers = Js.null (* TODO: Regexp *)
  val moduleResolver = js_opt (module_resolver t) (fun t -> Js.string (Path.to_string t))
  val moduleResourceExts = sset_to_js (module_resource_exts t)
  val moduleSystem = Js.string (Options.module_system_to_string (module_system t))
  val modulesAreUseStrict = Js.bool (modules_are_use_strict t)
  val mungeUnderscores = Js.bool (munge_underscores t)
  val noFlowlib = Js.bool (no_flowlib t)
  val nodeResolverDirnames = to_js_string_array (node_resolver_dirnames t)
  val rootName = js_opt (root_name t) Js.string
  val savedStateFetcher = Js.string (Options.saved_state_fetcher_to_string (saved_state_fetcher t))
  val shmDepTablePow = shm_dep_table_pow t
  val shmDirs = to_js_string_array (shm_dirs t)
  val shmGlobalSize = shm_global_size t
  val shmHashTablePow = shm_hash_table_pow t
  val shmHeapSize = shm_heap_size t
  val shmLogLevel = shm_log_level t
  val shmMinAvail = shm_min_avail t
  val suppressComments = Js.array (Array.of_list [Js.null]) (* TODO: Regexp *)
  val suppressTypes = sset_to_js (suppress_types t)
  val tempDir = Js.string (temp_dir t)
  val traces = traces t
  val trustMode = Js.string (Options.trust_mode_to_string (trust_mode t))
  val version = js_opt (required_version t) Js.string
  val waitForRecheck = Js.bool (wait_for_recheck t)
  val weak = Js.bool (weak t)
end)

let file_key_to_js (t: File_key.t) =
  let make1 typ str1 = object%js
    val _type = Js.string typ
    val value = Js.string str1
  end in
  let make0 typ = object%js
    val _type = Js.string typ
  end in
  File_key.(
    match t with
      | LibFile s -> Js.Unsafe.inject (make1 "LibFile" s)
      | SourceFile s -> Js.Unsafe.inject (make1 "SourceFile" s)
      | JsonFile s -> Js.Unsafe.inject (make1 "JsonFile" s)
      | ResourceFile s -> Js.Unsafe.inject (make1 "ResourceFile" s)
      | Builtins -> Js.Unsafe.inject (make0 "Builtins")
  )

let position_to_js (p: Loc.position) = Loc.(object%js
  val line = p.line
  val column = p.column
end)

let loc_to_js (l: Loc.t) = Loc.(object%js
  val source = js_opt l.source file_key_to_js
  val start = position_to_js l.start
  val _end = position_to_js l._end
end)

let severity_lintSettings_to_js (t: Severity.severity LintSettings.t) = object%js
  val defaultValue = Js.string @@ Severity.string_of_severity (LintSettings.get_default t)
  val explicitValues =
    let f (s, l) =
      let s' = Js.string @@ Severity.string_of_severity s in
      let l' = js_opt l loc_to_js in
      Js.array [|Js.Unsafe.inject s'; Js.Unsafe.inject l'|]
    in
    lintmap_to_js (LintSettings.get_explicit_values t) f
end

let config_to_js (config: config) = FlowConfig.(object%js
  val ignores = to_js_string_array (ignores config)
  val untyped = to_js_string_array (untyped config)
  val declarations = to_js_string_array (declarations config)
  val includes = to_js_string_array (includes config)
  val libs = to_js_string_array (libs config)
  val lintSeverities = severity_lintSettings_to_js (lint_severities config)
  val strictMode = strict_mode_settings_to_js (strict_mode config)
  val options = opts_to_js config
end)

let warnings_to_js warnings = 
  let warning_to_js (n, s) = Js.array (Array.of_list [
    Js.Unsafe.inject n; 
    Js.Unsafe.inject (Js.string s)
  ]) in
  let items = array_of_list warning_to_js warnings in
  Js.array items

let parse contents =
  let contents = Js.to_string contents in
  let config = FlowConfig.empty_config in
  let lines = contents
    |> Sys_utils.split_lines
    |> List.mapi (fun i line -> (i+1, String.trim line))
    |> List.filter FlowConfig.is_not_comment
  in
  match FlowConfig.parse config lines with
    | Error (line, msg) -> failwith
      @@ "parse error " ^ string_of_int line ^ ": " ^ msg
    | Ok (config, warnings) -> object%js
      val config = config_to_js config
      val warnings = warnings_to_js warnings
    end

let () = Js.export
  "parse" (Js.wrap_callback parse)
