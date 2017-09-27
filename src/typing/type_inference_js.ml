(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* infer phase services *)

module Flow = Flow_js
module FlowError = Flow_error
module ImpExp = Import_export
module Utils = Utils_js

(**********)
(* Driver *)
(**********)

let force_annotations cx require_loc_map =
  let m = Context.module_ref cx in
  let tvar = Flow_js.lookup_module cx m in
  let _, id = Type.open_tvar tvar in
  Flow_js.enforce_strict cx id (SMap.keys require_loc_map)

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  try
    statements |> Statement.toplevel_decls cx;
    statements |> Statement.toplevels cx;
  with
  | Abnormal.Exn Abnormal.Throw ->
    (* throw is allowed as a top-level statement *)
    ()
  | Abnormal.Exn _ ->
    (* should never happen *)
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    Flow_js.add_output cx FlowError.(EInternal (loc, AbnormalControlFlow))
  | exc ->
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    Flow_js.add_output cx FlowError.(EInternal (loc, UncaughtException exc))

(* There's a .flowconfig option to specify suppress_comments regexes. Any
 * comments that match those regexes will suppress any errors on the next line
 *)
let scan_for_error_suppressions =
  let should_suppress suppress_comments comment =
    List.exists (fun r -> Str.string_match r comment 0) suppress_comments

  in fun cx comments ->
    let suppress_comments = Context.suppress_comments cx in
    let should_suppress = should_suppress suppress_comments in

    (* Bail immediately if we're not using error suppressing comments *)
    if suppress_comments <> []
    then List.iter (function
      | loc, Ast.Comment.Block comment
      | loc, Ast.Comment.Line comment when should_suppress comment ->
          Context.add_error_suppression cx loc
      | _ -> ()) comments

type 'a located = {
  value: 'a;
  loc: Loc.t;
}

let scan_for_lint_suppressions =
  let flowlint_keywords =
    ["flowlint";
    "flowlint-line";
    "flowlint-next-line";]
  in

  let ws_and_stars_regex = Str.regexp "[ \t\n\r\\*]+" in

  let starts_with_keyword comment =
    match Str.split ws_and_stars_regex comment with
    | head::_ -> List.exists ((=) head) flowlint_keywords
    | [] -> false
  in

  (* Get the position induced by reading the string str from the starting position pos *)
  let update_pos =
    (* Get the position induced by reading [the substring of str from index
     * onwards] from the starting position pos *)
    let rec update_pos' pos str index length =
      let open Loc in
      if index < length then
        let new_loc, ind_diff =
          match str.[index] with
          | '\r' ->
            if index + 1 < length && str.[index + 1] = '\n' then
              {line = pos.line + 1; column = 0; offset = pos.offset + 2}, 2
            else {line = pos.line + 1; column = 0; offset = pos.offset + 1}, 1
          | '\n' -> {line = pos.line + 1; column = 0; offset = pos.offset + 1}, 1
          | _ -> {pos with column = pos.column + 1; offset = pos.offset + 1}, 1
        in update_pos' new_loc str (index + ind_diff) length
      else
        pos
    in fun pos str ->
      update_pos' pos str 0 (String.length str)
  in

  (* Trims whitespace and stars from the front and end of loc_str. *)
  let trim_and_stars_locational =
      let open Loc in

      let rec load_buffer buffer = function
        | [] | [Str.Delim _] -> ()
        | (Str.Text head | Str.Delim head)::tail ->
          Buffer.add_string buffer head;
          load_buffer buffer tail
      in

      fun loc_str ->
        let split_str = Str.full_split ws_and_stars_regex loc_str.value in
        let prefix, split_str = match split_str with
          | (Str.Delim prefix)::tail -> prefix, tail
          | _ -> "", split_str
        in
        let buffer = loc_str.value |> String.length |> Buffer.create in
        let () = load_buffer buffer split_str in
        let trimmed_str = Buffer.contents buffer in

        let orig_loc = loc_str.loc in
        let new_start = update_pos orig_loc.start prefix in
        let new_end = update_pos new_start trimmed_str in
        let new_loc = {orig_loc with start = new_start; _end = new_end} in

        {value = trimmed_str; loc = new_loc}
  in

  let convert_split_results =
    let open Loc in

    let rec convert' ((loc_strings, (source, current_pos)) as acc) =
      let open Str in function
        (* Text to be added to the result *)
        | (Text text)::tail ->
          let next_pos = update_pos current_pos text in
          let loc = {source; start = current_pos; _end = next_pos} in
          convert' ({loc; value = text}::loc_strings, (source, next_pos)) tail
        (* Delim that can be skipped over *)
        | (Delim delim)::((Text _)::_ as tail) ->
          convert' (loc_strings, (source, update_pos current_pos delim)) tail
        (* When we have adjacent Delims, insert an empy string between them *)
        | (Delim _ as head)::((Delim _)::_ as tail) ->
          convert' acc (head::Text ""::tail)
        (* When the result ends with a Delim, append an empty string *)
        | [Delim _ as ending] -> convert' acc [ending; Text ""]
        (* Return the results when we get to the end of the list *)
        | [] -> acc |> fst |> List.rev
    in

    fun source start_pos split_results ->
      let split_results = match split_results with
        | (Str.Delim _)::_ -> (Str.Text "")::split_results
        | _ -> split_results
      in
      convert' ([], (source, start_pos)) split_results
  in

  let split_delim_locational regex loc_str =
    let open Loc in
    Str.full_split regex loc_str.value
    |> convert_split_results loc_str.loc.source loc_str.loc.start
  in

  let bounded_split_delim_locational regex loc_str max_substrs =
    let open Loc in
    Str.bounded_full_split regex loc_str.value max_substrs
    |> convert_split_results loc_str.loc.source loc_str.loc.start
  in

  let split_comment comment =
    match bounded_split_delim_locational ws_and_stars_regex comment 2 with
    | [keyword; args] -> (keyword, Some args)
    | [keyword] -> (keyword, None)
    | _ -> Utils.assert_false
      "Unreachable match case. (split_comment is only called when comment starts with a keyword)"
  in

  let add_error cx (loc, kind) =
    let err = FlowError.ELintSetting (loc, kind) in
    FlowError.error_of_msg ~trace_reasons:[] ~op:None ~source_file:(Context.file cx) err
    |> Context.add_error cx
  in

  let comma_regex = Str.regexp "," in
  let colon_regex = Str.regexp ":" in

  let parse_kind loc_str =
    match Lints.kinds_of_string loc_str.value with
    | Some kinds -> Ok kinds
    | None -> Error (loc_str.loc, LintSettings.Nonexistent_rule)
  in

  let parse_value loc_value =
    match Severity.severity_of_string loc_value.value with
      | Some state -> Ok state
      | None -> Error (loc_value.loc, LintSettings.Invalid_setting)
  in

  let get_kind_setting cx arg =
    let arg = trim_and_stars_locational arg in
    match split_delim_locational colon_regex arg with
    | [rule; setting] ->
      let rule = trim_and_stars_locational rule in
      let setting = trim_and_stars_locational setting in
      begin match parse_kind rule, parse_value setting with
        | Ok kinds, Ok setting ->
          Some (List.map (fun kind -> ({value = kind; loc = arg.loc}, setting)) kinds)
        | rule_result, setting_result ->
          Result.iter_error rule_result ~f:(add_error cx);
          Result.iter_error setting_result ~f:(add_error cx);
          None
      end
    | _ ->
      add_error cx (arg.loc, LintSettings.Malformed_argument);
      None
  in

  (* parse arguments of the form lint1:setting1,lint2:setting2... *)
  let get_settings_list cx args =
    split_delim_locational comma_regex args
    |> List.map (fun rule -> get_kind_setting cx rule |> Option.value ~default:[])
  in

  let contains_r regex str start_index =
    try Str.search_forward regex str start_index |> ignore; true
    with Not_found -> false
  in

  let line_regex = Str.regexp "line" in
  let next_regex = Str.regexp "next" in

  (* Doesn't preserve offset, but is only used in locations where offset isn't used,
   * so that's fine. *)
  let get_range =
    let open Loc in

    let range_of_line source line =
      let start = {line; column = 0; offset = 0} in
      let _end = {line = line + 1; column = 0; offset = 0} in
      {source; start; _end}
    in

    let range_unending loc =
      let new_end = {line = max_int / 2; column = max_int / 2; offset = max_int / 2}
      in {loc with _end = new_end}
    in

    fun {loc; value = keyword} ->
      if contains_r line_regex keyword 0 then
        if contains_r next_regex keyword 0 then (* covers next line *)
          range_of_line loc.source (loc._end.line + 1)
        else (* covers current line *)
          range_of_line loc.source loc._end.line
      else (* Comment lasting until negated *)
        range_unending loc
  in

  let convert_comment (loc, comment) =
    (* Comment locs contain the comment characters themselves. (//, /*, and */)
     * Trim the locs to line up with the contents of the comment. *)
    let open Loc in
    match comment with
    | Ast.Comment.Block s ->
      let new_start = {loc.start with
        column = loc.start.column + 2;
        offset = loc.start.offset + 2} in
      let new_end = {loc._end with
        column = loc._end.column - 2;
        offset = loc._end.offset - 2} in
      let new_loc = {loc with start = new_start; _end = new_end} in
      {loc = new_loc; value = s}
    | Ast.Comment.Line s ->
      let new_start = {loc.start with
        column = loc.start.column + 2;
        offset = loc.start.offset + 2} in
      let new_loc = {loc with start = new_start} in
      {loc = new_loc; value = s}
  in

  let nested_map f outer_list =
    List.map (List.map f) outer_list
  in

  let process_comment
      cx
      ((severity_cover_builder, running_settings, suppression_locs) as acc)
      comment =
    let loc_comment = comment |> convert_comment |> trim_and_stars_locational in
    if starts_with_keyword loc_comment.value then
      let keyword, args = split_comment loc_comment in
      let covered_range = get_range keyword in
      match args with
        (* Case where we're changing certain lint settings *)
        | Some args ->
          let settings_list =
            get_settings_list cx args
              |> nested_map (fun ({loc; value = kind}, state) -> (kind, (state, loc)))
          in
          let error_encountered = ref false in
          let (new_builder, new_running_settings) =
            ExactCover.update_settings_and_running running_settings
              (fun err -> error_encountered := true; add_error cx err)
              covered_range settings_list severity_cover_builder in
          (* Only report overwritten arguments if there are no no-op arguments,
           * to avoid error duplication *)
          let () = if not !error_encountered then
            (* Check for overwritten arguments *)
            let used_locs = LintSettings.fold
              (fun _ (_, loc) loc_set -> match loc with
                | Some loc -> Loc.LocSet.add loc loc_set
                | None -> loc_set)
              new_running_settings Loc.LocSet.empty
            in
            let arg_locs = List.map
              (function
                | (_,(_,loc))::_ -> Some loc
                | [] -> None)
              settings_list
            in
            List.iter (function
              | Some arg_loc ->
                if not (Loc.LocSet.mem arg_loc used_locs) then begin
                  error_encountered := true;
                  add_error cx (arg_loc, LintSettings.Overwritten_argument)
                end
              | None -> ()) arg_locs
          in
          let suppression_locs =
            (* Only report unused suppressions if there are no redundant settings,
             * to avoid error duplication. (The suppression_locs are later used to detect
             * unused suppressions; by never storing their locations we are effectively
             * immediately using them.) *)
            if not !error_encountered then
              List.fold_left (
                fun suppression_locs -> function
                  | (_, (Severity.Off, loc))::_ -> Loc.LocSet.add loc suppression_locs
                  | _ -> suppression_locs
                ) suppression_locs settings_list
            else suppression_locs
          in
          if contains_r line_regex keyword.value 0
            then (new_builder, running_settings, suppression_locs)
            else (new_builder, new_running_settings, suppression_locs)
        (* Case where we're wholly enabling/disabling linting *)
        | None ->
          add_error cx (keyword.loc, LintSettings.Naked_comment);
          acc (* TODO (rballard): regional lint disabling *)
    else acc
  in

  fun cx base_settings comments ->
    let severity_cover_builder = ExactCover.new_builder (Context.file cx) base_settings in
    let severity_cover_builder, _, suppression_locs = List.fold_left
      (process_comment cx) (severity_cover_builder, base_settings, Loc.LocSet.empty) comments
    in
    let severity_cover = ExactCover.bake severity_cover_builder in
    Context.set_severity_cover cx severity_cover;
    Context.set_unused_lint_suppressions cx suppression_locs

let scan_for_suppressions cx base_settings comments =
  scan_for_error_suppressions cx comments;
  match base_settings with
  | Some base_settings ->
    scan_for_lint_suppressions cx base_settings comments
  | None -> ()
  ;
  ()

(* build module graph *)
(* Lint suppressions are handled iff lint_severities is Some. *)
let infer_ast ~lint_severities ~file_sig cx filename ast =
  Flow_js.Cache.clear();

  let _, statements, comments = ast in

  let module_ref = Context.module_ref cx in

  let dep_mapper = new Dep_mapper.mapper in
  let _ = dep_mapper#program ast in
  let _ = Context.set_dep_map cx dep_mapper#dep_map in
  let _ = Context.set_use_def_map cx dep_mapper#use_def_map in

  let checked = Context.is_checked cx in

  let reason_exports_module =
    let desc = Reason.RCustom (
      Utils.spf "exports of file `%s`" module_ref
    ) in
    Reason.locationless_reason desc
  in

  let local_exports_var = Flow_js.mk_tvar cx reason_exports_module in

  let module_scope = Scope.(
    let scope = fresh ~var_scope_kind:Module () in

    add_entry "exports"
      (Entry.new_var ~loc:(Type.loc_of_t local_exports_var) local_exports_var)
      scope;

    add_entry (Reason.internal_name "exports")
      (Entry.new_var
        ~loc:(Reason.loc_of_reason reason_exports_module)
        ~specific:(Type.DefT (
          Reason.replace_reason_const
            (Reason.RCustom "undefined exports")
            reason_exports_module,
          Type.EmptyT))
        (Type.DefT (reason_exports_module, Type.AnyT)))
      scope;

    scope
  ) in

  Env.init_env cx module_scope;

  let file_loc = Loc.({ none with source = Some filename }) in
  let reason = Reason.mk_reason (Reason.RCustom "exports") file_loc in

  let require_loc_map = File_sig.(require_loc_map file_sig.module_sig) in

  let initial_module_t = ImpExp.module_t_of_cx cx in
  if checked then (
    SMap.iter (Import_export.add_require_tvar cx) require_loc_map;

    let init_exports = Flow.mk_object cx reason in
    ImpExp.set_module_exports cx file_loc init_exports;

    (* infer *)
    Flow_js.flow_t cx (init_exports, local_exports_var);
    infer_core cx statements;

    scan_for_suppressions cx lint_severities comments;

    let module_t = Context.(
      match Context.module_kind cx with
      (* CommonJS with a clobbered module.exports *)
      | CommonJSModule(Some(loc)) ->
        let module_exports_t = ImpExp.get_module_exports cx file_loc in
        let reason = Reason.mk_reason (Reason.RCustom "exports") loc in
        ImpExp.mk_commonjs_module_t cx reason_exports_module
          reason module_exports_t

      (* CommonJS with a mutated 'exports' object *)
      | CommonJSModule(None) ->
        ImpExp.mk_commonjs_module_t cx reason_exports_module
          reason local_exports_var

      (* Uses standard ES module exports *)
      | ESModule -> ImpExp.mk_module_t cx reason_exports_module
    ) in
    Flow_js.flow_t cx (module_t, initial_module_t)
  ) else (
    Flow_js.unify cx initial_module_t Type.Locationless.AnyT.t
  );

  (* insist that whatever type flows into exports is fully annotated *)
  force_annotations cx require_loc_map;

  ()


(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
 *)
let infer_lib_file ~metadata ~exclude_syms ~lint_severities file ast =
  let _, statements, comments = ast in
  Flow_js.Cache.clear();

  let cx = Flow_js.fresh_context metadata file Files.lib_module_ref in

  let () =
    (* TODO: Wait a minute, why do we bother with requires for lib files? Pretty
       confident that we don't support them in any sensible way. *)
    let open File_sig in
    let file_sig = program ~ast in
    let require_loc_map = require_loc_map file_sig.module_sig in
    SMap.iter (Import_export.add_require_tvar cx) require_loc_map
  in

  let module_scope = Scope.fresh () in
  Env.init_env ~exclude_syms cx module_scope;

  infer_core cx statements;
  scan_for_suppressions cx lint_severities comments;

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  cx, SMap.keys Scope.(module_scope.entries)
