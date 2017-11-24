(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* infer phase services *)

module FlowError = Flow_error
module ImpExp = Import_export
module Utils = Utils_js

(**********)
(* Driver *)
(**********)

let force_annotations cx =
  let m = Context.module_ref cx in
  let tvar = Flow_js.lookup_module cx m in
  let _, id = Type.open_tvar tvar in
  Flow_js.enforce_strict cx id

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

type range_keyword =
  | Unending (* Comment lasting until negated *)
  | Line (* covers current line *)
  | Next_line (* covers next line *)

let scan_for_lint_suppressions =
  let ignore_chars = " \t\n\r*" in

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


  (* Given a string like `"flowlint-line foo:bar"`, returns `Some (Line, Some "foo:bar")` *)
  let parse_keyword : string located -> (range_keyword located * string located option) option =
    let keywords = [
      "flowlint-line", Line;
      "flowlint-next-line", Next_line;
      "flowlint", Unending;
    ] in

    (* [prefix_length prefix str] returns the position of the first non-whitespace character in
       [str] after [prefix]. If [str] does not start with [prefix], or [prefix] is not followed by
       whitespace, returns [None]. *)
    let prefix_length prefix str =
      let sl = String.length prefix in
      if not (String_utils.string_starts_with str prefix) then None
      else if String.length str = sl then Some sl
      else match String_utils.index_not_from_opt str sl ignore_chars with
        | Some i when i = sl -> None
        | Some i -> Some i
        | None -> None
    in

    let rec try_keyword comment = function
    | [] -> None
    | (prefix, range)::todo ->
      let { loc; value } = comment in
      let value_len = String.length value in
      begin match prefix_length prefix value with
      | Some i when i = value_len ->
          Some ({ loc; value = range }, None)
      | Some i ->
          let range_end = update_pos loc.Loc.start prefix in
          let args_start = update_pos loc.Loc.start (String.sub value 0 i) in
          let range = {
            value = range;
            loc = { loc with Loc._end = range_end };
          } in
          let args = {
            value = String.sub value i (String.length value - i);
            loc = { loc with Loc.start = args_start }
          } in
          Some (range, Some args)
      | None -> try_keyword comment todo
      end
    in

    fun comment -> try_keyword comment keywords
  in

  (* Trims whitespace and stars from the front and end of loc_str. *)
  let trim_and_stars_locational { value; loc } =
    let open Loc in
    let start_offset = String_utils.index_not_opt value ignore_chars in
    let end_offset = String_utils.rindex_not_opt value ignore_chars in
    let start = match start_offset with
      | Some offset -> update_pos loc.start (String.sub value 0 offset)
      | None -> loc.start
    in
    let value = match start_offset, end_offset with
      | Some i, Some j -> String.sub value i (j - i + 1)
      | Some i, None -> String.sub value i (String.length value - i)
      | None, Some j -> String.sub value 0 (j + 1)
      | None, None -> value
    in
    let _end = update_pos start value in
    let loc = { loc with start; _end } in
    { value; loc }
  in

  let split_delim_locational delim { loc; value } =
    let delim_str = String.make 1 delim in
    let source = loc.Loc.source in
    let parts = String_utils.split_on_char delim value in
    let parts, _ = List.fold_left (fun (parts, start) value ->
      let _end = update_pos start value in
      let next_start = update_pos _end delim_str in
      ({loc = {Loc.source; start; _end}; value}::parts, next_start)
    ) ([], loc.Loc.start) parts in
    List.rev parts
  in

  let add_error cx (loc, kind) =
    let err = FlowError.ELintSetting (loc, kind) in
    FlowError.error_of_msg ~trace_reasons:[] ~op:None ~source_file:(Context.file cx) err
    |> Context.add_error cx
  in

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
    match split_delim_locational ':' arg with
    | [rule; setting] ->
      let rule = trim_and_stars_locational rule in
      let setting = trim_and_stars_locational setting in
      begin match parse_kind rule, parse_value setting with
        | Ok kinds, Ok setting ->
          Some (List.map (fun kind -> ({value = kind; loc = arg.loc}, setting)) kinds)
        | rule_result, setting_result ->
          Core_result.iter_error rule_result ~f:(add_error cx);
          Core_result.iter_error setting_result ~f:(add_error cx);
          None
      end
    | _ ->
      add_error cx (arg.loc, LintSettings.Malformed_argument);
      None
  in

  (* parse arguments of the form lint1:setting1,lint2:setting2... *)
  let get_settings_list cx args =
    split_delim_locational ',' args
    |> List.map (fun rule -> get_kind_setting cx rule |> Option.value ~default:[])
  in

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
      match keyword with
      | Unending -> range_unending loc
      | Line -> range_of_line loc.source loc._end.line
      | Next_line -> range_of_line loc.source (loc._end.line + 1)
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
    match parse_keyword loc_comment with
    | Some (keyword, Some args) ->
        (* Case where we're changing certain lint settings *)
        let settings_list =
          get_settings_list cx args
            |> nested_map (fun ({loc; value = kind}, state) -> (kind, (state, loc)))
        in
        let error_encountered = ref false in
        let (new_builder, new_running_settings) =
          let covered_range = get_range keyword in
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
        begin match keyword.value with
        | Line
        | Next_line ->
          (new_builder, running_settings, suppression_locs)
        | Unending ->
          (new_builder, new_running_settings, suppression_locs)
        end
    | Some (keyword, None) ->
        (* Case where we're wholly enabling/disabling linting *)
        add_error cx (keyword.loc, LintSettings.Naked_comment);
        acc (* TODO (rballard): regional lint disabling *)
    | None -> acc
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
  scan_for_lint_suppressions cx base_settings comments

let add_require_tvars =
  let add cx desc loc =
    let reason = Reason.mk_reason desc loc in
    let t = Tvar.mk cx reason in
    Context.add_require cx loc t
  in
  let add_decl cx m_name desc loc =
    (* TODO: Imports within `declare module`s can only reference other `declare
       module`s (for now). This won't fly forever so at some point we'll need to
       move `declare module` storage into the modulemap just like normal modules
       and merge them as such. *)
    let reason = Reason.mk_reason desc loc in
    let t = Flow_js.get_builtin cx m_name reason in
    Context.add_require cx loc t
  in
  fun cx file_sig ->
    let open File_sig in
    SMap.iter (fun mref req ->
      let desc = Reason.RCustom mref in
      List.iter (add cx desc) req.cjs_requires;
      List.iter (add cx desc) req.es_imports;
    ) file_sig.module_sig.requires;
    SMap.iter (fun _ (_, module_sig) ->
      SMap.iter (fun mref req ->
        let m_name = Reason.internal_module_name mref in
        let desc = Reason.RCustom mref in
        List.iter (add_decl cx m_name desc) req.cjs_requires;
        List.iter (add_decl cx m_name desc) req.es_imports;
      ) module_sig.requires;
    ) file_sig.declare_modules

(* build module graph *)
(* Lint suppressions are handled iff lint_severities is Some. *)
let infer_ast ~lint_severities ~file_sig cx filename ast =
  Flow_js.Cache.clear();

  let _, statements, comments = ast in

  add_require_tvars cx file_sig;

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

  let local_exports_var = Tvar.mk cx reason_exports_module in

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

  let initial_module_t = ImpExp.module_t_of_cx cx in
  if checked then (
    let init_exports = Obj_type.mk cx reason in
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
  force_annotations cx;

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
    add_require_tvars cx file_sig
  in

  let module_scope = Scope.fresh () in
  Env.init_env ~exclude_syms cx module_scope;

  infer_core cx statements;
  scan_for_suppressions cx lint_severities comments;

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  cx, SMap.keys Scope.(module_scope.entries)
