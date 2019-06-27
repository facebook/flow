(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(* infer phase services *)

module ImpExp = Import_export

(**********)
(* Driver *)
(**********)

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  try
    statements |> Statement.toplevel_decls cx;
    statements |> Statement.toplevels cx
  with
  | Abnormal.Exn (Abnormal.Stmts stmts, Abnormal.Throw) ->
    (* throw is allowed as a top-level statement *)
    stmts
  | Abnormal.Exn (Abnormal.Stmts stmts, _) ->
    (* should never happen *)
    let loc = Loc.({ none with source = Some (Context.file cx) }) |> ALoc.of_loc in
    Flow_js.add_output cx Error_message.(EInternal (loc, AbnormalControlFlow));
    stmts
  | Abnormal.Exn _ ->
    failwith "Flow bug: Statement.toplevels threw with non-stmts payload"
  | exc ->
    raise exc

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
              {line = pos.line + 1; column = 0; }, 2
            else {line = pos.line + 1; column = 0; }, 1
          | '\n' -> {line = pos.line + 1; column = 0; }, 1
          | _ -> {pos with column = pos.column + 1; }, 1
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
    Error_message.ELintSetting (loc, kind) |> Flow_js.add_output cx
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
          Some (Core_list.map ~f:(fun kind -> ({value = kind; loc = arg.loc}, setting)) kinds)
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
    |> Core_list.map ~f:(fun rule -> get_kind_setting cx rule |> Option.value ~default:[])
  in

  (* Doesn't preserve offset, but is only used in locations where offset isn't used,
   * so that's fine. *)
  let get_range =
    let open Loc in

    let range_of_line source line =
      let start = {line; column = 0;} in
      let _end = {line = line + 1; column = 0;} in
      {source; start; _end}
    in

    let range_unending loc =
      let new_end = {line = max_int / 2; column = max_int / 2;}
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
        column = loc.start.column + 2} in
      let new_end = {loc._end with
        column = loc._end.column - 2} in
      let new_loc = {loc with start = new_start; _end = new_end} in
      {loc = new_loc; value = s}
    | Ast.Comment.Line s ->
      let new_start = {loc.start with
        column = loc.start.column + 2} in
      let new_loc = {loc with start = new_start} in
      {loc = new_loc; value = s}
  in

  let nested_map f outer_list =
    Core_list.map ~f:(Core_list.map ~f:f) outer_list
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
              | Some loc -> Loc_collections.LocSet.add loc loc_set
              | None -> loc_set)
            new_running_settings Loc_collections.LocSet.empty
          in
          let arg_locs = List.map
            (function
              | (_,(_,loc))::_ -> Some loc
              | [] -> None)
            settings_list
          in
          List.iter (function
            | Some arg_loc ->
              if not (Loc_collections.LocSet.mem arg_loc used_locs) then begin
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
                | (_, (Severity.Off, loc))::_ -> Loc_collections.LocSet.add loc suppression_locs
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
      (process_comment cx) (severity_cover_builder, base_settings, Loc_collections.LocSet.empty) comments
    in
    let severity_cover = ExactCover.bake severity_cover_builder in
    Context.add_severity_cover cx (Context.file cx) severity_cover;
    Context.add_lint_suppressions cx suppression_locs

let scan_for_suppressions cx lint_severities file_options comments =
  let filename = File_key.to_string (Context.file cx) in
  let declaration = match file_options with
  | Some file_options -> Files.is_declaration file_options filename
  | None -> false
  in
  if declaration then
    (* Declaration mode.
     * We don't report any warnings or errors. *)
    Context.remove_all_errors cx
  else
    (* Scan comments for line suppressions. *)
    scan_for_error_suppressions cx comments;
    scan_for_lint_suppressions cx lint_severities comments
    ;
  ()

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
  fun cx (file_sig: File_sig.With_ALoc.t) ->
    let open File_sig.With_ALoc in
    SMap.iter (fun mref locs ->
      let desc = Reason.RCustom mref in
      Nel.iter (add cx desc) locs
    ) (require_loc_map file_sig.module_sig);
    SMap.iter (fun _ (_, module_sig) ->
      SMap.iter (fun mref locs ->
        let m_name = Reason.internal_module_name mref in
        let desc = Reason.RCustom mref in
        Nel.iter (add_decl cx m_name desc) locs
      ) (require_loc_map module_sig)
    ) file_sig.declare_modules

(* build module graph *)
(* Lint suppressions are handled iff lint_severities is Some. *)
let infer_ast ~lint_severities ~file_options ~file_sig cx filename comments aloc_ast =
  assert (Context.is_checked cx);

  Flow_js.Cache.clear();

  let prog_aloc, aloc_statements, aloc_comments = aloc_ast in

  add_require_tvars cx file_sig;

  let module_ref = Context.module_ref cx in

  begin
    try Context.set_use_def cx @@ Ssa_builder.With_ALoc.program_with_scope aloc_ast
    with _ -> ()
  end;

  let reason_exports_module =
    let desc = Reason.RModule module_ref in
    Loc.({ none with source = Some (Context.file cx) })
    |> ALoc.of_loc
    |> Reason.mk_reason desc
  in

  let local_exports_var = Tvar.mk cx reason_exports_module in

  let module_scope = Scope.(
    let scope = fresh ~var_scope_kind:Module () in

    add_entry "exports"
      (Entry.new_var ~loc:(Type.loc_of_t local_exports_var) local_exports_var)
      scope;

    add_entry (Reason.internal_name "exports")
      (Entry.new_var
        ~loc:(Reason.aloc_of_reason reason_exports_module)
        ~specific:(Type.DefT (
          Reason.replace_reason_const
            (Reason.RCustom "undefined exports")
            reason_exports_module,
          Type.bogus_trust (),
          Type.EmptyT Type.Bottom))
        (Type.Unsoundness.exports_any reason_exports_module))
      scope;

    scope
  ) in

  Env.init_env cx module_scope;

  let file_loc = Loc.({ none with source = Some filename }) |> ALoc.of_loc in
  let reason = Reason.mk_reason (Reason.RCustom "exports") file_loc in

  let init_exports = Obj_type.mk cx reason in
  ImpExp.set_module_exports cx file_loc init_exports;

  (* infer *)
  Flow_js.flow_t cx (init_exports, local_exports_var);
  let typed_statements = infer_core cx aloc_statements in

  scan_for_suppressions cx lint_severities file_options comments;

  let module_t = Import_export.mk_module_t cx reason in
  Context.add_module cx module_ref module_t;

  prog_aloc, typed_statements, aloc_comments


(* Because libdef parsing is overly permissive, a libdef file might include an
   unexpected top-level statement like `export type` which mutates the module
   map and overwrites the builtins object.

   Since all libdefs share a sig_cx, this mutation will cause problems in later
   lib files if not unwound.

   Until we can restrict libdef parsing to forbid unexpected behaviors like
   this, we need this wrapper to preserve the existing behavior. However, none
   of this should be necessary.
*)
let with_libdef_builtins cx f =
  (* Store the original builtins and replace with a fresh tvar. *)
  let orig_builtins = Flow_js.builtins cx in
  Flow_js.mk_builtins cx;

  (* This function call might replace the builtins we just installed. *)
  f ();

  (* Connect the original builtins to the one we just calculated. *)
  let () =
    let builtins = Context.find_module cx Files.lib_module_ref in
    Flow_js.flow_t cx (orig_builtins, builtins)
  in

  (* Restore the original builtins tvar for the next file. *)
  Context.add_module cx Files.lib_module_ref orig_builtins

(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
 *)
let infer_lib_file ~exclude_syms ~lint_severities ~file_options ~file_sig cx ast =
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let _, _, comments = ast in
  let _, aloc_statements, _ = aloc_ast in
  Flow_js.Cache.clear();

  let () =
    (* TODO: Wait a minute, why do we bother with requires for lib files? Pretty
       confident that we don't support them in any sensible way. *)
    add_require_tvars cx file_sig
  in

  let module_scope = Scope.fresh () in
  Env.init_env ~exclude_syms cx module_scope;

  with_libdef_builtins cx (fun () ->
    ignore (infer_core cx aloc_statements : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t list);
    scan_for_suppressions cx lint_severities file_options comments;
  );

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  SMap.keys Scope.(module_scope.entries)
