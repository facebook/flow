(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
open Utils_js
module Ast = Flow_ast

(* infer phase services *)

module NameResolver = Name_resolver.Make_of_flow (Context) (Flow_js_utils)
module NameDefOrdering = Name_def_ordering.Make (Context) (Flow_js_utils)

let add_require_tvars =
  let add cx desc loc =
    let reason = Reason.mk_reason desc loc in
    let id = Tvar.mk_no_wrap cx reason in
    Context.add_require cx loc (reason, id)
  in
  let add_decl cx m_name desc loc =
    (* TODO: Imports within `declare module`s can only reference other `declare
        module`s (for now). This won't fly forever so at some point we'll need to
        move `declare module` storage into the modulemap just like normal modules
        and merge them as such. *)
    let reason = Reason.mk_reason desc loc in
    let tvar = Flow_js.get_builtin_tvar cx m_name reason in
    Context.add_require cx loc (reason, tvar)
  in
  fun cx (file_sig : File_sig.With_ALoc.t) ->
    File_sig.With_ALoc.(
      SMap.iter
        (fun mref locs ->
          let desc = Reason.RCustom mref in
          Nel.iter (add cx desc) locs)
        (require_loc_map file_sig.module_sig);
      SMap.iter
        (fun _ (_, module_sig) ->
          SMap.iter
            (fun mref locs ->
              let m_name = Reason.internal_module_name mref in
              let desc = Reason.RCustom mref in
              Nel.iter (add_decl cx m_name desc) locs)
            (require_loc_map module_sig))
        file_sig.declare_modules
    )

(* Scan the list of comments to place suppressions on the appropriate locations.
    Because each comment can only contain a single code, in order to support
    suppressing multiple types of errors on one location we allow you to stack
    comments like so:
    //$FlowFixMe[x]
    //$FlowFixMe[y]
     some code causing errors x and y

   This logic produces a set of error codes associated with the location of the
   bottom suppression in the stack *)

let scan_for_error_suppressions cx =
  let open Suppression_comments in
  (* If multiple comments are stacked together, we join them into a codeset positioned on the
     location of the last comment *)
  Base.List.fold_left
    ~f:
      (fun acc -> function
        | (({ Loc.start = { Loc.line; _ }; _ } as loc), { Ast.Comment.text; _ }) -> begin
          match (should_suppress text loc, acc) with
          | (Ok (Some (Specific _ as codes)), Some (prev_loc, (Specific _ as prev_codes)))
            when line = prev_loc.Loc._end.Loc.line + 1 ->
            Some ({ prev_loc with Loc._end = loc.Loc._end }, join_applicable_codes codes prev_codes)
          | (Ok codes, _) ->
            Base.Option.iter ~f:(Context.add_error_suppression cx |> uncurry) acc;
            Base.Option.map codes ~f:(mk_tuple loc)
          | (Error (), _) ->
            Flow_js.add_output cx Error_message.(EMalformedCode (ALoc.of_loc loc));
            Base.Option.iter ~f:(Context.add_error_suppression cx |> uncurry) acc;
            None
        end)
    ~init:None
  %> Base.Option.iter ~f:(Context.add_error_suppression cx |> uncurry)

type 'a located = {
  value: 'a;
  loc: Loc.t;
}

type range_keyword =
  | Unending (* Comment lasting until negated *)
  | Line (* covers current line *)
  | Next_line

(* covers next line *)

let scan_for_lint_suppressions =
  let ignore_chars = " \t\n\r*" in
  (* Get the position induced by reading the string str from the starting position pos *)
  let update_pos =
    (* Get the position induced by reading [the substring of str from index
     * onwards] from the starting position pos *)
    let rec update_pos' pos str index length =
      Loc.(
        if index < length then
          let (new_loc, ind_diff) =
            match str.[index] with
            | '\r' ->
              if index + 1 < length && str.[index + 1] = '\n' then
                ({ line = pos.line + 1; column = 0 }, 2)
              else
                ({ line = pos.line + 1; column = 0 }, 1)
            | '\n' -> ({ line = pos.line + 1; column = 0 }, 1)
            | _ -> ({ pos with column = pos.column + 1 }, 1)
          in
          update_pos' new_loc str (index + ind_diff) length
        else
          pos
      )
    in
    (fun pos str -> update_pos' pos str 0 (String.length str))
  in
  (* Given a string like `"flowlint-line foo:bar"`, returns `Some (Line, Some "foo:bar")` *)
  let parse_keyword : string located -> (range_keyword located * string located option) option =
    let keywords =
      [("flowlint-line", Line); ("flowlint-next-line", Next_line); ("flowlint", Unending)]
    in
    (* [prefix_length prefix str] returns the position of the first non-whitespace character in
       [str] after [prefix]. If [str] does not start with [prefix], or [prefix] is not followed by
       whitespace, returns [None]. *)
    let prefix_length prefix str =
      let sl = String.length prefix in
      if not (String.starts_with ~prefix str) then
        None
      else if String.length str = sl then
        Some sl
      else
        match String_utils.index_not_from_opt str sl ignore_chars with
        | Some i when i = sl -> None
        | Some i -> Some i
        | None -> None
    in
    let rec try_keyword comment = function
      | [] -> None
      | (prefix, range) :: todo ->
        let { loc; value } = comment in
        let value_len = String.length value in
        begin
          match prefix_length prefix value with
          | Some i when i = value_len -> Some ({ loc; value = range }, None)
          | Some i ->
            let range_end = update_pos loc.Loc.start prefix in
            let args_start = update_pos loc.Loc.start (String.sub value 0 i) in
            let range = { value = range; loc = { loc with Loc._end = range_end } } in
            let args =
              {
                value = String.sub value i (String.length value - i);
                loc = { loc with Loc.start = args_start };
              }
            in
            Some (range, Some args)
          | None -> try_keyword comment todo
        end
    in
    (fun comment -> try_keyword comment keywords)
  in
  (* Trims whitespace and stars from the front and end of loc_str. *)
  let trim_and_stars_locational { value; loc } =
    Loc.(
      let start_offset = String_utils.index_not_opt value ignore_chars in
      let end_offset = String_utils.rindex_not_opt value ignore_chars in
      let start =
        match start_offset with
        | Some offset -> update_pos loc.start (String.sub value 0 offset)
        | None -> loc.start
      in
      let value =
        match (start_offset, end_offset) with
        | (Some i, Some j) -> String.sub value i (j - i + 1)
        | (Some i, None) -> String.sub value i (String.length value - i)
        | (None, Some j) -> String.sub value 0 (j + 1)
        | (None, None) -> value
      in
      let _end = update_pos start value in
      let loc = { loc with start; _end } in
      { value; loc }
    )
  in
  let split_delim_locational delim { loc; value } =
    let delim_str = Base.String.of_char delim in
    let source = loc.Loc.source in
    let parts = String.split_on_char delim value in
    let (parts, _) =
      List.fold_left
        (fun (parts, start) value ->
          let _end = update_pos start value in
          let next_start = update_pos _end delim_str in
          ({ loc = { Loc.source; start; _end }; value } :: parts, next_start))
        ([], loc.Loc.start)
        parts
    in
    List.rev parts
  in
  let add_error cx (loc, kind) =
    Error_message.ELintSetting (ALoc.of_loc loc, kind) |> Flow_js.add_output cx
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
      begin
        match (parse_kind rule, parse_value setting) with
        | (Ok kinds, Ok setting) ->
          Some (Base.List.map ~f:(fun kind -> ({ value = kind; loc = arg.loc }, setting)) kinds)
        | (rule_result, setting_result) ->
          Base.Result.iter_error rule_result ~f:(add_error cx);
          Base.Result.iter_error setting_result ~f:(add_error cx);
          None
      end
    | _ ->
      add_error cx (arg.loc, LintSettings.Malformed_argument);
      None
  in
  (* parse arguments of the form lint1:setting1,lint2:setting2... *)
  let get_settings_list cx args =
    split_delim_locational ',' args
    |> Base.List.map ~f:(fun rule -> get_kind_setting cx rule |> Base.Option.value ~default:[])
  in
  (* Doesn't preserve offset, but is only used in locations where offset isn't used,
   * so that's fine. *)
  let get_range =
    Loc.(
      let range_of_line source line =
        let start = { line; column = 0 } in
        let _end = { line = line + 1; column = 0 } in
        { source; start; _end }
      in
      let range_unending loc =
        let new_end = { line = max_int / 2; column = max_int / 2 } in
        { loc with _end = new_end }
      in
      fun { loc; value = keyword } ->
        match keyword with
        | Unending -> range_unending loc
        | Line -> range_of_line loc.source loc._end.line
        | Next_line -> range_of_line loc.source (loc._end.line + 1)
    )
  in
  let convert_comment (loc, comment) =
    (* Comment locs contain the comment characters themselves. (//, /*, and */)
     * Trim the locs to line up with the contents of the comment. *)
    Loc.(
      match comment with
      | { Ast.Comment.kind = Ast.Comment.Block; text = s; _ } ->
        let new_start = { loc.start with column = loc.start.column + 2 } in
        let new_end = { loc._end with column = loc._end.column - 2 } in
        let new_loc = { loc with start = new_start; _end = new_end } in
        { loc = new_loc; value = s }
      | { Ast.Comment.kind = Ast.Comment.Line; text = s; _ } ->
        let new_start = { loc.start with column = loc.start.column + 2 } in
        let new_loc = { loc with start = new_start } in
        { loc = new_loc; value = s }
    )
  in
  let nested_map f outer_list = Base.List.map ~f:(Base.List.map ~f) outer_list in
  let process_comment
      cx ((severity_cover_builder, running_settings, suppression_locs) as acc) comment =
    let loc_comment = comment |> convert_comment |> trim_and_stars_locational in
    match parse_keyword loc_comment with
    | Some (keyword, Some args) ->
      (* Case where we're changing certain lint settings *)
      let settings_list =
        get_settings_list cx args
        |> nested_map (fun ({ loc; value = kind }, state) -> (kind, (state, loc)))
      in
      let error_encountered = ref false in
      let (new_builder, new_running_settings) =
        let covered_range = get_range keyword in
        ExactCover.update_settings_and_running
          running_settings
          (fun err ->
            error_encountered := true;
            add_error cx err)
          covered_range
          settings_list
          severity_cover_builder
      in
      (* Only report overwritten arguments if there are no no-op arguments,
       * to avoid error duplication *)
      let () =
        if not !error_encountered then
          (* Check for overwritten arguments *)
          let used_locs =
            LintSettings.fold
              (fun _ (_, loc) loc_set ->
                match loc with
                | Some loc -> Loc_collections.LocSet.add loc loc_set
                | None -> loc_set)
              new_running_settings
              Loc_collections.LocSet.empty
          in
          let arg_locs =
            Base.List.map
              ~f:(function
                | (_, (_, loc)) :: _ -> Some loc
                | [] -> None)
              settings_list
          in
          List.iter
            (function
              | Some arg_loc ->
                if not (Loc_collections.LocSet.mem arg_loc used_locs) then (
                  error_encountered := true;
                  add_error cx (arg_loc, LintSettings.Overwritten_argument)
                )
              | None -> ())
            arg_locs
      in
      let suppression_locs =
        (* Only report unused suppressions if there are no redundant settings,
         * to avoid error duplication. (The suppression_locs are later used to detect
         * unused suppressions; by never storing their locations we are effectively
         * immediately using them.) *)
        if not !error_encountered then
          List.fold_left
            (fun suppression_locs -> function
              | (_, (Severity.Off, loc)) :: _ -> Loc_collections.LocSet.add loc suppression_locs
              | _ -> suppression_locs)
            suppression_locs
            settings_list
        else
          suppression_locs
      in
      begin
        match keyword.value with
        | Line
        | Next_line ->
          (new_builder, running_settings, suppression_locs)
        | Unending -> (new_builder, new_running_settings, suppression_locs)
      end
    | Some (keyword, None) ->
      (* Case where we're wholly enabling/disabling linting *)
      add_error cx (keyword.loc, LintSettings.Naked_comment);
      acc (* TODO (rballard): regional lint disabling *)
    | None -> acc
  in
  fun cx base_settings comments ->
    let severity_cover_builder = ExactCover.new_builder (Context.file cx) base_settings in
    let (severity_cover_builder, _, suppression_locs) =
      List.fold_left
        (process_comment cx)
        (severity_cover_builder, base_settings, Loc_collections.LocSet.empty)
        comments
    in
    let severity_cover = ExactCover.bake severity_cover_builder in
    Context.add_severity_cover cx (Context.file cx) severity_cover;
    Context.add_lint_suppressions cx suppression_locs

let scan_for_suppressions cx lint_severities comments =
  let comments = List.sort (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2) comments in
  scan_for_error_suppressions cx comments;
  scan_for_lint_suppressions cx lint_severities comments

module Statement = Fix_statement.Statement_

(**********)
(* Driver *)
(**********)

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  Abnormal.try_with_abnormal_exn
    ~f:(fun () -> statements |> Toplevels.toplevels Statement.statement cx)
    ~on_abnormal_exn:(function
      | (Abnormal.Stmts stmts, Abnormal.Throw) ->
        (* throw is allowed as a top-level statement *)
        stmts
      | (Abnormal.Stmts stmts, _) ->
        (* should never happen *)
        let loc = Loc.{ none with source = Some (Context.file cx) } |> ALoc.of_loc in
        Flow_js.add_output cx Error_message.(EInternal (loc, AbnormalControlFlow));
        stmts
      | _ -> failwith "Flow bug: Statement.toplevels threw with non-stmts payload")
    ()

let initialize_env
    ~lib ?(exclude_syms = NameUtils.Set.empty) ?local_exports_var cx aloc_ast toplevel_scope_kind =
  let (_abrupt_completion, ({ Env_api.env_entries; env_values; providers; _ } as info)) =
    NameResolver.program_with_scope cx ~lib ~exclude_syms aloc_ast
  in
  let autocomplete_hooks =
    {
      Env_api.id_hook = Type_inference_hooks_js.dispatch_id_hook cx;
      literal_hook = Type_inference_hooks_js.dispatch_literal_hook cx;
      obj_prop_decl_hook = Type_inference_hooks_js.dispatch_obj_prop_decl_hook cx;
    }
  in
  let (name_def_graph, hint_map) =
    Name_def.find_defs ~autocomplete_hooks env_entries env_values providers aloc_ast
  in
  let hint_map = ALocMap.mapi (Env_resolution.lazily_resolve_hints cx) hint_map in
  let env = Loc_env.with_info Name_def.Global hint_map info in
  Context.set_environment cx env;
  let components = NameDefOrdering.build_ordering cx ~autocomplete_hooks info name_def_graph in
  if Context.cycle_errors cx then
    Base.List.iter ~f:(Cycles.handle_component cx name_def_graph) components;
  Env.init_env cx toplevel_scope_kind;
  let env = Context.environment cx in
  Base.Option.iter local_exports_var ~f:(fun local_exports_var ->
      let loc = TypeUtil.loc_of_t local_exports_var in
      let t =
        Base.Option.value_exn
          ~message:(ALoc.debug_to_string ~include_source:true loc)
          (Loc_env.find_write env Env_api.GlobalExportsLoc loc)
      in
      Flow_js.unify cx t local_exports_var
  );
  let { Loc_env.scope_kind; class_stack; _ } = Context.environment cx in
  Base.List.iter ~f:(Env_resolution.resolve_component cx name_def_graph) components;
  Debug_js.Verbose.print_if_verbose_lazy cx (lazy ["Finished all components"]);
  let env = Context.environment cx in
  Context.set_environment cx { env with Loc_env.scope_kind; class_stack }

(* build module graph *)
(* Lint suppressions are handled iff lint_severities is Some. *)
let infer_ast ~lint_severities cx filename comments aloc_ast =
  assert (Context.is_checked cx);

  let ( prog_aloc,
        {
          Ast.Program.statements = aloc_statements;
          comments = aloc_comments;
          all_comments = aloc_all_comments;
        }
      ) =
    aloc_ast
  in

  let file_loc = Loc.{ none with source = Some filename } |> ALoc.of_loc in
  let reason = Reason.mk_reason Reason.RExports file_loc in
  let dict =
    {
      Type.key = Type.StrT.make reason (Trust.bogus_trust ());
      value = Type.MixedT.make reason (Trust.bogus_trust ());
      dict_name = None;
      dict_polarity = Polarity.Negative;
    }
  in
  let init_exports = Obj_type.mk cx ~obj_kind:(Type.Indexed dict) reason in
  let reason_exports_module =
    Reason.mk_reason Reason.(RModule (OrdinaryName (File_key.to_string filename))) file_loc
  in
  let local_exports_var =
    Tvar.mk_no_wrap_where cx reason_exports_module (fun (_, id) ->
        Flow_js.resolve_id cx id init_exports
    )
  in

  try
    initialize_env ~lib:false ~local_exports_var cx aloc_ast Name_def.Module;

    (* infer *)
    let typed_statements = infer_core cx aloc_statements in
    scan_for_suppressions cx lint_severities comments;

    let program =
      ( prog_aloc,
        {
          Ast.Program.statements = typed_statements;
          comments = aloc_comments;
          all_comments = aloc_all_comments;
        }
      )
    in

    Exists_marker.mark cx program;
    program
  with
  | Env_api.Env_invariant (loc, inv) ->
    let loc = Base.Option.value ~default:prog_aloc loc in
    Flow_js.add_output cx Error_message.(EInternal (loc, EnvInvariant inv));
    ( prog_aloc,
      {
        Ast.Program.statements = Typed_ast_utils.error_mapper#statement_list aloc_statements;
        comments = aloc_comments;
        all_comments = aloc_all_comments;
      }
    )

(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
*)
let infer_lib_file ~exclude_syms ~lint_severities ~file_sig cx ast =
  let aloc_ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  let (_, { Ast.Program.all_comments; _ }) = ast in
  let (prog_aloc, { Ast.Program.statements = aloc_statements; _ }) = aloc_ast in

  let () =
    (* TODO: Wait a minute, why do we bother with requires for lib files? Pretty
       confident that we don't support them in any sensible way. *)
    add_require_tvars cx file_sig
  in
  try
    initialize_env ~lib:true ~exclude_syms cx aloc_ast Name_def.Global;

    let t_stmts = infer_core cx aloc_statements in
    scan_for_suppressions cx lint_severities all_comments;

    (Env.init_builtins_from_libdef cx, t_stmts)
  with
  | Env_api.Env_invariant (loc, inv) ->
    let loc = Base.Option.value ~default:prog_aloc loc in
    Flow_js.add_output cx Error_message.(EInternal (loc, EnvInvariant inv));
    ([], Typed_ast_utils.error_mapper#statement_list aloc_statements)
