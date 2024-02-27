(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
module Ast = Flow_ast

(* infer phase services *)

module NameResolver = Name_resolver.Make (Context) (Flow_js_utils)
module NameDefOrdering = Name_def_ordering.Make (Context) (Flow_js_utils)

(* Scan the list of comments to place suppressions on the appropriate locations.
    Because each comment can only contain a single code, in order to support
    suppressing multiple types of errors on one location we allow you to stack
    comments like so:
    //$FlowFixMe[x]
    //$FlowFixMe[y]
     some code causing errors x and y

   This logic produces a set of error codes associated with the location of the
   bottom suppression in the stack *)

let scan_for_error_suppressions acc errs comments =
  let open Suppression_comments in
  let open Loc in
  (* If multiple comments are stacked together, we join them into a codeset positioned on the
     location of the last comment *)
  let (supps, errs) =
    Base.List.fold_left comments ~init:([], errs) ~f:(fun (supps, errs) comment ->
        let (loc, { Ast.Comment.text; _ }) = comment in
        match (should_suppress text loc, supps) with
        | (Error (), _) -> (supps, Error_message.EMalformedCode (ALoc.of_loc loc) :: errs)
        | (Ok None, _) -> (supps, errs)
        | (Ok (Some (Specific _ as codes)), (prev_loc, (Specific _ as prev_codes)) :: supps)
          when loc.start.line = prev_loc._end.line + 1 ->
          let supp = ({ prev_loc with _end = loc._end }, join_applicable_codes codes prev_codes) in
          (supp :: supps, errs)
        | (Ok (Some codes), _) -> ((loc, codes) :: supps, errs)
    )
  in
  let acc =
    List.fold_left (fun acc (loc, codes) -> Error_suppressions.add loc codes acc) acc supps
  in
  (acc, errs)

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
  let add_error (loc, kind) acc = Error_message.ELintSetting (ALoc.of_loc loc, kind) :: acc in
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
  let get_kind_setting (acc, errs) arg =
    let arg = trim_and_stars_locational arg in
    match split_delim_locational ':' arg with
    | [rule; setting] ->
      let rule = trim_and_stars_locational rule in
      let setting = trim_and_stars_locational setting in
      begin
        match (parse_kind rule, parse_value setting) with
        | (Ok kinds, Ok setting) ->
          let settings = Base.List.map ~f:(fun kind -> (kind, (setting, arg.loc))) kinds in
          (settings :: acc, errs)
        | (Error e, Ok _) -> (acc, add_error e errs)
        | (Ok _, Error e) -> (acc, add_error e errs)
        | (Error e1, Error e2) -> (acc, add_error e1 (add_error e2 errs))
      end
    | _ -> (acc, add_error (arg.loc, LintSettings.Malformed_argument) errs)
  in
  (* parse arguments of the form lint1:setting1,lint2:setting2... *)
  let get_settings_list errs args =
    let args = split_delim_locational ',' args in
    let (settings, errs) = List.fold_left get_kind_setting ([], errs) args in
    (List.rev settings, errs)
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
  let process_comment ~in_libdef acc comment =
    let (severity_cover_builder, running_settings, suppression_locs, errs) = acc in
    let loc_comment = comment |> convert_comment |> trim_and_stars_locational in
    match parse_keyword loc_comment with
    | Some (keyword, Some args) ->
      (* Case where we're changing certain lint settings *)
      let (settings_list, errs) = get_settings_list errs args in
      let error_encountered = ref false in
      let errs = ref errs in
      let (new_builder, new_running_settings) =
        let covered_range = get_range keyword in
        ExactCover.update_settings_and_running
          ~in_libdef
          running_settings
          (fun err ->
            error_encountered := true;
            errs := add_error err !errs)
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
                  errs := add_error (arg_loc, LintSettings.Overwritten_argument) !errs
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
          (new_builder, running_settings, suppression_locs, !errs)
        | Unending -> (new_builder, new_running_settings, suppression_locs, !errs)
      end
    | Some (keyword, None) ->
      (* Case where we're wholly enabling/disabling linting *)
      (* TODO (rballard): regional lint disabling *)
      let errs = add_error (keyword.loc, LintSettings.Naked_comment) errs in
      (severity_cover_builder, running_settings, suppression_locs, errs)
    | None -> acc
  in
  fun ~in_libdef base_settings acc errs file_keys_with_comments ->
    Base.List.fold
      file_keys_with_comments
      ~init:(Utils_js.FilenameMap.empty, acc, errs)
      ~f:(fun (severity_covers, acc, errs) (file_key, comments) ->
        let severity_cover_builder = ExactCover.new_builder file_key base_settings in
        let (severity_cover_builder, _, suppression_locs, errs) =
          List.fold_left
            (process_comment ~in_libdef)
            (severity_cover_builder, base_settings, Loc_collections.LocSet.empty, errs)
            comments
        in
        let severity_cover = ExactCover.bake severity_cover_builder in
        let acc = Error_suppressions.add_lint_suppressions suppression_locs acc in
        (Utils_js.FilenameMap.add file_key severity_cover severity_covers, acc, errs)
    )

let scan_for_suppressions ~in_libdef lint_severities file_keys_with_comments =
  let file_keys_with_comments =
    Base.List.map file_keys_with_comments ~f:(fun (file, comments) ->
        (file, List.sort (fun (loc1, _) (loc2, _) -> Loc.compare loc1 loc2) comments)
    )
  in
  let acc = Error_suppressions.empty in
  let (acc, errs) =
    scan_for_error_suppressions acc [] (Base.List.bind file_keys_with_comments ~f:snd)
  in
  scan_for_lint_suppressions ~in_libdef lint_severities acc errs file_keys_with_comments

module Statement = Fix_statement.Statement_

(**********)
(* Driver *)
(**********)

let initialize_env ~lib ?(exclude_syms = SSet.empty) cx aloc_ast toplevel_scope_kind =
  try
    let (_abrupt_completion, info) =
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
      Name_def.find_defs ~autocomplete_hooks info toplevel_scope_kind aloc_ast
    in
    let hint_map = ALocMap.mapi (Env_resolution.lazily_resolve_hints cx) hint_map in
    let pred_func_map =
      ALocMap.map (Env_resolution.resolve_pred_func cx) info.Env_api.pred_func_map
    in
    let env = Loc_env.with_info Name_def.Global hint_map info pred_func_map name_def_graph in
    Context.set_environment cx env;
    let components = NameDefOrdering.build_ordering cx ~autocomplete_hooks info name_def_graph in
    Base.List.iter ~f:(Cycles.handle_component cx name_def_graph) components;
    Type_env.init_env cx toplevel_scope_kind;
    let { Loc_env.scope_kind; class_stack; _ } = Context.environment cx in
    Base.List.iter ~f:(Env_resolution.resolve_component cx name_def_graph) components;
    Debug_js.Verbose.print_if_verbose_lazy cx (lazy ["Finished all components"]);
    let env = Context.environment cx in
    Context.set_environment cx { env with Loc_env.scope_kind; class_stack }
  with
  | Env_api.Env_invariant (loc, inv) ->
    let loc = Base.Option.value ~default:(fst aloc_ast) loc in
    Flow_js.add_output cx Error_message.(EInternal (loc, EnvInvariant inv))

(* Lint suppressions are handled iff lint_severities is Some. *)
let infer_ast ~lint_severities cx filename loc_comments aloc_ast =
  assert (Context.is_checked cx);
  let (prog_aloc, { Ast.Program.statements; interpreter; comments; all_comments }) = aloc_ast in
  initialize_env ~lib:false cx aloc_ast Name_def.Module;
  let typed_statements = Statement.statement_list cx statements in
  let (severity_cover, suppressions, suppression_errors) =
    scan_for_suppressions ~in_libdef:false lint_severities [(filename, loc_comments)]
  in
  Context.add_severity_covers cx severity_cover;
  Context.add_error_suppressions cx suppressions;
  List.iter (Flow_js.add_output cx) suppression_errors;
  (prog_aloc, { Ast.Program.statements = typed_statements; interpreter; comments; all_comments })

class lib_def_loc_mapper_and_validator cx =
  let stmt_validator ~in_toplevel_scope (loc, stmt) =
    let error kind =
      Error_message.(
        EUnsupportedSyntax
          (loc, ContextDependentUnsupportedStatement (UnsupportedStatementInLibdef kind))
      )
    in
    let error_opt =
      let open Flow_ast.Statement in
      match stmt with
      | DeclareClass _
      | DeclareComponent _
      | DeclareEnum _
      | DeclareExportDeclaration _
      | DeclareFunction _
      | DeclareInterface _
      | DeclareModule _
      | DeclareModuleExports _
      | DeclareNamespace _
      | DeclareTypeAlias _
      | DeclareOpaqueType _
      | DeclareVariable _
      | Empty _
      | EnumDeclaration _
      (* directives are not used and could be banned as well *)
      | Expression { Expression.directive = Some _; _ }
      | ExportNamedDeclaration { ExportNamedDeclaration.export_kind = ExportType; _ }
      | InterfaceDeclaration _
      | TypeAlias _
      | OpaqueType _ ->
        None
      | ExportNamedDeclaration { ExportNamedDeclaration.export_kind = ExportValue; _ } ->
        Some (error "export")
      | ImportDeclaration _ ->
        if in_toplevel_scope then
          Some
            Error_message.(
              EUnsupportedSyntax (loc, ContextDependentUnsupportedStatement ToplevelLibraryImport)
            )
        else
          None
      | Block _ -> Some (error "block")
      | Break _ -> Some (error "break")
      | ClassDeclaration _ -> Some (error "class declaration")
      | ComponentDeclaration _ -> Some (error "component declaration")
      | Continue _ -> Some (error "continue")
      | Debugger _ -> Some (error "debugger")
      | DoWhile _ -> Some (error "do while")
      | ExportDefaultDeclaration _ -> Some (error "export default")
      | Expression _ -> Some (error "expression")
      | For _ -> Some (error "for")
      | ForIn _ -> Some (error "for in")
      | ForOf _ -> Some (error "for of")
      | FunctionDeclaration _ -> Some (error "function declaration")
      | If _ -> Some (error "if")
      | Labeled _ -> Some (error "labeled")
      | Return _ -> Some (error "return")
      | Switch _ -> Some (error "switch")
      | Throw _ -> Some (error "throw")
      | Try _ -> Some (error "try")
      | VariableDeclaration _ -> Some (error "variable declaration")
      | While _ -> Some (error "while")
      | With _ -> Some (error "with")
    in
    match error_opt with
    | None -> true
    | Some error ->
      let node_cache = Context.node_cache cx in
      Node_cache.set_statement node_cache (Typed_ast_utils.error_mapper#statement (loc, stmt));
      Flow_js_utils.add_output cx error;
      false
  in
  object
    inherit [ALoc.t] Flow_ast_mapper.mapper as super

    method! toplevel_statement_list stmts =
      stmts |> Base.List.filter ~f:(stmt_validator ~in_toplevel_scope:true) |> super#statement_list

    method! declare_module l m =
      let open Ast.Statement.DeclareModule in
      let { id; body = (body_loc, body_block); comments } = m in
      super#declare_module
        l
        {
          Ast.Statement.DeclareModule.id;
          body =
            ( body_loc,
              {
                body_block with
                Ast.Statement.Block.body =
                  Base.List.filter
                    ~f:(stmt_validator ~in_toplevel_scope:false)
                    body_block.Ast.Statement.Block.body;
              }
            );
          comments;
        }
  end

(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
*)
let infer_lib_file ~lint_severities cx file_key loc_comments aloc_ast =
  let validator_visitor = new lib_def_loc_mapper_and_validator cx in
  let filtered_aloc_ast = validator_visitor#program aloc_ast in
  let (prog_aloc, { Ast.Program.statements; interpreter; comments; all_comments }) = aloc_ast in
  let exclude_syms = cx |> Context.builtins |> Builtins.builtin_ordinary_name_set in
  initialize_env ~lib:true ~exclude_syms cx filtered_aloc_ast Name_def.Global;
  let (severity_cover, suppressions, suppression_errors) =
    scan_for_suppressions ~in_libdef:true lint_severities [(file_key, loc_comments)]
  in
  let typed_statements = Statement.statement_list cx statements in
  Context.add_severity_covers cx severity_cover;
  Context.add_error_suppressions cx suppressions;
  List.iter (Flow_js.add_output cx) suppression_errors;
  (prog_aloc, { Ast.Program.statements = typed_statements; interpreter; comments; all_comments })

let infer_file ~lint_severities cx file_key all_comments aloc_ast =
  if File_key.is_lib_file file_key then
    infer_lib_file ~lint_severities cx file_key all_comments aloc_ast
  else
    infer_ast ~lint_severities cx file_key all_comments aloc_ast
