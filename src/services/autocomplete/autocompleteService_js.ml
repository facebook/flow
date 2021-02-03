(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Autocomplete_js
open Base.Result
open Parsing_heaps_utils
open Loc_collections

let max_autoimport_suggestions = 100

let default_autoimport_options =
  Fuzzy_path.
    {
      default_options with
      max_results = max_autoimport_suggestions;
      num_threads = Base.Int.max 1 (Sys_utils.nbr_procs - 2);
    }

let autocomplete_suffix = "AUTO332"

let suffix_len = String.length autocomplete_suffix

let add_autocomplete_token contents line column =
  let line = line - 1 in
  let contents_with_token =
    Line.transform_nth contents line (fun line_str ->
        let length = String.length line_str in
        if length >= column then
          let start = String.sub line_str 0 column in
          let end_ = String.sub line_str column (length - column) in
          start ^ autocomplete_suffix ^ end_
        else
          line_str)
  in
  let f (_, x, _) = x in
  let default = "" in
  ( contents_with_token,
    Base.Option.value_map ~f ~default (Line.split_nth contents_with_token (line - 1))
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_token line)
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_token (line + 1)) )

(* the autocomplete token inserts `suffix_len` characters, which are included
 * in `ac_loc` returned by `Autocomplete_js`. They need to be removed before
 * showing `ac_loc` to the client. *)
let remove_autocomplete_token_from_loc loc =
  Loc.{ loc with _end = { loc._end with column = loc._end.column - suffix_len } }

let remove_autocomplete_token =
  let regexp = Str.regexp_string autocomplete_suffix in
  fun str ->
    match Str.bounded_split_delim regexp str 2 with
    | [] -> ("", "")
    | [str] -> (str, "")
    | before :: after :: _ -> (before, after)

let lsp_completion_of_type =
  let open Ty in
  function
  | InlineInterface _ -> Some Lsp.Completion.Interface
  | StrLit _
  | NumLit _
  | BoolLit _ ->
    Some Lsp.Completion.Value
  | Fun _ -> Some Lsp.Completion.Function
  | Union _ -> Some Lsp.Completion.Enum
  | Tup _
  | Bot _
  | Null
  | Obj _
  | Inter _
  | TVar _
  | Bound _
  | Generic _
  | Any _
  | Top
  | Void
  | Symbol
  | Num _
  | Str _
  | Bool _
  | Arr _
  | TypeOf _
  | Utility _
  | Mu _
  | CharSet _ ->
    Some Lsp.Completion.Variable

let lsp_completion_of_decl =
  let open Ty in
  function
  | VariableDecl _ -> Lsp.Completion.Variable
  | TypeAliasDecl _ -> Lsp.Completion.Enum
  | ClassDecl _ -> Lsp.Completion.Class
  | InterfaceDecl _ -> Lsp.Completion.Interface
  | EnumDecl _ -> Lsp.Completion.Enum
  | ModuleDecl _ -> Lsp.Completion.Module

let sort_text_of_rank rank = Some (Printf.sprintf "%020u" rank)

let text_edit ?insert_text (name, loc) =
  let newText = Base.Option.value ~default:name insert_text in
  (loc, newText)

let autocomplete_create_result
    ?insert_text ?(rank = 0) ?(preselect = false) ?documentation ~exact_by_default (name, loc) ty =
  let detail = Ty_printer.string_of_t_single_line ~with_comments:false ~exact_by_default ty in
  let kind = lsp_completion_of_type ty in
  let text_edits = [text_edit ?insert_text (name, loc)] in
  let sort_text = sort_text_of_rank rank in
  {
    ServerProt.Response.Completion.kind;
    name;
    text_edits;
    detail;
    sort_text;
    preselect;
    documentation;
  }

let autocomplete_create_result_decl
    ?insert_text ~rank ?(preselect = false) ?documentation ~exact_by_default (name, loc) d =
  let open Ty in
  let (kind, detail) =
    match d with
    | ModuleDecl _ -> (Some Lsp.Completion.Module, "module " ^ name)
    | Ty.VariableDecl (_, ty) ->
      ( Some Lsp.Completion.Variable,
        Ty_printer.string_of_t_single_line ~with_comments:false ~exact_by_default ty )
    | d ->
      ( Some (lsp_completion_of_decl d),
        Ty_printer.string_of_decl_single_line ~with_comments:false ~exact_by_default d )
  in
  let text_edits = [text_edit ?insert_text (name, loc)] in
  let sort_text = sort_text_of_rank rank in
  {
    ServerProt.Response.Completion.kind;
    name;
    text_edits;
    detail;
    sort_text;
    preselect;
    documentation;
  }

let autocomplete_create_result_elt
    ?insert_text ?(rank = 0) ?preselect ?documentation ~exact_by_default (name, loc) elt =
  match elt with
  | Ty.Type t ->
    autocomplete_create_result
      ?insert_text
      ~rank
      ?preselect
      ?documentation
      ~exact_by_default
      (name, loc)
      t
  | Ty.Decl d ->
    autocomplete_create_result_decl
      ?insert_text
      ~rank
      ?preselect
      ?documentation
      ~exact_by_default
      (name, loc)
      d

let ty_normalizer_options =
  Ty_normalizer_env.
    {
      expand_internal_types = true;
      expand_type_aliases = false;
      flag_shadowed_type_params = true;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = true;
      optimize_types = true;
      omit_targ_defaults = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 50;
    }

type autocomplete_service_result =
  | AcResult of {
      result: ServerProt.Response.Completion.t;
      errors_to_log: string list;
    }
  | AcEmpty of string
  | AcFatalError of string

let documentation_of_member ~reader ~cx ~typed_ast this name =
  match GetDef_js.extract_member_def ~reader cx this name with
  | Ok loc ->
    Find_documentation.jsdoc_of_getdef_loc ~current_ast:typed_ast ~reader loc
    |> Base.Option.bind ~f:Find_documentation.documentation_of_jsdoc
  | Error _ -> None

let members_of_type
    ~reader ~exclude_proto_members ?(exclude_keys = SSet.empty) ~tparams cx file_sig typed_ast type_
    =
  let ty_members =
    Ty_members.extract
      ~include_proto_members:(not exclude_proto_members)
      ~cx
      ~typed_ast
      ~file_sig
      Type.TypeScheme.{ tparams; type_ }
  in
  let is_valid_member (s, _) =
    (* filter out constructor, it shouldn't be called manually *)
    s <> "constructor"
    (* filter out indexer/call properties *)
    && (not (String.length s >= 1 && s.[0] = '$'))
    (* strip out members from prototypes which are implicitly created for internal reasons *)
    && (not (Reason.is_internal_name s))
    && (* exclude members the caller told us to exclude *)
    not (SSet.mem s exclude_keys)
  in
  match ty_members with
  | Error error -> fail error
  | Ok Ty_members.{ members; errors; in_idx } ->
    return
      ( members
        |> SMap.bindings
        |> List.filter is_valid_member
        |> List.map (fun (name, info) ->
               (name, documentation_of_member ~reader ~cx ~typed_ast type_ name, info)),
        (match errors with
        | [] -> []
        | _ :: _ -> Printf.sprintf "members_of_type %s" (Debug_js.dump_t cx type_) :: errors),
        in_idx )

let autocomplete_member
    ~reader
    ~exclude_proto_members
    ?exclude_keys
    cx
    file_sig
    typed_ast
    this
    in_optional_chain
    ac_loc
    ~tparams =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let exact_by_default = Context.exact_by_default cx in
  match
    members_of_type ~reader ~exclude_proto_members ?exclude_keys cx file_sig typed_ast this ~tparams
  with
  | Error err -> AcFatalError err
  | Ok (mems, errors_to_log, in_idx) ->
    let items =
      mems
      |> Base.List.map
           ~f:(fun (name, documentation, Ty_members.{ ty; from_proto; from_nullable; def_loc = _ })
                   ->
             let rank =
               if from_proto then
                 1
               else
                 0
             in
             let opt_chain_ty =
               Ty_utils.simplify_type ~merge_kinds:true (Ty.Union (Ty.Void, ty, []))
             in
             match (from_nullable, in_optional_chain, in_idx) with
             | (false, _, _)
             | (_, _, true) ->
               autocomplete_create_result ~rank ?documentation ~exact_by_default (name, ac_loc) ty
             | (true, false, false) ->
               let opt_chain_name = "?." ^ name in
               let opt_chain_ac_loc = Loc.btwn (Loc.char_before ac_loc) ac_loc in
               autocomplete_create_result
                 ~insert_text:opt_chain_name
                 ~rank
                 ?documentation
                 ~exact_by_default
                 (opt_chain_name, opt_chain_ac_loc)
                 opt_chain_ty
             | (true, true, false) ->
               autocomplete_create_result
                 ~rank
                 ?documentation
                 ~exact_by_default
                 (name, ac_loc)
                 opt_chain_ty)
    in
    let result = { ServerProt.Response.Completion.items; is_incomplete = false } in
    AcResult { result; errors_to_log }

(* The fact that we need this feels convoluted.
   We run Scope_builder on the untyped AST and now we go back to the typed AST to get the types
   of the locations we got from Scope_api. We wouldn't need to do this separate pass if
   Scope_builder/Scope_api were polymorphic.
*)
class type_collector (reader : Parsing_heaps.Reader.reader) (locs : LocSet.t) =
  object
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    val mutable acc = LocMap.empty

    method on_loc_annot x = x

    method on_type_annot x = x

    method collected_types = acc

    method! t_identifier (((aloc, t), _) as ident) =
      let loc = loc_of_aloc ~reader aloc in
      if LocSet.mem loc locs then acc <- LocMap.add loc t acc;
      ident
  end

let collect_types ~reader locs typed_ast =
  let collector = new type_collector reader locs in
  Stdlib.ignore (collector#program typed_ast);
  collector#collected_types

let documentation_of_loc ~options ~reader ~cx ~file_sig ~typed_ast loc =
  let open GetDef_js.Get_def_result in
  match GetDef_js.get_def ~options ~reader ~cx ~file_sig ~typed_ast loc with
  | Def getdef_loc
  | Partial (getdef_loc, _) ->
    Find_documentation.jsdoc_of_getdef_loc ~current_ast:typed_ast ~reader getdef_loc
    |> Base.Option.bind ~f:Find_documentation.documentation_of_jsdoc
  | Bad_loc
  | Def_error _ ->
    None

let local_value_identifiers ~options ~reader ~cx ~ac_loc ~file_sig ~ast ~typed_ast ~tparams =
  let scope_info = Scope_builder.program ~with_types:false ast in
  let open Scope_api.With_Loc in
  (* get the innermost scope enclosing the requested location *)
  let (ac_scope_id, _) =
    IMap.fold
      (fun this_scope_id this_scope (prev_scope_id, prev_scope) ->
        if
          Reason.in_range ac_loc this_scope.Scope.loc
          && Reason.in_range this_scope.Scope.loc prev_scope.Scope.loc
        then
          (this_scope_id, this_scope)
        else
          (prev_scope_id, prev_scope))
      scope_info.scopes
      (0, scope scope_info 0)
  in
  (* gather all in-scope variables *)
  let names_and_locs =
    fold_scope_chain
      scope_info
      (fun _ scope acc ->
        let scope_vars = scope.Scope.defs |> SMap.map (fun Def.{ locs; _ } -> Nel.hd locs) in

        (* don't suggest lexically-scoped variables declared after the current location.
           this filtering isn't perfect:

             let foo = /* request here */
                 ^^^
                 def_loc

           since def_loc is the location of the identifier within the declaration statement
           (not the entire statement), we don't filter out foo when declaring foo. *)
        let relevant_scope_vars =
          if scope.Scope.lexical then
            SMap.filter (fun _name def_loc -> Loc.compare def_loc ac_loc < 0) scope_vars
          else
            scope_vars
        in
        SMap.union acc relevant_scope_vars)
      ac_scope_id
      SMap.empty
  in
  let types = collect_types ~reader (LocSet.of_list (SMap.values names_and_locs)) typed_ast in
  names_and_locs
  |> SMap.bindings
  |> Base.List.filter_map ~f:(fun (name, loc) ->
         (* TODO(vijayramamurthy) do something about sometimes failing to collect types *)
         Base.Option.map (LocMap.find_opt loc types) ~f:(fun type_ ->
             ( (name, documentation_of_loc ~options ~reader ~cx ~file_sig ~typed_ast loc),
               Type.TypeScheme.{ tparams; type_ } )))
  |> Ty_normalizer.from_schemes
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

let src_dir_of_loc ac_loc =
  Loc.source ac_loc |> Base.Option.map ~f:(fun key -> File_key.to_string key |> Filename.dirname)

let completion_item_of_autoimport
    ~options ~reader ~src_dir ~ast ~ac_loc { Export_search.name; source; kind } =
  let options =
    Js_layout_generator.{ default_opts with single_quotes = Options.format_single_quotes options }
  in
  match
    Code_action_service.text_edits_of_import ~options ~reader ~src_dir ~ast kind name source
  with
  | None ->
    {
      ServerProt.Response.Completion.kind = Some Lsp.Completion.Variable;
      name;
      detail = name;
      text_edits = [text_edit (name, ac_loc)];
      sort_text = sort_text_of_rank 101 (* TODO: use a constant *);
      preselect = false;
      documentation = None;
    }
  | Some { Code_action_service.title; edits } ->
    let edits =
      Base.List.map
        ~f:(fun { Lsp.TextEdit.range; newText } ->
          let loc = Flow_lsp_conversions.lsp_range_to_flow_loc range in
          (loc, newText))
        edits
    in
    {
      ServerProt.Response.Completion.kind = Some Lsp.Completion.Variable;
      name;
      detail = name;
      text_edits = text_edit (name, ac_loc) :: edits;
      sort_text = sort_text_of_rank 100 (* TODO: use a constant *);
      preselect = false;
      documentation = Some title;
    }

let append_completion_items_of_autoimports ~options ~reader ~ast ~ac_loc auto_imports acc =
  let src_dir = src_dir_of_loc ac_loc in
  Base.List.fold_left
    ~init:acc
    ~f:(fun acc auto_import ->
      let item = completion_item_of_autoimport ~options ~reader ~src_dir ~ast ~ac_loc auto_import in
      item :: acc)
    auto_imports

(* env is all visible bound names at cursor *)
let autocomplete_id
    ~env
    ~options
    ~reader
    ~cx
    ~ac_loc
    ~file_sig
    ~ast
    ~typed_ast
    ~include_super
    ~include_this
    ~imports
    ~tparams
    ~token =
  (* TODO: filter to results that match `token` *)
  let open ServerProt.Response.Completion in
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let exact_by_default = Context.exact_by_default cx in
  let (items, errors_to_log) =
    local_value_identifiers ~options ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~ast ~tparams
    |> List.fold_left
         (fun (items, errors_to_log) ((name, documentation), elt_result) ->
           match elt_result with
           | Ok elt ->
             let result =
               autocomplete_create_result_elt
                 ~insert_text:name
                 ?documentation
                 ~exact_by_default
                 (name, ac_loc)
                 elt
             in
             (result :: items, errors_to_log)
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (items, error_to_log :: errors_to_log))
         ([], [])
  in
  (* "this" is legal inside classes and (non-arrow) functions *)
  let items =
    if include_this then
      {
        kind = Some Lsp.Completion.Variable;
        name = "this";
        detail = "this";
        text_edits = [text_edit ("this", ac_loc)];
        sort_text = sort_text_of_rank 0;
        preselect = false;
        documentation = None;
      }
      :: items
    else
      items
  in
  (* "super" is legal inside classes *)
  let items =
    if include_super then
      {
        kind = Some Lsp.Completion.Variable;
        name = "super";
        detail = "super";
        text_edits = [text_edit ("super", ac_loc)];
        sort_text = sort_text_of_rank 0;
        preselect = false;
        documentation = None;
      }
      :: items
    else
      items
  in
  let result =
    if imports then
      let (before, _after) = remove_autocomplete_token token in
      if before = "" then
        (* for empty autocomplete requests (hitting ctrl-space without typing anything),
           don't include any autoimport results, but do set `is_incomplete` so that
           it queries again when you type something. *)
        { ServerProt.Response.Completion.items; is_incomplete = true }
      else
        let { Export_search.results = auto_imports; is_incomplete } =
          Export_search.search_values
            ~options:default_autoimport_options
            before
            env.ServerEnv.exports
        in
        let items =
          append_completion_items_of_autoimports ~options ~reader ~ast ~ac_loc auto_imports items
        in
        { ServerProt.Response.Completion.items; is_incomplete }
    else
      (* if autoimports are not enabled, then we have all the results *)
      { ServerProt.Response.Completion.items; is_incomplete = false }
  in
  AcResult { result; errors_to_log }

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx_attribute
    ~reader cx file_sig typed_ast cls ac_name ~used_attr_names ~has_value ac_loc ~tparams =
  let open Flow_js in
  let reason = Reason.mk_reason (Reason.RCustom ac_name) ac_loc in
  let props_object =
    Tvar.mk_where cx reason (fun tvar ->
        let use_op = Type.Op Type.UnknownUse in
        flow cx (cls, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar)))
  in
  let exact_by_default = Context.exact_by_default cx in
  (* The `children` prop (if it exists) is set with the contents between the opening and closing
   * elements, rather than through an explicit `children={...}` attribute, so we should exclude
   * it from the autocomplete results, along with already used attribute names. *)
  let exclude_keys = SSet.add "children" used_attr_names in
  (* Only include own properties, so we don't suggest things like `hasOwnProperty` as potential JSX properties *)
  let mems_result =
    members_of_type
      ~reader
      ~exclude_proto_members:true
      ~exclude_keys
      cx
      file_sig
      typed_ast
      props_object
      ~tparams
  in
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  match mems_result with
  | Error err -> AcFatalError err
  | Ok (mems, errors_to_log, _) ->
    let items =
      mems
      |> Base.List.map ~f:(fun (name, documentation, Ty_members.{ ty; _ }) ->
             let insert_text =
               if has_value then
                 name
               else
                 name ^ "="
             in
             autocomplete_create_result
               ~insert_text
               ?documentation
               ~exact_by_default
               (name, ac_loc)
               ty)
    in
    let result = { ServerProt.Response.Completion.items; is_incomplete = false } in
    AcResult { result; errors_to_log }

(* TODO(vijayramamurthy) think about how to break this file down into smaller modules *)
(* NOTE: excludes classes, because we'll get those from local_value_identifiers *)
class local_type_identifiers_searcher =
  object (this)
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = x

    method on_type_annot x = x

    val mutable ids = []

    method ids = ids

    method add_id id = ids <- id :: ids

    method! type_alias (Flow_ast.Statement.TypeAlias.{ id; _ } as x) =
      this#add_id id;
      x

    method! opaque_type (Flow_ast.Statement.OpaqueType.{ id; _ } as x) =
      this#add_id id;
      x

    method! interface (Flow_ast.Statement.Interface.{ id; _ } as x) =
      this#add_id id;
      x

    method! import_declaration _ x =
      let open Flow_ast.Statement.ImportDeclaration in
      let { import_kind; specifiers; default; _ } = x in
      let binds_type = function
        | ImportType
        | ImportTypeof ->
          true
        | ImportValue -> false
      in
      let declaration_binds_type = binds_type import_kind in
      if declaration_binds_type then Base.Option.iter default ~f:this#add_id;
      Base.Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers specifiers ->
            List.iter
              (fun { kind; local; remote } ->
                let specifier_binds_type =
                  match kind with
                  | None -> declaration_binds_type
                  | Some k -> binds_type k
                in
                if specifier_binds_type then this#add_id (Base.Option.value local ~default:remote))
              specifiers
          | ImportNamespaceSpecifier _ -> ( (* namespaces can't be types *) ));
      x
  end

let local_type_identifiers ~typed_ast ~cx ~file_sig =
  let search = new local_type_identifiers_searcher in
  Stdlib.ignore (search#program typed_ast);
  search#ids
  |> Base.List.map ~f:(fun ((loc, t), Flow_ast.Identifier.{ name; _ }) -> ((name, loc), t))
  |> Ty_normalizer.from_types
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

let type_exports_of_module_ty ~ac_loc ~exact_by_default ~documentation_of_module_member =
  let open ServerProt.Response.Completion in
  let open Ty in
  function
  | Decl (ModuleDecl { exports; _ }) ->
    Base.List.filter_map
      ~f:(function
        | TypeAliasDecl { name = { Ty.sym_name; _ }; type_ = Some t; _ } as d ->
          Some
            {
              kind = lsp_completion_of_type t;
              name = sym_name;
              text_edits = [text_edit (sym_name, ac_loc)];
              detail = Ty_printer.string_of_decl_single_line ~exact_by_default d;
              sort_text = None;
              preselect = false;
              documentation = documentation_of_module_member sym_name;
            }
        | InterfaceDecl ({ Ty.sym_name; _ }, _) as d ->
          Some
            {
              kind = Some Lsp.Completion.Interface;
              name = sym_name;
              text_edits = [text_edit (sym_name, ac_loc)];
              detail = Ty_printer.string_of_decl_single_line ~exact_by_default d;
              sort_text = None;
              preselect = false;
              documentation = documentation_of_module_member sym_name;
            }
        | ClassDecl ({ Ty.sym_name; _ }, _) as d ->
          Some
            {
              kind = Some Lsp.Completion.Class;
              name = sym_name;
              text_edits = [text_edit (sym_name, ac_loc)];
              detail = Ty_printer.string_of_decl_single_line ~exact_by_default d;
              sort_text = None;
              preselect = false;
              documentation = documentation_of_module_member sym_name;
            }
        | EnumDecl { Ty.sym_name; _ } as d ->
          Some
            {
              kind = Some Lsp.Completion.Enum;
              name = sym_name;
              text_edits = [text_edit (sym_name, ac_loc)];
              detail = Ty_printer.string_of_decl_single_line ~exact_by_default d;
              sort_text = None;
              preselect = false;
              documentation = documentation_of_module_member sym_name;
            }
        | _ -> None)
      exports
    |> Base.List.sort ~compare:(fun { name = a; _ } { name = b; _ } -> String.compare a b)
    |> Base.List.mapi ~f:(fun i r -> { r with sort_text = sort_text_of_rank i })
  | _ -> []

let autocomplete_unqualified_type
    ~env ~options ~reader ~cx ~imports ~tparams ~file_sig ~ac_loc ~ast ~typed_ast ~token =
  (* TODO: filter to results that match `token` *)
  let open ServerProt.Response.Completion in
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let exact_by_default = Context.exact_by_default cx in
  let tparam_results =
    Base.List.map
      ~f:(fun (_, name) ->
        {
          kind = Some Lsp.Completion.TypeParameter;
          name;
          detail = name;
          text_edits = [text_edit (name, ac_loc)];
          sort_text = sort_text_of_rank 0;
          preselect = false;
          documentation = None;
        })
      tparams
  in
  let (tparam_and_tident_results, tparam_and_tident_errors_to_log) =
    local_type_identifiers ~typed_ast ~cx ~file_sig
    |> List.fold_left
         (fun (results, errors_to_log) ((name, aloc), ty_result) ->
           let documentation =
             loc_of_aloc ~reader aloc
             |> documentation_of_loc ~options ~reader ~cx ~file_sig ~typed_ast
           in
           match ty_result with
           | Ok elt ->
             let result =
               autocomplete_create_result_elt ?documentation ~exact_by_default (name, ac_loc) elt
             in
             (result :: results, errors_to_log)
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (results, error_to_log :: errors_to_log))
         (tparam_results, [])
  in
  (* The value-level identifiers we suggest in type autocompletion:
     - classes
     - modules (followed by a dot) *)
  let (items, errors_to_log) =
    local_value_identifiers ~options ~ast ~typed_ast ~reader ~ac_loc ~tparams ~cx ~file_sig
    |> List.fold_left
         (fun (items, errors_to_log) ((name, documentation), ty_res) ->
           match ty_res with
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (items, error_to_log :: errors_to_log)
           | Ok (Ty.Decl (Ty.ClassDecl _ | Ty.EnumDecl _) as elt) ->
             let result =
               autocomplete_create_result_elt ?documentation ~exact_by_default (name, ac_loc) elt
             in
             (result :: items, errors_to_log)
           | Ok elt
             when type_exports_of_module_ty
                    ~ac_loc
                    ~exact_by_default
                    ~documentation_of_module_member:Base.Option.some
                    elt
                  <> [] ->
             let result =
               autocomplete_create_result_elt
                 ?documentation
                 ~exact_by_default
                 (name, ac_loc)
                 elt
                 ~insert_text:(name ^ ".")
             in
             (result :: items, errors_to_log)
           | Ok _ -> (items, errors_to_log))
         (tparam_and_tident_results, tparam_and_tident_errors_to_log)
  in
  let result =
    if imports then
      let { Export_search.results = auto_imports; is_incomplete } =
        let (before, _after) = remove_autocomplete_token token in
        Export_search.search_types ~options:default_autoimport_options before env.ServerEnv.exports
      in
      let items =
        append_completion_items_of_autoimports ~options ~reader ~ast ~ac_loc auto_imports items
      in
      { ServerProt.Response.Completion.items; is_incomplete }
    else
      { ServerProt.Response.Completion.items; is_incomplete = false }
  in
  AcResult { result; errors_to_log }

let autocomplete_qualified_type ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams ~qtype =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let qtype_scheme = Type.TypeScheme.{ tparams; type_ = qtype } in
  let exact_by_default = Context.exact_by_default cx in
  let module_ty_res =
    Ty_normalizer.from_scheme
      ~options:ty_normalizer_options
      ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)
      qtype_scheme
  in
  let documentation_of_module_member = documentation_of_member ~reader ~cx ~typed_ast qtype in
  let (items, errors_to_log) =
    match module_ty_res with
    | Error err -> ([], [Ty_normalizer.error_to_string err])
    | Ok module_ty ->
      ( type_exports_of_module_ty ~ac_loc ~exact_by_default ~documentation_of_module_member module_ty,
        [] )
  in
  AcResult
    { result = { ServerProt.Response.Completion.items; is_incomplete = false }; errors_to_log }

let autocomplete_get_results
    ~env ~options ~reader ~cx ~file_sig ~ast ~typed_ast ~imports trigger_character cursor =
  let file_sig = File_sig.abstractify_locs file_sig in
  match Autocomplete_js.process_location ~trigger_character ~cursor ~typed_ast with
  | Some (_, _, Ac_binding) -> ("Empty", AcEmpty "Binding")
  | Some (_, _, Ac_ignored) -> ("Empty", AcEmpty "Ignored")
  | Some (_, _, Ac_comment) -> ("Empty", AcEmpty "Comment")
  | Some (_, _, Ac_literal) -> ("Empty", AcEmpty "Literal")
  | Some (_, _, Ac_jsx_text) -> ("Empty", AcEmpty "JSXText")
  | Some (_, _, Ac_module) ->
    (* TODO: complete module names *)
    ("Acmodule", AcEmpty "Module")
  | Some (_, _, Ac_key) ->
    (* TODO: complete object keys based on their upper bounds *)
    let result = { ServerProt.Response.Completion.items = []; is_incomplete = false } in
    ("Ackey", AcResult { result; errors_to_log = [] })
  | Some (tparams, ac_loc, Ac_id { token; include_super; include_this }) ->
    ( "Acid",
      autocomplete_id
        ~env
        ~options
        ~reader
        ~cx
        ~ac_loc
        ~file_sig
        ~ast
        ~typed_ast
        ~include_super
        ~include_this
        ~imports
        ~tparams
        ~token )
  | Some (tparams, ac_loc, Ac_member { obj_type; in_optional_chain }) ->
    ( "Acmem",
      autocomplete_member
        ~reader
        ~exclude_proto_members:false
        cx
        file_sig
        typed_ast
        obj_type
        in_optional_chain
        ac_loc
        ~tparams )
  | Some
      (tparams, ac_loc, Ac_jsx_attribute { attribute_name; used_attr_names; component_t; has_value })
    ->
    ( "Acjsx",
      autocomplete_jsx_attribute
        ~reader
        cx
        file_sig
        typed_ast
        component_t
        attribute_name
        ~used_attr_names
        ~has_value
        ac_loc
        ~tparams )
  | Some (tparams, ac_loc, Ac_type { token }) ->
    ( "Actype",
      autocomplete_unqualified_type
        ~env
        ~options
        ~reader
        ~cx
        ~imports
        ~tparams
        ~ac_loc
        ~ast
        ~typed_ast
        ~file_sig
        ~token )
  | Some (tparams, ac_loc, Ac_qualified_type qtype) ->
    ( "Acqualifiedtype",
      autocomplete_qualified_type ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams ~qtype )
  | None ->
    let result = { ServerProt.Response.Completion.items = []; is_incomplete = false } in
    ("None", AcResult { result; errors_to_log = ["Autocomplete token not found in AST"] })
