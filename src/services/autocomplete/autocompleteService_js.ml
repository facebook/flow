(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result
open Loc_collections

let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc

let max_autoimport_suggestions = 100

let default_autoimport_options =
  let open Fuzzy_path in
  {
    default_options with
    max_results = max_autoimport_suggestions;
    num_threads = Base.Int.max 1 (Sys_utils.nbr_procs - 2);
  }

let lsp_completion_of_type =
  let open Ty in
  function
  | InlineInterface _ -> Lsp.Completion.Interface
  | StrLit _
  | NumLit _
  | BigIntLit _
  | BoolLit _ ->
    Lsp.Completion.Value
  | Fun _ -> Lsp.Completion.Function
  | Union _ -> Lsp.Completion.Enum
  | Tup _
  | Bot _
  | Null
  | Obj _
  | Inter _
  | Bound _
  | Generic _
  | Any _
  | Top
  | Void
  | Symbol
  | Num _
  | Str _
  | Bool _
  | BigInt _
  | Arr _
  | TypeOf _
  | Utility _
  | IndexedAccess _
  | CharSet _ ->
    Lsp.Completion.Variable

let lsp_completion_of_decl =
  let open Ty in
  function
  | VariableDecl (_name, ty) -> lsp_completion_of_type ty
  | TypeAliasDecl { type_; _ } ->
    (match type_ with
    | Some type_ -> lsp_completion_of_type type_
    | None -> Lsp.Completion.Enum)
  | ClassDecl _ -> Lsp.Completion.Class
  | InterfaceDecl _ -> Lsp.Completion.Interface
  | EnumDecl _ -> Lsp.Completion.Enum
  | ModuleDecl _ -> Lsp.Completion.Module

let sort_text_of_rank rank = Some (Printf.sprintf "%020u" rank)

let text_edit ?insert_text name (insert, replace) =
  let newText = Base.Option.value ~default:name insert_text in
  { ServerProt.Response.newText; insert; replace }

let detail_of_ty ~exact_by_default ty =
  let type_ = Ty_printer.string_of_t_single_line ~with_comments:false ~exact_by_default ty in
  let detail = ": " ^ type_ in
  (* [detail] is rendered immediately after the name, with no space.
     We add a [:] so it renders like an annotation *)
  (Some type_, Some detail)

let detail_of_ty_decl ~exact_by_default d =
  let type_ = Ty_printer.string_of_decl_single_line ~with_comments:false ~exact_by_default d in
  let detail =
    (* this is rendered immediately after the name, with no space.
       for most of these, there's nothing to show because the "kind" icon
       captures whether it's a class, enum, interface, etc. *)
    match d with
    | Ty.ClassDecl _ -> None
    | Ty.EnumDecl _ -> None
    | Ty.InterfaceDecl _ -> None
    | Ty.ModuleDecl _ -> None
    | Ty.TypeAliasDecl _ ->
      (* TODO: the "signature" of a type alias arguably includes type params,
         and could include the RHS too like we do for variables. *)
      None
    | Ty.VariableDecl (_, ty) ->
      let (_, detail) = detail_of_ty ~exact_by_default ty in
      detail
  in
  (Some type_, detail)

let autocomplete_create_result_method
    ~method_
    ~options
    ?(rank = 0)
    ?(preselect = false)
    ?documentation
    ?tags
    ~log_info
    (name, edit_locs)
    ty =
  let method_ =
    (* Replace the method's body with just one placeholder statement expression: AUTO332 *)
    method_
    |> Ast_builder.Classes.Methods.with_body
         ~body:
           (Ast_builder.Functions.body
              [Ast_builder.Statements.expression (Ast_builder.Expressions.identifier "AUTO332")]
           )
    |> Ast_builder.Classes.Methods.with_docs ~docs:None
  in
  let insert_text =
    (* Print the node to a string and replace the AUTO332 expression with an LSP snippet placeholder *)
    method_
    |> Js_layout_generator.class_method ~opts:(Code_action_service.layout_options options)
    |> Pretty_printer.print ~source_maps:None ~skip_endline:true
    |> Source.contents
    |> Base.String.substr_replace_first ~pattern:"AUTO332;" ~with_:"$0"
  in
  let labelDetail =
    (* Print just the params and return type to a string and add { ... } *)
    method_
    |> (fun (_, { Flow_ast.Class.Method.value; _ }) -> value)
    |> Js_layout_generator.function_params_and_return
         ~opts:(Code_action_service.layout_options options)
    |> Pretty_printer.print ~source_maps:None ~skip_endline:true
    |> Source.contents
    |> fun params_and_return -> params_and_return ^ "{ … }"
  in
  let kind = Some (lsp_completion_of_type ty) in
  let text_edit = Some (text_edit ~insert_text name edit_locs) in
  let sort_text = sort_text_of_rank rank in
  {
    ServerProt.Response.Completion.kind;
    name;
    labelDetail = Some labelDetail;
    description = None;
    itemDetail = None;
    text_edit;
    additional_text_edits = [];
    sort_text;
    preselect;
    documentation;
    tags;
    log_info;
    insert_text_format = Lsp.Completion.SnippetFormat;
  }

let autocomplete_create_result
    ?insert_text
    ?(rank = 0)
    ?(preselect = false)
    ?documentation
    ?tags
    ?(snippet = false)
    ~exact_by_default
    ~log_info
    (name, edit_locs)
    ty =
  let (itemDetail, labelDetail) = detail_of_ty ~exact_by_default ty in
  let kind = Some (lsp_completion_of_type ty) in
  let text_edit = Some (text_edit ?insert_text name edit_locs) in
  let sort_text = sort_text_of_rank rank in
  let insert_text_format =
    if snippet then
      Lsp.Completion.SnippetFormat
    else
      Lsp.Completion.PlainText
  in
  {
    ServerProt.Response.Completion.kind;
    name;
    labelDetail;
    description = None;
    itemDetail;
    text_edit;
    additional_text_edits = [];
    sort_text;
    preselect;
    documentation;
    tags;
    log_info;
    insert_text_format;
  }

let autocomplete_create_result_decl
    ?insert_text
    ~rank
    ?(preselect = false)
    ?documentation
    ?tags
    ?(snippet = false)
    ~exact_by_default
    ~log_info
    (name, edit_locs)
    d =
  let kind = Some (lsp_completion_of_decl d) in
  let (itemDetail, labelDetail) = detail_of_ty_decl ~exact_by_default d in
  let text_edit = Some (text_edit ?insert_text name edit_locs) in
  let sort_text = sort_text_of_rank rank in
  let insert_text_format =
    if snippet then
      Lsp.Completion.SnippetFormat
    else
      Lsp.Completion.PlainText
  in
  {
    ServerProt.Response.Completion.kind;
    name;
    labelDetail;
    description = None;
    itemDetail;
    text_edit;
    additional_text_edits = [];
    sort_text;
    preselect;
    documentation;
    tags;
    log_info;
    insert_text_format;
  }

let autocomplete_create_result_elt
    ?insert_text
    ?(rank = 0)
    ?preselect
    ?documentation
    ?tags
    ~exact_by_default
    ~log_info
    (name, edit_locs)
    elt =
  match elt with
  | Ty.Type t ->
    autocomplete_create_result
      ?insert_text
      ~rank
      ?preselect
      ?documentation
      ?tags
      ~exact_by_default
      ~log_info
      (name, edit_locs)
      t
  | Ty.Decl d ->
    autocomplete_create_result_decl
      ?insert_text
      ~rank
      ?preselect
      ?documentation
      ?tags
      ~exact_by_default
      ~log_info
      (name, edit_locs)
      d

let ty_normalizer_options =
  {
    Ty_normalizer_env.expand_internal_types = true;
    flag_shadowed_type_params = true;
    preserve_inferred_literal_types = false;
    evaluate_type_destructors = Ty_normalizer_env.EvaluateAll;
    optimize_types = true;
    omit_targ_defaults = false;
    merge_bot_and_any_kinds = true;
    verbose_normalizer = false;
    max_depth = Some 50;
  }

type ac_result = {
  result: ServerProt.Response.Completion.t;
  errors_to_log: string list;
}

type autocomplete_service_result =
  | AcResult of ac_result
  | AcEmpty of string
  | AcFatalError of string

let jsdoc_of_def_loc ~reader ~typed_ast def_loc =
  loc_of_aloc ~reader def_loc
  |> Find_documentation.jsdoc_of_getdef_loc ~current_ast:typed_ast ~reader

let jsdoc_of_member ~reader ~typed_ast info =
  Base.Option.bind info.Ty_members.def_loc ~f:(jsdoc_of_def_loc ~reader ~typed_ast)

let jsdoc_of_loc ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc =
  let open GetDef_js.Get_def_result in
  match GetDef_js.get_def ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc with
  | Def getdef_loc
  | Partial (getdef_loc, _) ->
    Find_documentation.jsdoc_of_getdef_loc ~current_ast:typed_ast ~reader getdef_loc
  | Bad_loc
  | Def_error _ ->
    None

let documentation_and_tags_of_jsdoc jsdoc =
  let docs = Find_documentation.documentation_of_jsdoc jsdoc in
  let tags =
    Base.Option.map (Jsdoc.deprecated jsdoc) ~f:(fun _ -> [Lsp.CompletionItemTag.Deprecated])
  in
  (docs, tags)

let documentation_and_tags_of_member ~reader ~typed_ast info =
  jsdoc_of_member ~reader ~typed_ast info
  |> Base.Option.value_map ~default:(None, None) ~f:documentation_and_tags_of_jsdoc

let documentation_and_tags_of_loc ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc =
  jsdoc_of_loc ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc
  |> Base.Option.value_map ~default:(None, None) ~f:documentation_and_tags_of_jsdoc

let documentation_and_tags_of_def_loc ~reader ~typed_ast def_loc =
  jsdoc_of_def_loc ~reader ~typed_ast def_loc
  |> Base.Option.value_map ~default:(None, None) ~f:documentation_and_tags_of_jsdoc

let members_of_type
    ~reader
    ~exclude_proto_members
    ?(force_instance = false)
    ?(include_interface_members = false)
    ?(exclude_keys = SSet.empty)
    ~tparams_rev
    cx
    file_sig
    typed_ast
    type_ =
  let ty_members =
    Ty_members.extract
      ~force_instance
      ~cx
      ~typed_ast
      ~file_sig
      Type.TypeScheme.{ tparams_rev; type_ }
  in
  let include_valid_member (s, (Ty_members.{ inherited; source; _ } as info)) =
    if
      (exclude_proto_members && inherited)
      || ((not include_interface_members) && source = Ty.Interface)
      || source = Ty.PrimitiveProto "Object"
    then
      None
    else
      let open Reason in
      match s with
      | OrdinaryName "constructor"
      | InternalName _
      | InternalModuleName _ ->
        None
      (* TODO consider making the $-prefixed names internal *)
      | OrdinaryName str when (String.length str >= 1 && str.[0] = '$') || SSet.mem str exclude_keys
        ->
        None
      | OrdinaryName str -> Some (str, info)
  in
  match ty_members with
  | Error error -> fail error
  | Ok Ty_members.{ members; errors; in_idx } ->
    return
      ( members
        |> NameUtils.Map.bindings
        |> Base.List.filter_map ~f:include_valid_member
        |> List.map (fun (name, info) ->
               let (document, tags) = documentation_and_tags_of_member ~reader ~typed_ast info in
               (name, document, tags, info)
           ),
        (match errors with
        | [] -> []
        | _ :: _ -> Printf.sprintf "members_of_type %s" (Debug_js.dump_t cx type_) :: errors),
        in_idx
      )

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

let local_value_identifiers
    ~options ~reader ~cx ~genv ~ac_loc ~file_sig ~ast ~typed_ast ~tparams_rev =
  let scope_info =
    Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:false ast
  in
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
        let scope_vars = scope.Scope.defs in

        (* don't suggest lexically-scoped variables declared after the current location.
           this filtering isn't perfect:

             let foo = /* request here */
                 ^^^
                 def_loc

           since def_loc is the location of the identifier within the declaration statement
           (not the entire statement), we don't filter out foo when declaring foo. *)
        let relevant_scope_vars =
          SMap.filter
            (fun _name { Def.locs; kind; _ } ->
              Bindings.allow_forward_ref kind || Loc.compare (Nel.hd locs) ac_loc < 0)
            scope_vars
        in

        let relevant_scope_vars =
          SMap.map (fun { Def.locs; _ } -> Nel.hd locs) relevant_scope_vars
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
             let (documentation, tags) =
               documentation_and_tags_of_loc ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc
             in
             ((name, documentation, tags), Type.TypeScheme.{ tparams_rev; type_ })
         )
     )
  |> Ty_normalizer.from_schemes ~options:ty_normalizer_options ~genv

(* Roughly collects upper bounds of a type.
 * This logic will be changed or made unnecessary once we have contextual typing *)
let upper_bound_t_of_t ~cx =
  let rec upper_bounds_of_t : ISet.t -> Type.t -> Type.t list =
    let open Base.List.Let_syntax in
    fun seen -> function
      | Type.OpenT (_, id) ->
        (* A given tvar might be reachable from many paths through uses. If
         * we've seen this tvar before, we should not re-add the uses we've
         * already seen. *)
        if ISet.mem id seen then
          []
        else
          let seen = ISet.add id seen in
          let%bind use = Flow_js_utils.possible_uses cx id in
          upper_bounds_of_use_t seen use
      | t -> return t
  and upper_bounds_of_use_t : ISet.t -> Type.use_t -> Type.t list =
   fun seen -> function
    | Type.ReposLowerT (_, _, use) -> upper_bounds_of_use_t seen use
    | Type.UseT (_, t) -> upper_bounds_of_t seen t
    | _ -> []
  in
  fun lb_type ->
    let seen = ISet.empty in
    let reason = TypeUtil.reason_of_t lb_type in
    match upper_bounds_of_t seen lb_type with
    | [] -> Type.MixedT.make reason (Trust.bogus_trust ())
    | [upper_bound] -> upper_bound
    | ub1 :: ub2 :: ubs -> Type.IntersectionT (reason, Type.InterRep.make ub1 ub2 ubs)

let rec literals_of_ty acc ty =
  match ty with
  | Ty.Union (_, t1, t2, ts) -> Base.List.fold_left (t1 :: t2 :: ts) ~f:literals_of_ty ~init:acc
  | Ty.StrLit _
  | Ty.NumLit _
  | Ty.BoolLit _
  | Ty.Null ->
    ty :: acc
  | Ty.Bool _ -> Ty.BoolLit true :: Ty.BoolLit false :: acc
  | _ -> acc

let quote_kind token =
  if token = "" then
    None
  else
    match token.[0] with
    | '\'' -> Some `Single
    | '"' -> Some `Double
    | _ -> None

let autocomplete_literals
    ~prefer_single_quotes ~cx ~genv ~tparams_rev ~edit_locs ~upper_bound ~token =
  let scheme = { Type.TypeScheme.tparams_rev; type_ = upper_bound } in
  let options = ty_normalizer_options in
  let upper_bound_ty =
    Result.value (Ty_normalizer.expand_literal_union ~options ~genv scheme) ~default:Ty.Top
  in
  let exact_by_default = Context.exact_by_default cx in
  let literals = literals_of_ty [] upper_bound_ty in
  Base.List.map literals ~f:(fun ty ->
      let prefer_single_quotes =
        (* If the user already typed a quote, use that kind regardless of the config option *)
        match quote_kind token with
        | Some `Single -> true
        | Some `Double -> false
        | None -> prefer_single_quotes
      in
      let edit_locs =
        (* When completing a string literal, always replace. Inserting is likely to produce
           an invalid string which is probably not what the user wnats. *)
        match quote_kind token with
        | Some _ -> (snd edit_locs, snd edit_locs)
        | None -> edit_locs
      in
      (* TODO: since we're inserting values, we shouldn't really be using the Ty_printer *)
      let name =
        Ty_printer.string_of_t_single_line
          ~prefer_single_quotes
          ~with_comments:false
          ~exact_by_default
          ty
      in
      (* TODO: if we had both the expanded and unexpanded type alias, we'd
         use the unexpanded alias for `ty` and the expanded literal for `name`. *)
      autocomplete_create_result
        ~insert_text:name
        ~rank:0
        ~preselect:true
        ~exact_by_default
        ~log_info:"literal from upper bound"
        (name, edit_locs)
        ty
  )

let src_dir_of_loc ac_loc =
  Loc.source ac_loc |> Base.Option.map ~f:(fun key -> File_key.to_string key |> Filename.dirname)

let flow_text_edit_of_lsp_text_edit { Lsp.TextEdit.range; newText } =
  let loc = Flow_lsp_conversions.lsp_range_to_flow_loc range in
  (loc, newText)

let completion_item_of_autoimport
    ~options ~reader ~src_dir ~ast ~edit_locs { Export_search.name; source; kind } rank =
  match
    Code_action_service.text_edits_of_import ~options ~reader ~src_dir ~ast kind name source
  with
  | None ->
    {
      ServerProt.Response.Completion.kind = Some Lsp.Completion.Variable;
      name;
      labelDetail = None;
      description = None;
      itemDetail = Some "(global)";
      text_edit = Some (text_edit name edit_locs);
      additional_text_edits = [];
      sort_text = sort_text_of_rank rank;
      preselect = false;
      documentation = None;
      tags = None;
      log_info = "global";
      insert_text_format = Lsp.Completion.PlainText;
    }
  | Some { Code_action_service.title; edits; from } ->
    let additional_text_edits = Base.List.map ~f:flow_text_edit_of_lsp_text_edit edits in
    {
      ServerProt.Response.Completion.kind = Some Lsp.Completion.Variable;
      name;
      labelDetail = None;
      description = Some from;
      itemDetail = Some title;
      text_edit = Some (text_edit name edit_locs);
      additional_text_edits;
      sort_text = sort_text_of_rank rank;
      preselect = false;
      documentation = None;
      tags = None;
      log_info = "autoimport";
      insert_text_format = Lsp.Completion.PlainText;
    }

let add_locals ~f locals set =
  Base.List.iter ~f:(fun local -> Base.Hash_set.add set (f local)) locals

let set_of_locals ~f locals =
  let set = Base.Hash_set.create ~size:(Base.List.length locals) (module Base.String) in
  add_locals ~f locals set;
  set

let is_reserved name kind =
  if Export_index.kind_is_value kind then
    Parser_env.is_reserved name || Parser_env.is_strict_reserved name
  else
    Parser_env.is_reserved_type name

let append_completion_items_of_autoimports
    ~options ~reader ~ast ~ac_loc ~locals ~imports_ranked_usage ~edit_locs auto_imports acc =
  let src_dir = src_dir_of_loc ac_loc in
  let sorted_auto_imports =
    if imports_ranked_usage then
      let open Export_search in
      (* sort by score, then base the rank on the ordering so each one has a unique rank *)
      auto_imports
      |> Base.List.sort ~compare:(fun (import_a, score_a) (import_b, score_b) ->
             match Int.compare score_a score_b with
             | 0 -> String.compare import_a.name import_b.name
             | score_diff -> -1 * score_diff
         )
      |> Base.List.mapi ~f:(fun index (autoimport, _score) -> (autoimport, index))
    else
      (* legacy behavior is to not sort and not rank. all imports have the same
         constant rank. *)
      Base.List.map ~f:(fun (autoimport, _score) -> (autoimport, 0)) auto_imports
  in
  Base.List.fold_left
    ~init:acc
    ~f:(fun acc (auto_import, rank) ->
      let { Export_search.name; kind; source = _ } = auto_import in
      if is_reserved name kind || Base.Hash_set.mem locals name then
        (* exclude reserved words and already-defined locals, because they can't be imported
           without aliasing them, which we can't do automatically in autocomplete. for example,
           `import {null} from ...` is invalid; and if we already have `const foo = ...` then
           we can't auto-import another value named `foo`. *)
        acc
      else
        let item =
          completion_item_of_autoimport
            ~options
            ~reader
            ~src_dir
            ~ast
            ~edit_locs
            auto_import
            (200 + rank (* after builtins *))
        in
        item :: acc)
    sorted_auto_imports

let compare_completion_items a b =
  let open ServerProt.Response.Completion in
  let rankCompare =
    match (a.sort_text, b.sort_text) with
    | (Some a, Some b) -> String.compare a b
    | (Some _, None) -> -1
    | (None, Some _) -> 1
    | (None, None) -> 0
  in
  if rankCompare = 0 then
    Base.String.Caseless.compare a.name b.name
  else
    rankCompare

let filter_by_token_and_sort token items =
  let (before, _after) = Autocomplete_sigil.remove token in
  if String.length before = 0 then
    (* if there's nothing to fuzzy match by, just sort regularly *)
    Base.List.sort items ~compare:compare_completion_items
  else
    Base.List.rev_filter_map items ~f:(fun item ->
        let open ServerProt.Response.Completion in
        match Fuzzy_score.fuzzy_score ~pattern:before item.name with
        | None -> None
        | Some score -> Some (score, item)
    )
    |> Base.List.stable_sort ~compare:(fun a b ->
           (* since the list is reversed, we sort backwards *)
           Base.Int.compare (fst a) (fst b)
       )
    |> Base.List.rev_map ~f:snd

(** [filter_by_token_and_sort_rev token rev_items] fuzzy scores [rev_items]
     based on [token], sorts them by score (highest to lowest), then
     re-reverses the list. It is stable when two items have the same score. *)
let filter_by_token_and_sort_rev token items =
  let (before, _after) = Autocomplete_sigil.remove token in
  if String.length before = 0 then
    (* if there's nothing to fuzzy match by, just sort regularly. since the
       list is reversed, we sort backwards. *)
    Base.List.sort items ~compare:(fun a b -> -1 * compare_completion_items a b)
  else
    Base.List.rev_filter_map items ~f:(fun item ->
        let open ServerProt.Response.Completion in
        match Fuzzy_score.fuzzy_score ~pattern:before item.name with
        | None -> None
        | Some score -> Some (score, item)
    )
    |> Base.List.stable_sort ~compare:(fun a b -> Base.Int.compare (fst b) (fst a))
    |> Base.List.rev_map ~f:snd

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
    ~imports_ranked_usage
    ~tparams_rev
    ~edit_locs
    ~token
    ~type_ =
  let open ServerProt.Response.Completion in
  let ac_loc = loc_of_aloc ~reader ac_loc |> Autocomplete_sigil.remove_from_loc in
  let exact_by_default = Context.exact_by_default cx in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig in
  let upper_bound = upper_bound_t_of_t ~cx type_ in
  let prefer_single_quotes = Options.format_single_quotes options in
  let results =
    autocomplete_literals
      ~prefer_single_quotes
      ~cx
      ~genv
      ~tparams_rev
      ~edit_locs
      ~upper_bound
      ~token
  in
  let rank =
    if results = [] then
      0
    else
      1
  in
  let identifiers =
    local_value_identifiers
      ~options
      ~reader
      ~cx
      ~genv
      ~ac_loc
      ~file_sig
      ~typed_ast
      ~ast
      ~tparams_rev
  in
  let (items_rev, errors_to_log) =
    identifiers
    |> List.fold_left
         (fun (items_rev, errors_to_log) ((name, documentation, tags), elt_result) ->
           match elt_result with
           | Ok elt ->
             let result =
               autocomplete_create_result_elt
                 ~insert_text:name
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"local value identifier"
                 (name, edit_locs)
                 elt
             in
             (result :: items_rev, errors_to_log)
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (items_rev, error_to_log :: errors_to_log))
         (results, [])
  in
  (* "this" is legal inside classes and (non-arrow) functions *)
  let items_rev =
    if include_this then
      {
        kind = Some Lsp.Completion.Variable;
        name = "this";
        labelDetail = None;
        description = Some "this";
        itemDetail = Some "this";
        text_edit = Some (text_edit "this" edit_locs);
        additional_text_edits = [];
        sort_text = sort_text_of_rank rank;
        preselect = false;
        documentation = None;
        tags = None;
        log_info = "this";
        insert_text_format = Lsp.Completion.PlainText;
      }
      :: items_rev
    else
      items_rev
  in
  (* "super" is legal inside classes *)
  let items_rev =
    if include_super then
      {
        kind = Some Lsp.Completion.Variable;
        name = "super";
        labelDetail = None;
        description = Some "super";
        itemDetail = Some "super";
        text_edit = Some (text_edit "super" edit_locs);
        additional_text_edits = [];
        sort_text = sort_text_of_rank rank;
        preselect = false;
        documentation = None;
        tags = None;
        log_info = "super";
        insert_text_format = Lsp.Completion.PlainText;
      }
      :: items_rev
    else
      items_rev
  in
  let (items_rev, is_incomplete, sorted) =
    if imports then
      let (before, _after) = Autocomplete_sigil.remove token in
      if before = "" then
        (* for empty autocomplete requests (hitting ctrl-space without typing anything),
           don't include any autoimport results, but do set `is_incomplete` so that
           it queries again when you type something. *)
        (items_rev, true, false)
      else
        let locals = set_of_locals ~f:(fun ((name, _docs, _tags), _ty) -> name) identifiers in
        let { Export_search.results = auto_imports; is_incomplete } =
          Export_search.search_values
            ~options:default_autoimport_options
            before
            env.ServerEnv.exports
        in
        let items_rev =
          if imports_ranked_usage then
            (* to maintain the order of the autoimports, we sort the non-imports
               here, and then don't sort the whole list later. *)
            filter_by_token_and_sort_rev token items_rev
          else
            items_rev
        in
        let items_rev =
          append_completion_items_of_autoimports
            ~options
            ~reader
            ~ast
            ~ac_loc
            ~locals
            ~imports_ranked_usage
            ~edit_locs
            auto_imports
            items_rev
        in
        let items_rev =
          if imports_ranked_usage then
            (* when ranked imports are enabled, don't fuzzy score them *)
            items_rev
          else
            filter_by_token_and_sort_rev token items_rev
        in
        (items_rev, is_incomplete, true)
    else
      (* if autoimports are not enabled, then we have all the results *)
      (items_rev, false, false)
  in
  let items_rev =
    if not sorted then
      filter_by_token_and_sort_rev token items_rev
    else
      items_rev
  in
  let result = { ServerProt.Response.Completion.items = List.rev items_rev; is_incomplete } in
  { result; errors_to_log }

let exports_of_module_ty
    ~edit_locs ~exact_by_default ~documentation_and_tags_of_module_member ~kind ?filter_name =
  let open ServerProt.Response.Completion in
  let open Ty in
  let is_kind export_kind = kind = `Either || export_kind = `Either || export_kind = kind in
  let filter_name name =
    match name with
    | Reason.InternalName _
    | Reason.InternalModuleName _ ->
      (* don't show internal names in autocomplete *)
      false
    | Reason.OrdinaryName name ->
      (match filter_name with
      | Some filter_name -> filter_name name
      | None -> true)
  in
  let is_ok export_kind name = is_kind export_kind && filter_name name in
  function
  | Decl (ModuleDecl { exports; _ }) ->
    Base.List.filter_map
      ~f:(function
        | TypeAliasDecl { name = { Ty.sym_name; sym_def_loc; _ }; _ } as d when is_ok `Type sym_name
          ->
          let sym_name = Reason.display_string_of_name sym_name in
          let (documentation, tags) = documentation_and_tags_of_module_member sym_def_loc in
          Some
            (autocomplete_create_result_decl
               ~rank:0
               ?documentation
               ?tags
               ~exact_by_default
               ~log_info:"qualified type alias"
               (sym_name, edit_locs)
               d
            )
        | InterfaceDecl ({ Ty.sym_name; sym_def_loc; _ }, _) as d when is_ok `Type sym_name ->
          let sym_name = Reason.display_string_of_name sym_name in
          let (documentation, tags) = documentation_and_tags_of_module_member sym_def_loc in
          Some
            (autocomplete_create_result_decl
               ~rank:0
               ?documentation
               ?tags
               ~exact_by_default
               ~log_info:"qualified interface"
               (sym_name, edit_locs)
               d
            )
        | ClassDecl ({ Ty.sym_name; sym_def_loc; _ }, _) as d when is_ok `Either sym_name ->
          let sym_name = Reason.display_string_of_name sym_name in
          let (documentation, tags) = documentation_and_tags_of_module_member sym_def_loc in
          Some
            (autocomplete_create_result_decl
               ~rank:0
               ?documentation
               ?tags
               ~exact_by_default
               ~log_info:"qualified class"
               (sym_name, edit_locs)
               d
            )
        | EnumDecl { Ty.sym_name; sym_def_loc; _ } as d when is_ok `Either sym_name ->
          let sym_name = Reason.display_string_of_name sym_name in
          let (documentation, tags) = documentation_and_tags_of_module_member sym_def_loc in
          Some
            (autocomplete_create_result_decl
               ~rank:0
               ?documentation
               ?tags
               ~exact_by_default
               ~log_info:"qualified enum"
               (sym_name, edit_locs)
               d
            )
        | VariableDecl (name, _) as d when is_ok `Value name ->
          let name = Reason.display_string_of_name name in
          Some
            (autocomplete_create_result_decl
               ~rank:0
               ~exact_by_default
               ~log_info:"qualified variable"
               (name, edit_locs)
               d
            )
        | _ -> None)
      exports
    |> Base.List.sort ~compare:(fun { name = a; _ } { name = b; _ } -> String.compare a b)
    |> Base.List.mapi ~f:(fun i r -> { r with sort_text = sort_text_of_rank i })
  | _ -> []

(* TODO(vijayramamurthy) think about how to break this file down into smaller modules *)
(* NOTE: excludes classes, because we'll get those from local_value_identifiers *)
class local_type_identifiers_searcher =
  object (this)
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = x

    method on_type_annot x = x

    val mutable rev_ids = []

    method rev_ids = rev_ids

    method add_id id = rev_ids <- id :: rev_ids

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
          | ImportNamespaceSpecifier _ -> ( (* namespaces can't be types *) )
          );
      x
  end

let make_builtin_type ~edit_locs name =
  {
    ServerProt.Response.Completion.kind = Some Lsp.Completion.Variable;
    name;
    labelDetail = None;
    description = None;
    itemDetail = Some name;
    text_edit = Some (text_edit name edit_locs);
    additional_text_edits = [];
    sort_text = sort_text_of_rank 100 (* after local results *);
    preselect = false;
    documentation = None;
    tags = None;
    log_info = "builtin type";
    insert_text_format = Lsp.Completion.PlainText;
  }

let builtin_types =
  [
    "any";
    "boolean";
    "empty";
    "false";
    "mixed";
    "null";
    "number";
    "bigint";
    "string";
    "true";
    "void";
    "symbol";
  ]

let make_utility_type ~edit_locs name =
  {
    ServerProt.Response.Completion.kind = Some Lsp.Completion.Function;
    name;
    labelDetail = None;
    description = None;
    itemDetail = Some name;
    text_edit = Some (text_edit name edit_locs);
    additional_text_edits = [];
    sort_text = sort_text_of_rank 300 (* after autoimports/globals *);
    preselect = false;
    documentation = None;
    tags = None;
    log_info = "builtin type";
    insert_text_format = Lsp.Completion.PlainText;
  }

let utility_types =
  [
    "Class";
    "$Call";
    "$CharSet";
    "$Diff";
    "$ElementType";
    "$Exact";
    "$Exports";
    "$KeyMirror";
    "$Keys";
    "$NonMaybeType";
    "$ObjMap";
    "$ObjMapi";
    "$PropertyType";
    "$ReadOnly";
    "$Rest";
    "$Shape";
    "$TupleMap";
    "$Values";
  ]

let make_type_param ~edit_locs { Type.name; _ } =
  let name = Subst_name.string_of_subst_name name in
  {
    ServerProt.Response.Completion.kind = Some Lsp.Completion.TypeParameter;
    name;
    labelDetail = None;
    description = None;
    itemDetail = Some name;
    text_edit = Some (text_edit name edit_locs);
    additional_text_edits = [];
    sort_text = sort_text_of_rank 0;
    preselect = false;
    documentation = None;
    tags = None;
    log_info = "unqualified type parameter";
    insert_text_format = Lsp.Completion.PlainText;
  }

let local_type_identifiers ~typed_ast ~cx ~file_sig =
  let search = new local_type_identifiers_searcher in
  Stdlib.ignore (search#program typed_ast);
  search#rev_ids
  |> Base.List.rev_map ~f:(fun ((loc, t), Flow_ast.Identifier.{ name; _ }) -> ((name, loc), t))
  |> Ty_normalizer.from_types
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

let autocomplete_unqualified_type
    ~env
    ~options
    ~reader
    ~cx
    ~imports
    ~imports_ranked_usage
    ~tparams_rev
    ~file_sig
    ~ac_loc
    ~ast
    ~typed_ast
    ~edit_locs
    ~token =
  let ac_loc = loc_of_aloc ~reader ac_loc |> Autocomplete_sigil.remove_from_loc in
  let exact_by_default = Context.exact_by_default cx in
  let items_rev =
    []
    |> Base.List.rev_map_append builtin_types ~f:(make_builtin_type ~edit_locs)
    |> Base.List.rev_map_append utility_types ~f:(make_utility_type ~edit_locs)
    |> Base.List.rev_map_append tparams_rev ~f:(make_type_param ~edit_locs)
  in
  let type_identifiers = local_type_identifiers ~typed_ast ~cx ~file_sig in
  let (items_rev, errors_to_log) =
    type_identifiers
    |> List.fold_left
         (fun (items_rev, errors_to_log) ((name, aloc), ty_result) ->
           let (documentation, tags) =
             loc_of_aloc ~reader aloc
             |> documentation_and_tags_of_loc ~options ~reader ~cx ~file_sig ~ast ~typed_ast
           in
           match ty_result with
           | Ok elt ->
             let result =
               autocomplete_create_result_elt
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"unqualified type: local type identifier"
                 (name, edit_locs)
                 elt
             in
             (result :: items_rev, errors_to_log)
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (items_rev, error_to_log :: errors_to_log))
         (items_rev, [])
  in
  let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig in
  let value_identifiers =
    local_value_identifiers
      ~options
      ~ast
      ~typed_ast
      ~reader
      ~genv
      ~ac_loc
      ~tparams_rev
      ~cx
      ~file_sig
  in
  (* The value-level identifiers we suggest in type autocompletion:
      - classes
      - enums
      - modules (followed by a dot) *)
  let (items_rev, errors_to_log) =
    value_identifiers
    |> List.fold_left
         (fun (items_rev, errors_to_log) ((name, documentation, tags), ty_res) ->
           match ty_res with
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (items_rev, error_to_log :: errors_to_log)
           | Ok (Ty.Decl (Ty.ClassDecl _ | Ty.EnumDecl _) as elt) ->
             let result =
               autocomplete_create_result_elt
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"unqualified type: class or enum"
                 (name, edit_locs)
                 elt
             in
             (result :: items_rev, errors_to_log)
           | Ok elt
             when exports_of_module_ty
                    ~edit_locs
                    ~exact_by_default
                    ~documentation_and_tags_of_module_member:(fun _ -> (None, None))
                    ~kind:`Type
                    elt
                  <> [] ->
             let result =
               autocomplete_create_result_elt
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"unqualified type -> qualified type"
                 (name, edit_locs)
                 elt
                 ~insert_text:(name ^ ".")
             in
             (result :: items_rev, errors_to_log)
           | Ok _ -> (items_rev, errors_to_log))
         (items_rev, errors_to_log)
  in
  let items_rev = filter_by_token_and_sort_rev token items_rev in
  let (items_rev, is_incomplete) =
    if imports then
      let locals =
        let set = set_of_locals ~f:(fun ((name, _aloc), _ty) -> name) type_identifiers in
        add_locals ~f:(fun ((name, _docs, _tags), _ty) -> name) value_identifiers set;
        set
      in
      let { Export_search.results = auto_imports; is_incomplete } =
        let (before, _after) = Autocomplete_sigil.remove token in
        Export_search.search_types ~options:default_autoimport_options before env.ServerEnv.exports
      in
      let items_rev =
        append_completion_items_of_autoimports
          ~options
          ~reader
          ~ast
          ~ac_loc
          ~locals
          ~imports_ranked_usage
          ~edit_locs
          auto_imports
          items_rev
      in
      (items_rev, is_incomplete)
    else
      (items_rev, false)
  in
  let result = { ServerProt.Response.Completion.items = Base.List.rev items_rev; is_incomplete } in
  { result; errors_to_log }

(** If the token is a string literal, then the end of the token may be inaccurate
    due to parse errors. See tests/autocomplete/bracket_syntax_3.js for example. *)
let fix_loc_of_string_token token loc =
  match quote_kind token with
  | Some _ -> { loc with Loc._end = loc.Loc.start }
  | None -> loc

let fix_locs_of_string_token token (insert_loc, replace_loc) =
  let insert_loc = fix_loc_of_string_token token insert_loc in
  let replace_loc = fix_loc_of_string_token token replace_loc in
  (insert_loc, replace_loc)

let layout_options options token =
  let opts = Code_action_service.layout_options options in
  match quote_kind token with
  | Some `Single -> { opts with Js_layout_generator.single_quotes = true }
  | Some `Double -> { opts with Js_layout_generator.single_quotes = false }
  | None -> opts

let print_expression ~options ~token expression =
  expression
  |> Js_layout_generator.expression ~opts:(layout_options options token)
  |> Pretty_printer.print ~source_maps:None ~skip_endline:true
  |> Source.contents

let autocomplete_member
    ~env
    ~reader
    ~options
    ~cx
    ~file_sig
    ~ast
    ~typed_ast
    ~imports
    ~imports_ranked_usage
    ~edit_locs
    ~token
    this
    in_optional_chain
    ac_aloc
    ~tparams_rev
    ~bracket_syntax
    ~member_loc
    ~is_type_annotation
    ~force_instance =
  let edit_locs = fix_locs_of_string_token token edit_locs in
  let exact_by_default = Context.exact_by_default cx in
  match
    members_of_type
      ~reader
      ~exclude_proto_members:false
      ~force_instance
      cx
      file_sig
      typed_ast
      this
      ~tparams_rev
  with
  | Error err -> AcFatalError err
  | Ok (mems, errors_to_log, in_idx) ->
    let items =
      mems
      |> Base.List.map
           ~f:(fun (name, documentation, tags, Ty_members.{ ty; source; from_nullable; _ }) ->
             let rank =
               match source with
               | Ty.PrimitiveProto _ -> 1
               | _ -> 0
             in
             let opt_chain_ty =
               Ty_utils.simplify_type ~merge_kinds:true (Ty.Union (false, Ty.Void, ty, []))
             in
             let name_as_indexer =
               lazy
                 (let expression =
                    match Base.String.chop_prefix ~prefix:"@@" name with
                    | None -> Ast_builder.Expressions.literal (Ast_builder.Literals.string name)
                    | Some symbol ->
                      Ast_builder.Expressions.member_expression_ident_by_name
                        (Ast_builder.Expressions.identifier "Symbol")
                        symbol
                  in
                  print_expression ~options ~token expression
                 )
             in
             let name_is_valid_identifier = Parser_flow.string_is_valid_identifier_name name in
             let edit_loc_of_member_loc member_loc =
               if Loc.(member_loc.start.line = member_loc._end.line) then
                 Autocomplete_sigil.remove_from_loc member_loc
               else
                 Loc.{ member_loc with _end = member_loc.start }
             in
             match
               ( from_nullable,
                 in_optional_chain,
                 in_idx,
                 bracket_syntax,
                 member_loc,
                 name_is_valid_identifier
               )
             with
             (* TODO: only complete obj destructuring pattern when name is valid identifier *)
             | (_, _, _, _, None, _)
             | (false, _, _, None, _, true)
             | (_, true, _, None, _, true)
             | (_, _, true, None, _, true) ->
               let ty =
                 if from_nullable && in_optional_chain then
                   opt_chain_ty
                 else
                   ty
               in
               autocomplete_create_result
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"member"
                 (name, edit_locs)
                 ty
             | (false, _, _, Some _, _, _)
             | (_, true, _, Some _, _, _)
             | (_, _, true, Some _, _, _) ->
               let insert_text = Lazy.force name_as_indexer in
               autocomplete_create_result
                 ~insert_text
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"bracket syntax member"
                 (insert_text, edit_locs)
                 ty
             | (false, _, _, None, Some member_loc, false)
             | (_, true, _, None, Some member_loc, false)
             | (_, _, true, None, Some member_loc, false) ->
               let insert_text = Printf.sprintf "[%s]" (Lazy.force name_as_indexer) in
               let edit_loc = edit_loc_of_member_loc member_loc in
               autocomplete_create_result
                 ~insert_text
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"dot-member switched to bracket-syntax member"
                 (insert_text, (edit_loc, edit_loc))
                 ty
             | (true, false, false, _, Some member_loc, _) ->
               let opt_chain_name =
                 match (bracket_syntax, name_is_valid_identifier) with
                 | (None, true) -> Printf.sprintf "?.%s" name
                 | (Some _, _)
                 | (_, false) ->
                   Printf.sprintf "?.[%s]" (Lazy.force name_as_indexer)
               in
               let edit_loc = Autocomplete_sigil.remove_from_loc member_loc in
               autocomplete_create_result
                 ~insert_text:opt_chain_name
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"start optional chain"
                 (opt_chain_name, (edit_loc, edit_loc))
                 opt_chain_ty
         )
    in
    (match bracket_syntax with
    | None ->
      let items = filter_by_token_and_sort token items in
      let result = { ServerProt.Response.Completion.items; is_incomplete = false } in
      AcResult { result; errors_to_log }
    | Some Autocomplete_js.{ include_this; include_super; type_; _ } ->
      let {
        result = { ServerProt.Response.Completion.items = id_items; is_incomplete };
        errors_to_log = id_errors_to_log;
      } =
        if is_type_annotation then
          autocomplete_unqualified_type
            ~env
            ~options
            ~reader
            ~cx
            ~imports
            ~imports_ranked_usage
            ~tparams_rev
            ~ac_loc:ac_aloc
            ~ast
            ~typed_ast
            ~file_sig
            ~edit_locs
            ~token
        else
          autocomplete_id
            ~env
            ~options
            ~reader
            ~cx
            ~ac_loc:ac_aloc
            ~file_sig
            ~ast
            ~typed_ast
            ~include_super
            ~include_this
            ~imports
            ~imports_ranked_usage
            ~tparams_rev
            ~edit_locs
            ~token
            ~type_
      in
      let items =
        Base.List.rev items
        |> Base.List.rev_append id_items
        |> filter_by_token_and_sort_rev token
        |> Base.List.rev
      in
      let result = { ServerProt.Response.Completion.items; is_incomplete } in
      let errors_to_log = errors_to_log @ id_errors_to_log in
      AcResult { result; errors_to_log })

let rec binds_react = function
  | File_sig.With_ALoc.BindIdent (_, name) -> name = "React"
  | File_sig.With_ALoc.BindNamed bindings ->
    Base.List.exists ~f:(fun (_remote, local) -> binds_react local) bindings

(** Determines whether to autoimport React when autocompleting a JSX element.

    By default JSX transforms into [React.createElement] which needs [React] to be
    in scope. However, when [react.runtime=automatic] is set in [.flowconfig], this
    happens behind the scenes.

    We use a bit of a heuristic here. If [React] is imported or required anywhere
    in the file, we won't autoimport it, even if it's not in scope for the JSX
    component being completed. This is because imports are top-level so we'd cause
    potential shadowing issues elsewhere in the file. *)
let should_autoimport_react ~options ~imports ~file_sig =
  if imports then
    match Options.react_runtime options with
    | Options.ReactRuntimeAutomatic -> false
    | Options.ReactRuntimeClassic ->
      let open File_sig.With_ALoc in
      let requires_react =
        Base.List.exists
          ~f:(function
            | Require { bindings = Some bindings; _ } -> binds_react bindings
            | Import { ns = Some (_, "React"); _ } -> true
            | Import { named; _ } ->
              SMap.exists
                (fun _remote local -> SMap.exists (fun name _locs -> name = "React") local)
                named
            | Require { bindings = None; _ }
            | Import0 _
            | ImportDynamic _
            | ExportFrom _ ->
              false)
          file_sig.module_sig.requires
      in
      not requires_react
  else
    false

let autocomplete_jsx_element
    ~env
    ~options
    ~reader
    ~cx
    ~ac_loc
    ~file_sig
    ~ast
    ~typed_ast
    ~imports
    ~imports_ranked_usage
    ~tparams_rev
    ~edit_locs
    ~token
    ~type_ =
  let ({ result; errors_to_log } as results) =
    autocomplete_id
      ~env
      ~options
      ~reader
      ~cx
      ~ac_loc
      ~file_sig
      ~ast
      ~typed_ast
      ~include_super:false
      ~include_this:false
      ~imports
      ~imports_ranked_usage
      ~tparams_rev
      ~edit_locs
      ~token
      ~type_
  in
  if should_autoimport_react ~options ~imports ~file_sig then
    let open ServerProt.Response.Completion in
    let import_edit =
      let src_dir = src_dir_of_loc (loc_of_aloc ~reader ac_loc) in
      let kind = Export_index.Namespace in
      let name = "React" in
      (* TODO: make this configurable between React and react *)
      let source = Export_index.Builtin "react" in
      Code_action_service.text_edits_of_import ~options ~reader ~src_dir ~ast kind name source
    in
    match import_edit with
    | None -> AcResult results
    | Some { Code_action_service.title = _; edits; from = _ } ->
      let edits = Base.List.map ~f:flow_text_edit_of_lsp_text_edit edits in
      let { items; _ } = result in
      let items =
        Base.List.map
          ~f:(fun item ->
            let { additional_text_edits; _ } = item in
            let additional_text_edits = additional_text_edits @ edits in
            { item with additional_text_edits })
          items
      in
      let result = { result with items } in
      AcResult { result; errors_to_log }
  else
    AcResult results

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx_attribute
    ~reader
    ~used_attr_names
    ~has_value
    ~tparams_rev
    ~edit_locs
    ~token
    cx
    file_sig
    typed_ast
    cls
    attribute =
  let open Flow_js in
  let reason =
    let (aloc, name) = attribute in
    Reason.mk_reason (Reason.RCustom name) aloc
  in
  let props_object =
    Tvar.mk_where cx reason (fun tvar ->
        let use_op = Type.Op Type.UnknownUse in
        flow cx (cls, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar))
    )
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
      ~tparams_rev
  in
  match mems_result with
  | Error err -> AcFatalError err
  | Ok (mems, errors_to_log, _) ->
    let items =
      mems
      |> Base.List.map ~f:(fun (name, documentation, tags, Ty_members.{ ty; _ }) ->
             let insert_text =
               if has_value then
                 name
               else
                 name ^ "="
             in
             autocomplete_create_result
               ~insert_text
               ?documentation
               ?tags
               ~exact_by_default
               ~log_info:"jsx attribute"
               (name, edit_locs)
               ty
         )
    in
    let items = filter_by_token_and_sort token items in
    let result = { ServerProt.Response.Completion.items; is_incomplete = false } in
    AcResult { result; errors_to_log }

let autocomplete_module_exports
    ~reader ~cx ~file_sig ~typed_ast ~tparams_rev ~edit_locs ~token ~kind ?filter_name module_type =
  let scheme = Type.TypeScheme.{ tparams_rev; type_ = module_type } in
  let exact_by_default = Context.exact_by_default cx in
  let module_ty_res =
    Ty_normalizer.from_scheme
      ~options:ty_normalizer_options
      ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)
      scheme
  in
  let documentation_and_tags_of_module_member =
    documentation_and_tags_of_def_loc ~reader ~typed_ast
  in
  let (items, errors_to_log) =
    match module_ty_res with
    | Error err -> ([], [Ty_normalizer.error_to_string err])
    | Ok module_ty ->
      ( exports_of_module_ty
          ~edit_locs
          ~exact_by_default
          ~documentation_and_tags_of_module_member
          ~kind
          ?filter_name
          module_ty,
        []
      )
  in
  let items = filter_by_token_and_sort token items in
  AcResult
    { result = { ServerProt.Response.Completion.items; is_incomplete = false }; errors_to_log }

let unused_super_methods
    ~reader
    ~options
    ~cx
    ~file_sig
    ~typed_ast
    ~edit_locs
    ~tparams_rev
    ~exclude_keys
    enclosing_class_t =
  let open Base.Result.Let_syntax in
  let%bind (mems, errors_to_log, _) =
    members_of_type
      ~reader
      ~exclude_proto_members:false
      ~force_instance:true
      ~include_interface_members:true
      ~exclude_keys
      cx
      file_sig
      typed_ast
      enclosing_class_t
      ~tparams_rev
  in
  let items =
    mems
    |> Base.List.filter_map ~f:(fun (name, documentation, tags, { Ty_members.ty; def_loc; _ }) ->
           let open Base.Option in
           (* Find the AST node for member we want to override *)
           def_loc >>| loc_of_aloc ~reader >>= Find_method.find reader >>| fun method_ ->
           autocomplete_create_result_method
             ~method_
             ~options
             ?documentation
             ?tags
             ~log_info:"class key"
             (name, edit_locs)
             ty
       )
  in
  return (items, errors_to_log)

let autocomplete_class_key
    ~reader ~options ~cx ~file_sig ~typed_ast ~token ~edit_locs ~tparams_rev enclosing_class_t =
  match enclosing_class_t with
  | Some enclosing_class_t -> begin
    match
      let open Base.Result.Let_syntax in
      let%bind (existing_members, _, _) =
        members_of_type
          ~reader
          ~exclude_proto_members:true
          ~force_instance:true
          cx
          file_sig
          typed_ast
          enclosing_class_t
          ~tparams_rev
      in
      let exclude_keys =
        existing_members |> Base.List.map ~f:(fun (name, _, _, _) -> name) |> SSet.of_list
      in
      let%bind (items, errors_to_log) =
        unused_super_methods
          ~reader
          ~options
          ~cx
          ~file_sig
          ~typed_ast
          ~edit_locs
          ~tparams_rev
          ~exclude_keys
          enclosing_class_t
      in
      let items = filter_by_token_and_sort token items in
      return (items, errors_to_log)
    with
    | Error err -> AcFatalError err
    | Ok (items, errors_to_log) ->
      AcResult
        { result = { ServerProt.Response.Completion.items; is_incomplete = false }; errors_to_log }
  end
  | None ->
    AcResult
      {
        result = { ServerProt.Response.Completion.items = []; is_incomplete = false };
        errors_to_log = [];
      }

let autocomplete_object_key
    ~reader
    ~options
    ~cx
    ~file_sig
    ~typed_ast
    ~edit_locs
    ~token
    ~used_keys
    ~spreads
    obj_type
    ~tparams_rev =
  let edit_locs = fix_locs_of_string_token token edit_locs in
  let (insert_loc, _replace_loc) = edit_locs in
  let exact_by_default = Context.exact_by_default cx in
  let exclude_keys =
    Base.List.fold ~init:used_keys spreads ~f:(fun acc (spread_loc, spread_type) ->
        (* Only exclude the keys of spreads after our prop. If the spread is before our
           insertion we do not use it to exclude properties from our autcomplete
           suggestions. It is a common pattern to use a spread to set default values and
           then override some of those defaults. *)
        if Loc.compare spread_loc insert_loc < 0 then
          acc
        else
          let spread_keys =
            match
              members_of_type
                ~reader
                ~exclude_proto_members:true
                cx
                file_sig
                typed_ast
                spread_type
                ~tparams_rev
            with
            | Error _ -> SSet.empty
            | Ok (members, _, _) ->
              Base.List.map members ~f:(fun (name, _, _, _) -> name) |> SSet.of_list
          in
          SSet.union acc spread_keys
    )
  in
  let upper_bound = upper_bound_t_of_t ~cx obj_type in
  match
    let open Base.Result.Let_syntax in
    let%bind (methods, methods_errors_to_log) =
      unused_super_methods
        ~reader
        ~options
        ~cx
        ~file_sig
        ~typed_ast
        ~edit_locs
        ~tparams_rev
        ~exclude_keys
        upper_bound
    in
    let%bind (mems, mems_errors_to_log, _) =
      members_of_type
        ~reader
        ~exclude_keys
        ~exclude_proto_members:true
        cx
        file_sig
        typed_ast
        upper_bound
        ~tparams_rev
    in
    let items =
      mems
      |> Base.List.map ~f:(fun (name, documentation, tags, Ty_members.{ ty; _ }) ->
             let rank = 0 in
             let name_is_valid_identifier = Parser_flow.string_is_valid_identifier_name name in
             if name_is_valid_identifier then
               autocomplete_create_result
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"object key"
                 (name, edit_locs)
                 ty
             else
               let insert_text =
                 let expression =
                   match Base.String.chop_prefix ~prefix:"@@" name with
                   | None -> Ast_builder.Expressions.literal (Ast_builder.Literals.string name)
                   | Some symbol ->
                     Ast_builder.Expressions.member_expression_ident_by_name
                       (Ast_builder.Expressions.identifier "Symbol")
                       symbol
                 in
                 print_expression ~options ~token expression
               in
               autocomplete_create_result
                 ~insert_text
                 ~rank
                 ?documentation
                 ?tags
                 ~exact_by_default
                 ~log_info:"bracket syntax object key"
                 (insert_text, edit_locs)
                 ty
         )
    in
    return (items @ methods, methods_errors_to_log @ mems_errors_to_log)
  with
  | Error err -> AcFatalError err
  | Ok (items, errors_to_log) ->
    let items = filter_by_token_and_sort token items in
    AcResult
      { result = { ServerProt.Response.Completion.items; is_incomplete = false }; errors_to_log }

(** Used for logging to classify the kind of completion request *)
let string_of_autocomplete_type ac_type =
  let open Autocomplete_js in
  match ac_type with
  | Ac_ignored -> "Empty"
  | Ac_binding -> "Empty"
  | Ac_comment -> "Empty"
  | Ac_enum -> "Acenum"
  | Ac_module -> "Acmodule"
  | Ac_type -> "Actype"
  | Ac_jsx_text -> "Empty"
  | Ac_id _ -> "Acid"
  | Ac_class_key _ -> "Ac_class_key"
  | Ac_import_specifier _ -> "Ac_import_specifier"
  | Ac_key _ -> "Ackey"
  | Ac_literal _ -> "Acliteral"
  | Ac_qualified_type _ -> "Acqualifiedtype"
  | Ac_member _ -> "Acmem"
  | Ac_jsx_element _ -> "Ac_jsx_element"
  | Ac_jsx_attribute _ -> "Acjsx"

let autocomplete_get_results
    ~env
    ~options
    ~reader
    ~cx
    ~file_sig
    ~ast
    ~typed_ast
    ~imports
    ~imports_ranked_usage
    trigger_character
    cursor =
  let file_sig = File_sig.abstractify_locs file_sig in
  let open Autocomplete_js in
  match process_location ~trigger_character ~cursor ~typed_ast with
  | None ->
    let result = { ServerProt.Response.Completion.items = []; is_incomplete = false } in
    ( None,
      None,
      "None",
      AcResult { result; errors_to_log = ["Autocomplete token not found in AST"] }
    )
  | Some { tparams_rev; ac_loc; token; autocomplete_type } ->
    (* say you're completing `foo|Baz`. the token is "fooBaz" and ac_loc points at
       "fooAUTO332Baz". if the suggestion is "fooBar", the user can choose to insert
       into the token, yielding "fooBarBaz", or they can choose to replace the token,
       yielding "fooBar". *)
    let edit_locs =
      let replace_loc = loc_of_aloc ~reader ac_loc |> Autocomplete_sigil.remove_from_loc in
      (* invariant: the cursor must be inside ac_loc *)
      let insert_loc = Loc.btwn replace_loc cursor in
      (insert_loc, replace_loc)
    in
    let result =
      match autocomplete_type with
      | Ac_binding -> AcEmpty "Binding"
      | Ac_ignored -> AcEmpty "Ignored"
      | Ac_comment -> AcEmpty "Comment"
      | Ac_jsx_text -> AcEmpty "JSXText"
      | Ac_class_key { enclosing_class_t } ->
        autocomplete_class_key
          ~reader
          ~options
          ~cx
          ~file_sig
          ~typed_ast
          ~token
          ~edit_locs
          ~tparams_rev
          enclosing_class_t
      | Ac_module ->
        (* TODO: complete module names *)
        AcEmpty "Module"
      | Ac_import_specifier { module_type; used_keys; is_type } ->
        (* TODO: is_type should be an import_kind:
           ImportType = `Type
           ImportTypeof = `Value
           ImportValue = `Either *)
        let kind =
          if is_type then
            `Type
          else
            `Either
        in
        let filter_name name = not (SSet.mem name used_keys) in
        autocomplete_module_exports
          ~reader
          ~cx
          ~file_sig
          ~typed_ast
          ~tparams_rev
          ~edit_locs
          ~token
          ~kind
          ~filter_name
          module_type
      | Ac_enum -> AcEmpty "Enum"
      | Ac_key { obj_type; used_keys; spreads } ->
        autocomplete_object_key
          ~reader
          ~options
          ~cx
          ~file_sig
          ~typed_ast
          ~edit_locs
          ~token
          ~used_keys
          ~spreads
          obj_type
          ~tparams_rev
      | Ac_literal { lit_type } ->
        let genv =
          Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig
        in
        let upper_bound = upper_bound_t_of_t ~cx lit_type in
        let prefer_single_quotes = Options.format_single_quotes options in
        let items =
          autocomplete_literals
            ~prefer_single_quotes
            ~cx
            ~genv
            ~tparams_rev
            ~edit_locs
            ~upper_bound
            ~token
          |> filter_by_token_and_sort token
        in
        let result = { ServerProt.Response.Completion.items; is_incomplete = false } in
        AcResult { result; errors_to_log = [] }
      | Ac_id { include_super; include_this; type_; enclosing_class_t } ->
        let result_id =
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
            ~imports_ranked_usage
            ~tparams_rev
            ~edit_locs
            ~token
            ~type_
        in
        (match enclosing_class_t with
        | Some t ->
          let result_member =
            autocomplete_member
              ~env
              ~reader
              ~options
              ~cx
              ~file_sig
              ~ast
              ~typed_ast
              ~imports
              ~imports_ranked_usage
              ~edit_locs
              ~token
              t
              false
              ac_loc
              ~tparams_rev
              ~bracket_syntax:None
              ~member_loc:None
              ~is_type_annotation:false
              ~force_instance:true
          in
          (match result_member with
          | AcFatalError _ as err -> err
          | AcEmpty _ -> AcResult result_id
          | AcResult result_member ->
            let open ServerProt.Response.Completion in
            let rev_items =
              Base.List.fold
                ~init:(List.rev result_id.result.items)
                ~f:(fun acc item ->
                  let name = "this." ^ item.name in
                  {
                    item with
                    name;
                    text_edit = Some (text_edit ?insert_text:(Some name) name edit_locs);
                  }
                  :: acc)
                result_member.result.items
              |> filter_by_token_and_sort_rev token
            in
            AcResult
              {
                result =
                  {
                    ServerProt.Response.Completion.items = List.rev rev_items;
                    is_incomplete =
                      result_id.result.is_incomplete || result_member.result.is_incomplete;
                  };
                errors_to_log = result_id.errors_to_log @ result_member.errors_to_log;
              })
        | _ -> AcResult result_id)
      | Ac_member { obj_type; in_optional_chain; bracket_syntax; member_loc; is_type_annotation } ->
        autocomplete_member
          ~env
          ~reader
          ~options
          ~cx
          ~file_sig
          ~ast
          ~typed_ast
          ~imports
          ~imports_ranked_usage
          ~edit_locs
          ~token
          obj_type
          in_optional_chain
          ac_loc
          ~tparams_rev
          ~bracket_syntax
          ~member_loc
          ~is_type_annotation
          ~force_instance:false
      | Ac_jsx_element { type_ } ->
        autocomplete_jsx_element
          ~env
          ~options
          ~reader
          ~cx
          ~ac_loc
          ~file_sig
          ~ast
          ~typed_ast
          ~imports
          ~imports_ranked_usage
          ~tparams_rev
          ~edit_locs
          ~token
          ~type_
      | Ac_jsx_attribute { attribute_name; used_attr_names; component_t; has_value } ->
        autocomplete_jsx_attribute
          ~reader
          ~used_attr_names
          ~has_value
          ~tparams_rev
          ~edit_locs
          ~token
          cx
          file_sig
          typed_ast
          component_t
          (ac_loc, attribute_name)
      | Ac_type ->
        AcResult
          (autocomplete_unqualified_type
             ~env
             ~options
             ~reader
             ~cx
             ~imports
             ~imports_ranked_usage
             ~tparams_rev
             ~ac_loc
             ~ast
             ~typed_ast
             ~file_sig
             ~edit_locs
             ~token
          )
      | Ac_qualified_type qtype ->
        autocomplete_module_exports
          ~reader
          ~cx
          ~file_sig
          ~typed_ast
          ~tparams_rev
          ~edit_locs
          ~token
          ~kind:`Type
          qtype
    in
    (Some token, Some ac_loc, string_of_autocomplete_type autocomplete_type, result)
