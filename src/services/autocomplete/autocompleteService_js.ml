(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Autocomplete_js
open Base.Result
open ServerProt.Response
open Parsing_heaps_utils
open Loc_collections

let add_autocomplete_token contents line column =
  let line = line - 1 in
  let contents_with_token =
    Line.transform_nth contents line (fun line_str ->
        let length = String.length line_str in
        if length >= column then
          let start = String.sub line_str 0 column in
          let end_ = String.sub line_str column (length - column) in
          start ^ Autocomplete_js.autocomplete_suffix ^ end_
        else
          line_str)
  in
  let f (_, x, _) = x in
  let default = "" in
  ( contents_with_token,
    Option.value_map ~f ~default (Line.split_nth contents_with_token (line - 1))
    ^ Option.value_map ~f ~default (Line.split_nth contents_with_token line)
    ^ Option.value_map ~f ~default (Line.split_nth contents_with_token (line + 1)) )

(* the autocomplete token inserts `suffix_len` characters, which are included
 * in `ac_loc` returned by `Autocomplete_js`. They need to be removed before
 * showing `ac_loc` to the client. *)
let remove_autocomplete_token_from_loc loc =
  Loc.{ loc with _end = { loc._end with column = loc._end.column - Autocomplete_js.suffix_len } }

let autocomplete_result_to_json ~strip_root result =
  let func_param_to_json param =
    Hh_json.JSON_Object
      [
        ("name", Hh_json.JSON_String param.param_name);
        ("type", Hh_json.JSON_String param.param_ty);
      ]
  in
  let func_details_to_json details =
    match details with
    | Some fd ->
      Hh_json.JSON_Object
        [
          ("return_type", Hh_json.JSON_String fd.return_ty);
          ("params", Hh_json.JSON_Array (Base.List.map ~f:func_param_to_json fd.param_tys));
        ]
    | None -> Hh_json.JSON_Null
  in
  let name = result.res_name in
  let (ty_loc, ty) = result.res_ty in
  (* This is deprecated for two reasons:
   *   1) The props are still our legacy, flat format rather than grouped into
   *      "loc" and "range" properties.
   *   2) It's the location of the definition of the type (the "type loc"),
   *      which may be interesting but should be its own field. The loc should
   *      instead be the range to replace (usually but not always the token
   *      being completed; perhaps we also want to replace the whole member
   *      expression, for example). That's `result.res_loc`, but we're not
   *      exposing it in the legacy `flow autocomplete` API; use
   *      LSP instead.
   *)
  let deprecated_loc = Errors.deprecated_json_props_of_loc ~strip_root ty_loc in
  Hh_json.JSON_Object
    ( ("name", Hh_json.JSON_String name)
    :: ("type", Hh_json.JSON_String ty)
    :: ("func_details", func_details_to_json result.func_details)
    :: deprecated_loc )

let autocomplete_response_to_json ~strip_root response =
  Hh_json.(
    match response with
    | Error error ->
      JSON_Object
        [
          ("error", JSON_String error);
          ("result", JSON_Array []);
            (* TODO: remove this? kept for BC *)
          
        ]
    | Ok completions ->
      let results = List.map (autocomplete_result_to_json ~strip_root) completions in
      JSON_Object [("result", JSON_Array results)])

let parameter_name is_opt name =
  let opt =
    if is_opt then
      "?"
    else
      ""
  in
  Option.value name ~default:"_" ^ opt

let lsp_completion_of_type =
  Ty.(
    function
    | InterfaceDecl _
    | InlineInterface _ ->
      Some Lsp.Completion.Interface
    | ClassDecl _ -> Some Lsp.Completion.Class
    | StrLit _
    | NumLit _
    | BoolLit _ ->
      Some Lsp.Completion.Value
    | Fun _ -> Some Lsp.Completion.Function
    | TypeAlias _
    | Union _ ->
      Some Lsp.Completion.Enum
    | Module _ -> Some Lsp.Completion.Module
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
    | Num _
    | Str _
    | Bool _
    | Arr _
    | TypeOf _
    | Utility _
    | Mu _ ->
      Some Lsp.Completion.Variable)

let autocomplete_create_result ?(show_func_details = true) ?insert_text (name, loc) (ty, ty_loc) =
  let res_ty = (ty_loc, Ty_printer.string_of_t ~with_comments:false ty) in
  let res_kind = lsp_completion_of_type ty in
  Ty.(
    match ty with
    | Fun { fun_params; fun_rest_param; fun_return; _ } when show_func_details ->
      let param_tys =
        Base.List.map
          ~f:(fun (n, t, fp) ->
            let param_name = parameter_name fp.prm_optional n in
            let param_ty = Ty_printer.string_of_t ~with_comments:false t in
            { param_name; param_ty })
          fun_params
      in
      let param_tys =
        match fun_rest_param with
        | None -> param_tys
        | Some (name, t) ->
          let param_name = "..." ^ parameter_name false name in
          let param_ty = Ty_printer.string_of_t ~with_comments:false t in
          param_tys @ [{ param_name; param_ty }]
      in
      let return = Ty_printer.string_of_t ~with_comments:false fun_return in
      {
        res_loc = loc;
        res_kind;
        res_name = name;
        res_insert_text = insert_text;
        res_ty;
        func_details = Some { param_tys; return_ty = return };
      }
    | _ ->
      {
        res_loc = loc;
        res_kind;
        res_name = name;
        res_insert_text = insert_text;
        res_ty;
        func_details = None;
      })

let autocomplete_is_valid_member key =
  (* This is really for being better safe than sorry. It shouldn't happen. *)
  (not (is_autocomplete key))
  (* filter out constructor, it shouldn't be called manually *)
  && (not (key = "constructor"))
  && (* strip out members from prototypes which are implicitly created for
     internal reasons *)
     not (Reason.is_internal_name key)

let ty_normalizer_options =
  Ty_normalizer_env.
    {
      fall_through_merged = true;
      expand_internal_types = true;
      expand_type_aliases = false;
      flag_shadowed_type_params = true;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = true;
      optimize_types = true;
      omit_targ_defaults = false;
      merge_bot_and_any_kinds = true;
    }

type autocomplete_service_result =
  | AcResult of {
      results: ServerProt.Response.complete_autocomplete_result list;
      errors_to_log: string list;
    }
  | AcEmpty of string
  | AcFatalError of string

let autocomplete_member
    ~reader
    ~exclude_proto_members
    ?(exclude_keys = SSet.empty)
    ?compute_insert_text
    cx
    file_sig
    typed_ast
    this
    ac_loc
    ~tparams =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let result = Members.extract ~exclude_proto_members cx this in
  match Members.to_command_result result with
  | Error error -> AcFatalError error
  | Ok result_map ->
    let file = Context.file cx in
    let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~typed_ast ~file_sig in
    let rev_result =
      SMap.fold
        (fun name (_id_loc, type_) acc ->
          if (not (autocomplete_is_valid_member name)) || SSet.mem name exclude_keys then
            acc
          else
            let loc = Type.loc_of_t type_ |> loc_of_aloc ~reader in
            let scheme = Type.TypeScheme.{ tparams; type_ } in
            ((name, loc), scheme) :: acc)
        result_map
        []
    in
    let (results, errors_to_log) =
      rev_result
      |> Ty_normalizer.from_schemes ~options:ty_normalizer_options ~genv
      |> Base.List.fold_left
           ~init:([], [])
           ~f:(fun (results, errors_to_log) ((name, ty_loc), ty_res) ->
             match ty_res with
             | Ok ty ->
               let result =
                 autocomplete_create_result
                   ?insert_text:(Option.map ~f:(fun f -> f name) compute_insert_text)
                   (name, ac_loc)
                   (ty, ty_loc)
               in
               (result :: results, errors_to_log)
             | Error err ->
               let error_to_log = Ty_normalizer.error_to_string err in
               (results, error_to_log :: errors_to_log))
    in
    AcResult { results; errors_to_log }

(* turns typed AST into normal AST so we can run Scope_builder on it *)
(* TODO(vijayramamurthy): make scope builder polymorphic *)
class type_killer (reader : Parsing_heaps.Reader.reader) =
  object
    inherit [ALoc.t, ALoc.t * Type.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = loc_of_aloc ~reader x

    method on_type_annot (x, _) = loc_of_aloc ~reader x
  end

(* The fact that we need this feels convoluted.
    We started with a typed AST, then stripped the types off of it to run Scope_builder on it,
    and now we go back to the typed AST to get the types of the locations we got from Scope_api.
    We wouldn't need to do this separate pass if Scope_builder/Scope_api were polymorphic.
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
  Pervasives.ignore (collector#program typed_ast);
  collector#collected_types

let local_value_identifiers ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams =
  let scope_info = Scope_builder.program ((new type_killer reader)#program typed_ast) in
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
         Option.map (LocMap.find_opt loc types) ~f:(fun type_ ->
             ((name, loc), Type.TypeScheme.{ tparams; type_ })))
  |> Ty_normalizer.from_schemes
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

(* env is all visible bound names at cursor *)
let autocomplete_id
    ~reader ~cx ~ac_loc ~id_type ~file_sig ~typed_ast ~include_super ~include_this ~tparams =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let (results, errors_to_log) =
    local_value_identifiers ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams
    |> List.fold_left
         (fun (results, errors_to_log) ((name, loc), normalization_result) ->
           match normalization_result with
           | Ok ty ->
             let result =
               autocomplete_create_result
                 ~show_func_details:(id_type <> JSXIdent)
                 (name, ac_loc)
                 (ty, loc)
             in
             (result :: results, errors_to_log)
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (results, error_to_log :: errors_to_log))
         ([], [])
  in
  (* "this" is legal inside classes and (non-arrow) functions *)
  let results =
    if include_this then
      {
        res_loc = ac_loc;
        res_kind = Some Lsp.Completion.Variable;
        res_name = "this";
        res_ty = (Loc.none, "this");
        func_details = None;
        res_insert_text = None;
      }
      :: results
    else
      results
  in
  (* "super" is legal inside classes *)
  let results =
    if include_super then
      {
        res_loc = ac_loc;
        res_kind = Some Lsp.Completion.Variable;
        res_name = "super";
        res_ty = (Loc.none, "super");
        func_details = None;
        res_insert_text = None;
      }
      :: results
    else
      results
  in
  AcResult { results; errors_to_log }

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx ~reader cx file_sig typed_ast cls ac_name ~used_attr_names ac_loc ~tparams =
  let open Flow_js in
  let reason = Reason.mk_reason (Reason.RCustom ac_name) ac_loc in
  let props_object =
    Tvar.mk_where cx reason (fun tvar ->
        let use_op = Type.Op Type.UnknownUse in
        flow cx (cls, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar)))
  in
  (* The `children` prop (if it exists) is set with the contents between the opening and closing
   * elements, rather than through an explicit `children={...}` attribute, so we should exclude
   * it from the autocomplete results, along with already used attribute names. *)
  let exclude_keys = SSet.add "children" used_attr_names in
  (* Only include own properties, so we don't suggest things like `hasOwnProperty` as potential JSX properties *)
  autocomplete_member
    ~reader
    ~exclude_proto_members:true
    ~exclude_keys
    ~compute_insert_text:(fun name -> name ^ "=")
    cx
    file_sig
    typed_ast
    props_object
    ac_loc
    ~tparams

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
      let { importKind; specifiers; default; _ } = x in
      let binds_type = function
        | ImportType
        | ImportTypeof ->
          true
        | ImportValue -> false
      in
      let declaration_binds_type = binds_type importKind in
      if declaration_binds_type then Option.iter default ~f:this#add_id;
      Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers specifiers ->
            List.iter
              (fun { kind; local; remote } ->
                let specifier_binds_type =
                  match kind with
                  | None -> declaration_binds_type
                  | Some k -> binds_type k
                in
                if specifier_binds_type then this#add_id (Option.value local ~default:remote))
              specifiers
          | ImportNamespaceSpecifier _ -> ( (* namespaces can't be types *) ));
      x
  end

let local_type_identifiers ~typed_ast ~cx ~file_sig =
  let search = new local_type_identifiers_searcher in
  Pervasives.ignore (search#program typed_ast);
  search#ids
  |> List.map (fun ((loc, t), Flow_ast.Identifier.{ name; _ }) -> ((name, loc), t))
  |> Ty_normalizer.from_types
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

let is_type_export =
  let open Ty in
  function
  | InterfaceDecl _
  | InlineInterface _
  | ClassDecl _
  | TypeAlias _ ->
    true
  | _ -> false

let type_exports_of_module_ty ~ac_loc =
  let open Ty in
  function
  | Module (_, { exports; _ }) ->
    Base.List.filter_map exports ~f:(fun (name, ty) ->
        if is_type_export ty then
          Some (autocomplete_create_result (name, ac_loc) (ty, Loc.none))
        else
          None)
  (* workaround while the ty normalizer sometimes renders modules as objects *)
  | Obj { obj_props; _ } ->
    Base.List.filter_map obj_props ~f:(function
        | NamedProp (name, Field (ty, _)) when is_type_export ty ->
          Some (autocomplete_create_result (name, ac_loc) (ty, Loc.none))
        | _ -> None)
  | _ -> []

let autocomplete_unqualified_type ~reader ~cx ~tparams ~file_sig ~ac_loc ~typed_ast =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let tparam_results =
    List.map
      Type.(
        fun { name; _ } ->
          {
            res_loc = ac_loc;
            res_kind = Some Lsp.Completion.TypeParameter;
            res_name = name;
            res_ty = (Loc.none, name);
            func_details = None;
            res_insert_text = None;
          })
      tparams
  in
  let (tparam_and_tident_results, tparam_and_tident_errors_to_log) =
    local_type_identifiers ~typed_ast ~cx ~file_sig
    |> List.fold_left
         (fun (results, errors_to_log) ((name, loc), ty_result) ->
           match ty_result with
           | Ok ty ->
             let result =
               autocomplete_create_result (name, ac_loc) (ty, loc_of_aloc ~reader loc)
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
  let (results, errors_to_log) =
    local_value_identifiers ~typed_ast ~reader ~ac_loc ~tparams ~cx ~file_sig
    |> List.fold_left
         (fun (results, errors_to_log) ((name, loc), ty_res) ->
           match ty_res with
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (results, error_to_log :: errors_to_log)
           | Ok (Ty.ClassDecl _ as ty) ->
             let result = autocomplete_create_result (name, ac_loc) (ty, loc) in
             (result :: results, errors_to_log)
           | Ok ty when type_exports_of_module_ty ~ac_loc ty <> [] ->
             let result =
               autocomplete_create_result (name, ac_loc) (ty, loc) ~insert_text:(name ^ ".")
             in
             (result :: results, errors_to_log)
           | Ok _ -> (results, errors_to_log))
         (tparam_and_tident_results, tparam_and_tident_errors_to_log)
  in
  AcResult { results; errors_to_log }

let autocomplete_qualified_type ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams ~qtype =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let qtype_scheme = Type.TypeScheme.{ tparams; type_ = qtype } in
  let module_ty_res =
    Ty_normalizer.from_scheme
      ~options:ty_normalizer_options
      ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)
      qtype_scheme
  in
  match module_ty_res with
  | Error err -> AcResult { results = []; errors_to_log = [Ty_normalizer.error_to_string err] }
  | Ok module_ty ->
    AcResult { results = type_exports_of_module_ty ~ac_loc module_ty; errors_to_log = [] }

let autocomplete_get_results ~reader cx file_sig typed_ast trigger_character cursor =
  let file_sig = File_sig.abstractify_locs file_sig in
  match Autocomplete_js.process_location ~trigger_character ~cursor ~typed_ast with
  | Some (_, _, Acbinding) -> ("Empty", AcEmpty "Binding")
  | Some (_, _, Acignored) -> ("Empty", AcEmpty "Ignored")
  | Some (_, _, Accomment) -> ("Empty", AcEmpty "Comment")
  | Some (_, _, Acliteral) -> ("Empty", AcEmpty "Literal")
  | Some (_, _, Acjsxtext) -> ("Empty", AcEmpty "JSXText")
  | Some (_, _, Acmodule) ->
    (* TODO: complete module names *)
    ("Acmodule", AcEmpty "Module")
  | Some (_, _, Ackey) ->
    (* TODO: complete object keys based on their upper bounds *)
    ("Ackey", AcResult { results = []; errors_to_log = [] })
  | Some (tparams, ac_loc, Acid { id_type; include_super; include_this }) ->
    ( "Acid",
      autocomplete_id
        ~reader
        ~cx
        ~ac_loc
        ~id_type
        ~file_sig
        ~typed_ast
        ~include_super
        ~include_this
        ~tparams )
  | Some (tparams, ac_loc, Acmem this) ->
    ( "Acmem",
      autocomplete_member
        ~reader
        ~exclude_proto_members:false
        cx
        file_sig
        typed_ast
        this
        ac_loc
        ~tparams )
  | Some (tparams, ac_loc, Acjsx (ac_name, used_attr_names, cls)) ->
    ( "Acjsx",
      autocomplete_jsx ~reader cx file_sig typed_ast cls ac_name ~used_attr_names ac_loc ~tparams
    )
  | Some (tparams, ac_loc, Actype) ->
    ("Actype", autocomplete_unqualified_type ~reader ~cx ~tparams ~ac_loc ~typed_ast ~file_sig)
  | Some (tparams, ac_loc, Acqualifiedtype qtype) ->
    ( "Acqualifiedtype",
      autocomplete_qualified_type ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams ~qtype )
  | None ->
    ("None", AcResult { results = []; errors_to_log = ["Autocomplete token not found in AST"] })
