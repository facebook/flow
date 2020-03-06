(*
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
    Base.Option.value_map ~f ~default (Line.split_nth contents_with_token (line - 1))
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_token line)
    ^ Base.Option.value_map ~f ~default (Line.split_nth contents_with_token (line + 1)) )

(* the autocomplete token inserts `suffix_len` characters, which are included
 * in `ac_loc` returned by `Autocomplete_js`. They need to be removed before
 * showing `ac_loc` to the client. *)
let remove_autocomplete_token_from_loc loc =
  Loc.{ loc with _end = { loc._end with column = loc._end.column - Autocomplete_js.suffix_len } }

let autocomplete_result_to_json ~strip_root result =
  let func_param_to_json param =
    Hh_json.JSON_Object
      [("name", Hh_json.JSON_String param.param_name); ("type", Hh_json.JSON_String param.param_ty)]
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
  Pervasives.ignore strip_root;
  Hh_json.JSON_Object
    [
      ("name", Hh_json.JSON_String name);
      ("type", Hh_json.JSON_String result.res_ty);
      ("func_details", func_details_to_json result.func_details);
    ]

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
      let results = Base.List.map ~f:(autocomplete_result_to_json ~strip_root) completions in
      JSON_Object [("result", JSON_Array results)])

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

let autocomplete_create_result ?(show_func_details = true) ?insert_text ?(rank = 0) (name, loc) ty =
  let res_ty = Ty_printer.string_of_t_single_line ~with_comments:false ty in
  let res_kind = lsp_completion_of_type ty in
  let func_details =
    match ty with
    | Ty.(Fun { fun_params; fun_rest_param; fun_return; _ }) when show_func_details ->
      Some (Signature_help.func_details fun_params fun_rest_param fun_return)
    | _ -> None
  in
  {
    res_loc = loc;
    res_kind;
    res_name = name;
    res_insert_text = insert_text;
    res_ty;
    func_details;
    rank;
  }

let autocomplete_create_result_decl ~show_func_details:_ ?insert_text:_ ~rank (name, loc) d =
  let open Ty in
  match d with
  | ModuleDecl _ ->
    {
      res_loc = loc;
      res_kind = Some Lsp.Completion.Module;
      res_name = name;
      res_insert_text = None;
      res_ty = "module " ^ name;
      func_details = None;
      rank;
    }
  | Ty.VariableDecl (_, ty) ->
    {
      res_loc = loc;
      res_kind = Some Lsp.Completion.Variable;
      res_name = name;
      res_insert_text = None;
      res_ty = Ty_printer.string_of_t_single_line ~with_comments:false ty;
      func_details = None;
      rank;
    }
  | d ->
    {
      res_loc = loc;
      res_kind = Some (lsp_completion_of_decl d);
      res_name = name;
      res_insert_text = None;
      res_ty = Ty_printer.string_of_decl_single_line ~with_comments:false d;
      func_details = None;
      rank;
    }

let autocomplete_create_result_elt
    ?(show_func_details = true) ?insert_text ?(rank = 0) (name, loc) elt =
  match elt with
  | Ty.Type t -> autocomplete_create_result ~show_func_details ?insert_text ~rank (name, loc) t
  | Ty.Decl d -> autocomplete_create_result_decl ~show_func_details ?insert_text ~rank (name, loc) d

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
      verbose_normalizer = false;
      expand_toplevel_members = None;
      max_depth = Some 50;
    }

type autocomplete_service_result =
  | AcResult of {
      results: ServerProt.Response.complete_autocomplete_result list;
      errors_to_log: string list;
    }
  | AcEmpty of string
  | AcFatalError of string

(* helper types for autocomplete_member *)
module MemberInfo = struct
  type 'a t = {
    ty: 'a;
    (* Autocomplete ranks members from primitive prototypes below user-defined members.
     * `from_proto` indicates that the member is from a primitive prototype. *)
    from_proto: bool;
    (* If a member came from a possibly-null/undefined object, autocomplete may suggest
     * that the user use optional chaining to access it.
     * `from_nullable` indicates that the member is from a possibly-null/undefined object. *)
    from_nullable: bool;
  }

  let map f info = { info with ty = f info.ty }

  (*  There are two special cases we'll consider when recursively getting the members of
   *  a constituent type in a union/intersection:
   *  - `EmptyOrAny`:
   *    the given type's member set is the universal set,
   *    where each member's type is the given type.
   *  - `Nullish`:
   *    the given type is Void or Null.
   *  `Normal` indicates a type which falls into neither of these special cases. *)
  type membership_behavior =
    | EmptyOrAny
    | Nullish
    | Normal

  let membership_behavior =
    let open Ty in
    function
    | Bot _
    | Any _ ->
      EmptyOrAny
    | Void
    | Null ->
      Nullish
    | _ -> Normal
end

(* returns (members, errors to log) *)
let rec members_of_ty : Ty.t -> Ty.t MemberInfo.t SMap.t * string list =
  let open Ty in
  let ty_of_named_prop = function
    | Field { t; optional = false; _ }
    | Get t
    | Set t ->
      t
    | Field { t; optional = true; _ } -> Union (t, Void, [])
    | Method ft -> Fun ft
  in
  let members_of_obj obj_props =
    obj_props
    |> Base.List.fold_left ~init:(SMap.empty, []) ~f:(fun (mems1, errs1) prop ->
           let (mems2, errs2) =
             match prop with
             | NamedProp { name; prop; from_proto } ->
               ( SMap.singleton
                   name
                   MemberInfo.{ ty = ty_of_named_prop prop; from_proto; from_nullable = false },
                 [] )
             | SpreadProp ty -> members_of_ty ty
             | IndexProp _
             | CallProp _ ->
               (SMap.empty, [])
           in
           (SMap.union ~combine:(fun _ _ snd -> Some snd) mems1 mems2, errs1 @ errs2))
  in
  let members_of_union (t1, t2, ts) =
    let ((t1_members, errs1), (t2_members, errs2), (ts_members, errss)) =
      (members_of_ty t1, members_of_ty t2, Base.List.map ~f:members_of_ty ts |> List.split)
    in
    let errs = Base.List.concat (errs1 :: errs2 :: errss) in
    let universe =
      (* set union of all child members *)
      List.fold_right
        (SMap.merge (fun _ _ _ -> Some ()))
        (t1_members :: t2_members :: ts_members)
        SMap.empty
    in
    (* empty and any have all possible members *)
    let (t1_members, t2_members, ts_members) =
      let f ty ty_members =
        let open MemberInfo in
        match membership_behavior ty with
        | EmptyOrAny ->
          SMap.map (Fn.const { ty; from_proto = true; from_nullable = false }) universe
        | Nullish ->
          SMap.map
            (* Bot is the identity of type union *)
            (Fn.const { ty = Bot EmptyType; from_proto = true; from_nullable = true })
            universe
        | Normal -> ty_members
      in
      (f t1 t1_members, f t2 t2_members, List.map2 f ts ts_members)
    in
    let mems =
      (* set intersection of members; type union upon overlaps *)
      SMap.map (MemberInfo.map Nel.one) t1_members
      |> List.fold_right
           (SMap.merge (fun _ ty_opt tys_opt ->
                let open MemberInfo in
                match (ty_opt, tys_opt) with
                | ( Some { ty; from_proto = fp; from_nullable = fn },
                    Some { ty = tys; from_proto = fps; from_nullable = fns } ) ->
                  (* We say that a member formed by unioning other members should be treated:
                    * - as from a prototype only if all its constituent members are.
                    * - as from a nullable object if any of its constituent members are. *)
                  Some { ty = Nel.cons ty tys; from_proto = fp && fps; from_nullable = fn || fns }
                | (None, _)
                | (_, None) ->
                  None))
           (t2_members :: ts_members)
      |> SMap.map
           (MemberInfo.map (function
               | (t, []) -> t
               | (t1, t2 :: ts) -> Ty_utils.simplify_type ~merge_kinds:true (Union (t1, t2, ts))))
    in
    (mems, errs)
  in
  let members_of_intersection (t1, t2, ts) =
    let ((t1_members, errs1), (t2_members, errs2), (ts_members, errss)) =
      (members_of_ty t1, members_of_ty t2, Base.List.map ~f:members_of_ty ts |> List.split)
    in
    let errs = Base.List.concat (errs1 :: errs2 :: errss) in
    let special_cases =
      Base.List.filter_map
        ~f:(fun ty ->
          let open MemberInfo in
          match membership_behavior ty with
          | EmptyOrAny -> Some ty
          | Nullish
          | Normal ->
            None)
        (t1 :: t2 :: ts)
    in
    let mems =
      (* set union of members; type intersection upon overlaps *)
      SMap.map (MemberInfo.map Nel.one) t1_members
      |> List.fold_right
           (SMap.merge (fun _ ty_opt tys_opt ->
                let open MemberInfo in
                match (ty_opt, tys_opt) with
                | ( Some { ty; from_proto = fp; from_nullable = fn },
                    Some { ty = tys; from_proto = fps; from_nullable = fns } ) ->
                  (* We say that a member formed by intersecting other members should be treated:
                    * - as from a prototype only if all its constituent members are.
                    * - as from a nullable object only if all its constituent members are. *)
                  Some { ty = Nel.cons ty tys; from_proto = fp && fps; from_nullable = fn && fns }
                | (Some info, None) -> Some (MemberInfo.map Nel.one info)
                | (None, Some info) -> Some info
                | (None, None) -> None))
           (t2_members :: ts_members)
      |> SMap.map (MemberInfo.map (List.fold_right Nel.cons special_cases))
      |> SMap.map
           (MemberInfo.map (function
               | (t, []) -> t
               | (t1, t2 :: ts) -> Ty_utils.simplify_type ~merge_kinds:true (Inter (t1, t2, ts))))
    in
    (mems, errs)
  in
  function
  | Obj { obj_props; _ } -> members_of_obj obj_props
  | Fun { fun_static; _ } -> members_of_ty fun_static
  | Union (t1, t2, ts) -> members_of_union (t1, t2, ts)
  | Inter (t1, t2, ts) -> members_of_intersection (t1, t2, ts)
  | ( TVar _ | Bound _ | Generic _ | Symbol | Num _ | Str _ | Bool _ | NumLit _ | StrLit _
    | BoolLit _ | Arr _ | Tup _ ) as t ->
    (SMap.empty, [Printf.sprintf "members_of_ty unexpectedly applied to (%s)" (Ty_debug.dump_t t)])
  | Any _
  | Top
  | Bot _
  | Void
  | Null
  | InlineInterface _
  | TypeOf _
  | Utility _
  | Mu _
  | CharSet _ ->
    (SMap.empty, [])

let members_of_type
    ~exclude_proto_members
    ?(exclude_keys = SSet.empty)
    ?(idx_hook = Pervasives.ignore)
    cx
    file_sig
    typed_ast
    this
    ~tparams =
  let this_ty_res =
    Ty_normalizer.from_scheme
      ~options:
        {
          ty_normalizer_options with
          Ty_normalizer_env.expand_toplevel_members =
            Some Ty_normalizer_env.{ include_proto_members = not exclude_proto_members; idx_hook };
        }
      ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)
      Type.TypeScheme.{ tparams; type_ = this }
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
  match this_ty_res with
  | Error error -> return ([], [Ty_normalizer.error_to_string error])
  | Ok (Ty.Type (Ty.Any _)) -> fail "not enough type information to autocomplete"
  | Ok (Ty.Type this_ty) ->
    let (mems, errs) = members_of_ty this_ty in
    return
      ( mems |> SMap.bindings |> List.filter is_valid_member,
        match errs with
        | [] -> []
        | _ :: _ -> Printf.sprintf "members_of_type %s" (Debug_js.dump_t cx this) :: errs )
  | Ok _ -> return ([], ["Non type"])

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
  let in_idx = ref false in
  let idx_hook () = in_idx := true in
  match
    members_of_type
      ~exclude_proto_members
      ?exclude_keys
      ~idx_hook
      cx
      file_sig
      typed_ast
      this
      ~tparams
  with
  | Error err -> AcFatalError err
  | Ok (mems, errors_to_log) ->
    let results =
      mems
      |> Base.List.map ~f:(fun (name, MemberInfo.{ ty; from_proto; from_nullable }) ->
             let rank =
               if from_proto then
                 1
               else
                 0
             in
             let opt_chain_ty =
               Ty_utils.simplify_type ~merge_kinds:true (Ty.Union (Ty.Void, ty, []))
             in
             match (from_nullable, in_optional_chain, !in_idx) with
             | (false, _, _)
             | (_, _, true) ->
               autocomplete_create_result ~rank (name, ac_loc) ty
             | (true, false, false) ->
               let opt_chain_name = "?." ^ name in
               let opt_chain_ac_loc = Loc.btwn (Loc.char_before ac_loc) ac_loc in
               autocomplete_create_result
                 ~insert_text:opt_chain_name
                 ~rank
                 (opt_chain_name, opt_chain_ac_loc)
                 opt_chain_ty
             | (true, true, false) -> autocomplete_create_result ~rank (name, ac_loc) opt_chain_ty)
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
         Base.Option.map (LocMap.find_opt loc types) ~f:(fun type_ ->
             (name, Type.TypeScheme.{ tparams; type_ })))
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
         (fun (results, errors_to_log) (name, elt_result) ->
           match elt_result with
           | Ok elt ->
             let result =
               autocomplete_create_result_elt
                 ~show_func_details:(id_type <> JSXIdent)
                 (name, ac_loc)
                 elt
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
        res_ty = "this";
        func_details = None;
        res_insert_text = None;
        rank = 0;
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
        res_ty = "super";
        func_details = None;
        res_insert_text = None;
        rank = 0;
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
  let mems_result =
    members_of_type
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
  | Ok (mems, errors_to_log) ->
    let results =
      mems
      |> Base.List.map ~f:(fun (name, MemberInfo.{ ty; _ }) ->
             autocomplete_create_result ~insert_text:(name ^ "=") (name, ac_loc) ty)
    in
    AcResult { results; errors_to_log }

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
  Pervasives.ignore (search#program typed_ast);
  search#ids
  |> Base.List.map ~f:(fun ((_, t), Flow_ast.Identifier.{ name; _ }) -> (name, t))
  |> Ty_normalizer.from_types
       ~options:ty_normalizer_options
       ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)

let type_exports_of_module_ty ~ac_loc =
  let open Ty in
  function
  | Decl (ModuleDecl { exports; _ }) ->
    Base.List.filter_map
      ~f:(function
        | TypeAliasDecl { name; type_ = Some t; _ } as d ->
          Some
            {
              res_loc = ac_loc;
              res_kind = lsp_completion_of_type t;
              res_name = name.Ty.sym_name;
              res_insert_text = None;
              res_ty = Ty_printer.string_of_decl_single_line d;
              func_details = None;
              rank = 0;
            }
        | InterfaceDecl (name, _) as d ->
          Some
            {
              res_loc = ac_loc;
              res_kind = Some Lsp.Completion.Interface;
              res_name = name.Ty.sym_name;
              res_insert_text = None;
              res_ty = Ty_printer.string_of_decl_single_line d;
              func_details = None;
              rank = 0;
            }
        | ClassDecl (name, _) as d ->
          Some
            {
              res_loc = ac_loc;
              res_kind = Some Lsp.Completion.Class;
              res_name = name.Ty.sym_name;
              res_insert_text = None;
              res_ty = Ty_printer.string_of_decl_single_line d;
              func_details = None;
              rank = 0;
            }
        | _ -> None)
      exports
    |> Base.List.sort ~compare:(fun { res_name = a; _ } { res_name = b; _ } ->
           Pervasives.compare a b)
    |> Base.List.mapi ~f:(fun i r -> { r with rank = i })
  | _ -> []

let autocomplete_unqualified_type ~reader ~cx ~tparams ~file_sig ~ac_loc ~typed_ast =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let tparam_results =
    Base.List.map
      ~f:(fun (_, name) ->
        {
          res_loc = ac_loc;
          res_kind = Some Lsp.Completion.TypeParameter;
          res_name = name;
          res_ty = name;
          func_details = None;
          res_insert_text = None;
          rank = 0;
        })
      tparams
  in
  let (tparam_and_tident_results, tparam_and_tident_errors_to_log) =
    local_type_identifiers ~typed_ast ~cx ~file_sig
    |> List.fold_left
         (fun (results, errors_to_log) (name, ty_result) ->
           match ty_result with
           | Ok elt ->
             let result = autocomplete_create_result_elt (name, ac_loc) elt in
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
         (fun (results, errors_to_log) (name, ty_res) ->
           match ty_res with
           | Error err ->
             let error_to_log = Ty_normalizer.error_to_string err in
             (results, error_to_log :: errors_to_log)
           | Ok (Ty.Decl (Ty.ClassDecl _) as elt) ->
             let result = autocomplete_create_result_elt (name, ac_loc) elt in
             (result :: results, errors_to_log)
           | Ok elt when type_exports_of_module_ty ~ac_loc elt <> [] ->
             let result =
               autocomplete_create_result_elt (name, ac_loc) elt ~insert_text:(name ^ ".")
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
  | Some (tparams, ac_loc, Acmem { obj_type; in_optional_chain }) ->
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
  | Some (tparams, ac_loc, Acjsx (ac_name, used_attr_names, cls)) ->
    ( "Acjsx",
      autocomplete_jsx ~reader cx file_sig typed_ast cls ac_name ~used_attr_names ac_loc ~tparams )
  | Some (tparams, ac_loc, Actype) ->
    ("Actype", autocomplete_unqualified_type ~reader ~cx ~tparams ~ac_loc ~typed_ast ~file_sig)
  | Some (tparams, ac_loc, Acqualifiedtype qtype) ->
    ( "Acqualifiedtype",
      autocomplete_qualified_type ~reader ~cx ~ac_loc ~file_sig ~typed_ast ~tparams ~qtype )
  | None ->
    ("None", AcResult { results = []; errors_to_log = ["Autocomplete token not found in AST"] })
