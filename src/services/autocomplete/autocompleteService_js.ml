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
          ("params", Hh_json.JSON_Array (Core_list.map ~f:func_param_to_json fd.param_tys));
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

let autocomplete_create_result (name, loc) (ty, ty_loc) =
  let res_ty = (ty_loc, Ty_printer.string_of_t ~with_comments:false ty) in
  let res_kind = lsp_completion_of_type ty in
  Ty.(
    match ty with
    | Fun { fun_params; fun_rest_param; fun_return; _ } ->
      let param_tys =
        Core_list.map
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
        res_ty;
        func_details = Some { param_tys; return_ty = return };
      }
    | _ -> { res_loc = loc; res_kind; res_name = name; res_ty; func_details = None })

let autocomplete_is_valid_member key =
  (* This is really for being better safe than sorry. It shouldn't happen. *)
  (not (is_autocomplete key))
  (* filter out constructor, it shouldn't be called manually *)
  && (not (key = "constructor"))
  && (* strip out members from prototypes which are implicitly created for
     internal reasons *)
     not (Reason.is_internal_name key)

let autocomplete_member
    ~reader
    ~exclude_proto_members
    ?(exclude_keys = SSet.empty)
    ~ac_type
    cx
    file_sig
    typed_ast
    this
    ac_name
    ac_loc
    ac_trigger
    docblock
    ~broader_context =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let result = Members.extract ~exclude_proto_members cx this in
  Hh_json.(
    let (result_str, t) =
      Members.(
        match result with
        | Success _ -> ("SUCCESS", this)
        | SuccessModule _ -> ("SUCCESS", this)
        | FailureNullishType -> ("FAILURE_NULLABLE", this)
        | FailureAnyType -> ("FAILURE_NO_COVERAGE", this)
        | FailureUnhandledType t -> ("FAILURE_UNHANDLED_TYPE", t)
        | FailureUnhandledMembers t -> ("FAILURE_UNHANDLED_MEMBERS", t))
    in
    let json_data_to_log =
      JSON_Object
        [
          ("ac_type", JSON_String ac_type);
          ("ac_name", JSON_String ac_name);
          (* don't need to strip root for logging *)
            ("ac_loc", JSON_Object (Errors.deprecated_json_props_of_loc ~strip_root:None ac_loc));
          ("ac_trigger", JSON_String (Option.value ac_trigger ~default:"None"));
          ("loc", Reason.json_of_loc ~offset_table:None ac_loc);
          ("docblock", Docblock.json_of_docblock docblock);
          ("result", JSON_String result_str);
          ("type", Debug_js.json_of_t ~depth:3 cx t);
          ("broader_context", JSON_String broader_context);
        ]
    in
    match Members.to_command_result result with
    | Error error -> Error (error, Some json_data_to_log)
    | Ok result_map ->
      let options =
        {
          Ty_normalizer_env.fall_through_merged = true;
          expand_internal_types = true;
          expand_type_aliases = false;
          flag_shadowed_type_params = true;
          preserve_inferred_literal_types = false;
          evaluate_type_destructors = true;
          optimize_types = true;
          omit_targ_defaults = false;
          merge_bot_and_any_kinds = true;
        }
      in
      let file = Context.file cx in
      let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~typed_ast ~file_sig in
      let rev_result =
        SMap.fold
          (fun name (_id_loc, t) acc ->
            if (not (autocomplete_is_valid_member name)) || SSet.mem name exclude_keys then
              acc
            else
              let loc = Type.loc_of_t t |> loc_of_aloc ~reader in
              ((name, loc), t) :: acc)
          result_map
          []
      in
      let result =
        rev_result
        |> Ty_normalizer.from_types ~options ~genv
        |> Core_list.rev_filter_map ~f:(function
               | ((name, ty_loc), Ok ty) ->
                 Some (autocomplete_create_result (name, ac_loc) (ty, ty_loc))
               | _ -> None)
      in
      Ok (result, Some json_data_to_log))

(* env is all visible bound names at cursor *)
let autocomplete_id ~reader cx ac_loc ac_trigger file_sig env typed_ast ~broader_context =
  let ac_loc = loc_of_aloc ~reader ac_loc |> remove_autocomplete_token_from_loc in
  let (result, errors, this_result) =
    SMap.fold
      (fun name entry (acc, errors, this_result) ->
        (* Filter out internal environment variables except for this and
       super. `this` may appear twice in the scope entries when inside a class, once as `.this` and
       a second time as `this`. In order to avoid duplicate autocomplete results,
       we take care to only add this to the results once. *)
        let is_this = name = "this" in
        let is_internal_this = name = Reason.internal_name "this" in
        let is_internal_super = name = Reason.internal_name "super" in
        if (not (is_internal_this || is_internal_super)) && Reason.is_internal_name name then
          (acc, errors, this_result)
        (* Prefer this over .this *)
        else if is_internal_this && this_result <> None then
          (acc, errors, this_result)
        else
          let (ty_loc, name) =
            (* renaming of this/super *)
            if is_internal_this then
              (Loc.none, "this")
            else if is_internal_super then
              (Loc.none, "super")
            else
              (Scope.Entry.entry_loc entry |> loc_of_aloc ~reader, name)
          in
          let options =
            {
              Ty_normalizer_env.fall_through_merged = true;
              expand_internal_types = true;
              expand_type_aliases = false;
              flag_shadowed_type_params = true;
              preserve_inferred_literal_types = false;
              evaluate_type_destructors = true;
              optimize_types = true;
              omit_targ_defaults = false;
              merge_bot_and_any_kinds = true;
            }
          in
          let file = Context.file cx in
          let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~typed_ast ~file_sig in
          let type_ = Scope.Entry.actual_type entry in
          match Ty_normalizer.from_type ~options ~genv type_ with
          | Ok ty ->
            let result = autocomplete_create_result (name, ac_loc) (ty, ty_loc) in
            if is_this || is_internal_this then
              (acc, errors, Some result)
            else
              (result :: acc, errors, this_result)
          | Error err -> (acc, err :: errors, this_result))
      env
      ([], [], None)
  in
  let result =
    match this_result with
    | Some this_result -> this_result :: result
    | None -> result
  in
  let json_data_to_log =
    Hh_json.(
      let result_str =
        match (result, errors) with
        | (_, []) -> "SUCCESS"
        | ([], _) -> "FAILURE_NORMALIZER"
        | (_, _) -> "PARTIAL"
      in
      JSON_Object
        [
          ("ac_type", JSON_String "Acid");
          ("ac_trigger", JSON_String (Option.value ac_trigger ~default:"None"));
          ("result", JSON_String result_str);
          ("count", JSON_Number (result |> List.length |> string_of_int));
          ( "errors",
            JSON_Array
              (Core_list.rev_map errors ~f:(fun err ->
                   JSON_String (Ty_normalizer.error_to_string err))) );
          ("broader_context", JSON_String broader_context);
        ])
  in
  Ok (result, Some json_data_to_log)

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx
    ~reader cx file_sig typed_ast cls ac_name ac_loc ac_trigger docblock ~broader_context =
  Flow_js.(
    let reason = Reason.mk_reason (Reason.RCustom ac_name) ac_loc in
    let props_object =
      Tvar.mk_where cx reason (fun tvar ->
          let use_op = Type.Op Type.UnknownUse in
          flow cx (cls, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar)))
    in
    (* The `children` prop (if it exists) is set with the contents between the opening and closing
     * elements, rather than through an explicit `children={...}` attribute, so we should exclude
     * it from the autocomplete results. *)
    let exclude_keys = SSet.singleton "children" in
    (* Only include own properties, so we don't suggest things like `hasOwnProperty` as potential JSX properties *)
    autocomplete_member
      ~reader
      ~exclude_proto_members:true
      ~exclude_keys
      ~ac_type:"Acjsx"
      cx
      file_sig
      typed_ast
      props_object
      ac_name
      ac_loc
      ac_trigger
      docblock
      ~broader_context)

let autocomplete_get_results
    ~reader cx file_sig typed_ast state trigger_character docblock ~broader_context =
  let file_sig = File_sig.abstractify_locs file_sig in
  match !state with
  | Some { ac_loc; ac_type = Acid env; _ } ->
    autocomplete_id ~reader cx ac_loc trigger_character file_sig env typed_ast ~broader_context
  | Some { ac_name; ac_loc; ac_type = Acmem this } ->
    autocomplete_member
      ~reader
      ~exclude_proto_members:false
      ~ac_type:"Acmem"
      cx
      file_sig
      typed_ast
      this
      ac_name
      ac_loc
      trigger_character
      docblock
      ~broader_context
  | Some { ac_name; ac_loc; ac_type = Acjsx cls } ->
    autocomplete_jsx
      ~reader
      cx
      file_sig
      typed_ast
      cls
      ac_name
      ac_loc
      trigger_character
      docblock
      ~broader_context
  | Some { ac_name = _; ac_loc = _; ac_type = Ackey } ->
    let json_data_to_log =
      Hh_json.(
        JSON_Object
          [
            ("ac_type", JSON_String "Ackey");
            ("ac_trigger", JSON_String (Option.value trigger_character ~default:"None"));
          ])
    in
    Ok ([], Some json_data_to_log)
  | None ->
    let json_data_to_log =
      Hh_json.(
        JSON_Object
          [
            ("ac_type", JSON_String "None");
            ("ac_trigger", JSON_String (Option.value trigger_character ~default:"None"));
            ("broader_context", JSON_String broader_context);
          ])
    in
    Ok ([], Some json_data_to_log)
