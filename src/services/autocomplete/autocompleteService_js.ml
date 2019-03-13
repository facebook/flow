(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Autocomplete_js
open Core_result
open ServerProt.Response

let add_autocomplete_token contents line column =
  let line = line - 1 in
  Line.transform_nth contents line (fun line_str ->
    let length = String.length line_str in
    if length >= column
    then (
      let start = String.sub line_str 0 column in
      let end_ = String.sub line_str column (length - column) in
      start ^ Autocomplete_js.autocomplete_suffix ^ end_
    ) else line_str
  )

let autocomplete_result_to_json ~strip_root result =
  let func_param_to_json param =
    Hh_json.JSON_Object [
      "name", Hh_json.JSON_String param.param_name;
      "type", Hh_json.JSON_String param.param_ty;
    ]
  in
  let func_details_to_json details =
    match details with
     | Some fd -> Hh_json.JSON_Object [
         "return_type", Hh_json.JSON_String fd.return_ty;
         "params", Hh_json.JSON_Array (Core_list.map ~f:func_param_to_json fd.param_tys);
       ]
     | None -> Hh_json.JSON_Null
  in
  let name = result.res_name in
  let ty = result.res_ty in
  Hh_json.JSON_Object (
    ("name", Hh_json.JSON_String name) ::
    ("type", Hh_json.JSON_String ty) ::
    ("func_details", func_details_to_json result.func_details) ::
    (Errors.deprecated_json_props_of_loc ~strip_root result.res_loc)
  )

let autocomplete_response_to_json ~strip_root response =
  let open Hh_json in
  match response with
    | Error error ->
      JSON_Object [
        "error", JSON_String error;
        "result", JSON_Array []; (* TODO: remove this? kept for BC *)
      ]
    | Ok completions ->
      let results = List.map
        (autocomplete_result_to_json ~strip_root)
        completions
      in
      JSON_Object ["result", JSON_Array results]

let parameter_name is_opt name =
  let opt = if is_opt then "?" else "" in
  (Option.value name ~default:"_") ^ opt

let lsp_completion_of_type (ty: Ty.t) =
  let open Lsp.Completion in
  match ty with
  | Ty.InterfaceDecl _ -> Some Interface
  | Ty.ClassDecl _ -> Some Class
  | Ty.(StrLit _ | NumLit _ | BoolLit _) -> Some Value
  | Ty.Fun _ -> Some Function
  | Ty.TypeAlias _
  | Ty.Union _ -> Some Enum
  | Ty.Module _ -> Some Module
  | Ty.(Tup _ | Bot | Null | Obj _ | Inter _ | TVar _ | Bound _ | Generic _ |
      Any _ | Top | Void | Num _ | Str _ | Bool _ | Arr _ | TypeOf _ |
      Utility _ | Mu _
    ) ->  Some Variable

let autocomplete_create_result ((name, loc), ty) =
  let res_ty = Ty_printer.string_of_t ~with_comments:false ty in
  let res_kind = lsp_completion_of_type ty in
  Ty.(match ty with
  | Fun {fun_params; fun_rest_param; fun_return; _} ->
      let param_tys = Core_list.map ~f:(fun (n, t, fp) ->
        let param_name = parameter_name fp.prm_optional n in
        let param_ty = Ty_printer.string_of_t ~with_comments:false t in
        { param_name; param_ty }
      ) fun_params in
      let param_tys = match fun_rest_param with
      | None -> param_tys
      | Some (name, t) ->
        let param_name = "..." ^ parameter_name false name in
        let param_ty = Ty_printer.string_of_t ~with_comments:false t in
        param_tys @ [{ param_name; param_ty; }]
      in
      let return = Ty_printer.string_of_t ~with_comments:false fun_return in
      { res_loc = loc;
        res_kind;
        res_name = name;
        res_ty;
        func_details = Some { param_tys; return_ty = return } }
  | _ -> { res_loc = loc;
        res_kind;
        res_name = name;
        res_ty;
        func_details = None }
  )

let autocomplete_filter_members members =
  SMap.filter (fun key _ ->
    (* This is really for being better safe than sorry. It shouldn't happen. *)
    not (is_autocomplete key)
    &&
    (* filter out constructor, it shouldn't be called manually *)
    not (key = "constructor")
    &&
    (* strip out members from prototypes which are implicitly created for
       internal reasons *)
    not (Reason.is_internal_name key)
  ) members

let autocomplete_member ~exclude_proto_members ~ac_type cx file_sig this ac_name ac_loc docblock =
  let ac_loc = ALoc.to_loc ac_loc in
  let this_t = Members.resolve_type cx this in
  (* Resolve primitive types to their internal class type. We do this to allow
     autocompletion on these too. *)
  let this_t = Members.resolve_builtin_class cx this_t in
  let result = Members.extract ~exclude_proto_members cx this_t in

  let open Hh_json in

  let result_str, t = Members.(match result with
    | Success _ -> "SUCCESS", this
    | SuccessModule _ -> "SUCCESS", this
    | FailureNullishType -> "FAILURE_NULLABLE", this
    | FailureAnyType -> "FAILURE_NO_COVERAGE", this
    | FailureUnhandledType t -> "FAILURE_UNHANDLED_TYPE", t) in

  let json_data_to_log = JSON_Object [
    "ac_type", JSON_String ac_type;
    "ac_name", JSON_String ac_name;
    (* don't need to strip root for logging *)
    "ac_loc", JSON_Object (Errors.deprecated_json_props_of_loc ~strip_root:None ac_loc);
    "loc", Reason.json_of_loc ~offset_table:None ac_loc;
    "docblock", Docblock.json_of_docblock docblock;
    "result", JSON_String result_str;
    "type", Debug_js.json_of_t ~depth:3 cx t;
  ] in

  match Members.to_command_result result with
  | Error error -> Error (error, Some json_data_to_log)
  | Ok result_map ->
    let options = {
      Ty_normalizer_env.
      fall_through_merged = true;
      expand_internal_types = true;
      expand_type_aliases = false;
      flag_shadowed_type_params = true;
      preserve_inferred_literal_types = false;
      optimize_types = true;
      omit_targ_defaults = false;
    } in
    let file = Context.file cx in
    let type_table = Context.type_table cx in
    let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~type_table ~file_sig in
    let result = result_map
    |> autocomplete_filter_members
    |> SMap.mapi (fun name (_id_loc, t) -> ((name, Type.loc_of_t t |> ALoc.to_loc), t))
    |> SMap.values
    |> Ty_normalizer.from_types ~options ~genv
    |> Core_list.filter_map ~f:(function
     | l, Ok s -> Some (l, s)
     | _ -> None
     )
    |> Core_list.map ~f:autocomplete_create_result
    |> List.rev in
    Ok (result, Some json_data_to_log)


(* env is all visible bound names at cursor *)
let autocomplete_id cx file_sig env =
  let result = SMap.fold (fun name entry acc ->
    (* Filter out internal environment variables except for this and
       super. *)
    let is_this = name = (Reason.internal_name "this") in
    let is_super = name = (Reason.internal_name "super") in
    if not (is_this || is_super) && Reason.is_internal_name name
    then acc
    else (
      let (loc, name) =
        (* renaming of this/super *)
        if is_this
        then (Loc.none, "this")
        else if is_super
        then (Loc.none, "super")
        else (Scope.Entry.entry_loc entry |> ALoc.to_loc, name)
      in
      let options = {
        Ty_normalizer_env.
        fall_through_merged = true;
        expand_internal_types = true;
        expand_type_aliases = false;
        flag_shadowed_type_params = true;
        preserve_inferred_literal_types = false;
        optimize_types = true;
        omit_targ_defaults = false;
      } in
      let file = Context.file cx in
      let type_table = Context.type_table cx in
      let genv = Ty_normalizer_env.mk_genv ~full_cx:cx ~file ~type_table ~file_sig in
      let type_ = Scope.Entry.actual_type entry in
      match Ty_normalizer.from_type ~options ~genv type_ with
      | Ok t -> autocomplete_create_result ((name, loc), t) :: acc
      | Error _ -> acc
    )
  ) env [] in
  Ok (result, None)

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx cx file_sig cls ac_name ac_loc docblock = Flow_js.(
    let reason = Reason.mk_reason (Reason.RCustom ac_name) ac_loc in
    let component_instance = mk_instance cx reason cls in
    let props_object = Tvar.mk_where cx reason (fun tvar ->
      let use_op = Type.Op Type.UnknownUse in
      flow cx (
        component_instance,
        Type.GetPropT (use_op, reason, Type.Named (reason, "props"), tvar))
    ) in
    (* Only include own properties, so we don't suggest things like `hasOwnProperty` as potential JSX properties *)
    autocomplete_member ~exclude_proto_members:true ~ac_type:"Acjsx" cx file_sig props_object ac_name ac_loc docblock
  )

let autocomplete_get_results cx file_sig state docblock =
  let file_sig = File_sig.abstractify_locs file_sig in
  match !state with
  | Some { ac_type = Acid (env); _; } ->
    autocomplete_id cx file_sig env
  | Some { ac_name; ac_loc; ac_type = Acmem (this); } ->
    autocomplete_member ~exclude_proto_members:false ~ac_type:"Acmem" cx file_sig this ac_name ac_loc docblock
  | Some { ac_name; ac_loc; ac_type = Acjsx (cls); } ->
    autocomplete_jsx cx file_sig cls ac_name ac_loc docblock
  | None -> Ok ([], None)
