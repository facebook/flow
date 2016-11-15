(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Autocomplete_js
open Type_printer
open Utils_js

(* Details about functions to be added in json output *)
type func_param_result = {
    param_name     : string;
    param_ty       : string;
  }

type func_details_result = {
    params    : func_param_result list;
    return_ty : string;
  }

(* Results ready to be displayed to the user *)
type complete_autocomplete_result = {
    res_loc      : Loc.t;
    res_ty       : string;
    res_name     : string;
    func_details : func_details_result option;
  }

let autocomplete_result_to_json loc_preprocessor result =
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
             "params", Hh_json.JSON_Array (List.map func_param_to_json fd.params);
           ]
     | None -> Hh_json.JSON_Null
  in
  let loc = loc_preprocessor result.res_loc in
  let name = result.res_name in
  let ty = result.res_ty in
  Hh_json.JSON_Object (
    ("name", Hh_json.JSON_String name) ::
    ("type", Hh_json.JSON_String ty) ::
    ("func_details", func_details_to_json result.func_details) ::
    (Errors.deprecated_json_props_of_loc loc)
  )

let print_type cx type_ =
  if is_printed_type_parsable ~weak:true cx type_
  then string_of_t cx type_
  else ""

let rec autocomplete_create_result cx name type_ loc =
  Type.(match type_ with
  | FunT (_, _, _, {params_tlist = params;
                    params_names = pnames;
                    return_t = return; _}) ->
      let pnames =
        match pnames with
        | Some pnames -> pnames
        | None -> List.map (fun _ -> "_") params in
      let params = List.map2 (fun name type_ ->
        let param_name = parameter_name cx name type_ in
        let param_ty =
          if is_printed_param_type_parsable ~weak:true cx type_
          then string_of_param_t cx type_
          else ""
        in
        { param_name; param_ty }
      ) pnames params in
      let return = print_type cx return in
      { res_loc = loc;
        res_name = name;
        res_ty = (print_type cx type_);
        func_details = Some { params; return_ty = return } }
  | PolyT (_, sub_type) ->
      let result = autocomplete_create_result cx name sub_type loc in
      (* This is not exactly pretty but we need to replace the type to
         be sure to use the same format for poly types as print_type *)
      { res_loc = result.res_loc;
        res_name = result.res_name;
        res_ty = (print_type cx type_);
        func_details = result.func_details; }
  | _ ->
      { res_loc = loc;
        res_name = name;
        res_ty = (print_type cx type_);
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
    (* strip out members from prototypes which are implicity created for
       internal reasons *)
    not (Reason.is_internal_name key)
  ) members

let autocomplete_member
  profiling
  client_logging_context
  cx
  this
  ac_name
  ac_loc
  docblock = Flow_js.(

  let this_t = resolve_type cx this in
  (* Resolve primitive types to their internal class type. We do this to allow
     autocompletion on these too. *)
  let this_t = resolve_builtin_class cx this_t in
  let result = Autocomplete.extract_members cx this_t in

  let open Hh_json in

  let json_data_list = [
    "ac_name", JSON_String ac_name;
    "ac_loc", JSON_Object (Errors.deprecated_json_props_of_loc ac_loc);
    "loc", Reason.json_of_loc ac_loc;
    "docblock", Docblock.json_of_docblock docblock;
  ] in

  let result_str, t = Autocomplete.(match result with
    | Success _ -> "SUCCESS", this
    | SuccessModule _ -> "SUCCESS", this
    | FailureMaybeType -> "FAILURE_NULLABLE", this
    | FailureAnyType -> "FAILURE_NO_COVERAGE", this
    | FailureUnhandledType t -> "FAILURE_UNHANDLED_TYPE", t) in

  let json_data = JSON_Object (
    ("type", Debug_js.json_of_t ~depth:3 cx t)::json_data_list
  ) in
  FlowEventLogger.autocomplete_member_result
    ~client_context:client_logging_context
    ~result_str
    ~json_data
    ~profiling;

  match Autocomplete.command_result_of_member_result result with
  | Err error -> Err error
  | OK result_map ->
    OK (
      result_map
      |> autocomplete_filter_members
      |> SMap.mapi (fun name t ->
          let loc = Type.loc_of_t t in
          let gt = Type_normalizer.normalize_type cx t in
          autocomplete_create_result cx name gt loc
        )
      |> SMap.values
      |> List.rev
    )
)

(* env is all visible bound names at cursor *)
let autocomplete_id cx env =
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
        else (Scope.Entry.loc entry, name)
      in

      let type_ = Scope.Entry.actual_type entry in
      let type_ = Type_normalizer.normalize_type cx type_ in
      let result =
        autocomplete_create_result cx name type_ loc in
      result :: acc
    )
  ) env [] in
  OK result

(* Similar to autocomplete_member, except that we're not directly given an
   object type whose members we want to enumerate: instead, we are given a
   component class and we want to enumerate the members of its declared props
   type, so we need to extract that and then route to autocomplete_member. *)
let autocomplete_jsx
  profiling
  client_logging_context
  cx
  cls
  ac_name
  ac_loc
  parse_result = Flow_js.(
    let reason = Reason.mk_reason (Reason.RCustom ac_name) ac_loc in
    let component_instance = mk_instance cx reason cls in
    let props_object = mk_tvar_where cx reason (fun tvar ->
      flow cx (
        component_instance,
        Type.GetPropT (reason, Type.Named (reason, "props"), tvar))
    ) in
    autocomplete_member
      profiling
      client_logging_context
      cx
      props_object
      ac_name
      ac_loc
      parse_result
  )

let autocomplete_get_results
  profiling client_logging_context cx state parse_result =
  (* FIXME: See #5375467 *)
  Type_normalizer.suggested_type_cache := IMap.empty;
  match !state with
  | Some { ac_type = Acid (env); _; } ->
      autocomplete_id cx env
  | Some { ac_name; ac_loc; ac_type = Acmem (this); } ->
      autocomplete_member
        profiling client_logging_context cx this ac_name ac_loc
  parse_result
  | Some { ac_name; ac_loc; ac_type = Acjsx (cls); } ->
      autocomplete_jsx
        profiling client_logging_context cx cls ac_name ac_loc parse_result
  | None -> OK []
