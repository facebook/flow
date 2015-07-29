(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Autocomplete_js
open Constraint_js
open Utils

module Json = Hh_json

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
    Json.JAssoc [
      "name", Json.JString param.param_name;
      "type", Json.JString param.param_ty;
    ]
  in
  let func_details_to_json details =
    match details with
     | Some fd -> Json.JAssoc [
             "return_type", Json.JString fd.return_ty;
             "params", Json.JList (List.map func_param_to_json fd.params);
           ]
     | None -> Json.JNull
  in
  let loc = loc_preprocessor result.res_loc in
  let name = result.res_name in
  let ty = result.res_ty in
  Json.JAssoc (
    ("name", Json.JString name) ::
    ("type", Json.JString ty) ::
    ("func_details", func_details_to_json result.func_details) ::
    (Errors_js.json_of_loc loc)
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
  | PolyT (type_params, sub_type) ->
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
  SMap.filter (fun key value ->
    (* This is really for being better safe than sorry. It shouldn't happen. *)
    not (is_autocomplete key)
    &&
    (* filter out constructor, it shouldn't be called manually *)
    not (key = "constructor")
    &&
    (* strip out members from prototypes which are implicity created for
       internal reasons *)
    not (Reason_js.is_internal_name key)
  ) members

let autocomplete_member cx this = Flow_js.(
  let this_t = resolve_type cx this in
  (* Resolve primitive types to their internal class type. We do this to allow
     autocompletion on these too. *)
  let this_t = resolve_builtin_class cx this_t in
  let result = Autocomplete.extract_members cx this_t in

  let result_str, json_data = Autocomplete.(Hh_json.(match result with
    | Success _ -> "SUCCESS", JAssoc []
    | FailureMaybeType -> "FAILURE_NULLABLE", JAssoc []
    | FailureUnhandledType t -> "FAILURE_UNHANDLED_TYPE", JAssoc [
      "type", Constraint_js.json_of_t ~depth:3 cx t;
    ])) in
  FlowEventLogger.autocomplete_member_result result_str json_data;

  let result_map = Autocomplete.map_of_member_result result in

  let result_map = autocomplete_filter_members result_map in
  let result_map = SMap.mapi (fun name t ->
      let loc = loc_of_t t in
      let gt = printified_type cx t in
      autocomplete_create_result cx name gt loc
    ) result_map in
  List.rev (SMap.values result_map)
)

let autocomplete_id cx env =
  Utils.SMap.fold Scope.(fun key value acc ->
      (* Filter out internal environment variables except for this and
         super. *)
      let is_this = key = (Reason_js.internal_name "this") in
      let is_super = key = (Reason_js.internal_name "super") in
      if not (is_this || is_super) && Reason_js.is_internal_name key
      then acc
      else (
        let (loc, name) =
          (* renaming of this/super *)
          if is_this
          then (None, "this")
          else if is_super
          then (None, "super")
          else (value.def_loc, key) in
        let loc = match loc with
          | Some loc -> loc
          | None -> Loc.none
        in

        let type_ = Flow_js.printified_type cx value.specific in
        let result =
          autocomplete_create_result cx name type_ loc in
        result :: acc
      )
    ) env []

let autocomplete_get_results cx state =
  (* FIXME: See #5375467 *)
  Flow_js.suggested_type_cache := IMap.empty;
  match !state with
  | Some Acid (env) ->
      autocomplete_id cx env
  | Some Acmem (this) ->
      autocomplete_member cx this
  | _ -> []
