(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
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
    res_pos      : Pos.t;
    res_ty       : string;
    res_name     : string;
    func_details : func_details_result option;
  }

let autocomplete_result_to_json result =
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
  let pos = result.res_pos in
  let name = result.res_name in
  let ty = result.res_ty in
  Json.JAssoc (
    ("name", Json.JString name) ::
    ("type", Json.JString ty) ::
    ("func_details", func_details_to_json result.func_details) ::
    (Errors_js.pos_to_json pos)
  )

let rec autocomplete_create_result cx name type_ pos =
  Type.(match type_ with
  | FunT (_, _, _, {params_tlist = params;
                    params_names = pnames;
                    return_t = return; _}) ->
      let pnames =
        match pnames with
        | Some pnames -> pnames
        | None -> List.map (fun _ -> "_") params in
      let params = List.map2 (fun name type_ ->
        { param_name = name; param_ty = (string_of_t cx type_) }
      ) pnames params in
      let return = string_of_t cx return in
      { res_pos = pos;
        res_name = name;
        res_ty = (string_of_t cx type_);
        func_details = Some { params; return_ty = return } }
  | PolyT (type_params, sub_type) ->
      let result = autocomplete_create_result cx name sub_type pos in
      (* This is not exactly pretty but we need to replace the type to
         be sure to use the same format for poly types as string_of_t_ *)
      { res_pos = result.res_pos;
        res_name = result.res_name;
        res_ty = (string_of_t cx type_);
        func_details = result.func_details; }
  | _ ->
      { res_pos = pos;
        res_name = name;
        res_ty = (string_of_t cx type_);
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

let autocomplete_member cx this =
  let this_t = Flow_js.resolve_type cx this in
  (* Resolve primitive types to their internal class type. We do this to allow
     autocompletion on these too. *)
  let this_t = Flow_js.resolve_builtin_class cx this_t in
  let result_map = Flow_js.extract_members cx this_t in
  let result_map = autocomplete_filter_members result_map in
  let result_map = SMap.mapi (fun name t ->
      let pos = pos_of_t t in
      let gt = Flow_js.get_ground_type cx t in
      autocomplete_create_result cx name gt pos
    ) result_map in
  List.rev (SMap.values result_map)

let autocomplete_id cx env =
  Utils.SMap.fold (fun key value acc ->
      (* filter out internal return variable *)
      if key = (Reason_js.internal_name "return")
      then acc
      else (
        let (loc, name) =
          (* renaming of this/super *)
          if key = (Reason_js.internal_name "this")
          then (None, "this")
          else if key = (Reason_js.internal_name "super")
          then (None, "super")
          else (value.def_loc, key) in
        let pos = match loc with
          | Some loc -> Reason_js.pos_of_loc loc
          | None -> Pos.none
        in

        let type_ = Flow_js.get_ground_type cx value.specific in
        let result =
          autocomplete_create_result cx name type_ pos in
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
