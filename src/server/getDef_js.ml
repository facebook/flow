(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

type getdef_type =
| Gdloc of Loc.t
| Gdval of Type.t
| Gdmem of (string * Type.t)
| Gdrequire of string * Loc.t

let id state name =
  let env = Env.all_entries () in
  Scope.Entry.(match SMap.get name env with
  | Some (Value { kind = Const ConstImportBinding; general = v; _ }) ->
    (* for references to import bindings, point directly to the exports they
       resolve to (rather than to the import bindings, which would themselves in
       turn point to the exports they resolve to) *)
    state := Some (Gdval v)
  | Some (Type { type_binding_kind = ImportTypeBinding; _type = v; _ }) ->
    (* similarly for import type bindings *)
    state := Some (Gdval v)
  | Some entry ->
    state := Some (Gdloc (entry_loc entry))
  | None ->
    ()
  )

let getdef_id (state, loc1) _cx name loc2 =
  if Reason.in_range loc1 loc2
  then id state name;
  false

let getdef_lval (state, loc1) _cx name loc2 rhs =
  if Reason.in_range loc1 loc2
  then match rhs with
  | Type_inference_hooks_js.Val v ->
    state := Some (Gdval v)
  | Type_inference_hooks_js.Parent t ->
    state := Some (Gdmem (name, t))
  | Type_inference_hooks_js.Id ->
    id state name

let getdef_member (state, loc1) _cx name loc2 this_t =
  if (Reason.in_range loc1 loc2)
  then (
    state := Some (Gdmem (name, this_t))
  );
  false

let getdef_call (state, loc1) _cx name loc2 this_t =
  if (Reason.in_range loc1 loc2)
  then (
    state := Some (Gdmem (name, this_t))
  )

let getdef_require (state, user_requested_loc) _cx name require_loc =
  if (Reason.in_range user_requested_loc require_loc)
  then (
    state := Some (Gdrequire (name, require_loc))
  )

(* TODO: the uses of `resolve_type` in the implementation below are pretty
   delicate, since in many cases the resulting type lacks location
   information. Edit with caution. *)
let getdef_get_result profiling client_logging_context ~options cx state =
  match !state with
  | Some Gdloc loc -> loc
  | Some Gdval v ->
    (* Use `possible_types_of_type` instead of `resolve_type` because we're
       actually interested in the location of the resolved types. *)
    let ts = Flow_js.possible_types_of_type cx v in
    begin match ts with
    | [t] -> Type.def_loc_of_t t
    | _ -> Loc.none
    end
  | Some Gdmem (name, this) ->
      let this_t = Flow_js.resolve_type cx this in
      let member_result = Flow_js.Members.extract cx this_t in

      let result_str, t = Flow_js.Members.(match member_result with
        | Success _ -> "SUCCESS", this
        | SuccessModule _ -> "SUCCESS", this
        | FailureMaybeType -> "FAILURE_NULLABLE", this
        | FailureAnyType -> "FAILURE_NO_COVERAGE", this
        | FailureUnhandledType t -> "FAILURE_UNHANDLED_TYPE", t) in

      let json_data = Hh_json.(JSON_Object [
        "type", Debug_js.json_of_t ~depth:3 cx t;
        "gd_name", JSON_String name
      ]) in
      FlowEventLogger.get_def_member_result
        ~client_context:client_logging_context
        ~result_str
        ~json_data
        ~profiling;

      let command_result = Flow_js.Members.to_command_result member_result in
      begin match command_result with
      | Err _ -> Loc.none
      | OK result_map ->
        begin match SMap.get name result_map with
        | Some t -> Type.loc_of_t t
        | None -> Loc.none
        end
      end
  | Some Gdrequire (name, require_loc) ->
      let module_t = Flow_js.resolve_type cx (
        SMap.find_unsafe name (Context.module_map cx)
      ) in
      (* function just so we don't do the work unless it's actually needed. *)
      let get_imported_file () =
        let filename = Module_js.get_file Expensive.warn (
          Module_js.imported_module ~options
            ~node_modules_containers:!Files.node_modules_containers
            (Context.file cx) require_loc name
        ) in
        (match filename with
        | Some file -> Loc.({none with source = Some file;})
        | None -> Loc.none)
      in
      Type.(match module_t with
      (**
       * TODO: Specialized `import` hooks so that get-defs on named
       *       imports point to their actual remote def location.
       *)
      | ModuleT(_, {cjs_export; _; }) ->
          (* If we have a location for the cjs export, go there. Otherwise
           * fall back to just the top of the file *)
          let loc = match cjs_export with
            | Some t -> loc_of_t t (* This can return Loc.none *)
            | None -> Loc.none
          in
          if loc = Loc.none then
            get_imported_file ()
          else
            loc
      | DefT (_, AnyT) ->
          get_imported_file ()
      | _ -> failwith (
        spf "Internal Flow Error: Expected ModuleT for %S, but got %S!"
          name
          (string_of_ctor module_t)
        )
      )
  | None -> Loc.none

let getdef_set_hooks pos =
  let state = ref None in
  Type_inference_hooks_js.set_id_hook (getdef_id (state, pos));
  Type_inference_hooks_js.set_lval_hook (getdef_lval (state, pos));
  Type_inference_hooks_js.set_member_hook (getdef_member (state, pos));
  Type_inference_hooks_js.set_call_hook (getdef_call (state, pos));
  Type_inference_hooks_js.set_require_hook (getdef_require (state, pos));
  Type_inference_hooks_js.set_import_hook (getdef_require (state, pos));
  state

let getdef_unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()
