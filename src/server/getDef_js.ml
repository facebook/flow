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
| Gdmem of (string * Type.t)
| Gdrequire of string * Loc.t

let getdef_id (state, loc1) _cx name loc2 =
  if Reason.in_range loc1 loc2
  then (
    let env = Env.all_entries () in
    match SMap.get name env with
    | Some entry ->
        state := Some (Gdloc (Scope.Entry.assign_loc entry))
    | None ->
      ());
  false

let getdef_lval (state, loc1) cx name loc2 rhs =
  if Reason.in_range loc1 loc2
  then (match rhs with
    | Type_inference_hooks_js.RHSLoc loc ->
      state := Some (Gdloc loc)
    | Type_inference_hooks_js.RHSType t ->
      state := Some (Gdmem (name, t))
    | Type_inference_hooks_js.NoRHS ->
      let _ = getdef_id (state, loc1) cx name loc2 in
      ()
  )

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

let getdef_get_result ~options cx state =
  match !state with
  | Some Gdloc loc -> loc
  | Some Gdmem (name, this) ->
      let this_t = Flow_js.resolve_type cx this in
      let member_result = Flow_js.Autocomplete.extract_members cx this_t in
      let command_result =
        Flow_js.Autocomplete.command_result_of_member_result member_result in
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
        let filename = Module_js.get_module_file Expensive.warn (
          Module_js.imported_module ~options cx require_loc name
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
      | AnyT _ ->
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
