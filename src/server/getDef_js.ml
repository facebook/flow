(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open Utils_js

type getdef_type =
  | Gdloc of Loc.t
  | Gdval of Type.t
  | Gdmem of (string * Type.t)
  | Gdrequire of (Loc.t * string) * Loc.t

type state = {
  mutable getdef_type: getdef_type option;
  mutable getdef_require_patterns: Loc.t list;
}

(* The result of a get-def is either a final location or an intermediate
   location that is fed into a subsequent get-def. *)
type result =
  | Done of Loc.t
  | Chain of int * int (* line, column *)

let id state name =
  let env = Env.all_entries () in
  Scope.Entry.(match SMap.get name env with
  | Some (Value { kind = Const ConstImportBinding; general = v; _ }) ->
    (* for references to import bindings, point directly to the exports they
       resolve to (rather than to the import bindings, which would themselves in
       turn point to the exports they resolve to) *)
    state.getdef_type <- Some (Gdval v)
  | Some (Type { type_binding_kind = ImportTypeBinding; _type = v; _ }) ->
    (* similarly for import type bindings *)
    state.getdef_type <- Some (Gdval v)
  | Some entry ->
    state.getdef_type <- Some (Gdloc (entry_loc entry))
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
    state.getdef_type <- Some (Gdval v)
  | Type_inference_hooks_js.Parent t ->
    state.getdef_type <- Some (Gdmem (name, t))
  | Type_inference_hooks_js.Id ->
    id state name

let getdef_import (state, user_requested_loc) _cx source import_loc =
  if (Reason.in_range user_requested_loc import_loc)
  then (
    state.getdef_type <- Some (Gdrequire (source, import_loc))
  )

let getdef_require_pattern state loc =
  state.getdef_require_patterns <- loc::state.getdef_require_patterns

let extract_member_def cx this name =
  let this_t = Flow_js.resolve_type cx this in
  let member_result = Flow_js.Members.extract cx this_t in

  let result_str, t = Flow_js.Members.(match member_result with
    | Success _ -> "SUCCESS", this
    | SuccessModule _ -> "SUCCESS", this
    | FailureNullishType -> "FAILURE_NULLABLE", this
    | FailureAnyType -> "FAILURE_NO_COVERAGE", this
    | FailureUnhandledType t -> "FAILURE_UNHANDLED_TYPE", t) in

  let json_data_to_log = Hh_json.(JSON_Object [
    "type", Debug_js.json_of_t ~depth:3 cx t;
    "gd_name", JSON_String name;
    "result", JSON_String result_str;
  ]) in

  let command_result = Flow_js.Members.to_command_result member_result in
  Done begin match command_result with
  | Error _ -> Loc.none
  | Ok result_map ->
    begin match SMap.get name result_map with
    | Some (loc, t) ->
        begin match loc with
          | None -> Type.loc_of_t t
          | Some x -> x
        end
    | None -> Loc.none
    end
  end, Some json_data_to_log

let getdef_from_type_table cx loc =
  let typetable = Context.type_table cx in
  let type_info =
    Type_table.find_type_info typetable ~pred:(fun l -> Loc.contains l loc)
  in
  Option.bind type_info begin function
    | _, (_, _, Type_table.Import (name, obj_t))
    | _, (name, _, Type_table.PropertyAccess obj_t) ->
        Some (extract_member_def cx obj_t name)
    | _ -> None
  end

(* TODO: the uses of `resolve_type` in the implementation below are pretty
   delicate, since in many cases the resulting type lacks location
   information. Edit with caution. *)
let getdef_get_result_from_hooks ~options cx state =
  Ok begin match state.getdef_type with
  | Some Gdloc loc ->
    if List.exists (fun range -> Loc.contains range loc) state.getdef_require_patterns
    then
      let { Loc.line; column; _ } = loc.Loc.start in
      Chain (line, column), None
    else Done loc, None
  | Some Gdval v ->
    (* Use `possible_types_of_type` instead of `resolve_type` because we're
       actually interested in the location of the resolved types. *)
    let ts = Flow_js.possible_types_of_type cx v in
    Done begin match ts with
    | [t] -> Type.def_loc_of_t t
    | _ -> Loc.none
    end, None
  | Some Gdmem (name, this) ->
      extract_member_def cx this name
  | Some Gdrequire ((source_loc, name), require_loc) ->
      let module_t = Flow_js.resolve_type cx (Context.find_require cx source_loc) in
      (* function just so we don't do the work unless it's actually needed. *)
      let get_imported_file () =
        let filename = Module_js.get_file Expensive.warn (
          Module_js.imported_module ~options
            ~node_modules_containers:!Files.node_modules_containers
            (Context.file cx) (Nel.one require_loc) name
        ) in
        (match filename with
        | Some file -> Loc.({none with source = Some file;})
        | None -> Loc.none)
      in
      Done Type.(match module_t with
      (**
       * TODO: Specialized `import` hooks so that get-defs on named
       *       imports point to their actual remote def location.
       *)
      | ModuleT(_, {cjs_export; _; }, _) ->
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
      ), None
  | None -> Done Loc.none, None
  end

let getdef_get_result ~options cx state loc =
  match getdef_from_type_table cx loc with
  | Some x -> Ok x
  | None -> getdef_get_result_from_hooks ~options cx state

let getdef_set_hooks pos =
  let state = { getdef_type = None; getdef_require_patterns = [] } in
  Type_inference_hooks_js.set_id_hook (getdef_id (state, pos));
  Type_inference_hooks_js.set_lval_hook (getdef_lval (state, pos));
  Type_inference_hooks_js.set_import_hook (getdef_import (state, pos));
  Type_inference_hooks_js.set_require_pattern_hook (getdef_require_pattern state);
  state

let getdef_unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()

let rec get_def ~options ~workers ~env ~profiling ~depth (file_input, line, col) =
  (* Since we may call check contents repeatedly, we must prefix our timing keys *)
  Profiling_js.with_timer_prefix_lwt profiling ~prefix:(spf "GetDef%d_" depth) ~f:(fun () ->
    let filename = File_input.filename_of_file_input file_input in
    let file = File_key.SourceFile filename in
    let loc = Loc.make file line col in
    let state = getdef_set_hooks loc in
    let%lwt check_result =
      File_input.content_of_file_input file_input
      %>>= (fun content ->
        Types_js.basic_check_contents ~options ~workers ~env ~profiling content file
      )
    in
    let%lwt getdef_result =
      map_error ~f:(fun str -> str, None) check_result
      %>>= (fun (cx, _) -> Profiling_js.with_timer_lwt profiling ~timer:"GetResult" ~f:(fun () ->
        try_with_json (fun () -> Lwt.return (getdef_get_result ~options cx state loc))
      ))
    in
    let result, json_object = split_result getdef_result in
    getdef_unset_hooks ();
    match result with
    | Error e -> Lwt.return (Error e, json_object)
    | Ok ok -> (match ok with
      | Done loc -> Lwt.return (Ok loc, json_object)
      | Chain (line, col) ->
        let%lwt result, chain_json_object =
          get_def ~options ~workers ~env ~profiling ~depth:(depth+1) (file_input, line, col) in
        Lwt.return (match result with
        | Error e -> Error e, json_object
        | Ok loc' ->
          (* Chaining can sometimes lead to a dead end, due to lack of type
             information. In that case, fall back to the previous location. *)
          if loc' = Loc.none
          then (Ok loc, json_object)
          else (Ok loc', chain_json_object)
        )
      )
  )
