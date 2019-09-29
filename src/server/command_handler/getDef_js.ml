(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open Utils_js
open Parsing_heaps_utils

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
  | Chain of int * int

(* line, column *)

let id ~reader state name =
  let env = Env.all_entries () in
  Scope.Entry.(
    match SMap.get name env with
    | Some (Value { kind = Const ConstImportBinding; general = v; _ }) ->
      (* for references to import bindings, point directly to the exports they
       resolve to (rather than to the import bindings, which would themselves in
       turn point to the exports they resolve to) *)
      state.getdef_type <- Some (Gdval v)
    | Some (Type { type_binding_kind = ImportTypeBinding; type_ = v; _ }) ->
      (* similarly for import type bindings *)
      state.getdef_type <- Some (Gdval v)
    | Some entry -> state.getdef_type <- Some (Gdloc (entry_loc entry |> loc_of_aloc ~reader))
    | None -> ())

let getdef_id ~reader (state, loc1) _cx name loc2 =
  let loc2 = loc_of_aloc ~reader loc2 in
  if Reason.in_range loc1 loc2 then id ~reader state name;
  false

let getdef_lval ~reader (state, loc1) _cx name loc2 rhs =
  let loc2 = loc_of_aloc ~reader loc2 in
  if Reason.in_range loc1 loc2 then
    match rhs with
    | Type_inference_hooks_js.Val v -> state.getdef_type <- Some (Gdval v)
    | Type_inference_hooks_js.Parent t -> state.getdef_type <- Some (Gdmem (name, t))
    | Type_inference_hooks_js.Id -> id ~reader state name

let getdef_import ~reader (state, user_requested_loc) _cx (loc, name) import_loc =
  let source = (loc_of_aloc ~reader loc, name) in
  let import_loc = loc_of_aloc ~reader import_loc in
  if Reason.in_range user_requested_loc import_loc then
    state.getdef_type <- Some (Gdrequire (source, import_loc))

let getdef_require_pattern ~reader state loc =
  let loc = loc_of_aloc ~reader loc in
  state.getdef_require_patterns <- loc :: state.getdef_require_patterns

let extract_member_def ~reader cx this name =
  let member_result = Members.extract cx this in
  let (result_str, t) =
    Members.(
      match member_result with
      | Success _ -> ("SUCCESS", this)
      | SuccessModule _ -> ("SUCCESS", this)
      | FailureNullishType -> ("FAILURE_NULLABLE", this)
      | FailureAnyType -> ("FAILURE_NO_COVERAGE", this)
      | FailureUnhandledType t -> ("FAILURE_UNHANDLED_TYPE", t)
      | FailureUnhandledMembers t -> ("FAILURE_UNHANDLED_MEMBERS", t))
  in
  let json_data_to_log =
    Hh_json.(
      JSON_Object
        [
          ("type", Debug_js.json_of_t ~depth:3 cx t);
          ("gd_name", JSON_String name);
          ("result", JSON_String result_str);
        ])
  in
  let command_result = Members.to_command_result member_result in
  ( Done
      begin
        match command_result with
        | Error _ -> Loc.none
        | Ok result_map ->
          begin
            match SMap.get name result_map with
            | Some (loc, t) ->
              begin
                match loc with
                | None -> Type.loc_of_t t |> loc_of_aloc ~reader
                | Some x -> loc_of_aloc ~reader x
              end
            | None -> Loc.none
          end
      end,
    Some json_data_to_log )

let getdef_from_typed_ast ~reader cx typed_ast loc =
  match Typed_ast_utils.find_get_def_info typed_ast loc with
  | Some { Typed_ast_utils.get_def_prop_name = name; get_def_object_source } ->
    let obj_t =
      match get_def_object_source with
      | Typed_ast_utils.GetDefType t -> t
      | Typed_ast_utils.GetDefRequireLoc loc -> Context.find_require cx loc
    in
    Some (extract_member_def ~reader cx obj_t name)
  | _ -> None

(* TODO: the uses of `resolve_type` in the implementation below are pretty
   delicate, since in many cases the resulting type lacks location
   information. Edit with caution. *)
let getdef_get_result_from_hooks ~options ~reader cx state =
  Ok
    begin
      match state.getdef_type with
      | Some (Gdloc loc) ->
        if List.exists (fun range -> Loc.contains range loc) state.getdef_require_patterns then
          let { Loc.line; column; _ } = loc.Loc.start in
          (Chain (line, column), None)
        else
          (Done loc, None)
      | Some (Gdval v) ->
        (* Use `possible_types_of_type` instead of `resolve_type` because we're
       actually interested in the location of the resolved types. *)
        let ts = Flow_js.possible_types_of_type cx v in
        ( Done
            begin
              match ts with
              | [t] -> Type.def_loc_of_t t |> loc_of_aloc ~reader
              | _ -> Loc.none
            end,
          None )
      | Some (Gdmem (name, this)) -> extract_member_def ~reader cx this name
      | Some (Gdrequire ((source_loc, name), require_loc)) ->
        let module_t =
          ALoc.of_loc source_loc |> Context.find_require cx |> Members.resolve_type cx
        in
        (* function just so we don't do the work unless it's actually needed. *)
        let get_imported_file () =
          let filename =
            Module_heaps.Reader.get_file
              ~reader
              ~audit:Expensive.warn
              (Module_js.imported_module
                 ~options
                 ~reader:(Abstract_state_reader.State_reader reader)
                 ~node_modules_containers:!Files.node_modules_containers
                 (Context.file cx)
                 (Nel.one (ALoc.of_loc require_loc))
                 name)
          in
          match filename with
          | Some file -> Loc.{ none with source = Some file }
          | None -> Loc.none
        in
        ( Done
            Type.(
              match module_t with
              (*
               * TODO: Specialized `import` hooks so that get-defs on named
               *       imports point to their actual remote def location.
               *)
              | ModuleT (_, { cjs_export; _ }, _) ->
                (* If we have a location for the cjs export, go there. Otherwise
                 * fall back to just the top of the file *)
                let loc =
                  match cjs_export with
                  | Some t -> loc_of_t t |> loc_of_aloc ~reader
                  (* This can return Loc.none *)
                  | None -> Loc.none
                in
                if loc = Loc.none then
                  get_imported_file ()
                else
                  loc
              | AnyT _ -> get_imported_file ()
              | _ ->
                failwith
                  (spf
                     "Internal Flow Error: Expected ModuleT for %S, but got %S!"
                     name
                     (string_of_ctor module_t))),
          None )
      | None -> (Done Loc.none, None)
    end

let getdef_get_result ~options ~reader cx typed_ast state loc =
  match getdef_from_typed_ast ~reader cx typed_ast loc with
  | Some x -> Ok x
  | None -> getdef_get_result_from_hooks ~options ~reader cx state

let getdef_set_hooks ~reader pos =
  let state = { getdef_type = None; getdef_require_patterns = [] } in
  Type_inference_hooks_js.set_id_hook (getdef_id ~reader (state, pos));
  Type_inference_hooks_js.set_lval_hook (getdef_lval ~reader (state, pos));
  Type_inference_hooks_js.set_import_hook (getdef_import ~reader (state, pos));
  Type_inference_hooks_js.set_require_pattern_hook (getdef_require_pattern ~reader state);
  state

let getdef_unset_hooks () = Type_inference_hooks_js.reset_hooks ()

let rec get_def ~options ~reader ~env ~profiling ~depth (file_input, line, col) =
  let filename = File_input.filename_of_file_input file_input in
  let file = File_key.SourceFile filename in
  let loc = Loc.make file line col in
  let state = getdef_set_hooks ~reader loc in
  let%lwt check_result =
    File_input.content_of_file_input file_input
    %>>= (fun content -> Types_js.basic_check_contents ~options ~env ~profiling content file)
  in
  let%lwt getdef_result =
    map_error ~f:(fun str -> (str, None)) check_result
    %>>= fun (cx, _, _, typed_ast) ->
    Profiling_js.with_timer_lwt profiling ~timer:"GetResult" ~f:(fun () ->
        try_with_json (fun () ->
            Lwt.return (getdef_get_result ~reader ~options cx typed_ast state loc)))
  in
  let (result, json_object) = split_result getdef_result in
  getdef_unset_hooks ();
  match result with
  | Error e -> Lwt.return (Error e, json_object)
  | Ok ok ->
    (match ok with
    | Done loc -> Lwt.return (Ok loc, json_object)
    | Chain (line, col) ->
      let%lwt (result, chain_json_object) =
        get_def ~options ~reader ~env ~profiling ~depth:(depth + 1) (file_input, line, col)
      in
      Lwt.return
        (match result with
        | Error e -> (Error e, json_object)
        | Ok loc' ->
          (* Chaining can sometimes lead to a dead end, due to lack of type
           information. In that case, fall back to the previous location. *)
          if loc' = Loc.none then
            (Ok loc, json_object)
          else
            (Ok loc', chain_json_object)))
