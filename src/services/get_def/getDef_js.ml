(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base.Result
open Utils_js
open Parsing_heaps_utils

module Get_def_result = struct
  type t =
    (* the final location of the definition *)
    | Def of Loc.t
    (* if an intermediate get-def failed, return partial progress and the error message *)
    | Partial of Loc.t * string
    (* the input loc didn't point at anything you can call get-def on *)
    | Bad_loc
    (* an unexpected, internal error *)
    | Def_error of string
end

open Get_def_result

let extract_member_def ~reader cx this name =
  match Members.extract cx this |> Members.to_command_result with
  | Ok result_map ->
    (match SMap.find_opt name result_map with
    | Some (None, t) -> Ok (TypeUtil.loc_of_t t |> loc_of_aloc ~reader)
    | Some (Some x, _) -> Ok (loc_of_aloc ~reader x)
    | None -> Error (spf "failed to find member %s in members map" name))
  | Error msg -> Error msg

(* turns typed AST into normal AST so we can run Scope_builder on it *)
(* TODO(vijayramamurthy): make scope builder polymorphic *)
class type_killer (reader : Parsing_heaps.Reader.reader) =
  object
    inherit [ALoc.t, ALoc.t * Type.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot x = loc_of_aloc ~reader x

    method on_type_annot (x, _) = loc_of_aloc ~reader x
  end

let rec process_request ~options ~reader ~cx ~is_legit_require ~typed_ast :
    Get_def_request.t -> (Loc.t, string) result = function
  | Get_def_request.Identifier { name = _; loc = aloc; type_ } ->
    let loc = loc_of_aloc ~reader aloc in
    let ast = (new type_killer reader)#program typed_ast in
    let scope_info = Scope_builder.program ast in
    let all_uses = Scope_api.With_Loc.all_uses scope_info in
    Loc_collections.(
      let matching_uses = LocSet.filter (fun use -> Loc.contains use loc) all_uses in
      begin
        match LocSet.elements matching_uses with
        | [use] ->
          let def = Scope_api.With_Loc.def_of_use scope_info use in
          let def_loc = def.Scope_api.With_Loc.Def.locs |> Nel.hd in
          Ok def_loc
        | [] ->
          process_request
            ~options
            ~reader
            ~cx
            ~is_legit_require
            ~typed_ast
            (Get_def_request.Type type_)
        | _ :: _ :: _ -> Error "Scope builder found multiple matching identifiers"
      end)
  | Get_def_request.(Member { prop_name = name; object_source }) ->
    let obj_t =
      match object_source with
      | Get_def_request.ObjectType t -> t
      | Get_def_request.ObjectRequireLoc loc -> Context.find_require cx loc
    in
    extract_member_def ~reader cx obj_t name
  | Get_def_request.Type v ->
    let rec loop =
      let open Type in
      function
      | OpenT _ as t ->
        (match Flow_js.possible_types_of_type cx t with
        | [t'] -> loop t'
        | [] -> Error "No possible types"
        | _ :: _ -> Error "More than one possible type")
      | AnnotT (_, t, _)
      | DefT (_, _, TypeT ((ImportTypeofKind | ImportClassKind | ImportEnumKind), t)) ->
        loop t
      | t -> Ok (TypeUtil.def_loc_of_t t |> loc_of_aloc ~reader)
    in
    loop v
  | Get_def_request.Require ((source_loc, name), require_loc) ->
    let module_t = Context.find_require cx source_loc |> Members.resolve_type cx in
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
             require_loc
             name)
      in
      match filename with
      | Some file -> Ok Loc.{ none with source = Some file }
      | None -> Error (spf "Failed to find imported file %s" name)
    in
    Type.(
      (match module_t with
      | ModuleT (_, { cjs_export; _ }, _) ->
        (* If we have a location for the cjs export, go there. Otherwise
          * fall back to just the top of the file *)
        let loc =
          match cjs_export with
          | Some t -> TypeUtil.loc_of_t t |> loc_of_aloc ~reader (* This can return Loc.none *)
          | None -> Loc.none
        in
        if loc = Loc.none then
          get_imported_file ()
        else
          Ok loc
      | AnyT _ -> get_imported_file ()
      | _ ->
        Error
          (spf
             "Internal Flow Error: Expected ModuleT for %S, but got %S!"
             name
             (string_of_ctor module_t))))
  | Get_def_request.JsxAttribute { component_t; name; loc } ->
    let reason = Reason.mk_reason (Reason.RCustom name) loc in
    let props_object =
      Tvar.mk_where cx reason (fun tvar ->
          let use_op = Type.Op Type.UnknownUse in
          Flow_js.flow cx (component_t, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar)))
    in
    process_request
      ~options
      ~reader
      ~cx
      ~is_legit_require
      ~typed_ast
      Get_def_request.(Member { prop_name = name; object_source = ObjectType props_object })

let get_def ~options ~reader ~cx ~file_sig ~typed_ast requested_loc =
  let require_aloc_map = File_sig.With_ALoc.(require_loc_map file_sig.module_sig) in
  let is_legit_require source_aloc =
    SMap.exists
      (fun _ alocs ->
        Nel.exists (fun aloc -> loc_of_aloc ~reader aloc = loc_of_aloc ~reader source_aloc) alocs)
      require_aloc_map
  in
  let rec loop req_loc =
    let open Get_def_process_location in
    match process_location ~is_legit_require ~typed_ast req_loc with
    | OwnDef aloc -> Def (loc_of_aloc ~reader aloc)
    | Request request ->
      (match process_request ~options ~reader ~cx ~is_legit_require ~typed_ast request with
      | Ok res_loc ->
        (* two scenarios where we stop trying to recur:
           - when req_loc = res_loc, meaning we've reached a fixed point so
             continuing would make us infinite loop
           - when res_loc is not in the file the request originated from, meaning
             the typed_ast we have is the wrong one to recur with *)
        if Loc.equal req_loc res_loc || Loc.(res_loc.source <> requested_loc.source) then
          Def res_loc
        else (
          match loop res_loc with
          | Bad_loc -> Def res_loc
          | Def_error msg -> Partial (res_loc, msg)
          | (Def _ | Partial _) as res -> res
        )
      | Error msg -> Def_error msg)
    | LocNotFound -> Bad_loc
  in
  loop requested_loc
