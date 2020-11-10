(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let ( >>= ) = Lwt_result.Infix.( >>= )

let ( >>| ) = Base.Result.( >>| )

open Utils_js
open Loc_collections

let locmap_of_bindings =
  List.fold_left
    begin
      fun map (loc, x) ->
      LocMap.add loc x map
    end
    LocMap.empty

(* Extract the loc from each ref, then sort and dedup by loc. This will have to be revisited
   if we ever need to report multiple ref kinds for a single location. *)
let sort_and_dedup refs =
  refs
  |> Base.List.map ~f:(fun ((_, loc) as reference) -> (loc, reference))
  |> locmap_of_bindings
  |> LocMap.bindings
  |> Base.List.map ~f:snd

let local_variable_refs ast_info loc =
  let (ast, _, _) = ast_info in
  match VariableFindRefs.local_find_refs ast loc with
  | None -> (None, loc)
  | Some (var_refs, local_def_loc) -> (Some var_refs, local_def_loc)

let global_property_refs ~reader ~genv ~env ~def_info ~multi_hop =
  match def_info with
  | None -> Lwt.return (Ok None)
  | Some def_info ->
    let%lwt refs = PropertyFindRefs.find_global_refs ~reader genv env ~multi_hop def_info in
    Lwt.return (refs >>| Base.Option.some)

let local_property_refs ~reader ~options ~file_key ~ast_info ~def_info =
  match def_info with
  | None -> Lwt.return (Ok None)
  | Some def_info ->
    let refs = PropertyFindRefs.find_local_refs ~reader ~options file_key ast_info def_info in
    Lwt.return (refs >>| Base.Option.some)

let find_global_refs ~reader ~genv ~env ~profiling ~file_input ~line ~col ~multi_hop =
  let options = genv.ServerEnv.options in
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.cursor (Some file_key) line col in
  let%lwt result =
    File_input.content_of_file_input file_input %>>= fun content ->
    FindRefsUtils.compute_ast_result options file_key content %>>= fun ast_info ->
    (* Start by running local variable find references *)
    let (var_refs, loc) = local_variable_refs ast_info loc in
    (* Run get-def on the local loc *)
    GetDefUtils.get_def_info ~reader ~options !env profiling file_key ast_info loc
    >>= fun def_info ->
    (* Then run property find-refs *)
    global_property_refs ~reader ~genv ~env ~def_info ~multi_hop >>= fun prop_refs ->
    (* If property find-refs returned nothing (for example if we are importing from an untyped
      * module), then fall back on the local refs we computed earlier. *)
    Lwt.return
      (Ok
         (match prop_refs with
         | Some _ -> prop_refs
         | None ->
           begin
             match var_refs with
             | Some var_refs -> Some (var_refs, None)
             | None -> None
           end))
  in
  let (result, dep_count) =
    match result with
    | Ok (Some ((name, refs), dep_count)) -> (Ok (Some (name, sort_and_dedup refs)), dep_count)
    | Ok None -> (Ok None, None)
    | Error err -> (Error err, None)
  in
  Lwt.return (result, dep_count)

let find_local_refs ~reader ~options ~env ~profiling ~file_input ~line ~col =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.cursor (Some file_key) line col in
  File_input.content_of_file_input file_input %>>= fun content ->
  FindRefsUtils.compute_ast_result options file_key content %>>= fun ast_info ->
  (* Start by running local variable find references *)
  let (var_refs, loc) = local_variable_refs ast_info loc in
  (* Run get-def on the local loc *)
  GetDefUtils.get_def_info ~reader ~options env profiling file_key ast_info loc >>= fun def_info ->
  (* Then run property find-refs *)
  local_property_refs ~reader ~options ~file_key ~ast_info ~def_info >>= fun prop_refs ->
  (* If property find-refs returned nothing (for example if we are importing from an untyped
    * module), then fall back on the local refs we computed earlier. *)
  let refs = Base.Option.first_some prop_refs var_refs in
  let refs =
    match refs with
    | Some (name, refs) -> Some (name, sort_and_dedup refs)
    | None -> None
  in
  Lwt.return (Ok refs)
