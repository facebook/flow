(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module File_sig = File_sig.With_Loc
open Loc_collections
open GetDefUtils

let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc

let add_ref_kind kind = Base.List.map ~f:(fun loc -> (kind, loc))

module LiteralToPropLoc : sig
  (* Returns a map from object_literal_loc to prop_loc, for all object literals which contain the
   * given property name. *)
  val make : (Loc.t, Loc.t) Ast.Program.t -> prop_name:string -> Loc.t LocMap.t
end = struct
  class locmap_builder prop_name =
    object (this)
      inherit [Loc.t LocMap.t] Object_key_visitor.visitor ~init:LocMap.empty

      method! private visit_object_key
          (literal_loc : Loc.t) (key : (Loc.t, Loc.t) Ast.Expression.Object.Property.key) =
        let open Ast.Expression.Object in
        match key with
        | Property.Identifier (prop_loc, { Ast.Identifier.name; comments = _ })
          when name = prop_name ->
          this#update_acc (fun map -> LocMap.add literal_loc prop_loc map)
        (* TODO consider supporting other property keys (e.g. literals). Also update the
         * optimization in property_access_searcher below when this happens. *)
        | _ -> ()
    end

  let make ast ~prop_name =
    let builder = new locmap_builder prop_name in
    builder#eval builder#program ast
end

let set_get_refs_hook potential_refs potential_matching_literals target_name =
  let hook ret _ctxt name loc ty =
    if name = target_name then
      (* Replace previous bindings of `loc`. We should always use the result of the last call to
       * the hook for a given location (this may no longer be relevant with the removal of
         generate-tests) *)
      potential_refs := ALocMap.add loc ty !potential_refs;
    ret
  in
  let lval_hook cx name loc = function
    (* Treat destructuring as a property access *)
    | Type_inference_hooks_js.Parent ty -> hook () cx name loc ty
    | _ -> ()
  in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    let open Type in
    match (get_object_literal_loc obj1, obj2) with
    | (Some aloc, DefT (_, _, ObjT _)) ->
      potential_matching_literals := (aloc, obj2) :: !potential_matching_literals
    | _ -> ()
  in
  Type_inference_hooks_js.set_member_hook (hook false);
  Type_inference_hooks_js.set_call_hook (hook ());
  Type_inference_hooks_js.set_lval_hook lval_hook;
  Type_inference_hooks_js.set_obj_to_obj_hook obj_to_obj_hook

(* Returns `true` iff the given type is a reference to the symbol we are interested in *)
let type_matches_locs ~reader cx ty prop_def_info name =
  let rec def_loc_matches_locs = function
    | FoundClass ty_def_locs ->
      prop_def_info
      |> Nel.exists (function
             | Object _ -> false
             | Class loc ->
               (* Only take the first extracted def loc -- that is, the one for the actual definition
                * and not overridden implementations, and compare it to the list of def locs we are
                * interested in *)
               loc = Nel.hd ty_def_locs
             )
    | FoundObject loc ->
      prop_def_info
      |> Nel.exists (function
             | Class _ -> false
             | Object def_loc -> loc = def_loc
             )
    | FoundUnion def_locs -> def_locs |> Nel.map def_loc_matches_locs |> Nel.fold_left ( || ) false
    (* TODO we may want to surface AnyType results somehow since we can't be sure whether they
     * are references or not. For now we'll leave them out. *)
    | NoDefFound
    | UnsupportedType
    | AnyType ->
      false
  in
  extract_def_loc ~reader cx ty name >>| def_loc_matches_locs

let process_prop_refs ~reader cx potential_refs file_key prop_def_info name =
  potential_refs
  |> ALocMap.bindings
  |> Base.List.map ~f:(fun (ref_loc, ty) ->
         type_matches_locs ~reader cx ty prop_def_info name >>| function
         | true -> Some (loc_of_aloc ~reader ref_loc)
         | false -> None
     )
  |> Result.all
  |> Result.map_error ~f:(fun err ->
         Printf.sprintf
           "Encountered while finding refs in `%s`: %s"
           (File_key.to_string file_key)
           err
     )
  >>| fun refs -> refs |> Base.List.filter_opt |> add_ref_kind FindRefsTypes.PropertyAccess

let property_find_refs_in_file ~reader options ast_info file_key def_info name =
  let potential_refs : Type.t ALocMap.t ref = ref ALocMap.empty in
  let potential_matching_literals : (ALoc.t * Type.t) list ref = ref [] in
  let (ast, file_sig, info) = ast_info in
  let info = Docblock.set_flow_mode_for_ide_command info in
  let local_defs =
    Nel.to_list (all_locs_of_property_def_info def_info)
    |> List.filter (fun loc -> loc.Loc.source = Some file_key)
    |> add_ref_kind FindRefsTypes.PropertyDefinition
  in
  let has_symbol = PropertyAccessSearcher.search name ast in
  if not has_symbol then
    Ok local_defs
  else (
    set_get_refs_hook potential_refs potential_matching_literals name;
    let (cx, _) = Merge_service.check_contents_context ~reader options file_key ast info file_sig in
    unset_hooks ();
    let literal_prop_refs_result =
      (* Lazy to avoid this computation if there are no potentially-relevant object literals to
       * examine *)
      let prop_loc_map = lazy (LiteralToPropLoc.make ast name) in
      let get_prop_loc_if_relevant (obj_aloc, into_type) =
        type_matches_locs ~reader cx into_type def_info name >>| function
        | false -> None
        | true ->
          let obj_loc = loc_of_aloc ~reader obj_aloc in
          LocMap.find_opt obj_loc (Lazy.force prop_loc_map)
      in
      !potential_matching_literals |> Base.List.map ~f:get_prop_loc_if_relevant |> Result.all
      >>| fun refs -> refs |> Base.List.filter_opt |> add_ref_kind FindRefsTypes.PropertyDefinition
    in
    literal_prop_refs_result >>= fun literal_prop_refs_result ->
    process_prop_refs ~reader cx !potential_refs file_key def_info name
    >>| ( @ ) local_defs
    >>| ( @ ) literal_prop_refs_result
  )

let export_find_refs_in_file ~reader ast_info file_key def_loc =
  File_sig.(
    let (_, file_sig, _) = ast_info in
    let is_relevant module_ref =
      Loc.source def_loc = file_key_of_module_ref ~reader file_key module_ref
    in
    let locs =
      List.fold_left
        begin
          fun acc require ->
          match require with
          | Require { source = (_, module_ref); require_loc; _ } ->
            if is_relevant module_ref then
              require_loc :: acc
            else
              acc
          | _ -> acc
        end
        []
        file_sig.module_sig.requires
    in
    let locs =
      if Loc.source def_loc = Some file_key then
        def_loc :: locs
      else
        locs
    in
    Ok locs
  )

let add_related_bindings file_sig scope_info refs =
  let locs = Base.List.map ~f:snd refs in
  let related_bindings = ImportExportSymbols.find_related_symbols file_sig locs in
  List.fold_left
    begin
      fun acc loc ->
      let new_refs =
        VariableFindRefs.local_find_refs scope_info loc
        |> Base.Option.value_map ~default:[] ~f:(fun ((_, refs), _) -> refs)
      in
      List.rev_append new_refs acc
    end
    refs
    related_bindings

let find_refs_in_file ~reader options ast_info scope_info file_key def_info =
  let (_, file_sig, _) = ast_info in
  let refs =
    match def_info with
    | Property (def_info, name) ->
      property_find_refs_in_file ~reader options ast_info file_key def_info name
    | CJSExport loc ->
      export_find_refs_in_file ~reader ast_info file_key loc >>| fun refs ->
      add_ref_kind FindRefsTypes.Other refs
  in
  refs >>| add_related_bindings file_sig scope_info

let find_local_refs ~reader ~options ~env ~profiling file_key ast_info scope_info loc =
  match get_def_info ~reader ~options env profiling file_key ast_info loc with
  | Error _ as err -> err
  | Ok None -> Ok None
  | Ok (Some def_info) ->
    find_refs_in_file ~reader options ast_info scope_info file_key def_info >>= fun refs ->
    Ok (Some (display_name_of_def_info def_info, refs))
