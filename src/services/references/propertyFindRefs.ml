(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module File_sig = File_sig.With_Loc
open Utils_js
open Parsing_heaps_utils
open Loc_collections
open ServerEnv
open FindRefsUtils
open GetDefUtils

let add_ref_kind kind = Base.List.map ~f:(fun loc -> (kind, loc))

module LiteralToPropLoc : sig
  (* Returns a map from object_literal_loc to prop_loc, for all object literals which contain the
   * given property name. *)
  val make : (Loc.t, Loc.t) Ast.program -> prop_name:string -> Loc.t LocMap.t
end = struct
  class locmap_builder prop_name =
    object (this)
      inherit [Loc.t LocMap.t] object_key_visitor ~init:LocMap.empty

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

let set_get_refs_hook ~reader potential_refs potential_matching_literals target_name =
  let hook ret _ctxt name loc ty =
    if name = target_name then
      (* Replace previous bindings of `loc`. We should always use the result of the last call to
       * the hook for a given location. For details see the comment on the generate_tests function
       * in flow_js.ml *)
      potential_refs := ALocMap.add loc ty !potential_refs;
    ret
  in
  let lval_hook cx name loc = function
    (* Treat destructuring as a property access *)
    | Type_inference_hooks_js.Parent ty -> hook () cx name loc ty
    | _ -> ()
  in
  let obj_to_obj_hook _ctxt obj1 obj2 =
    Type.(
      match (get_object_literal_loc ~reader obj1, obj2) with
      | (Some loc, DefT (_, _, ObjT _)) ->
        let entry = (loc, obj2) in
        potential_matching_literals := entry :: !potential_matching_literals
      | _ -> ())
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
               loc = Nel.hd ty_def_locs)
    | FoundObject loc ->
      prop_def_info
      |> Nel.exists (function
             | Class _ -> false
             | Object def_loc -> loc = def_loc)
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
         | false -> None)
  |> Result.all
  |> Result.map_error ~f:(fun err ->
         Printf.sprintf
           "Encountered while finding refs in `%s`: %s"
           (File_key.to_string file_key)
           err)
  >>| fun refs -> refs |> ListUtils.cat_maybes |> add_ref_kind FindRefsTypes.PropertyAccess

let property_find_refs_in_file ~reader options ast_info file_key def_info name =
  let potential_refs : Type.t ALocMap.t ref = ref ALocMap.empty in
  let potential_matching_literals : (Loc.t * Type.t) list ref = ref [] in
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
    set_get_refs_hook ~reader potential_refs potential_matching_literals name;
    let (cx, _) = Merge_service.merge_contents_context ~reader options file_key ast info file_sig in
    unset_hooks ();
    let literal_prop_refs_result =
      (* Lazy to avoid this computation if there are no potentially-relevant object literals to
       * examine *)
      let prop_loc_map = lazy (LiteralToPropLoc.make ast name) in
      let get_prop_loc_if_relevant (obj_loc, into_type) =
        type_matches_locs ~reader cx into_type def_info name >>| function
        | false -> None
        | true -> LocMap.find_opt obj_loc (Lazy.force prop_loc_map)
      in
      !potential_matching_literals |> Base.List.map ~f:get_prop_loc_if_relevant |> Result.all
      >>| fun refs -> refs |> ListUtils.cat_maybes |> add_ref_kind FindRefsTypes.PropertyDefinition
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
    Ok locs)

let add_related_bindings ast_info refs =
  let (ast, file_sig, _) = ast_info in
  let locs = Base.List.map ~f:snd refs in
  let related_bindings = ImportExportSymbols.find_related_symbols file_sig locs in
  List.fold_left
    begin
      fun acc loc ->
      let new_refs =
        VariableFindRefs.local_find_refs ast loc
        |> Base.Option.value_map ~default:[] ~f:(fun ((_, refs), _) -> refs)
      in
      List.rev_append new_refs acc
    end
    refs
    related_bindings

let find_refs_in_file ~reader options ast_info file_key def_info =
  let refs =
    match def_info with
    | Property (def_info, name) ->
      property_find_refs_in_file ~reader options ast_info file_key def_info name
    | CJSExport loc ->
      export_find_refs_in_file ~reader ast_info file_key loc >>| fun refs ->
      add_ref_kind FindRefsTypes.Other refs
  in
  refs >>| add_related_bindings ast_info

let find_refs_in_multiple_files ~reader genv all_deps def_info =
  let { options; workers } = genv in
  let dep_list : File_key.t list = FilenameSet.elements all_deps in
  let node_modules_containers = !Files.node_modules_containers in
  let%lwt result =
    MultiWorkerLwt.call
      workers
      ~job:
        begin
          fun _acc deps ->
          (* Yay for global mutable state *)
          Files.node_modules_containers := node_modules_containers;
          deps
          |> Base.List.map ~f:(fun dep ->
                 get_ast_result ~reader dep >>= fun ast_info ->
                 find_refs_in_file ~reader options ast_info dep def_info)
        end
      ~merge:(fun refs acc -> List.rev_append refs acc)
      ~neutral:[]
      ~next:(MultiWorkerLwt.next workers dep_list)
  in
  (* The types got a little too complicated here. Writing out the intermediate types makes it a
   * bit clearer. *)
  let result : (FindRefsTypes.single_ref list list, string) Result.t = Result.all result in
  let result : (FindRefsTypes.single_ref list, string) Result.t = result >>| List.concat in
  Lwt.return result

(* Get the source for each loc. Error if any loc is missing a source. *)
let files_of_locs (locs : Loc.t Nel.t) : (FilenameSet.t, string) result =
  let files_result =
    locs
    |> Nel.map (fun loc -> loc.Loc.source)
    |> Nel.map (Result.of_option ~error:"Expected a location with a source file")
    |> Nel.result_all
  in
  files_result >>| fun files -> Nel.to_list files |> FilenameSet.of_list

(* Error if the set is empty *)
let nel_of_filename_set (set : FilenameSet.t) : (File_key.t Nel.t, string) result =
  set
  |> FilenameSet.elements
  |> Nel.of_list
  |> Result.of_option ~error:"Expected a nonempty filename set"

(* Returns the file(s) at which we should begin looking downstream for references. *)
let roots_of_def_info def_info : (File_key.t Nel.t, string) result =
  let root_locs = all_locs_of_def_info def_info in
  files_of_locs root_locs >>= nel_of_filename_set

let deps_of_file_key ~reader genv env (file_key : File_key.t) : (FilenameSet.t, string) result Lwt.t
    =
  let { options; workers } = genv in
  File_key.to_path file_key %>>= fun path ->
  let fileinput = File_input.FileName path in
  File_input.content_of_file_input fileinput %>>| fun content ->
  let%lwt all_deps = get_all_dependents ~reader options workers env file_key content in
  Lwt.return all_deps

let deps_of_file_keys ~reader genv env (file_keys : File_key.t list) :
    (FilenameSet.t, string) result Lwt.t =
  (* We need to use map_s (rather than map_p) because we cannot interleave calls into
   * MultiWorkers. *)
  let%lwt deps_result = Lwt_list.map_s (deps_of_file_key ~reader genv env) file_keys in
  Result.all deps_result %>>| fun (deps : FilenameSet.t list) ->
  Lwt.return @@ List.fold_left FilenameSet.union FilenameSet.empty deps

let focus_and_check genv env paths =
  let%lwt (new_env, _) = Lazy_mode_utils.focus_and_check genv !env paths in
  env := new_env;
  Lwt.return_unit

let focus_and_check_filename_set genv env files =
  let paths = files |> FilenameSet.elements |> Base.List.map ~f:File_key.to_path |> Result.all in
  paths %>>| fun paths ->
  Nel.of_list paths |> Base.Option.value_map ~default:Lwt.return_unit ~f:(focus_and_check genv env)

(* Returns location pairs such that:
 * - Each location is the definition location for a property with the given
 *   name.
 * - Each pair indicates that the definition locations it contains are related.
 * - For two definition locations to be related, their enclosing object types
 *   must be related.
 * - Two object types are considered related when, in the course of typechecking
 *   the given file, we evaluate an `ObjT ~> ObjT` constraint relating the two
 *   object types.
 * - Note that this can return locations outside of the given file.
 *)
let find_related_defs_in_file ~reader options name file =
  let get_single_def_info_pairs_if_relevant cx (t1, t2) =
    map2 (extract_def_loc ~reader cx t1 name) (extract_def_loc ~reader cx t2 name) ~f:(fun x y ->
        match (x, y) with
        | (FoundObject loc1, FoundObject loc2) -> [(Object loc1, Object loc2)]
        | (FoundClass class_locs, FoundObject obj_loc) ->
          class_locs
          |> Nel.to_list
          |> Base.List.map ~f:(fun class_loc -> (Class class_loc, Object obj_loc))
        | _ -> [])
    (* TODO union types *)
  in
  let related_types : (Type.t * Type.t) list ref = ref [] in
  let hook _cx t1 t2 = related_types := (t1, t2) :: !related_types in
  Type_inference_hooks_js.set_obj_to_obj_hook hook;
  Type_inference_hooks_js.set_instance_to_obj_hook hook;
  let cx_result =
    get_ast_result ~reader file >>| fun (ast, file_sig, docblock) ->
    Merge_service.merge_contents_context ~reader options file ast docblock file_sig
  in
  unset_hooks ();
  cx_result >>= fun (cx, _) ->
  let results : ((single_def_info * single_def_info) list list, string) result =
    !related_types |> Base.List.map ~f:(get_single_def_info_pairs_if_relevant cx) |> Result.all
  in
  results >>| List.concat

(* Returns all locations which are considered related to the given definition locations. Definition
 * locations are considered related if they refer to a property with the same name, and their
 * enclosing object types appear in a subtype relationship with each other. *)
let find_related_defs ~reader genv env (def_info : property_def_info) (name : string) :
    (property_def_info, string) result Lwt.t =
  (* Outline:
   * - Create a disjoint set for definition locations
   * - Seed it with every given def_loc
   * - In all files we need to inspect:
   *   - Look for cases where both ObjTs in an ObjT ~> ObjT constraint have a
   *     property with the name we are looking for.
   *   - Add the definition locations of those properties to the disjoint set
   * - Look in the set of related definition locations that we are interested in:
   *   - If any has a source file we have not yet inspected, inspect that file
   *     and all its dependents (less those we have already inspected) as
   *     described above.
   *   - Iterate until we reach a fixed point
   *)
  let { options; workers } = genv in
  let related_defs =
    let uf = UnionFind.of_list (Nel.to_list def_info) in
    let (hd, tl) = def_info in
    List.iter (UnionFind.union uf hd) tl;
    uf
  in
  let process_files file_set =
    let node_modules_containers = !Files.node_modules_containers in
    let%lwt (result : ((single_def_info * single_def_info) list, string) result list) =
      MultiWorkerLwt.call
        workers
        ~job:
          begin
            fun _acc files ->
            Files.node_modules_containers := node_modules_containers;
            Base.List.map ~f:(find_related_defs_in_file ~reader options name) files
          end
        ~merge:List.rev_append
        ~neutral:[]
        ~next:(MultiWorkerLwt.next workers (FilenameSet.elements file_set))
    in
    Result.all result %>>| fun (pairs : (single_def_info * single_def_info) list list) ->
    List.iter (List.iter (fun (x, y) -> UnionFind.union related_defs x y)) pairs;
    Lwt.return_unit
  in
  let get_unchecked_roots current_def_info checked_files =
    current_def_info |> all_locs_of_property_def_info |> files_of_locs >>| fun roots ->
    FilenameSet.diff roots checked_files
  in
  let get_files_to_check unchecked_roots checked_files =
    let%lwt deps = deps_of_file_keys ~reader genv env (FilenameSet.elements unchecked_roots) in
    deps %>>| fun deps ->
    Lwt.return (FilenameSet.union (FilenameSet.diff deps checked_files) unchecked_roots)
  in
  let rec loop current_def_info checked_files =
    get_unchecked_roots current_def_info checked_files %>>= fun unchecked_roots ->
    if FilenameSet.is_empty unchecked_roots then
      Lwt.return (Ok current_def_info)
    else
      let%lwt result = focus_and_check_filename_set genv env unchecked_roots in
      result %>>= fun () ->
      let%lwt files_to_check = get_files_to_check unchecked_roots checked_files in
      files_to_check %>>= fun files_to_check ->
      let%lwt check_result = process_files files_to_check in
      check_result %>>= fun () ->
      let checked_files = FilenameSet.union checked_files files_to_check in
      let current_def_info =
        let updated_def_info = UnionFind.members related_defs (Nel.hd current_def_info) in
        Nel.of_list updated_def_info |> Result.of_option ~error:"Unexpected empty list"
      in
      current_def_info %>>= fun current_def_info -> loop current_def_info checked_files
  in
  loop def_info FilenameSet.empty

let find_global_refs ~reader genv env ~multi_hop def_info =
  let%lwt def_info =
    if multi_hop then
      match def_info with
      | Property (property_def_info, name) ->
        let%lwt result = find_related_defs ~reader genv env property_def_info name in
        result %>>| fun x -> Lwt.return @@ Property (x, name)
      | CJSExport _ -> Lwt.return (Ok def_info)
    else
      Lwt.return (Ok def_info)
  in
  def_info %>>= fun def_info ->
  roots_of_def_info def_info %>>= fun root_file_keys ->
  let root_file_paths_result = Nel.map File_key.to_path root_file_keys |> Nel.result_all in
  root_file_paths_result %>>= fun root_file_paths ->
  let%lwt () = focus_and_check genv env root_file_paths in
  let%lwt deps_result = deps_of_file_keys ~reader genv env (Nel.to_list root_file_keys) in
  deps_result %>>= fun deps ->
  let dependent_file_count = FilenameSet.cardinal deps in
  let relevant_files =
    Nel.to_list root_file_keys |> FilenameSet.of_list |> FilenameSet.union deps
  in
  Hh_logger.info "find-refs: searching %d dependent modules for references" dependent_file_count;
  let%lwt refs = find_refs_in_multiple_files ~reader genv relevant_files def_info in
  refs %>>| fun refs ->
  Lwt.return ((display_name_of_def_info def_info, refs), Some dependent_file_count)

let find_local_refs ~reader ~options file_key ast_info def_info =
  find_refs_in_file ~reader options ast_info file_key def_info >>= fun refs ->
  Ok (display_name_of_def_info def_info, refs)
