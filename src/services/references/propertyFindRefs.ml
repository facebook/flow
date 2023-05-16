(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Loc_collections
open GetDefUtils

let ( >>= ) = Base.Result.( >>= )

let ( >>| ) = Base.Result.( >>| )

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

module Potential_refs_search = struct
  exception Found_import

  class searcher ~(target_name : string) ~(potential_refs : Type.t ALocMap.t ref) =
    object (_this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot x = x

      method on_type_annot x = x

      method! import_declaration loc decl =
        let open Flow_ast.Statement.ImportDeclaration in
        try super#import_declaration loc decl with
        | Found_import ->
          let { source = ((_, module_t), _); _ } = decl in
          (* Replace previous bindings of `loc`. We should always use the result of the last call to
             the hook for a given location (this may no longer be relevant with the removal of
             generate-tests) *)
          potential_refs := ALocMap.add loc module_t !potential_refs;
          decl

      method! import_named_specifier ~import_kind:_ specifier =
        let open Flow_ast.Statement.ImportDeclaration in
        let { kind = _; local; remote } = specifier in
        let (_, { Flow_ast.Identifier.name; _ }) = Base.Option.value ~default:remote local in
        if name = target_name then
          raise Found_import
        else
          specifier

      method! member expr =
        let open Flow_ast.Expression.Member in
        let { _object = ((_, ty), _); property; comments = _ } = expr in
        (match property with
        | PropertyIdentifier ((loc, _), { Flow_ast.Identifier.name; _ })
        | PropertyPrivateName (loc, { Flow_ast.PrivateName.name; _ })
          when name = target_name ->
          potential_refs := ALocMap.add loc ty !potential_refs
        | PropertyIdentifier _
        | PropertyPrivateName _
        | PropertyExpression _ ->
          ());
        super#member expr

      method! pattern ?kind expr =
        let ((_, ty), patt) = expr in
        let _ =
          match patt with
          | Ast.Pattern.Object { Ast.Pattern.Object.properties; _ } ->
            List.iter
              (fun prop ->
                let open Ast.Pattern.Object in
                match prop with
                | Property (_, { Property.key; _ }) ->
                  (match key with
                  | Property.Identifier ((loc, _), { Ast.Identifier.name; _ })
                  | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String name; _ }) ->
                    if name = target_name then potential_refs := ALocMap.add loc ty !potential_refs;
                    ()
                  | Property.Computed _
                  | Property.Literal _ ->
                    ())
                | RestElement _ -> ())
              properties
          | Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ } ->
            let ((loc, ty), { Flow_ast.Identifier.name = id_name; _ }) = name in
            (* If the location is already in the map, it was set by a parent *)
            if id_name = target_name && (not @@ ALocMap.mem loc !potential_refs) then
              potential_refs := ALocMap.add loc ty !potential_refs;
            ()
          | Ast.Pattern.Array _
          | Ast.Pattern.Expression _ ->
            ()
        in
        super#pattern ?kind expr
    end

  let search ~target_name ~potential_refs ast =
    let s = new searcher ~target_name ~potential_refs in
    let _ = s#program ast in
    ()
end

(* Returns `true` iff the given type is a reference to the symbol we are interested in *)
let type_matches_locs ~loc_of_aloc cx ty prop_def_info name =
  let rec def_loc_matches_locs = function
    | FoundClass ty_def_locs ->
      prop_def_info
      |> Nel.exists (function
             | ObjectProperty _ -> false
             | ClassProperty loc ->
               (* Only take the first extracted def loc -- that is, the one for the actual definition
                * and not overridden implementations, and compare it to the list of def locs we are
                * interested in *)
               loc = Nel.hd ty_def_locs
             )
    | FoundObject loc ->
      prop_def_info
      |> Nel.exists (function
             | ClassProperty _ -> false
             | ObjectProperty def_loc -> loc = def_loc
             )
    | FoundUnion def_locs -> def_locs |> Nel.map def_loc_matches_locs |> Nel.fold_left ( || ) false
    (* TODO we may want to surface AnyType results somehow since we can't be sure whether they
     * are references or not. For now we'll leave them out. *)
    | NoDefFound
    | UnsupportedType
    | AnyType ->
      false
  in
  extract_def_loc ~loc_of_aloc cx ty name >>| def_loc_matches_locs

let get_loc_of_def_info ~cx ~loc_of_aloc ~obj_to_obj_map prop_def_info =
  let prop_obj_locs =
    Nel.fold_left
      (fun acc def_info ->
        match def_info with
        | ClassProperty _ -> acc
        | ObjectProperty def_loc -> Loc_collections.LocSet.add def_loc acc)
      Loc_collections.LocSet.empty
      prop_def_info
  in
  (* Iterates all the map prop values. If any match prop_def_info, add the obj loc to the result *)
  Loc_collections.LocMap.fold
    (fun loc props_tmap_set result ->
      Type.Properties.Set.fold
        (fun props_id result' ->
          let props = Context.find_props cx props_id in
          NameUtils.Map.fold
            (fun _name prop result'' ->
              match Type.Property.read_loc prop with
              | Some aloc when Loc_collections.LocSet.mem (loc_of_aloc aloc) prop_obj_locs ->
                loc :: result''
              | _ -> result'')
            props
            result')
        props_tmap_set
        result)
    obj_to_obj_map
    []

let process_prop_refs ~loc_of_aloc cx potential_refs file_key prop_def_info name =
  potential_refs
  |> ALocMap.bindings
  |> Base.List.map ~f:(fun (ref_loc, ty) ->
         type_matches_locs ~loc_of_aloc cx ty prop_def_info name >>| function
         | true -> Some (loc_of_aloc ref_loc)
         | false -> None
     )
  |> Base.Result.all
  |> Base.Result.map_error ~f:(fun err ->
         Printf.sprintf
           "Encountered while finding refs in `%s`: %s"
           (File_key.to_string file_key)
           err
     )
  >>| fun refs -> refs |> Base.List.filter_opt |> add_ref_kind FindRefsTypes.PropertyAccess

let property_find_refs_in_file ~loc_of_aloc ast_info type_info file_key def_info name =
  let potential_refs : Type.t ALocMap.t ref = ref ALocMap.empty in
  let (Types_js_types.Typecheck_artifacts { cx; typed_ast; obj_to_obj_map }) = type_info in
  let (ast, _file_sig, _info) = ast_info in
  let local_defs =
    Nel.to_list (all_locs_of_property_def_info def_info)
    |> List.filter (fun loc -> loc.Loc.source = Some file_key)
    |> add_ref_kind FindRefsTypes.PropertyDefinition
  in
  let has_symbol = PropertyAccessSearcher.search name ast in
  if not has_symbol then
    Ok local_defs
  else (
    Potential_refs_search.search ~target_name:name ~potential_refs typed_ast;
    let literal_prop_refs_result =
      (* Lazy to avoid this computation if there are no potentially-relevant object literals to
       * examine *)
      let prop_loc_map = lazy (LiteralToPropLoc.make ast ~prop_name:name) in

      get_loc_of_def_info ~cx ~loc_of_aloc ~obj_to_obj_map def_info
      |> List.filter_map (fun obj_loc -> LocMap.find_opt obj_loc (Lazy.force prop_loc_map))
      |> add_ref_kind FindRefsTypes.PropertyDefinition
    in

    process_prop_refs ~loc_of_aloc cx !potential_refs file_key def_info name
    >>| ( @ ) local_defs
    >>| ( @ ) literal_prop_refs_result
  )

let find_local_refs ~reader file_key ast_info type_info loc =
  let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
  match get_property_def_info ~loc_of_aloc type_info loc with
  | Error _ as err -> err
  | Ok None -> Ok None
  | Ok (Some (def_info, name)) ->
    property_find_refs_in_file ~loc_of_aloc ast_info type_info file_key def_info name
    >>= fun refs -> Ok (Some refs)
