(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let (>>|) = Core_result.(>>|)

open Utils_js

let locmap_of_bindings =
  List.fold_left begin fun map (loc, x) ->
    LocMap.add loc x map
  end LocMap.empty

let sort_and_dedup =
  Core_result.map ~f:begin
    Option.map ~f:begin fun (name, refs) ->
      let refs =
        (* Extract the loc from each ref, then sort and dedup by loc. This will have to be revisited
         * if we ever need to report multiple ref kinds for a single location. *)
        refs
        |> List.map (fun ((_, loc) as reference) -> (loc, reference))
        |> locmap_of_bindings
        |> LocMap.bindings
        |> List.map snd
      in
      name, refs
    end
  end

let find_refs ~genv ~env ~profiling ~file_input ~line ~col ~global ~multi_hop =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.make file_key line col in
  match File_input.content_of_file_input file_input with
  | Error err -> Lwt.return (Error err, None)
  | Ok content ->
    let%lwt result =
      FindRefsUtils.compute_ast_result file_key content %>>= fun (ast, file_sig, _) ->
      let property_find_refs start_loc =
        PropertyFindRefs.find_refs genv env ~profiling ~content file_key start_loc ~global ~multi_hop
      in
      (* Start by running local variable find references *)
      match VariableFindRefs.local_find_refs ast loc with
      (* Got nothing from local variable find-refs, try object property find-refs *)
      | None -> property_find_refs loc
      | Some ((name, local_refs), local_def_loc) ->
        (* We got something from local variable find-refs -- now let's check if it's an exported
         * symbol *)
        let start_loc = match ImportExportSymbols.find_related_symbol file_sig local_def_loc with
        (* It's a local variable but it's not related to an export/import. However, let's try
         * property find-refs anyway in case the local is used as an object property shorthand. *)
        | None -> loc
        | Some related_loc -> related_loc
        in
        let%lwt refs = property_find_refs start_loc in
        refs %>>| fun refs ->
        (* If property find-refs returned nothing (for example if we are importing from an untyped
         * module), then fall back on the local refs we computed earlier. *)
        Lwt.return @@ Some (Option.value ~default:((name, local_refs), None) refs)
    in
    let json_data = match result with
      | Ok (Some (_, Some count)) -> ["deps", Hh_json.JSON_Number (string_of_int count)]
      | _ -> []
    in
    (* Drop the dependent file count  from the result *)
    let result = result >>| Option.map ~f:(fun (result, _) -> result) in
    let result = sort_and_dedup result in
    let json_data =
      ("result", Hh_json.JSON_String (match result with Ok _ -> "SUCCESS" | _ -> "FAILURE"))
      :: ("global", Hh_json.JSON_Bool global)
      :: json_data
    in
    Lwt.return (result, Some (Hh_json.JSON_Object json_data))
