(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let (>>|) = Core_result.(>>|)

open Utils_js

let sort_and_dedup =
  Core_result.map ~f:begin
    Option.map ~f:begin
      fun (name, locs) -> name, LocSet.of_list locs |> LocSet.elements
    end
  end

(* Checks if the symbol we are interested in is introduced as part of an export or an import. If so,
 * use the canonical definition for that export or import so that global find-refs for that symbol
 * is triggered. The original starting location will be added back later. *)
let get_import_or_export_location ast file_sig loc =
  let local_refs = VariableFindRefs.local_find_refs ast loc in
  Option.bind local_refs begin fun (_, _, def_loc) ->
    ImportExportSymbols.find_related_symbol file_sig def_loc
  end

let find_refs ~genv ~env ~profiling ~file_input ~line ~col ~global ~multi_hop =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.make file_key line col in
  match File_input.content_of_file_input file_input with
  | Error err -> Lwt.return (Error err, None)
  | Ok content ->
    let%lwt result =
      let%lwt refs =
        FindRefsUtils.compute_ast_result file_key content %>>= fun (ast, file_sig, _) ->
        let loc =
          get_import_or_export_location ast file_sig loc
          |> Option.value ~default:loc
        in
        PropertyFindRefs.find_refs genv env ~profiling ~content file_key loc ~global ~multi_hop
      in
      refs %>>= function
        | Some _ as result -> Lwt.return (Ok result)
        | None -> VariableFindRefs.find_refs genv env file_key ~content loc ~global
    in
    let json_data = match result with
      | Ok (Some (_, _, Some count)) -> ["deps", Hh_json.JSON_Number (string_of_int count)]
      | _ -> []
    in
    (* Drop the dependent file count  from the result *)
    let result = result >>| Option.map ~f:(fun (name, locs, _) -> (name, locs)) in
    let result = sort_and_dedup result in
    let json_data =
      ("result", Hh_json.JSON_String (match result with Ok _ -> "SUCCESS" | _ -> "FAILURE"))
      :: ("global", Hh_json.JSON_Bool global)
      :: json_data
    in
    Lwt.return (result, Some (Hh_json.JSON_Object json_data))
