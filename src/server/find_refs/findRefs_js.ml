(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let (>>|) = Core_result.(>>|)

open Utils_js

let sort_find_refs_result = function
  | Ok (Some (name, locs)) ->
      let locs = List.fast_sort Loc.compare locs in
      Ok (Some (name, locs))
  | x -> x

let find_refs ~genv ~env ~profiling ~file_input ~line ~col ~global =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.make file_key line col in
  match File_input.content_of_file_input file_input with
  | Error err -> Lwt.return (Error err, None)
  | Ok content ->
    let%lwt result =
      let%lwt refs = VariableFindRefs.find_refs genv env file_key ~content loc ~global in
      refs %>>= function
        | Some _ as result -> Lwt.return (Ok result)
        | None -> PropertyFindRefs.find_refs genv env ~profiling ~content file_key loc ~global
    in
    let json_data = match result with
      | Ok (Some (_, _, Some count)) -> ["deps", Hh_json.JSON_Number (string_of_int count)]
      | _ -> []
    in
    (* Drop the dependent file count  from the result *)
    let result = result >>| Option.map ~f:(fun (name, locs, _) -> (name, locs)) in
    let result = sort_find_refs_result result in
    let json_data =
      ("result", Hh_json.JSON_String (match result with Ok _ -> "SUCCESS" | _ -> "FAILURE"))
      :: ("global", Hh_json.JSON_Bool global)
      :: json_data
    in
    Lwt.return (result, Some (Hh_json.JSON_Object json_data))
