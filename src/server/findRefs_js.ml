(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

module Result = Core_result
let (>>=) = Result.(>>=)

let get_docblock file content =
  let open Parsing_service_js in
  let max_tokens = docblock_max_tokens in
  let _errors, docblock = get_docblock ~max_tokens file content in
  docblock

let get_ast_result file content =
  let docblock = get_docblock file content in
  let open Parsing_service_js in
  let types_mode = TypesAllowed in
  let use_strict = true in
  let result = do_parse ~fail:false ~types_mode ~use_strict ~info:docblock content file in
  match result with
    | Parse_ok ast -> Ok ast
    (* The parse should not fail; we have passed ~fail:false *)
    | Parse_fail _ -> Error "Parse unexpectedly failed"
    | Parse_skip _ -> Error "Parse unexpectedly skipped"

let get_imported_locations symbol file_key (dep_file_key: File_key.t) : (Loc.t list, string) result =
  let open File_sig in
  File_key.to_path dep_file_key >>= fun path ->
  let dep_fileinput = File_input.FileName path in
  File_input.content_of_file_input dep_fileinput >>= fun content ->
  get_ast_result dep_file_key content >>= fun ast ->
  let sig_ = File_sig.program ast in
  let relevant_requires: require SMap.t =
    sig_.module_sig.requires |>
      SMap.filter begin fun from_module _ ->
        let resolved = Module_js.find_resolved_module
          ~audit:Expensive.warn
          dep_file_key
          from_module
        in
        match Module_js.get_file ~audit:Expensive.warn resolved with
          | None -> false
          | Some x -> x = file_key
      end
  in
  (* The keys in relevant_requires are all just references to the module where the symbol is
  defined, so let's get just the values since we have already filterd out irrelevant keys *)
  let only_requires: require list =
    SMap.bindings relevant_requires |>
      List.map (fun (_, x) -> x)
  in
  let locs =
    let get_relevant_locs require =
      match SMap.get symbol require.named with
        | None -> []
        | Some local_name_to_locs ->
            SMap.bindings local_name_to_locs |>
              List.map (fun (_, locs) -> Nel.to_list locs) |>
              List.concat
    in
    List.map get_relevant_locs only_requires |> List.concat
  in
  Ok locs

let local_find_refs file_key file_input loc : ((string * Loc.t list) option, string) result =
  let open Scope_api in
  File_input.content_of_file_input file_input >>= fun content ->
  get_ast_result file_key content >>= fun ast ->
  let scope_info = Scope_builder.program ast in
  let all_uses = all_uses scope_info in
  let matching_uses = List.filter (fun use -> Loc.contains use loc) all_uses in
  begin match matching_uses with
    | [] -> Ok None
    | [use] ->
        let def = def_of_use scope_info use in
        let unsorted_locs = uses_of_def scope_info ~exclude_def:false def in
        let sorted_locs = List.fast_sort Loc.compare unsorted_locs in
        let name = Def.(def.actual_name) in
        Ok (Some (name, sorted_locs))
    | _ -> Error "Multiple identifiers were unexpectedly matched"
  end

let find_external_refs options workers env file_key content name local_refs =
  let docblock = get_docblock file_key content in
  let modulename = Module_js.exported_module options file_key docblock in
  let _, direct_dependents =
    Dep_service.dependent_files
      workers
      (* Surprisingly, creating this set doesn't seem to cause horrible performance but it's
      probably worth looking at if you are searching for optimizations *)
      ~unchanged:ServerEnv.(CheckedSet.all !env.checked_files)
      ~new_or_changed:(FilenameSet.singleton file_key)
      ~changed_modules:(Modulename.Set.singleton modulename)
  in
  (* Get a map from dependent file path to locations where the symbol in question is imported in
  that file *)
  let imported_locations_result: (Loc.t list, string) result =
    FilenameSet.elements direct_dependents |>
      List.map (get_imported_locations name file_key) |>
      Result.all >>= fun loc_lists ->
      Ok (List.concat loc_lists)
  in
  imported_locations_result >>= fun imported_locations ->
  let all_external_locations =
    imported_locations |>
    List.map begin fun imported_loc ->
      let filekey_result =
        Result.of_option
          Loc.(imported_loc.source)
          ~error:"local_find_refs should return locs with sources"
      in
      filekey_result >>= fun filekey ->
      File_key.to_path filekey >>= fun path ->
      let file_input = File_input.FileName path in
      local_find_refs filekey file_input imported_loc >>= fun refs_option ->
      Result.of_option refs_option ~error:"Expected results from local_find_refs" >>= fun (_name, locs) ->
      Ok locs
    end |>
    Result.all >>= fun locs ->
    Ok (List.concat locs)
  in
  all_external_locations >>= fun all_external_locations ->
  Ok (List.concat [all_external_locations; local_refs])

let find_refs options workers env file_input line col global =
  let filename = File_input.filename_of_file_input file_input in
  let file_key = File_key.SourceFile filename in
  let loc = Loc.make file_key line col in
  (* TODO right now we assume that the symbol was defined in the given file. do a get-def or similar
  first *)
  local_find_refs file_key file_input loc >>= function
    | None -> Ok None
    | Some (name, refs) ->
        if global then
          File_input.content_of_file_input file_input >>= fun content ->
          get_ast_result file_key content >>= fun ast ->
          let sig_ = File_sig.program ast in
          let open File_sig in
          begin match sig_.module_sig.module_kind with
            (* TODO support CommonJS *)
            | CommonJS _ -> Ok (Some (name, refs))
            | ES {named; _} -> begin match SMap.get name named with
                | None
                | Some None (* lol *) -> Ok (Some (name, refs))
                | Some (Some loc) ->
                    if List.mem loc refs then
                      let all_refs_result = find_external_refs options workers env file_key content name refs in
                      all_refs_result >>= fun all_refs -> Ok (Some (name, all_refs))
                    else
                      Ok (Some (name, refs))
              end
          end
        else
          Ok (Some (name, refs))
