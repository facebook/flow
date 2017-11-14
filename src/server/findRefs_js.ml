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
  defined, so let's get just the values since we have already filtered out irrelevant keys *)
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

let set_def_loc_hook prop_access_info target_loc =
  let use_hook ret _ctxt name loc ty =
    begin if Loc.contains loc target_loc then
      prop_access_info := Some (`Use (ty, name))
    end;
    ret
  in
  let def_hook _ctxt name loc =
    if Loc.contains loc target_loc then
      prop_access_info := Some (`Def (loc, name))
  in
  Type_inference_hooks_js.set_member_hook (use_hook false);
  Type_inference_hooks_js.set_call_hook (use_hook ());
  Type_inference_hooks_js.set_method_decl_hook def_hook;
  Type_inference_hooks_js.set_prop_decl_hook def_hook

let set_get_refs_hook potential_refs target_name =
  let hook ret _ctxt name loc ty =
    begin if name = target_name then
      potential_refs := (ty, loc) :: !potential_refs
    end;
    ret
  in
  Type_inference_hooks_js.set_member_hook (hook false);
  Type_inference_hooks_js.set_call_hook (hook ())

let unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()

(* Ok None indicates an expected case where no location was found, whereas an Error return indicates
 * that something went wrong. *)
let extract_def_loc cx ty name : (Loc.t option, string) result =
  let open Flow_js.Members in
  let open Type in
  let resolved = Flow_js.resolve_type cx ty in
  match Flow_js.Members.extract_type cx resolved with
    | Success (DefT (_, InstanceT _)) as extracted_type ->
        let members = Flow_js.Members.extract_members cx extracted_type in
        let command_result = Flow_js.Members.to_command_result members in
        command_result >>= fun map ->
        Result.of_option ~error:"Expected a property definition" (SMap.get name map) >>= fun (loc, _) ->
        Result.of_option ~error:"Expected a location associated with the definition" loc >>= fun loc ->
        Ok (Some loc)
    | Success _
    | SuccessModule _ ->
        Ok None
    | FailureMaybeType ->
        Error "Extracting definition loc from possibly null or undefined value"
    | FailureAnyType ->
        Error "not enough type information to find definition"
    | FailureUnhandledType t ->
        Error (spf
          "find-refs on unexpected type of value %s (please file a task!)"
          (string_of_ctor t))

let find_prop_refs options workers env content file_key loc : ((string * Loc.t list) option, string) result =
  let check_contents () = Types_js.basic_check_contents ~options ~workers ~env content file_key in
  let get_def_info: unit -> ((Loc.t * string) option, string) result = fun () ->
    let props_access_info = ref None in
    set_def_loc_hook props_access_info loc;
    let check_contents_result = check_contents () in
    unset_hooks ();
    check_contents_result >>= fun (_, cx, _) ->
    match !props_access_info with
      | None -> Ok None
      | Some (`Def (loc, name)) -> Ok (Some (loc, name))
      | Some (`Use (ty, name)) ->
          extract_def_loc cx ty name >>= fun loc ->
          match loc with
            | None -> Ok None
            | Some loc -> Ok (Some (loc, name))
  in
  let get_ref_info: Loc.t -> string -> (Loc.t list, string) result = fun def_loc name ->
    let potential_refs: (Type.t * Loc.t) list ref = ref [] in
    set_get_refs_hook potential_refs name;
    let check_contents_result = check_contents () in
    unset_hooks ();
    check_contents_result >>= fun (_, cx, _) ->
    !potential_refs |>
      List.map begin fun (ty, ref_loc) ->
        extract_def_loc cx ty name >>= fun loc ->
        match loc with
          | Some loc when loc = def_loc ->
              Ok (Some ref_loc)
          | _ -> Ok None
      end |> Result.all >>= fun refs ->
      Ok (List.fold_left (fun acc -> function None -> acc | Some loc -> loc::acc) [] refs)
  in
  get_def_info () >>= fun def_info_opt ->
  match def_info_opt with
    | None -> Ok None
    | Some (def_loc, name) ->
        get_ref_info def_loc name >>= fun refs ->
        (* Include the def_loc if it is in the same file *)
        let refs = match Loc.(def_loc.source) with
          | Some source when source = file_key -> def_loc::refs
          | _ -> refs
        in
        Ok (Some (name, refs))

let local_var_find_refs file_key content loc : ((string * Loc.t list) option, string) result =
  let open Scope_api in
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

let local_find_refs options workers env file_key file_input loc : ((string * Loc.t list) option, string) result =
  File_input.content_of_file_input file_input >>= fun content ->
  local_var_find_refs file_key content loc >>= function
    | Some _ as result -> Ok result
    | None -> find_prop_refs options workers env content file_key loc

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
      local_find_refs options workers env filekey file_input imported_loc >>= fun refs_option ->
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
  local_find_refs options workers env file_key file_input loc >>= function
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
                | None -> Ok (Some (name, refs))
                | Some (File_sig.ExportDefault _) -> Ok (Some (name, refs))
                | Some (File_sig.ExportNamed { loc; _ } | File_sig.ExportNs { loc; _ }) ->
                    if List.mem loc refs then
                      let all_refs_result = find_external_refs options workers env file_key content name refs in
                      all_refs_result >>= fun all_refs -> Ok (Some (name, all_refs))
                    else
                      Ok (Some (name, refs))
              end
          end
        else
          Ok (Some (name, refs))
