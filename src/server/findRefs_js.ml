(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open ServerEnv

module Result = Core_result
let (>>=) = Result.(>>=)
let (>>|) = Result.(>>|)

(* The problem with Core_result's >>= and >>| is that the function second argument cannot return
 * an Lwt.t. These helper infix operators handle that case *)
let (%>>=) (result: ('ok, 'err) Result.t) (f: 'ok -> ('a, 'err) Result.t Lwt.t) : ('a, 'err) Result.t Lwt.t =
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok x -> f x

let (%>>|) (result: ('ok, 'err) Result.t) (f: 'ok -> 'a Lwt.t) : ('a, 'err) Result.t Lwt.t =
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok x ->
    let%lwt new_x = f x in
    Lwt.return (Ok new_x)

let compute_docblock file content =
  let open Parsing_service_js in
  let max_tokens = docblock_max_tokens in
  let _errors, docblock = parse_docblock ~max_tokens file content in
  docblock

(* We use compute_ast_result (as opposed to get_ast_result) when the file contents we have might be
 * different from what's on disk (and what is therefore stored in shared memory). This can be the
 * case for local find-refs requests, where the client may pipe in file contents rather than just
 * specifying a filename. For global find-refs, we assume that all dependent files are the same as
 * what's on disk, so we can grab the AST from the heap instead. *)
let compute_ast_result file content =
  let docblock = compute_docblock file content in
  let open Parsing_service_js in
  let types_mode = TypesAllowed in
  let use_strict = true in
  let result = do_parse ~fail:false ~types_mode ~use_strict ~info:docblock content file in
  match result with
    | Parse_ok (ast, file_sig) -> Ok (ast, file_sig, docblock)
    (* The parse should not fail; we have passed ~fail:false *)
    | Parse_fail _ -> Error "Parse unexpectedly failed"
    | Parse_skip _ -> Error "Parse unexpectedly skipped"

let get_ast_result file : (Loc.t Ast.program * File_sig.t * Docblock.t, string) result =
  let open Parsing_service_js in
  let get_result f kind =
    let error =
      Printf.sprintf "Expected %s to be available for %s"
        kind
        (File_key.to_string file)
    in
    Result.of_option ~error (f file)
  in
  let ast_result = get_result get_ast "AST" in
  let file_sig_result = get_result get_file_sig "file sig" in
  let docblock_result = get_result get_docblock "docblock" in
  ast_result >>= fun ast ->
  file_sig_result >>= fun file_sig ->
  docblock_result >>= fun docblock ->
  Ok (ast, file_sig, docblock)

let get_dependents options workers env file_key content =
  let docblock = compute_docblock file_key content in
  let modulename = Module_js.exported_module options file_key docblock in
  Dep_service.dependent_files
    workers
    (* Surprisingly, creating this set doesn't seem to cause horrible performance but it's
    probably worth looking at if you are searching for optimizations *)
    ~unchanged:ServerEnv.(CheckedSet.all !env.checked_files)
    ~new_or_changed:(FilenameSet.singleton file_key)
    ~changed_modules:(Modulename.Set.singleton modulename)

let lazy_mode_focus genv env path =
  let%lwt env, _ = Lazy_mode_utils.focus_and_check genv env (Nel.one path) in
  Lwt.return env

module VariableRefs: sig
  val find_refs:
    ServerEnv.genv ->
    ServerEnv.env ref ->
    File_key.t ->
    content: string ->
    Loc.t ->
    global: bool ->
    ((string * Loc.t list * int option) option, string) result Lwt.t
end = struct

  (* Sometimes we want to find a specific symbol in a dependent file, but other
   * times we know that we want whatever symbol the import is assigned to. *)
  type import_query =
    | Symbol of string
    | CJSIdent

  let get_imported_locations (query: import_query) file_key (dep_file_key: File_key.t) : (Loc.t list, string) result =
    let open File_sig in
    get_ast_result dep_file_key >>| fun (_, file_sig, _) ->
    let is_relevant mref =
      let resolved = Module_js.find_resolved_module
        ~audit:Expensive.warn
        dep_file_key
        mref
      in
      match Module_js.get_file ~audit:Expensive.warn resolved with
      | None -> false
      | Some x -> x = file_key
    in
    let locs = List.fold_left (fun acc require ->
      match require with
      | Require { source = (_, mref); bindings = Some bindings; _ } ->
        if not (is_relevant mref) then acc else
        begin match bindings with
        | BindIdent (loc, _) ->
          if query = CJSIdent
          then loc::acc
          else acc
        | BindNamed bindings ->
          SMap.fold (fun _ (local_loc, (_, remote)) acc ->
            if query = Symbol remote
            then local_loc::acc
            else acc
          ) bindings acc
        end
      | Require _
      | ImportDynamic _
      | Import0 _ -> acc
      | Import { source = (_, mref); named; _ } ->
        if not (is_relevant mref) then acc else
        match query with
        | Symbol symbol -> begin match SMap.get symbol named with
          | None -> acc
          | Some local_name_to_locs ->
            SMap.fold (fun _ locs acc ->
              List.rev_append (Nel.to_list locs) acc
            ) local_name_to_locs acc
          end
        | _ -> acc
    ) [] file_sig.module_sig.requires in
    List.fast_sort Loc.compare locs


  let local_find_refs file_key ~content loc =
    let open File_sig in
    let open Scope_api in
    compute_ast_result file_key content >>= fun (ast, file_sig, _) ->
    let scope_info = Scope_builder.program ast in
    let all_uses = all_uses scope_info in
    let matching_uses = LocSet.filter (fun use -> Loc.contains use loc) all_uses in
    let num_matching_uses = LocSet.cardinal matching_uses in
    if num_matching_uses = 0 then begin match file_sig.module_sig.module_kind with
    | CommonJS { exports = Some (CJSExportIdent (id_loc, id_name)) }
      when Loc.contains id_loc loc -> Ok (Some (id_name, [id_loc]))
    | CommonJS { exports = Some (CJSExportProps props) } ->
      let props = SMap.filter (fun _ (CJSExport prop) -> Loc.contains prop.loc loc) props in
      begin match SMap.choose props with
      | Some (prop_name, CJSExport prop) -> Ok (Some (prop_name, [prop.loc]))
      | None -> Ok None
      end
    | _ -> Ok None
    end
    else if num_matching_uses > 1 then Error "Multiple identifiers were unexpectedly matched"
    else
      let use = LocSet.choose matching_uses in
      let def = def_of_use scope_info use in
      let sorted_locs = LocSet.elements @@ uses_of_def scope_info ~exclude_def:false def in
      let name = Def.(def.actual_name) in
      Ok (Some (name, sorted_locs))

  let find_external_refs genv env file_key ~content ~query local_refs =
    let {options; workers} = genv in
    File_key.to_path file_key %>>= fun path ->
    let%lwt new_env = lazy_mode_focus genv !env path in
    env := new_env;
    let%lwt _, direct_dependents = get_dependents options workers env file_key content in
    (* Get a map from dependent file path to locations where the symbol in question is imported in
    that file *)
    let imported_locations_result: (Loc.t list, string) result =
      FilenameSet.elements direct_dependents |>
        List.map (get_imported_locations query file_key) |>
        Result.all >>= fun loc_lists ->
        Ok (List.concat loc_lists)
    in
    Lwt.return (
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
          File_input.content_of_file_input file_input >>= fun content ->
          local_find_refs filekey ~content imported_loc >>= fun refs_option ->
          Result.of_option refs_option ~error:"Expected results from local_find_refs" >>= fun (_name, locs) ->
          Ok locs
        end |>
        Result.all >>= fun locs ->
        Ok (List.concat locs)
      in
      all_external_locations >>= fun all_external_locations ->
      Ok (all_external_locations @ local_refs, FilenameSet.cardinal direct_dependents)
    )

  let find_refs genv env file_key ~content loc ~global =
    (* TODO right now we assume that the symbol was defined in the given file. do a get-def or similar
    first *)
    local_find_refs file_key content loc %>>= function
      | None -> Lwt.return (Ok None)
      | Some (name, refs) ->
          if global then
            compute_ast_result file_key content %>>= fun (_, file_sig, _) ->
            let open File_sig in
            let find_exported_loc loc query =
              if List.mem loc refs then
                let%lwt all_refs_result = find_external_refs genv env file_key content query refs in
                all_refs_result %>>= fun (all_refs, num_deps) ->
                Lwt.return (Ok (Some (name, all_refs, Some num_deps)))
              else
                Lwt.return (Ok (Some (name, refs, None))) in
            begin match file_sig.module_sig.module_kind with
              | CommonJS { exports = None } -> Lwt.return (Ok (Some (name, refs, None)))
              | CommonJS { exports = Some (CJSExportIdent (id_loc, id_name)) } ->
                if id_name = name
                then find_exported_loc id_loc CJSIdent
                else Lwt.return (Ok (Some (name, refs, None)))
              | CommonJS { exports = Some CJSExportOther } ->
                Lwt.return (Ok (Some (name, refs, None)))
              | CommonJS { exports = Some (CJSExportProps props) } ->
                begin match SMap.get name props with
                  | None -> Lwt.return (Ok (Some (name, refs, None)))
                  | Some (CJSExport { loc; _ }) -> find_exported_loc loc (Symbol name)
                end
              | ES {named; _} -> begin match SMap.get name named with
                  | None -> Lwt.return (Ok (Some (name, refs, None)))
                  | Some (File_sig.ExportDefault _) -> Lwt.return (Ok (Some (name, refs, None)))
                  | Some (File_sig.ExportNamed { loc; _ } | File_sig.ExportNs { loc; _ }) ->
                      find_exported_loc loc (Symbol name)
                end
            end
          else
            Lwt.return (Ok (Some (name, refs, None)))
end

module PropertyRefs: sig
  val find_refs:
    ServerEnv.genv ->
    ServerEnv.env ref ->
    profiling: Profiling_js.running ->
    content: string ->
    File_key.t ->
    Loc.t ->
    global: bool ->
    ((string * Loc.t list * int option) option, string) result Lwt.t

end = struct

  class property_access_searcher name = object(this)
    inherit [bool] Flow_ast_visitor.visitor ~init:false as super
    method! member expr =
      let open Ast.Expression.Member in
      begin match expr.property with
        | PropertyIdentifier (_, x) when x = name ->
            this#set_acc true
        | _ -> ()
      end;
      super#member expr
  end

  (* Returns true iff the given AST contains an access to a property with the given name *)
  let check_for_matching_prop name ast =
    let checker = new property_access_searcher name in
    checker#eval checker#program ast

  let set_def_loc_hook prop_access_info target_loc =
    let use_hook ret _ctxt name loc ty =
      begin if Loc.contains loc target_loc then
        prop_access_info := Some (`Use (ty, name))
      end;
      ret
    in
    let class_def_hook _ctxt ty name loc =
      if Loc.contains loc target_loc then
        prop_access_info := Some (`Class_def (ty, name))
    in
    let obj_def_hook _ctxt name loc =
      if Loc.contains loc target_loc then
        prop_access_info := Some (`Obj_def (loc, name))
    in

    Type_inference_hooks_js.set_member_hook (use_hook false);
    Type_inference_hooks_js.set_call_hook (use_hook ());
    Type_inference_hooks_js.set_class_member_decl_hook class_def_hook;
    Type_inference_hooks_js.set_obj_prop_decl_hook obj_def_hook

  let set_get_refs_hook potential_refs target_name =
    let hook ret _ctxt name loc ty =
      begin if name = target_name then
        (* Replace previous bindings of `loc`. We should always use the result of the last call to
         * the hook for a given location. For details see the comment on the generate_tests function
         * in flow_js.ml *)
        potential_refs := LocMap.add loc ty !potential_refs
      end;
      ret
    in
    Type_inference_hooks_js.set_member_hook (hook false);
    Type_inference_hooks_js.set_call_hook (hook ())

  let unset_hooks () =
    Type_inference_hooks_js.reset_hooks ()

  type def_loc =
    (* We found at least one def loc. Superclass implementations are also included. The list is
    ordered such that subclass implementations are first and superclass implementations are last. *)
    | Found of Loc.t Nel.t
    (* This means we resolved the receiver type but did not find the definition. If this happens
     * there must be a type error (which may be suppresssed) *)
    | NoDefFound
    (* This means it's a known type that we deliberately do not currently support. *)
    | UnsupportedType
    (* This means it's not well-typed, and could be anything *)
    | AnyType

  (* Disable the unused value warning -- we want to keep this around for debugging *)
  [@@@warning "-32"]
  let debug_string_of_def_loc = function
    | Found locs ->
      spf
        "Found (%s)"
        (locs |> Nel.to_list |> List.map Loc.to_string |> String.concat ", ")
    | NoDefFound -> "NoDefFound"
    | UnsupportedType -> "UnsupportedType"
    | AnyType -> "AnyType"
  (* Re-enable the unused value warning *)
  [@@@warning "+32"]

  let extract_instancet cx ty : (Type.t, string) result =
    let open Type in
    let resolved = Flow_js.resolve_type cx ty in
    match resolved with
      | ThisClassT (_, t)
      | DefT (_, PolyT (_, ThisClassT (_, t), _)) -> Ok t
      | _ ->
        let type_string = string_of_ctor resolved in
        Error ("Expected a class type to extract an instance type from, got " ^ type_string)

  (* Must be called with the result from Flow_js.Members.extract_type *)
  let get_def_loc_from_extracted_type cx extracted_type name =
    extracted_type
    |> Flow_js.Members.extract_members cx
    |> Flow_js.Members.to_command_result
    >>= fun map -> match SMap.get name map with
      | None -> Ok None
      | Some (None, _) -> Error "Expected a location associated with the definition"
      | Some (Some loc, _) -> Ok (Some loc)

  let rec extract_def_loc cx ty name : (def_loc, string) result =
    let resolved = Flow_js.resolve_type cx ty in
    extract_def_loc_resolved cx resolved name

  (* The same as get_def_loc_from_extracted_type except it recursively checks for overridden
   * definitions of the member in superclasses and returns those as well *)
  and extract_def_loc_from_instancet cx extracted_type super name : (def_loc, string) result =
    let current_class_def_loc = get_def_loc_from_extracted_type cx extracted_type name in
    current_class_def_loc
    >>= begin function
      | None -> Ok NoDefFound
      | Some loc ->
        extract_def_loc cx super name
        >>| begin function
          | Found lst ->
              (* Avoid duplicate entries. This can happen if a class does not override a method,
               * so the definition points to the method definition in the parent class. Then we
               * look at the parent class and find the same definition. *)
              let lst =
                if Nel.hd lst = loc then
                  lst
                else
                  Nel.cons loc lst
              in
              Found lst
          (* If the superclass does not have a definition for this method, or it is for some reason
           * not a class type, or we don't know its type, just return the location we already know
           * about. *)
          | NoDefFound | UnsupportedType | AnyType -> Found (Nel.one loc)
        end
    end

  and extract_def_loc_resolved cx ty name : (def_loc, string) result =
    let open Flow_js.Members in
    let open Type in
    match Flow_js.Members.extract_type cx ty with
      | Success (DefT (_, InstanceT (_, super, _, _))) as extracted_type ->
          extract_def_loc_from_instancet cx extracted_type super name
      | Success (DefT (_, ObjT _)) as extracted_type ->
          get_def_loc_from_extracted_type cx extracted_type name
          >>| begin function
            | None -> NoDefFound
            | Some loc -> Found (Nel.one loc)
          end
      | Success _
      | SuccessModule _
      | FailureNullishType
      | FailureUnhandledType _ ->
          Ok UnsupportedType
      | FailureAnyType ->
          Ok AnyType

  let filter_refs cx potential_refs file_key local_defs def_locs name =
    potential_refs |>
      LocMap.bindings |>
      (* The location where a shadow prop is introduced is considered both a definition and a use.
       * Make sure we include it only once despite that. *)
      List.filter (fun (loc, _) -> not (List.mem loc local_defs)) |>
      List.map begin fun (ref_loc, ty) ->
        extract_def_loc cx ty name >>| function
          | Found locs ->
              (* Only take the first extracted def loc -- that is, the one for the actual definition
               * and not overridden implementations, and compare it to the list of def locs we are
               * interested in *)
              let loc = Nel.hd locs in
              if Nel.mem loc def_locs then
                Some ref_loc
              else
                None
          (* TODO we may want to surface AnyType results somehow since we can't be sure whether they
           * are references or not. For now we'll leave them out. *)
          | NoDefFound | UnsupportedType | AnyType -> None
      end
      |> Result.all
      |> Result.map_error ~f:(fun err ->
          Printf.sprintf
            "Encountered while finding refs in `%s`: %s"
            (File_key.to_string file_key)
            err
        )
      >>|
      List.fold_left (fun acc -> function None -> acc | Some loc -> loc::acc) []

  let find_refs_in_file options ast_info file_key def_locs name =
    let potential_refs: Type.t LocMap.t ref = ref LocMap.empty in
    let (ast, file_sig, info) = ast_info in
    let local_defs =
      Nel.to_list def_locs
      |> List.filter (fun loc -> loc.Loc.source = Some file_key)
    in
    let has_symbol = check_for_matching_prop name ast in
    if not has_symbol then
      Ok local_defs
    else begin
      set_get_refs_hook potential_refs name;
      let cx = Merge_service.merge_contents_context_without_ensure_checked_dependencies
        options file_key ast info file_sig
      in
      unset_hooks ();
      filter_refs cx !potential_refs file_key local_defs def_locs name
      >>| (@) local_defs
    end

  let find_external_refs genv all_deps def_locs name =
    let {options; workers} = genv in
    let dep_list: File_key.t list = FilenameSet.elements all_deps in
    let node_modules_containers = !Files.node_modules_containers in
    let%lwt result = MultiWorkerLwt.call workers
      ~job: begin fun _acc deps ->
        (* Yay for global mutable state *)
        Files.node_modules_containers := node_modules_containers;
        deps |> List.map begin fun dep ->
          get_ast_result dep >>= fun ast_info ->
          find_refs_in_file options ast_info dep def_locs name
        end
      end
      ~merge: (fun refs acc -> refs::acc)
      ~neutral: []
      ~next: (MultiWorkerLwt.next workers dep_list)
    in
    Lwt.return (List.concat result |> Result.all)

  let find_refs genv env ~profiling ~content file_key loc ~global =
    let options, workers = genv.options, genv.workers in
    let get_def_info: unit -> ((Loc.t Nel.t * string) option, string) result Lwt.t = fun () ->
      let props_access_info = ref None in
      set_def_loc_hook props_access_info loc;
      let%lwt cx_result =
        compute_ast_result file_key content
        %>>| fun (ast, file_sig, info) ->
          Profiling_js.with_timer_lwt profiling ~timer:"MergeContents" ~f:(fun () ->
            let ensure_checked =
              Types_js.ensure_checked_dependencies ~options ~profiling ~workers ~env in
            Merge_service.merge_contents_context options file_key ast info file_sig ensure_checked
          )
      in
      unset_hooks ();
      Lwt.return (
        cx_result >>= fun cx ->
        match !props_access_info with
          | None -> Ok None
          | Some (`Obj_def (loc, name)) ->
              Ok (Some (Nel.one loc, name))
          | Some (`Class_def (ty, name)) ->
              (* We get the type of the class back here, so we need to extract the type of an instance *)
              extract_instancet cx ty >>= fun ty ->
              begin extract_def_loc_resolved cx ty name >>= function
                | Found locs -> Ok (Some (locs, name))
                | _ -> Error "Unexpectedly failed to extract definition from known type"
              end
          | Some (`Use (ty, name)) ->
              begin extract_def_loc cx ty name >>= function
                | Found locs -> Ok (Some (locs, name))
                | NoDefFound
                | UnsupportedType
                | AnyType -> Ok None
              end
      )
    in
    let%lwt def_info = get_def_info () in
    def_info %>>= fun def_info_opt ->
    match def_info_opt with
      | None -> Lwt.return (Ok None)
      | Some (def_locs, name) ->
          if global then
            (* Start from the file where the symbol is defined, instead of the one where find-refs
             * was called. *)
            (* Since the def_locs are ordered from sub-est class to super-est class, and the
             * subclasses must depend on the superclasses, then we can safely take the super-est
             * class location and consider that the root. *)
            let last_def_loc = Nel.nth def_locs ((Nel.length def_locs) - 1) in
            Result.of_option Loc.(last_def_loc.source) ~error:"Expected a location with a source file" %>>= fun file_key ->
            File_key.to_path file_key %>>= fun path ->
            let%lwt new_env = lazy_mode_focus genv !env path in
            env := new_env;
            let fileinput = File_input.FileName path in
            File_input.content_of_file_input fileinput %>>= fun content ->
            compute_ast_result file_key content %>>= fun ast_info ->
            find_refs_in_file options ast_info file_key def_locs name %>>= fun refs ->
            let%lwt all_deps, _ = get_dependents options workers env file_key content in
            let dependent_file_count = FilenameSet.cardinal all_deps in
            Hh_logger.info
              "find-refs: searching %d dependent modules for references"
              dependent_file_count;
            let%lwt external_refs = find_external_refs genv all_deps def_locs name in
            external_refs %>>| fun external_refs ->
            Lwt.return (Some (name, List.concat (refs::external_refs), Some dependent_file_count))
          else
            Lwt.return (
              compute_ast_result file_key content >>= fun ast_info ->
              find_refs_in_file options ast_info file_key def_locs name >>= fun refs ->
              Ok (Some (name, refs, None))
            )
end

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
      let%lwt refs = VariableRefs.find_refs genv env file_key ~content loc ~global in
      refs %>>= function
        | Some _ as result -> Lwt.return (Ok result)
        | None -> PropertyRefs.find_refs genv env ~profiling ~content file_key loc ~global
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
