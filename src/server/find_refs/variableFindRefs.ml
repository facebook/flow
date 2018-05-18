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

open FindRefsUtils

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
            let local_name_locs = Nel.to_list locs |> List.map (fun {local_loc;_} -> local_loc) in
            List.rev_append local_name_locs acc
          ) local_name_to_locs acc
        end
      | _ -> acc
  ) [] file_sig.module_sig.requires in
  List.fast_sort Loc.compare locs

let local_find_refs ast loc =
  let open Scope_api in
  let scope_info = Scope_builder.program ast in
  let all_uses = all_uses scope_info in
  let matching_uses = LocSet.filter (fun use -> Loc.contains use loc) all_uses in
  let num_matching_uses = LocSet.cardinal matching_uses in
  if num_matching_uses = 0 then
    None
  else if num_matching_uses > 1 then
    (* This is unlikely enough that we can just throw *)
    failwith "Multiple identifiers were unexpectedly matched"
  else
    let use = LocSet.choose matching_uses in
    let def = def_of_use scope_info use in
    let sorted_locs = LocSet.elements @@ uses_of_def scope_info ~exclude_def:false def in
    let name = Def.(def.actual_name) in
    Some (name, sorted_locs, Nel.hd def.Def.locs)

let local_find_refs_with_exports file_key ~content loc =
  let open File_sig in
  compute_ast_result file_key content >>| fun (ast, file_sig, _) ->
  match local_find_refs ast loc with
  | Some (name, refs, _def_loc) -> Some (name, refs)
  | None ->
    begin match file_sig.module_sig.module_kind with
    | CommonJS { exports = Some (CJSExportIdent (id_loc, id_name)) }
      when Loc.contains id_loc loc -> Some (id_name, [id_loc])
    | CommonJS { exports = Some (CJSExportProps props) } ->
      let props = SMap.filter (fun _ (CJSExport prop) -> Loc.contains prop.loc loc) props in
      begin match SMap.choose props with
      | Some (prop_name, CJSExport prop) -> Some (prop_name, [prop.loc])
      | None -> None
      end
    | _ -> None
    end

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
            ~error:"local_find_refs_with_exports should return locs with sources"
        in
        filekey_result >>= fun filekey ->
        File_key.to_path filekey >>= fun path ->
        let file_input = File_input.FileName path in
        File_input.content_of_file_input file_input >>= fun content ->
        local_find_refs_with_exports filekey ~content imported_loc >>= fun refs_option ->
        Result.of_option refs_option ~error:"Expected results from local_find_refs_with_exports" >>= fun (_name, locs) ->
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
  local_find_refs_with_exports file_key content loc %>>= function
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
