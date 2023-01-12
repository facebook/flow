(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig.With_ALoc

(* Collect the names and locations of types that are available as we scan
 * the imports. Later we'll match them with some remote defining loc. *)
type acc_t = Ty.imported_ident list

let from_imported_locs_map ~import_mode map (acc : acc_t) =
  SMap.fold
    (fun _remote remote_map acc ->
      SMap.fold
        (fun local imported_locs_nel acc ->
          Nel.fold_left
            (fun acc { local_loc; _ } -> (local_loc, local, import_mode) :: acc)
            acc
            imported_locs_nel)
        remote_map
        acc)
    map
    acc

let rec from_binding ~import_mode binding (acc : acc_t) =
  match binding with
  | BindIdent (loc, name) -> (loc, name, import_mode) :: acc
  | BindNamed map ->
    List.fold_left (fun acc (_, binding) -> from_binding ~import_mode binding acc) acc map

let from_bindings ~import_mode bindings_opt acc =
  match bindings_opt with
  | Some bindings -> from_binding ~import_mode bindings acc
  | None -> acc

let from_require require (acc : acc_t) =
  match require with
  | Require { source = _; require_loc = _; bindings } ->
    from_bindings ~import_mode:Ty.ValueMode bindings acc
  | Import { import_loc = _; source = _; named; ns = _; types; typesof; typesof_ns = _ } ->
    (* TODO import namespaces (`ns`) as modules that might contain imported types *)
    acc
    |> from_imported_locs_map ~import_mode:Ty.ValueMode named
    |> from_imported_locs_map ~import_mode:Ty.TypeMode types
    |> from_imported_locs_map ~import_mode:Ty.TypeofMode typesof
  | ImportDynamic _
  | Import0 _
  | ExportFrom _ ->
    acc

let extract_imported_idents file_sig =
  let { module_sig = { requires; _ }; _ } = file_sig in
  List.fold_left (fun acc require -> from_require require acc) [] requires

let extract_schemes cx typed_ast (imported_locs : acc_t) =
  List.fold_left
    (fun acc (loc, name, import_mode) ->
      match Typed_ast_utils.find_exact_match_annotation cx typed_ast loc with
      | Some scheme -> (name, loc, import_mode, scheme) :: acc
      | None -> acc)
    []
    imported_locs
