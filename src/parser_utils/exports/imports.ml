(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig

type kind =
  | Default
  | Named
  | NamedType
  | Namespace
  | Unknown
[@@deriving show { with_path = false }]

type source =
  | Unresolved_source of Flow_import_specifier.userland
  | Global
[@@deriving show { with_path = false }]

type import = {
  export: string;
  source: source;
  kind: kind;
}
[@@deriving show { with_path = false }]

type t = import list [@@deriving show { with_path = false }]

let add_globals (globals : SSet.t) (imports : t) =
  SSet.fold (fun export acc -> { export; kind = Unknown; source = Global } :: acc) globals imports

let of_file_sig (file_sig : File_sig.t) =
  List.fold_left
    (fun acc (require : File_sig.require) ->
      match require with
      | Import require ->
        let source = Flow_import_specifier.userland (snd require.source) in
        let acc =
          SMap.fold
            (fun export names acc ->
              match export with
              | "default" ->
                SMap.fold
                  (fun name _imported_locs acc ->
                    { export = name; kind = Default; source = Unresolved_source source } :: acc)
                  names
                  acc
              | _ -> { export; kind = Named; source = Unresolved_source source } :: acc)
            require.named
            acc
        in
        let acc =
          SMap.fold
            (fun export _elem acc ->
              { export; kind = NamedType; source = Unresolved_source source } :: acc)
            require.types
            acc
        in
        let acc =
          match require.ns with
          | Some _ -> { export = ""; kind = Namespace; source = Unresolved_source source } :: acc
          | None -> acc
        in
        acc
      (* TODO: Require, ImportDynamic, etc. *)
      | _ -> acc)
    []
    (File_sig.requires file_sig)
