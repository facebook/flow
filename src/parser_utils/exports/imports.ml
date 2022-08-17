(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig.With_Loc

type kind =
  | Default
  | Named
  | NamedType
  | Namespace
  | Unknown

type source =
  | Unresolved_source of string
  | Global

type import = {
  export: string;
  source: source;
  kind: kind;
}

type t = import list

let add_globals (globals : SSet.t) (imports : t) =
  SSet.fold (fun export acc -> { export; kind = Unknown; source = Global } :: acc) globals imports

let of_file_sig (file_sig : File_sig.With_Loc.t) =
  let requires = file_sig.module_sig.requires in
  List.fold_left
    (fun acc (require : File_sig.With_Loc.require) ->
      match require with
      | Import require ->
        let source = snd require.source in
        let acc =
          SMap.fold
            (fun export _elem acc ->
              let item =
                match export with
                | "default" -> { export = ""; kind = Default; source = Unresolved_source source }
                | _ -> { export; kind = Named; source = Unresolved_source source }
              in
              item :: acc)
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
    requires
