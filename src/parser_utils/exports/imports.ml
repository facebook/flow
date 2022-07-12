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

type import = {
  export: string;
  unresolved_source: string;
  kind: kind;
}

type t = import list

let of_file_sig (file_sig : File_sig.With_Loc.t) =
  let requires = file_sig.module_sig.requires in
  List.fold_left
    (fun acc (require : File_sig.With_Loc.require) ->
      match require with
      | Import require ->
        let source = snd require.source in
        let named =
          SMap.fold
            (fun export _elem acc -> { export; kind = Named; unresolved_source = source } :: acc)
            require.named
            []
        in
        let types =
          SMap.fold
            (fun export _elem acc ->
              { export; kind = NamedType; unresolved_source = source } :: acc)
            require.types
            []
        in
        let ns =
          match require.ns with
          | Some ns -> [{ export = snd ns; kind = Namespace; unresolved_source = source }]
          | None -> []
        in
        if
          List.length named = 0
          && List.length types = 0
          && List.length ns = 0
          (*TODO: Can possibly include TypesOf and TypesofNs*)
          && SMap.is_empty require.typesof
          && Option.is_none require.typesof_ns
        then
          { export = ""; kind = Default; unresolved_source = source } :: acc
        else
          acc @ named @ types @ ns
      (* TODO: Require, ImportDynamic, etc. *)
      | _ -> [])
    []
    requires
