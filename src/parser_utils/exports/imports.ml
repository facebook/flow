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
        let acc =
          SMap.fold
            (fun export _elem acc -> { export; kind = Named; unresolved_source = source } :: acc)
            require.named
            acc
        in
        let acc =
          SMap.fold
            (fun export _elem acc ->
              { export; kind = NamedType; unresolved_source = source } :: acc)
            require.types
            acc
        in
        let acc =
          match require.ns with
          | Some ns -> { export = snd ns; kind = Namespace; unresolved_source = source } :: acc
          | None -> acc
        in
        if
          SMap.is_empty require.named
          && SMap.is_empty require.types
          && Option.is_none require.ns
          (*TODO: Can possibly include TypesOf and TypesofNs*)
          && SMap.is_empty require.typesof
          && Option.is_none require.typesof_ns
        then
          { export = ""; kind = Default; unresolved_source = source } :: acc
        else
          acc
      (* TODO: Require, ImportDynamic, etc. *)
      | _ -> acc)
    []
    requires
