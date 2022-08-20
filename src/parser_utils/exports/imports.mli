(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

val of_file_sig : File_sig.With_Loc.t -> t

val add_globals : SSet.t -> t -> t
