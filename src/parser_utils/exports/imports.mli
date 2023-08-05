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
[@@deriving show { with_path = false }]

type source =
  | Unresolved_source of string
  | Global
[@@deriving show { with_path = false }]

type import = {
  export: string;
  source: source;
  kind: kind;
}
[@@deriving show { with_path = false }]

type t = import list [@@deriving show { with_path = false }]

val of_file_sig : File_sig.t -> t

val add_globals : SSet.t -> t -> t
