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

type import = {
  export: string;
  unresolved_source: string;
  kind: kind;
}

type t = import list

val of_file_sig : File_sig.With_Loc.t -> t
