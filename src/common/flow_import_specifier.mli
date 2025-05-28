(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type userland [@@deriving show, eq, ord]

val userland : string -> userland

val map_userland : f:(string -> string) -> userland -> userland

val display_userland : userland -> string

val unwrap_userland : userland -> string

type t =
  | Userland of userland
  | HasteImportWithSpecifiedNamespace of {
      namespace: Bitset.t;
      name: string;
      allow_implicit_platform_specific_import: bool;
    }
[@@deriving show, ord]

val userland_specifier : string -> t

module Map : WrappedMap_sig.S with type key = t

module Set : Flow_set.S with type elt = t
