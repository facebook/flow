(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type export =
  | Default  (** e.g. `export default function() {}` *)
  | Named of string  (** `export const foo: string = "foo"` *)
  | NamedType of string  (** `export type T = string` *)
  | Module of string * export list  (** `declare module "foo" { ... exports ... }` *)
[@@deriving show { with_path = false }]

type t = export list [@@deriving show { with_path = false }]

val of_module : 'a Packed_type_sig.Module.t -> t

val of_builtins : 'a Packed_type_sig.Builtins.t -> t

val empty : t
