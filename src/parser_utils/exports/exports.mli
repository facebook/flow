(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type export =
  | DefaultType of string option (* e.g. `export default class Foo {}` *)
  | Default of string option  (** e.g. `export default function() {}` *)
  | Named of string  (** `export const foo: string = "foo"` *)
  | NamedType of string  (** `export type T = string` *)
  | Module of Flow_import_specifier.userland * export list
      (** `declare module "foo" { ... exports ... }` *)
  | ReExportModule of Flow_import_specifier.userland
  | ReExportModuleTypes of Flow_import_specifier.userland
[@@deriving show { with_path = false }]

type t = export list [@@deriving show { with_path = false }]

val of_module : 'a Packed_type_sig.Module.t -> t

val of_builtins : 'a Packed_type_sig.Builtins.t -> t

val empty : t
