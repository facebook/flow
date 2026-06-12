(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = { module_name: string } [@@deriving show]

let mk ~module_name = { module_name }

let module_name { module_name } = module_name

let equal i1 i2 = String.equal i1.module_name i2.module_name

let compare i1 i2 = String.compare i1.module_name i2.module_name

let to_string { module_name } = module_name
