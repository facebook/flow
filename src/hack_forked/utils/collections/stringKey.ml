(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = string

let compare (x : t) (y : t) = String.compare x y

let to_string x = x

let pp fmt x = Format.fprintf fmt "%S" x
