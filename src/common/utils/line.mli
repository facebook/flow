(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* line split/transform utils *)

(* split string at nth line. if it exists, returns pre, line, post *)
val split_nth : string -> int -> (string * string * string) option

(* transform nth line, if it exists. returns reconstructed string *)
val transform_nth : string -> int -> (string -> string) -> string
