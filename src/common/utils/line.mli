(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* line split/transform utils *)

(* split string at nth line. if it exists, returns pre, line, post *)
val split_nth : string -> int -> (string * string * string) option

(* transform nth line, if it exists. returns reconstructed string *)
val transform_nth : string -> int -> (string -> string) -> string
