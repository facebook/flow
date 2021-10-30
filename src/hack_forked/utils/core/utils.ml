(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Callstack is simply a typed way to indicate that a string is a callstack *)
type callstack = Callstack of string
