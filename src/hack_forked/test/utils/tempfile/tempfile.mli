(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
   definitions that should not actually be exposed to the caller. If you notice
   that this interface file is a poor interface, please take a few minutes to
   clean it up manually, and then delete this comment once the interface is in
   shape. *)

exception Out_of_retries

val mkdtemp : skip_mocking:bool -> Path.t

val with_real_tempdir : (Path.t -> 'a) -> 'a

val with_tempdir : (Path.t -> 'a) -> 'a
