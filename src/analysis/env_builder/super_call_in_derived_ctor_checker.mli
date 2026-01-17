(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Checks that super() is called before accessing this or super in derived class constructors.

    In JavaScript/Flow, a derived class constructor (a class that extends another)
    must call super() before any access to [this] or [super]. This checker validates
    that constraint by tracking control flow through the constructor body. *)

val check :
  enable_enums:bool ->
  add_output:(Error_message.t -> unit) ->
  ALoc.t ->
  (ALoc.t, ALoc.t) Flow_ast.Class.t ->
  unit
