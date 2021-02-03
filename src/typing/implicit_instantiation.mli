(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Emits missing annotation errors if the implicit instantiation produces underconstrained tvars *)
val check_implicit_instantiation : Context.t -> Context.implicit_instantiation_check -> unit
