(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Intermediate representation for functions *)

include module type of Func_sig_intf

module Make (Env : Env_sig.S) (_ : Abnormal_sig.S with module Env := Env) (F : Func_params.S) :
  S with type func_params = F.t and type func_params_tast = (ALoc.t * Type.t) F.ast

(** The location of the return type for a function. *)
val return_loc : (ALoc.t, ALoc.t) Flow_ast.Function.t -> ALoc.t
