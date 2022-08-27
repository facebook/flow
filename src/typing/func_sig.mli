(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Func_sig_intf

module Make
    (_ : Statement_sig.S)
    (CT : Func_class_sig_types.Config.S)
    (C : Func_params.Config with module Types := CT)
    (F : Func_params.S with module Config_types := CT and module Config := C)
    (T : Func_class_sig_types.Func.S with module Config := CT and module Param := F.Types) :
  S with module Config_types := CT and module Config := C and module Param := F and module Types = T

val return_loc : (ALoc.t, ALoc.t) Flow_ast.Function.t -> ALoc.t
