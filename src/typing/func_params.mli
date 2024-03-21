(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make
    (CT : Func_class_sig_types.Config.S)
    (C : Func_params_intf.Config with module Types := CT)
    (T : Func_class_sig_types.Param.S with module Config := CT) :
  Func_params_intf.S with module Config_types := CT and module Config := C and module Types = T
