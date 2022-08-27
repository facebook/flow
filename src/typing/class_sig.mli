(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Class_sig_intf

module Make
    (CT : Func_class_sig_types.Config.S)
    (C : Func_params.Config with module Types := CT)
    (P : Func_params.S with module Config_types := CT and module Config := C)
    (F : Func_sig_intf.S with module Config_types := CT and module Config := C and module Param := P)
    (T : Func_class_sig_types.Class.S
           with module Config := CT
            and module Param := P.Types
            and module Func := F.Types) :
  S
    with module Config_types = CT
     and module Config = C
     and module Param = P
     and module Func = F
     and module Types = T
