(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make
    (_ : Destructuring_sig.S)
    (_ : Func_stmt_config_sig.S with module Types := Func_stmt_config_types.Types)
    (_ : Component_params_intf.Config
           with module Types := Component_sig_types.DeclarationParamConfig)
    (_ : Statement_sig.S) : Statement_sig.S
