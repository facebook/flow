(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Config_types : Component_sig_types.ParamConfig.S

  module Config : Component_sig_types.Config with module Types := Config_types

  module Param :
    Component_params.S with module Config_types := Config_types and module Config := Config

  module Types :
    Component_sig_types.ComponentSig.S
      with module Config := Config_types
       and module Param := Param.Types

  open Types

  val toplevels :
    Context.t -> t -> component_params_tast * (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.Block.t
end
