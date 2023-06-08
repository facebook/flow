(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type ComponentBody = sig
  module Config : Component_sig_types.BodyConfig.S

  val eval : Context.t -> Reason.t -> Type.t -> ALoc.t Config.body -> (ALoc.t * Type.t) Config.body
end

module type S = sig
  module BodyConfig : Component_sig_types.BodyConfig.S

  module ComponentBody : ComponentBody with module Config := BodyConfig

  module Config_types : Component_sig_types.ParamConfig.S

  module Config : Component_params_intf.Config with module Types := Config_types

  module Param :
    Component_params.S with module Config_types := Config_types and module Config := Config

  module Types :
    Component_sig_types.ComponentSig.S
      with module Config := Config_types
       and module Param := Param.Types
       and module Body := BodyConfig

  open Types

  val toplevels : Context.t -> t -> component_params_tast * (ALoc.t * Type.t) BodyConfig.body

  val component_type : Context.t -> ALoc.t -> t -> Type.t
end
