(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type C = sig
  type t

  val enable_enums : t -> bool

  val file : t -> File_key.t

  val jsx : t -> Options.jsx_mode

  val react_runtime : t -> Options.react_runtime

  val enable_const_params : t -> bool

  val lti : t -> bool

  val add_literal_subtypes : t -> ALoc.t * Env_api.literal_check -> unit

  val add_matching_props : t -> string * ALoc.t * ALoc.t -> unit

  val add_exhaustive_check : t -> ALoc.t -> ALoc.t list * bool -> unit

  val exhaustive_check : t -> ALoc.t -> ALoc.t list * bool
end

module type F = sig
  type cx

  val add_output : cx -> ?trace:Type.trace -> ALoc.t Error_message.t' -> unit
end
