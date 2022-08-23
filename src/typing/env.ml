(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module describes the representation of lexical environments and defines
   various operations on them, including "stack" operations to push/pop scopes,
   and "lookup" operations to find, read, and write variables and their
   associated type information. *)

open Reason
module Flow = Flow_js

module type S = sig
  val get_global_value_type : Context.t -> Reason.name -> Reason.t -> Type.t

  val valid_declaration_check : Context.t -> Reason.name -> ALoc.t -> unit
end

module Env : S = struct
  let get_global_value_type cx name reason =
    match Context.global_value_cache_find_opt cx name with
    | Some t -> t
    | None ->
      let t = Flow.get_builtin cx name reason in
      Context.add_global_value_cache_entry cx name t;
      t

  let valid_declaration_check cx name loc =
    let { Loc_env.var_info = { Env_api.scopes = info; ssa_values = values; providers; _ }; _ } =
      Context.environment cx
    in
    let error null_write =
      let null_write =
        Base.Option.map
          ~f:(fun null_loc -> Error_message.{ null_loc; initialized = ALoc.equal loc null_loc })
          null_write
      in
      Flow.add_output
        cx
        Error_message.(
          EInvalidDeclaration { declaration = mk_reason (RIdentifier name) loc; null_write }
        )
    in
    match Invalidation_api.declaration_validity info values providers loc with
    | Invalidation_api.Valid -> ()
    | Invalidation_api.NotWritten -> error None
    | Invalidation_api.NullWritten null_loc -> error (Some null_loc)
end
