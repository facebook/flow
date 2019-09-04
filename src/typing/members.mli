(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

  type ('success, 'success_module) generic_t =
    | Success of 'success
    | SuccessModule of 'success_module
    | FailureNullishType
    | FailureAnyType
    | FailureUnhandledType of Type.t
    | FailureUnhandledMembers of Type.t

  type t = (
    (* Success *) (ALoc.t option * Type.t) SMap.t,
    (* SuccessModule *) (ALoc.t option * Type.t) SMap.t * (Type.t option)
  ) generic_t

  (* For debugging purposes *)
  val string_of_extracted_type: (Type.t, Type.t) generic_t -> string

  val to_command_result: t -> ((ALoc.t option * Type.t) SMap.t, string) result

  val extract: ?exclude_proto_members: bool -> Context.t -> Type.t -> t
  val extract_type: Context.t -> Type.t -> (Type.t, Type.t) generic_t
  val extract_members: ?exclude_proto_members: bool -> Context.t -> (Type.t, Type.t) generic_t -> t
  val resolve_type: Context.t -> Type.t -> Type.t
