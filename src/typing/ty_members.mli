(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a member_info = {
  ty: 'a;
  def_loc: ALoc.t option;
  (* Autocomplete ranks members from primitive prototypes below user-defined members.
     * `from_proto` indicates that the member is from a primitive prototype. *)
  from_proto: bool;
  (* If a member came from a possibly-null/undefined object, autocomplete may suggest
     * that the user use optional chaining to access it.
     * `from_nullable` indicates that the member is from a possibly-null/undefined object. *)
  from_nullable: bool;
}

type ty_members = {
  members: Ty.t member_info NameUtils.Map.t;
  errors: string list;
  in_idx: bool;
}

val extract :
  include_proto_members:bool ->
  cx:Context.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  file_sig:File_sig.With_ALoc.t ->
  Type.TypeScheme.t ->
  (ty_members, string) result
