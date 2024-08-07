(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a member_info = {
  ty: 'a;
  def_locs: ALoc.t list;
  inherited: bool;
  source: Ty.prop_source;
  from_nullable: bool;
      (** If a member came from a possibly-null/undefined object, autocomplete may suggest
          that the user use optional chaining to access it.
          [from_nullable] indicates that the member is from a possibly-null/undefined object. *)
}

type ty_members = {
  members: Ty.t member_info NameUtils.Map.t;
  errors: string list;
}

val extract :
  ?force_instance:bool ->
  ?max_depth:int ->
  cx:Context.t ->
  typed_ast_opt:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
  file_sig:File_sig.t ->
  Type.t ->
  (ty_members, string) result
