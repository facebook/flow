(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val string_of_scope_entry : Context.t -> Scope.Entry.t -> string

val string_of_scope_entries : Context.t -> Scope.Entry.t SMap.t -> string

val string_of_scope_refi : Context.t -> Scope.refi_binding -> string

val string_of_scope_refis : Context.t -> Scope.refi_binding Key_map.t -> string

val string_of_scope : Context.t -> Scope.t -> string

val string_of_reason : Context.t -> Reason.t -> string

val string_of_file : Context.t -> string

val string_of_selector : Type.TypeTerm.selector -> string

val string_of_destructor : Type.TypeTerm.destructor -> string

val string_of_default : (Loc.t, Loc.t) Flow_ast.Expression.t Default.t -> string

val string_of_signature_error : ('loc -> string) -> 'loc Signature_error.t -> string

val dump_t : ?depth:int -> Context.t -> Type.t -> string

val dump_use_t : ?depth:int -> Context.t -> Type.use_t -> string

val dump_tvar : ?depth:int -> Context.t -> Constraint.ident -> string

val dump_prop : ?depth:int -> Context.t -> Type.Property.t -> string

val dump_reason : Context.t -> Reason.t -> string

val dump_error_message : Context.t -> Error_message.t -> string

val dump_flow : ?depth:int -> Context.t -> Type.t * Type.use_t -> string

module Verbose : sig
  val print_if_verbose_lazy :
    Context.t -> Type.trace -> ?delim:string -> ?indent:int -> string list Lazy.t -> unit

  val print_if_verbose :
    Context.t -> Type.trace -> ?delim:string -> ?indent:int -> string list -> unit

  val print_types_if_verbose :
    Context.t -> Type.trace -> ?note:string -> Type.t * Type.use_t -> unit
end
