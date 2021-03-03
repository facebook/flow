(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val string_of_scope_entry : 'phase Context.t_ -> Scope.Entry.t -> string

val string_of_scope_entries : 'phase Context.t_ -> Scope.Entry.t NameUtils.Map.t -> string

val string_of_scope_refi : 'phase Context.t_ -> Scope.refi_binding -> string

val string_of_scope_refis : 'phase Context.t_ -> Scope.refi_binding Key_map.t -> string

val string_of_scope : 'phase Context.t_ -> Scope.t -> string

val string_of_reason : 'phase Context.t_ -> Reason.t -> string

val string_of_file : 'phase Context.t_ -> string

val string_of_selector : Type.TypeTerm.selector -> string

val string_of_destructor : Type.TypeTerm.destructor -> string

val string_of_default : (Loc.t, Loc.t) Flow_ast.Expression.t Default.t -> string

val string_of_signature_error : ('loc -> string) -> 'loc Signature_error.t -> string

val dump_t : ?depth:int -> 'phase Context.t_ -> Type.t -> string

val dump_use_t : ?depth:int -> 'phase Context.t_ -> Type.use_t -> string

val dump_tvar : ?depth:int -> 'phase Context.t_ -> Type.ident -> string

val dump_prop : ?depth:int -> 'phase Context.t_ -> Type.Property.t -> string

val dump_reason : 'phase Context.t_ -> Reason.t -> string

val dump_error_message : 'phase Context.t_ -> Error_message.t -> string

val dump_flow : ?depth:int -> 'phase Context.t_ -> Type.t * Type.use_t -> string

module Verbose : sig
  val print_if_verbose_lazy :
    'phase Context.t_ -> Type.trace -> ?delim:string -> ?indent:int -> string list Lazy.t -> unit

  val print_if_verbose :
    'phase Context.t_ -> Type.trace -> ?delim:string -> ?indent:int -> string list -> unit

  val print_types_if_verbose :
    'phase Context.t_ -> Type.trace -> ?note:string -> Type.t * Type.use_t -> unit
end
