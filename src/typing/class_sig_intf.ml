(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type func_sig

  type func_params_tast

  type t

  type set_asts =
    func_params_tast option
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Function.body option
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t option ->
    unit

  type set_type = Type.t -> unit

  and field =
    | Annot of Type.t
    | Infer of func_sig * set_asts

  type field' = ALoc.t option * Polarity.t * field

  type super =
    | Interface of {
        inline: bool;
        extends: typeapp list;
        callable: bool;
      }
    | Class of {
        extends: extends;
        mixins: typeapp list;
        (* declare class only *)
        implements: typeapp list;
      }

  and extends =
    | Explicit of typeapp
    | Implicit of { null: bool }

  and typeapp = ALoc.t * Type.t * Type.t list option

  (** 1. Constructors **)

  val empty : ALoc.id -> Reason.t -> Type.typeparams -> Type.t SMap.t -> super -> t
  (** Create signature with no elements. *)

  val add_constructor :
    ALoc.t option -> func_sig -> ?set_asts:set_asts -> ?set_type:set_type -> t -> t
  (** Add constructor to signature.

      Overwrites any existing constructor. This implements the behavior of
      classes, which permit duplicate definitions where latter definitions
      overwrite former ones. *)

  val add_default_constructor : Reason.t -> t -> t

  val append_constructor :
    ALoc.t option -> func_sig -> ?set_asts:set_asts -> ?set_type:set_type -> t -> t
  (** Add constructor override to signature.

      Does not overwrite existing constructors. This implements the behavior of
      interfaces, which interpret duplicate definitions as branches of a single
      overloaded constructor. *)

  val add_field : static:bool -> string -> ALoc.t -> Polarity.t -> field -> t -> t
  (** Add field to signature. *)

  val add_indexer : static:bool -> Polarity.t -> key:Type.t -> value:Type.t -> t -> t
  (** Add indexer to signature. *)

  val add_name_field : t -> t
  (** Add static `name` field. *)

  val add_proto_field : string -> ALoc.t -> Polarity.t -> field -> t -> t
  (** Add proto field to signature. *)

  val add_private_field : string -> ALoc.t -> Polarity.t -> field -> static:bool -> t -> t
  (** Add private field to signature. *)

  (* Access public fields of signature *)
  val public_fields_of_signature : static:bool -> t -> field' SMap.t

  (* Access private fields of signature *)
  val private_fields_of_signature : static:bool -> t -> field' SMap.t

  val add_method :
    static:bool ->
    string ->
    ALoc.t ->
    func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t
  (** Add method to signature.

      Overwrites any existing synonymous method. This implements the behavior of
      classes, which permit duplicate definitions where latter definitions
      overwrite former ones. *)

  val append_method :
    static:bool ->
    string ->
    ALoc.t ->
    func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t
  (** Add method override to signature.

      Does not overwrite existing synonymous methods. This implements the
      behavior of interfaces, which interpret duplicate definitions as branches
      of a single overloaded method. *)

  val append_call : static:bool -> Type.t -> t -> t

  val add_getter :
    static:bool ->
    string ->
    ALoc.t ->
    func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t
  (** Add getter to signature. *)

  val add_setter :
    static:bool ->
    string ->
    ALoc.t ->
    func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t
  (** Add setter to signature. *)

  val mem_field : string -> static:bool -> t -> bool
  (** Check if this signature defines a given field *)

  val mem_constructor : t -> bool
  (** Check if this signature defines a constructor *)

  val add_this :
    Type.t ->
    (* self *)
    Context.t ->
    Reason.t ->
    Type.typeparams ->
    Type.t SMap.t ->
    (* tparams_map *)
    Type.typeparams * Type.t SMap.t

  val to_prop_map : Context.t -> field' SMap.t -> Type.Properties.id

  (** 1. Manipulation *)

  val check_implements : Context.t -> Reason.reason -> t -> unit
  (** Emits constraints to ensure the signature is compatible with its declared
      interface implementations (classes) *)

  val check_super : Context.t -> Reason.reason -> t -> unit
  (** Emits constraints to ensure the signature is compatible with its declared
      superclass (classes) or extends/mixins (interfaces) *)

  val check_methods : Context.t -> Reason.reason -> t -> unit
  (** Emits constraints to ensure that the signature's methods are compatible
      with its type **)

  val check_with_generics : Context.t -> (t -> 'a) -> t -> 'a
  (** Invoke callback with type parameters substituted by upper/lower bounds. *)

  val toplevels :
    Context.t ->
    decls:(Context.t -> (ALoc.t, ALoc.t) Flow_ast.Statement.t list -> unit) ->
    stmts:
      (Context.t ->
      (ALoc.t, ALoc.t) Flow_ast.Statement.t list ->
      (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list) ->
    expr:
      (?cond:Type.cond_context ->
      Context.t ->
      annot:unit option ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
    private_property_map:Type.Properties.id ->
    t ->
    unit
  (** Evaluate the class body. *)

  (** 1. Type Conversion *)

  val thistype : Context.t -> t -> Type.t

  (* Create a (polymorphic) class type. In the return tuple, the first type is the internal view of the
     class and the second type is the external view--which differ because the internal view can be
     comparible with `this`, while the external view shouldn't be. *)
  val classtype : Context.t -> ?check_polarity:bool -> t -> Type.t * Type.t

  module This : sig
    val is_bound_to_empty : t -> bool

    val in_class : (ALoc.t, ALoc.t) Flow_ast.Class.t -> bool
  end
end
