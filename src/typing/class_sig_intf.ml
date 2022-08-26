(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Config_types : Func_class_sig_types.Config.S

  module Config : Func_params_intf.Config with module Types := Config_types

  module Param :
    Func_params_intf.S with module Config_types := Config_types and module Config := Config

  module Func :
    Func_sig_intf.S
      with module Config_types := Config_types
       and module Config := Config
       and module Param := Param

  module Types :
    Func_class_sig_types.Class.S
      with module Config := Config_types
       and module Param := Param.Types
       and module Func := Func.Types

  (** 1. Constructors **)

  open Types

  (** Create signature with no elements. *)
  val empty :
    ALoc.id -> ALoc.t -> Reason.t -> Type.typeparams -> Type.t Subst_name.Map.t -> super -> t

  (** Add constructor to signature.

      Overwrites any existing constructor. This implements the behavior of
      classes, which permit duplicate definitions where latter definitions
      overwrite former ones. *)
  val add_constructor :
    id_loc:ALoc.t option -> func_sig:func_sig -> ?set_asts:set_asts -> ?set_type:set_type -> t -> t

  val add_default_constructor : Reason.t -> t -> t

  (** Add constructor override to signature.

      Does not overwrite existing constructors. This implements the behavior of
      interfaces, which interpret duplicate definitions as branches of a single
      overloaded constructor. *)
  val append_constructor :
    id_loc:ALoc.t option -> func_sig:func_sig -> ?set_asts:set_asts -> ?set_type:set_type -> t -> t

  (** Add field to signature. *)
  val add_field : static:bool -> string -> ALoc.t -> Polarity.t -> field -> t -> t

  (** Add indexer to signature. *)
  val add_indexer : static:bool -> Polarity.t -> key:Type.t -> value:Type.t -> t -> t

  (** Add static `name` field. *)
  val add_name_field : t -> t

  (** Add proto field to signature. *)
  val add_proto_field : string -> ALoc.t -> Polarity.t -> field -> t -> t

  (** Add private field to signature. *)
  val add_private_field : string -> ALoc.t -> Polarity.t -> field -> static:bool -> t -> t

  (** Add private method to signature. *)
  val add_private_method :
    static:bool ->
    string ->
    id_loc:ALoc.t ->
    this_write_loc:ALoc.t option ->
    func_sig:func_sig ->
    set_asts:set_asts ->
    set_type:set_type ->
    t ->
    t

  (* Access public fields of signature *)
  val public_fields_of_signature : static:bool -> t -> field' SMap.t

  (* Access private fields of signature *)
  val private_fields_of_signature : static:bool -> t -> field' SMap.t

  val mk_class_binding : Context.t -> t -> Type.class_binding

  (** Add method to signature.

      Overwrites any existing synonymous method. This implements the behavior of
      classes, which permit duplicate definitions where latter definitions
      overwrite former ones. *)
  val add_method :
    static:bool ->
    string ->
    id_loc:ALoc.t ->
    this_write_loc:ALoc.t option ->
    func_sig:func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t

  (** Add method override to signature.

      Does not overwrite existing synonymous methods. This implements the
      behavior of interfaces, which interpret duplicate definitions as branches
      of a single overloaded method. *)
  val append_method :
    static:bool ->
    string ->
    id_loc:ALoc.t ->
    this_write_loc:ALoc.t option ->
    func_sig:func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t

  val append_call : static:bool -> Type.t -> t -> t

  (** Add getter to signature. *)
  val add_getter :
    static:bool ->
    string ->
    id_loc:ALoc.t ->
    this_write_loc:ALoc.t option ->
    func_sig:func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t

  (** Add setter to signature. *)
  val add_setter :
    static:bool ->
    string ->
    id_loc:ALoc.t ->
    this_write_loc:ALoc.t option ->
    func_sig:func_sig ->
    ?set_asts:set_asts ->
    ?set_type:set_type ->
    t ->
    t

  (** Check if this signature defines a given field *)
  val mem_field : string -> static:bool -> t -> bool

  (** Check if this signature defines a constructor *)
  val mem_constructor : t -> bool

  val mk_this :
    Type.t -> (* self *)
              Context.t -> Reason.t -> Type.typeparams -> Type.typeparam * Type.t

  val this_or_mixed_of_t : static:bool -> t -> Type.t

  val fields_to_prop_map : Context.t -> field' SMap.t -> Type.Properties.id

  (** 1. Manipulation *)

  (** Emits constraints to ensure the signature is compatible with its declared
      interface implementations (classes) *)
  val check_implements : Context.t -> Reason.reason -> t -> unit

  (** Emits constraints to ensure the signature is compatible with its declared
      superclass (classes) or extends/mixins (interfaces) *)
  val check_super : Context.t -> Reason.reason -> t -> unit

  (** Emits constraints to ensure that the signature's methods are compatible
      with its type **)
  val check_methods : Context.t -> Reason.reason -> t -> unit

  val make_thises : Context.t -> t -> Type.t * Type.t * Type.t * Type.t

  (** Evaluate the class body. *)
  val toplevels : Context.t -> t -> unit

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
