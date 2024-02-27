(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type

module type BASE = sig
  val flow : Context.t -> Type.t * Type.use_t -> unit

  val flow_opt : Context.t -> ?trace:Type.trace -> Type.t * Type.use_t -> unit

  val flow_p :
    Context.t ->
    use_op:use_op ->
    reason ->
    reason ->
    Type.propref ->
    Type.property_type * Type.property_type ->
    unit

  val flow_t : Context.t -> Type.t * Type.t -> unit

  val reposition :
    Context.t ->
    ?trace:Type.trace ->
    ALoc.t ->
    ?desc:reason_desc ->
    ?annot_loc:ALoc.t ->
    Type.t ->
    Type.t

  val rec_flow : Context.t -> Type.trace -> Type.t * Type.use_t -> unit

  val rec_flow_t : Context.t -> Type.trace -> use_op:Type.use_op -> Type.t * Type.t -> unit

  val rec_unify :
    Context.t -> Type.trace -> use_op:Type.use_op -> ?unify_any:bool -> Type.t -> Type.t -> unit

  val unify : Context.t -> ?use_op:Type.use_op -> Type.t -> Type.t -> unit

  val unify_opt :
    Context.t ->
    ?trace:Type.trace ->
    use_op:Type.use_op ->
    ?unify_any:bool ->
    Type.t ->
    Type.t ->
    unit

  val filter_optional : Context.t -> ?trace:Type.trace -> reason -> Type.t -> Type.ident

  val mk_typeapp_instance_annot :
    Context.t ->
    ?trace:Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    from_value:bool ->
    ?cache:bool ->
    Type.t ->
    Type.t list ->
    Type.t

  val mk_typeapp_instance :
    Context.t ->
    ?trace:Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    from_value:bool ->
    Type.t ->
    Type.t list ->
    Type.t

  val resolve_id :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    ?fully_resolved:bool ->
    Type.ident ->
    Type.t ->
    unit

  val flow_use_op : Context.t -> Type.use_op -> Type.use_t -> Type.use_t
end

module type CHECK_POLARITY = sig
  val check_polarity :
    Context.t ->
    ?trace:Type.trace ->
    Type.typeparam Subst_name.Map.t ->
    Polarity.t ->
    Type.t ->
    unit
end

module type BUILTINS = sig
  val get_builtin_type :
    Context.t -> ?trace:Type.trace -> Reason.reason -> ?use_desc:bool -> string -> Type.t

  val get_builtin_typeapp : Context.t -> reason -> ?use_desc:bool -> string -> Type.t list -> Type.t

  val perform_read_prop_action :
    Context.t ->
    Type.trace ->
    ALoc.t Type.virtual_use_op ->
    Type.propref ->
    Type.property_type ->
    Reason.reason ->
    (ALoc.t * Type.dro_type) option ->
    Type.tvar ->
    unit
end

module type SUBTYPING = sig
  val speculative_subtyping_succeeds : Context.t -> Type.t -> Type.t -> bool

  val possible_concrete_types_for_inspection : Context.t -> Reason.reason -> Type.t -> Type.t list

  val possible_concrete_types_for_imports_exports :
    Context.t -> Reason.reason -> Type.t -> Type.t list

  val reposition_reason :
    Context.t -> ?trace:Type.trace -> Reason.reason -> ?use_desc:bool -> Type.t -> Type.t

  val eval_destructor :
    Context.t ->
    trace:Type.trace ->
    Type.use_op ->
    Reason.reason ->
    Type.t ->
    Type.destructor ->
    Type.tvar ->
    unit

  val multiflow_subtype :
    Context.t ->
    Type.trace ->
    use_op:ALoc.t Type.virtual_use_op ->
    Reason.reason ->
    Type.call_arg list ->
    Type.funtype ->
    unit

  val flow_type_args :
    Context.t ->
    Type.trace ->
    use_op:use_op ->
    reason ->
    reason ->
    (Subst_name.t * reason * Type.t * Polarity.t) list ->
    (Subst_name.t * reason * Type.t * Polarity.t) list ->
    unit

  val instantiate_this_class :
    Context.t ->
    Type.trace ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.t ->
    Type.t list option ->
    Type.t ->
    Type.cont ->
    unit

  val instantiate_poly_with_targs :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.t ->
    ?cache:bool ->
    ?errs_ref:Context.subst_cache_err list ref ->
    ?unify_bounds:bool ->
    ALoc.t * Type.typeparam Nel.t * Type.t ->
    Type.t list ->
    Type.t * (Type.t * Subst_name.t) list

  val instantiate_poly :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    ?cache:bool ->
    ?unify_bounds:bool ->
    ALoc.t * Type.typeparam Nel.t * Type.t ->
    Type.t * (Type.t * Subst_name.t) list

  val mk_typeapp_of_poly :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    ?cache:bool ->
    Type.Poly.id ->
    ALoc.t ->
    Type.typeparam Nel.t ->
    Type.t ->
    Type.t list ->
    Type.t

  val mk_instance :
    Context.t ->
    ?type_t_kind:Type.type_t_kind ->
    ?trace:Type.trace ->
    reason ->
    ?use_desc:bool ->
    Type.t ->
    Type.t
end

module type EVAL = sig
  val eval_selector :
    Context.t ->
    ?trace:Type.trace ->
    annot:bool ->
    reason ->
    Type.t ->
    Type.selector ->
    Type.tvar ->
    int ->
    unit

  val mk_type_destructor :
    Context.t ->
    trace:Type.trace ->
    use_op ->
    reason ->
    Type.t ->
    Type.destructor ->
    Type.Eval.id ->
    Type.t

  val mk_possibly_evaluated_destructor :
    Context.t -> Type.use_op -> Reason.reason -> Type.t -> Type.destructor -> Type.Eval.id -> Type.t
end

module type REACT = sig
  val react_subtype_class_component_render :
    Context.t -> Type.trace -> use_op:Type.use_op -> Type.t -> reason_op:reason -> Type.t -> unit

  val react_get_config :
    Context.t ->
    Type.trace ->
    Type.t ->
    use_op:ALoc.t Type.virtual_use_op ->
    reason_op:Reason.reason ->
    Type.React.tool ->
    Polarity.t ->
    Type.t ->
    unit
end

module type S = sig
  include BASE

  include BUILTINS

  include EVAL

  include REACT

  include SUBTYPING

  include CHECK_POLARITY

  val resolve_spread_list :
    Context.t ->
    use_op:use_op ->
    reason_op:reason ->
    unresolved_param list ->
    spread_resolve ->
    unit

  val possible_concrete_types_for_inspection : Context.t -> Reason.reason -> Type.t -> Type.t list

  val singleton_concrete_type_for_inspection : Context.t -> Reason.reason -> Type.t -> Type.t

  val possible_concrete_types_for_computed_props :
    Context.t -> Reason.reason -> Type.t -> Type.t list
end
