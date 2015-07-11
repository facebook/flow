(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type 'a error_
type error = Pos.t error_
type t = error list

val is_hh_fixme : (Pos.t -> int -> bool) ref
val to_list : 'a error_ -> ('a * string) list
val get_code : 'a error_ -> int
val get_pos : error -> Pos.t
val make_error : int -> (Pos.t * string) list -> error

val error_code_to_string : int -> string

val fixme_format : Pos.t -> unit
val typeparam_alok : Pos.t * string -> unit
val unexpected_eof : Pos.t -> unit
val missing_field : Pos.t -> Pos.t -> string -> unit
val generic_class_var : Pos.t -> unit
val explain_constraint : Pos.t -> Pos.t -> string -> error -> unit
val explain_type_constant : (Pos.t * string) list -> error -> unit
val unexpected_arrow : Pos.t -> string -> unit
val missing_arrow : Pos.t -> string -> unit
val disallowed_xhp_type : Pos.t -> string -> unit
val overflow : Pos.t -> unit
val unterminated_comment : Pos.t -> unit
val unterminated_xhp_comment : Pos.t -> unit
val name_already_bound : string -> Pos.t -> Pos.t -> unit
val method_name_already_bound : Pos.t -> string -> unit
val error_name_already_bound : string -> string -> Pos.t -> Pos.t -> unit
val unbound_name : Pos.t -> string -> [< `cls | `func | `const ] -> unit
val different_scope : Pos.t -> string -> Pos.t -> unit
val undefined : Pos.t -> string -> unit
val this_reserved : Pos.t -> unit
val start_with_T : Pos.t -> unit
val already_bound : Pos.t -> string -> unit
val unexpected_typedef : Pos.t -> Pos.t -> unit
val fd_name_already_bound : Pos.t -> unit
val primitive_toplevel : Pos.t -> unit
val primitive_invalid_alias : Pos.t -> string -> string -> unit
val dynamic_new_in_strict_mode : Pos.t -> unit
val void_cast: Pos.t -> unit
val object_cast: Pos.t -> string -> unit
val unset_cast: Pos.t -> unit
val this_no_argument : Pos.t -> unit
val this_hint_outside_class : Pos.t -> unit
val this_type_forbidden : Pos.t -> unit
val lowercase_this : Pos.t -> string -> unit
val classname_param : Pos.t -> unit
val tparam_with_tparam : Pos.t -> string -> unit
val shadowed_type_param : Pos.t -> Pos.t -> string -> unit
val missing_typehint : Pos.t -> unit
val expected_variable : Pos.t -> unit
val naming_too_few_arguments : Pos.t -> unit
val naming_too_many_arguments : Pos.t -> unit
val expected_collection : Pos.t -> string -> unit
val illegal_CLASS : Pos.t -> unit
val illegal_TRAIT : Pos.t -> unit
val dynamic_method_call : Pos.t -> unit
val nullsafe_property_write_context : Pos.t -> unit
val illegal_fun : Pos.t -> unit
val illegal_meth_fun : Pos.t -> unit
val illegal_inst_meth : Pos.t -> unit
val illegal_meth_caller : Pos.t -> unit
val illegal_class_meth : Pos.t -> unit
val assert_arity : Pos.t -> unit
val gena_arity : Pos.t -> unit
val genva_arity : Pos.t -> unit
val gen_array_rec_arity : Pos.t -> unit
val dynamic_class : Pos.t -> unit
val uninstantiable_class : Pos.t -> Pos.t -> string -> unit
val abstract_const_usage: Pos.t -> Pos.t -> string -> unit
val typedef_constraint : Pos.t -> unit
val add_a_typehint : Pos.t -> unit
val local_const : Pos.t -> unit
val illegal_constant : Pos.t -> unit
val cyclic_constraint : Pos.t -> unit
val parsing_error : Pos.t * string -> unit
val format_string :
  Pos.t -> string -> string -> Pos.t -> string -> string -> unit
val expected_literal_string : Pos.t -> unit
val generic_array_strict : Pos.t -> unit
val strict_members_not_known : Pos.t -> string -> unit
val option_return_only_typehint : Pos.t -> [< `void | `noreturn ] -> unit
val tuple_syntax : Pos.t -> unit
val class_arity : Pos.t -> Pos.t -> string -> int -> unit
val expecting_type_hint : Pos.t -> unit
val expecting_type_hint_suggest : Pos.t -> string -> unit
val expecting_return_type_hint : Pos.t -> unit
val expecting_return_type_hint_suggest : Pos.t -> string -> unit
val field_kinds : Pos.t -> Pos.t -> unit
val unbound_name_typing : Pos.t -> string -> unit
val did_you_mean_naming : Pos.t -> string -> Pos.t -> string -> unit
val previous_default : Pos.t -> unit
val nullable_parameter: Pos.t -> unit
val return_only_typehint : Pos.t -> [< `void | `noreturn ] -> unit
val unexpected_type_arguments : Pos.t -> unit
val too_many_type_arguments : Pos.t -> unit
val return_in_void : Pos.t -> Pos.t -> unit
val this_in_static : Pos.t -> unit
val this_var_outside_class : Pos.t -> unit
val unbound_global : Pos.t -> unit
val private_inst_meth : Pos.t -> Pos.t -> unit
val protected_inst_meth : Pos.t -> Pos.t -> unit
val private_class_meth : Pos.t -> Pos.t -> unit
val protected_class_meth : Pos.t -> Pos.t -> unit
val array_cast : Pos.t -> unit
val anonymous_recursive : Pos.t -> unit
val static_outside_class : Pos.t -> unit
val self_outside_class : Pos.t -> unit
val new_inconsistent_construct : Pos.t -> (Pos.t * string)
  -> [< `static | `classname ] -> unit
val pair_arity : Pos.t -> unit
val tuple_arity : Pos.t -> int -> Pos.t -> int -> unit
val undefined_parent : Pos.t -> unit
val parent_outside_class : Pos.t -> unit
val parent_abstract_call : string -> Pos.t -> Pos.t -> unit
val self_abstract_call : string -> Pos.t -> Pos.t -> unit
val classname_abstract_call : string -> string -> Pos.t -> Pos.t -> unit
val isset_empty_in_strict : Pos.t -> string -> unit
val unset_nonidx_in_strict : Pos.t -> (Pos.t * string) list -> unit
val array_get_arity : Pos.t -> string -> Pos.t -> unit
val typing_error : Pos.t -> string -> unit
val typing_error_l : error -> unit
val undefined_field : Pos.t -> string -> unit
val array_access : Pos.t -> Pos.t -> string -> unit
val array_append : Pos.t -> Pos.t -> string -> unit
val const_mutation : Pos.t -> Pos.t -> string -> unit
val expected_class : Pos.t -> unit
val smember_not_found :
  [< `class_constant | `class_variable | `static_method | `class_typeconst] ->
  Pos.t ->
  Pos.t * string ->
  string ->
  [< `closest of Pos.t * string | `did_you_mean of Pos.t * string | `no_hint ] ->
  unit
val not_found_hint :
  [< `closest of 'a * string | `did_you_mean of 'a * string | `no_hint ] ->
  ('a * string) list
val member_not_found :
  [< `member | `method_ ] ->
  Pos.t ->
  Pos.t * string ->
  string ->
  [< `closest of Pos.t * string | `did_you_mean of Pos.t * string | `no_hint ] ->
  (Pos.t * string) list ->
  unit
val parent_in_trait : Pos.t -> unit
val parent_undefined : Pos.t -> unit
val constructor_no_args : Pos.t -> unit
val visibility : Pos.t -> string -> Pos.t -> string -> unit
val typing_too_many_args : Pos.t -> Pos.t -> unit
val typing_too_few_args : Pos.t -> Pos.t -> unit
val anonymous_recursive_call : Pos.t -> unit
val bad_call : Pos.t -> string -> unit
val sketchy_null_check : Pos.t -> unit
val sketchy_null_check_primitive : Pos.t -> unit
val extend_final : Pos.t -> Pos.t -> string -> unit
val read_before_write : Pos.t * string -> unit
val interface_final : Pos.t -> unit
val trait_final : Pos.t -> unit
val implement_abstract : Pos.t -> Pos.t -> string -> string -> unit
val generic_static : Pos.t -> string -> unit
val fun_too_many_args : Pos.t -> Pos.t -> unit
val fun_too_few_args : Pos.t -> Pos.t -> unit
val fun_unexpected_nonvariadic : Pos.t -> Pos.t -> unit
val fun_variadicity_hh_vs_php56 : Pos.t -> Pos.t -> unit
val expected_tparam : Pos.t -> int -> unit
val object_string : Pos.t -> Pos.t -> unit
val type_param_arity : Pos.t -> string -> string -> unit
val cyclic_typedef : Pos.t -> unit
val type_arity_mismatch : Pos.t -> string -> Pos.t -> string -> unit
val this_final : Pos.t * string -> Pos.t -> error -> unit
val exact_class_final : Pos.t * string -> Pos.t -> error -> unit
val tuple_arity_mismatch : Pos.t -> string -> Pos.t -> string -> unit
val fun_arity_mismatch : Pos.t -> Pos.t -> unit
val discarded_awaitable : Pos.t -> Pos.t -> unit
val gena_expects_array : Pos.t -> Pos.t -> string -> unit
val unify_error : (Pos.t * string) list -> (Pos.t * string) list -> unit
val static_dynamic : Pos.t -> Pos.t -> string -> unit
val null_member : string -> Pos.t -> (Pos.t * string) list -> unit
val non_object_member : string -> Pos.t -> string -> Pos.t -> unit
val null_container : Pos.t -> (Pos.t * string) list -> unit
val option_mixed : Pos.t -> unit
val declared_covariant : Pos.t -> Pos.t -> (Pos.t * string) list -> unit
val declared_contravariant : Pos.t -> Pos.t -> (Pos.t * string) list -> unit
val wrong_extend_kind : Pos.t -> string -> Pos.t -> string -> unit
val unsatisfied_req : Pos.t -> string -> Pos.t -> unit
val cyclic_class_def : Utils.SSet.t -> Pos.t -> unit
val override_final : parent:Pos.t -> child:Pos.t -> unit
val should_be_override : Pos.t -> string -> string -> unit
val override_per_trait : Pos.t * string -> string -> Pos.t -> unit
val missing_assign : Pos.t -> unit
val private_override : Pos.t -> string -> string -> unit
val no_construct_parent : Pos.t -> unit
val constructor_required : Pos.t * string -> Utils.SSet.t -> unit
val not_initialized : Pos.t * string -> Utils.SSet.t -> unit
val call_before_init : Pos.t -> string -> unit
val type_arity : Pos.t -> string -> string -> unit
val invalid_req_implements : Pos.t -> unit
val invalid_req_extends : Pos.t -> unit
val abstract_with_body : Pos.t * 'a -> unit
val not_abstract_without_body : Pos.t * 'a -> unit
val return_in_gen : Pos.t -> unit
val return_in_finally : Pos.t -> unit
val toplevel_break: Pos.t -> unit
val toplevel_continue: Pos.t -> unit
val continue_in_switch: Pos.t -> unit
val await_in_sync_function : Pos.t -> unit
val magic : Pos.t * string -> unit
val non_interface : Pos.t -> string -> string -> unit
val toString_returns_string : Pos.t -> unit
val toString_visibility : Pos.t -> unit
val uses_non_trait : Pos.t -> string -> string -> unit
val requires_non_class : Pos.t -> string -> string -> unit
val abstract_body : Pos.t -> unit
val not_public_interface : Pos.t -> unit
val interface_with_member_variable : Pos.t -> unit
val interface_with_static_member_variable : Pos.t -> unit
val dangerous_method_name : Pos.t -> unit
val illegal_function_name : Pos.t -> string -> unit
val case_fallthrough : Pos.t -> Pos.t -> unit
val default_fallthrough : Pos.t -> unit
val visibility_extends : string -> Pos.t -> Pos.t -> string -> unit
val member_not_implemented : string -> Pos.t -> Pos.t -> Pos.t -> unit
val bad_decl_override : Pos.t -> string -> Pos.t -> string -> error -> unit
val missing_constructor : Pos.t -> unit
val enum_constant_type_bad : Pos.t -> Pos.t -> string -> Pos.t list -> unit
val enum_type_bad : Pos.t -> string -> Pos.t list -> unit
val enum_type_typedef_mixed : Pos.t -> unit
val enum_switch_redundant : string -> Pos.t -> Pos.t -> unit
val enum_switch_nonexhaustive : Pos.t -> string list -> Pos.t -> unit
val enum_switch_redundant_default : Pos.t -> Pos.t -> unit
val enum_switch_not_const : Pos.t -> unit
val enum_switch_wrong_class : Pos.t -> string -> string -> unit
val invalid_shape_field_name : Pos.t -> unit
val invalid_shape_field_name_empty : Pos.t -> unit
val invalid_shape_field_name_number : Pos.t -> unit
val invalid_shape_field_type : Pos.t -> Pos.t -> string -> Pos.t list -> unit
val invalid_shape_field_literal : Pos.t -> Pos.t -> unit
val invalid_shape_field_const : Pos.t -> Pos.t -> unit
val shape_field_class_mismatch : Pos.t -> Pos.t -> string -> string -> unit
val shape_field_type_mismatch : Pos.t -> Pos.t -> string -> string -> unit
val shape_fields_unknown: Pos.t -> Pos.t  -> unit
val invalid_shape_remove_key : Pos.t -> unit
val missing_optional_field : Pos.t -> Pos.t -> string -> unit
val shape_field_unset : Pos.t -> Pos.t -> string -> unit
val using_internal_class : Pos.t -> string -> unit
val nullsafe_not_needed : Pos.t -> (Pos.t * string) list -> unit
val trivial_strict_eq : Pos.t -> string -> (Pos.t * string) list
  -> (Pos.t * string) list -> Pos.t list -> Pos.t list -> unit
val void_usage : Pos.t -> (Pos.t * string) list -> unit
val noreturn_usage : Pos.t -> (Pos.t * string) list -> unit
val generic_at_runtime : Pos.t -> unit
val not_abstract_without_typeconst : (Pos.t * string) -> unit
val typeconst_depends_on_external_tparam : Pos.t -> Pos.t -> string -> unit
val typeconst_assigned_tparam : Pos.t -> string -> unit
val invalid_type_access_root : (Pos.t * string) -> unit
val duplicate_user_attribute : (Pos.t * string) -> Pos.t -> unit
val unbound_attribute_name : Pos.t -> string -> unit
val attribute_arity : Pos.t -> string -> int -> unit
val attribute_param_type : Pos.t -> string -> unit
val deprecated_use : Pos.t -> Pos.t -> string -> unit
val abstract_with_typeconst : (Pos.t * string) -> unit
val cannot_declare_constant:
  [< `enum | `trait] -> Pos.t -> (Pos.t * string) -> unit
val ambiguous_inheritance: Pos.t -> string -> string -> error -> unit
val cyclic_typeconst : Pos.t -> string list -> unit
val explain_contravariance : Pos.t -> string -> error -> unit
val this_lvalue : Pos.t -> unit

val to_json : Pos.absolute error_ -> Hh_json.json
val to_string : Pos.absolute error_ -> string
val try_ : (unit -> 'a) -> (error -> 'a) -> 'a
val try_with_error : (unit -> 'a) -> (unit -> 'a) -> 'a
val try_add_err : Pos.t -> string -> (unit -> 'a) -> (unit -> 'a) -> 'a
val do_ : (unit -> 'a) -> error list * 'a
val ignore_ : (unit -> 'a) -> 'a
val try_when :
  (unit -> 'a) -> when_:(unit -> bool) -> do_:(error -> unit) -> 'a
val has_no_errors : (unit -> 'a) -> bool
val must_error : (unit -> unit) -> (unit -> unit) -> unit
val to_absolute : error -> Pos.absolute error_
