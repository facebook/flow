(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type error_code = int
(* We use `Pos.t message` on the server and convert to `Pos.absolute message`
 * before sending it to the client *)
type 'a message = 'a * string
type 'a error_ = error_code * 'a message list
type error = Pos.t error_
type t = error list

(*****************************************************************************)
(* HH_FIXMEs hook *)
(*****************************************************************************)

let (is_hh_fixme: (Pos.t -> error_code -> bool) ref) = ref (fun _ _ -> false)

(*****************************************************************************)
(* Errors accumulator. *)
(*****************************************************************************)

let (error_list: t ref) = ref []
let accumulate_errors = ref false

let add_error error =
  if !accumulate_errors
  then error_list := error :: !error_list
  else
    (* We have an error, but haven't handled it in any way *)
    assert false

let add code pos msg =
  if !is_hh_fixme pos code then () else
  add_error (code, [pos, msg])

let add_list code pos_msg_l =
  let pos = fst (List.hd pos_msg_l) in
  if !is_hh_fixme pos code then () else
  add_error (code, pos_msg_l)

(*****************************************************************************)
(* Accessors. *)
(*****************************************************************************)

let get_code (error: 'a error_) = ((fst error): error_code)
let get_pos (error : error) = fst (List.hd (snd error))
let to_list (error : 'a error_) = snd error

let make_error code (x: (Pos.t * string) list) = ((code, x): error)

(*****************************************************************************)
(* Error code printing. *)
(*****************************************************************************)

let error_kind error_code =
  match error_code / 1000 with
  | 1 -> "Parsing"
  | 2 -> "Naming"
  | 3 -> "NastCheck"
  | 4 -> "Typing"
  | 5 -> "Lint"
  | _ -> "Other"

let error_code_to_string error_code =
  let error_kind = error_kind error_code in
  let error_number = string_of_int error_code in
  error_kind^"["^error_number^"]"

(*****************************************************************************)
(* Error codes.
 * Each error has a unique number associated with it. The following modules
 * define the error code associated with each kind of error.
 * It is ok to extend the codes with new values, it is NOT OK to change the
 * value of an existing error to a different error code!
 * I added some comments to make that extra clear :-)
 *)
(*****************************************************************************)

module Parsing = struct
  let fixme_format                          = 1001 (* DONT MODIFY!!!! *)
  let parsing_error                         = 1002 (* DONT MODIFY!!!! *)
  let unexpected_eof                        = 1003 (* DONT MODIFY!!!! *)
  let unterminated_comment                  = 1004 (* DONT MODIFY!!!! *)
  let unterminated_xhp_comment              = 1005 (* DONT MODIFY!!!! *)

  (* EXTEND HERE WITH NEW VALUES IF NEEDED *)
end

module Naming                               = struct
  let add_a_typehint                        = 2001 (* DONT MODIFY!!!! *)
  let typeparam_alok                        = 2002 (* DONT MODIFY!!!! *)
  let assert_arity                          = 2003 (* DONT MODIFY!!!! *)
  let primitive_invalid_alias               = 2004 (* DONT MODIFY!!!! *)
  let cyclic_constraint                     = 2005 (* DONT MODIFY!!!! *)
  let did_you_mean_naming                   = 2006 (* DONT MODIFY!!!! *)
  let different_scope                       = 2007 (* DONT MODIFY!!!! *)
  let disallowed_xhp_type                   = 2008 (* DONT MODIFY!!!! *)
  (* DEPRECATED let double_instead_of_float = 2009 *)
  (* DEPRECATED let dynamic_class           = 2010 *)
  let dynamic_method_call                   = 2011 (* DONT MODIFY!!!! *)
  let error_name_already_bound              = 2012 (* DONT MODIFY!!!! *)
  let expected_collection                   = 2013 (* DONT MODIFY!!!! *)
  let expected_variable                     = 2014 (* DONT MODIFY!!!! *)
  let fd_name_already_bound                 = 2015 (* DONT MODIFY!!!! *)
  let gen_array_rec_arity                   = 2016 (* DONT MODIFY!!!! *)
  (* let gen_array_va_rec_arity             = 2017 *)
  let gena_arity                            = 2018 (* DONT MODIFY!!!! *)
  let generic_class_var                     = 2019 (* DONT MODIFY!!!! *)
  let genva_arity                           = 2020 (* DONT MODIFY!!!! *)
  let illegal_CLASS                         = 2021 (* DONT MODIFY!!!! *)
  let illegal_class_meth                    = 2022 (* DONT MODIFY!!!! *)
  let illegal_constant                      = 2023 (* DONT MODIFY!!!! *)
  let illegal_fun                           = 2024 (* DONT MODIFY!!!! *)
  let illegal_inst_meth                     = 2025 (* DONT MODIFY!!!! *)
  let illegal_meth_caller                   = 2026 (* DONT MODIFY!!!! *)
  let illegal_meth_fun                      = 2027 (* DONT MODIFY!!!! *)
  (* DEPRECATED integer_instead_of_int      = 2028 *)
  let invalid_req_extends                   = 2029 (* DONT MODIFY!!!! *)
  let invalid_req_implements                = 2030 (* DONT MODIFY!!!! *)
  let local_const                           = 2031 (* DONT MODIFY!!!! *)
  let lowercase_this                        = 2032 (* DONT MODIFY!!!! *)
  let method_name_already_bound             = 2033 (* DONT MODIFY!!!! *)
  let missing_arrow                         = 2034 (* DONT MODIFY!!!! *)
  let missing_typehint                      = 2035 (* DONT MODIFY!!!! *)
  let name_already_bound                    = 2036 (* DONT MODIFY!!!! *)
  let naming_too_few_arguments              = 2037 (* DONT MODIFY!!!! *)
  let naming_too_many_arguments             = 2038 (* DONT MODIFY!!!! *)
  let primitive_toplevel                    = 2039 (* DONT MODIFY!!!! *)
  (* DEPRECATED let real_instead_of_float   = 2040 *)
  let shadowed_type_param                   = 2041 (* DONT MODIFY!!!! *)
  let start_with_T                          = 2042 (* DONT MODIFY!!!! *)
  let this_must_be_return                   = 2043 (* DONT MODIFY!!!! *)
  let this_no_argument                      = 2044 (* DONT MODIFY!!!! *)
  let this_hint_outside_class               = 2045 (* DONT MODIFY!!!! *)
  let this_reserved                         = 2046 (* DONT MODIFY!!!! *)
  let tparam_with_tparam                    = 2047 (* DONT MODIFY!!!! *)
  let typedef_constraint                    = 2048 (* DONT MODIFY!!!! *)
  let unbound_name                          = 2049 (* DONT MODIFY!!!! *)
  let undefined                             = 2050 (* DONT MODIFY!!!! *)
  let unexpected_arrow                      = 2051 (* DONT MODIFY!!!! *)
  let unexpected_typedef                    = 2052 (* DONT MODIFY!!!! *)
  let using_internal_class                  = 2053 (* DONT MODIFY!!!! *)
  let void_cast                             = 2054 (* DONT MODIFY!!!! *)
  let object_cast                           = 2055 (* DONT MODIFY!!!! *)
  let unset_cast                            = 2056 (* DONT MODIFY!!!! *)
  (* DEPRECATED let nullsafe_property_access = 2057 *)
  let illegal_TRAIT                         = 2058 (* DONT MODIFY!!!! *)
  (* DEPRECATED let shape_typehint          = 2059  *)
  let dynamic_new_in_strict_mode            = 2060 (* DONT MODIFY!!!! *)
  let invalid_type_access_root              = 2061 (* DONT MODIFY!!!! *)
  let duplicate_user_attribute              = 2062 (* DONT MODIFY!!!! *)
  let return_only_typehint                  = 2063 (* DONT MODIFY!!!! *)
  let unexpected_type_arguments             = 2064 (* DONT MODIFY!!!! *)
  let too_many_type_arguments               = 2065 (* DONT MODIFY!!!! *)
  let classname_param                       = 2066 (* DONT MODIFY!!!! *)

  (* EXTEND HERE WITH NEW VALUES IF NEEDED *)
end

module NastCheck                            = struct
  let abstract_body                         = 3001 (* DONT MODIFY!!!! *)
  let abstract_with_body                    = 3002 (* DONT MODIFY!!!! *)
  let await_in_sync_function                = 3003 (* DONT MODIFY!!!! *)
  let call_before_init                      = 3004 (* DONT MODIFY!!!! *)
  let case_fallthrough                      = 3005 (* DONT MODIFY!!!! *)
  let continue_in_switch                    = 3006 (* DONT MODIFY!!!! *)
  let dangerous_method_name                 = 3007 (* DONT MODIFY!!!! *)
  let default_fallthrough                   = 3008 (* DONT MODIFY!!!! *)
  let interface_with_member_variable        = 3009 (* DONT MODIFY!!!! *)
  let interface_with_static_member_variable = 3010 (* DONT MODIFY!!!! *)
  let magic                                 = 3011 (* DONT MODIFY!!!! *)
  let no_construct_parent                   = 3012 (* DONT MODIFY!!!! *)
  let non_interface                         = 3013 (* DONT MODIFY!!!! *)
  let not_abstract_without_body             = 3014 (* DONT MODIFY!!!! *)
  let not_initialized                       = 3015 (* DONT MODIFY!!!! *)
  let not_public_interface                  = 3016 (* DONT MODIFY!!!! *)
  let requires_non_class                    = 3017 (* DONT MODIFY!!!! *)
  let return_in_finally                     = 3018 (* DONT MODIFY!!!! *)
  let return_in_gen                         = 3019 (* DONT MODIFY!!!! *)
  let toString_returns_string               = 3020 (* DONT MODIFY!!!! *)
  let toString_visibility                   = 3021 (* DONT MODIFY!!!! *)
  let toplevel_break                        = 3022 (* DONT MODIFY!!!! *)
  let toplevel_continue                     = 3023 (* DONT MODIFY!!!! *)
  let uses_non_trait                        = 3024 (* DONT MODIFY!!!! *)
  let illegal_function_name                 = 3025 (* DONT MODIFY!!!! *)
  let not_abstract_without_typeconst        = 3026 (* DONT MODIFY!!!! *)
  let typeconst_depends_on_external_tparam  = 3027 (* DONT MODIFY!!!! *)
  let typeconst_assigned_tparam             = 3028 (* DONT MODIFY!!!! *)
  let abstract_with_typeconst               = 3029 (* DONT MODIFY!!!! *)
  let constructor_required                  = 3030 (* DONT MODIFY!!!! *)

  (* EXTEND HERE WITH NEW VALUES IF NEEDED *)
end

module Typing                               = struct
  (* let abstract_class_final                  = 4001 (\* DONT MODIFY!!!! *\) *)
  let uninstantiable_class                  = 4002 (* DONT MODIFY!!!! *)
  let anonymous_recursive                   = 4003 (* DONT MODIFY!!!! *)
  let anonymous_recursive_call              = 4004 (* DONT MODIFY!!!! *)
  let array_access                          = 4005 (* DONT MODIFY!!!! *)
  let array_append                          = 4006 (* DONT MODIFY!!!! *)
  let array_cast                            = 4007 (* DONT MODIFY!!!! *)
  let array_get_arity                       = 4008 (* DONT MODIFY!!!! *)
  let bad_call                              = 4009 (* DONT MODIFY!!!! *)
  let class_arity                           = 4010 (* DONT MODIFY!!!! *)
  let const_mutation                        = 4011 (* DONT MODIFY!!!! *)
  let constructor_no_args                   = 4012 (* DONT MODIFY!!!! *)
  let cyclic_class_def                      = 4013 (* DONT MODIFY!!!! *)
  let cyclic_typedef                        = 4014 (* DONT MODIFY!!!! *)
  let discarded_awaitable                   = 4015 (* DONT MODIFY!!!! *)
  let isset_empty_in_strict                 = 4016 (* DONT MODIFY!!!! *)
  (* DEPRECATED dynamic_yield_private       = 4017 *)
  let enum_constant_type_bad                = 4018 (* DONT MODIFY!!!! *)
  let enum_switch_nonexhaustive             = 4019 (* DONT MODIFY!!!! *)
  let enum_switch_not_const                 = 4020 (* DONT MODIFY!!!! *)
  let enum_switch_redundant                 = 4021 (* DONT MODIFY!!!! *)
  let enum_switch_redundant_default         = 4022 (* DONT MODIFY!!!! *)
  let enum_switch_wrong_class               = 4023 (* DONT MODIFY!!!! *)
  let enum_type_bad                         = 4024 (* DONT MODIFY!!!! *)
  let enum_type_typedef_mixed               = 4025 (* DONT MODIFY!!!! *)
  let expected_class                        = 4026 (* DONT MODIFY!!!! *)
  let expected_literal_string               = 4027 (* DONT MODIFY!!!! *)
  (* DEPRECATED expected_static_int         = 4028 *)
  let expected_tparam                       = 4029 (* DONT MODIFY!!!! *)
  let expecting_return_type_hint            = 4030 (* DONT MODIFY!!!! *)
  let expecting_return_type_hint_suggest    = 4031 (* DONT MODIFY!!!! *)
  let expecting_type_hint                   = 4032 (* DONT MODIFY!!!! *)
  let expecting_type_hint_suggest           = 4033 (* DONT MODIFY!!!! *)
  let extend_final                          = 4035 (* DONT MODIFY!!!! *)
  let field_kinds                           = 4036 (* DONT MODIFY!!!! *)
  (* DEPRECATED field_missing               = 4037 *)
  let format_string                         = 4038 (* DONT MODIFY!!!! *)
  let fun_arity_mismatch                    = 4039 (* DONT MODIFY!!!! *)
  let fun_too_few_args                      = 4040 (* DONT MODIFY!!!! *)
  let fun_too_many_args                     = 4041 (* DONT MODIFY!!!! *)
  let fun_unexpected_nonvariadic            = 4042 (* DONT MODIFY!!!! *)
  let fun_variadicity_hh_vs_php56           = 4043 (* DONT MODIFY!!!! *)
  let gena_expects_array                    = 4044 (* DONT MODIFY!!!! *)
  let generic_array_strict                  = 4045 (* DONT MODIFY!!!! *)
  let generic_static                        = 4046 (* DONT MODIFY!!!! *)
  let implement_abstract                    = 4047 (* DONT MODIFY!!!! *)
  let interface_final                       = 4048 (* DONT MODIFY!!!! *)
  let invalid_shape_field_const             = 4049 (* DONT MODIFY!!!! *)
  let invalid_shape_field_literal           = 4050 (* DONT MODIFY!!!! *)
  let invalid_shape_field_name              = 4051 (* DONT MODIFY!!!! *)
  let invalid_shape_field_type              = 4052 (* DONT MODIFY!!!! *)
  let member_not_found                      = 4053 (* DONT MODIFY!!!! *)
  let member_not_implemented                = 4054 (* DONT MODIFY!!!! *)
  let missing_assign                        = 4055 (* DONT MODIFY!!!! *)
  let missing_constructor                   = 4056 (* DONT MODIFY!!!! *)
  let missing_field                         = 4057 (* DONT MODIFY!!!! *)
  (* DEPRECATED negative_tuple_index        = 4058 *)
  let self_outside_class                    = 4059 (* DONT MODIFY!!!! *)
  let new_static_inconsistent               = 4060 (* DONT MODIFY!!!! *)
  let static_outside_class                  = 4061 (* DONT MODIFY!!!! *)
  let non_object_member                     = 4062 (* DONT MODIFY!!!! *)
  let null_container                        = 4063 (* DONT MODIFY!!!! *)
  let null_member                           = 4064 (* DONT MODIFY!!!! *)
  let nullable_parameter                    = 4065 (* DONT MODIFY!!!! *)
  let option_return_only_typehint           = 4066 (* DONT MODIFY!!!! *)
  let object_string                         = 4067 (* DONT MODIFY!!!! *)
  let option_mixed                          = 4068 (* DONT MODIFY!!!! *)
  let overflow                              = 4069 (* DONT MODIFY!!!! *)
  let override_final                        = 4070 (* DONT MODIFY!!!! *)
  let override_per_trait                    = 4071 (* DONT MODIFY!!!! *)
  let pair_arity                            = 4072 (* DONT MODIFY!!!! *)
  let abstract_call                         = 4073 (* DONT MODIFY!!!! *)
  let parent_in_trait                       = 4074 (* DONT MODIFY!!!! *)
  let parent_outside_class                  = 4075 (* DONT MODIFY!!!! *)
  let parent_undefined                      = 4076 (* DONT MODIFY!!!! *)
  let previous_default                      = 4077 (* DONT MODIFY!!!! *)
  let private_class_meth                    = 4078 (* DONT MODIFY!!!! *)
  let private_inst_meth                     = 4079 (* DONT MODIFY!!!! *)
  let private_override                      = 4080 (* DONT MODIFY!!!! *)
  let protected_class_meth                  = 4081 (* DONT MODIFY!!!! *)
  let protected_inst_meth                   = 4082 (* DONT MODIFY!!!! *)
  let read_before_write                     = 4083 (* DONT MODIFY!!!! *)
  let return_in_void                        = 4084 (* DONT MODIFY!!!! *)
  let shape_field_class_mismatch            = 4085 (* DONT MODIFY!!!! *)
  let shape_field_type_mismatch             = 4086 (* DONT MODIFY!!!! *)
  let should_be_override                    = 4087 (* DONT MODIFY!!!! *)
  let sketchy_null_check                    = 4088 (* DONT MODIFY!!!! *)
  let sketchy_null_check_primitive          = 4089 (* DONT MODIFY!!!! *)
  let smember_not_found                     = 4090 (* DONT MODIFY!!!! *)
  let static_dynamic                        = 4091 (* DONT MODIFY!!!! *)
  (* DEPRECATED let static_overflow         = 4092 *)
  let this_in_static                        = 4094 (* DONT MODIFY!!!! *)
  let this_var_outside_class                = 4095 (* DONT MODIFY!!!! *)
  let trait_final                           = 4096 (* DONT MODIFY!!!! *)
  let tuple_arity                           = 4097 (* DONT MODIFY!!!! *)
  let tuple_arity_mismatch                  = 4098 (* DONT MODIFY!!!! *)
  (* DEPRECATED tuple_index_too_large       = 4099 *)
  let tuple_syntax                          = 4100 (* DONT MODIFY!!!! *)
  let type_arity_mismatch                   = 4101 (* DONT MODIFY!!!! *)
  let type_param_arity                      = 4102 (* DONT MODIFY!!!! *)
  let typing_too_few_args                   = 4104 (* DONT MODIFY!!!! *)
  let typing_too_many_args                  = 4105 (* DONT MODIFY!!!! *)
  let unbound_global                        = 4106 (* DONT MODIFY!!!! *)
  let unbound_name_typing                   = 4107 (* DONT MODIFY!!!! *)
  let undefined_field                       = 4108 (* DONT MODIFY!!!! *)
  let undefined_parent                      = 4109 (* DONT MODIFY!!!! *)
  let unify_error                           = 4110 (* DONT MODIFY!!!! *)
  let unsatisfied_req                       = 4111 (* DONT MODIFY!!!! *)
  let visibility                            = 4112 (* DONT MODIFY!!!! *)
  let visibility_extends                    = 4113 (* DONT MODIFY!!!! *)
  (* DEPRECATED void_parameter              = 4114 *)
  let wrong_extend_kind                     = 4115 (* DONT MODIFY!!!! *)
  let generic_unify                         = 4116 (* DONT MODIFY!!!! *)
  let nullsafe_not_needed                   = 4117 (* DONT MODIFY!!!! *)
  let trivial_strict_eq                     = 4118 (* DONT MODIFY!!!! *)
  let void_usage                            = 4119 (* DONT MODIFY!!!! *)
  let declared_covariant                    = 4120 (* DONT MODIFY!!!! *)
  let declared_contravariant                = 4121 (* DONT MODIFY!!!! *)
  (* DEPRECATED unset_in_strict             = 4122 *)
  let strict_members_not_known              = 4123 (* DONT MODIFY!!!! *)
  let generic_at_runtime                    = 4124 (* DONT MODIFY!!!! *)
  let dynamic_class                         = 4125 (* DONT MODIFY!!!! *)
  let attribute_arity                       = 4126 (* DONT MODIFY!!!! *)
  let attribute_param_type                  = 4127 (* DONT MODIFY!!!! *)
  let deprecated_use                        = 4128 (* DONT MODIFY!!!! *)
  let abstract_const_usage                  = 4129 (* DONT MODIFY!!!! *)
  let cannot_declare_constant               = 4130 (* DONT MODIFY!!!! *)
  let cyclic_typeconst                      = 4131 (* DONT MODIFY!!!! *)
  let nullsafe_property_write_context       = 4132 (* DONT MODIFY!!!! *)
  let noreturn_usage                        = 4133 (* DONT MODIFY!!!! *)
  let this_lvalue                           = 4134 (* DONT MODIFY!!!! *)
  let unset_nonidx_in_strict                = 4135 (* DONT MODIFY!!!! *)
  let invalid_shape_field_name_empty        = 4136 (* DONT MODIFY!!!! *)
  let invalid_shape_field_name_number       = 4137 (* DONT MODIFY!!!! *)
  let shape_fields_unknown                  = 4138 (* DONT MODIFY!!!! *)
  let invalid_shape_remove_key              = 4139 (* DONT MODIFY!!!! *)
  let missing_optional_field                = 4140 (* DONT MODIFY!!!! *)
  let shape_field_unset                     = 4141 (* DONT MODIFY!!!! *)
  (* EXTEND HERE WITH NEW VALUES IF NEEDED *)
end

(*****************************************************************************)
(* Parsing errors. *)
(*****************************************************************************)

let fixme_format pos =
  add Parsing.fixme_format pos
    "HH_FIXME wrong format, expected '/* HH_FIXME[ERROR_NUMBER] */'"

let unexpected_eof pos =
  add Parsing.unexpected_eof pos "Unexpected end of file"

let unterminated_comment pos =
  add Parsing.unterminated_comment pos "unterminated comment"

let unterminated_xhp_comment pos =
  add Parsing.unterminated_xhp_comment pos "unterminated xhp comment"

let parsing_error (p, msg) =
  add Parsing.parsing_error p msg

(*****************************************************************************)
(* Naming errors *)
(*****************************************************************************)

let typeparam_alok (pos, x) =
  add Naming.typeparam_alok pos (
  "You probably forgot to bind this type parameter right?\nAdd <"^x^
  "> somewhere (after the function name definition, \
    or after the class name)\nExamples: "^"function foo<T> or class A<T>")

let generic_class_var pos =
  add Naming.generic_class_var pos
    "A class variable cannot be generic"

let unexpected_arrow pos cname =
  add Naming.unexpected_arrow pos (
  "Keys may not be specified for "^cname^" initialization"
 )

let missing_arrow pos cname =
  add Naming.missing_arrow pos (
  "Keys must be specified for "^cname^" initialization"
 )

let disallowed_xhp_type pos name =
  add Naming.disallowed_xhp_type pos (
  name^" is not a valid type. Use :xhp or XHPChild."
 )

let name_already_bound name pos1 pos2 =
  let name = Utils.strip_ns name in
  add_list Naming.name_already_bound [
    pos1, "Name already bound: "^name;
    pos2, "Previous definition is here"
]

let method_name_already_bound pos name =
  add Naming.method_name_already_bound pos (
  "Method name already bound: "^name
 )

let error_name_already_bound name name_prev p p_prev =
  let name = Utils.strip_ns name in
  let name_prev = Utils.strip_ns name_prev in
  let errs = [
    p, "Name already bound: "^name;
    p_prev, (if String.compare name name_prev == 0
      then "Previous definition is here"
      else "Previous definition "^name_prev^" differs only in capitalization ")
  ] in
  let hhi_msg =
    "This appears to be defined in an hhi file included in your project "^
    "root. The hhi files for the standard library are now a part of the "^
    "typechecker and must be removed from your project. Typically, you can "^
    "do this by deleting the \"hhi\" directory you copied into your "^
    "project when first starting with Hack." in
  let errs =
    if (Relative_path.prefix p.Pos.pos_file) = Relative_path.Hhi
    then errs @ [p_prev, hhi_msg]
    else if (Relative_path.prefix p_prev.Pos.pos_file) = Relative_path.Hhi
    then errs @ [p, hhi_msg]
    else errs in
  add_list Naming.error_name_already_bound errs

let unbound_name pos name kind =
  let kind_str = match kind with
    | `cls -> "an object type"
    | `func -> "a global function"
    | `const -> "a global constant"
  in
  add Naming.unbound_name pos
    ("Unbound name: "^(strip_ns name)^" ("^kind_str^")")

let different_scope pos var_name pos' =
  add_list Naming.different_scope [
  pos, ("The variable "^ var_name ^" is defined");
  pos', ("But in a different scope")
]

let undefined pos var_name =
  add Naming.undefined pos ("Undefined variable: "^var_name)

let this_reserved pos =
  add Naming.this_reserved pos
    "The type parameter \"this\" is reserved"

let start_with_T pos =
  add Naming.start_with_T pos
    "Please make your type parameter start with the letter T (capital)"

let already_bound pos name =
  add Naming.name_already_bound pos ("Argument already bound: "^name)

let unexpected_typedef pos def_pos =
  add_list Naming.unexpected_typedef [
  pos, "Unexpected typedef";
  def_pos, "Definition is here";
]

let fd_name_already_bound pos =
  add Naming.fd_name_already_bound pos
    "Field name already bound"

let primitive_toplevel pos =
  add Naming.primitive_toplevel pos (
  "Primitive type annotations are always available and may no \
    longer be referred to in the toplevel namespace."
)

let primitive_invalid_alias pos used valid =
  add Naming.primitive_invalid_alias pos
    ("Invalid Hack type. Using '"^used^"' in Hack is considered \
    an error. Use '"^valid^"' instead, to keep the codebase \
    consistent.")

let dynamic_new_in_strict_mode pos =
  add Naming.dynamic_new_in_strict_mode pos
  "Cannot use dynamic new in strict mode"

let invalid_type_access_root (pos, id) =
  add Naming.invalid_type_access_root pos
  (id^" must be an identifier for a class, \"self\", or \"this\"")

let duplicate_user_attribute (pos, name) existing_attr_pos =
  add_list Naming.duplicate_user_attribute [
    pos, "You cannot reuse the attribute "^name;
    existing_attr_pos, name^" was already used here";
  ]

let unbound_attribute_name pos name =
  let reason = if (str_starts_with name "__")
    then "starts with __ but is not a standard attribute"
    else "is not listed in .hhconfig"
  in add Naming.unbound_name pos
    ("Unrecognized user attribute: "^name^" "^reason)

let this_no_argument pos =
  add Naming.this_no_argument pos "\"this\" expects no arguments"

let void_cast pos =
  add Naming.void_cast pos "Cannot cast to void."

let unset_cast pos =
  add Naming.unset_cast pos "Don't use (unset), just assign null!"

let object_cast pos x =
  add Naming.object_cast pos ("Object casts are unsupported. "^
    "Try 'if ($var instanceof "^x^")' or "^
    "'invariant($var instanceof "^x^", ...)'.")

let this_hint_outside_class pos =
   add Naming.this_hint_outside_class pos
    "Cannot use \"this\" outside of a class"

let this_type_forbidden pos =
 add Naming.this_must_be_return pos
    "The type \"this\" cannot be used as a constraint on a classes generic, \
     or as the type of a static member variable"

let lowercase_this pos type_ =
  add Naming.lowercase_this pos (
  "Invalid Hack type \""^type_^"\". Use \"this\" instead"
 )

let classname_param pos =
  add Naming.classname_param pos
    ("Missing type parameter to classname; classname is entirely"
     ^" meaningless without one")

let tparam_with_tparam pos x =
  add Naming.tparam_with_tparam pos (
  Printf.sprintf "%s is a type parameter. Type parameters cannot \
    themselves take type parameters (e.g. %s<int> doesn't make sense)" x x
 )

let shadowed_type_param p pos name =
  add_list Naming.shadowed_type_param [
    p, Printf.sprintf "You cannot re-bind the type parameter %s" name;
    pos, Printf.sprintf "%s is already bound here" name
  ]

let missing_typehint pos =
  add Naming.missing_typehint pos
    "Please add a type hint"

let expected_variable pos =
  add Naming.expected_variable pos
    "Was expecting a variable name"

let naming_too_few_arguments pos =
  add Naming.naming_too_few_arguments pos
    "Too few arguments"

let naming_too_many_arguments pos =
  add Naming.naming_too_many_arguments pos
    "Too many arguments"

let expected_collection pos cn =
  add Naming.expected_collection pos (
  "Unexpected collection type " ^ (Utils.strip_ns cn)
 )

let illegal_CLASS pos =
  add Naming.illegal_CLASS pos
    "Using __CLASS__ outside a class or trait"

let illegal_TRAIT pos =
  add Naming.illegal_TRAIT pos
    "Using __TRAIT__ outside a trait"

let dynamic_method_call pos =
  add Naming.dynamic_method_call pos
    "Dynamic method call"

let nullsafe_property_write_context pos =
  add Typing.nullsafe_property_write_context pos
  "?-> syntax not supported here, this function effectively does a write"

let illegal_fun pos =
  let msg = "The argument to fun() must be a single-quoted, constant "^
    "literal string representing a valid function name." in
  add Naming.illegal_fun pos msg

let illegal_meth_fun pos =
  let msg = "String argument to fun() contains ':';"^
    " for static class methods, use"^
    " class_meth(Cls::class, 'method_name'), not fun('Cls::method_name')" in
  add Naming.illegal_meth_fun pos msg

let illegal_inst_meth pos =
  let msg = "The argument to inst_meth() must be an expression and a "^
    "constant literal string representing a valid method name." in
  add Naming.illegal_inst_meth pos msg

let illegal_meth_caller pos =
  let msg =
    "The two arguments to meth_caller() must be:"
    ^"\n - first: ClassOrInterface::class"
    ^"\n - second: a single-quoted string literal containing the name"
    ^" of a non-static method of that class" in
  add Naming.illegal_meth_caller pos msg

let illegal_class_meth pos =
  let msg =
    "The two arguments to class_meth() must be:"
    ^"\n - first: ValidClassname::class"
    ^"\n - second: a single-quoted string literal containing the name"
    ^" of a static method of that class" in
  add Naming.illegal_class_meth pos msg

let assert_arity pos =
  add Naming.assert_arity pos
    "assert expects exactly one argument"

let gena_arity pos =
  add Naming.gena_arity pos
    "gena() expects exactly 1 argument"

let genva_arity pos =
  add Naming.genva_arity pos
    "genva() expects at least 1 argument"

let gen_array_rec_arity pos =
  add Naming.gen_array_rec_arity pos
    "gen_array_rec() expects exactly 1 argument"

let dynamic_class pos =
  add Typing.dynamic_class pos
    "Don't use dynamic classes"

let uninstantiable_class usage_pos decl_pos name =
  let name = strip_ns name in
  add_list Typing.uninstantiable_class [
    usage_pos, (name^" is uninstantiable");
    decl_pos, "Declaration is here"
  ]

let abstract_const_usage usage_pos decl_pos name =
  let name = strip_ns name in
  add_list Typing.abstract_const_usage [
    usage_pos, ("Cannot reference abstract constant "^name^" directly");
    decl_pos, "Declaration is here"
  ]

let typedef_constraint pos =
  add Naming.typedef_constraint pos
    "Constraints on typedefs are not supported"

let add_a_typehint pos =
  add Naming.add_a_typehint pos
    "Please add a type hint"

let local_const var_pos =
  add Naming.local_const var_pos
    "You cannot use a local variable in a constant definition"

let illegal_constant pos =
  add Naming.illegal_constant pos
    "Illegal constant value"

let cyclic_constraint pos =
  add Naming.cyclic_constraint pos
    "Cyclic constraint"

let invalid_req_implements pos =
  add Naming.invalid_req_implements pos
    "Only traits may use 'require implements'"

let invalid_req_extends pos =
  add Naming.invalid_req_extends pos
    "Only traits and interfaces may use 'require extends'"

let did_you_mean_naming pos name suggest_pos suggest_name =
  add_list Naming.did_you_mean_naming [
  pos, "Could not find "^(strip_ns name);
  suggest_pos, "Did you mean "^(strip_ns suggest_name)^"?"
]

let using_internal_class pos name =
  add Naming.using_internal_class pos (
  name^" is an implementation internal class that cannot be used directly"
 )

(*****************************************************************************)
(* Init check errors *)
(*****************************************************************************)

let no_construct_parent pos =
  add NastCheck.no_construct_parent pos (
  sl["You are extending a class that needs to be initialized\n";
     "Make sure you call parent::__construct.\n"
   ]
 )

let constructor_required (pos, name) prop_names =
  let name = Utils.strip_ns name in
  let props_str = SSet.fold (fun x acc -> x^" "^acc) prop_names "" in
  add NastCheck.constructor_required pos
    ("Lacking __construct, class "^name^" does not initialize its private member(s): "^props_str)

let not_initialized (pos, cname) prop_names =
  let cname = Utils.strip_ns cname in
  let props_str = SSet.fold (fun x acc -> x^" "^acc) prop_names "" in
  let members, verb = if 1 == SSet.cardinal prop_names then "member", "is"
    else "members", "are" in
  let setters_str = SSet.fold (fun x acc -> "$this->"^x^" "^acc) prop_names "" in
  add NastCheck.not_initialized pos (
    sl[
      "Class "; cname ; " does not initialize all of its members; ";
      props_str; verb; " not always initialized.";
      "\nMake sure you systematically set "; setters_str;
      "when the method __construct is called.";
      "\nAlternatively, you can define the "; members ;" as optional (?...)\n"
    ])

let call_before_init pos cv =
  add NastCheck.call_before_init pos (
  sl([
     "Until the initialization of $this is over,";
     " you can only call private methods\n";
     "The initialization is not over because ";
   ] @
     if cv = "parent::__construct"
     then ["you forgot to call parent::__construct"]
     else ["$this->"; cv; " can still potentially be null"])
 )

(*****************************************************************************)
(* Nast errors check *)
(*****************************************************************************)

let type_arity pos name nargs =
  add Typing.type_arity_mismatch pos (
  sl["The type ";(Utils.strip_ns name);
     " expects ";nargs;" type parameter(s)"]
 )

let abstract_with_body (p, _) =
  add NastCheck.abstract_with_body p
    "This method is declared as abstract, but has a body"

let not_abstract_without_body (p, _) =
  add NastCheck.not_abstract_without_body p
    "This method is not declared as abstract, it must have a body"

let not_abstract_without_typeconst (p, _) =
  add NastCheck.not_abstract_without_typeconst p
    ("This type constant is not declared as abstract, it must have"^
     " an assigned type")

let abstract_with_typeconst (p, _) =
  add NastCheck.abstract_with_typeconst p
    ("This type constant is declared as abstract, it cannot be assigned a type")

let typeconst_depends_on_external_tparam pos ext_pos ext_name =
  add_list NastCheck.typeconst_depends_on_external_tparam [
    pos, ("A type constant can only use type parameters declared in its own"^
      " type parameter list");
    ext_pos, (ext_name ^ " was declared as a type parameter here");
  ]

let typeconst_assigned_tparam pos tp_name =
  add NastCheck.typeconst_assigned_tparam pos
    (tp_name ^" is a type parameter. It cannot be assigned to a type constant")

let return_in_gen p =
  add NastCheck.return_in_gen p
    ("You cannot return a value in a generator (a generator"^
     " is a function that uses yield)")

let return_in_finally p =
  add NastCheck.return_in_finally p
    ("Don't use return in a finally block;"^
     " there's nothing to receive the return value")

let toplevel_break p =
  add NastCheck.toplevel_break p
    "break can only be used inside loops or switch statements"

let toplevel_continue p =
  add NastCheck.toplevel_continue p
    "continue can only be used inside loops"

let continue_in_switch p =
  add NastCheck.continue_in_switch p
    ("In PHP, 'continue;' inside a switch \
       statement is equivalent to 'break;'."^
  " Hack does not support this; use 'break' if that is what you meant.")

let await_in_sync_function p =
  add NastCheck.await_in_sync_function p
    "await can only be used inside async functions"

let magic (p, s) =
  add NastCheck.magic p
    ("Don't call "^s^" it's one of these magic things we want to avoid")

let non_interface (p : Pos.t) (c2: string) (verb: string): 'a =
  add NastCheck.non_interface p
    ("Cannot " ^ verb ^ " " ^ (strip_ns c2) ^ " - it is not an interface")

let toString_returns_string pos =
  add NastCheck.toString_returns_string pos "__toString should return a string"

let toString_visibility pos =
  add NastCheck.toString_visibility pos
    "__toString must have public visibility and cannot be static"

let uses_non_trait (p: Pos.t) (n: string) (t: string) =
  add NastCheck.uses_non_trait p
    ((Utils.strip_ns n) ^ " is not a trait. It is " ^ t ^ ".")

let requires_non_class (p: Pos.t) (n: string) (t: string) =
  add NastCheck.requires_non_class p
    ((Utils.strip_ns n) ^ " is not a class. It is " ^ t ^ ".")

let abstract_body pos =
  add NastCheck.abstract_body pos "This method shouldn't have a body"

let not_public_interface pos =
  add NastCheck.not_public_interface pos
    "Access type for interface method must be public"

let interface_with_member_variable pos =
  add NastCheck.interface_with_member_variable pos
    "Interfaces cannot have member variables"

let interface_with_static_member_variable pos =
  add NastCheck.interface_with_static_member_variable pos
    "Interfaces cannot have static variables"

let illegal_function_name pos mname =
  add NastCheck.illegal_function_name pos
    ("Illegal function name: " ^ strip_ns mname)

let dangerous_method_name pos =
  add NastCheck.dangerous_method_name pos (
  "This is a dangerous method name, "^
  "if you want to define a constructor, use "^
  "__construct"
)

(*****************************************************************************)
(* Nast terminality *)
(*****************************************************************************)

let case_fallthrough pos1 pos2 =
  add_list NastCheck.case_fallthrough [
  pos1, ("This switch has a case that implicitly falls through and is "^
      "not annotated with // FALLTHROUGH");
  pos2, "This case implicitly falls through"
]

let default_fallthrough pos =
  add NastCheck.default_fallthrough pos
    ("This switch has a default case that implicitly falls "^
     "through and is not annotated with // FALLTHROUGH")

(*****************************************************************************)
(* Typing errors *)
(*****************************************************************************)

let visibility_extends vis pos parent_pos parent_vis =
  let msg1 = pos, "This member visibility is: " ^ vis in
  let msg2 = parent_pos, parent_vis ^ " was expected" in
  add_list Typing.visibility_extends [msg1; msg2]

let member_not_implemented member_name parent_pos pos defn_pos =
  let msg1 = pos, "This object doesn't implement the method "^member_name in
  let msg2 = parent_pos, "Which is required by this interface" in
  let msg3 = defn_pos, "As defined here" in
  add_list Typing.member_not_implemented [msg1; msg2; msg3]

let bad_decl_override parent_pos parent_name pos name (error: error) =
  let msg1 = pos, ("This object is of type "^(strip_ns name)) in
  let msg2 = parent_pos,
    ("It is incompatible with this object of type "^(strip_ns parent_name)^
     "\nbecause some declarations are incompatible."^
     "\nRead the following to see why:"
    ) in
  (* This is a cascading error message *)
  let code, msgl = error in
  add_list code (msg1 :: msg2 :: msgl)

let missing_constructor pos =
  add Typing.missing_constructor pos
    "The constructor is not implemented"

let typedef_trail_entry pos =
  pos, "Typedef definition comes from here"

let add_with_trail code errs trail =
  add_list code (errs @ List.map typedef_trail_entry trail)

let enum_constant_type_bad pos ty_pos ty trail =
  add_with_trail Typing.enum_constant_type_bad
    [pos, "Enum constants must be an int or string";
     ty_pos, "Not " ^ ty]
    trail

let enum_type_bad pos ty trail =
  add_with_trail Typing.enum_type_bad
    [pos, "Enums must be int or string, not " ^ ty]
    trail

let enum_type_typedef_mixed pos =
  add Typing.enum_type_typedef_mixed pos
    "Can't use typedef that resolves to mixed in enum"

let enum_switch_redundant const first_pos second_pos =
  add_list Typing.enum_switch_redundant [
    second_pos, "Redundant case statement";
    first_pos, const ^ " already handled here"
  ]

let enum_switch_nonexhaustive pos missing enum_pos =
  add_list Typing.enum_switch_nonexhaustive [
    pos, "Switch statement nonexhaustive; the following cases are missing: " ^
            String.concat ", " missing;
    enum_pos, "Enum declared here"
  ]

let enum_switch_redundant_default pos enum_pos =
  add_list Typing.enum_switch_redundant_default [
    pos, "All cases already covered; a redundant default case prevents "^
         "detecting future errors";
    enum_pos, "Enum declared here"
  ]

let enum_switch_not_const pos =
  add Typing.enum_switch_not_const pos
    "Case in switch on enum is not an enum constant"

let enum_switch_wrong_class pos expected got =
  add Typing.enum_switch_wrong_class pos
    ("Switching on enum " ^ expected ^ " but using constant from " ^ got)

let invalid_shape_field_name p =
  add Typing.invalid_shape_field_name p
    "Was expecting a constant string or class constant (for shape access)"

let invalid_shape_field_name_empty p =
  add Typing.invalid_shape_field_name_empty p
    "A shape field name cannot be an empty string"

let invalid_shape_field_name_number p =
  add Typing.invalid_shape_field_name_number p
    "A shape field name cannot start with numbers"

let invalid_shape_field_type pos ty_pos ty trail =
  add_with_trail Typing.invalid_shape_field_type
    [pos, "A shape field name must be an int or string";
     ty_pos, "Not " ^ ty]
    trail

let invalid_shape_field_literal key_pos witness_pos =
  add_list Typing.invalid_shape_field_literal
    [key_pos, "Shape uses literal string as field name";
     witness_pos, "But expected a class constant"]

let invalid_shape_field_const key_pos witness_pos =
  add_list Typing.invalid_shape_field_const
    [key_pos, "Shape uses class constant as field name";
     witness_pos, "But expected a literal string"]

let shape_field_class_mismatch key_pos witness_pos key_class witness_class =
  add_list Typing.shape_field_class_mismatch
    [key_pos, "Shape field name is class constant from " ^ key_class;
     witness_pos, "But expected constant from " ^ witness_class]

let shape_field_type_mismatch key_pos witness_pos key_ty witness_ty =
  add_list Typing.shape_field_type_mismatch
    [key_pos, "Shape field name is " ^ key_ty ^ " class constant";
     witness_pos, "But expected " ^ witness_ty]

let missing_field pos1 pos2 name =
  add_list Typing.missing_field (
    (pos1, "The field '"^name^"' is missing")::
    [pos2, "The field '"^name^"' is defined"])

let missing_optional_field pos1 pos2 name =
  add_list Typing.missing_optional_field
    (* We have the position of shape type that is marked as optional -
     * explain why we can't omit it despite this.*)
    (if pos2 <> Pos.none then (
      (pos1, "The field '"^name^"' may be set to an unknown type. " ^
              "Explicitly null out the field, or remove it " ^
              "(with Shapes::removeKey(...))")::
      [pos2, "The field '"^name^"' is defined as optional"])
   else
      [pos1, "The field '"^name^"' is missing"])

let shape_fields_unknown pos1 pos2 =
  add_list Typing.shape_fields_unknown
    [pos1, "This is a shape type coming from a type annotation. Because of " ^
            "structural subtyping it might have some other fields besides " ^
            "those listed in its declaration.";
     pos2, "It is incompatible with a shape created using \"shape\" "^
           "constructor, which has all the fields known"]

let shape_field_unset pos1 pos2 name =
  add_list Typing.shape_field_unset (
    [(pos1, "The field '"^name^"' was unset here");
     (pos2, "The field '"^name^"' might be present in this shape because of " ^
            "structural subtyping")]
  )

let invalid_shape_remove_key p =
  add Typing.invalid_shape_remove_key p
    "You can only unset fields of local variables"

let explain_constraint p_inst pos name (error : error) =
  let inst_msg = "Some type constraint(s) here are violated" in
  let code, msgl = error in
  (* There may be multiple constraints instantiated at one spot; avoid
   * duplicating the instantiation message *)
  let msgl = match msgl with
    | (p, x) :: rest when x = inst_msg && p = p_inst -> rest
    | _ -> msgl in
  let name = Utils.strip_ns name in
  add_list code begin
    [p_inst, inst_msg;
     pos, "'"^name^"' is a constrained type"] @ msgl
  end

let explain_type_constant reason_msgl (error: error) =
  let code, msgl = error in
  add_list code (msgl @ reason_msgl)

let overflow p =
  add Typing.overflow p "Value is too large"

let format_string pos snippet s class_pos fname class_suggest =
  add_list Typing.format_string [
  (pos, "I don't understand the format string " ^ snippet ^ " in " ^ s);
  (class_pos,
   "You can add a new format specifier by adding "
   ^fname^"() to "^class_suggest)]

let expected_literal_string pos =
  add Typing.expected_literal_string pos
    "This argument must be a literal string"

let generic_array_strict p =
  add Typing.generic_array_strict p
    "You cannot have an array without generics in strict mode"

let strict_members_not_known p name =
  let name = Utils.strip_ns name in
  add Typing.strict_members_not_known p
    (name^" has a non-<?hh grandparent; this is not allowed in strict mode"
     ^" because that parent may define methods of unknowable name and type")

let option_return_only_typehint p kind =
  let (typehint, reason) = match kind with
    | `void -> ("?void", "only return implicitly")
    | `noreturn -> ("?noreturn", "never return")
  in
  add Typing.option_return_only_typehint p
    (typehint^" is a nonsensical typehint; a function cannot both "^reason
     ^" and return null.")

let tuple_syntax p =
  add Typing.tuple_syntax p
    ("Did you want a tuple? Try (X,Y), not tuple<X,Y>")

let class_arity usage_pos class_pos class_name arity =
  add_list Typing.class_arity
    [usage_pos, ("The class "^(Utils.strip_ns class_name)^" expects "^
                    soi arity^" arguments");
     class_pos, "Definition is here"]

let expecting_type_hint p =
  add Typing.expecting_type_hint p "Was expecting a type hint"

let expecting_type_hint_suggest p ty =
  add Typing.expecting_type_hint_suggest p
    ("Was expecting a type hint (what about: "^ty^")")

let expecting_return_type_hint p =
  add Typing.expecting_return_type_hint p
    "Was expecting a return type hint"

let expecting_return_type_hint_suggest p ty =
  add Typing.expecting_return_type_hint_suggest p
    ("Was expecting a return type hint (what about: ': "^ty^"')")

let field_kinds pos1 pos2 =
  add_list Typing.field_kinds
    [pos1, "You cannot use this kind of field (value)";
     pos2, "Mixed with this kind of field (key => value)"]

let unbound_name_typing pos name =
  add Typing.unbound_name_typing pos
    ("Unbound name (typing): "^(strip_ns name))

let previous_default p =
  add Typing.previous_default p
    ("A previous parameter has a default value.\n"^
     "Remove all the default values for the preceding parameters,\n"^
     "or add a default value to this one.")

let return_only_typehint p kind =
  let msg = match kind with
    | `void -> "void"
    | `noreturn -> "noreturn" in
  add Naming.return_only_typehint p
    ("The "^msg^" typehint can only be used to describe a function return type")

let unexpected_type_arguments p =
  add Naming.unexpected_type_arguments p
    ("Type arguments are not expected for this type")

let too_many_type_arguments p =
  add Naming.too_many_type_arguments p
    ("Too many type arguments for this type")

let nullable_parameter pos =
  add Typing.nullable_parameter pos
    "Please add a ?, this argument can be null"

let return_in_void pos1 pos2 =
  add_list Typing.return_in_void [
  pos1,
  "You cannot return a value";
  pos2,
  "This is a void function"
]

let this_in_static p =
  add Typing.this_in_static p "Don't use $this in a static method"

let this_var_outside_class p =
  add Typing.this_var_outside_class p "Can't use $this outside of a class"

let unbound_global cst_pos =
  add Typing.unbound_global cst_pos "Unbound global constant (Typing)"

let private_inst_meth method_pos p =
  add_list Typing.private_inst_meth [
  method_pos, "This is a private method";
  p, "you cannot use it with inst_meth \
    (whether you are in the same class or not)."
]

let protected_inst_meth method_pos p =
  add_list Typing.protected_inst_meth [
  method_pos, "This is a protected method";
  p, "you cannot use it with inst_meth \
    (whether you are in the same class hierarchy or not)."
]

let private_class_meth pos1 pos2 =
  add_list Typing.private_class_meth [
  pos1, "This is a private method";
  pos2, "you cannot use it with class_meth \
    (whether you are in the same class or not)."
]

let protected_class_meth pos1 pos2 =
  add_list Typing.protected_class_meth [
  pos1, "This is a protected method";
  pos2, "you cannot use it with class_meth \
    (whether you are in the same class hierarchy or not)."
]

let array_cast pos =
  add Typing.array_cast pos
    "(array) cast forbidden in strict mode; arrays with unspecified \
    key and value types are not allowed"

let anonymous_recursive pos =
  add Typing.anonymous_recursive pos
    "Anonymous functions cannot be recursive"

let static_outside_class pos =
  add Typing.static_outside_class pos
    "'static' is undefined outside of a class"

let self_outside_class pos =
  add Typing.self_outside_class pos
    "'self' is undefined outside of a class"

let new_inconsistent_construct new_pos (cpos, cname) kind =
  let name = Utils.strip_ns cname in
  let preamble = match kind with
    | `static -> "Can't use new static() for "^name
    | `classname -> "Can't use new on classname<"^name^">"
  in
  add_list Typing.new_static_inconsistent [
    new_pos, preamble^"; __construct arguments are not \
    guaranteed to be consistent in child classes";
    cpos, ("This declaration neither defines an abstract/final __construct"
           ^" nor uses <<__ConsistentConstruct>> attribute")]

let pair_arity pos =
  add Typing.pair_arity pos "A pair has exactly 2 elements"

let tuple_arity pos2 size2 pos1 size1 =
  add_list Typing.tuple_arity [
  pos2, "This tuple has "^ string_of_int size2^" elements";
  pos1, string_of_int size1 ^ " were expected"]

let undefined_parent pos =
  add Typing.undefined_parent pos
    "The parent class is undefined"

let parent_outside_class pos =
  add Typing.parent_outside_class pos
    "'parent' is undefined outside of a class"

let parent_abstract_call meth_name call_pos decl_pos =
  add_list Typing.abstract_call [
    call_pos, ("Cannot call parent::"^meth_name^"(); it is abstract");
    decl_pos, "Declaration is here"
  ]

let self_abstract_call meth_name call_pos decl_pos =
  add_list Typing.abstract_call [
    call_pos, ("Cannot call self::"^meth_name^"(); it is abstract. Did you mean static::"^meth_name^"()?");
    decl_pos, "Declaration is here"
  ]

let classname_abstract_call cname meth_name call_pos decl_pos =
  let cname = Utils.strip_ns cname in
  add_list Typing.abstract_call [
    call_pos, ("Cannot call "^cname^"::"^meth_name^"(); it is abstract");
    decl_pos, "Declaration is here"
  ]

let isset_empty_in_strict pos name =
  let name = Utils.strip_ns name in
  add Typing.isset_empty_in_strict pos
    (name^" cannot be used in a completely type safe way and so is banned in "
     ^"strict mode")

let unset_nonidx_in_strict pos msgs =
  add_list Typing.unset_nonidx_in_strict
    ([pos, "In strict mode, unset is banned except on array indexing"] @
     msgs)

let array_get_arity pos1 name pos2 =
  add_list Typing.array_get_arity [
  pos1, "You cannot use this "^(Utils.strip_ns name);
  pos2, "It is missing its type parameters"
]

let typing_error pos msg =
  add Typing.generic_unify pos msg

let typing_error_l err =
  add_error err

let undefined_field p name =
  add Typing.undefined_field p ("The field "^name^" is undefined")

let array_access pos1 pos2 ty =
  add_list Typing.array_access ((pos1,
    "This is not an object of type KeyedContainer, this is "^ty) ::
            if pos2 != Pos.none
            then [pos2, "You might want to check this out"]
            else [])

let array_append pos1 pos2 ty =
  add_list Typing.array_append
    ((pos1, ty^" does not allow array append") ::
     if pos2 != Pos.none
     then [pos2, "You might want to check this out"]
     else [])

let const_mutation pos1 pos2 ty =
  add_list Typing.const_mutation
    ((pos1, "You cannot mutate this") ::
     if pos2 != Pos.none
     then [(pos2, "This is " ^ ty)]
     else [])

let expected_class pos =
  add Typing.expected_class pos
    "Was expecting a class"

let snot_found_hint = function
  | `no_hint ->
      []
  | `closest (pos, v) ->
      [pos, "The closest thing is "^v^" but it's not a static method"]
  | `did_you_mean (pos, v) ->
      [pos, "Did you mean: "^v]

let string_of_class_member_kind = function
  | `class_constant -> "class constant"
  | `static_method  -> "static method"
  | `class_variable -> "class variable"
  | `class_typeconst -> "type constant"

let smember_not_found kind pos (cpos, class_name) member_name hint =
  let kind = string_of_class_member_kind kind in
  let class_name = strip_ns class_name in
  let msg = "Could not find "^kind^" "^member_name^" in type "^class_name in
  add_list Typing.smember_not_found
    ((pos, msg) :: (snot_found_hint hint
                    @ [(cpos, "Declaration of "^class_name^" is here")]))

let not_found_hint = function
  | `no_hint ->
      []
  | `closest (pos, v) ->
      [pos, "The closest thing is "^v^" but it's a static method"]
  | `did_you_mean (pos, v) ->
      [pos, "Did you mean: "^v]

let member_not_found kind pos (cpos, type_name) member_name hint reason =
  let type_name = strip_ns type_name in
  let kind =
    match kind with
    | `method_ -> "method"
    | `member -> "member"
  in
  let msg = "Could not find "^kind^" "^member_name^" in an object of type "^
    type_name in
  add_list Typing.member_not_found
    ((pos, msg) :: (not_found_hint hint @ reason
                    @ [(cpos, "Declaration of "^type_name^" is here")]))

let parent_in_trait pos =
  add Typing.parent_in_trait pos
    ("parent:: inside a trait is undefined"
     ^" without 'require extends' of a class defined in <?hh")

let parent_undefined pos =
  add Typing.parent_undefined pos
    "parent is undefined"

let constructor_no_args pos =
  add Typing.constructor_no_args pos
    "This constructor expects no argument"

let visibility p msg1 p_vis msg2 =
  add_list Typing.visibility [p, msg1; p_vis, msg2]

let typing_too_many_args pos pos_def =
  add_list Typing.typing_too_many_args
    [pos, "Too many arguments"; pos_def, "Definition is here"]

let typing_too_few_args pos pos_def =
  add_list Typing.typing_too_few_args
    [pos, "Too few arguments"; pos_def, "Definition is here"]

let anonymous_recursive_call pos =
  add Typing.anonymous_recursive_call pos
    "recursive call to anonymous function"

let bad_call pos ty =
  add Typing.bad_call pos
    ("This call is invalid, this is not a function, it is "^ty)

let sketchy_null_check pos =
  add Typing.sketchy_null_check pos (
  "You are using a sketchy null check ...\n"^
  "Use is_null, or $x === null instead"
)

let sketchy_null_check_primitive pos =
  add Typing.sketchy_null_check_primitive pos (
  "You are using a sketchy null check on a primitive type ...\n"^
  "Use is_null, or $x === null instead"
 )

let extend_final extend_pos decl_pos name =
  let name = (strip_ns name) in
  add_list Typing.extend_final [
    extend_pos, ("You cannot extend final class "^name);
    decl_pos, "Declaration is here"
  ]

let read_before_write (pos, v) =
  add Typing.read_before_write pos (
  sl[
  "Read access to $this->"; v; " before initialization"
])

let interface_final pos =
  add Typing.interface_final pos
    "Interfaces cannot be final"

let trait_final pos =
  add Typing.trait_final pos
    "Traits cannot be final"

let implement_abstract pos1 pos2 kind x =
  let name = "abstract "^kind^" '"^x^"'" in
  add_list Typing.implement_abstract [
    pos1, "This class must be declared abstract, or provide an implementation for the "^name;
    pos2, "Declaration is here";
  ]

let generic_static pos x =
  add Typing.generic_static pos (
  "This static variable cannot use the type parameter "^x^"."
 )

let fun_too_many_args pos1 pos2 =
  add_list Typing.fun_too_many_args [
  pos1, "Too many mandatory arguments";
  pos2, "Because of this definition";
]

let fun_too_few_args pos1 pos2 =
  add_list Typing.fun_too_few_args [
  pos1, "Too few arguments";
  pos2, "Because of this definition";
]

let fun_unexpected_nonvariadic pos1 pos2 =
  add_list Typing.fun_unexpected_nonvariadic [
  pos1, "Should have a variadic argument";
  pos2, "Because of this definition";
]

let fun_variadicity_hh_vs_php56 pos1 pos2 =
  add_list Typing.fun_variadicity_hh_vs_php56 [
  pos1, "Variadic arguments: ...-style is not a subtype of ...$args";
  pos2, "Because of this definition";
]

let expected_tparam pos n =
  add Typing.expected_tparam pos (
  "Expected " ^
  (match n with
  | 0 -> "no type parameter"
  | 1 -> "a type parameter"
  | n -> string_of_int n ^ " type parameters"
  )
 )

let object_string pos1 pos2 =
  add_list Typing.object_string [
  pos1, "You cannot use this object as a string";
  pos2, "This object doesn't implement __toString";
]

let type_param_arity pos x n =
  add Typing.type_param_arity pos (
  "The type "^x^" expects "^n^" parameters"
 )

let cyclic_typedef p =
  add Typing.cyclic_typedef p
    "Cyclic typedef"

let type_arity_mismatch pos1 n1 pos2 n2 =
  add_list Typing.type_arity_mismatch [
  pos1, "This type has "^n1^" arguments";
  pos2, "This one has "^n2;
]

let this_final id pos2 (error: error) =
  let n = Utils.strip_ns (snd id) in
  let message1 = "Since "^n^" is not final" in
  let message2 = "this might not be a "^n in
  let code, msgl = error in
  add_list code (msgl @ [(fst id, message1); (pos2, message2)])

let exact_class_final id pos2 (error: error) =
  let n = Utils.strip_ns (snd id) in
  let message1 = "This requires the late-bound type to be exactly "^n in
  let message2 =
    "Since " ^n^" is not final this might be an instance of a child class" in
  let code, msgl = error in
  add_list code (msgl @ [(fst id, message1); (pos2, message2)])

let tuple_arity_mismatch pos1 n1 pos2 n2 =
  add_list Typing.tuple_arity_mismatch [
  pos1, "This tuple has "^n1^" elements";
  pos2, "This one has "^n2^" elements"
]

let fun_arity_mismatch pos1 pos2 =
  add_list Typing.fun_arity_mismatch [
  pos1, "Number of arguments doesn't match";
  pos2, "Because of this definition";
]

let discarded_awaitable pos1 pos2 =
  add_list Typing.discarded_awaitable [
  pos1, "This expression is of type Awaitable, but it's "^
  "either being discarded or used in a dangerous way before "^
  "being awaited";
  pos2, "This is why I think it is Awaitable"
]

let gena_expects_array pos1 pos2 ty_str =
  add_list Typing.gena_expects_array [
  pos1, "gena expects an array";
  pos2, "It is incompatible with " ^ ty_str;
]

let unify_error left right =
  add_list Typing.unify_error (left @ right)

let static_dynamic static_position dyn_position method_name =
  let msg_static = "The function "^method_name^" is static" in
  let msg_dynamic = "It is defined as dynamic here" in
  add_list Typing.static_dynamic [
  static_position, msg_static;
  dyn_position, msg_dynamic
]

let null_member s pos r =
  add_list Typing.null_member ([
  pos,
  "You are trying to access the member "^s^
  " but this object can be null. "
] @ r
)

let non_object_member s pos1 ty pos2 =
  add_list Typing.non_object_member [
  pos1,
  ("You are trying to access the member "^s^
   " but this is not an object, it is "^
   ty);
  pos2,
  "Check this out"
]

let null_container p null_witness =
  add_list Typing.null_container (
  [
   p,
   "You are trying to access an element of this container"^
   " but the container could be null. "
 ] @ null_witness)

let option_mixed pos =
  add Typing.option_mixed pos
    "?mixed is a redundant typehint - just use mixed"

let declared_covariant pos1 pos2 emsg =
  add_list Typing.declared_covariant (
  [pos2, "Illegal usage of a covariant type parameter";
   pos1, "This is where the parameter was declared as covariant (+)"
 ] @ emsg
 )

let declared_contravariant pos1 pos2 emsg =
  add_list Typing.declared_contravariant (
  [pos2, "Illegal usage of a contravariant type parameter";
   pos1, "This is where the parameter was declared as contravariant (-)"
 ] @ emsg
 )

let cyclic_typeconst pos sl =
  let sl = List.map strip_ns sl in
  add Typing.cyclic_typeconst pos
    ("Cyclic type constant:\n  "^String.concat " -> " sl)

let this_lvalue pos =
  add Typing.this_lvalue pos "Cannot assign a value to $this"

(*****************************************************************************)
(* Typing decl errors *)
(*****************************************************************************)

let wrong_extend_kind child_pos child parent_pos parent =
  let msg1 = child_pos, child^" cannot extend "^parent in
  let msg2 = parent_pos, "This is "^parent in
  add_list Typing.wrong_extend_kind [msg1; msg2]

let unsatisfied_req parent_pos req_name req_pos =
  let s1 = "Failure to satisfy requirement: "^(Utils.strip_ns req_name) in
  let s2 = "Required here" in
  if req_pos = parent_pos
  then add Typing.unsatisfied_req parent_pos s1
  else add_list Typing.unsatisfied_req [parent_pos, s1; req_pos, s2]

let cyclic_class_def stack pos =
  let stack = SSet.fold (fun x y -> (Utils.strip_ns x)^" "^y) stack "" in
  add Typing.cyclic_class_def pos ("Cyclic class definition : "^stack)

let override_final ~parent ~child =
  add_list Typing.override_final [child, "You cannot override this method";
            parent, "It was declared as final"]

let should_be_override pos class_id id =
  add Typing.should_be_override pos
    ((Utils.strip_ns class_id)^"::"^id^"() is marked as override; \
       no non-private parent definition found \
       or overridden parent is defined in non-<?hh code")

let override_per_trait class_name id m_pos =
    let c_pos, c_name = class_name in
    let err_msg =
      ("Method "^(Utils.strip_ns c_name)^"::"^id^" is should be an override \
        per the declaring trait; no non-private parent definition found \
        or overridden parent is defined in non-<?hh code")
    in add_list Typing.override_per_trait [
      c_pos, err_msg;
      m_pos, "Declaration of "^id^"() is here"
    ]

let missing_assign pos =
  add Typing.missing_assign pos "Please assign a value"

let private_override pos class_id id =
  add Typing.private_override pos ((Utils.strip_ns class_id)^"::"^id
          ^": combining private and override is nonsensical")

let nullsafe_not_needed p nonnull_witness =
  add_list Typing.nullsafe_not_needed (
  [
   p,
   "You are using the ?-> operator but this object cannot be null. "
 ] @ nonnull_witness)

let generic_at_runtime p =
  add Typing.generic_at_runtime p
    "Generics can only be used in type hints since they are erased at runtime."

let trivial_strict_eq p b left right left_trail right_trail =
  let msg = "This expression is always "^b in
  let left_trail = List.map typedef_trail_entry left_trail in
  let right_trail = List.map typedef_trail_entry right_trail in
  add_list Typing.trivial_strict_eq
    ((p, msg) :: left @ left_trail @ right @ right_trail)

let void_usage p void_witness =
  let msg = "You are using the return value of a void function" in
  add_list Typing.void_usage ((p, msg) :: void_witness)

let noreturn_usage p noreturn_witness =
  let msg = "You are using the return value of a noreturn function" in
  add_list Typing.noreturn_usage ((p, msg) :: noreturn_witness)

let attribute_arity pos x n =
  let n = string_of_int n in
  add Typing.attribute_arity pos (
    "The attribute "^x^" expects "^n^" parameters"
  )

let attribute_param_type pos x =
  add Typing.attribute_param_type pos (
    "This attribute parameter should be "^x
  )

let deprecated_use pos pos_def msg =
  add_list Typing.deprecated_use [
    pos, msg;
    pos_def, "Definition is here";
  ]

let cannot_declare_constant kind pos (class_pos, class_name) =
  let kind_str =
    match kind with
    | `enum -> "an enum"
    | `trait -> "a trait"
  in
  add_list Typing.cannot_declare_constant [
    pos, "Cannot declare a constant in "^kind_str;
    class_pos, (strip_ns class_name)^" was defined as "^kind_str^" here";
  ]

let ambiguous_inheritance pos class_ origin (error: error) =
  let origin = strip_ns origin in
  let class_ = strip_ns class_ in
  let message = "This declaration was inherited from an object of type "^origin^
    ". Redeclare this member in "^class_^" with a compatible signature." in
  let code, msgl = error in
  add_list code (msgl @ [pos, message])

let explain_contravariance pos c_name error =
  let message = "Considering that this type argument is contravariant "^
                "with respect to " ^ strip_ns c_name in
  let code, msgl = error in
  add_list code (msgl @ [pos, message])

(*****************************************************************************)
(* Convert relative paths to absolute. *)
(*****************************************************************************)

let to_absolute (code, msg_l) =
  let msg_l = List.map (fun (p, s) -> Pos.to_absolute p, s) msg_l in
  code, msg_l

(*****************************************************************************)
(* Printing *)
(*****************************************************************************)

let to_json ((error_code, msgl) : Pos.absolute error_) = Hh_json.(
  let elts = List.map (fun (p, w) ->
                        let line, scol, ecol = Pos.info_pos p in
                        JAssoc [ "descr", JString w;
                                 "path",  JString p.Pos.pos_file;
                                 "line",  JInt line;
                                 "start", JInt scol;
                                 "end",   JInt ecol;
                                 "code",  JInt error_code
                               ]
                      ) msgl
  in
  JAssoc [ "message", JList elts ]
)

let to_string ((error_code, msgl) : Pos.absolute error_) : string =
  let buf = Buffer.create 50 in
  (match msgl with
  | [] -> assert false
  | (pos1, msg1) :: rest_of_error ->
      Buffer.add_string buf begin
        let error_code = error_code_to_string error_code in
        Printf.sprintf "%s\n%s (%s)\n"
          (Pos.string pos1) msg1 error_code
      end;
      List.iter begin fun (p, w) ->
        let msg = Printf.sprintf "%s\n%s\n" (Pos.string p) w in
        Buffer.add_string buf msg
      end rest_of_error
  );
  Buffer.contents buf

(*****************************************************************************)
(* Try if errors. *)
(*****************************************************************************)

let try_with_result f1 f2 =
  let error_list_copy = !error_list in
  let accumulate_errors_copy = !accumulate_errors in
  error_list := [];
  accumulate_errors := true;
  let result = f1 () in
  let errors = !error_list in
  error_list := error_list_copy;
  accumulate_errors := accumulate_errors_copy;
  match List.rev errors with
  | [] -> result
  | l :: _ -> f2 result l

let try_ f1 f2 =
  try_with_result f1 (fun _ l -> f2 l)

let try_with_error f1 f2 =
  try_ f1 (fun err -> add_error err; f2())

let try_add_err pos err f1 f2 =
  try_ f1 begin fun (error_code, l) ->
    add_list error_code ((pos, err) :: l);
    f2()
  end

let has_no_errors f =
  try_ (fun () -> f(); true) (fun _ -> false)

(*****************************************************************************)
(* Do. *)
(*****************************************************************************)

let do_ f =
  let error_list_copy = !error_list in
  let accumulate_errors_copy = !accumulate_errors in
  error_list := [];
  accumulate_errors := true;
  let result = f () in
  let out_errors = !error_list in
  error_list := error_list_copy;
  accumulate_errors := accumulate_errors_copy;
  List.rev out_errors, result

let ignore_ f =
  snd (do_ f)

let try_when f ~when_ ~do_ =
  try_with_result f begin fun result (error: error) ->
    if when_()
    then do_ error
    else add_error error;
    result
  end

(* Runs the first function that is expected to produce an error. If it doesn't
 * then we run the second function we are given
 *)
let must_error f error_fun =
  let had_no_errors = try_with_error (fun () -> f(); true) (fun _ -> false) in
  if had_no_errors then error_fun();
