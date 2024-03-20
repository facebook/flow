(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type bracket_syntax = {
  include_super: bool;
  include_this: bool;
  type_: Type.t;
}

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment of {
      text: string;
      loc: ALoc.t;
    }  (** inside a comment *)
  | Ac_id of {
      include_super: bool;
      include_this: bool;
      type_: Type.t;
      enclosing_class_t: Type.t option;
    }  (** identifier references *)
  | Ac_class_key of { enclosing_class_t: Type.t option }  (** class method name or property name *)
  | Ac_enum  (** identifier in enum declaration *)
  | Ac_import_specifier of {
      module_type: Type.t;
      used_keys: SSet.t;
      is_type: bool;
    }  (** Import named specifiers *)
  | Ac_key of {
      obj_type: Type.t;
      used_keys: SSet.t;
      spreads: (Loc.t * Type.t) list;
    }  (** object key *)
  | Ac_literal of { lit_type: Type.t option }  (** inside a literal like a string or regex *)
  | Ac_module  (** a module name *)
  | Ac_type of { allow_react_element_shorthand: bool }  (** type identifiers *)
  | Ac_type_binding
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
      bracket_syntax: bracket_syntax option;
      (* loc of `.foo` or `[foo]` *)
      member_loc: Loc.t option;
      is_type_annotation: bool;
      is_super: bool;
    }  (** member expressions *)
  | Ac_jsx_element of { type_: Type.t }  (** JSX element name *)
  | Ac_jsx_attribute of {
      attribute_name: string;
      used_attr_names: SSet.t;
      component_t: Type.t;
      has_value: bool;
    }  (** JSX attributes *)
  | Ac_jsx_text  (** JSX text child *)

type process_location_result = {
  tparams_rev: string list;
  ac_loc: ALoc.t;
  token: string;
  autocomplete_type: autocomplete_type;
}

val process_location :
  Context.t ->
  trigger_character:string option ->
  cursor:Loc.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast_opt:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t option ->
  (process_location_result option, string) result

val autocomplete_set_hooks : cursor:Loc.t -> unit

val autocomplete_unset_hooks : unit -> unit

(* We don't really need to export this type, but it is a convenient way to enforce
 * that this class is indeed polymorphic over 'T (instead of being pinned to
 * `ALoc.t * Type.t`). *)
class virtual ['T] process_request_searcher :
  from_trigger_character:bool
  -> cursor:Loc.t
  -> object
       inherit [ALoc.t, 'T, ALoc.t, 'T, string] Typed_ast_finder.type_parameter_mapper_generic

       val mutable enclosing_node_stack : (ALoc.t, 'T) Typed_ast_finder.enclosing_node list

       method virtual private type_from_enclosing_node : 'T -> Type.t

       method virtual private type_of_expression : (ALoc.t, 'T) Flow_ast.Expression.t -> Type.t

       method virtual private infer_expression :
         (ALoc.t, 'T) Flow_ast.Expression.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

       method virtual private infer_statement :
         (ALoc.t, 'T) Flow_ast.Statement.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t

       method virtual private check_closest_enclosing_statement : unit

       method virtual private type_of_component_name_of_jsx_element :
         'T -> (ALoc.t, 'T) Flow_ast.JSX.element -> Type.t

       method virtual private type_of_class_id : ALoc.t -> (ALoc.t, 'T) Flow_ast.Class.t -> Type.t

       method virtual loc_of_annot : 'T -> ALoc.t
     end
