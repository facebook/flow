(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ac_id = {
  include_super: bool;
  include_this: bool;
  type_: Type.t;
}

type autocomplete_type =
  | Ac_ignored  (** ignore extraneous requests the IDE sends *)
  | Ac_binding  (** binding identifiers introduce new names *)
  | Ac_comment  (** inside a comment *)
  | Ac_id of ac_id  (** identifier references *)
  | Ac_enum  (** identifier in enum declaration *)
  | Ac_key of { obj_type: Type.t }  (** object key *)
  | Ac_literal of { lit_type: Type.t }  (** inside a literal like a string or regex *)
  | Ac_module  (** a module name *)
  | Ac_type  (** type identifiers *)
  | Ac_qualified_type of Type.t  (** qualified type identifiers *)
  | Ac_member of {
      obj_type: Type.t;
      in_optional_chain: bool;
      bracket_syntax: ac_id option;
      (* loc of `.foo` or `[foo]` *)
      member_loc: Loc.t option;
      is_type_annotation: bool;
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
  tparams_rev: Type.typeparam list;
  ac_loc: ALoc.t;
  token: string;
  autocomplete_type: autocomplete_type;
}

val process_location :
  trigger_character:string option ->
  cursor:Loc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  process_location_result option

val autocomplete_set_hooks : cursor:Loc.t -> unit

val autocomplete_unset_hooks : unit -> unit
