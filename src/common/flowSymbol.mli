(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * Symbol keeps track of properties of a named construct at the definition site.
 * Unlike reason that has to exist on every single type, symbol does not have to.
 * When they are attached to a type, it must always have a name and always be reliable.
 *)

type kind =
  | SymbolClass
  | SymbolComponent
  | SymbolConstant
  | SymbolConstructor
  | SymbolEnum
  | SymbolEnumMember
  | SymbolFile
  | SymbolFunction
  | SymbolInterface
  | SymbolMethod
  | SymbolModule
  | SymbolNamespace
  | SymbolProperty
  | SymbolTypeAlias
  | SymbolTypeParameter
  | SymbolVariable

val string_of_kind : kind -> string

type symbol

val mk_class_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_component_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_constant_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_enum_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_module_symbol : name:Flow_import_specifier.userland -> def_loc:ALoc.t -> symbol

val mk_namespace_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_type_alias_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_type_parameter_symbol : name:string -> def_loc:ALoc.t -> symbol

val mk_variable_symbol : name:string -> def_loc:ALoc.t -> symbol

val kind_of_symbol : symbol -> kind

val name_of_symbol : symbol -> string

val def_loc_of_symbol : symbol -> ALoc.t

val dump_symbol : symbol -> string

val dump_symbol_opt : symbol option -> string
