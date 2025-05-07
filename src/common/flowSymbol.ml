(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

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

let string_of_kind = function
  | SymbolClass -> "class"
  | SymbolComponent -> "component"
  | SymbolConstant -> "const"
  | SymbolConstructor -> "constructor"
  | SymbolEnum -> "enum"
  | SymbolEnumMember -> "enum member"
  | SymbolFile -> "file"
  | SymbolFunction -> "function"
  | SymbolInterface -> "interface"
  | SymbolMethod -> "method"
  | SymbolModule -> "module"
  | SymbolNamespace -> "namespace"
  | SymbolProperty -> "property"
  | SymbolTypeAlias -> "type alias"
  | SymbolTypeParameter -> "type parameter"
  | SymbolVariable -> "variable"

type symbol = {
  kind: kind;
  name: string;
  def_loc: ALoc.t;
}

let mk_symbol ~kind ~name ~def_loc = { kind; name; def_loc }

let mk_class_symbol ~name = mk_symbol ~kind:SymbolClass ~name

let mk_component_symbol ~name = mk_symbol ~kind:SymbolComponent ~name

let mk_constant_symbol ~name = mk_symbol ~kind:SymbolConstant ~name

let mk_enum_symbol ~name = mk_symbol ~kind:SymbolEnum ~name

let mk_module_symbol ~name =
  mk_symbol ~kind:SymbolModule ~name:(Flow_import_specifier.display_userland name)

let mk_namespace_symbol ~name = mk_symbol ~kind:SymbolNamespace ~name

let mk_type_alias_symbol ~name = mk_symbol ~kind:SymbolTypeAlias ~name

let mk_type_parameter_symbol ~name = mk_symbol ~kind:SymbolTypeParameter ~name

let mk_variable_symbol ~name = mk_symbol ~kind:SymbolVariable ~name

(* TODO: create more constructors for other kinds of symbols as needed *)

let kind_of_symbol { kind; _ } = kind

let name_of_symbol { name; _ } = name

let def_loc_of_symbol { def_loc; _ } = def_loc

let string_of_aloc aloc =
  match ALoc.source aloc with
  | None -> ""
  | Some file -> spf "%s:%s" (File_key.to_string file) (ALoc.to_string_no_source aloc)

let dump_symbol { kind; name; def_loc } =
  spf "%s: %s %s" (string_of_aloc def_loc) (string_of_kind kind) name

let dump_symbol_opt = Base.Option.value_map ~default:"no symbol" ~f:dump_symbol
