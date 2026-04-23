(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type kind =
  | Var
  | Let
  | ThisAnnot
  | Const
  | DeclaredVar
  | DeclaredLet
  | DeclaredConst
  | Type of {
      imported: bool;
      type_only_namespace: bool;
    }
  | Interface of {
      imported: bool;
      type_only_namespace: bool;
    }
  | Enum
  | Function
  | Class
  | DeclaredClass
  | DeclaredNamespace
  | Parameter
  | CatchParameter
  | Import
  | TsImport
  | DeclaredFunction
  | Internal
  | GeneratorNext
  | Component
  | ComponentParameter
  | Record
[@@deriving show]

type 'loc t

type 'loc entry = ('loc, 'loc) Ast.Identifier.t * kind

val empty : 'loc t

val singleton : 'loc entry -> 'loc t

val add : 'loc entry -> 'loc t -> 'loc t

val push : 'loc t -> 'loc t -> 'loc t

val exists : ('loc entry -> bool) -> 'loc t -> bool

val to_assoc : 'loc t -> (string * (kind * 'loc Nel.t)) list

(** For each name, returns the canonical kind (first-declaration kind) and the
    non-empty list of (loc, kind) pairs for every declaration of that name. *)
val to_map : 'loc t -> (kind * ('loc * kind) Nel.t) SMap.t

val allow_forward_ref : kind -> bool

val allow_redeclaration : kind -> bool

(** TypeScript-style value/type namespace partition.
    Names live independently in the value namespace and the type namespace,
    so [const A = 1; interface A {}] is permitted: [A] in value position
    resolves to the const, [A] in type position resolves to the interface. *)
type namespace =
  | NValue
  | NType
[@@deriving show]

(** The namespaces a binding kind populates.
    [Class | DeclaredClass | Enum | DeclaredNamespace] populate both. *)
val namespaces_of_kind : kind -> namespace list

val same_namespace : kind -> kind -> bool

(** Splits a [Bindings.t] into [(value_bindings, type_bindings)] preserving
    declaration order within each namespace. A binding in both namespaces
    (e.g. [Class]) appears in both halves. *)
val split_by_namespace : 'loc t -> 'loc t * 'loc t
