(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type id_kind =
  | PropertyAccess of Type.t (* receiver type *)
  | Import of string (* remote name *) * Type.t (* module type *)
  | Exists
  | Other

type name = string

(* An environment of type parameters that are in scope. *)
type tparam_env = (name * Loc.t) list

(* Type scheme: a type along with the type parameters that are in scope

 * The parameter environment is useful in handling inferred type parameters. During checking,
 * type parameters are expanded to so called generated tests, that hold the `RPolyTest` reason.
 * During normalization we wish to recover the more informative original parameter names,
 * instead of these tests. To do so it's important to know what type parameters were in scope at the
 * location where a type was added in the type tables. The type parameter name is saved with that
 * `RPolyTest` reason. What is not guaranteed, however, is that the parameter is in scope at the
 * point of the query. For example, consider:
 *
 *   function f<T>(x: T) { return x; }
 *   const y = f(1);
 *
 * The type for `x` within `f` is synthesized (at normalization) from the generated bounds (for
 * `T`): Empty and Mixed. From the `RPolyTest` reasons on these types we can recover the name `T`
 * and return that instead of the bounds.
 *
 * Due to the lack of a return type for `f`, however, querying `y` returns the exact same answer
 * even outside the context of `f`. The reasons will still point to `T` even though it's now out of
 * scope. In this case we need to fall back to the actual bounds and return those instead. So the
 * normalized type here would be: Empty | Mixed, which simplifies to Mixed.
 *
 * Therefore to determine which of the two cases to pick we need to know what parameters are in
 * scope.
 *
 * This structure grows and shrinks as we go deeper in scopes but does not cross `Context.t`
 * boundaries and so does not need to persist like type tables.
 *)
type type_scheme = Scheme of (tparam_env * Type.t) [@@unboxed]

type scheme_entry = name * type_scheme * id_kind
type type_entry = name * Type.t * id_kind

type t

val create: unit -> t
val set: (string * Loc.t) list -> t -> Loc.t -> Type.t -> unit
val set_info: (string * Loc.t) list -> t -> Loc.t -> type_entry -> unit
val fold_coverage: (Loc.t -> type_scheme -> 'a -> 'a) -> t -> 'a -> 'a
val find_unsafe_coverage: t -> Loc.t -> type_scheme
val find_unsafe_coverage_type: t -> Loc.t -> Type.t
val reset: t -> unit
val copy: t -> t
val find_type_info: pred:(Loc.t -> bool) -> t -> (Loc.t * scheme_entry) option
val function_decl_loc : (Loc.t * 'a) option -> Loc.t -> Loc.t
val coverage_to_list: t -> (Loc.t * type_scheme) list
val coverage_hashtbl: t -> (Loc.t, type_scheme) Hashtbl.t
val type_info_hashtbl: t -> (Loc.t, scheme_entry) Hashtbl.t
