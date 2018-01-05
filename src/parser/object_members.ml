(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Parser_env
module Error = Parse_error
module SSet = Set.Make(String)

module SMap = struct
  include Map.Make(String)
  let get x t = try Some (find x t) with Not_found -> None
end

type mark =
  | Abstract of Loc.t
  | Native

let empty = (SSet.empty, SMap.empty, SMap.empty, [])

let members (_, _, _, members) = List.rev members

let property_name = Expression.Object.Property.(function
  | Literal (_, { Literal.value = Literal.String n; _; })
  | Identifier (_, n) ->
      Some n
  | PrivateName (_, (_, n)) -> Some ("#" ^ n)
  | _ -> None)

module type S = sig
  type internal_t

  module AbstractMethod : sig
    type t
    val is_static: t -> bool
    val intern: t -> internal_t
    val add: env -> Loc.t Variance.t option -> t -> mark SMap.t -> mark SMap.t
  end
  module Method : sig
    type t
    val is_static: t -> bool
    val intern: t -> internal_t
    val add: env -> Loc.t Variance.t option -> t -> mark SMap.t -> mark SMap.t
  end
  module Property : sig
    type t
    val is_static: t -> bool
    val intern: t -> internal_t
    val add: env -> Loc.t option -> t -> mark SMap.t -> mark SMap.t
  end
  module Getter : sig
    type t
    val is_static: t -> bool
    val intern: t -> internal_t
    val add: env -> Loc.t option -> Loc.t Variance.t option -> t -> mark SMap.t -> mark SMap.t
  end
  module Setter : sig
    type t
    val is_static: t -> bool
    val intern: t -> internal_t
    val add: env -> Loc.t option -> Loc.t Variance.t option -> t -> mark SMap.t -> mark SMap.t
  end
end

module Make(X : S) = struct
  type abstract_method_t = X.AbstractMethod.t
  type method_t = X.Method.t
  type property_t = X.Property.t

  let add_member m (privates, static_marks, marks, members) =
    (privates, static_marks, marks, m::members)

  let update_privates f (privates, static_marks, marks, methods) =
    (f privates, static_marks, marks, methods)

  let update_marks ~static f (privates, static_marks, marks, methods) =
    if static
    then (privates, f static_marks, marks, methods)
    else (privates, static_marks, f marks, methods)

  let add_abstract_method env variance (m: X.AbstractMethod.t) t =
    let static = X.AbstractMethod.is_static m in
    let f = X.AbstractMethod.add env variance m in
    t |> update_marks ~static f |> add_member (X.AbstractMethod.intern m)

  let add_method env variance (m: X.Method.t) t =
    let static = X.Method.is_static m in
    let f = X.Method.add env variance m in
    t |> update_marks ~static f |> add_member (X.Method.intern m)

  let add_property env abstract (p: X.Property.t) t =
    let static = X.Property.is_static p in
    let f = X.Property.add env abstract p in
    t |> update_marks ~static f |> add_member (X.Property.intern p)

  let add_getter env abstract variance (g: X.Getter.t) t =
    let static = X.Getter.is_static g in
    let f = X.Getter.add env abstract variance g in
    t |> update_marks ~static f |> add_member (X.Getter.intern g)

  let add_setter env abstract variance (s: X.Setter.t) t =
    let static = X.Setter.is_static s in
    let f = X.Setter.add env abstract variance s in
    t |> update_marks ~static f |> add_member (X.Setter.intern s)
end

module type MemberS = sig
  type t
  type internal_t
  val loc: t -> Loc.t
  val name: t -> string option
  val is_static: t -> bool
  val intern: t -> internal_t
end

module AbstractMethod(X : MemberS) = struct
  type t = X.t

  let is_static = X.is_static
  let intern = X.intern

  let add env variance t marks =
    match X.is_static t, variance, X.name t with
    | _, Some (loc, _), _ ->
        error_at env (loc, Error.UnexpectedVariance);
        marks
    | _, None, Some "constructor" ->
        error_at env (X.loc t, Error.AbstractConstructor);
        marks
    | true, None, Some "name" ->
        error_at env (X.loc t, Error.AbstractStaticName);
        marks
    | _, None, None -> marks
    | _, None, Some name ->
        match SMap.get name marks with
        | Some (Abstract _) ->
            error_at env (X.loc t, Error.DuplicateAbstractMethods name);
            marks
        | Some Native ->
            error_at env (X.loc t, Error.ImmediateAbstract name);
            marks
        | None ->
            SMap.add name (Abstract (X.loc t)) marks
end

module Method(X : MemberS) = struct
  type t = X.t

  let is_static = X.is_static
  let intern = X.intern

  let add env variance t marks =
    match variance with
    | Some (loc, _) ->
        error_at env (loc, Error.UnexpectedVariance);
        marks
    | None ->
        match X.name t with
        | None -> marks
        | Some name ->
            match SMap.get name marks with
            | Some (Abstract loc) ->
                error_at env (loc, Error.ImmediateAbstract name);
                marks
            | Some Native -> marks
            | None -> SMap.add name Native marks
end

module Property(X : MemberS) = struct
  type t = X.t

  let is_static = X.is_static
  let intern = X.intern

  let add env abstract t marks =
    match abstract with
    | Some _ ->
        error_at env (X.loc t, Error.AbstractProperty);
        marks
    | None ->
        match X.name t with
        | None -> marks
        | Some name ->
            match SMap.get name marks with
            | Some (Abstract loc) ->
                error_at env (loc, Error.ImmediateAbstract name);
                marks
            | Some Native -> marks
            | None -> SMap.add name Native marks
end

module Etter (X : MemberS) (Y : sig
  val error: Error.t
end) = struct
  type t = X.t

  let is_static = X.is_static
  let intern = X.intern

  let add env abstract variance t marks =
    match abstract, variance with
    | Some _, _ ->
        error_at env (X.loc t, Y.error);
        marks
    | None, Some (loc, _) ->
        error_at env (loc, Error.UnexpectedVariance);
        marks
    | None, None ->
        match X.name t with
        | None -> marks
        | Some name ->
            match SMap.get name marks with
            | Some (Abstract loc) ->
                error_at env (loc, Error.ImmediateAbstract name);
                marks
            | Some Native -> marks
            | None -> SMap.add name Native marks
end

module Getter(X : MemberS) = Etter (X) (struct
 let error = Error.AbstractGetter
end)

module Setter(X : MemberS) = Etter (X) (struct
  let error = Error.AbstractSetter
end)

module PrivateField = struct
  type t = Loc.t Class.PrivateField.t

  let intern t = Class.Body.PrivateField t

  let add env abstract t privates =
    match abstract with
    | Some _ ->
        let loc, _ = t in
        error_at env (loc, Error.AbstractProperty);
        privates
    | None ->
        Class.PrivateField.(match t with
        | _, {key = (loc, (_, "constructor")); static; _} ->
            let error = Error.InvalidFieldName ("constructor", static, true) in
            error_at env (loc, error);
            privates
        | _, {key = (loc, (_, name)); _} ->
            if SSet.mem name privates
            then (error_at env (loc, Error.DuplicatePrivateFields name); privates)
            else SSet.add name privates)
end
