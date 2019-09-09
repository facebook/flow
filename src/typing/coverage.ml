(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This module computes whether a type is considered covered. It does so by
 * visiting the type until a "concrete" constructor is found. We consider a
 * constructor concrete if it has some runtime significance. So for example the
 * arrow constructor and the object constructor are concrete, whereas the union
 * or the intersection constructors are not. This means that whenever we encounter
 * a union we need to visit its parts to determine if this is a covered type.
 *
 * Some constructors that we, perhaps controversially, consider concrete are:
 *  - AnyWithLowerBoundT
 *  - AnyWithUpperBoundT
 *  - BoundT
 *  - KeysT
 *  - ExistsT
 *
 * In addition to being considered concrete the above constructors are also
 * considered covered.
 *)

open Type
open Utils_js

type op_mode =
  | OpAnd
  | OpOr

let unit_of_op = function
  | OpAnd -> true (* mixed *)
  | OpOr -> false

(* empty *)

module Taint = struct
  type t =
    | Untainted
    | Tainted

  let of_trust cx tr =
    if Trust_helpers.actual_trust cx tr |> Trust.is_tainted then
      Tainted
    else
      Untainted

  let to_string = function
    | Untainted -> "Untainted"
    | Tainted -> "Tainted"

  let m_and = function
    | (Tainted, t)
    | (t, Tainted) ->
      t
    | (Untainted, Untainted) -> Untainted

  let m_or = function
    | (Tainted, _)
    | (_, Tainted) ->
      Tainted
    | (Untainted, Untainted) -> Untainted

  let to_bool = function
    | Untainted -> true
    | Tainted -> false

  let merge = function
    | OpAnd -> m_and
    | OpOr -> m_or
end

module Kind = struct
  type t =
    | Checked
    | Any
    | Empty

  let to_string = function
    | Checked -> "Checked"
    | Any -> "Any"
    | Empty -> "Empty"

  let m_and = function
    | (Any, _) -> Any
    | (_, Any) -> Any
    | (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Checked, Checked) -> Checked

  let m_or = function
    | (Any, _) -> Any
    | (_, Any) -> Any
    | (Empty, m2) -> m2
    | (m1, Empty) -> m1
    | (Checked, Checked) -> Checked

  let merge kind x =
    match kind with
    | OpAnd -> m_and x
    | OpOr -> m_or x

  let to_bool = function
    | Any
    | Empty ->
      false
    | Checked -> true
end

let merge op ((k1, t1), (k2, t2)) = (Kind.merge op (k1, k2), Taint.merge op (t1, t2))

type tvar_status =
  | Started
  | Done of (Kind.t * Taint.t)

class visitor =
  object (self)
    val mutable tvar_cache : tvar_status IMap.t = IMap.empty
    (**
   * Type variables may appear in a cycle in the dependency graph, which requires
   * us to track the ones we've visited to avoid infinite recursion. There are three
   * stages of tvar resolution w.r.t coverage:
   *
   * - The tvar has not been seen before (there is no entry in tvar_cache). In this
   *   case we descend into the lower bounds of the tvar, marking its binding as
   *   Started in the tvar_cache.
   *
   * - The tvar has been seen and has been resolved (status = Done _). In this case
   *   we reuse the cached result.
   *
   * - The tvar is in the process of resolution (status = Started).
   *   These are types of the form:
   *
   *     type X = X | number
   *              ^
   *   we consider the recursive occurence as uncovered (Any). This case should
   *   be rare and it's arguable if we should be allowing it in the first place,
   *   so we assign the value that corresponds to the fewest guarantees.
   *)

    method private tvar cx id =
      let (root_id, constraints) = Context.find_constraints cx id in
      if id != root_id then
        self#tvar cx root_id
      else
        match IMap.get root_id tvar_cache with
        | Some Started -> (Kind.Any, Taint.Tainted)
        | Some (Done cov) -> cov
        | None ->
          tvar_cache <- IMap.add root_id Started tvar_cache;
          Constraint.(
            let cov =
              match constraints with
              | Resolved (_, t)
              | FullyResolved (_, t) ->
                self#type_ cx t
              | Unresolved bounds ->
                let bounds = TypeMap.keys bounds.lower in
                self#types_list cx OpOr bounds
            in
            tvar_cache <- IMap.add root_id (Done cov) tvar_cache;
            cov)

    method type_ cx =
      function
      | OpenT (_, id) -> self#tvar cx id
      | MergedT (_, uses) -> self#merged_t cx uses
      | EvalT (t, _, id) -> self#eval_t cx t id
      (* Non-concrete (fallthrough) constructors *)
      | AnnotT (_, t, _)
      | ExactT (_, t)
      | DefT (_, _, PolyT (_, _, t, _))
      | TypeAppT (_, _, t, _)
      | DefT (_, _, TypeT (_, t))
      | OpenPredT (_, t, _, _)
      | ReposT (_, t)
      | ShapeT t
      | ThisClassT (_, t)
      | ThisTypeAppT (_, t, _, _) ->
        self#type_ cx t
      | UnionT (_, rep) ->
        let (t0, (t1, ts)) = UnionRep.members_nel rep in
        self#types_nel cx OpOr (t0, t1 :: ts)
      | IntersectionT (_, rep) ->
        let (t0, (t1, ts)) = InterRep.members_nel rep in
        self#types_nel cx OpAnd (t0, t1 :: ts)
      (* Concrete covered constructors *)
      | AnyWithLowerBoundT _
      | AnyWithUpperBoundT _
      | BoundT _
      | CustomFunT _
      | ExistsT _
      | FunProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _
      | InternalT _
      | KeysT _
      | MaybeT _
      | ModuleT _
      | NullProtoT _
      | OpaqueT _
      | ObjProtoT _
      | OptionalT _ ->
        (Kind.Checked, Taint.Untainted)
      | DefT (_, t, ArrT _)
      | DefT (_, t, BoolT _)
      | DefT (_, t, CharSetT _)
      | DefT (_, t, ClassT _)
      | DefT (_, t, FunT _)
      | DefT (_, t, InstanceT _)
      | DefT (_, t, IdxWrapper _)
      | DefT (_, t, MixedT _)
      | DefT (_, t, NumT _)
      | DefT (_, t, NullT)
      | DefT (_, t, ObjT _)
      | DefT (_, t, ReactAbstractComponentT _)
      | DefT (_, t, SingletonNumT _)
      | DefT (_, t, SingletonStrT _)
      | DefT (_, t, SingletonBoolT _)
      | DefT (_, t, StrT _)
      | DefT (_, t, VoidT) ->
        (Kind.Checked, Taint.of_trust cx t)
      (* Concrete uncovered constructors *)
      (* TODO: Rethink coverage and trust for these types *)
      | MatchingPropT _
      | TypeDestructorTriggerT _ ->
        (Kind.Empty, Taint.Untainted)
      | DefT (_, t, EmptyT _) -> (Kind.Empty, Taint.of_trust cx t)
      | AnyT _ -> (Kind.Any, Taint.Tainted)

    method private types_of_use acc =
      function
      | UseT (_, t) -> t :: acc
      | ReposLowerT (_, _, u) -> self#types_of_use acc u
      | _ -> acc

    method private merged_t cx uses =
      let ts = List.fold_left self#types_of_use [] uses in
      self#types_list cx OpAnd ts

    method private eval_t cx t id =
      let evaluated = Context.evaluated cx in
      let t =
        match IMap.get id evaluated with
        | Some cached -> cached
        | None -> t
      in
      self#type_ cx t

    method private types_ cx op acc =
      function
      | [] -> acc
      | t :: ts ->
        let cov = self#type_ cx t in
        let ((merged_kind, _) as merged) = merge op (cov, acc) in
        begin
          match merged_kind with
          | Kind.Any ->
            (* Cannot recover from Any, so exit early *)
            (Kind.Any, Taint.Tainted)
          | Kind.Checked
          | Kind.Empty ->
            self#types_ cx op merged ts
        end

    method private types_list cx op ts =
      match ts with
      | [] -> (Kind.Empty, Taint.Tainted)
      | t :: ts -> self#types_nel cx op (t, ts)

    method private types_nel cx op (t, ts) =
      let (init_kind, init_trust) = self#type_ cx t in
      match init_kind with
      | Kind.Any -> (Kind.Any, Taint.Tainted)
      | Kind.Checked
      | Kind.Empty ->
        self#types_ cx op (init_kind, init_trust) ts
  end

open Coverage_response

let result_of_coverage = function
  | (Kind.Any, Taint.Untainted) ->
    assert_false "Any coverage kind cannot be associated with untainted"
  | (Kind.Any, _) -> Uncovered
  | (Kind.Empty, _) -> Empty
  | (Kind.Checked, Taint.Tainted) -> Tainted
  | (Kind.Checked, Taint.Untainted) -> Untainted

let to_bool = function
  | Empty
  | Uncovered ->
    false
  | Tainted
  | Untainted ->
    true

let m_or = function
  | (Uncovered, _)
  | (_, Uncovered) ->
    Uncovered
  | (Empty, m2)
  | (Untainted, m2) ->
    m2
  | (m1, Empty)
  | (m1, Untainted) ->
    m1
  | (Tainted, Tainted) -> Tainted

let initial_coverage = { untainted = 0; tainted = 0; uncovered = 0; empty = 0 }
