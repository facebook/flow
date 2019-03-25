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

type op_mode =
  | OpAnd
  | OpOr

let unit_of_op = function
  | OpAnd -> true (* mixed *)
  | OpOr -> false (* empty *)

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
    | Any, _ -> Any
    | _, Any -> Any
    | Empty, _ -> Empty
    | _, Empty -> Empty
    | Checked, Checked -> Checked

  let m_or = function
    | Any, _ -> Any
    | _, Any -> Any
    | Empty, m2 -> m2
    | m1, Empty -> m1
    | Checked, Checked -> Checked

  let merge kind x =
    match kind with
    | OpAnd -> m_and x
    | OpOr -> m_or x

  let to_bool = function
    | Any
    | Empty -> false
    | Checked -> true
end

type tvar_status =
  | Started
  | Done of Kind.t

class visitor = object (self)

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
  val mutable tvar_cache: tvar_status IMap.t = IMap.empty

  method private tvar cx id =
    let root_id, constraints = Context.find_constraints cx id in
    if id != root_id
    then self#tvar cx root_id
    else begin
      match IMap.get root_id tvar_cache with
      | Some Started -> Kind.Any
      | Some Done cov -> cov
      | None ->
        tvar_cache <- IMap.add root_id Started tvar_cache;
        let open Constraint in
        let cov = match constraints with
          | Resolved t
          | FullyResolved t ->
            self#type_ cx t
          | Unresolved bounds ->
            let bounds = TypeMap.keys bounds.lower in
            self#types_list cx OpOr bounds
        in
        tvar_cache <- IMap.add root_id (Done cov) tvar_cache;
        cov
    end

  method type_ cx = function
    | OpenT (_, id) -> self#tvar cx id
    | MergedT (_, uses) -> self#merged_t cx uses
    | EvalT (t, _, id) -> self#eval_t cx t id

    (* Non-concrete (fallthrough) constructors *)
    | AnnotT (_, t, _)
    | ExactT (_, t)
    | DefT (_, _, PolyT (_, _, t, _))
    | DefT (_, _, TypeAppT (_, t, _))
    | DefT (_, _, TypeT (_, t))
    | OpenPredT (_, t, _, _)
    | ReposT (_, t)
    | ShapeT t
    | ThisClassT (_, t)
    | ThisTypeAppT (_, t, _, _)
      ->
      self#type_ cx t

    | UnionT (_, rep) ->
      let t0, (t1, ts) = UnionRep.members_nel rep in
      self#types_nel cx OpOr (t0, t1::ts)

    | IntersectionT (_, rep) ->
      let t0, (t1, ts) = InterRep.members_nel rep in
      self#types_nel cx OpAnd (t0, t1::ts)

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
    | OptionalT _

    | DefT (_, _, ArrT _)
    | DefT (_, _, BoolT _)
    | DefT (_, _, CharSetT _)
    | DefT (_, _, ClassT _)
    | DefT (_, _, FunT _)
    | DefT (_, _, InstanceT _)
    | DefT (_, _, IdxWrapper _)
    | DefT (_, _, MixedT _)
    | DefT (_, _, NumT _)
    | DefT (_, _, NullT)
    | DefT (_, _, ObjT _)
    | DefT (_, _, ReactAbstractComponentT _)
    | DefT (_, _, SingletonNumT _)
    | DefT (_, _, SingletonStrT _)
    | DefT (_, _, SingletonBoolT _)
    | DefT (_, _, StrT _)
    | DefT (_, _, VoidT)
      ->
      Kind.Checked

    (* Concrete uncovered constructors *)
    | MatchingPropT _
    | TypeDestructorTriggerT _

    | DefT (_, _, EmptyT) -> Kind.Empty
    | AnyT _ -> Kind.Any

  method private types_of_use acc = function
    | UseT (_, t) -> t::acc
    | ReposLowerT (_, _, u) -> self#types_of_use acc u
    | _ -> acc

  method private merged_t cx uses =
    let ts = List.fold_left self#types_of_use [] uses in
    self#types_list cx OpAnd ts

  method private eval_t cx t id =
    let evaluated = Context.evaluated cx in
    let t = match IMap.get id evaluated with
      | Some cached -> cached
      | None -> t
    in
    self#type_ cx t

  method private types_ cx op acc = function
    | [] -> acc
    | t::ts ->
      let cov = self#type_ cx t in
      let acc = Kind.merge op (acc, cov) in
      begin match acc with
        | Kind.Any ->
          (* Cannot recover from Any, so exit early *)
          Kind.Any
        | Kind.Checked
        | Kind.Empty -> self#types_ cx op acc ts
      end

  method private types_list cx op ts =
    match ts with
    | [] -> Kind.Empty
    | t::ts -> self#types_nel cx op (t, ts)

  method private types_nel cx op (t, ts) =
    let init = self#type_ cx t in
    match init with
    | Kind.Any -> Kind.Any
    | Kind.Checked
    | Kind.Empty -> self#types_ cx op init ts

end

type file_coverage = {
  covered: int;
  any: int;
  empty: int;
}

let initial_coverage = {
  covered = 0;
  any = 0;
  empty = 0;
}
