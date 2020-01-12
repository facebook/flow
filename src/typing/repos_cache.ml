(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Reposition cache to prevent repositioning loops

  Repositioning improves error messages by changing reasons to positions close
  to the type incompatibility in the source file. Many operations result in
  repositioning, for example function return types are repositioned to call
  sites and binding types are repositioned to variable references.

  Repositioning a tvar poses an additional complication. We don't want to
  reposition the OpenT directly; we want to reposition any new lower bounds the
  tvar will get. To accomplish this, repositioning may create new tvars which
  are connected to the original tvar via a ReposLowerT constraint.

  Because repositioning can create new tvars, we need to take care to generate a
  finite number of them. A "repositioning loop" can occur when repositioning a
  UnionT or union-like types MaybeT and OptionalT.

  See `Flow_js.reposition` for details of the repositioning logic w.r.t. tvars
  and union-like types MaybeT, OptionalT, and UnionT.

  This cache breaks repositioning loops by remembering the provenance of
  "repositioning tvars" -- i.e., tvars created for the purpose of repositioning.
  This provenance is stored in two parts.

  1. A cache of (tvar id, reason) pairs to repositioning tvars. If we see the
  same pair twice, we can re-use the existing repositioning tvar. This ensures
  we do not create an unbounded number of tvars.

  2. A breadcrumb trail of repositioning tvar ids. A repositioning loop can
  involve multiple repositionings. If we reposition (T1,r1) => T2 and (T2,r2) =>
  T3, repositioning (T3,r1) can re-use T2 instead of creating a new T4. To
  realize this, store the fact that T3 came from T2, which came from T1.

  Why is it reasonable to re-use repositioning tvars? Do we preserve the same
  typechecking behavior with the cache and without?

  Say we create T1 for id/reason as follows:

    OpenT (_, id) -> ReposLowerT (reason, T1)

  Then for any lower bound L such that

    L -> OpenT (_, id)

  T1 is going to hold `mod_reason (reason, L)`

  If we created another T2, it would also hold `mod_reason (reason, L)` so
  effectively T2 would have the same lower bounds as T1.

  What about upper bounds? You could have half attached to T1 and half attached
  to T2, but the claim is that `mod_reason (reason, L)` is going to flow to all
  of them anyway, and it's those interactions that could either lead to errors
  or more constraints, so nothing changes.

  It's illustrative to follow the inference step-by-step. In the following
  examples, assume a simplistic Map class declaration:

  > declare class Map<K,V> {
  >   get(k:K): ?V
  >   set(k:K, v:V): void
  > }

  Note: the return type of `get` is modeled as a maybe type, ?V, for syntactic
  convenience in the step-by-step explanation but the same principle can be
  applied to unions and optionals.

  Example 1: Single-repositioning loop

  Now consider the following line of code:

  > map.set(key, Map.find_opt(key));
                 ^^^^^^^^^^^^ A. ?V -> RL (r1, T1)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^ B. T1 -> V

  The return type of `get` is repositioned to the call site. Repositioning
  creates a new tvar `T1` which flows into the parameter type `V` of the
  function `set`.

  A. ?V -> RL (r1, T1)
  1. repos(?V, r1) -> T1
  2. ?repos(V, r1) -> T1
  3. INTRO T1v. V -> RL (r1, T1v)  { add to cache }
  4. ?T1v -> T1

  B. T1 -> V
  1. ?T1v -> V  { from A4 }
  2. ?T1v -> RL (r1, T1v)  { from A3 }
  3. repos(?T1v, r1) -> T1v
  4. ?repos(T1v, r1) -> T1v
  6. ?T1v -> T1v { cache hit reuses T1v }

  In step 6 above, we return T1v instead of creating a new tvar. We find T1v
  because (V, r1) -> T1v is in the cache, and V is in the breadcrumb trail of
  T1v.

  This example exploits the fact that A and B are able to reuse the same cache,
  but it's important to note that this cache is also designed to detect loops
  within a single invocation of `flow`.

  What if the cache were empty before B? We would still find the loop:

  B. T1 -> V
  1. ?T1v -> V  { from A4 }
  2. ?T1v -> RL (r1, T1v)  { from A3 }
  3. repos(?T1v, r1) -> T1v
  4. ?repos(T1v, r1) -> T1v
  5. INTRO T1v'. T1v -> RL (r1, T1v')  { add to cache }
  6. ?T1v' -> T1v
  7. ?T1v' -> RL (r1, T1v')  { from B5 }
  8. repos(?T1v', r1) -> T1v'
  9. ?repos(T1v', r1) -> T1v'
  10. ?T1v' -> T1v'  { cache hit reuses T1v' }

  Example 2: Multiple-repositioning loop

  A single reposition is rare in practice. The below example shows how the cache
  breaks loops that result from multiple repositions. First, the return type of
  `get` is repositioned to the call site. The repositioning tvar is then
  repositioned again to a variable reference. The var-ref repositioning tvar
  then flows back into `V`.

  > var val = Map.find_opt(key);
              ^^^^^^^^^^^^ A. ?V -> RL (r1, T1)
  > map.set(key, val);
                 ^^^ B. T1 -> RL (r2, T2)
    ^^^^^^^^^^^^^^^^^ C. T2 -> V

  A. (same as above)

  B. T1 -> RL (r2, T2)
  1. ?T1v -> RL (r2, T2)
  2. repos(?T1v, r2) -> T2
  3. ?repos(T1v, r2) -> T2
  4. INTRO T2v. T1v -> RL (r2, T2v)  { add to cache }
  5. ?T2v -> T2

  C. T2 -> V
  1. ?T2v -> V  { from B5 }
  2. ?T2v -> RL (r1, T1v)  { from A3 }
  3. repos(?T2v, r1) -> T1v
  4. ?repos(T2v, r1) -> T1v
  5. ?T1v -> T1v  { cache hit reuses T1v }
  6. ?T1v -> RL (r2, T2v)  { from B4 }
  7. repos(?T1v, r2) -> T2v
  8. ?repos(T1v, r2) -> T2v
  9. ?T2v -> T2v  { cache hit reuses T2v }
*)

type ident = Constraint.ident

module ReposMap = WrappedMap.Make (struct
  type key = ident * Reason.t

  type t = key

  let compare = Pervasives.compare
end)

type t = {
  cache: Type.t ReposMap.t;
  back: ident list IMap.t;
}

let empty = { cache = ReposMap.empty; back = IMap.empty }

let breadcrumb id x =
  match IMap.find_opt id x.back with
  | None -> []
  | Some bs -> bs

let find id reason x =
  let rec loop hd tl =
    match ReposMap.find_opt (hd, reason) x.cache with
    | Some _ as found -> found
    | None ->
      (match tl with
      | [] -> None
      | hd :: tl -> loop hd tl)
  in
  loop id (breadcrumb id x)

let add reason t t' x =
  let (_, id) = Type.open_tvar t in
  let (_, id') = Type.open_tvar t' in
  {
    cache = ReposMap.add (id, reason) t' x.cache;
    back = IMap.add id' (id :: breadcrumb id x) x.back;
  }
