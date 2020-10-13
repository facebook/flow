(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The definitions in this module handle the sealing and unsealing of generics. *)

(* A generic is the ALoc.id of a type variable definition site (or class
   definition site for `this`.) *)
type generic = ALoc.id * string

(* A bound corresponds to the upper bound annotated on a type variable,
   but it can also include more information if that annotation is itself a
   type variable. This lets us compress variables bounded by other variables
   into a single data structure. For example:
     function f<X: number>(x: X) { ... }
   The ID for `X` is its definition location and name, and nothing else
   (because the fact that it is bounded by the type `number` is handled by the
   `bound` type field in a `GenericT`), but compare:
     function f<X, Y: X>(y: Y) { ... }
   Here, `Y` is bounded by `X` which also has an ID. We compress these together
   for `X` to avoid having nested `GenericT`s, and so the ID for `Y` is its own
   definition and name, followed by `X`'s ID.

   In the comments of this module, I write such complex ids like "Y:X".
   *)

(* The id type has a Bound constructor rather than just being an alias for bound
   because later diffs will introduce other kinds of IDs (for spreads) *)
type id = Bound of bound

and bound = generic * id option

let rec to_string id =
  let open Utils_js in
  match id with
  | Bound ((_, name), None) -> name
  | Bound ((_, name), Some id) -> spf "%s:%s" name (to_string id)

let rec equal_bound b1 b2 =
  match (b1, b2) with
  | (((id1, _), Some k1), ((id2, _), Some k2)) -> ALoc.equal_id id1 id2 && equal_id k1 k2
  | (((id1, _), None), ((id2, _), None)) -> ALoc.equal_id id1 id2
  | _ -> false

and equal_id k1 k2 =
  match (k1, k2) with
  | (Bound b1, Bound b2) -> equal_bound b1 b2

let rec collapse l u =
  match l with
  | Bound (g, None) -> Some (Bound (g, Some u))
  | Bound (g, Some u') -> Some (Bound (g, collapse u' u))

let make_bound_id aloc_id name = Bound ((aloc_id, name), None)

(* legacy, just for RPolyTest reasons *)
let aloc_of_id id =
  match id with
  | Bound ((l, _), _) -> l

type sat_result =
  | Satisfied
  | Lower of id
  | Upper of id

(* When flowing one generic into another, determine if the lower generic's ID
   satisfies the requirements of the upper, allowing the type check to be inclusion
   rather than bound-to-bound. In the simple case where all generic
   IDs were just ALoc.ids, this would just be equality and return a boolean success
   or failure, and in the failure case, we'd flow the generic bound of the lower
   type into the upper generic.

   Since IDs now can represent multiple generics, we need to do more work, and there's
   multiple possible results. For example, suppose X and Y are generics, and Y's upper
   bound is X. Then the Generic.id for Y is "Y:X". When a GenericT for Y flows into a
   GenericT for X, we check "Y:X" against "X" to see if it's satisfied. Y =/= X,
   so we "strip off" Y from Y:X and check X against X, which succeds, so we return
   Satisfied.

   If Z:W is not bounded by X, and we check if Z:W satisfies X, we first strip off
   Z, then see that W doesn't satisfy X. This then returns "Upper X"--this means
   that after matching generics against each other as much as possible, X was still
   unsatisfied. In flow_js, the caller, this means that we'll flow the upper bound
   of the lower GenericT into empty, and presumably raise an error.

   On the other hand, what if we have a program like:

    function g<X, Y: X | number>(a: {y: Y}): {y: Y} {
      if (typeof a.y !== 'number') {
        return a;
      }
      // ...
    }

   When we return a, we check the type of `a.y` against Y invariantly.
   Because we've refined a.y, we're able to collapse it into a single GenericT
   with id `Y:X`. However, the `Y` in the return type hasn't been refined, so it's
   a GenericT with id `Y`, whose bound is the union of NumT and a GenericT for X.

   When we flow the type of a.y into Y, we're comparing the ids `Y:X` and `Y`. We see
   that they match, but then there's a hanging `X` on the left. We therefore return
   Lower X, which tells flow_js that it needs to flow a GenericT with id X on the left
   into the bound of the GenericT on the right--in this case, X | number.
    *)

let rec satisfies id1 id2 =
  let opt_satisfies o1 o2 =
    match (o1, o2) with
    (* everything in id2 was satisfied by id1 with nothing left *)
    | (None, None) -> Satisfied
    (* everything in id2 was satisfied, but there's generics on the left still
       unmatched *)
    | (Some id, None) -> Lower id
    (* something in id2 wasn't satisfied *)
    | (None, Some id) -> Upper id
    (* continue to the next layer *)
    | (Some id1, Some id2) -> satisfies id1 id2
  in
  match (id1, id2) with
  (* if top-level ids match, strip them off and compare the next levels, if they exist *)
  | (Bound ((b1, _), o1), Bound ((b2, _), o2)) when ALoc.equal_id b1 b2 -> opt_satisfies o1 o2
  (* if the top-level ids don't match but the LHS has more to look at, strip it off and try again *)
  | (Bound (_, Some id1), Bound _) -> satisfies id1 id2
  (* something in id2 wasn't satisfied *)
  | (Bound (_, None), Bound _) -> Upper id2
