(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* The definitions in this module handle the sealing and unsealing of generics. *)

(* A generic is the ALoc.id of a type variable definition site (or class
   definition site for `this`.) *)
type generic = {
  id: ALoc.id;
  name: string;
}

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

type bound = {
  generic: generic;
  super: id option;
}

(* A spread is a nonempty list of bounds, each of which corresponds to an object
   spread operator applied to a generic with that bound. For example, in a program
   like
     function f<X: {}, Y: {}>(x: X, y: Y): {...X, ...Y} { return {...x, ...y} }
   the type {...X, ...Y} will be represented as a GenericT, whose type upper bound is
   {} and whose Generic.id is Spread [Y, X]. (Note that the ordering of this list of
   generics is reversed from how it appears in values). This information can then be used to enforce
   that the only values that are well-typed lower bounds for the type are other
   spreads from the same generics, in the same order--as opposed to the unsoundnesses that
   currently (e.g. around v0.130) exist:

     function f<X, Y>(x: X, y: Y): {...X, ...Y} {
      return {...y, ...x}; // out of order!
     }
*)
and spread = bound Nel.t

and id =
  | Spread of spread
  | Bound of bound

(* Used in the object_kit representation of objects, representing whatever generics,
   if any, have been spread into an object *)
type spread_id = bound list

let rec bound_to_string =
  let open Utils_js in
  function
  | { generic = { name; _ }; super = None } -> name
  | { generic = { name; _ }; super = Some id } -> spf "%s:%s" name (to_string id)

and to_string id =
  let open Utils_js in
  match id with
  | Bound bound -> bound_to_string bound
  | Spread ids ->
    spf "{ ...%s }" (String.concat ", ..." (Base.List.map ~f:bound_to_string (Nel.to_list ids)))

let rec equal_bound
    { generic = { id = id1; _ }; super = super1 } { generic = { id = id2; _ }; super = super2 } =
  if not (ALoc.equal_id id1 id2) then
    false
  else
    match (super1, super2) with
    | (None, None) -> true
    | (Some sup1, Some sup2) -> equal_id sup1 sup2
    | _ -> false

and equal_spreads s1 s2 =
  match Base.List.for_all2 s1 s2 ~f:equal_bound with
  | Base.List.Or_unequal_lengths.Ok res -> res
  | Base.List.Or_unequal_lengths.Unequal_lengths -> false

and equal_id k1 k2 =
  match (k1, k2) with
  | (Bound b1, Bound b2) -> equal_bound b1 b2
  | (Spread bs1, Spread bs2) -> equal_spreads (Nel.to_list bs1) (Nel.to_list bs2)
  | _ -> false

let rec collapse l u =
  match l with
  | Bound { generic; super = None } -> Some (Bound { generic; super = Some u })
  | Bound { generic; super = Some u' } -> Some (Bound { generic; super = collapse u' u })
  | Spread _ -> None

let spread_empty = []

let make_spread = function
  | Spread spread -> Nel.to_list spread
  | Bound bound -> [bound]

let make_bound_id aloc_id name = Bound { generic = { id = aloc_id; name }; super = None }

let make_spread_id (opt : spread_id) : id option =
  Base.Option.map ~f:(fun spread -> Spread spread) (Nel.of_list opt)

let make_spread_id_exn opt = Spread (Nel.of_list_exn opt)

let spread_subtract id1 id2 =
  Base.List.filter ~f:(fun a -> not @@ Base.List.mem ~equal:equal_bound id2 a) id1

(* We here enforce the invariant that no bound appears more than once
   in the spread list. *)
let spread_append id1 id2 = spread_subtract id1 id2 @ id2

let rec fold_ids ~f ~acc id =
  let test_bound acc { generic = { id; name }; super } =
    match super with
    | Some super -> f id name (fold_ids ~f ~acc super)
    | None -> f id name acc
  in
  match id with
  | Bound bound -> test_bound acc bound
  | Spread spread -> Nel.fold_left test_bound acc spread

let spread_exists = Base.List.is_empty %> not

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

   === Complex bounds ===

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

   === Spreads ===

   This function is also used to determine whether two spreaded generics are
   compatible. For the most part, this logic is orthogonal to compatibility of
   bounds, although a generic can flow into a spread that contains only
   itself--this should be safe:
     function f<X: {}>(x: X): {...X} {
       return x;
     }

   The rules below show examples of what scenario triggers them.

*)

let rec satisfies ~printer id1 id2 =
  let opt_satisfies o1 o2 =
    match (o1, o2) with
    (* everything in id2 was satisfied by id1 with nothing left *)
    | (None, None) ->
      printer (lazy ["Generics satisfied"]);
      Satisfied
    (* everything in id2 was satisfied, but there's generics on the left still
       unmatched *)
    | (Some id, None) ->
      printer (lazy [spf "Generics satisfied, with %s unmatched on lower bound" (to_string id)]);
      Lower id
    (* something in id2 wasn't satisfied *)
    | (None, Some id) ->
      printer (lazy [spf "Generics unsatisfied, with %s unmatched on upper bound" (to_string id)]);
      Upper id
    (* continue to the next layer *)
    | (Some id1, Some id2) -> satisfies ~printer id1 id2
  in
  let bound_satisfies
      { generic = { id = id1; _ }; super = super1 }
      ({ generic = { id = id2; _ }; super = super2 } as bound2) =
    if ALoc.equal_id id1 id2 then
      (* if top-level ids match, strip them off and compare the next levels, if they exist *)
      opt_satisfies super1 super2
    else
      match super1 with
      | Some super_id ->
        (* if the top-level ids don't match but the LHS has more to look at, strip it off and try again *)
        satisfies ~printer super_id (Bound bound2)
      | None ->
        (* something in id2 wasn't satisfied *)
        printer
          ( lazy
            [spf "Generics unsatisfied, with %s unmatched on upper bound" (bound_to_string bound2)]
            );
        Upper (Bound bound2)
  in
  printer (lazy [spf "Checking generics compatibility: %s ~> %s" (to_string id1) (to_string id2)]);
  match (id1, id2) with
  | (Bound bound1, Bound bound2) -> bound_satisfies bound1 bound2
  (* A generic is interchangeable with a spread of itself,
      as long as the spread doesn't contain other generics, and as long as the type bounds
     are compatible too. Example:

       function f<X: {}>(x: X): {...X} {
         return x; // ok
       }
  *)
  | (Bound bound1, Spread (bound2, [])) -> bound_satisfies bound1 bound2
  (* A 'bound' generic only represents one bound, so it can't
     satisfy a spread of more than one generic on its own. We can see if it's bounded by a
     'spread', though. Example:

      function f<X: {}, Y: {}, Z: {...X, ...Y}>(z: Z): {...X, ...Y} {
        return z; // ok
      }
  *)
  | (Bound { generic = _; super = Some id1 }, Spread _) -> satisfies ~printer id1 id2
  (* As above, but if it's not bounded by a spread, we can't do
     anything but strip off the generic from the lower type.

      function f<X: {}, Y: {}>(y: Y): {...X, ...Y} {
        return y; // should error
      }
  *)
  | (Bound { generic = _; super = None }, Spread _) ->
    printer
      (lazy ["Generics unsatisfied: single bound cannot satisfy spread with multiple elements"]);
    Upper id2
  (* A spread can flow into a bound if the LAST generic that was
     spread matches the lower bound (and assuming the types match).

      function fa<X: {}, Y: {}>(y: {...X, ...Y}): Y {
        return y;
      }

      function fb<X: {}, Y: {}>(y: {...Y, ...X}): Y {
        return y; // should error (but didn't previously with old generics)
      }
  *)
  | (Spread bounds, Bound _) ->
    let bound1 = Nel.rev bounds |> Nel.hd in
    satisfies ~printer (Bound bound1) id2
  (* If an upper bound spread expects more generic components in the spread than are provided in
     the lower bound, it clearly can't be satisfied *)
  | (Spread s1, Spread s2) when Nel.length s2 > Nel.length s1 ->
    printer (lazy ["Generics unsatisfied: more elements in upper bound than lower bound"]);
    Upper id2
  (* When comparing two spreads, we drop the tail elements of the lower bound so that
      its length matches the upper bound, and then we compare the elements pairwise. If they're
      all satisfied, then the spreads are satisfied. Recall the invariant that any generic exists
      only once in the spread list.

      function a<X: {}, Y: {}, Z: {}>(x: X, y: Y) {
       ({...x, ...y}: {...X}); // should be error
       ({...y, ...x}: {...X}); // yup
       ({...x}: {...Y, ...X}); // nope
       ({...y, ...x}: {...X, ...Y}); // should be error
       ({...x, ...y}: {...X, ...Y}); // yup
       ({...x, ...y}: {...X, ...Y, ...Y}); // yup
       ({...x, ...y}: {...Y, ...X, ...Y}); // yup
       ({...x, ...y}: {...X, ...Z, ...Y}); // nope
     }
  *)
  | (Spread s1, Spread s2) ->
    let s1 = Nel.to_list s1 in
    let s2 = Nel.to_list s2 in
    let s1 = Base.List.drop s1 (List.length s1 - List.length s2) in
    let is_satisfied sat =
      match sat with
      | Satisfied
      | Lower _ ->
        true
      | Upper _ -> false
    in
    if
      Base.List.fold2_exn
        ~f:(fun acc id1 id2 ->
          acc
          &&
          ( printer
              ( lazy
                [
                  spf
                    "Checking generics compatibility (from spread): %s ~> %s"
                    (bound_to_string id1)
                    (bound_to_string id2);
                ] );
            is_satisfied (bound_satisfies id1 id2) ))
        s1
        s2
        ~init:true
    then begin
      printer (lazy ["Generics satisfied: all elements of spread satisfied"]);
      Satisfied
    end else begin
      printer (lazy ["Generics unsatisfied: at least one element of spread unsatisfied"]);
      Upper id2
    end

module ArraySpread = struct
  type ro_status =
    | NonROSpread
    | ROSpread

  (* Simple lattice for seeing if all elements of a spread have the same generic ID *)
  type t =
    | Bottom
    | Top
    | Generic of (id * ro_status)

  (* A generic array spread of a read-only array type can only be reconstructed into a generic if the
     spread consists of exactly one element, which is the read-only array. *)
  let merge_ro t s =
    match (t, s) with
    | (Bottom, _)
    | (Top, _) ->
      t
    | (Generic _, ROSpread) -> Top
    | (Generic (id, _), s) -> Generic (id, s)

  let merge ~printer t g ro' =
    match (t, g) with
    | (Bottom, Some stat) -> Generic (stat, ro')
    | (Generic _, None)
    | (Top, _) ->
      Top
    | (Generic (id, _), Some id') when equal_id id id' -> merge_ro t ro'
    | (Generic (id, ro), Some id') ->
      begin
        match (satisfies ~printer id' id, satisfies ~printer id id') with
        | (Upper _, Upper _) -> Top
        | (Upper _, _) -> merge_ro (Generic (id', ro')) ro
        | _ -> merge_ro t ro'
      end
    | _ -> Top

  let to_option = function
    | Generic (id, _) -> Some id
    | _ -> None
end
