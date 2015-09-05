(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* A "unification environment" to track certain metadata about the unification
 * or subtyping of a type that isn't captured by the type itself. This is
 * currently managed manually by unify and sub_type, but TODO we should probably
 * have some sort of "apply" function that handles this sort of thing for
 * recursive (but not corecursive) functions, managing to make sure they don't
 * get stuck in a loop with Tvar, mapping across Tunresolved, dealing with "as"
 * constraints, etc. *)
type uenv = {
  (* The type has already been proved to be non-null, and we should drop any
   * further Toption we see. This can happen with types which are
   * Toption[Tunresolved[Toption]] or similar. *)
  non_null: bool;

  (* unify/sub_type are careful to make sure they don't get stuck in an infinite
   * loop expanding recursive types, but most other code isn't (and actually
   * doesn't care about Tvar at all). Keep track of the Tvar's we've hit in
   * those other functions so we aren't infinitely recursive.
   * TODO implement this as part of the aforementioned "apply" function. *)
   (* tvar_seen: ISet.t; *)

  (* Silence warnings when overwriting the non_null field, since it's currently
   * the only field, but may not always be that way. *)
  placeholder: unit
}

let empty = { non_null = false; placeholder = () }
