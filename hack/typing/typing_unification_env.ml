(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

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

  (* Keep track of the Tvar's we've hit in those other functions so we aren't
   * infinitely recursive. TODO implement this as part of the aforementioned
   * "apply" function. *)
  seen_tvars: ISet.t;

  (* The list of expression dependent types we have visited thus far. This is
   * used in Typing_subtype to properly handle the 'this' type for inheritance.
   *)
  dep_tys: (Typing_reason.t * Typing_defs.dependent_type) list;
}

let empty = { non_null = false; seen_tvars = ISet.empty; dep_tys = []; }
