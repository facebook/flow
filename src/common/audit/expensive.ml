(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module provides a simple way to audit calls to functions that are deemed
   expensive: have such a call explicitly pass a "token" that describes the
   result of a manual audit of the call.

   Tokens are of type `audit`. This type is abstract outside this module, and
   the only ways of "creating" values of that type are by using the constants
   below. To further keep the overhead of this mechanism down, we implement
   these tokens as unit values; in principle we could have used an ADT instead.

   Currently, this mechanism is to track accesses to shared memory, which
   involve expensive (both in time and in space!) serialization and
   deserialization of data. Since the API for these accesses doesn't look very
   different from calls to "normal" functions, we can easily abuse the API. The
   solution is to `wrap` the API so that the type system then *demands* passing
   tokens describing the results of manual audits of calls to the API.
*)

type audit = unit

(* Use this token to describe expensive calls that are deemed OK.

   For example, shared memory accesses that happen in (short-lived, parallel)
   worker processes are usually OK, since they happen, well, in parallel (so
   time cost is OK) and their memory is reclaimed after they die (so memory cost
   is OK).
*)
let ok : audit = ()

(* Use this token to describe expensive calls that either need immediate fixing
   or careful monitoring of costs.

   For example, shared memory accesses that happen in the master deserve
   warning. Especially when these calls are multiplied over a large number of
   files, they cause both a time bottleneck in the master *and* build up memory
   pressure that might eventually cultimate in memory leaks or long / frequent
   GC pauses.
*)
let warn : audit = ()

(* Given a function `f`, `wrap f` demands an extra argument, ~audit preceding
   other arguments, but otherwise behaves the same way. *)
type 'a t = audit:audit -> 'a

let wrap f ~audit:_ = f
