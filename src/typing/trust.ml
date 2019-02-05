(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Currently, trust-tracking is unimplemented. As a placeholder, we us a unit
 * value as a placeholder. *)
type trust = unit

(* Trust creation functions:
   These functions will be used to generate trust information when DefTs are
   instantiated (unless the trust information ought to be propagated from some
   other type. *)

(* bogus_trust is a development placeholder, used to invent trust when we
   haven't yet figured out what the right trust information to use is. The
   number of calls to bogus_trust in the codebase is likely to be a decent
   measure of how complete the trusted types project is. *)
(* Currently, bogus_trust is the only way to create trust, since actual trust
   tracking is unimplemented. *)
let bogus_trust () = ()


(* Given a trust datum, add (if not already present) the requirement that it
   be trusted or private. *)
let make_trusted () = ()
let make_private () = ()
