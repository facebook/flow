(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type trust

val string_of_trust: trust -> string

val bogus_trust  : unit -> trust
val literal_trust: unit -> trust
val annot_trust  : unit -> trust
val dynamic_trust: unit -> trust

val make_trusted: trust -> trust
val make_private: trust -> trust

val is_public: trust -> bool
val is_tainted: trust -> bool

val subtype_trust: trust -> trust -> bool
