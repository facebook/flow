(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type trust_rep

val bogus_trust : unit -> trust_rep

val literal_trust : unit -> trust_rep
