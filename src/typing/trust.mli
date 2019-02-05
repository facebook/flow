(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type trust

val bogus_trust: unit -> trust

val make_trusted: trust -> trust
val make_private: trust -> trust
