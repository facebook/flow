(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type audit

val ok : audit

val warn : audit

type 'a t = audit:audit -> 'a

val wrap : 'a -> 'a t
