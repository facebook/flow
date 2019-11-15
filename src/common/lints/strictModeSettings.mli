(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lints

type t

val empty : t

val fold : (lint_kind -> 'acc -> 'acc) -> t -> 'acc -> 'acc

val iter : (lint_kind -> unit) -> t -> unit

val of_lines : (int * string) list -> (t, int * string) result
