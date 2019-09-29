(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t

val stems : t -> Path.t list

val add : t -> Path.t -> t

val matches : t -> string -> bool
