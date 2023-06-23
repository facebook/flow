(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val empty : t

val stems : t -> File_path.t list

val add : t -> File_path.t -> t

val matches : t -> string -> bool
