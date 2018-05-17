(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val iter_all: unit Lwt.t list -> unit Lwt.t

val all: 'a Lwt.t list -> 'a list Lwt.t
