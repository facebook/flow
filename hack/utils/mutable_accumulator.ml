(*
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type 'a t = 'a list ref

let create () = ref []

let add t s = t := s :: !t

let segments t = List.rev !t
