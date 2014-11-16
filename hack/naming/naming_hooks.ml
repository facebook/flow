(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let (hint_hooks: (Pos.t * string -> unit) list ref) = ref []

let (lvar_hooks: (Pos.t * string -> (Pos.t * Ident.t) SMap.t -> unit) list ref) = ref []

let attach_hint_hook hook =
  hint_hooks := hook :: !hint_hooks

let attach_lvar_hook hook =
  lvar_hooks := hook :: !lvar_hooks

let dispatch_hint_hook id =
  List.iter begin fun hook -> hook id end !hint_hooks

let dispatch_lvar_hook id locals =
  List.iter begin fun hook -> hook id locals end !lvar_hooks

let remove_all_hooks () =
  hint_hooks := [];
  lvar_hooks := [];
