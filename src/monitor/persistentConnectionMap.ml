(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module keeps track of the persistent connections. Since the Flow server monitor is designed
 * to be super thin, the Flow server manages most of the persistent connection state. All the
 * monitor really needs to do is keep track of which connection goes with which ID *)

let map = ref IMap.empty

let add ~client_id ~client = map := IMap.add client_id client !map

let get ~client_id = IMap.find_opt client_id !map

let remove ~client_id = map := IMap.remove client_id !map

let cardinal () = IMap.cardinal !map

let get_all_clients () = IMap.bindings !map |> Base.List.map ~f:snd
