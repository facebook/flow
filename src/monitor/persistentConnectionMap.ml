(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module keeps track of the persistent connections. Since the Flow server monitor is designed
 * to be super thin, the Flow server manages most of the persistent connection state. All the
 * monitor really needs to do is keep track of which connection goes with which ID *)

let mutex = Lwt_mutex.create ()
let map = ref IMap.empty

let add ~client_id ~client =
  (* TODO(ljw): doesn't really need mutexes since it doesn't yield *)
  Lwt_mutex.with_lock mutex (fun () ->
    map := IMap.add client_id client !map;
    Lwt.return_unit
  )

let get ~client_id =
  (* TODO(ljw): doesn't really need mutexes since it doesn't yield *)
  Lwt_mutex.with_lock mutex (fun () ->
    Lwt.return (IMap.get client_id !map)
  )

let remove ~client_id =
  map := IMap.remove client_id !map

let cardinal () =
  IMap.cardinal !map
