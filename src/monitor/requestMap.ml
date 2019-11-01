(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The Flow server monitor sends requests to the server. When the server responds, we need to know
 * which client to forward the response to. The RequestMap keeps track of this info.
 *
 * Every request in this map has been sent to the server and no reply has been processed yet *)

let mutex = Lwt_mutex.create ()

let map = ref SMap.empty

let last_id = ref 0

let add ~request ~client =
  (* TODO(ljw): doesn't really need mutexes since it doesn't yield *)
  Lwt_mutex.with_lock mutex (fun () ->
      incr last_id;
      let request_id = Printf.sprintf "Request %d" !last_id in
      map := SMap.add request_id (request, client) !map;
      Lwt.return request_id)

let remove ~request_id =
  (* TODO(ljw): doesn't really need mutexes since it doesn't yield *)
  Lwt_mutex.with_lock mutex (fun () ->
      let ret = SMap.get request_id !map in
      map := SMap.remove request_id !map;
      Lwt.return ret)

let remove_all () =
  (* TODO(ljw): doesn't really need mutexes since it doesn't yield *)
  Lwt_mutex.with_lock mutex (fun () ->
      let ret = SMap.elements !map |> Base.List.map ~f:snd in
      map := SMap.empty;
      Lwt.return ret)

let cardinal () = SMap.cardinal !map
