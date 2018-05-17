(**
 * Copyright (c) 2018-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Lwt.join is a great way to run multiple threads in parallel. However it has this really annoying
 * property where it won't exit early if one of the threads fails. It's not a big deal if you
 * expect this behavior, but it can be dangerous if you expect the same behavior as Promise.all or
 * Hack's await.
 *
 * We can instead simulate how Lwt.join should work by calling Lwt.nchoose multiple times until
 * one thread throws an exception or until all threads have finished.
 *
 * In the exceptional case, we won't cancel the still-sleeping threads. I (glevi) tried to get this
 * to work, but it wouldn't preserve stack traces. Anyway, Promise.all doesn't cancel running
 * promises either :P
 *)
let rec iter_all threads =
  if threads = []
  then Lwt.return_unit
  else
    (* If any thread in threads fails during this nchoose, the whole all function will fail *)
    let%lwt _, sleeping_threads = Lwt.nchoose_split threads in
    iter_all sleeping_threads

let get_value_unsafe thread = match Lwt.state thread with
  | Lwt.Return x -> x
  | _ -> failwith "Not yet completed"

let all threads =
  let%lwt () = iter_all threads in
  threads
  |> List.map get_value_unsafe
  |> Lwt.return
