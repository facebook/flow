(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* timeout.ml is not very Lwt friendly, and the various timeouts that come with Lwt don't quite do
 * what we want:
 * - Lwt_timeout provides a low-level interface that allows callbacks to be registered to be called
 *   at a later time (analogous to setTimeout in JS).
 * - Lwt_unix.with_timeout raises an exception when the timeout is reached, but we would often
 *   rather have errors explicitly propagated with `result`.
 * - The timeouts that come along with the Lwt_process module just kill the process but don't
 *   otherwise provide an indication that a timeout was reached.
 *)

let with_timeout ?timeout_msg ?on_timeout timeout f =
  Lwt.pick
    [
      f ();
      (let%lwt () = Lwt_unix.sleep timeout in
       let timeout_msg = Base.Option.value timeout_msg ~default:"Operation timed out" in
       let%lwt () =
         match on_timeout with
         | None -> Lwt.return_unit
         | Some on_timeout -> on_timeout ()
       in
       Lwt.return (Error timeout_msg)
      );
    ]
