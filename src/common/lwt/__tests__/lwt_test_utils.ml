(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2

(* Like `>::` except it expects the function to return `unit Lwt.t` rather than `unit` *)
let ( %>:: ) name f = name >:: fun ctxt -> LwtInit.run_lwt (fun () -> f ctxt)
