(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module Unit_key = struct
  type t = unit

  let to_string () = ""

  let compare () () = 0
end

module MasterContextHeap =
  SharedMem.NoCache
    (Unit_key)
    (struct
      type t = Context.master_context

      let description = "MasterContext"
    end)

let add_master master_cx =
  WorkerCancel.with_no_cancellations (fun () -> MasterContextHeap.add () master_cx)

let find_master () =
  match MasterContextHeap.get () with
  | Some master_cx -> master_cx
  | None -> raise (Key_not_found ("MasterContextHeap", "master context"))
