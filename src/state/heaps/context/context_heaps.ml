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
  SharedMem.WithCache
    (Unit_key)
    (struct
      type t = Context.master_context

      let description = "MasterContext"
    end)

let add_master ~audit master_cx =
  WorkerCancel.with_no_cancellations (fun () ->
      (Expensive.wrap MasterContextHeap.add) ~audit () master_cx
  )

module Init_master_context_mutator : sig
  val add_master : (Context.master_context -> unit) Expensive.t
end = struct
  let add_master ~audit cx = add_master ~audit cx
end

module type READER = sig
  type reader

  val find_master : reader:reader -> Context.master_context
end

let find_master ~reader:_ =
  match MasterContextHeap.get () with
  | Some master_cx -> master_cx
  | None -> raise (Key_not_found ("MasterContextHeap", "master context"))

module Mutator_reader = struct
  type reader = Mutator_state_reader.t

  let find_master ~reader = find_master ~reader
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let find_master ~reader = find_master ~reader
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let find_master ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.find_master ~reader
    | State_reader reader -> Reader.find_master ~reader
end
