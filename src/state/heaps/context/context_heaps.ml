(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module MasterContextHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = Context.master_context

      let description = "MasterContext"
    end)

let master_cx_ref : Context.master_context option ref = ref None

let add_master ~audit master_cx =
  WorkerCancel.with_no_cancellations (fun () ->
      master_cx_ref := None;
      let master_context =
        { Context.master_sig_cx = Context.sig_cx master_cx; builtins = Context.builtins master_cx }
      in
      (Expensive.wrap MasterContextHeap.add) ~audit File_key.Builtins master_context
  )

module Init_master_context_mutator : sig
  val add_master : (Context.t -> unit) Expensive.t
end = struct
  let add_master ~audit cx = add_master ~audit cx
end

module type READER = sig
  type reader

  val find_master : reader:reader -> Context.master_context
end

let find_master ~reader:_ =
  match !master_cx_ref with
  | Some master_cx -> master_cx
  | None ->
    begin
      match MasterContextHeap.get File_key.Builtins with
      | Some master_cx ->
        master_cx_ref := Some master_cx;
        master_cx
      | None -> raise (Key_not_found ("MasterContextHeap", "master context"))
    end

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
