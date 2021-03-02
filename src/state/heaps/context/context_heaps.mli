(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type READER = sig
  type reader

  val find_sig : reader:reader -> File_key.t -> Context.sig_t

  val find_leader : reader:reader -> File_key.t -> File_key.t

  val sig_hash_opt : reader:reader -> File_key.t -> Xx.hash option
end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val sig_hash_changed : reader:reader -> File_key.t -> bool

  val sig_cx_mem_old : reader:reader -> File_key.t -> bool
end

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

module Init_master_context_mutator : sig
  val add_master_sig : (Context.t -> unit) Expensive.t
end

module Merge_context_mutator : sig
  type master_mutator

  type worker_mutator

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val add_merge_on_diff :
    (worker_mutator -> Context.t -> File_key.t Nel.t -> Xx.hash -> bool) Expensive.t

  val add_merge_on_exn :
    (options:Options.t -> worker_mutator -> File_key.t Nel.t -> bool) Expensive.t

  val revive_files : master_mutator -> Utils_js.FilenameSet.t -> unit
end

module From_saved_state : sig
  val add_sig_hash : File_key.t -> Xx.hash -> unit
end
