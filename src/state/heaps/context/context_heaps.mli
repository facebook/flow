(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_sig: File_key.t -> Context.sig_t

val find_leader: File_key.t -> File_key.t

val sig_hash_changed: File_key.t -> bool

module Init_master_context_mutator: sig
  val add_master_sig: (Context.t -> unit) Expensive.t
end

module Merge_context_mutator: sig
  type master_mutator
  type worker_mutator
  val create: Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator
  val add_merge_on_diff:
    (worker_mutator -> Context.t -> File_key.t Nel.t -> Xx.hash -> unit) Expensive.t
  val add_merge_on_exn:
    (worker_mutator -> options:Options.t -> File_key.t Nel.t -> unit) Expensive.t
  val revive_files: master_mutator -> Utils_js.FilenameSet.t -> unit
end
