(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
 * The Diff Heap is specialized to store the patches used by the Replacement_printer
 * when updating files that have been transformed by a worker process.
 *)
type patch = (int * int * string) list

type key = File_key.t

module DiffPatchHeap =
  SharedMem_js.NoCache (SharedMem_js.Immediate) (File_key)
    (struct
      type t = patch

      let prefix = Prefix.make ()

      let description = "DiffPatch"
    end)

let set_diff = Expensive.wrap DiffPatchHeap.add

let get_diff = DiffPatchHeap.get

let remove_batch = DiffPatchHeap.remove_batch
