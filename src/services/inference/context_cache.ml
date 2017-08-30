(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(****************** shared context heap *********************)

module SigContextHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
  type t = Context.cacheable_t
  let prefix = Prefix.make()
  let description = "SigContext"
end)

let add_sig_context = Expensive.wrap (fun file cx -> SigContextHeap.add file (Context.to_cache cx))

let add_sig ~audit cx =
  let cx_file = Context.file cx in
  add_sig_context ~audit cx_file cx

let find_sig ~options file =
  match SigContextHeap.get file with
  | Some cx -> Context.from_cache ~options cx
  | None -> raise (Key_not_found ("SigContextHeap", string_of_filename file))

module SigHashHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
  type t = SigHash.t
  let prefix = Prefix.make()
  let description = "SigHash"
end)

module LeaderHeap = SharedMem_js.WithCache (Loc.FilenameKey) (struct
  type t = filename
  let prefix = Prefix.make()
  let description = "Leader"
end)

let find_leader file =
  match LeaderHeap.get file with
  | Some leader -> leader
  | None -> raise (Key_not_found ("LeaderHeap", (string_of_filename file)))

(* While merging, we must keep LeaderHeap, SigContextHeap, and SigHashHeap in
   sync, sometimes creating new entries and sometimes reusing old entries. *)

(* Add a sig only if it has not changed meaningfully, and return the result of
   that check. *)
let add_merge_on_diff ~audit leader_cx component_files md5 =
  let leader_f = Context.file leader_cx in
  let diff = match SigHashHeap.get_old leader_f with
    | Some md5_old -> Loc.check_suffix leader_f Files.flow_ext || md5 <> md5_old
    | None -> true in
  if diff then begin
    List.iter (fun f -> LeaderHeap.add f leader_f) component_files;
    add_sig_context ~audit leader_f leader_cx;
    SigHashHeap.add leader_f md5;
  end;
  diff

let oldify_merge_batch files =
  LeaderHeap.oldify_batch files;
  SigContextHeap.oldify_batch files;
  SigHashHeap.oldify_batch files

let remove_old_merge_batch files =
  LeaderHeap.remove_old_batch files;
  SigContextHeap.remove_old_batch files;
  SigHashHeap.remove_old_batch files

let revive_merge_batch files =
  LeaderHeap.revive_batch files;
  SigContextHeap.revive_batch files;
  SigHashHeap.revive_batch files
