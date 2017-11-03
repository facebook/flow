(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(****************** shared context heap *********************)

module SigContextHeap = SharedMem_js.WithCache (File_key) (struct
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
  | None -> raise (Key_not_found ("SigContextHeap", File_key.to_string file))

module SigHashHeap = SharedMem_js.WithCache (File_key) (struct
  type t = Xx.hash
  let prefix = Prefix.make()
  let description = "SigHash"
end)

module LeaderHeap = SharedMem_js.WithCache (File_key) (struct
  type t = File_key.t
  let prefix = Prefix.make()
  let description = "Leader"
end)

let find_leader file =
  match LeaderHeap.get file with
  | Some leader -> leader
  | None -> raise (Key_not_found ("LeaderHeap", (File_key.to_string file)))

(* While merging, we must keep LeaderHeap, SigContextHeap, and SigHashHeap in
   sync, sometimes creating new entries and sometimes reusing old entries. *)

(* Add a sig only if it has not changed meaningfully, and return the result of
   that check. *)
let add_merge_on_diff ~audit leader_cx component_files xx =
  let leader_f = Context.file leader_cx in
  let diff = match SigHashHeap.get_old leader_f with
    | Some xx_old -> File_key.check_suffix leader_f Files.flow_ext || xx <> xx_old
    | None -> true in
  if diff then begin
    List.iter (fun f -> LeaderHeap.add f leader_f) component_files;
    add_sig_context ~audit leader_f leader_cx;
    SigHashHeap.add leader_f xx;
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
