(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* map from module name to filename *)
module NameHeap = SharedMem_js.WithCache (Modulename.Key) (struct
  type t = File_key.t
  let prefix = Prefix.make()
  let description = "Name"
  let use_sqlite_fallback () = false
end)

let get_file = Expensive.wrap NameHeap.get
let add_file = Expensive.wrap NameHeap.add
let module_exists = NameHeap.mem
let remove_file_batch = NameHeap.remove_batch

let get_file_unsafe ~audit m =
  match get_file ~audit m with
  | Some file -> file
  | None -> failwith
      (Printf.sprintf "file name not found for module %s" (Modulename.to_string m))
