(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module IntVal = struct
  type t = int

  let prefix = Prefix.make ()

  let description = "IntVal"
end

let test_add_remove
    (module IntHeap : SharedMem.NoCache with type key = string and type value = int) () =
  IntHeap.add "a" 4;
  assert (IntHeap.mem "a");
  IntHeap.remove_batch (IntHeap.KeySet.singleton "a");
  assert (not (IntHeap.mem "a"))

module TestNoCache = SharedMem.NoCache (StringKey) (IntVal)

let tests () =
  let list = [("test_add_remove", test_add_remove (module TestNoCache))] in
  let setup_test (name, test) =
    ( name,
      fun () ->
        let num_workers = 0 in
        let handle =
          SharedMem.init
            ~num_workers
            { SharedMem.heap_size = 1024; hash_table_pow = 3; log_level = 0 }
        in
        ignore (handle : SharedMem.handle);
        test ();
        true )
  in
  List.map setup_test list

let () = Unit_test.run_all (tests ())
