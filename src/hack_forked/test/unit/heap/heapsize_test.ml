(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* heap_size only counts the size in the OCaml heap or young gen storage, but
   data created for this test will be statically allocated. This function will
   ensure that the values we create for this test are actually on the heap. *)
let clone x = Marshal.from_string (Marshal.to_string x []) 0

let test_heapsize_simple () =
  let wordsize = Sys.word_size / 8 in
  let pair = clone (1, 2) in
  (* Have to be careful: Caml shares constant pairs *)
  let nonshared = clone [(1, 2); (3, 4)] in
  let shared = clone [pair; pair] in
  let str1 = clone "0123456" in
  let str2 = clone "01234567" in
  (* We expect a two-element pair to use 3 words *)
  let c1 = SharedMem.debug_value_size (Obj.repr pair) / wordsize in
  (* We expect a two element list of two-element pairs to use 12 words *)
  let c2 = SharedMem.debug_value_size (Obj.repr nonshared) / wordsize in
  (* But if the pair is shared then only 9 words *)
  let c3 = SharedMem.debug_value_size (Obj.repr shared) / wordsize in
  (* We expect strings to use a word and then words filled with characters,
     including null termination *)
  let c4 = SharedMem.debug_value_size (Obj.repr str1) / wordsize in
  let c4expected = 2 + (String.length str1 / wordsize) in
  let c5 = SharedMem.debug_value_size (Obj.repr str2) / wordsize in
  let c5expected = 2 + (String.length str2 / wordsize) in
  Printf.printf "c1 = %d c2 = %d c3 = %d c4 = %d c5 = %d\n" c1 c2 c3 c4 c5;
  c1 == 3 && c2 == 12 && c3 == 9 && c4 == c4expected && c5 == c5expected

let tests = [("test_heapsize_simple", test_heapsize_simple)]

let () =
  (* this call might not return *)
  Daemon.check_entry_point ();

  Unit_test.run_all tests
