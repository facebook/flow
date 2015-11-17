(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Utils for hh_match tests
 *)

(* Print a code extent (lexing position pair) from the specified source
   along with a divider *)
val pretty_print_test_output :
      string -> File_pos.t * File_pos.t -> unit

(* Parse the specified unit test file into a list of files (allows us
   to fake multiple files in one file for when we need text, pattern and
   target files to test hh_match matching and patching)
   Return value is (dummy filename, content, parsed file) list *)
val parse_file : Relative_path.t ->
    (Relative_path.t * string * Parser_hack.parser_return) list
