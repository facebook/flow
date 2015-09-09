(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type parser_return = {
    file_mode  : FileInfo.mode option; (* None if PHP *)
    comments   : (Pos.t * string) list;
    ast        : Ast.program;
  }

val program : ?elaborate_namespaces:bool -> Relative_path.t ->
  string -> parser_return

(* Parses a file *)
val from_file : Relative_path.t -> parser_return

type saved_lb
type assoc
val save_lexbuf_state: Lexing.lexbuf -> saved_lb
val restore_lexbuf_state: Lexing.lexbuf -> saved_lb -> unit
val get_priority: Lexer_hack.token -> assoc * int
