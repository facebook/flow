(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

{
open Semver_parser
}

let number = '0'|['1'-'9'](['0'-'9'])*
let identifier_start = ['0'-'9' 'A'-'Z' 'a'-'z']
let identifier = identifier_start (identifier_start | '_')*

rule token = parse
  [' ' '\t'] {
    (* skip blanks *)
    token lexbuf
  }
| number as nr {
    NR nr
  }
| identifier as id {
    ID id
  }
| '-' { HYPHEN }
| '+' { PLUS }
| '.' { DOT }
| '<' { LT }
| '<''=' { LTE }
| '>' { GT }
| '>''=' { GTE }
| '=' { EQ }
| '^' { CARET }
| eof { EOF }
