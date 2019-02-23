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

rule token = parse
  [' ' '\t'] {
    (* skip blanks *)
    token lexbuf
  }
| number as nr {
    NR nr
  }
| '.' { DOT }
| '<' { LT }
| '<''=' { LTE }
| '>' { GT }
| '>''=' { GTE }
| '=' { EQ }
| '^' { CARET }
| eof { EOF }
