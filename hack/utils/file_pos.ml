(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}

let compare = Pervasives.compare

let dummy = {
  pos_lnum = 0;
  pos_bol = 0;
  pos_cnum = -1;
}

let is_dummy t = (t = dummy)

let beg_of_file = {
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

(* constructors *)

let of_line_column_offset ~line ~column ~offset = {
  pos_lnum = line;
  pos_bol = offset - column;
  pos_cnum = offset;
}

let of_lexing_pos lp = {
  pos_lnum = lp.Lexing.pos_lnum;
  pos_bol = lp.Lexing.pos_bol;
  pos_cnum = lp.Lexing.pos_cnum;
}

(* accessors *)

let offset t = t.pos_cnum
let line t = t.pos_lnum
let column t = t.pos_cnum - t.pos_bol
let beg_of_line t = t.pos_bol

let line_beg t = t.pos_lnum, t.pos_bol

let line_column t = t.pos_lnum, t.pos_cnum - t.pos_bol

let line_column_beg t = t.pos_lnum, t.pos_cnum - t.pos_bol, t.pos_bol

let line_column_offset t = t.pos_lnum, t.pos_cnum - t.pos_bol, t.pos_cnum

let line_beg_offset t = t.pos_lnum, t.pos_bol, t.pos_cnum

let to_lexing_pos pos_fname t = {
  Lexing.pos_fname;
  Lexing.pos_lnum = t.pos_lnum;
  Lexing.pos_bol = t.pos_bol;
  Lexing.pos_cnum = t.pos_cnum;
}
