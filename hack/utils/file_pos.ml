(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Lexing

type t = position

let compare = Pervasives.compare

let dummy = dummy_pos

let is_dummy t = (t = dummy)

let beg_of_file = {
  pos_fname = "";
  pos_lnum = 1;
  pos_bol = 0;
  pos_cnum = 0;
}

(* constructors *)

let of_line_column_offset ~line ~column ~offset = {
  pos_fname = "";
  pos_lnum = line;
  pos_bol = offset - column;
  pos_cnum = offset;
}

let of_lexing_pos lp = lp

(* accessors *)

let offset t = t.pos_cnum
let line t = t.pos_lnum
let column t = t.pos_cnum - t.pos_bol
let beg_of_line t = t.pos_bol

let line_beg t = t.pos_lnum, t.pos_bol

let line_column t = t.pos_lnum, t.pos_cnum - t.pos_bol

let line_column_beg t = t.pos_lnum, t.pos_cnum - t.pos_bol, t.pos_bol

let line_column_offset t = t.pos_lnum, t.pos_cnum - t.pos_bol, t.pos_cnum

let to_lexing_pos pos_fname t =
  { t with pos_fname }
