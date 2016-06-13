(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* implementation of pretty printing library based on Philip Wadler's paper
 * titled "A Prettier Printer", and the strict ocaml implementation based on
 * Christian Lindig's paper "Strictly Pretty" *)
open Pretty_printing_library_sig
module type LineSpec = sig
  val line_width : int
end
(* comparator using line width as constraint *)
module WidthConstrainedDocComparator (C : LineSpec) : DocCompare = struct
  type t = doc
  type mode =
  | Flat
  | Vert
  let strlen = String.length
  let line_width = C.line_width

  (* implementation as specified in the Prettier Printer paper *)
  (* width is the constraint, and k is the number of chars already occupied *)
  let rec format width k = function
  | [] -> LNil
  | (i, m,    Nil)        :: tl -> format width k tl
  | (i, m,    Cons(x, y)) :: tl -> format width k ((i, m, x) :: (i, m, y) :: tl)
  | (i, m,    Nest(j, x)) :: tl -> format width k ((i + j, m, x) :: tl)
  | (i, m,    Text s)     :: tl -> LText (s, format width (k + strlen s) tl)
  | (i, Flat, Break s)    :: tl -> LText (s, format width (k + strlen s) tl)
  | (i, Vert, Break s)    :: tl -> LLine (i, format width i tl)
  | (i, m,    MustBreak)  :: tl -> LLine (i, format width i tl)
  (* "lazy evaluation". We expand group only at here.
   * Note also that only one of if and else will be executed *)
  | (i, m,    Group x)    :: tl ->
    if   fits (width - k) ((i, Flat, x)::tl) && not (must_break x)
    then format width k   ((i, Flat, x)::tl)
    else format width k   ((i, Vert, x)::tl)
  (* recursively check that the subgroup fits in the given width
   * "Fit" is checked by seeing that the document being expanded
   * horizontally can stay in one line within the width limit, unitil
   * a known newline is seen *)
  and fits w = function
    | _ when w < 0 -> false
    | [] -> true
    | (i, m,    Nil)         :: tl -> fits w tl
    | (i, m,    Cons (x, y)) :: tl -> fits w ((i,m,x)::(i,m,y)::tl)
    | (i, m,    Nest (j, x)) :: tl -> fits w ((i+j,m,x)::tl)
    | (i, m,    Text s)      :: tl -> fits (w - strlen s) tl
    | (i, Flat, Break s)     :: tl -> fits (w - strlen s) tl
    | (i, Vert, Break _)     :: tl -> true
    | (i, m,    MustBreak)   :: tl -> true
    (* See flatten in Wadler's paper. Entire document is flattened *)
    | (i, m,    Group x)     :: tl -> fits w ((i,Flat,x)::tl)
  (* returns true if the doc expands to contain a Mustbreak. If it does, then
   * we know that all breaks in this part of the doc have to be newlines *)
  and must_break x =
    let rec aux_must_break = function
    | [] -> false
    | Nil         :: tl -> aux_must_break tl
    | Cons (x, y) :: tl -> aux_must_break (x :: y :: tl)
    | Nest (j, x) :: tl -> aux_must_break (x :: tl)
    | Text s      :: tl -> aux_must_break tl
    | Break s     :: tl -> aux_must_break tl
    | MustBreak   :: tl -> true
    | Group x     :: tl -> aux_must_break (x :: tl)
  in
  aux_must_break [x]

  let best k doc = format line_width k [(0, Flat, Group doc)]
end
