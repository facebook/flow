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

(********************* copied definition from mli files ***********************)
open Pretty_printing_library_sig

(******************************************************************************)

(* main functionality of pretty printing library *)
module Make (C : DocCompare) : Library = struct

  type t = Pretty_printing_library_sig.doc

  (* constructor functions *)
  let nil = Nil

  let cons x y =
    match x, y with
    | Nil, _ -> y
    | _, Nil -> x
    | _, _ -> Cons (x, y)

  let (^^) = cons

  let nest n x = if x= Nil then Nil else Nest (n, x)

  let text s = Text s

  let break = Break " "

  let breakwith s = Break s

  let must_break = MustBreak
  (* create alternatives of original layout and one line layout
   * Isolate corner cases to improve performance *)
  let group = function
  | Nil -> Nil
  | Group x -> Group x
  | x -> Group x


  (* observation functions *)
  let rec layout = function
  | LNil -> ""
  | LText (s, x) -> s ^ (layout x)
  | LLine (n, x) ->
    let newline_string = "\n" in
    let space_string = String.make n ' ' in
    (* TODO optimize string concat *)
    newline_string ^ space_string ^ (layout x)

  let pretty k x = layout (C.best k x)

  let dump doc =
    let rec dump_doc level =
      let indent = String.make level ' ' in
      function
      | Nil -> "nil"
      | Cons (x, y) -> Printf.sprintf "cons \n %s (%s, %s)" indent
            (dump_doc (level + 1) x) (dump_doc (level + 1) y)
      | Nest (i, x) -> Printf.sprintf "nest \n %s (%d, %s)" indent i
            (dump_doc (level + 1) x)
      | Text s -> s
      | Break _ -> "Break"
      | MustBreak -> "Must"
      | Group x -> Printf.sprintf "group \n %s (%s)"
          indent (dump_doc (level + 1) x)
    in
    dump_doc 0 doc
end
