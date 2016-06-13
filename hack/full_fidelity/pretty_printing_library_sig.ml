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

(* document type used during construction of multiple layout alternatives *)
type doc =
  | Nil
  | Cons of doc * doc
  | Nest of int * doc
  | Text of string
  | Break of string
  | MustBreak
  (** [Group x] represent two alternative layouts of [x]:
    * vertical layout is where Breaks are considered newline in the group
    * horizontal layout is where Breaks are considered spaces in the group
    * This decision is made independently in subgroups
    * This implicity definition allows docs to be constructed in a linear
    * fashion, and the better layout is dynamically generated and selected *)
  | Group of doc

(* represents one particular layout, used for printing *)
type layout =
  | LNil
  | LText of string * layout
  | LLine of int * layout

(* docs are lazily evaluated *)
module type Library = sig

  (* document type as specified in prettier printer paper *)
  type t = doc

  val nil : t (* the unit document *)

  val cons : t -> t -> t (* put two DOC horizontally *)

  (* adopted notation from Lindig paper Strictly Pretty *)
  val (^^) : t -> t -> t (* same as cons *)

  val nest : int -> t -> t (* DOC with i amount of indentation *)

  val text : string -> t (* creates a DOC that contains string s *)

  val break : t
  (* a "linebreak" followed by a string.
   * Note that line breaks here can be turned into space *)
  val breakwith : string -> t

  val must_break : t

  val group : t -> t (* create choices of t *)

  (* TODO should string be some custom data type? *)
  (* [pretty k doc] pretty prints doc assuming an initial indentation of
   * [k]. [k] is useful when pretty printing partial trees *)
  val pretty : int -> t -> string

  val dump : t -> string

end

(* DOCCOMPARE uses some criteria to make decision.
 * For now the configuration is global: i.e. there are some parameters that
 * the comparator is initialized with, and the comparator makes decision
 * using them along with any information carried in type doc.
 * A alternative would be to let documents carry with them more context
 * and the comparator uses these context, instead of a global config to
 * compare documents *)
module type DocCompare = sig

  type t = doc

  (* from the choices, select best layout, considering initial indent [k] *)
  val best : int -> t -> layout

end
