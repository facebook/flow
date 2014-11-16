(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(* Module maintaining which "mode" the lexer is in *)

type mode =
  | Toplevel
  | Class
  | ClassKw
  | Function
  | Expr
  | Attr
  | Xhp
  | Enum
  | UAttr
  | Initializer
  | Type

(* The type of the stack *)
type t

(* The empty environment *)
val empty: unit -> t

(* We are entering a class *)
val in_class: t -> unit

(* We are entering a typedef *)
val in_type: t -> unit

(* We are entering a user attribute *)
val in_uattr: t -> unit

(* We are entering an XHP enum definition *)
val in_enum: t -> unit

(* We are entering a function *)
val in_function: t -> unit

(* We are entering a block *)
val in_block: t -> unit

(* We are leaving a class *)
val out_block: t -> unit

(* We are leaving a user attribute *)
val out_uattr: t -> unit

(* Just saw the ';' character *)
val semi: t -> unit

(* Opening XHP tag *)
val otag: t -> unit

(* Just saw the '>' character *)
val gt: t -> unit

(* Just saw the /> *)
val slash_gt: t -> unit

(* closing XHP tag *)
val ctag: t -> unit

(* Returns true if we are in a class, but not in a method *)
val is_in_class: t -> bool

(* Return true if we are inside a user attribute *)
val is_in_uattr: t -> bool

(* Returns true if we are in xhp *)
val is_xhp: t -> bool

(* Returns true if we are somewhere where xhp is possible  *)
val could_be_xhp: t -> bool

(* Returns true if we're parsing XHP attributes right now *)
val below_attr: t -> bool

(* Returns the mode stack we are in *)
val get_modes: t -> mode list

(* Returns the mode we are in *)
val get_mode: t -> mode
