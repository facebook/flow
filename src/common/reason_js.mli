(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Spider_monkey_ast (* Loc *)

type reason

val lexpos: string -> int -> int -> Lexing.position

(* reason constructors *)

val new_reason : string -> Pos.t -> reason
val mk_reason : string -> Spider_monkey_ast.Loc.t -> reason

(* ranges *)
(* TODO convert all range stuff to use single Ast loc *)
val pos_of_loc : Spider_monkey_ast.Loc.t -> Pos.t
val diff_range : Spider_monkey_ast.Loc.t -> int * int
val in_range : Pos.t -> Spider_monkey_ast.Loc.t -> bool

val string_of_pos : Pos.t -> string
val json_of_pos : Pos.t -> Hh_json.json

val reason_of_string : string -> reason

val is_internal_name : string -> bool
val internal_name : string -> string

val derivable_reason : reason -> reason
val is_derivable_reason : reason -> bool

(* used in builtins *)
val builtin_reason : string -> reason

val string_of_reason : reason -> string
val json_of_reason : reason -> Hh_json.json
val dump_reason : reason -> string

(* accessors *)
val loc_of_reason : reason -> Loc.t
val pos_of_reason : reason -> Pos.t

val desc_of_reason : reason -> string

(* simple way to get derived reasons whose descriptions are
   simple prefix-extensions of the original *)
val prefix_reason : string -> reason -> reason

(* simple way to get derived reasons whose descriptions are
   simple replacements of the original *)
val replace_reason : string -> reason -> reason

val repos_reason : Pos.t -> reason -> reason

val compare : reason -> reason -> int

val do_patch : string list -> (int * int * string) list -> string

val same_scope : reason -> reason -> bool
