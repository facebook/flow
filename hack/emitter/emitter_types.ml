(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Formatting for type information *)

open Core
open Utils

module N = Nast
module T = Typing_defs
module C = Emitter_core

let fmt_prim x =
  match x with
  | N.Tvoid   -> "HH\\void"
  | N.Tint    -> "HH\\int"
  | N.Tbool   -> "HH\\bool"
  | N.Tfloat  -> "HH\\float"
  | N.Tstring -> "HH\\string"
  | N.Tnum    -> "HH\\num"
  | N.Tresource -> "HH\\resource"
  | N.Tarraykey -> "HH\\arraykey"
  | N.Tnoreturn -> "HH\\noreturn"

(* Produce the "userType" bit of the annotation *)
let rec fmt_hint (_, h) =
  match h with
  | N.Hany -> ""
  | N.Hmixed -> "HH\\mixed"
  | N.Hthis -> "HH\\this"
  | N.Hprim prim -> fmt_prim prim
  | N.Habstr (s, _) -> C.fmt_name s

  | N.Happly ((_, s), []) -> C.fmt_name s
  | N.Happly ((_, s), args) ->
    C.fmt_name s ^ "<" ^ String.concat ", " (List.map args fmt_hint) ^ ">"

  | N.Hfun (args, _, ret) ->
    "(function (" ^ String.concat ", " (List.map args fmt_hint) ^ "): " ^
      fmt_hint ret ^ ")"

  | N.Htuple hs ->
    "(" ^ String.concat ", " (List.map hs fmt_hint) ^ ")"

  | N.Haccess (h, accesses) ->
    fmt_hint h ^ "::" ^ String.concat "::" (List.map accesses snd)

  | N.Hoption t -> "?" ^ fmt_hint t

  | N.Harray (None, None) -> "array"
  | N.Harray (Some h, None) -> "array<" ^ fmt_hint h ^ ">"
  | N.Harray (Some h1, Some h2) ->
    "array<" ^ fmt_hint h1 ^ ", " ^ fmt_hint h2 ^ ">"
  | N.Harray _ -> C.bug "bogus array"
  (* We have to say Nast.Hshape instead of N.Hshape because otherwise
   * OCaml 4.01 reports a type error when trying to call
   * C.extract_shape_fields. asdf. *)
  | Nast.Hshape smap ->
    let fmt_field = function
      | N.SFlit (_, s) -> "'" ^ s ^ "'"
      | N.SFclass_const ((_, s1), (_, s2)) -> C.fmt_name s1 ^ "::" ^ s2
    in
    let shape_fields =
      List.map ~f:(fun (k, h) -> fmt_field k ^ "=>" ^ fmt_hint h)
        (C.extract_shape_fields smap) in
    "HH\\shape(" ^ String.concat ", " shape_fields ^ ")"


(* Given a type hint, break it up into an (optional) underlying
 * "vanilla" name and a set of flags which will be used to construct
 * a runtime TypeConstraint (hphp/runtime/vm/type-constraint.h).
 *
 * see determine_type_constraint{,from_annot} in
 * hphp/compiler/analysis/emitter.cpp and TypeAnnotation::vanillaName in
 * hphp/compiler/type_annotation.cpp.
 *
 * Type variables and type constants produce an empty string instead of
 * no string as their name, since that it what emitter.cpp does; this is
 * silly, probably.
 *
 * XXX: determine_type_constraint_from_annot has logic to ignore extended
 * hints for parameters (but not returns) when the constraint doesn't have
 * any of {type_var, nullable, soft, type_constant} and also does not
 * have an extended name. I don't understand how this could happen and
 * it isn't implemented here.
 *)
let rec hint_info ~tparams (_, h) =
  match h with
  (* We don't care about mixed/fun/this constraints *)
  | N.Hany | N.Hmixed | N.Hfun (_, _, _) | N.Hthis
  | N.Hprim N.Tvoid -> (None, [])

  | N.Hprim prim -> Some (fmt_prim prim), ["hh_type"]

  | N.Haccess (_, _) -> Some "", ["hh_type"; "extended_hint"; "type_constant"]
  (* Need to differentiate between type params and classes *)
  | N.Habstr (s, _) | N.Happly ((_, s), _) ->
    if List.mem tparams s then
      Some "", ["hh_type"; "extended_hint"; "type_var"]
    else
      Some (C.fmt_name s), ["hh_type"]

  (* shapes and tuples are just arrays *)
  | N.Harray (_, _) | N.Hshape _ |  N.Htuple _ -> Some "array", ["hh_type"]

  | N.Hoption t ->
    let name, flags = hint_info ~tparams t in
    name, ["nullable"; "hh_type"; "extended_hint"] @ flags

let fmt_maybe_str = function
  | None -> "N"
  | Some s -> C.quote_str s


let fmt_hint_inner ~tparams ~always_extended t =
    let tparams = List.map ~f:(fun (_, (_, s), _) -> s) tparams in

    let name, flags = hint_info tparams t in
    (* uniqify the flags *)
    let flags = List.dedup
      (flags @ C.bool_option "extended_hint" always_extended) in
    fmt_maybe_str name ^ " " ^ String.concat " " flags


(* Format a type hint for assembly output when full type-info is wanted.
 * (TypeConstraint+"userType"). tparams is so that we can
 * distinguish between type variables and class names. We *could* try
 * get this information from Typing_decl, since it has been computed,
 * but to be honest it is simpler to just do it ourselves. *)
let fmt_hint_info ~tparams ~always_extended = function
  | None -> ""
  | Some t ->
    let core = fmt_hint_inner ~tparams ~always_extended t in
    let user_type = C.quote_str (fmt_hint t) in
    "<" ^ user_type ^ " " ^ core ^ "> "

(* Format a hint that only has the constraint part *)
let fmt_hint_constraint ~tparams ~always_extended t =
  let core = fmt_hint_inner ~tparams ~always_extended t in
  "<" ^ core ^ ">"
