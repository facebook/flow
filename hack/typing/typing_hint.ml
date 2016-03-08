(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Converts a type hint into a type  *)
(*****************************************************************************)
open Core
open Nast
open Typing_defs

module DeclEnv = Typing_decl_env

(* Unpacking a hint for typing *)

let rec hint env (p, h) =
  let h = hint_ p env h in
  Typing_reason.Rhint p, h

and hint_ p env = function
  | Hany -> Tany
  | Hmixed -> Tmixed
  | Hthis -> Tthis
  | Harray (h1, h2) ->
    if env.DeclEnv.mode = FileInfo.Mstrict && h1 = None
    then Errors.generic_array_strict p;
    let h1 = Option.map h1 (hint env) in
    let h2 = Option.map h2 (hint env) in
    Tarray (h1, h2)
  | Hprim p -> Tprim p
  | Habstr (x, cstr_opt) ->
    let cstr_opt = match cstr_opt with
      | Some (ck, h) ->
        let h = hint env h in
        Some (ck, h)
      | None -> None in
    Tgeneric (x, cstr_opt)
  | Hoption (_, Hprim Tvoid) ->
    Errors.option_return_only_typehint p `void;
    Tany
  | Hoption (_, Hprim Tnoreturn) ->
    Errors.option_return_only_typehint p `noreturn;
    Tany
  | Hoption (_, Hmixed) ->
    Errors.option_mixed p;
    Tany
  | Hoption h ->
    let h = hint env h in
    Toption h
  | Hfun (hl, b, h) ->
    let paraml = List.map hl (hint env) in
    let paraml = List.map paraml (fun x -> None, x) in
    let ret = hint env h in
    let arity_min = List.length paraml in
    let arity = if b
      then Fellipsis arity_min
      else Fstandard (arity_min, arity_min)
    in
    Tfun {
      ft_pos = p;
      ft_deprecated = None;
      ft_abstract = false;
      ft_arity = arity;
      ft_tparams = [];
      ft_params = paraml;
      ft_ret = ret;
    }
  | Happly ((p, "\\Tuple"), _)
  | Happly ((p, "\\tuple"), _) ->
    Errors.tuple_syntax p;
    Tany
  | Happly (((_p, c) as id), argl) ->
    Typing_hooks.dispatch_class_id_hook id None;
    DeclEnv.add_wclass env c;
    let argl = List.map argl (hint env) in
    Tapply (id, argl)
  | Haccess (root_ty, ids) ->
    let root_ty = hint env root_ty in
    Taccess (root_ty, ids)
  | Htuple hl ->
    let tyl = List.map hl (hint env) in
    Ttuple tyl
  | Hshape fdm ->
    let fdm = ShapeMap.map (hint env) fdm in
    (* Fields are only partially known, because this shape type comes from
     * type hint - shapes that contain listed fields can be passed here, but
     * due to structural subtyping they can also contain other fields, that we
     * don't know about. *)
    Tshape (FieldsPartiallyKnown ShapeMap.empty, fdm)
