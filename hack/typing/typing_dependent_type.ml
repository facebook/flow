(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Typing_defs

module ExprDepTy = struct
  module N = Nast

  let to_string dt = AbstractKind.to_string (AKdependent dt)

  (****************************************************************************)
  (* A type access "this::T" is translated to "<this>::T" during the
   * naming phase. While typing a body, "<this>" is a type hole that needs to
   * be filled with a final concrete type. Resolution is specified in typing.ml,
   * here is a high level break down:
   *
   * 1) When a class member "bar" is accessed via "[CID]->bar" or "[CID]::bar"
   * we resolves "<this>" in the type of "bar" to "<[CID]>"
   *
   * 2) When typing a method, we resolve "<this>" in the return type to
   * "this"
   *
   * 3) When typing a method, we resolve "<this>" in parameters of the
   * function to "<static>" in static methods or "<$this>" in non-static
   * methods
   *
   * More specific details are explained inline
   *)
  (****************************************************************************)
  let make cid cid_ty =
    let pos = Reason.to_pos (fst cid_ty) in
    let tag =
      match cid with
      | N.CIparent | N.CIself | N.CI _ -> None
      | N.CIstatic -> Some (pos, `static)
      | N.CIvar (p, N.This) -> Some (p, `static)
      (* For almost all expressions we generate a new identifier. In the future,
       * we might be able to do some local analysis to determine if two given
       * expressions refer to the same Late Static Bound Type, but for now we do
       * this since it is easy and sound.
       *)
      | N.CIvar (p, _) -> Some (p, `expr (Ident.tmp())) in
    match tag with
    | None -> cid_ty
    | Some (p, d) ->
        Reason.Rwitness p, Tabstract (AKdependent (d, []), Some cid_ty)
end
