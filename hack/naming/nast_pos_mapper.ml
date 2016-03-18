(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Nast

let rec expr f (p, e) =
  f p, expr_ f e

and expr_ f = function
  | Any -> Any
  | Array afl -> Array (List.map afl (afield f))
  | Shape sh -> Shape (shape f sh)
  | True -> True
  | False -> False
  | Int n -> Int (pstring f n)
  | Float n -> Float (pstring f n)
  | Null -> Null
  | String s -> String (pstring f s)
  | This -> This
  | Id sid -> Id (pstring f sid)
  | Lplaceholder pos -> Lplaceholder (f pos)
  | Dollardollar (p, id) -> Dollardollar (f p, id)
  | Lvar (p, id) -> Lvar (f p, id)
  | Fun_id sid -> Fun_id (pstring f sid)
  | Method_id (e, pstr) -> Method_id (expr f e, pstring f pstr)
  | Method_caller (sid, pstr) -> Method_caller (pstring f sid, pstring f pstr)
  | Smethod_id (sid, pstr) -> Smethod_id (pstring f sid, pstring f pstr)
  | Yield_break -> Yield_break
  | Yield e -> Yield (afield f e)
  | Await e -> Await (expr f e)
  | List el -> List (List.map el (expr f))
  | Assert (AE_assert ae) -> Assert (AE_assert (expr f ae))
  | Clone e -> Clone (expr f e)
  | Expr_list el -> Expr_list (List.map el (expr f))
  | Special_func sf -> Special_func (special_func f sf)
  | Obj_get (e1, e2, x) -> Obj_get (expr f e1, expr f e2, x)
  | Array_get (e1, e2) -> Array_get (expr f e1, Option.map e2 (expr f))
  | Class_get (cid, id) -> Class_get (class_id f cid, pstring f id)
  | Class_const (cid, id) -> Class_const (class_id f cid, pstring f id)
  | Call (ct, e, el, uel) ->
    Call (ct, expr f e, List.map el (expr f), List.map uel (expr f))
  | String2 el -> String2 (List.map el (expr f))
  | Pair (e1, e2) -> Pair (expr f e1, expr f e2)
  | Cast (h, e) -> Cast (hint f h, expr f e)
  | Unop (uop, e) -> Unop (uop, expr f e)
  | Binop (bop, e1, e2) -> Binop (bop, expr f e1, expr f e2)
  | Pipe ((p, id), e1, e2) -> Pipe ((f p, id), expr f e1, expr f e2)
  | Eif (e1, e2, e3) -> Eif (expr f e1, Option.map e2 (expr f), expr f e3)
  | NullCoalesce (e1, e2) -> NullCoalesce (expr f e1, expr f e2)
  | InstanceOf (e1, e2) -> InstanceOf (expr f e1, class_id f e2)
  | Typename n -> Typename (pstring f n)
  | New (cid, el, uel) ->
    New (class_id f cid, List.map el (expr f), List.map uel (expr f))
  | Efun (fun_, idl) ->
    (* properly handling this would involve writing the mapper for all
     * statements *)
    Errors.internal_error (fst fun_.f_name)
      "Nast_pos_mapper cannot handle lambdas";
    Efun (fun_, idl)
  | Xml (sid, attrl, el) ->
    Xml (pstring f sid, attr_list f attrl, List.map el (expr f))
  | ValCollection (s, el) -> ValCollection (s, List.map el (expr f))
  | KeyValCollection (s, fl) ->
    KeyValCollection (s, List.map fl (fun (e1, e2) -> expr f e1, expr f e2))

and afield f = function
  | AFvalue e -> AFvalue e (* (expr f e) *)
  | AFkvalue (e1, e2) ->
    let e1 : Nast.expr = expr f e1 in
    let e2 = expr f e2 in
    AFkvalue (e1, e2)

and shape f sm =
  ShapeMap.fold begin fun sf e acc ->
    let sf = shape_field f sf in
    let e = expr f e in
    ShapeMap.add sf e acc
  end sm ShapeMap.empty

and shape_field f = function
  | SFlit pstr -> SFlit (pstring f pstr)
  | SFclass_const (sid, pstr) -> SFclass_const (pstring f sid, pstring f pstr)

and pstring f (p, s) =
  f p, s

and special_func f = function
  | Gena e -> Gena (expr f e)
  | Genva el -> Genva (List.map el (expr f))
  | Gen_array_rec e -> Gen_array_rec (expr f e)

and class_id f = function
  | CIparent -> CIparent
  | CIself -> CIself
  | CIstatic -> CIstatic
  | CIexpr e -> CIexpr (expr f e)
  | CI sid -> CI (pstring f sid)

and hint f (p, h) = f p, hint_ f h

and hint_ f = function
  | Hany -> Hany
  | Hmixed -> Hmixed
  | Hthis -> Hthis
  | Htuple hl -> Htuple (List.map hl (hint f))
  | Habstr (s, cstr) ->
    let cstr = Option.map cstr begin fun (ck, h) ->
      ck, hint f h
    end in
    Habstr (s, cstr)
  | Harray (h1, h2) ->
    Harray (Option.map h1 (hint f), Option.map h2 (hint f))
  | Hprim tprim -> Hprim tprim
  | Hoption h -> Hoption (hint f h)
  | Hfun (hl, b, h) ->
    Hfun (List.map hl (hint f), b, hint f h)
  | Happly (sid, hl) -> Happly (pstring f sid, List.map hl (hint f))
  | Hshape sm ->
    let sm = ShapeMap.fold begin fun sf h acc ->
      let sf = shape_field f sf in
      let h = hint f h in
      ShapeMap.add sf h acc
    end sm ShapeMap.empty in
    Hshape sm
  | Haccess (h, sids) -> Haccess (hint f h, List.map sids (pstring f))

and attr_list f attrl =
  List.map attrl begin fun (pstr, e) ->
    pstring f pstr, expr f e
  end
