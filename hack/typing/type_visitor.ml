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
open Typing_defs
module Reason = Typing_reason

class type ['a] type_visitor_type = object
  method on_tany : 'a -> Reason.t -> 'a
  method on_tmixed : 'a -> Reason.t -> 'a
  method on_tthis : 'a -> Reason.t -> 'a
  method on_tarray : 'a -> Reason.t -> 'b ty option -> 'b ty option -> 'a
  method on_tgeneric :
    'a -> Reason.t -> string -> (Ast.constraint_kind * decl ty) option -> 'a
  method on_toption : 'a -> Reason.t -> 'b ty -> 'a
  method on_tprim : 'a -> Reason.t -> Nast.tprim -> 'a
  method on_tvar : 'a -> Reason.t -> Ident.t -> 'a
  method on_type : 'a -> 'b ty -> 'a
  method on_tfun : 'a -> Reason.t -> 'b fun_type -> 'a
  method on_tabstract : 'a -> Reason.t -> abstract_kind -> locl ty option -> 'a
  method on_tapply : 'a -> Reason.t -> Nast.sid -> decl ty list -> 'a
  method on_ttuple : 'a -> Reason.t -> 'b ty list -> 'a
  method on_tanon : 'a -> Reason.t -> locl fun_arity -> Ident.t -> 'a
  method on_tunresolved : 'a -> Reason.t -> locl ty list -> 'a
  method on_tobject : 'a -> Reason.t -> 'a
  method on_tshape :
    'a -> Reason.t -> shape_fields_known -> 'b ty Nast.ShapeMap.t -> 'a
  method on_taccess : 'a -> Reason.t -> taccess_type -> 'a
  method on_tclass : 'a -> Reason.t -> Nast.sid -> locl ty list -> 'a
  method on_tarraykind : 'a -> Reason.t -> array_kind -> 'a
end

class virtual ['a] type_visitor : ['a] type_visitor_type = object(this)
  method on_tany acc _ = acc
  method on_tmixed acc _ = acc
  method on_tthis acc _ = acc
  method on_tarray: type a. _ -> Reason.t -> a ty option -> a ty option -> _ =
    fun acc _ ty1_opt ty2_opt ->
    let acc = Option.fold ~f:this#on_type ~init:acc ty1_opt in
    let acc = Option.fold ~f:this#on_type ~init:acc ty2_opt in
    acc
  method on_tgeneric acc _ _ cstr =
    Option.fold ~f:this#on_type ~init:acc (Option.map cstr snd)
  method on_toption: type a. _ -> Reason.t -> a ty -> _ =
    fun acc _ ty -> this#on_type acc ty
  method on_tprim acc _ _ = acc
  method on_tvar acc _ _ = acc
  method on_tfun: type a. _ -> Reason.t -> a fun_type -> _ =
    fun acc _ {ft_params; ft_tparams; ft_ret; _} ->
    let acc =
      List.fold_left ~f:this#on_type ~init:acc (List.map ft_params snd) in
    let tparams = List.map ft_tparams (fun (_, _, x) -> Option.map x snd) in
    let acc = List.fold_left tparams ~f:(fun acc tp ->
      Option.fold ~f:this#on_type ~init:acc tp) ~init:acc in
    this#on_type acc ft_ret
  method on_tabstract acc _ ak ty_opt =
    let acc =
      match ak with
      | AKnewtype (_, tyl) -> List.fold_left tyl ~f:this#on_type ~init:acc
      | AKenum _name -> acc
      | AKgeneric (_, super) ->
          Option.fold ~f:this#on_type ~init:acc super
      | AKdependent (_, _) -> acc in
    let acc = Option.fold ~f:this#on_type ~init:acc ty_opt in
    acc
  method on_tapply acc _ _ tyl = List.fold_left tyl ~f:this#on_type ~init:acc
  method on_taccess acc _ (root, _ids) = this#on_type acc root
  method on_ttuple: type a. _ -> Reason.t -> a ty list -> _ =
    fun acc _ tyl -> List.fold_left tyl ~f:this#on_type ~init:acc
  method on_tanon acc _ _ _ = acc
  method on_tunresolved acc _ tyl = List.fold_left tyl ~f:this#on_type ~init:acc
  method on_tobject acc _ = acc
  method on_tshape: type a. _ -> Reason.t -> shape_fields_known
    -> a ty Nast.ShapeMap.t -> _ =
    fun acc _ _ fdm ->
    let f _ v acc = this#on_type acc v in
    Nast.ShapeMap.fold f fdm acc
  method on_tclass acc _ _ tyl =
    List.fold_left tyl ~f:this#on_type ~init:acc
  method on_tarraykind acc _ array_kind =
    match array_kind with
    | AKany -> acc
    | AKempty -> acc
    | AKvec ty -> this#on_type acc ty
    | AKmap (tk, tv) ->
      let acc = this#on_type acc tk in
      this#on_type acc tv
    | AKshape fdm ->
      let f _ (tk, tv) acc = begin
        let acc = this#on_type acc tk in
        this#on_type acc tv
      end in
      Nast.ShapeMap.fold f fdm acc
    | AKtuple fields ->
      let f _ ty acc = this#on_type acc ty in
      Utils.IMap.fold f fields acc
  method on_type: type a. _ -> a ty -> _ = fun acc (r, x) ->
    match x with
    | Tany -> this#on_tany acc r
    | Tmixed -> this#on_tmixed acc r
    | Tthis -> this#on_tthis acc r
    | Tarray (ty1_opt, ty2_opt) ->
      this#on_tarray acc r ty1_opt ty2_opt
    | Tgeneric (s, cstr_opt) -> this#on_tgeneric acc r s cstr_opt
    | Toption ty -> this#on_toption acc r ty
    | Tprim prim -> this#on_tprim acc r prim
    | Tvar id -> this#on_tvar acc r id
    | Tfun fty -> this#on_tfun acc r fty
    | Tabstract (ak, ty_opt) -> this#on_tabstract acc r ak ty_opt
    | Tapply (s, tyl) -> this#on_tapply acc r s tyl
    | Taccess aty -> this#on_taccess acc r aty
    | Ttuple tyl -> this#on_ttuple acc r tyl
    | Tanon (arity, id) -> this#on_tanon acc r arity id
    | Tunresolved tyl -> this#on_tunresolved acc r tyl
    | Tobject -> this#on_tobject acc r
    | Tshape (fields_known, fdm) -> this#on_tshape acc r fields_known fdm
    | Tclass (cls, tyl) -> this#on_tclass acc r cls tyl
    | Tarraykind akind -> this#on_tarraykind acc r akind
end
