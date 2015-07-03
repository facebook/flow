(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs

class type ['a] type_visitor_type = object
  method on_tany : 'a -> 'a
  method on_tmixed : 'a -> 'a
  method on_tthis : 'a -> 'a
  method on_tarray : 'a -> 'b ty option -> 'b ty option -> 'a
  method on_tgeneric : 'a -> string -> (Ast.constraint_kind * decl ty) option -> 'a
  method on_toption : 'a -> 'b ty -> 'a
  method on_tprim : 'a -> Nast.tprim -> 'a
  method on_tvar : 'a -> Ident.t -> 'a
  method on_type : 'a -> 'b ty -> 'a
  method on_tfun : 'a -> 'b fun_type -> 'a
  method on_tabstract : 'a -> abstract_kind -> locl ty option -> 'a
  method on_tapply : 'a -> Nast.sid -> decl ty list -> 'a
  method on_ttuple : 'a -> 'b ty list -> 'a
  method on_tanon : 'a -> locl fun_arity -> Ident.t -> 'a
  method on_tunresolved : 'a -> locl ty list -> 'a
  method on_tobject : 'a -> 'a
  method on_tshape : 'a -> shape_fields_known -> 'b ty Nast.ShapeMap.t -> 'a
  method on_taccess : 'a -> taccess_type -> 'a
  method on_tclass : 'a -> Nast.sid -> locl ty list -> 'a
end

class virtual ['a] type_visitor : ['a] type_visitor_type = object(this)
  method on_tany acc = acc
  method on_tmixed acc = acc
  method on_tthis acc = acc
  method on_tarray: type a. _ -> a ty option -> a ty option -> _ =
    fun acc ty1_opt ty2_opt ->
    let acc = Option.fold ~f:this#on_type ~init:acc ty1_opt in
    let acc = Option.fold ~f:this#on_type ~init:acc ty2_opt in
    acc
  method on_tgeneric acc _ cstr =
    Option.fold ~f:this#on_type ~init:acc (opt_map snd cstr)
  method on_toption: type a. _ -> a ty -> _ =
    fun acc ty -> this#on_type acc ty
  method on_tprim acc _ = acc
  method on_tvar acc _ = acc
  method on_tfun: type a. _ -> a fun_type -> _ =
    fun acc {ft_params; ft_tparams; ft_ret; _} ->
    let acc = List.fold_left this#on_type acc (List.map snd ft_params) in
    let tparams = List.map (fun (_, _, x) -> opt_map snd x) ft_tparams in
    let acc = List.fold_left (fun acc tp ->
      Option.fold ~f:this#on_type ~init:acc tp) acc tparams in
    this#on_type acc ft_ret
  method on_tabstract acc ak ty_opt =
    let acc =
      match ak with
      | AKnewtype (_, tyl) -> List.fold_left this#on_type acc tyl
      | AKgeneric (_, super) ->
          Option.fold ~f:this#on_type ~init:acc super
      | AKdependent (_, _) -> acc in
    let acc = Option.fold ~f:this#on_type ~init:acc ty_opt in
    acc
  method on_tapply acc _ tyl = List.fold_left this#on_type acc tyl
  method on_taccess acc (root, _ids) = this#on_type acc root
  method on_ttuple: type a. _ -> a ty list -> _ =
    fun acc tyl -> List.fold_left this#on_type acc tyl
  method on_tanon acc _ _ = acc
  method on_tunresolved acc tyl = List.fold_left this#on_type acc tyl
  method on_tobject acc = acc
  method on_tshape: type a. _ -> shape_fields_known
    -> a ty Nast.ShapeMap.t -> _ =
    fun acc _ fdm ->
    let f _ v acc = this#on_type acc v in
    Nast.ShapeMap.fold f fdm acc
  method on_tclass acc _ tyl =
    List.fold_left this#on_type acc tyl
  method on_type: type a. _ -> a ty -> _ = fun acc x ->
    match x with
    | _, Tany -> this#on_tany acc
    | _, Tmixed -> this#on_tmixed acc
    | _, Tthis -> this#on_tthis acc
    | _, Tarray (ty1_opt, ty2_opt) ->
      this#on_tarray acc ty1_opt ty2_opt
    | _, Tgeneric (s, cstr_opt) -> this#on_tgeneric acc s cstr_opt
    | _, Toption ty -> this#on_toption acc ty
    | _, Tprim prim -> this#on_tprim acc prim
    | _, Tvar id -> this#on_tvar acc id
    | _, Tfun fty -> this#on_tfun acc fty
    | _, Tabstract (ak, ty_opt) -> this#on_tabstract acc ak ty_opt
    | _, Tapply (s, tyl) -> this#on_tapply acc s tyl
    | _, Taccess aty -> this#on_taccess acc aty
    | _, Ttuple tyl -> this#on_ttuple acc tyl
    | _, Tanon (arity, id) -> this#on_tanon acc arity id
    | _, Tunresolved tyl -> this#on_tunresolved acc tyl
    | _, Tobject -> this#on_tobject acc
    | _, Tshape (fields_known, fdm) -> this#on_tshape acc fields_known fdm
    | _, Tclass (cls, tyl) -> this#on_tclass acc cls tyl
end
