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
  method on_tarray : 'a -> ty option -> ty option -> 'a
  method on_tgeneric : 'a -> string -> ty option -> 'a
  method on_toption : 'a -> ty -> 'a
  method on_tprim : 'a -> Nast.tprim -> 'a
  method on_tvar : 'a -> Ident.t -> 'a
  method on_type : 'a -> ty -> 'a
  method on_tfun : 'a -> fun_type -> 'a
  method on_tabstract : 'a -> Nast.sid -> ty list -> ty option -> 'a
  method on_tapply : 'a -> Nast.sid -> ty list -> 'a
  method on_ttuple : 'a -> ty list -> 'a
  method on_tanon : 'a -> fun_arity -> Ident.t -> 'a
  method on_tunresolved : 'a -> ty list -> 'a
  method on_tobject : 'a -> 'a
  method on_tshape : 'a -> ty Nast.ShapeMap.t -> 'a
  method on_taccess : 'a -> static_class_id -> Nast.sid -> Nast.sid list -> 'a
end

class virtual ['a] type_visitor : ['a] type_visitor_type = object(this)
  method on_tany acc = acc
  method on_tmixed acc = acc
  method on_tarray acc ty1_opt ty2_opt =
    let acc = opt_fold_left this#on_type acc ty1_opt in
    let acc = opt_fold_left this#on_type acc ty2_opt in
    acc
  method on_tgeneric acc _ ty_opt = opt_fold_left this#on_type acc ty_opt
  method on_toption acc ty = this#on_type acc ty
  method on_tprim acc _ = acc
  method on_tvar acc _ = acc
  method on_tfun acc {ft_params; ft_tparams; ft_ret; _} =
    let acc = List.fold_left this#on_type acc (List.map snd ft_params) in
    let tparams = List.map thd3 ft_tparams in
    let acc = List.fold_left (opt_fold_left this#on_type) acc tparams in
    this#on_type acc ft_ret
  method on_tabstract acc _ tyl ty_opt =
    let acc = List.fold_left this#on_type acc tyl in
    let acc = opt_fold_left this#on_type acc ty_opt in
    acc
  method on_tapply acc _ tyl = List.fold_left this#on_type acc tyl
  method on_taccess acc _ _id _ids = acc
  method on_ttuple acc tyl = List.fold_left this#on_type acc tyl
  method on_tanon acc _ _ = acc
  method on_tunresolved acc tyl = List.fold_left this#on_type acc tyl
  method on_tobject acc = acc
  method on_tshape acc fdm =
    let f _ v acc = this#on_type acc v in
    Nast.ShapeMap.fold f fdm acc
  method on_type acc = function
    | _, Tany -> this#on_tany acc
    | _, Tmixed -> this#on_tmixed acc
    | _, Tarray (ty1_opt, ty2_opt) ->
      this#on_tarray acc ty1_opt ty2_opt
    | _, Tgeneric (s, ty_opt) -> this#on_tgeneric acc s ty_opt
    | _, Toption ty -> this#on_toption acc ty
    | _, Tprim prim -> this#on_tprim acc prim
    | _, Tvar id -> this#on_tvar acc id
    | _, Tfun fty -> this#on_tfun acc fty
    | _, Tabstract (s, tyl, ty_opt) -> this#on_tabstract acc s tyl ty_opt
    | _, Tapply (s, tyl) -> this#on_tapply acc s tyl
    | _, Taccess (root, id, ids) -> this#on_taccess acc root id ids
    | _, Ttuple tyl -> this#on_ttuple acc tyl
    | _, Tanon (arity, id) -> this#on_tanon acc arity id
    | _, Tunresolved tyl -> this#on_tunresolved acc tyl
    | _, Tobject -> this#on_tobject acc
    | _, Tshape fdm -> this#on_tshape acc fdm
end
