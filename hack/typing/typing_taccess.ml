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
open Utils

module TUtils = Typing_utils
module Reason = Typing_reason
module Env = Typing_env
module Inst = Typing_instantiate
module SN = Naming_special_names

(* During expansion we do not have to worry about the "self" reference. "self"
 * is resolved in Naming and does not exist during the typing phase. "static"
 * is resolved in two places.
 *
 * 1) Typing_instantiate -
 *      Here we will instantiate "static" to the class
 *      referenced by the "this" type. This is done because we need to resolve
 *      the initial "static" reference outside the class where the type appears.
 *      We cannot boot strap using 'Env.get_self_id env' in those cases because
 *      it is either not set or is incorrect.
 *
 * 2) In this file we resolve all remaining "static" reference. We boot strap
 *    with the value set to 'Env.get_self_id env' and keep track of what static
 *    should be resolved to by threading through the last class that was
 *    accessed.
 *
 * It is tempting to resolve "static" earlier i.e. during naming like we do for
 * "self". However this is incorrect because type const can be overridden in
 * child classes. A good rule of thumb is if you do 'hh_client --show [Class]'
 * on a class that contains a return type of "static::SomeTypeConst", that we
 * ensure the type stored is "static::SomeTypeConst" and not
 * "SomeClass::SomeTypeConst". This will indicate we resolved "static" too soon.
 *)

let rec expand_ env root_ty ids =
  let root_pos = Reason.to_pos (fst root_ty) in
  match ids with
  | [] ->
      env, root_ty
  | (pos, tconst) :: rest ->
      let abstract_name, class_name = resolve_to_class_name env root_ty ids in
      let root_class =
        match Env.get_class env class_name with
        | None ->
            Errors.unbound_name_typing (root_pos) class_name;
            raise Exit
        | Some c -> c
      in
      let typeconst =
        match SMap.get tconst root_class.tc_typeconsts with
        | None ->
            Errors.smember_not_found
              `class_typeconst pos (root_pos, class_name) tconst `no_hint;
            raise Exit
        | Some tc -> tc
      in
      let tconst_ty =
        match abstract_name, typeconst with
        | _, {ttc_type = Some ty; ttc_constraint = None; _} -> ty
        | None, {ttc_type = Some ty; _} -> ty
        | Some root_name, {ttc_constraint = ty; _} ->
            Reason.Rnone, Tgeneric (root_name^"::"^tconst, ty)
        | None, {ttc_constraint = ty; _} ->
            Reason.Rnone, Tgeneric (class_name^"::"^tconst, ty)
      in
      let this_ty = make_this root_class in
      let env, tconst_ty = Inst.instantiate_this env tconst_ty this_ty in
      let tconst_ty = Reason.Rwitness (Pos.btw root_pos pos), snd tconst_ty in
      expand_ env tconst_ty rest

and make_this class_ =
  let tparams = List.map begin fun (_, (p, s), param) ->
    Reason.Rwitness p, Tgeneric (s, param)
  end class_.tc_tparams in
  let sid = class_.tc_pos, class_.tc_name in
  Reason.Rwitness class_.tc_pos, Tapply (sid, tparams)

and resolve_to_class_name env (r, ty) ids =
  let resolve env type_ = resolve_to_class_name env type_ ids in
  match ty with
  | Tapply ((_, tdef), tyl) when Env.is_typedef tdef ->
      let env, ty = TUtils.expand_typedef env r tdef tyl in
      resolve env ty
  | Tapply ((_, class_name), _) -> None, class_name
  | Taccess (root, ids) ->
      let env, fty = expand_ env root ids in
      resolve env fty
  | Toption ty ->
      resolve env ty
  | Tabstract ((_, x), _, Some ty) | Tgeneric (x, Some ty) ->
      let _, class_name = resolve env ty in
      Some x, class_name
  | Tany -> raise Exit
  | Tvar _ | Tunresolved _
  | Tanon _ | Tobject | Tmixed | Tprim _ | Tshape _ | Ttuple _
  | Tarray (_, _) | Tfun _ | Tabstract (_, _, _) | Tgeneric (_, _) ->
      let pos = Reason.to_pos r in
      let str = Typing_print.full env (r, ty) in
      Errors.unbound_name_typing pos str;
      raise Exit

let expand env (root, ids) =
  try expand_ env root ids with
  | Exit ->
      env, (Reason.none, Tany)
