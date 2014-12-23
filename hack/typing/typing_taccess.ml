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

module TUtils = Typing_utils
module Reason = Typing_reason
module Env = Typing_env

exception RootClassNotFound of Pos.t * string
exception RootInvalidType of ty
exception TypeConstNotFound of Pos.t * class_type * string

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
let rec expand_with_static env fty static =
  try expand_ env fty static with
  (* TODO(dreeves): improve error messages, these are placeholders *)
  | RootClassNotFound (pos, str) ->
      Errors.unbound_name_typing pos str;
      env, (Reason.none, Tany)
  | RootInvalidType ty ->
      let pos = Reason.to_pos (fst ty) in
      let str = Typing_print.full env ty in
      Errors.unbound_name_typing pos str;
      env, (Reason.none, Tany)
  | TypeConstNotFound (pos, class_, tconst) ->
      let class_pname = (class_.tc_pos, class_.tc_name) in
      Errors.smember_not_found `class_typeconst pos class_pname tconst `no_hint;
      env, (Reason.none, Tany)

and expand_ env (reason, type_) static =
  match type_ with
    | Taccess (root, (tconst_pos, tconst), rest) ->
        let pos = Reason.to_pos reason in
        let root_id =
          match root with
          | SCI root_id -> root_id
          | SCIstatic -> pos, static
        in
        let root_type = reason, Tapply (root_id, []) in
        let env, class_ = get_class_from_type env root_type static in
        let tconst_ty =
          match Env.get_typeconst_type env class_ tconst with
          | None -> raise (TypeConstNotFound (tconst_pos, class_, tconst))
          | Some tc -> tc
        in
        begin
          match tconst_ty, rest with
          | (_,
              ( Tapply (new_root, _)
              | Tgeneric (_, Some (_, Tapply (new_root, _)))
              | Tabstract (_, _, Some (_, Tapply (new_root, _)))
              )
            ), hd :: tail ->
              let new_root_cid = SCI new_root in
              let new_ty = reason, Taccess (new_root_cid, hd, tail) in
              (* NOTE: that we switch the meaning of "static" to be the name
               * of the class we accessed the type const from. Initially this
               * is set to 'Env.get_self_id env', but must change after each
               * access to ensure that "static" will be resolved correctly
               *)
              expand_ env new_ty class_.tc_name
          (* If we encounter one of the special type defs used to back type
           * constants then we have to continue expanding. To see why consider
           * the following.
           *
           * class D {
           *   type TC = int;
           *   type Foo = static::TC;
           * }
           *
           * class C { type Bar = D }
           *
           * Expanding "C::Bar::Foo" yields Tapply ("D::Foo"). If we stop here
           * then when we later expand the type def we will have "static::TC"
           * and we no longer have enough context to properly resolve "static".
           *)
          | (r, Tapply ((_, tdef), [])), [] when Env.is_typedef tdef
            && has_double_colon tdef ->
              let env, ty = TUtils.expand_typedef env r tdef [] in
              (* See the [NOTE] above that explains why we need to pass in
               * 'class_.tc_name'
               *)
              expand_ env ty class_.tc_name
          | tconst_ty, [] -> env, tconst_ty
          | (_,
              ( Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _
              | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _
              | Taccess (_, _, _) | Tanon (_, _) | Tunresolved _ | Tobject
              | Tshape _
              )
            ), rest ->
              raise (RootInvalidType (tconst_ty))
        end
    | Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tapply (_, _)
    | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _ ->
        env, (reason, type_)

and has_double_colon str =
  try
    let idx = String.index str ':' in
    str.[idx + 1] = ':'
  with
  | Invalid_argument _ | Not_found -> false

(* Retrieves the class name for a given type. This is recursive because if
 * we have a name that refers to a typedef then we will need expand the typedef.
 * For example:
 *
 * class Foo {};
 * type Bar = Foo;
 *)
and get_class_from_type env (reason, type_) static =
  let pos = Reason.to_pos reason in
  match type_ with
    | Tapply ((_, name), argl) when Env.is_typedef name ->
        let env, ty = TUtils.expand_typedef env reason name argl in
        get_class_from_type env ty static
    | Tapply ((_, class_name), _) ->
        env, begin
          match Env.get_class env class_name with
          | None -> raise (RootClassNotFound (pos, class_name))
          | Some class_ -> class_
        end
    | Tabstract (_, _, Some constraint_type) ->
        get_class_from_type env constraint_type static
    | Taccess (_, _, _) ->
        let env, ty = expand_with_static env (reason, type_) static in
        get_class_from_type env ty static
    | Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _
    | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _ ->
        raise (RootInvalidType (reason, type_))

and expand env fty = expand_with_static env fty (Env.get_self_id env)
