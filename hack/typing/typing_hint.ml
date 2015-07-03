(**
 * Copyright (c) 2014, Facebook, Inc.
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
open Utils
open Typing_defs
open Nast

module Env = Typing_env

(*****************************************************************************)
(* The signature of the visitor. *)
(*****************************************************************************)

class type ['a] hint_visitor_type = object
  method on_hint   : 'a -> Nast.hint -> 'a
  method on_hint_  : 'a -> Nast.hint_ -> 'a

  method on_any    : 'a -> 'a
  method on_mixed  : 'a -> 'a
  method on_this   : 'a -> 'a
  method on_tuple  : 'a -> Nast.hint list -> 'a
  method on_abstr  : 'a -> string -> (Ast.constraint_kind * Nast.hint) option
                      -> 'a
  method on_array  : 'a -> Nast.hint option -> Nast.hint option -> 'a
  method on_prim   : 'a -> Nast.tprim -> 'a
  method on_option : 'a -> Nast.hint -> 'a
  method on_fun    : 'a -> Nast.hint list -> bool -> Nast.hint -> 'a
  method on_apply  : 'a -> Nast.sid -> Nast.hint list -> 'a
  method on_shape  : 'a -> Nast.hint ShapeMap.t -> 'a
  method on_access : 'a -> Nast.hint -> Nast.sid list -> 'a
end

(*****************************************************************************)
(* The generic visitor ('a is the type of the accumulator). *)
(*****************************************************************************)

class virtual ['a] hint_visitor: ['a] hint_visitor_type = object(this)

  method on_hint acc (_, h) = this#on_hint_ acc h

  method on_hint_ acc h = match h with
    | Hany                  -> this#on_any    acc
    | Hmixed                -> this#on_mixed  acc
    | Hthis                 -> this#on_this   acc
    | Htuple hl             -> this#on_tuple  acc (hl:Nast.hint list)
    | Habstr (x, cstr_opt)  -> this#on_abstr  acc x cstr_opt
    | Harray (hopt1, hopt2) -> this#on_array  acc hopt1 hopt2
    | Hprim p               -> this#on_prim   acc p
    | Hoption h             -> this#on_option acc h
    | Hfun (hl, b, h)       -> this#on_fun    acc hl b h
    | Happly (i, hl)        -> this#on_apply  acc i hl
    | Hshape hm             -> this#on_shape  acc hm
    | Haccess (h, il)       -> this#on_access acc h il

  method on_any acc = acc
  method on_mixed acc = acc
  method on_this acc = acc
  method on_tuple acc hl =
    List.fold_left this#on_hint acc (hl:Nast.hint list)

  method on_abstr acc _ = function
    | None -> acc
    | Some (_ck, h) -> this#on_hint acc h

  method on_array acc hopt1 hopt2 =
    let acc = match hopt1 with
      | None -> acc
      | Some h -> this#on_hint acc h
    in
    let acc = match hopt2 with
      | None -> acc
      | Some h -> this#on_hint acc h
    in
    acc

  method on_prim acc _ = acc

  method on_option acc h = this#on_hint acc h

  method on_fun acc hl _ h =
    let acc = List.fold_left this#on_hint acc hl in
    let acc = this#on_hint acc h in
    acc

  method on_apply acc _ (hl:Nast.hint list) =
    let acc = List.fold_left this#on_hint acc hl in
    acc

  method on_shape acc hm =
    ShapeMap.fold begin fun _ h acc ->
      let acc = this#on_hint acc h in
      acc
    end hm acc

  method on_access acc h _ =
    this#on_hint acc h

end

(* For checking whether hint refers to a construct that cannot ever have
 * instances. To avoid race conditions, only look up class info after the decl
 * phase is complete. *)
module CheckInstantiability = struct

  let visitor =
  object
    inherit [Env.env] hint_visitor as super

    method! on_apply env (usage_pos, n) hl =
      let () = (match Typing_env.Classes.get n with
        | Some {tc_kind = Ast.Cabstract; tc_final = true;
                tc_name; tc_pos; _}
        | Some {tc_kind = Ast.Ctrait; tc_name; tc_pos; _} ->
          Errors.uninstantiable_class usage_pos tc_pos tc_name
        | _ -> ()) in
      super#on_apply env (usage_pos, n) hl

    method! on_abstr _env _ _ =
        (* there should be no need to descend into abstract params, as
         * the necessary param checks happen on the declaration of the
         * constraint *)
      _env

  end

  let check env h : Env.env = visitor#on_hint env h

end

let check_instantiable (env:Env.env) (h:Nast.hint) =
  CheckInstantiability.check env h

let check_params_instantiable (env:Env.env) (params:Nast.fun_param list)=
  List.fold_left (begin fun env param ->
    match (param.param_hint) with
      | None -> env
      | Some h -> check_instantiable env h
  end) env params

let check_tparams_instantiable (env:Env.env) (tparams:Nast.tparam list) =
  List.fold_left (begin fun env (_variance, _sid, cstr_opt) ->
    match cstr_opt with
      | None -> env
      | Some (_ck, h) -> check_instantiable env h
  end) env tparams

(* Unpacking a hint for typing *)

(* ensure_instantiable should only be set after the decl phase is done *)
let rec hint ?(ensure_instantiable=false) env (p, h) =
  let env = if not ensure_instantiable then env
    else check_instantiable env (p, h) in
  let env, h = hint_ p env h in
  env, (Typing_reason.Rhint p, h)

and hint_ p env = function
  | Hany ->
      env, Tany
  | Hmixed ->
      env, Tmixed
  | Hthis ->
     env, Tthis
  | Harray (h1, h2) ->
      if Env.is_strict env && h1 = None
      then Errors.generic_array_strict p;
      let env, h1 = opt hint env h1 in
      let env, h2 = opt hint env h2 in
      env, Tarray (h1, h2)
  | Hprim p -> env, Tprim p
  | Habstr (x, cstr_opt) ->
      let env, cstr_opt = match cstr_opt with
        | Some (ck, h) ->
            let env, h = hint env h in
            env, Some (ck, h)
        | None -> env, None in
      env, Tgeneric (x, cstr_opt)
  | Hoption (_, Hprim Tvoid) ->
      Errors.option_return_only_typehint p `void;
      env, Tany
  | Hoption (_, Hprim Tnoreturn) ->
      Errors.option_return_only_typehint p `noreturn;
      env, Tany
  | Hoption (_, Hmixed) ->
      Errors.option_mixed p;
      env, Tany
  | Hoption h ->
      let env, h = hint env h in
      env, Toption h
  | Hfun (hl, b, h) ->
      let env, paraml = lfold hint env hl in
      let paraml = List.map (fun x -> None, x) paraml in
      let env, ret = hint env h in
      let arity_min = List.length paraml in
      let arity = if b
        then Fellipsis arity_min
        else Fstandard (arity_min, arity_min)
      in
      env, Tfun {
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
      env, Tany
  | Happly (((_p, c) as id), argl) ->
      Typing_hooks.dispatch_class_id_hook id None;
      Env.add_wclass env c;
      let env, argl = lfold hint env argl in
      env, Tapply (id, argl)
  | Haccess (root_ty, ids) ->
      let env, root_ty = hint env root_ty in
      env, Taccess (root_ty, ids)
  | Htuple hl ->
      let env, tyl = lfold hint env hl in
      env, Ttuple tyl
  | Hshape fdm ->
      let env, fdm = ShapeMap.map_env hint env fdm in
      (* Fields are only partially known, because this shape type comes from
       * type hint - shapes that contain listed fields can be passed here, but
       * due to structural subtyping they can also contain other fields, that we
       * don't know about. *)
      env, Tshape (FieldsPartiallyKnown ShapeMap.empty, fdm)

let hint_locl ?(ensure_instantiable=false) env h =
  let env, h = hint ~ensure_instantiable env h in
  Typing_phase.localize_with_self env h

(*****************************************************************************)

let open_class_hint = function
  | r, Tapply (name, tparaml) -> r, name, tparaml
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
  | Tfun _ | Ttuple _ | Tshape _ | Taccess (_, _) | Tthis) ->
      assert false
