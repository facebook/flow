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
  method on_tuple  : 'a -> Nast.hint list -> 'a
  method on_abstr  : 'a -> string -> Nast.hint option -> 'a
  method on_array  : 'a -> Nast.hint option -> Nast.hint option -> 'a
  method on_prim   : 'a -> Nast.tprim -> 'a
  method on_option : 'a -> Nast.hint -> 'a
  method on_fun    : 'a -> Nast.hint list -> bool -> Nast.hint -> 'a
  method on_apply  : 'a -> Nast.sid -> Nast.hint list -> 'a
  method on_shape  : 'a -> Nast.hint ShapeMap.t -> 'a
  method on_access : 'a -> Nast.class_id -> Nast.sid -> Nast.sid list -> 'a
end

(*****************************************************************************)
(* The generic visitor ('a is the type of the accumulator). *)
(*****************************************************************************)

class virtual ['a] hint_visitor: ['a] hint_visitor_type = object(this)

  method on_hint acc (_, h) = this#on_hint_ acc h

  method on_hint_ acc h = match h with
    | Hany                  -> this#on_any    acc
    | Hmixed                -> this#on_mixed  acc
    | Htuple hl             -> this#on_tuple  acc (hl:Nast.hint list)
    | Habstr (x, hopt)      -> this#on_abstr  acc x hopt
    | Harray (hopt1, hopt2) -> this#on_array  acc hopt1 hopt2
    | Hprim p               -> this#on_prim   acc p
    | Hoption h             -> this#on_option acc h
    | Hfun (hl, b, h)       -> this#on_fun    acc hl b h
    | Happly (i, hl)        -> this#on_apply  acc i hl
    | Hshape hm             -> this#on_shape  acc hm
    | Haccess (i1, i2, il)  -> this#on_access acc i1 i2 il

  method on_any acc = acc
  method on_mixed acc = acc
  method on_tuple acc hl =
    List.fold_left this#on_hint acc (hl:Nast.hint list)

  method on_abstr acc _ hopt =
    match hopt with
      | None -> acc
      | Some h -> this#on_hint acc h

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

  method on_access acc _ _ _ = acc

end

(* For checking whether hint refers to a construct that cannot ever
 * have instances. To avoid race conditions with typing, only look up
 * class info in the naming heap (and run only after the naming heap is
 * complete) *)
module CheckInstantiability = struct

  let visitor =
  object(this)
    inherit [Env.env] hint_visitor

    method! on_apply env (usage_pos, n) hl =
      let () = (match Naming_heap.ClassHeap.get n with
        | Some {c_kind = Ast.Cabstract; c_final = true;
                c_name = (decl_pos, decl_name); _}
        | Some {c_kind = Ast.Ctrait; c_name = (decl_pos, decl_name); _} ->
          Errors.uninstantiable_class usage_pos decl_pos decl_name
        | _ -> ()) in
      let env = List.fold_left this#on_hint env hl in
      env

    method! on_abstr env _ _ =
        (* there should be no need to descend into abstract params, as
         * the necessary param checks happen on the declaration of the
         * constraint *)
      env

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
  List.fold_left (begin fun env (_variance, _sid, opt_hint) ->
    match (opt_hint) with
      | None -> env
      | Some h -> check_instantiable env h
  end) env tparams

(* Unpacking a hint for typing *)

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
  | Harray (h1, h2) ->
      if Env.is_strict env && h1 = None
      then Errors.generic_array_strict p;
      let env, h1 = opt hint env h1 in
      let env, h2 = opt hint env h2 in
      env, Tarray (h1, h2)
  | Hprim p -> env, Tprim p
  | Habstr (x, hopt) ->
      let env, ty_opt = opt hint env hopt in
      env, Tgeneric (x, ty_opt)
  | Hoption (_, Hprim Tvoid) ->
      Errors.nullable_void p;
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
        ft_unsafe = false;
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
  | Happly (((p, c) as id), argl) ->
      Find_refs.process_class_ref p c None;
      Typing_hooks.dispatch_class_id_hook id None;
      Env.add_wclass env c;
      let env, argl = lfold hint env argl in
      env, Tapply (id, argl)
  | Haccess (root, id, ids) ->
      let root = match root with
        | CIstatic -> Some SCIstatic
        | CI (pos, class_) ->
            Find_refs.process_class_ref pos class_ None;
            Typing_hooks.dispatch_class_id_hook id None;
            Env.add_wclass env class_;
            Some (SCI (pos, class_))
        | CIparent ->
            Errors.unbound_name_typing p "parent";
            None
        (* These should be stripped out earlier *)
        | CIself | CIvar _ ->
            assert false;
      in
      opt_map_default (fun r -> env, Taccess (r, id, ids)) (env, Tany) root
  | Htuple hl ->
      let env, tyl = lfold hint env hl in
      env, Ttuple tyl
  | Hshape fdm ->
      let env, fdm = ShapeMap.map_env hint env fdm in
      env, Tshape fdm
