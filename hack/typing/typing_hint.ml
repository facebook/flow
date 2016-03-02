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
    List.fold_left ~f:this#on_hint ~init:acc (hl:Nast.hint list)

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
    let acc = List.fold_left ~f:this#on_hint ~init:acc hl in
    let acc = this#on_hint acc h in
    acc

  method on_apply acc _ (hl:Nast.hint list) =
    let acc = List.fold_left ~f:this#on_hint ~init:acc hl in
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

  let validate_classname = function
    | _, (Happly _ | Hthis | Hany | Hmixed | Habstr _ | Haccess _) -> ()
    | p, (Htuple _ | Harray _ | Hprim _ | Hoption _ | Hfun _ | Hshape _) ->
        Errors.invalid_classname p

  let visitor env = object(this)
    inherit [unit] hint_visitor as super

    (* A type constant may appear inside an abstract final class, i.e.
     *
     *   abstract final class Foo { const type T = int; }
     *
     * Even though the type `Foo` is not instantiable, the type `Foo::T` is.
     * Thus for type checking the Taccess type, we skip the instantiability
     * for the root of the Taccess.
     *)
    method! on_access () h ids = match h with
      | _, Happly (_, hl) ->
        List.iter hl (this#on_hint ())
      | _ -> super#on_access () h ids

    method! on_apply () (usage_pos, n) hl =
      let () = (match Env.get_class env n with
        | Some {tc_kind = Ast.Cabstract; tc_final = true;
                tc_name; tc_pos; _}
        | Some {tc_kind = Ast.Ctrait; tc_name; tc_pos; _} ->
          Errors.uninstantiable_class usage_pos tc_pos tc_name []
        | _ -> ()) in
      if n = SN.Classes.cClassname
      then Option.iter (List.hd hl) validate_classname
      else super#on_apply () (usage_pos, n) hl

    method! on_abstr () _ _ =
      (* there should be no need to descend into abstract params, as
       * the necessary param checks happen on the declaration of the
       * constraint *)
      ()

  end

  let check env h = (visitor env)#on_hint () h

end

let check_instantiable (env:Env.env) (h:Nast.hint) =
  CheckInstantiability.check env h

let check_params_instantiable (env:Env.env) (params:Nast.fun_param list)=
  List.iter params ~f:begin fun param ->
    match param.param_hint with
    | None -> ()
    | Some h -> check_instantiable env h
  end

let check_tparams_instantiable (env:Env.env) (tparams:Nast.tparam list) =
  List.iter tparams ~f:begin fun (_variance, _sid, cstr_opt) ->
    match cstr_opt with
    | None -> ()
    | Some (_ck, h) -> check_instantiable env h
  end

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

let instantiable_hint env h =
  check_instantiable env h;
  hint env.Env.decl_env h

let hint_locl env h =
  let h = hint env.Env.decl_env h in
  Typing_phase.localize_with_self env h
