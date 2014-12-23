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

module Reason = Typing_reason
module Env    = Typing_env
module Inst   = Typing_instantiate
module TUtils = Typing_utils
module TAccess = Typing_taccess

(*****************************************************************************)
(* Expanding type definition *)
(*****************************************************************************)

let rec expand_typedef_ ?force_expand:(force_expand=false) seen env r x argl =
  let pos = Reason.to_pos r in
  let tdef = Typing_env.get_typedef env x in
  let tdef = match tdef with None -> assert false | Some x -> x in
  match tdef with
    | Env.Typedef.Error -> env, (r, Tany), pos
    | Env.Typedef.Ok tdef ->
  let visibility, tparaml, tcstr, expanded_ty, tdef_pos = tdef in
  let should_expand = force_expand ||
    match visibility with
    | Env.Typedef.Private ->
        Pos.filename tdef_pos = Env.get_file env
    | Env.Typedef.Public -> true
  in
  if List.length tparaml <> List.length argl
  then begin
    let n = List.length tparaml in
    let n = string_of_int n in
    Errors.type_param_arity pos x n
  end;
  let subst = Inst.make_subst tparaml argl in
  let env, expanded_ty =
    if should_expand
    then begin
      Inst.instantiate subst env expanded_ty
    end
    else begin
      let env, tcstr =
        match tcstr with
        | None -> env, None
        | Some tcstr ->
            let env, tcstr = Inst.instantiate subst env tcstr in
            env, Some tcstr
      in
      env, (r, Tabstract ((pos, x), argl, tcstr))
    end
  in
  Errors.try_with_error
    (fun () ->
      check_typedef seen env expanded_ty;
      env, (r, snd expanded_ty), tdef_pos
    )
    (fun () -> env, (r, Tany), tdef_pos)

and check_typedef seen env (r, t) =
  match t with
  | Tany -> ()
  | Tmixed -> ()
  | Tarray (ty1, ty2) ->
      check_typedef_opt seen env ty1;
      check_typedef_opt seen env ty2;
      ()
  | Tgeneric (_, ty) ->
      check_typedef_opt seen env ty
  | Toption ty -> check_typedef seen env ty
  | Tprim _ -> ()
  | Tvar _ ->
      (* This can happen after intantiantion of the typedef.
       * Having a cyclic typedef defined this way is fine, because of the
       * type variable, it will be handled gracefully.
       * Besides, it's not that the typedef depends on itself, it's that
       * it depends on a parameter that could use itself, which is different.
       * (cf tdef_tvar.php unit test for a use case).
       *)
      ()
  | Tfun fty ->
      check_fun_typedef seen env fty
  | Tapply ((p, x), argl) when Typing_env.is_typedef x ->
      if seen = x
      then Errors.cyclic_typedef p
      else
        let env, ty, _ = expand_typedef_ seen env r x argl in
        check_typedef seen env ty
  | Tabstract (_, tyl, cstr) ->
      check_typedef_list seen env tyl;
      check_typedef_opt seen env cstr
  | Tapply (_, tyl)
  | Ttuple tyl ->
      check_typedef_list seen env tyl
  | Taccess (_, _, _) -> ()
  | Tanon _ -> assert false
  | Tunresolved _ -> assert false
  | Tobject -> ()
  | Tshape tym ->
      Nast.ShapeMap.iter (fun _ v -> check_typedef seen env v) tym

and check_typedef_list seen env x =
  List.iter (check_typedef seen env) x

and check_fun_typedef seen env ft =
  check_typedef_tparam_list seen env ft.ft_tparams;
  check_typedef_fun_param_list seen env ft.ft_params;
  (match ft.ft_arity with
    | Fvariadic (_, p) -> check_typedef_fun_param seen env p
    | _ -> ());
  check_typedef seen env ft.ft_ret;
  ()

and check_typedef_fun_param_list seen env x =
  List.iter (check_typedef_fun_param seen env) x

and check_typedef_fun_param seen env (_, ty) =
  check_typedef seen env ty

and check_typedef_tparam_list seen env x =
  List.iter (check_typedef_tparam seen env) x

and check_typedef_tparam seen env (_, _, x) =
  check_typedef_opt seen env x

and check_typedef_opt seen env = function
  | None -> ()
  | Some x -> check_typedef seen env x

let expand_typedef env r x argl =
  let env, t, _ = expand_typedef_ x env r x argl
  in env, t

(* Expand a typedef, smashing abstraction and collecting a trail
 * of where the typedefs come from. *)
let rec force_expand_typedef_ trail env = function
  | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
    let env, t, pos = expand_typedef_ ~force_expand:true x env r x argl in
    (* We need to keep expanding until we hit something that isn't a typedef *)
    force_expand_typedef_ (pos::trail) env t
  | (_, Taccess _) as ty ->
      let env, ty = TAccess.expand env ty in
      force_expand_typedef_ trail env ty
  | r, t -> env, (r, t), List.rev trail
let force_expand_typedef = force_expand_typedef_ []

(*****************************************************************************)
(*****************************************************************************)

let () = TUtils.expand_typedef_ref := expand_typedef
