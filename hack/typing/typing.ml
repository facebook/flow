(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(* This module implements the typing.
 *
 * Given an Nast.program, it infers the type of all the local
 * variables, and checks that all the types are correct (aka
 * consistent) *)
open Utils
open Nast
open Typing_defs
open Autocomplete

module TUtils       = Typing_utils
module Reason       = Typing_reason
module Inst         = Typing_instantiate
module Type         = Typing_ops
module Env          = Typing_env
module LEnv         = Typing_lenv
module Dep          = Typing_deps.Dep
module Async        = Typing_async
module DynamicYield = Typing_dynamic_yield
module SubType      = Typing_subtype
module Unify        = Typing_unify
module TGen         = Typing_generic
module SN           = Naming_special_names
module TAccess      = Typing_taccess

(*****************************************************************************)
(* Debugging *)
(*****************************************************************************)

(* A guess as to the last position we were typechecking, for use in debugging,
 * such as figuring out what a runaway hh_server thread is doing. Updated
 * only best-effort -- it's an approximation to point debugging in the right
 * direction, nothing more. *)
let debug_last_pos = ref Pos.none
let debug_print_last_pos _ = print_endline (Pos.string (Pos.to_absolute
  !debug_last_pos))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let suggest env p ty =
  let ty = Typing_expand.fully_expand env ty in
  (match Typing_print.suggest ty with
  | "..." -> Errors.expecting_type_hint p
  | ty -> Errors.expecting_type_hint_suggest p ty
  )

let suggest_return env p ty =
  let ty = Typing_expand.fully_expand env ty in
  (match Typing_print.suggest ty with
  | "..." -> Errors.expecting_return_type_hint p
  | ty -> Errors.expecting_return_type_hint_suggest p ty
  )

let any = Reason.Rnone, Tany

let compare_field_kinds x y =
  match x, y with
  | Nast.AFvalue (p1, _), Nast.AFkvalue ((p2, _), _)
  | Nast.AFkvalue ((p2, _), _), Nast.AFvalue (p1, _) ->
      Errors.field_kinds p1 p2
  | _ -> ()

let check_consistent_fields x l =
  List.iter (compare_field_kinds x) l

let unbound_name env (pos, name)=
  (match Env.get_mode env with
  | Ast.Mstrict ->
      Errors.unbound_name_typing pos name
  | Ast.Mdecl | Ast.Mpartial ->
      ()
  );
  env, (Reason.Rnone, Tany)

(*****************************************************************************)
(* Global constants typing *)
(*****************************************************************************)

let gconst_decl cst =
   let env = Typing_env.empty (Pos.filename (fst cst.cst_name)) in
   let env = Env.set_mode env cst.cst_mode in
   let env = Env.set_root env (Dep.GConst (snd cst.cst_name)) in
   let env, hint_ty =
     match cst.cst_type with
     | None -> env, (Reason.Rnone, Tany)
     | Some h -> Typing_hint.hint env h
   in
   Typing_env.GConsts.add (snd cst.cst_name) hint_ty

(*****************************************************************************)
(* Handling function/method arguments *)
(*****************************************************************************)

let rec wfold_left_default f (env, def1) l1 l2 =
  match l1, def1, l2 with
  | _, _, [] -> env
  | [], None, _ -> env
  | [], Some d1, x2 :: rl2 ->
    let env = f env d1 x2 in
    wfold_left_default f (env, def1) [] rl2
  | x1 :: rl1, _, x2 :: rl2 ->
    let env = f env x1 x2 in
    wfold_left_default f (env, def1) rl1 rl2

let rec fun_decl f =
  let env = Typing_env.empty (Pos.filename (fst f.f_name)) in
  let env = Env.set_mode env f.f_mode in
  let env = Env.set_root env (Dep.Fun (snd f.f_name)) in
  let _, ft = fun_decl_in_env env f in
  Env.add_fun (snd f.f_name) ft;
  ()

and fun_decl_in_env env f =
  let mandatory_init = true in
  let env, arity_min, params = make_params env mandatory_init 0 f.f_params in
  let env, ret_ty = match f.f_ret, f.f_fun_kind with
    (* The utility in making FAsync to always be Awaitable is the "unused
     * awaitable" check, which doesn't apply to these. *)
    | None, FGenerator
    | None, FAsyncGenerator
    (* If there is no return type annotation, we clearly should make
     * it Tany but also want a witness so that we can point *somewhere*
     * in event of error. The function name itself isn't great, but is
     * better than nothing. *)
    | None, FSync -> env, (Reason.Rwitness (fst f.f_name), Tany)
    | None, FAsync ->
      let pos = fst f.f_name in
      env, (Reason.Rasync_ret pos,
            Tapply ((pos, SN.Classes.cAwaitable), [(Reason.Rwitness pos, Tany)]))
    | Some ty, _ -> Typing_hint.hint env ty in
  let env, arity = match f.f_variadic with
    | FVvariadicArg param ->
      assert param.param_is_variadic;
      assert (param.param_expr = None);
      let env, (p_name, p_ty) = make_param_ty env Reason.Rnone param in
      env, Fvariadic (arity_min, (p_name, p_ty))
    | FVellipsis    -> env, Fellipsis (arity_min)
    | FVnonVariadic -> env, Fstandard (arity_min, List.length f.f_params)
  in
  let env, tparams = lfold type_param env f.f_tparams in
  let ft = {
    ft_pos         = fst f.f_name;
    ft_unsafe      = false;
    ft_abstract    = false;
    ft_arity       = arity;
    ft_tparams     = tparams;
    ft_params      = params;
    ft_ret         = ret_ty;
  } in
  env, ft

and type_param env (variance, x, y) =
  let env, y = opt Typing_hint.hint env y in
  env, (variance, x, y)

and check_default pos mandatory e =
  if not mandatory && e = None
  then Errors.previous_default pos
  else ()

(* Functions building the types for the parameters of a function *)
(* It's not completely trivial because of optional arguments  *)
and make_param env mandatory arity param =
  let env, ty = make_param_ty env Reason.Rnone param in
  let mandatory =
    if param.param_is_variadic then begin
      assert(param.param_expr = None);
      false
    end else begin
      if Env.is_decl env then ()
      else check_default (fst param.param_id) mandatory param.param_expr;
      mandatory && param.param_expr = None
    end
  in
  let arity = if mandatory then arity + 1 else arity in
  env, arity, mandatory, ty

and make_params env mandatory arity paraml =
  match paraml with
  | [] -> env, arity, []
  | param :: rl ->
      let env, arity, mandatory, ty = make_param env mandatory arity param in
      let env, arity, rest = make_params env mandatory arity rl in
      env, arity, ty :: rest

(* externally exposed convenience wrapper *)
and make_param_ty env reason param =
  make_param_type_ ~for_body:false (fun() -> reason, Tany) env param

(* This function is used to determine the type of an argument.
 * When we want to type-check the body of a function, we need to
 * introduce the type of the arguments of the function in the environment
 * Let's take an example, we want to check the code of foo:
 *
 * function foo(int $x): int {
 *   // CALL TO make_param_type on (int $x)
 *   // Now we know that the type of $x is int
 *
 *   return $x; // in the environment $x is an int, the code is correct
 * }
 *)
and make_param_type_ ~for_body default env param =
  let param_pos = (fst param.param_id) in
  let env, ty =
    match param.param_hint with
      | None ->
        (* if the type is missing, use the default one (an unbound
         * type variable) *)
        let _r, ty = default() in
        let r = Reason.Rwitness param_pos in
        env, (r, ty)
      | Some (p, (Hprim Tvoid)) ->
        let r = Reason.Rwitness param_pos in
        Errors.void_parameter p;
        env, (r, Tany)
      (* if the code is strict, use the type-hint *)
      | Some x when Env.is_strict env -> Typing_hint.hint env x
      (* This code is there because we used to be more tolerant in
       * partial-mode we use to allow (A $x = null) as an argument
       * instead of (?A $x = null) for the transition, we give this error
       * message, that explains what's going on, that despite the the (=
       * null) users are now required to use the optional type (write ?A
       * instead of A).  *)
      | Some (_, (Hoption _ | Hmixed) as x) -> Typing_hint.hint env x
      | Some x ->
        match (param.param_expr) with
          | Some (null_pos, Null) when not (Env.is_decl env) ->
            Errors.nullable_parameter (fst x);
            let env, ty = Typing_hint.hint env x in
            env, (Reason.Rwitness null_pos, Toption ty)
          | Some _ | None -> Typing_hint.hint env x
  in
  let ty = match ty with
    | r, t when param.param_is_variadic && for_body ->
      (* when checking the body of a function with a variadic
       * argument, "f(C ...$args)", $args is an array<C> ... *)
      let r = Reason.Rvar_param param_pos in
      let arr_values = r, t in
      r, Tarray (Some arr_values, None)
    | r, t when param.param_is_variadic ->
      (* ... but when checking a call to such a function: "f($a, $b)",
       * both $a and $b must be of type C *)
      Reason.Rvar_param param_pos, t
    | x -> x
  in
  TUtils.save_infer env param_pos ty;
  env, (Some param.param_name, ty)

(* In strict mode, we force you to give a type declaration on a parameter *)
(* But the type checker is nice: it makes a suggestion :-) *)
and check_param env param (_, ty) =
  match (param.param_hint) with
  | None -> suggest env (fst param.param_id) ty
  | Some _ -> ()

and bind_param env (_, ty1) param =
  let env, ty2 = opt expr env param.param_expr in
  let ty2 =
    match ty2 with
    | None    -> Reason.none, Tany
    | Some ty -> ty
  in
  Typing_suggest.save_param (param.param_name) env ty1 ty2;
  let env = Type.sub_type (fst param.param_id) Reason.URhint env ty1 ty2 in
  Env.set_local env (snd param.param_id) ty1

(*****************************************************************************)
(* Now we are actually checking stuff! *)
(*****************************************************************************)
and fun_def env _ f =
  if f.f_mode = Ast.Mdecl then () else begin
    NastCheck.fun_ env f (Nast.assert_named_body f.f_body);
    (* Fresh type environment is actually unnecessary, but I prefer to
     * have a guarantee that we are using a clean typing environment. *)
    Env.fresh_tenv env (
      fun env_up ->
        let env = { env_up with Env.lenv = Env.empty_local } in
        let env = Env.set_mode env f.f_mode in
        let env = Env.set_root env (Dep.Fun (snd f.f_name)) in
        let env, hret =
          match f.f_ret with
            | None -> env, (Reason.Rwitness (fst f.f_name), Tany)
            | Some ret ->
              Typing_hint.hint ~ensure_instantiable:true env ret in
        let f_params = match f.f_variadic with
          | FVvariadicArg param -> param :: f.f_params
          | _ -> f.f_params
        in
        let env = Typing_hint.check_params_instantiable env f_params in
        let env = Typing_hint.check_tparams_instantiable env f.f_tparams in
        let env, params =
          lfold (make_param_type_ ~for_body:true Env.fresh_type) env f_params in
        let env = List.fold_left2 bind_param env params f_params in
        let env = fun_ env f.f_unsafe (f.f_ret <> None) hret (fst f.f_name) f.f_body f.f_fun_kind in
        let env = fold_fun_list env env.Env.todo in
        if Env.is_strict env then begin
          List.iter2 (check_param env) f_params params;
          match f.f_ret with
            | None -> suggest_return env (fst f.f_name) hret
            | Some _ -> ()
        end;
    )
  end

(*****************************************************************************)
(* function used to type closures, functions and methods *)
(*****************************************************************************)

and fun_ ?(abstract=false) env unsafe has_ret hret pos b fun_type =
  Env.with_return env begin fun env ->
    debug_last_pos := pos;
    let env = Env.set_return env hret in
    let env = Env.set_fn_kind env fun_type in
    let b = Nast.assert_named_body b in
    let env = block env b in
    let ret = Env.get_return env in
    let env =
      if Nast_terminality.Terminal.block b ||
        abstract ||
        unsafe ||
        !auto_complete
      then env
      else fun_implicit_return env pos ret b fun_type in
    debug_last_pos := Pos.none;
    env
  end

and fun_implicit_return env pos ret b = function
  | FSync -> implicit_return_noasync pos env ret
  | FAsync -> implicit_return_async b pos env ret (Reason.Rno_return_async pos)
  | FGenerator
  | FAsyncGenerator -> env

(* A function without a terminal block has an implicit return null *)
and implicit_return_noasync p env ret =
  let rty = Reason.Rno_return p, Tprim Nast.Tvoid in
  Typing_suggest.save_return env ret rty;
  Type.sub_type p Reason.URreturn env ret rty

(* An async function without a yield result() has an implicit
 * yield result(null) if it is a generator or an implicit
 * return null if it is an async function
 *)
and implicit_return_async b p env ret reason =
  let type_var = Env.fresh_type () in
  let rty_core = Toption type_var in
  let suggest_core =
    (* OK, this is pretty obnoxious. In principle, a function with no call to
     * yield result() can be either Awaitable<void> or Awaitable<?anything> due
     * to the special case around yield result(null). In practice, you almost
     * certainly meant Awaitable<void> and we should suggest that. IMO we
     * should extend this HasYieldResult check beyond just type suggestions,
     * or come up with something better than yield result(null) for
     * Awaitable<void>, but the former is a ton of cleanup in www and I have
     * no ideas on the latter right now, so let's just use this heuristic.
     *
     * These semantics also unfortunately got extended to async functions --
     * normally an omitted return will mean the function has a return type of
     * void, but for async functions we implicitly assume return null.
     *
     * The check for is_suggest_mode is superflouous (save_return will do it)
     * and is just to avoid the potentially expensive HasYieldResult call if
     * we know we won't ever care. *)
    if !is_suggest_mode && NastVisitor.HasReturn.block b
    then rty_core
    else Tprim Nast.Tvoid in
  let mk_rty core = reason, Tapply ((p, SN.Classes.cAwaitable), [reason, core]) in
  Typing_suggest.save_return env ret (mk_rty suggest_core);
  Type.sub_type p Reason.URreturn env ret (mk_rty rty_core)

and block env stl =
  List.fold_left stmt env stl

and stmt env = function
  | Fallthrough
  | Noop ->
      env
  | Expr e ->
      let env, ty = expr env e in
      (* NB: this check does belong here and not in expr, even though it only
       * applies to expressions -- we actually want to perform the check on
       * statements that are expressions, e.g., "foo();" we want to check, but
       * "return foo();" we do not even though the expression "foo()" is a
       * subexpression of the statement "return foo();". *)
       (match snd e with
         | Nast.Binop (Ast.Eq _, _, _) -> ()
         | _ -> Typing_async.enforce_not_awaitable env (fst e) ty);
      env
  | If (e, b1, b2)  ->
      let env, ty = expr env e in
      Typing_async.enforce_not_awaitable env (fst e) ty;
      let parent_lenv = env.Env.lenv in
      let env   = condition env true e in
      let env   = block env b1 in
      let lenv1 = env.Env.lenv in
      let env   = { env with Env.lenv = parent_lenv } in
      let env   = condition env false e in
      let env   = block env b2 in
      let lenv2 = env.Env.lenv in
      let terminal1 = Nast_terminality.Terminal.block b1 in
      let terminal2 = Nast_terminality.Terminal.block b2 in
      if terminal1 && terminal2
      then
        let env = LEnv.integrate env parent_lenv lenv1 in
        let env = LEnv.integrate env env.Env.lenv lenv2 in
        LEnv.integrate env env.Env.lenv parent_lenv
      else if terminal1
      then begin
        let env = LEnv.integrate env parent_lenv lenv1 in
        LEnv.integrate env env.Env.lenv lenv2
      end
      else if terminal2
      then begin
        let env = LEnv.integrate env parent_lenv lenv2 in
        LEnv.integrate env env.Env.lenv lenv1
      end
      else LEnv.intersect env parent_lenv lenv1 lenv2
  | Return (p, None) ->
      let rty = match Env.get_fn_kind env with
        | FSync -> (Reason.Rwitness p, Tprim Tvoid)
        | FGenerator
        | FAsyncGenerator -> any (* Return type checked against the "yield". *)
        | FAsync -> (Reason.Rwitness p, Tapply ((p, SN.Classes.cAwaitable), [(Reason.Rwitness p, Toption (Env.fresh_type ()))])) in
      let expected_return = Env.get_return env in
      Typing_suggest.save_return env expected_return rty;
      let env = Type.sub_type p Reason.URreturn env expected_return rty in
      env
  | Return (p, Some e) ->
      let pos = fst e in
      let env, rty = expr env e in
      let rty = match Env.get_fn_kind env with
        | FSync -> rty
        | FGenerator
        | FAsyncGenerator -> any (* Is an error, but caught in NastCheck. *)
        | FAsync -> (Reason.Rwitness p), Tapply ((p, SN.Classes.cAwaitable), [rty]) in
      let expected_return = Env.get_return env in
      (match snd (Env.expand_type env expected_return) with
      | r, Tprim Tvoid ->
          (* Yell about returning a value from a void function. This catches
           * more issues than just unifying with void would do -- in particular
           * just unifying allows you to return a Tany from a void function,
           * which is clearly wrong. Note this check is best-effort; if the
           * function returns a generic type which later ends up being Tvoid
           * then there's not much we can do here. *)
          Errors.return_in_void p (Reason.to_pos r);
          env
      | _, Tunresolved _ ->
          (* we allow return types to grow for anonymous functions *)
          let env, rty = TUtils.unresolved env rty in
          let env, _ = Type.unify pos Reason.URreturn env expected_return rty in
          env
      | _, (Tany | Tmixed | Tarray (_,_) | Tgeneric (_, _) | Toption _ | Tprim _
        | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _
        | Tanon (_, _) | Tobject | Tshape _ | Taccess (_, _, _)) ->
          Typing_suggest.save_return env expected_return rty;
          let env = Type.sub_type pos Reason.URreturn env expected_return rty in
          env
      )
  | Do (b, e) as st ->
      (* NOTE: leaks scope as currently implemented; this matches
         the behavior in naming (cf. `do_stmt` in naming/naming.ml).
       *)
      let parent_lenv = env.Env.lenv in
      let env = Env.freeze_local_env env in
      let env = block env b in
      let (env, _) = expr env e in
      let after_block = env.Env.lenv in
      let alias_depth = Typing_alias.get_depth st in
      let env = iter_n_acc alias_depth begin fun env ->
        let env = condition env true e in
        let env = block env b in
        env
      end env in
      let env =
        if NastVisitor.HasContinue.block b
        then LEnv.fully_integrate env parent_lenv
        else
          let env = LEnv.integrate env parent_lenv env.Env.lenv in
          let env = { env with Env.lenv = after_block } in
          env
      in
      condition env false e
  | While (e, b) as st ->
      let (env, _) = expr env e in
      let parent_lenv = env.Env.lenv in
      let env = Env.freeze_local_env env in
      let alias_depth = Typing_alias.get_depth st in
      let env = iter_n_acc alias_depth begin fun env ->
        let env = condition env true e in
        let env = block env b in
        env
      end env in
      let env = LEnv.fully_integrate env parent_lenv in
      condition env false e
  | For (e1, e2, e3, b) as st ->
      (* For loops leak their initalizer, but nothing that's defined in the
         body
       *)
      let (env, _) = expr env e1 in      (* initializer *)
      let (env, _) = expr env e2 in
      let parent_lenv = env.Env.lenv in
      let env = Env.freeze_local_env env in
      let alias_depth = Typing_alias.get_depth st in
      let env = iter_n_acc alias_depth begin fun env ->
        let env = condition env true e2 in (* iteration 0 *)
        let env = block env b in
        let (env, _) = expr env e3 in
        env
      end env in
      let env = LEnv.fully_integrate env parent_lenv in
      condition env false e2
  | Switch (e, cl) ->
      Nast_terminality.SafeCase.check (fst e) cl;
      let denv, ty = expr env e in
      let env = check_exhaustiveness env (fst e) ty cl in
      let parent_lenv = env.Env.lenv in
      let env, cl = case_list parent_lenv ty env cl in
      let terml, lenvl = List.split cl in
      LEnv.intersect_list env parent_lenv lenvl terml
  | Foreach (e1, e2, b) as st ->
      let env, ty1 = expr env e1 in
      let env, ty1 = TUtils.fold_unresolved env ty1 in
      let env, ety1 = Env.expand_type env ty1 in
      let parent_lenv = env.Env.lenv in
      let env = Env.freeze_local_env env in
      let env, ty2 = as_expr env (fst e1) e2 in
      let env = Type.sub_type (fst e1) Reason.URforeach env ty2 ty1 in
      let alias_depth = Typing_alias.get_depth st in
      let env = iter_n_acc alias_depth begin fun env ->
        let env = bind_as_expr env ty2 e2 in
        let env = block env b in
        env
      end env in
      let env = LEnv.fully_integrate env parent_lenv in
      env
  | Try (tb, cl, fb) ->
    let env = try_catch (tb, cl) env in
    let env = block env fb in
    env
  | Static_var el ->
    let env = List.fold_left begin fun env e ->
      match e with
        | _, Binop (Ast.Eq _, (_, Lvar (p, x)), _) ->
          Env.add_todo env (TGen.no_generic p x)
        | _ -> env
    end env el in
    let env, _ = lfold expr env el in
    env
  | Throw (_, e) ->
    let p = fst e in
    let env, ty = expr env e in
    let exn_ty = Reason.Rthrow p, Tapply ((p, SN.Classes.cException), []) in
    Type.sub_type p (Reason.URthrow) env exn_ty ty
  | Continue _
  | Break _ -> env

and check_exhaustiveness env pos ty caselist =
  (* Right now we only do exhaustiveness checking for enums. *)
  let env, (r, ty) = Env.expand_type env ty in
  match ty with
    | Tunresolved tyl ->
      List.fold_left begin fun env ty ->
        check_exhaustiveness env pos ty caselist
      end env tyl
    | Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, ty = Typing_tdef.expand_typedef env r x argl in
      check_exhaustiveness env pos ty caselist

    | Tapply ((_, id), _) ->
      (match Typing_env.get_enum id with
        | Some tc -> Typing_enum.check_enum_exhaustiveness env pos tc caselist
        | None -> ());
      env
    | Taccess (_, _, _) ->
        let env, fty = TAccess.expand env (r, ty) in
        check_exhaustiveness env pos fty caselist
    | Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
    | Tobject | Tshape _ -> env

and case_list parent_lenv ty env cl =
  let env = { env with Env.lenv = parent_lenv } in
  case_list_ parent_lenv ty env cl

and try_catch (tb, cl) env =
  let parent_lenv = env.Env.lenv in
  let env = Env.freeze_local_env env in
  let env = block env tb in
  let after_try = env.Env.lenv in
  let env, catchl = lfold (catch parent_lenv after_try) env cl in
  let terml = List.map (fun (_, _, b) -> Nast_terminality.Terminal.block b) cl in
  let lenvl = after_try :: catchl in
  let terml = Nast_terminality.Terminal.block tb :: terml in
  LEnv.intersect_list env parent_lenv lenvl terml

and case_list_ parent_lenv ty env = function
  | [] -> env, []
  | Default b :: _ ->
      (* TODO this is wrong, should continue on to the other cases, but it
       * doesn't matter in practice since our parser won't parse default
       * anywhere but in the last position :) Should fix all of this as well
       * as totality detection for switch. *)
    let env = block env b in
    env, [Nast_terminality.Terminal.case (Default b), env.Env.lenv]
  | Case (e, b) :: rl ->
    (* TODO - we should consider handling the comparisons the same
     * way as Binop Ast.EqEq, since case statements work using ==
     * comparison rules *)

    (* The way we handle terminal/nonterminal here is not quite right, you
     * can still break the type system with things like P3131824. *)
    let ty_num = (Reason.Rnone, Tprim Nast.Tnum) in
    let ty_arraykey = (Reason.Rnone, Tprim Nast.Tarraykey) in
    let both_are_sub_types env tprim ty1 ty2 =
      (SubType.is_sub_type env tprim ty1) &&
      (SubType.is_sub_type env tprim ty2) in
    if Nast_terminality.Terminal.block b then
      let env, ty2 = expr env e in
      let env, _ =
        if (both_are_sub_types env ty_num ty ty2) ||
          (both_are_sub_types env ty_arraykey ty ty2)
        then env, ty
        else Type.unify (fst e) Reason.URnone env ty ty2 in
      let env = block env b in
      let lenv = env.Env.lenv in
      let env, rl = case_list parent_lenv ty env rl in
      env, (Nast_terminality.Terminal.case (Case (e, b)), lenv) :: rl
    else
      let env, ty2 = expr env e in
      let env, _ =
        if (both_are_sub_types env ty_num ty ty2) ||
          (both_are_sub_types env ty_arraykey ty ty2)
        then env, ty
        else Type.unify (fst e) Reason.URnone env ty ty2 in
      let env = block env b in
      case_list_ parent_lenv ty env rl

and catch parent_lenv after_try env (ety, exn, b) =
  let env = { env with Env.lenv = after_try } in
  let env = LEnv.fully_integrate env parent_lenv in
  let cid = CI ety in
  instantiable_cid (fst ety) env cid;
  let env, ety = static_class_id (fst ety) env cid in
  let env = Env.set_local env (snd exn) ety in
  let env = block env b in
  (* Only keep the local bindings if this catch is non-terminal *)
  env, env.Env.lenv

and as_expr env pe = function
  | As_v e ->
      let ty = Env.fresh_type() in
      let tvector = Tapply ((pe, SN.Collections.cTraversable), [ty]) in
      env, (Reason.Rforeach pe, tvector)
  | As_kv (e1, e2) ->
      let ty1 = Env.fresh_type() in
      let ty2 = Env.fresh_type() in
      let tmap = Tapply ((pe, SN.Collections.cKeyedTraversable), [ty1; ty2]) in
      env, (Reason.Rforeach pe, tmap)
  | Await_as_v _ ->
      let ty = Env.fresh_type() in
      let tvector = Tapply ((pe, SN.Classes.cAsyncIterator), [ty]) in
      env, (Reason.Rasyncforeach pe, tvector)
  | Await_as_kv _ ->
      let ty1 = Env.fresh_type() in
      let ty2 = Env.fresh_type() in
      let tmap = Tapply ((pe, SN.Classes.cAsyncKeyedIterator), [ty1; ty2]) in
      env, (Reason.Rasyncforeach pe, tmap)

and bind_as_expr env ty aexpr =
  let env, ety = Env.expand_type env ty in
  match ety with
  | _, Tapply ((p, class_id), [ty2]) ->
      (match aexpr with
      | As_v ev
      | Await_as_v (_, ev) -> fst (assign p env ev ty2)
      | As_kv ((_, Lvar (_, k)), ev)
      | Await_as_kv (_, (_, Lvar (_, k)), ev) ->
          let env = Env.set_local env k (Reason.Rnone, Tmixed) in
          fst (assign p env ev ty2)
      | _ -> (* TODO Probably impossible, should check that *)
          env
      )
  | _, Tapply ((p, class_id), [ty1; ty2]) ->
      (match aexpr with
      | As_v ev
      | Await_as_v (_, ev) -> fst (assign p env ev ty2)
      | As_kv ((_, Lvar (_, k)), ev)
      | Await_as_kv (_, (_, Lvar (_, k)), ev) ->
          let env = Env.set_local env k ty1 in
          fst (assign p env ev ty2)
      | _ -> (* TODO Probably impossible, should check that *)
          env
      )
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _
    | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
    | Taccess (_, _, _)) -> assert false

and expr env e =
  raw_expr false env e

and raw_expr in_cond env e =
  debug_last_pos := fst e;
  let env, ty = expr_ in_cond false env e in
  if !accumulate_types
  then begin
    type_acc := (fst e, Typing_expand.fully_expand env ty) :: !type_acc
  end;
  TUtils.save_infer env (fst e) ty;
  env, ty

and lvalue env e =
  expr_ false true env e

and expr_ in_cond is_lvalue env (p, e) =
  match e with
  | Any -> env, (Reason.Rwitness p, Tany)
  | Array [] -> env, (Reason.Rwitness p, Tarray (None, None))
  | Array (x :: rl as l) ->
      check_consistent_fields x rl;
      let env, value = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, values =
        fold_left_env (apply_for_env_fold array_field_value) env [] l in
      let has_unknown = List.exists (fun (_, ty) -> ty = Tany) values in
      let env, values =
        fold_left_env (apply_for_env_fold TUtils.unresolved) env [] values in
      let unify_value = Type.unify p Reason.URarray_value in
      let env, value =
        if has_unknown (* If one of the values comes from PHP land,
                        * we have to be conservative and consider that
                        * we don't know what the type of the values are.
                        *)
        then env, (Reason.Rnone, Tany)
        else fold_left_env unify_value env value values
      in
      (match x with
      | Nast.AFvalue _ ->
          env, (Reason.Rwitness p, Tarray (Some value, None))
      | Nast.AFkvalue _ ->
          let env, key = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
          let env, keys =
            fold_left_env (apply_for_env_fold array_field_key) env [] l in
          let env, keys =
            fold_left_env (apply_for_env_fold TUtils.unresolved) env [] keys in
          let unify_key = Type.unify p Reason.URarray_key in
          let env, key = fold_left_env unify_key env key keys in
          env, (Reason.Rwitness p, Tarray (Some key, Some value))
      )
  | ValCollection (name, el) ->
      let env, x = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, tyl = lmap expr env el in
      let env, tyl = lfold TUtils.unresolved env tyl in
      let env, v = fold_left_env (Type.unify p Reason.URvector) env x tyl in
      let tvector = Tapply ((p, name), [v]) in
      let ty = Reason.Rwitness p, tvector in
      env, ty
  | KeyValCollection (name, l) ->
      let kl, vl = List.split l in
      let env, kl = lfold expr env kl in
      let env, vl = lfold expr env vl in
      let env, k = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, v = TUtils.in_var env (Reason.Rnone, Tunresolved []) in
      let env, kl = lfold TUtils.unresolved env kl in
      let env, k = fold_left_env (Type.unify p Reason.URkey) env k kl in
      let env, vl = lfold TUtils.unresolved env vl in
      let env, v = fold_left_env (Type.unify p Reason.URvalue) env v vl in
      let ty = Tapply ((p, name), [k; v])
      in
      env, (Reason.Rwitness p, ty)
  | Clone e -> expr env e
  | This when Env.is_static env ->
      Errors.this_in_static p;
      env, (Reason.Rwitness p, Tany)
  | This ->
      let r, _ = Env.get_self env in
      if r = Reason.Rnone
      then Errors.this_var_outside_class p;
      let env, (_, ty) = Env.get_local env this in
      let r = Reason.Rwitness p in
      let ty = (r, ty) in
      let ty = r, Tgeneric (SN.Typehints.this, Some ty) in
      env, ty
  | Assert (AE_assert e) ->
      let env = condition env true e in
      env, (Reason.Rwitness p, Tprim Tvoid)
  | Assert (AE_invariant_violation (e, el)) ->
      let env, _ = lfold expr env el in
      let env, ty = expr env e in
      let string = Reason.Rwitness (fst e), Tprim Tstring in
      let env, k = Type.unify (fst e) Reason.URparam env ty string in
      env, (Reason.Rwitness p, Tprim Tvoid)
  | Assert (AE_invariant (e1, e2, el)) ->
      let env, _ = lfold expr env el in
      let env, _ = expr env e1 in
      let env = condition env true e1 in
      let env, ty = expr env e2 in
      let string = Reason.Rwitness (fst e2), Tprim Tstring in
      let env, k = Type.unify (fst e2) Reason.URparam env ty string in
      env, (Reason.Rwitness p, Tprim Tvoid)
  | True
  | False ->
      env, (Reason.Rwitness p, Tprim Tbool)
  | Int _ ->
      env, (Reason.Rwitness p, Tprim Tint)
  | Float _ ->
      env, (Reason.Rwitness p, Tprim Tfloat)
  | Null ->
      let ty = Env.fresh_type() in
      env, (Reason.Rwitness p, Toption ty)
  | String _ ->
      env, (Reason.Rwitness p, Tprim Tstring)
  | String2 (idl, _) ->
      let env = string2 env idl in
      env, (Reason.Rwitness p, Tprim Tstring)
  | Fun_id x ->
      Typing_hooks.dispatch_id_hook x env;
      fun_type_of_id env x
  | Id ((cst_pos, cst_name) as id) ->
      Typing_hooks.dispatch_id_hook id env;
      (match Env.get_gconst env cst_name with
      | None when Env.is_strict env ->
          Errors.unbound_global cst_pos;
          env, (Reason.Rwitness cst_pos, Tany)
      | None ->
          env, (Reason.Rnone, Tany)
      | Some ty ->
          env, ty
      )
  | Method_id (instance, meth) ->
    (* Method_id is used when creating a "method pointer" using the magic
     * inst_meth function.
     *
     * Typing this is pretty simple, we just need to check that instance->meth
     * is public+not static and then return its type.
     *)
    let env, ty1 = expr env instance in
    let env, result, vis =
      obj_get_with_visibility ~is_method:true ~nullsafe:None env ty1
                              meth (fun x -> x) in
    let has_lost_info = Env.FakeMembers.is_invalid env instance (snd meth) in
    if has_lost_info
    then
      let name = "the method "^snd meth in
      let env, result = Env.lost_info name ISet.empty env result in
      env, result
    else
      begin
        (match vis with
        | Some (method_pos, Vprivate _) ->
            Errors.private_inst_meth method_pos p
        | Some (method_pos, Vprotected _) ->
            Errors.protected_inst_meth method_pos p
        | _ -> ()
        );
        env, result
      end
  | Method_caller ((pos, class_name) as pos_cname, meth_name) ->
    (* meth_caller('X', 'foo') desugars to:
     * $x ==> $x->foo()
     *)
    let class_ = Env.get_class env class_name in
    (match class_ with
    | None -> unbound_name env pos_cname
    | Some class_ ->
        let env, params = lfold begin fun env x ->
          TUtils.in_var env (Reason.Rwitness p, Tunresolved [])
        end env class_.tc_tparams in
        let obj_type = Reason.Rwitness p, Tapply (pos_cname, params) in
        (* We need to instantiate the object because it could have
         * type parameters.
         *)
        let env, fty =
          obj_get ~is_method:true ~nullsafe:None env obj_type
                  meth_name (fun x -> x) in
        (match fty with
        | reason, Tfun fty ->
            (* We are creating a fake closure:
             * function<T as Class>(T $x): return_type_of(Class:meth_name)
             *)
            let tparam = Ast.Invariant, pos_cname, Some obj_type in
            let param = Tgeneric (class_name, Some obj_type) in
            let param = Reason.Rwitness pos, param in
            let fty = { fty with
                        ft_tparams = [tparam];
                        ft_params = [None, param] } in
            let env, fty = Inst.instantiate_ft env fty in
            let caller = {
              ft_pos = pos;
              ft_unsafe = false;
              ft_abstract = false;
              ft_arity = Fstandard (1, 1);
              ft_tparams = [];
              ft_params = fty.ft_params;
              ft_ret = fty.ft_ret;
            } in
            env, (reason, Tfun caller)
        | x ->
            (* This can happen if the method lives in PHP *)
            env, (Reason.Rwitness pos, Tany)
        )
    )
  | Smethod_id (c, meth) ->
    (* Smethod_id is used when creating a "method pointer" using the magic
     * class_meth function.
     *
     * Typing this is pretty simple, we just need to check that c::meth is
     * public+static and then return its type.
     *)
    let class_ = Env.get_class env (snd c) in
    (match class_ with
    | None ->
      (* The class given as a static string was not found. *)
      unbound_name env c
    | Some class_ ->
      let smethod = Env.get_static_member true env class_ (snd meth) in
      (match smethod with
      | None -> (* The static method wasn't found. *)
        smember_not_found p ~is_const:false ~is_method:true env class_ (snd meth);
        env, (Reason.Rnone, Tany)
      | Some smethod ->
        (match smethod.ce_type, smethod.ce_visibility with
        | (r, (Tfun _ as ty)), Vpublic ->
          env, (r, ty)
        | (r, Tfun _), Vprivate _ ->
          Errors.private_class_meth (Reason.to_pos r) p;
            env, (r, Tany)
        | (r, Tfun _), Vprotected _ ->
          Errors.protected_class_meth (Reason.to_pos r) p;
            env, (r, Tany)
        | _, _ ->
          (* If this assert fails, we have a method which isn't callable. *)
          assert false
        )
      )
    )
  | Lvar ((_, x) as id) ->
      Typing_hooks.dispatch_lvar_hook id env;
      let env, x = Env.get_local env x in
      env, x
  | List el ->
      let env, tyl = lmap expr env el in
      let ty = Reason.Rwitness p, Ttuple tyl in
      env, ty
  | Pair (e1, e2) ->
      let env, ty1 = expr env e1 in
      let env, ty2 = expr env e2 in
      let ty = Reason.Rwitness p, Tapply ((p, SN.Collections.cPair), [ty1; ty2]) in
      env, ty
  | Expr_list el ->
      let env, tyl = lmap expr env el in
      let ty = Reason.Rwitness p, Ttuple tyl in
      env, ty
  | Array_get (e, None) ->
      let env, ty1 = expr env e in
      array_append is_lvalue p env ty1
  | Array_get (e1, Some e2) ->
      let env, ty1 = expr env e1 in
      let env, ty1 = TUtils.fold_unresolved env ty1 in
      let env, ety1 = Env.expand_type env ty1 in
      let env, ty2 = expr env e2 in
      array_get is_lvalue p env ty1 ety1 e2 ty2
  | Call (Cnormal, (_, Id (_, hh_show)), [x], [])
      when hh_show = SN.PseudoFunctions.hh_show ->
      let env, ty = expr env x in
      Env.debug env ty;
      env, Env.fresh_type()
  | Call (call_type, (_, fun_expr as e), el, uel) ->
      let env, result = dispatch_call p env call_type e el uel in
      let env = Env.forget_members env p in
      env, result
  | Binop (Ast.Eq (Some op), e1, e2) ->
      let e2 = p, Binop (op, e1, e2) in
      let env, ty = raw_expr in_cond env (p, Binop (Ast.Eq None, e1, e2)) in
      env, ty
  | Binop (Ast.Eq None, e1, e2) ->
      let env, ty2 = raw_expr in_cond env e2 in
      assign p env e1 ty2
  | Binop ((Ast.AMpamp | Ast.BArbar as c), e1, e2) ->
      let c = c = Ast.AMpamp in
      let lenv = env.Env.lenv in
      let env, ty1 = expr env e1 in
      let env = condition env c e1 in
      let env, ty2 = raw_expr in_cond env e2 in
      let env = { env with Env.lenv = lenv } in
      env, (Reason.Rlogic_ret p, Tprim Tbool)
  | Binop (bop, e1, e2) ->
      let env, ty1 = raw_expr in_cond env e1 in
      let env, ty2 = raw_expr in_cond env e2 in
      let env, ty = binop in_cond p env bop (fst e1) ty1 (fst e2) ty2 in
      env, ty
  | Unop (uop, e) ->
      let env, ty = raw_expr in_cond env e in
      unop p env uop ty
  | Eif (c, e1, e2) ->
      let env, tyc = raw_expr in_cond env c in
      Typing_async.enforce_not_awaitable env (fst c) tyc;
      let lenv = env.Env.lenv in
      let env  = condition env true c in
      let env, ty1 = match e1 with
      | None ->
          non_null env tyc
      | Some e1 ->
          expr env e1
      in
      let env  = { env with Env.lenv = lenv } in
      let env  = condition env false c in
      let env, ty2 = expr env e2 in
      (* This is a shortened form of what we do in Typing_lenv.intersect. The
       * latter takes local environments as arguments, but our types here
       * aren't assigned to local variables in an environment *)
      let env, ty1 = TUtils.unresolved env ty1 in
      let env, ty2 = TUtils.unresolved env ty2 in
      Unify.unify env ty1 ty2
  | Class_const (cid, mid) -> class_const env p (cid, mid)
  | Class_get (x, (_, y))
      when Env.FakeMembers.get_static env x y <> None ->
        let env, local = Env.FakeMembers.make_static p env x y in
        let local = p, Lvar (p, local) in
        expr env local
  | Class_get (cid, mid) ->
      Typing_utils.process_static_find_ref cid mid;
      let env, cty = static_class_id p env cid in
      let env, cty = Env.expand_type env cty in
      let env, ty = class_get ~is_method:false ~is_const:false env cty mid cid in
      if Env.FakeMembers.is_static_invalid env cid (snd mid)
      then
        let fake_name = Env.FakeMembers.make_static_id cid (snd mid) in
        let env, ty = Env.lost_info fake_name ISet.empty env ty in
        env, ty
      else env, ty
  | Obj_get (e, (_, Id (_, y)), _)
      when Env.FakeMembers.get env e y <> None ->
        let env, local = Env.FakeMembers.make p env e y in
        let local = p, Lvar (p, local) in
        expr env local
  | Obj_get (e1, (_, Id m), nullflavor) ->
      let nullsafe =
        (match nullflavor with
          | OG_nullthrows -> None
          | OG_nullsafe -> Some p
        ) in
      let env, ty1 = expr env e1 in
      let env, result =
        obj_get ~is_method:false ~nullsafe:nullsafe env ty1 m (fun x -> x) in
      let has_lost_info = Env.FakeMembers.is_invalid env e1 (snd m) in
      if has_lost_info
      then
        let name = "the member "^snd m in
        let env, result = Env.lost_info name ISet.empty env result in
        env, result
      else env, result
  | Obj_get (e1, _, _) ->
      let env, _ = expr env e1 in
      env, (Reason.Rwitness p, Tany)
  | Yield_break ->
      env, (Reason.Rwitness p, Tany)
  | Yield af ->
      let env, key = yield_field_key env af in
      let env, value = yield_field_value env af in
      let send = Env.fresh_type () in
      let rty = match Env.get_fn_kind env with
        | FGenerator ->
            Reason.Ryield_gen p,
            Tapply ((p, SN.Classes.cGenerator), [key; value; send])
        | FAsyncGenerator ->
            Reason.Ryield_asyncgen p,
            Tapply ((p, SN.Classes.cAsyncGenerator), [key; value; send])
        | _ -> assert false in (* Naming should never allow this *)
      let env =
        Type.sub_type p (Reason.URyield) env (Env.get_return env) rty in
      let env = Env.forget_members env p in
      env, (Reason.Ryield_send p, Toption send)
  | Await e ->
      let env, rty = expr env e in
      Async.overload_extract_from_awaitable env p rty
  | Special_func func -> special_func env p func
  | New (c, el, uel) ->
      Typing_hooks.dispatch_new_id_hook c env;
      Typing_utils.process_static_find_ref c (p, SN.Members.__construct);
      let check_not_abstract = true in
      let env, ty = new_object ~check_not_abstract p env c el uel in
      let env = Env.forget_members env p in
      env, ty
  | Cast ((_, Harray (None, None)), _) when Env.is_strict env ->
      Errors.array_cast p;
      env, (Reason.Rwitness p, Tany)
  | Cast (ty, e) ->
      let env, _ = expr env e in
      Typing_hint.hint env ty
  | InstanceOf (e1, e2) ->
    let env = instanceof_in_env p env e1 e2 in
    env, (Reason.Rwitness p, Tprim Tbool)
  | Efun (f, idl) ->
      NastCheck.fun_ env f (Nast.assert_named_body f.f_body);
      let env, ft = fun_decl_in_env env f in
      (* check for recursive function calls *)
      let anon = anon_make env.Env.lenv p f in
      let env, anon_id = Env.add_anonymous env anon in
      ignore (anon env ft.ft_params);
      env, (Reason.Rwitness p, Tanon (ft.ft_arity, anon_id))
  | Xml (sid, attrl, el) ->
      let env, obj = expr env (fst sid, New (CI sid, [], [])) in
      let env, attr_tyl = lfold expr env (List.map snd attrl) in
      let env, body = lfold expr env el in
      (* We don't perform any check on XHP body right now, because
       * it is unclear, what is allowed ...
       * I keep the code here, because one day, we might want to be
       * more restrictive.
       *)
      (*
        let env, _ = lfold2 begin fun env e ty ->
        let p = fst e in
        let r = Reason.Rxhp p in
        let xhp_ty = (r, (Toption (r, Tapply ((p, "XHP"), [])))) in
        let env = Type.sub_type p Reason.URxhp env xhp_ty ty in
        env, ()
        end env el body in
       *)
      env, obj
  | Shape fdm ->
      let env, fdm = ShapeMap.map_env expr env fdm in
      (* allow_inter adds a type-variable *)
      let env, fdm = ShapeMap.map_env TUtils.unresolved env fdm in
      let env = check_shape_keys_validity env p (ShapeMap.keys fdm) in
      env, (Reason.Rwitness p, Tshape fdm)

and class_const env p = function
  | (CIparent, mid) ->
      let env, cty = static_class_id p env CIparent in
      obj_get ~is_method:false ~nullsafe:None env cty mid (fun x -> x)
  | (cid, mid) ->
      Typing_utils.process_static_find_ref cid mid;
      let env, cty = static_class_id p env cid in
      let env, cty = Env.expand_type env cty in
      class_get ~is_method:false ~is_const:true env cty mid cid

(*****************************************************************************)
(* Anonymous functions. *)
(*****************************************************************************)

and anon_bind_param params env (param_name, ty as pname_ty) =
  match !params with
  | [] ->
      (* This code cannot be executed normally, because the arity is wrong
       * and it will error later. Bind as many parameters as we can and carry
       * on. *)
      env
  | param :: paraml ->
      params := paraml;
      match param.param_hint with
      | Some h ->
          let env, h = Typing_hint.hint env h in
          let pos = Reason.to_pos (fst ty) in
          let env = Type.sub_type pos Reason.URparam env h ty in
          (* Closures are allowed to have explicit type-hints. When
           * that is the case we should check that the argument passed
           * is compatible with the type-hint.
           * The body of the function should be type-checked with the
           * hint and not the type of the argument passed.
           * Otherwise it leads to strange results where
           * foo(?string $x = null) is called with a string and fails to
           * type-check. If $x is a string instead of ?string, null is not
           * subtype of string ...
           *)
          bind_param env (param_name, h) param
      | None -> bind_param env pname_ty param

and anon_bind_opt_param env param =
  match param.param_expr with
  | None ->
      let ty = Reason.Rnone, Tany in
      bind_param env (None, ty) param
  | Some default ->
      let env, ty = expr env default in
      bind_param env (None, ty) param

and anon_check_param env param =
  match param.param_hint with
  | None -> env
  | Some hty ->
      let env, hty = Typing_hint.hint env hty in
      let env, paramty = Env.get_local env (snd param.param_id) in
      let hint_pos = Reason.to_pos (fst hty) in
      let env = Type.sub_type hint_pos Reason.URhint env hty paramty in
      env

and anon_make anon_lenv p f =
  let is_typing_self = ref false in
  fun env tyl ->
    if !is_typing_self
    then begin
      Errors.anonymous_recursive p;
      env, (Reason.Rwitness p, Tany)
    end
    else begin
      is_typing_self := true;
      Env.anon anon_lenv env begin fun env ->
        let params = ref f.f_params in
        let env = List.fold_left (anon_bind_param params) env tyl in
        let env = List.fold_left anon_bind_opt_param env !params in
        let env = List.fold_left anon_check_param env f.f_params in
        let env, hret =
          match f.f_ret with
          | None -> TUtils.in_var env (Reason.Rnone, Tunresolved [])
          | Some x -> Typing_hint.hint env x in
        let env = Env.set_return env hret in
        let env = Env.set_fn_kind env f.f_fun_kind in
        let body = Nast.assert_named_body f.f_body in
        let env = block env body in
        let env =
          if Nast_terminality.Terminal.block body || f.f_unsafe || !auto_complete
          then env
          else fun_implicit_return env p hret body f.f_fun_kind
        in
        is_typing_self := false;
        env, hret
      end
    end

(*****************************************************************************)
(* End of anonymous functions. *)
(*****************************************************************************)

and special_func env p func =
  let env, ty = (match func with
  | Gena e ->
      let env, ety = expr env e in
      Async.gena env p ety
  | Genva el ->
      let env, etyl = lmap expr env el in
      Async.genva env p etyl
  | Gen_array_rec e ->
      let env, ety = expr env e in
      Async.gen_array_rec env p ety
  | Gen_array_va_rec el ->
      let env, etyl = lmap expr env el in
      Async.gen_array_va_rec env p etyl
  ) in
  env, (Reason.Rwitness p, Tapply ((p, SN.Classes.cAwaitable), [ty]))

and new_object ~check_not_abstract p env c el uel =
  let env, class_ = class_id p env c in
  (match class_ with
  | None ->
      let _ = lmap expr env el in
      let _ = lmap expr env uel in
      env, (Reason.Runknown_class p, Tobject)
  | Some (cname, class_) ->
      if check_not_abstract && class_.tc_abstract && c <> CIstatic
      then Errors.uninstantiable_class p class_.tc_pos class_.tc_name;
      let env, params = lfold begin fun env x ->
        TUtils.in_var env (Reason.Rnone, Tunresolved [])
      end env class_.tc_tparams in
      let env =
        if SSet.mem "XHP" class_.tc_extends then env else
        let env = call_construct p env class_ params el uel in
        env
      in
      let obj_type = Reason.Rwitness p, Tapply (cname, params) in
      match c with
      | CIstatic ->
        if not (snd class_.tc_construct) then
          Errors.new_static_inconsistent p cname
        else ();
        env, (Reason.Rwitness p, Tgeneric (SN.Typehints.this, Some obj_type))
      | CIparent ->
        (match (fst class_.tc_construct) with
          | Some ce ->
            ignore (check_abstract_parent_meth SN.Members.__construct p ce.ce_type)
          | None -> ());
        env, obj_type
      | _ -> env, obj_type
  )

and instanceof_naming = function
  | (_, Id (pos_c, name_c)) ->
    let cid = match name_c with
      | x when x = SN.Classes.cParent -> CIparent
      | x when x = SN.Classes.cSelf   -> CIself
      | x when x = SN.Classes.cStatic -> CIstatic
      | _ -> CI (pos_c, name_c)
    in Some cid
  | (_, Lvar var) as e ->
    let cid = CIvar e in
    Some cid
  | _ -> None

and instanceof_in_env p (env:Env.env) (e1:Nast.expr) (e2:Nast.expr) =
  let env, _ = expr env e1 in
  match instanceof_naming e2 with
    | Some cid ->
      let () = instantiable_cid p env cid in
      env
    | None ->
      let env, _ = expr env e2 in
      env

and instantiable_cid p env cid =
  let env, class_ = class_id p env cid in
  (match class_ with
    | Some ((pos, name), class_) when class_.tc_kind = Ast.Ctrait ->
      (match cid with
        | CI _ -> Errors.uninstantiable_class pos class_.tc_pos name
        | CIstatic | CIparent | CIself -> ()
        | CIvar _ -> ())
    | Some ((pos, name), class_) when class_.tc_kind = Ast.Cabstract && class_.tc_final ->
      Errors.uninstantiable_class pos class_.tc_pos name
    | None | Some _ -> ())

(* While converting code from PHP to Hack, some arrays are used
 * as tuples. Example: array('', 0). Since the elements have
 * incompatible types, it should be a tuple. However, while migrating
 * code, it is more flexible to allow it in partial.
 *
 * This probably isn't a good idea and should just use ty2 in all cases, but
 * FB www has about 50 errors if you just use ty2 -- not impossible to clean
 * up but more work right now than I want to do. Also it probably affects open
 * source code too, so this may be a nice small test case for our upcoming
 * migration/upgrade strategy.
 *)
and convert_array_as_tuple p env ty2 =
  let r2 = fst ty2 in
  if not (Env.is_strict env) && TUtils.is_array_as_tuple env ty2
  then env, (r2, Tany)
  else env, ty2

and shape_field_name p = function
  | String name -> SFlit name
  | Class_const (CI x, y) -> SFclass_const (x, y)
  | _ -> Errors.invalid_shape_field_name p;
    SFlit (p, "")

and shape_field_pos = function
  | SFlit (p, _) -> p
  | SFclass_const ((cls_pos, _), (member_pos, _)) -> Pos.btw cls_pos member_pos

and check_shape_keys_validity env pos keys =
    (* If the key is a class constant, get its class name and type. *)
    let get_field_info env key =
      let key_pos = shape_field_pos key in
      (match key with
        | SFlit _ -> env, key_pos, None
        | SFclass_const (_, cls as x, y) ->
          let env, ty = class_const env pos (CI x, y) in
          let env = Typing_enum.check_valid_array_key_type
            Errors.invalid_shape_field_type ~allow_any:false
            env key_pos ty in
          env, key_pos, Some (cls, ty))
    in

    let check_field witness_pos witness_info env key =
      let env, key_pos, key_info = get_field_info env key in
      (match witness_info, key_info with
        | Some _, None ->
          Errors.invalid_shape_field_literal key_pos witness_pos; env
        | None, Some _ ->
          Errors.invalid_shape_field_const key_pos witness_pos; env
        | None, None -> env
        | Some (cls1, ty1), Some (cls2, ty2) ->
          if cls1 <> cls2 then
            Errors.shape_field_class_mismatch
              key_pos witness_pos (strip_ns cls2) (strip_ns cls1);
          (* We want to use our own error message here instead of the normal
           * unification one. *)
          Errors.try_
            (fun () -> Unify.iunify env ty1 ty2)
            (fun _ ->
              Errors.shape_field_type_mismatch
                key_pos witness_pos
                (Typing_print.error (snd ty2)) (Typing_print.error (snd ty1));
              env))
    in

    (* Sort the keys by their positions since the error messages will make
     * more sense if we take the one that appears first as canonical and if
     * they are processed in source order. *)
    let cmp_keys x y = Pos.compare (shape_field_pos x) (shape_field_pos y) in
    let keys = List.sort cmp_keys keys in

    match keys with
      | [] -> env
      | witness :: rest_keys ->
        let env, pos, info = get_field_info env witness in
        List.fold_left (check_field pos info) env rest_keys

and assign p env e1 ty2 =
  let env, ty2 = convert_array_as_tuple p env ty2 in
  match e1 with
  | (_, Lvar (_, x)) ->
      let env = Env.set_local env x ty2 in
      env, ty2
  | (_, List el) ->
      let env, folded_ty2 = TUtils.fold_unresolved env ty2 in
      let env, folded_ety2 = Env.expand_type env folded_ty2 in
      (match folded_ety2 with
      | _, Taccess (_, _, _) ->
          let env, ty2 = TAccess.expand env folded_ety2 in
          assign p env e1 ty2
      | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
          let env, ty2 = Typing_tdef.expand_typedef env r x argl in
          assign p env e1 ty2
      | r, Tapply ((_, x), [elt_type])
        when x = SN.Collections.cVector || x = SN.Collections.cImmVector ->
          let env, _ = lfold begin fun env e ->
            assign (fst e) env e elt_type
          end env el in
          env, ty2
      | r, Tarray (Some elt_type, None) ->
          let env, _ = lfold begin fun env e ->
            assign (fst e) env e elt_type
          end env el in
          env, ty2
      | r, Tarray (None, None)
      | r, Tany ->
          let env, _ = lfold begin fun env e ->
            assign (fst e) env e (r, Tany)
          end env el in
          env, ty2
      | r, Tapply ((_, coll), [ty1; ty2]) when coll = SN.Collections.cPair ->
          (match el with
          | [x1; x2] ->
              let env, _ = assign p env x1 ty1 in
              let env, _ = assign p env x2 ty2 in
              env, (Reason.Rwitness (fst e1), Tprim Tvoid)
          | _ ->
              Errors.pair_arity p;
              env, (r, Tany)
          )
      | r, Ttuple tyl ->
          let size1 = List.length el in
          let size2 = List.length tyl in
          let p1 = fst e1 in
          let p2 = Reason.to_pos r in
          if size1 <> size2
          then begin
            Errors.tuple_arity p2 size2 p1 size1;
            env, (r, Tany)
          end
          else
            let env = List.fold_left2 begin fun env lvalue ty2 ->
              fst (assign p env lvalue ty2)
            end env el tyl in
            env, (Reason.Rwitness p1, Tprim Tvoid)
      | _, (Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
        | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tanon (_, _)
        | Tunresolved _ | Tapply (_, _) | Tobject | Tshape _) ->
          assign_simple p env e1 ty2
      )
  | _, Class_get _
  | _, Obj_get _ ->
      let env, ty1 = lvalue env e1 in
      let env, ety1 = Env.expand_type env ty1 in
      let fake_members, locals as lenv = env.Env.lenv in
      let no_fakes = Env.empty_fake_members, locals in
      (* In this section, we check that the assignment is compatible with
       * the real type of a member. Remember that members can change
       * type (cf fake_members). But when we assign a value to $this->x,
       * we want to make sure that the type assign to $this->x is compatible
       * with the actual type hint. In this portion of the code, type-check
       * the assignment in an envrionment without fakes, and therefore
       * check that the assignment is compatible with the type of
       * the member.
       *)
      let env, real_type = expr { env with Env.lenv = no_fakes } e1 in
      let env, exp_real_type = Env.expand_type env real_type in
      let env = { env with Env.lenv = lenv } in
      let env, ety2 = Env.expand_type env ty2 in
      let real_type_list =
        match exp_real_type with
        | _, Tunresolved tyl -> tyl
        | ty -> [ty]
      in
      let env = List.fold_left begin fun env real_type ->
        Type.sub_type p (Reason.URassign) env real_type ety2
      end env real_type_list in
      (match e1 with
      | _, Obj_get ((_, This | _, Lvar _ as obj),
                    (_, Id (_, member_name)),
                    _) ->
          let env, local = Env.FakeMembers.make p env obj member_name in
          (match obj with
          | _, This ->
              Typing_suggest.save_member member_name env exp_real_type ty2;
          | _ -> ());
          let env = Env.set_local env local ty2 in
          env, ty2
      | _, Class_get (x, (_, y)) ->
          let env, local = Env.FakeMembers.make_static p env x y in
          let env = Env.set_local env local ty2 in
          (match x with
          | CIself
          | CIstatic ->
              Typing_suggest.save_member y env exp_real_type ty2;
          | _ -> ());
          env, ty2
      | _ -> env, ty2
      )
  | _, Array_get ((_, Lvar (_, lvar)) as shape, Some (p1, (String _ as e)))
  | _, Array_get ((_, Lvar (_, lvar)) as shape,
                  Some (p1, (Class_const (CI _, _) as e))) ->
      (* In the case of an assignment of the form $x['new_field'] = ...;
       * $x could be a shape where the field 'new_field' is not yet defined.
       * When that is the case we want to add the field to its type.
       *)
      let env, shape_ty = expr env shape in
      let field = shape_field_name p1 e in
      let env, shape_ty = TUtils.grow_shape p e1 field ty2 env shape_ty in
      let env = Env.set_local env lvar shape_ty in

      (* We still need to call assign_simple, because shape_ty could be more
       * than just a shape. It could be an unresolved type where some elements
       * are shapes and some others are not.
       *)
      assign_simple p env e1 ty2
  | _ ->
      assign_simple p env e1 ty2

and assign_simple p env e1 ty2 =
  let env, ty1 = lvalue env e1 in
  let env, ty2 = TUtils.unresolved env ty2 in
  let env = Type.sub_type p (Reason.URassign) env ty1 ty2 in
  env, ty2

and array_field_value env = function
  | Nast.AFvalue x
  | Nast.AFkvalue (_, x) -> expr env x

and yield_field_value env x = array_field_value env x

and array_field_key env = function
  | Nast.AFvalue (p, _) ->
      env, (Reason.Rwitness p, Tprim Tint)
  | Nast.AFkvalue (x, _) ->
      expr env x

and yield_field_key env = function
  | Nast.AFvalue (p, _) ->
      env, (match Env.get_fn_kind env with
        | FSync
        | FAsync -> assert false
        | FGenerator ->
            (Reason.Rwitness p, Tprim Tint)
        | FAsyncGenerator ->
            (Reason.Ryield_asyncnull p, Toption (Env.fresh_type ())))
  | Nast.AFkvalue (x, _) ->
      expr env x

and check_parent_construct pos env el uel env_parent =
  let check_not_abstract = false in
  let env, parent = new_object ~check_not_abstract pos env CIparent el uel in
  let env, _ = Type.unify pos (Reason.URnone) env env_parent parent in
  env, (Reason.Rwitness pos, Tprim Tvoid)

and call_parent_construct pos env el uel =
  let parent = Env.get_parent env in
  match parent with
    | _, Tapply (_, params) ->
      check_parent_construct pos env el uel parent
    | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
      | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon _
      | Tunresolved _ | Tobject | Tshape _
      | Taccess (_, _, _)) -> (* continue here *)
      let default = env, (Reason.Rnone, Tany) in
      match Env.get_self env with
        | _, Tapply ((_, self), _) ->
          (match Env.get_class env self with
            | Some ({tc_kind = Ast.Ctrait; tc_req_ancestors ; tc_name; _}
                       as trait) ->
              (match trait_most_concrete_req_class trait env with
                | None -> Errors.parent_in_trait pos; default
                | Some (tc_parent, parent_ty) ->
                  check_parent_construct pos env el uel parent_ty
              )
            | Some self_tc ->
              if not self_tc.tc_members_fully_known
              then () (* Don't know the hierarchy, assume it's correct *)
              else Errors.undefined_parent pos;
              default
            | None -> assert false)
        | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _)
          | Toption _ | Tprim _ | Tvar _ | Tfun _
          | Tabstract (_, _, _) | Ttuple _ | Tanon _ | Tunresolved _
          | Tobject | Tshape _
          | Taccess (_, _, _)) -> Errors.parent_outside_class pos; default

(* parent::method() in a class definition invokes the specific parent
 * version of the method ... it better be callable *)
and check_abstract_parent_meth mname pos fty =
  (match fty with
    | r, Tfun { ft_abstract = true; _ } ->
      Errors.parent_abstract_call mname pos (Reason.to_pos r)
    | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _ | Tprim _
      | Tvar _ | Tfun _ | Tapply (_, _) | Tabstract (_, _, _) | Ttuple _
      | Tanon _ | Tunresolved _ | Tobject | Tshape _
      | Taccess (_, _, _)) -> ());
  fty

(* Depending on the kind of expression we are dealing with
 * The typing of call is different.
 *)
and dispatch_call p env call_type (fpos, fun_expr as e) el uel =
  match fun_expr with
  | Id (_, pseudo_func) when pseudo_func = SN.SpecialFunctions.echo ->
      let env, _ = lfold expr env el in
      env, (Reason.Rwitness p, Tprim Tvoid)
  | Id (_, pseudo_func)
      when
        pseudo_func = SN.PseudoFunctions.isset
        || pseudo_func = SN.PseudoFunctions.empty ->
    let env, _ = lfold expr env el in
    let env, _ = lfold unpack_expr env uel in
    if Env.is_strict env then
      Errors.isset_empty_unset_in_strict p pseudo_func;
    env, (Reason.Rwitness p, Tprim Tbool)
  | Id (_, pseudo_func) when pseudo_func = SN.PseudoFunctions.unset ->
      if Env.is_strict env then
        Errors.isset_empty_unset_in_strict p pseudo_func;
      env, (Reason.Rwitness p, Tprim Tvoid)
  | Id (_, x) when SSet.mem x Naming.predef_tests ->
      let env, ty = expr env (List.hd el) in
      env, (Reason.Rwitness p, Tprim Tbool)
  | Id ((_, array_filter) as id)
      when array_filter = SN.StdlibFunctions.array_filter && el <> [] && uel = [] ->
      (* dispatch the call to typecheck the arguments *)
      let env, fty = fun_type_of_id env id in
      let env, fty = Env.expand_type env fty in
      let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
      let env, res = call p env fty el uel in
      (* but ignore the result and overwrite it with custom return type *)
      let x = List.hd el in
      let env, ty = expr env x in
      let explain_array_filter (r, t) =
        (Reason.Rarray_filter (p, r), t) in
      let get_value_type env tv =
        let env, tv = if List.length el > 1 then env, tv else non_null env tv in
        env, explain_array_filter tv in
      let rec get_array_filter_return_type env ty =
        let env, ety = Env.expand_type env ty in
        (match ety with
        | (r, Tarray (None, None)) as array_type ->
            env, array_type
        | (r, Tarray (Some tv, None)) ->
            let env, tv = get_value_type env tv in
            env, (r, Tarray (Some tv, None))
        | (r, Tunresolved x) ->
            let env, x = lmap get_array_filter_return_type env x in
            env, (r, Tunresolved x)
        | (r, Tany) ->
            env, (r, Tany)
        | (r, _) ->
            let tk, tv = Env.fresh_type(), Env.fresh_type() in
            Errors.try_
              (fun () ->
                let keyed_container = (
                  Reason.Rnone,
                  Tapply (
                    (Pos.none, SN.Collections.cKeyedContainer), [tk; tv]
                  )
                ) in
                let env = SubType.sub_type env keyed_container ety in
                let env, tv = get_value_type env tv in
                env, (r, Tarray (
                  Some (explain_array_filter tk),
                  Some tv)
                ))
              (fun _ -> Errors.try_
                (fun () ->
                  let container = (
                    Reason.Rnone,
                    Tapply (
                      (Pos.none, SN.Collections.cContainer), [tv]
                    )
                  ) in
                  let env = SubType.sub_type env container ety in
                  let env, tv = get_value_type env tv in
                  env, (r, Tarray (
                    Some (explain_array_filter (r, Tprim Tarraykey)),
                    Some tv)))
                (fun _ -> env, res)))
      in get_array_filter_return_type env ty
  | Id ((_, array_map) as x)
      when array_map = SN.StdlibFunctions.array_map && el <> [] && uel = [] ->
      let env, fty = fun_type_of_id env x in
      let env, fty = Env.expand_type env fty in
      let env, fty = match fty, el with
        | ((r_fty, Tfun fty), _::args) when args <> [] ->
          let arity = List.length args in
          (*
            Builds a function with signature:

            function<T1, ..., Tn, Tr>(
              (function(T1, ..., Tn):Tr),
              Container<T1>,
              ...,
              Container<Tn>,
            ): R

            where R is constructed by build_output_container applied to Tr
          *)
          let build_function (build_output_container:(ty -> ty)) : ty  =
            let vars = List.map (fun _ -> Env.fresh_type()) args in
            let tr = Env.fresh_type() in
            let f = (None, (
              r_fty,
              Tfun {
                ft_pos = fty.ft_pos;
                ft_unsafe = false; ft_abstract = false;
                ft_arity = Fstandard (arity, arity); ft_tparams = [];
                ft_params = List.map (fun x -> (None, x)) vars;
                ft_ret = tr;
              }
            )) in
            let containers = List.map (fun var ->
              (None,
                (r_fty,
                  Tapply ((fty.ft_pos, SN.Collections.cContainer), [var])
                )
              )
            ) vars in
            (r_fty, Tfun {fty with
              ft_arity = Fstandard (arity+1, arity+1);
              ft_params = f::containers;
              ft_ret =  build_output_container tr;
            }) in

          (*
            Takes a Container type and returns a function that can "pack" a type
            into an array of appropriate shape, preserving the key type, i.e.:
            array                 -> f, where f R = array
            array<X>              -> f, where f R = array<R>
            array<X, Y>           -> f, where f R = array<X, R>
            Vector<X>             -> f  where f R = array<R>
            KeyedContainer<X, Y>  -> f, where f R = array<X, R>
            Container<X>          -> f, where f R = array<arraykey, R>
            X                     -> f, where f R = Y
          *)
          let rec build_output_container
            (env:Env.env) (x:ty) : (Env.env * (ty -> ty)) =
            let env, x = Env.expand_type env x in (match x with
              | (_, Tarray (None, None)) as array_type ->
                env, (fun _ -> array_type)
              | (r, Tarray (_, None)) ->
                env, (fun tr -> (r, Tarray (Some(tr), None)) )
              | ((_, Tany) as any) ->
                env, (fun _ -> any)
              | (r, Tunresolved x) ->
                let env, x = lmap build_output_container env x in
                env, (fun tr -> (r, Tunresolved (List.map (fun f -> f tr) x)))
              | (r, _) ->
                let tk, tv = Env.fresh_type(), Env.fresh_type() in
                let try_vector env =
                  let vector = (
                    r_fty,
                    Tapply (
                      (fty.ft_pos, SN.Collections.cConstVector), [tv]
                    )
                  ) in
                  let env = SubType.sub_type env vector x in
                  env, (fun tr -> (r, Tarray (
                    Some(tr),
                    None)
                  )) in
                let try_keyed_container env =
                  let keyed_container = (
                    r_fty,
                    Tapply (
                      (fty.ft_pos, SN.Collections.cKeyedContainer), [tk; tv]
                    )
                  ) in
                  let env = SubType.sub_type env keyed_container x in
                  env, (fun tr -> (r, Tarray (
                    Some(tk),
                    Some(tr))
                  )) in
                let try_container env =
                  let container = (
                    r_fty,
                    Tapply (
                      (fty.ft_pos, SN.Collections.cContainer), [tv]
                    )
                  ) in
                  let env = SubType.sub_type env container x in
                  env, (fun tr -> (r, Tarray (
                    Some((r, Tprim Tarraykey)),
                    Some(tr)))) in
                Errors.try_
                  (fun () ->
                    try_vector  env)
                  (fun _ -> Errors.try_
                    (fun () ->
                      try_keyed_container env)
                    (fun _ -> Errors.try_
                      (fun () ->
                        try_container env)
                      (fun _ -> env, (fun _ -> (Reason.Rwitness p, Tany)))))) in
          (*
            Single argument calls preserve the key type, multi argument
            calls always return an array<Tr>
          *)
          (match args with
            | [x] ->
              let env, x = expr env x in
              let env, output_container = build_output_container env x in
              env, build_function output_container
            | _ ->
              env, build_function (fun tr ->
                (r_fty, Tarray(Some(tr), None))))
        | _ -> env, fty in
      let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
      call p env fty el []
  | Id ((_, "\\idx") as id) ->
      (* Directly call get_fun so that we can muck with the type before
       * instantiation -- much easier to work in terms of Tgeneric Tk/Tv than
       * trying to figure out which Tvar is which. *)
      let fty = match Env.get_fun env (snd id) with
        | Some fty -> fty
        | None -> assert false in
      let param1, (name2, (r2, _)), (name3, (r3, _)) = match fty.ft_params with
        | [param1; param2; param3] -> param1, param2, param3
        | _ -> assert false in
      let params, ret = match List.length el with
        | 2 ->
          let param2 = (name2, (r2, Toption (r2, Tgeneric ("Tk", None)))) in
          let rret = fst fty.ft_ret in
          let ret = (rret, Toption (rret, Tgeneric ("Tv", None))) in
          [param1; param2], ret
        | 3 ->
          let param2 = (name2, (r2, Tgeneric ("Tk", None))) in
          let param3 = (name3, (r3, Tgeneric ("Tv", None))) in
          let ret = (fst fty.ft_ret, Tgeneric ("Tv", None)) in
          [param1; param2; param3], ret
        | _ -> fty.ft_params, fty.ft_ret in
      let fty = { fty with ft_params = params; ft_ret = ret } in
      let env, fty = Inst.instantiate_ft env fty in
      let tfun = Reason.Rwitness fty.ft_pos, Tfun fty in
      call p env tfun el []
  | Class_const (CIparent, (_, construct))
    when construct = SN.Members.__construct ->
      call_parent_construct p env el uel
  | Class_const (CIparent, m) ->
      let env, ty1 = static_class_id p env CIparent in
      if Env.is_static env
      then begin
        (* in static context, you can only call parent::foo() on static
         * methods *)
        let env, fty = class_get ~is_method:true ~is_const:false env ty1 m CIparent in
        let env, fty = Env.expand_type env fty in
        let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
        let fty = check_abstract_parent_meth (snd m) p fty in
        call p env fty el uel
      end
      else begin
        (* in instance context, you can call parent:foo() on static
         * methods as well as instance methods *)
        (match class_contains_smethod env ty1 m with
          | None ->
            (* parent::nonStaticFunc() is really weird. It's calling a method
             * defined on the parent class, but $this is still the child class.
             * We can deal with this by hijacking the continuation that
             * calculates the SN.Typehints.this type *)
            let k_lhs ty =
              Reason.Rwitness fpos, Tgeneric (SN.Typehints.this, Some (Env.get_self env))
            in
            let env, method_, _ =
              obj_get_ ~is_method:true ~nullsafe:None env ty1 m
              begin fun (env, fty, _) ->
                let env, fty = Env.expand_type env fty in
                let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
                let fty = check_abstract_parent_meth (snd m) p fty in
                let env, method_ = call p env fty el uel in
                env, method_, None
              end
              k_lhs
            in
            env, method_
          | Some _ ->
            let env, fty = class_get ~is_method:true ~is_const:false env ty1 m CIparent in
            let env, fty = Env.expand_type env fty in
            let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
            let fty = check_abstract_parent_meth (snd m) p fty in
            call p env fty el uel
        )
      end
  | Class_const(e1, m) ->
      Typing_utils.process_static_find_ref e1 m;
      let env, ty1 = static_class_id p env e1 in
      let env, fty = class_get ~is_method:true ~is_const:false env ty1 m e1 in
      let env, fty = Env.expand_type env fty in
      let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
      call p env fty el uel
  | Obj_get(e1, (_, Id m), nullflavor) ->
      let is_method = call_type = Cnormal in
      let env, ty1 = expr env e1 in
      let nullsafe =
        (match nullflavor with
          | OG_nullthrows -> None
          | OG_nullsafe -> Some p
        ) in
      let fn = (fun (env, fty, _) ->
        let env, fty = Env.expand_type env fty in
        let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
        let env, method_ = call p env fty el uel in
        env, method_, None) in
      (if nullflavor == OG_nullsafe && not (type_could_be_null env ty1) then
        let env, (r, _) = Env.expand_type env ty1 in
        Errors.nullsafe_not_needed p
          (Reason.to_string
           "This is what makes me believe it cannot be null"
           r));
      obj_get ~is_method:is_method ~nullsafe:nullsafe env ty1 m fn
  | Fun_id x
  | Id x ->
      Typing_hooks.dispatch_id_hook x env;
      let env, fty = fun_type_of_id env x in
      let env, fty = Env.expand_type env fty in
      let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
      call p env fty el uel
  | _ ->
      let env, fty = expr env e in
      let env, fty = Env.expand_type env fty in
      let env, fty = Inst.instantiate_fun env fty el in (* ignore uel ; el for FormatString *)
      call p env fty el uel

and fun_type_of_id env x =
  Typing_hooks.dispatch_fun_id_hook x;
  Find_refs.process_find_refs None (snd x) (fst x);
  let env, fty =
    match Env.get_fun env (snd x) with
    | None -> unbound_name env x
    | Some fty ->
        let env, fty = Inst.instantiate_ft env fty in
        env, (Reason.Rwitness fty.ft_pos, Tfun fty)
  in
  env, fty

(*****************************************************************************)
(* Function type-checking expressions accessing an array (example: $x[...]).
 * The parameter is_lvalue is true when the expression is on the left hand
 * side of an assignment (example: $x[...] = 0).
 *)
(*****************************************************************************)
and array_get is_lvalue p env ty1 ety1 e2 ty2 =
  (* This is a little weird -- we enforce the right arity when you use certain
   * collections, even in partial mode (where normally completely omitting the
   * type parameter list is admitted). Basically the "omit type parameter"
   * hole was for compatibility with certain interfaces like ArrayAccess, not
   * for collections! But it's hard to go back on now, so since we've always
   * errored (with an inscruitable error message) when you try to actually use
   * a collection with omitted type parameters, we can continue to error and
   * give a more useful error message. *)
  let arity_error (_, name) =
    Errors.array_get_arity p name (Reason.to_pos (fst ty1))
  in
  match snd ety1 with
  | Tunresolved tyl ->
      let env, tyl = lfold begin fun env ty1 ->
        let env, ety1 = Env.expand_type env ty1 in
        array_get is_lvalue p env ty1 ety1 e2 ty2
      end env tyl
      in
      env, (fst ety1, Tunresolved tyl)
  | Tarray (Some ty, None) ->
      let ty1 = Reason.Ridx (fst e2), Tprim Tint in
      let env, _ = Type.unify p Reason.URarray_get env ty2 ty1 in
      env, ty
  | Tgeneric (_, Some (_, Tapply ((_, cn) as id, argl)))
  | Tapply ((_, cn) as id, argl)
    when cn = SN.Collections.cVector ->
      let ty = match argl with
        | [ty] -> ty
        | _ -> arity_error id; Reason.Rwitness p, Tany in
      let ty1 = Reason.Ridx_vector (fst e2), Tprim Tint in
      let env, _ = Type.unify p Reason.URvector_get env ty2 ty1 in
      env, ty
  | Tgeneric (_, Some (_, Tapply ((_, cn) as id, argl)))
  | Tapply ((_, cn) as id, argl)
      when cn = SN.Collections.cMap
      || cn = SN.Collections.cStableMap ->
      let (k, v) = match argl with
        | [k; v] -> (k, v)
        | _ ->
            arity_error id;
            let any = (Reason.Rwitness p, Tany) in
            any, any
      in
      let env, ty2 = TUtils.unresolved env ty2 in
      let env, _ = Type.unify p Reason.URmap_get env k ty2 in
      env, v
  | Tgeneric (_, Some (_, Tapply ((_, cn) as id, argl)))
  | Tapply ((_, cn) as id, argl)
      when not is_lvalue &&
        (cn = SN.Collections.cConstMap
        || cn = SN.Collections.cImmMap) ->
      let (k, v) = match argl with
        | [k; v] -> (k, v)
        | _ ->
            arity_error id;
            let any = (Reason.Rwitness p, Tany) in
            any, any
      in
      let env, _ = Type.unify p Reason.URmap_get env k ty2 in
      env, v
  | Tgeneric (_, Some (_, Tapply ((_, cn), _)))
  | Tapply ((_, cn), _)
      when is_lvalue &&
        (cn = SN.Collections.cConstMap || cn = SN.Collections.cImmMap) ->
    error_const_mutation env p ety1
  | Tgeneric (_, Some (_, Tapply ((_, cn) as id, argl)))
  | Tapply ((_, cn) as id, argl)
      when cn = SN.Collections.cIndexish ->
      let (k, v) = match argl with
        | [k; v] -> (k, v)
        | _ ->
            arity_error id;
            let any = (Reason.Rwitness p, Tany) in
            any, any
      in
      let env, _ = Type.unify p Reason.URcontainer_get env k ty2 in
      env, v
  | Tgeneric (_, Some (_, Tapply ((_, cn) as id, argl)))
  | Tapply ((_, cn) as id, argl)
      when not is_lvalue &&
        (cn = SN.Collections.cConstVector || cn = SN.Collections.cImmVector) ->
      let ty = match argl with
        | [ty] -> ty
        | _ -> arity_error id; Reason.Rwitness p, Tany in
      let ty1 = Reason.Ridx (fst e2), Tprim Tint in
      let ur = (match cn with
        | x when x = SN.Collections.cConstVector -> Reason.URconst_vector_get
        | x when x = SN.Collections.cImmVector  -> Reason.URimm_vector_get
        | _ -> failwith ("Unexpected collection name: " ^ cn)) in
      let env, _ = Type.unify p ur env ty2 ty1 in
      env, ty
  | Tgeneric (_, Some (_, Tapply ((_, cn), _)))
  | Tapply ((_, cn), _)
      when is_lvalue &&
        (cn = SN.Collections.cConstVector || cn = SN.Collections.cImmVector) ->
    error_const_mutation env p ety1
  | Tarray (Some k, Some v) ->
      let env, ty2 = TUtils.unresolved env ty2 in
      let env, _ = Type.unify p Reason.URarray_get env k ty2 in
      (* The values in the array are not consistent
       * we just give up. Use Maps!
       *)
      let env, ev = TUtils.fold_unresolved env v in
      let env, ev = Env.expand_type env ev in
      (match ev with
      | _, Tunresolved _ -> env, (Reason.Rwitness p, Tany)
      | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_, _) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tapply (_, _) | Tabstract (_, _, _)
        | Ttuple _ | Tanon _ | Tobject | Tshape _ | Taccess (_, _, _)) -> env, v
      )
  | Tany | Tarray (None, None) -> env, (Reason.Rnone, Tany)
  | Tprim Tstring ->
      let ty = Reason.Rwitness p, Tprim Tstring in
      let env, ty = Type.unify p Reason.URnone env ty1 ty in
      let int = Reason.Ridx (fst e2), Tprim Tint in
      let env, _ = Type.unify p Reason.URarray_get env ty2 int in
      env, ty
  | Ttuple tyl ->
      (match e2 with
      | p, Int n ->
          (try
            let idx = int_of_string (snd n) in
            let nth = List.nth tyl idx in
            env, nth
          with _ ->
            Errors.typing_error p (Reason.string_of_ureason Reason.URtuple_get);
            env, (Reason.Rwitness p, Tany)
          )
      | p, _ ->
          Errors.typing_error p (Reason.string_of_ureason Reason.URtuple_access);
          env, (Reason.Rwitness p, Tany)
      )
  | Tapply ((_, cn) as id, argl) when cn = SN.Collections.cPair ->
      let (ty1, ty2) = match argl with
        | [ty1; ty2] -> (ty1, ty2)
        | _ ->
            arity_error id;
            let any = (Reason.Rwitness p, Tany) in
            any, any
      in
      (match e2 with
      | p, Int n ->
          (try
            let idx = int_of_string (snd n) in
            let nth = List.nth [ty1; ty2] idx in
            env, nth
          with _ ->
            Errors.typing_error p (Reason.string_of_ureason Reason.URpair_get);
            env, (Reason.Rwitness p, Tany)
          )
      | p, _ ->
          Errors.typing_error p (Reason.string_of_ureason Reason.URpair_access);
          env, (Reason.Rwitness p, Tany)
      )
  | Tshape fdm ->
    let p, e2' = e2 in
    let field = shape_field_name p e2' in
    (match ShapeMap.get field fdm with
      | None -> Errors.undefined_field p (TUtils.get_shape_field_name field);
              env, (Reason.Rwitness p, Tany)
          | Some ty -> env, ty
          )
  | Toption _ ->
      Errors.null_container p
        (Reason.to_string
          "This is what makes me believe it can be null"
          (fst ety1)
        );
      env, (Reason.Rwitness p, Tany)
  | Tobject ->
      if Env.is_strict env
      then error_array env p ety1
      else env, (Reason.Rnone, Tany)
  | Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, ty1 = Typing_tdef.expand_typedef env (fst ety1) x argl in
      let env, ety1 = Env.expand_type env ty1 in
      array_get is_lvalue p env ty1 ety1 e2 ty2
  | Taccess (_, _, _) ->
      let env, ty1 = TAccess.expand env ety1 in
      let env, ety1 = Env.expand_type env ty1 in
      array_get is_lvalue p env ty1 ety1 e2 ty2
  | Tabstract (_, _, Some ty) ->
      let env, ety = Env.expand_type env ty in
      Errors.try_
        (fun () -> array_get is_lvalue p env ty ety e2 ty2)
        (fun _ -> error_array env p ety1)
  | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Tprim _ | Tvar _ | Tfun _
  | Tabstract (_, _, _) | Tapply (_, _) | Tanon (_, _) ->
      error_array env p ety1

and array_append is_lvalue p env ty1 =
  let env, ty1 = TUtils.fold_unresolved env ty1 in
  let env, ety1 = Env.expand_type env ty1 in
  match snd ety1 with
  | Tany | Tarray (None, None) -> env, (Reason.Rnone, Tany)
  | Tgeneric (_, Some (_, Tapply ((_, n), [ty])))
  | Tapply ((_, n), [ty])
      when n = SN.Collections.cVector || n = SN.Collections.cSet ->
      env, ty
  | Tapply ((_, n), [tkey; tvalue]) when n = SN.Collections.cMap ->
      (* You can append a pair to a map *)
      env, (Reason.Rmap_append p, Tapply ((p, SN.Collections.cPair), [tkey; tvalue]))
  | Tarray (Some ty, None) ->
      env, ty
  | Tobject ->
      if Env.is_strict env
      then error_array_append env p ety1
      else env, (Reason.Rnone, Tany)
  | Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, ty1 = Typing_tdef.expand_typedef env (fst ety1) x argl in
      array_append is_lvalue p env ty1
  | Taccess (_, _, _) ->
      let env, ty1 = TAccess.expand env ety1 in
      array_append is_lvalue p env ty1
  | Tabstract (_, _, Some ty) ->
      Errors.try_
        (fun () -> array_append is_lvalue p env ty)
        (fun _ -> error_array_append env p ety1)
  | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
  | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _
  | Tanon (_, _) | Tunresolved _ | Tshape _ ->
      error_array_append env p ety1

and error_array env p (r, ty) =
  Errors.array_access p (Reason.to_pos r) (Typing_print.error ty);
  env, (Reason.Rwitness p, Tany)

and error_array_append env p (r, ty) =
  Errors.array_append p (Reason.to_pos r) (Typing_print.error ty);
  env, (Reason.Rwitness p, Tany)

and error_const_mutation env p (r, ty) =
  Errors.const_mutation p (Reason.to_pos r) (Typing_print.error ty);
  env, (Reason.Rwitness p, Tany)

(**
 * Checks if a class (given by cty) contains a given static method.
 *
 * We could refactor this + class_get
 *)
and class_contains_smethod env cty (p, mid) =
  match cty with
  | _, Tgeneric (_, Some (_, Tapply ((_, c), paraml)))
  | _, Tapply ((_, c), paraml) ->
      let class_ = Env.get_class env c in
      (match class_ with
      | None -> None
      | Some class_ ->
          Env.get_static_member true env class_ mid
      )
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
    | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _))-> None

and class_get ~is_method ~is_const env cty (p, mid) cid =
  let env, ty = class_get_ ~is_method ~is_const env cty (p, mid) cid in
  (* Replace the SN.Typehints.this type in the resulting type *)
  Inst.instantiate_this env ty cty

and class_get_ ~is_method ~is_const env cty (p, mid) cid =
  match cty with
  | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, cty = Typing_tdef.expand_typedef env r x argl in
      class_get_ ~is_method ~is_const env cty (p, mid) cid
  | _, Taccess (_, _, _) ->
      let env, cty = TAccess.expand env cty in
      class_get_ ~is_method ~is_const env cty (p, mid) cid
  | _, Tgeneric (_, Some (_, Tapply ((_, c), paraml)))
  | _, Tapply ((_, c), paraml) ->
      let class_ = Env.get_class env c in
      (match class_ with
      | None -> env, (Reason.Rnone, Tany)
      | Some class_ ->
          let smethod =
            if is_const
            then Env.get_const env class_ mid
            else Env.get_static_member is_method env class_ mid in
          if !Typing_defs.accumulate_method_calls then
            Typing_defs.accumulate_method_calls_result :=
                (p, (class_.tc_name^"::"^mid)) ::
                    !Typing_defs.accumulate_method_calls_result;
          Find_refs.process_find_refs (Some class_.tc_name) mid p;
          Typing_hooks.dispatch_smethod_hook class_ (p, mid) env (Some cid);
          (match smethod with
          | None ->
            (match Env.get_static_member is_method env class_ SN.Members.__callStatic with
              | None ->
                smember_not_found p ~is_const ~is_method env class_ mid;
                env, (Reason.Rnone, Tany)
              | Some {ce_visibility = vis; ce_type = (r, Tfun ft); _} ->
                check_visibility p env (Reason.to_pos r, vis) (Some cid);
                (* xxx: is there a need to subst in SN.Typehints.this *)
                let subst = Inst.make_subst class_.tc_tparams paraml in
                let env, ft_ret = Inst.instantiate subst env ft.ft_ret in
                let ft = { ft with
                  ft_arity = Fellipsis 0;
                  ft_tparams = []; ft_params = [];
                  ft_ret = ft_ret;
                } in
                env, (r, Tfun ft)
              | _ -> assert false)
          | Some { ce_visibility = vis; ce_type = method_; _ } ->
              check_visibility p env (Reason.to_pos (fst method_), vis) (Some cid);
              let subst = Inst.make_subst class_.tc_tparams paraml in
              let env, method_ = Inst.instantiate subst env method_ in
              env, method_)
      )
  | _, Tany ->
      (match Env.get_mode env with
      | Ast.Mstrict -> Errors.expected_class p
      | Ast.Mdecl | Ast.Mpartial -> ()
      );
      env, (Reason.Rnone, Tany)
  | _, (Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _ | Tvar _
    | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _) | Tunresolved _
    | Tobject | Tshape _) ->
      Errors.expected_class p;
      env, (Reason.Rnone, Tany)

and smember_not_found pos ~is_const ~is_method env class_ member_name =
  let kind =
    if is_const then `class_constant
    else if is_method then `static_method
    else `class_variable in
  let error hint =
    let cid = (class_.tc_pos, class_.tc_name) in
    Errors.smember_not_found kind pos cid member_name hint
  in
  match Env.suggest_static_member is_method class_ member_name with
  | None ->
      (match Env.suggest_member is_method class_ member_name with
      | None when not class_.tc_members_fully_known ->
          (* no error in this case ... the member might be present
           * in one of the parents of class_ that the typing cannot see *)
          ()
      | None ->
          error `no_hint
      | Some (pos2, v) ->
          error (`closest (pos2, v))
      );
  | Some (pos2, v) ->
      error (`did_you_mean (pos2, v))

and member_not_found pos ~is_method env class_ member_name class_name =
  let kind = if is_method then `method_ else `member in
  let error hint =
    Errors.member_not_found kind pos class_name member_name hint
  in
  match Env.suggest_member is_method class_ member_name with
    | None ->
      (match Env.suggest_static_member is_method class_ member_name with
        | None when not class_.tc_members_fully_known ->
          (* no error in this case ... the member might be present
           * in one of the parents of class_ that the typing cannot see *)
          ()
        | None ->
          error `no_hint
        | Some (def_pos, v) ->
          error (`closest (def_pos, v))
      )
    | Some (def_pos, v) ->
        error (`did_you_mean (def_pos, v))

and obj_get ~is_method:is_method ~nullsafe:nullsafe env ty1 id k =
  let env, method_, _ =
    obj_get_with_visibility ~is_method:is_method ~nullsafe:nullsafe env ty1
                            id k in
  env, method_

and obj_get_with_visibility ~is_method:is_method ~nullsafe:nullsafe env ty1
                            (p, s as id) k =
  obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty1 id k (fun ty -> ty)

and obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty1 (p, s as id)
             k k_lhs =
  let env, ety1 = Env.expand_type env ty1 in
  match ety1 with
  | _, Tunresolved tyl ->
      let (env, vis), tyl =
        lfold
          (fun (env, vis) ty ->
            let env, ty, vis' =
              obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty id
                       k k_lhs in
            (* There is one special case where we need to expose the
             * visibility outside of obj_get (checkout inst_meth special
             * function).
             * We keep a witness of the "most restrictive" visibility
             * we encountered (position + visibility), to be able to
             * special case inst_meth.
             *)
            let vis = TUtils.min_vis_opt vis vis' in
            (env, vis), ty)
          (env, None)
          tyl in
      let env, method_ = TUtils.in_var env (fst ety1, Tunresolved (tyl)) in
      env, method_, vis
  | p, Tgeneric (x, Some ty) ->
      let k_lhs' ty = k_lhs (p, Tgeneric (x, Some ty)) in
      obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty id k k_lhs'
  | p, Tapply ((_, x) as c, argl) when Typing_env.is_typedef x ->
      let env, ty1 = Typing_tdef.expand_typedef env (fst ety1) x argl in
      let k_lhs' ty = k_lhs (p, Tapply (c, argl)) in
      obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty1 id k k_lhs'
  | _, Taccess (_, _, _) ->
      let env, ty1 = TAccess.expand env ety1 in
      obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty1 id k k_lhs
  | _, Toption ty -> begin match nullsafe with
    | Some p1 ->
        let k' (env, fty, x) = begin
          let env, method_, x = k (env, fty, x) in
          let env, method_ = non_null env method_ in
          env, (Reason.Rwitness p1, Toption method_), x
        end in
        obj_get_ ~is_method:is_method ~nullsafe:nullsafe env ty id k' k_lhs
    | None ->
        Errors.null_member s p
          (Reason.to_string
             "This is what makes me believe it can be null"
             (fst ety1)
          );
        k (env, (fst ety1, Tany), None)
    end
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Tprim _ | Tvar _
    | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _)
    | Tobject | Tshape _) -> k begin match snd ety1 with
    | Tgeneric (_, Some (_, Tapply (x, paraml)))
    | Tapply (x, paraml) ->
        let class_ = Env.get_class env (snd x) in
        (match class_ with
          | None ->
            env, (Reason.Rnone, Tany), None
          | Some class_ when not is_method
              && class_.tc_name = SN.Classes.cStdClass ->
            env, (Reason.Rnone, Tany), None
          | Some class_ ->
            let paraml =
              if List.length paraml = 0
              then List.map (fun _ -> Reason.Rwitness p, Tany) class_.tc_tparams
              else paraml
            in
            let method_ = Env.get_member is_method env class_ s in
            if !Typing_defs.accumulate_method_calls then
              Typing_defs.accumulate_method_calls_result :=
                (p, (class_.tc_name^"::"^s)) ::
                !Typing_defs.accumulate_method_calls_result;
            Find_refs.process_find_refs (Some class_.tc_name) s p;
            Typing_hooks.dispatch_cmethod_hook class_ (p, s) env None;
            (match method_ with
              | None ->
                (match Env.get_member is_method env class_ SN.Members.__call with
                  | None ->
                    member_not_found p ~is_method env class_ s x;
                    env, (Reason.Rnone, Tany), None
                  | Some {ce_visibility = vis; ce_type = (r, Tfun ft); _}  ->
                    let meth_pos = Reason.to_pos r in
                    check_visibility p env (meth_pos, vis) None;
                    let new_name = "alpha_varied_this" in

                    let env, paraml = alpha_this ~new_name env paraml in
                    (* the return type of __call can depend on the
                     * class params or be this *)
                    let this_ty = k_lhs ety1 in
                    let subst = Inst.make_subst_with_this
                      ~this:this_ty class_.tc_tparams paraml in
                    let env, ft_ret = Inst.instantiate subst env ft.ft_ret in

                    (* we change the params of the underlying
                     * declaration to act as a variadic function
                     * ... this transform cannot be done when
                     * processing the declaratation of call because
                     * direct calls to $inst->__call are also
                     * valid.  *)
                    let ft = {ft with
                      ft_arity = Fellipsis 0;
                      ft_tparams = []; ft_params = [];
                      ft_ret = ft_ret;
                    } in
                    let env, method_ = Typing_generic.rename env new_name
                      SN.Typehints.this (r, Tfun ft) in
                    env, method_, Some (meth_pos, vis)
                  | _ -> assert false
                )
              | Some {ce_visibility = vis; ce_type = method_; _} ->
                let meth_pos = Reason.to_pos (fst method_) in
                check_visibility p env (meth_pos, vis) None;
                let new_name = "alpha_varied_this" in
                (* Since a paraml member might include a SN.Typehints.this type,
                 * let's alpha vary all the SN.Typehints.this types in the
                 * params*)
                let env, paraml = alpha_this ~new_name env paraml in

                let subst = Inst.make_subst class_.tc_tparams paraml in
                let env, method_ = Inst.instantiate subst env method_ in

                (* We must substitute out the this types separately
                 * from when the method is instantiated with its type
                 * variables. Consider Vector<this>::add(). It is
                 * declared with the return type this and argument
                 * T. We don't want to substitute SN.Typehints.this for T and
                 * then replace that SN.Typehints.this with Vector<this>. *)
                let this_ty = k_lhs ety1 in
                let env, method_ = Inst.instantiate_this env method_ this_ty in

                (* Now this has been substituted, we can de-alpha-vary *)
                let env, method_ =
                  Typing_generic.rename env new_name SN.Typehints.this method_ in
                env, method_, Some (meth_pos, vis)
            )
        )
    | Tobject
    | Tany -> env, (fst ety1, Tany), None
    | (Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Toption _
      | Tvar _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
      | Tfun _ | Tunresolved _ | Tshape _ | Taccess (_, _, _)) as ty ->
      Errors.non_object_member
          s p (Typing_print.error ty) (Reason.to_pos (fst ety1));
      env, (fst ety1, Tany), None
  end

and type_could_be_null env ty1 =
  let env, ety1 = Env.expand_type env ty1 in
  match (snd ety1) with
  | Tgeneric (x, Some ty) -> type_could_be_null env ty
  | Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, ty = Typing_tdef.expand_typedef env (fst ety1) x argl in
      type_could_be_null env ty
  | Taccess _ ->
      let env, ty = TAccess.expand env ety1 in
      type_could_be_null env ty
  | Toption _ | Tgeneric _ | Tunresolved _ | Tmixed | Tany -> true
  | Tarray (_, _) | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _)
  | Tapply (_, _) | Ttuple _ | Tanon (_, _) | Tobject
  | Tshape _ -> false

and alpha_this ~new_name env paraml =
  (* Since a param might include a SN.Typehints.this type, let's alpha vary
   * all the SN.Typehints.this types in the params*)
  List.fold_right
    (fun param (env, paraml) ->
      let env, param = Typing_generic.rename env SN.Typehints.this new_name param in
      env, param::paraml)
    paraml
    (env, [])

and class_id p env cid =
  let env, obj = static_class_id p env cid in
  match obj with
    | _, Tgeneric (this, Some (_, Tapply ((_, cid as c), _)))
      when this = SN.Typehints.this ->
      let class_ = Env.get_class env cid in
      (match class_ with
        | None -> env, None
        | Some class_ ->
          env, Some (c, class_)
      )
    | _, Tapply ((_, cid as c), _) ->
      let class_ = Env.get_class env cid in
      (match class_ with
        | None -> env, None
        | Some class_ ->
          env, Some (c, class_)
      )
    | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
      | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
      | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) -> env, None

(* To be a valid trait declaration, all of its 'require extends' must
 * match; since there's no multiple inheritance, it follows that all of
 * the 'require extends' must belong to the same inheritance hierarchy
 * and one of them should be the child of all the others *)
and trait_most_concrete_req_class trait env =
  SMap.fold (fun name ty acc ->
    let keep = match acc with
      | Some (c, _ty) -> SMap.mem name c.tc_ancestors
      | None -> false
    in
    if keep then acc
    else
      let class_ = Env.get_class env name in
      (match class_ with
        | None
        | Some { tc_kind = Ast.Cinterface; _ } -> acc
        | Some { tc_kind = Ast.Ctrait; _ } ->
          (* this is an error case for which the nastCheck spits out
           * an error, but does *not* currently remove the offending
           * 'require extends' or 'require implements' *)
          acc
        | Some c -> Some (c, ty)
      )
  ) trait.tc_req_ancestors None

and static_class_id p env = function
  | CIparent ->
    (match Env.get_self env with
      | _, Tapply ((self_pos, self), _) ->
        (match Env.get_class env self with
          | Some (
            {tc_kind = Ast.Ctrait; tc_req_ancestors ; tc_name; _}
              as trait) ->
            (match trait_most_concrete_req_class trait env with
              | None ->
                Errors.parent_in_trait p;
                env, (Reason.Rwitness p, Tany)
              | Some (tc_parent, parent_ty) ->
                (* inside a trait, parent is SN.Typehints.this, but with the
                 * type of the most concrete class that the trait has
                 * "require extend"-ed *)
                let r = Reason.Rwitness p in
                env, (r, Tgeneric (SN.Typehints.this, Some parent_ty))
            )
          | _ ->
            let parent = Env.get_parent env in
            let parent_defined = snd parent <> Tany in
            if not parent_defined
            then Errors.parent_undefined p;
            let r = Reason.Rwitness p in
            (* parent is still technically the same object. *)
            env, (r, Tgeneric (SN.Typehints.this, Some (r, snd parent)))
          )
      | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
        | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
        | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) ->
        let parent = Env.get_parent env in
        let parent_defined = snd parent <> Tany in
        if not parent_defined
        then Errors.parent_undefined p;
        let r = Reason.Rwitness p in
        (* parent is still technically the same object. *)
        env, (r, Tgeneric (SN.Typehints.this, Some (r, snd parent)))
    )
  | CIstatic ->
    env, (Reason.Rwitness p,
      Tgeneric (SN.Typehints.this, Some (Env.get_self env)))
  | CIself -> env, (Reason.Rwitness p, snd (Env.get_self env))
  | CI c ->
    let class_ = Env.get_class env (snd c) in
    (match class_ with
      | None -> env, (Reason.Rnone, Tany) (* Tobject *)
      | Some class_ ->
        let env, params = lfold begin fun env x ->
          TUtils.in_var env (Reason.Rnone, Tunresolved [])
        end env class_.tc_tparams in
        env, (Reason.Rwitness p, Tapply (c, params))
    )
  | CIvar e ->
      let env, ty = expr env e in
      let _, ty = Env.expand_type env ty in
      let ty =
        match ty with
        | _, Tgeneric (_, Some (_, Tapply _))
        | _, Tapply _ -> ty
        | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
          | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _
          | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
          | Taccess (_, _, _)) ->
            if Env.get_mode env = Ast.Mstrict
            then Errors.dynamic_class p;
            Reason.Rnone, Tany
      in env, ty

and call_construct p env class_ params el uel =
  let cstr = Env.get_construct env class_ in
  let mode = Env.get_mode env in
  Typing_hooks.dispatch_constructor_hook class_ env p;
  Find_refs.process_find_refs (Some class_.tc_name) SN.Members.__construct p;
  match (fst cstr) with
    | None ->
      if el <> [] &&
        (mode = Ast.Mstrict || mode = Ast.Mpartial) &&
        class_.tc_members_fully_known
      then Errors.constructor_no_args p;
      fst (lfold expr env el)
    | Some { ce_visibility = vis; ce_type = m; _ } ->
      check_visibility p env (Reason.to_pos (fst m), vis) None;
      let subst = Inst.make_subst class_.tc_tparams params in
      let env, m = Inst.instantiate subst env m in
      fst (call p env m el uel)

and check_visibility p env (p_vis, vis) cid =
  match is_visible env vis cid with
  | None -> ()
  | Some (msg1, msg2) -> Errors.visibility p msg1 p_vis msg2

and is_visible env vis cid =
  let self_id = Env.get_self_id env in
  match vis with
  | Vpublic -> None
  | Vprivate _ when self_id = "" ->
    Some ("You cannot access this member", "This member is private")
  | Vprivate x ->
    (match cid with
      | Some CIstatic ->
          let my_class = Env.get_class env self_id in
          begin match my_class with
            | Some {tc_final = true; _} -> None
            | _ -> Some (
              ("Private members cannot be accessed with static:: since"
               ^" a child class may also have an identically"
               ^" named private member"),
              "This member is private")
          end
      | Some CIparent ->
          Some (
            "You cannot access a private member with parent::",
            "This member is private")
      | Some CIself -> None
      | Some (CI (_, called_ci)) when x <> self_id ->
          (match Env.get_class env called_ci with
          | Some {tc_kind = Ast.Ctrait; _} ->
              Some ("You cannot access private members"
              ^" using the trait's name (did you mean to use self::?)",
              "This member is private")
          | _ ->
            Some ("You cannot access this member", "This member is private"))
      | Some (CIvar e) ->
          let env, ty = expr env e in
          let _, ty = Env.expand_type env ty in
          begin match ty with
            | _, Tgeneric (_, Some (_, Tapply ((_, c), _)))
            | _, Tapply ((_, c), _) ->
                (match Env.get_class env c with
                | Some {tc_final = true; _} -> None
                | _ -> Some (
                  ("Private members cannot be accessed dynamically. "
                     ^"Did you mean to use 'self::'?"),
                    "This member is private"))
            | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
              | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _
              | Tanon (_, _) | Tunresolved _ | Tobject
              | Tshape _ | Taccess (_, _, _)) -> assert false
          end
      | None when x <> self_id ->
        Some ("You cannot access this member", "This member is private")
      | Some (CI _)
      | None -> None)
  | Vprotected x when x = self_id -> None
  | Vprotected _ when self_id = "" ->
    Some ("You cannot access this member", "This member is protected")
  | Vprotected x ->
    let my_class = Env.get_class env self_id in
    let their_class = Env.get_class env x in
    match cid, their_class with
      | Some CI _, Some {tc_kind = Ast.Ctrait; _} ->
        Some ("You cannot access protected members"
        ^" using the trait's name (did you mean to use static:: or self::?)",
        "This member is protected")
      | _ -> (
        match my_class, their_class with
          | Some my_class, Some their_class ->
              (* Children can call parent's protected methods and
               * parents can call children's protected methods (like a
               * constructor) *)
              if SSet.mem x my_class.tc_extends
                || SSet.mem self_id their_class.tc_extends
                || SSet.mem x my_class.tc_req_ancestors_extends
                || not my_class.tc_members_fully_known
              then None
              else Some (
                "Cannot access this protected member, you don't extend "^
                  (Utils.strip_ns x),
                "This member is protected"
              )
            | _, _ -> None
        )

and check_arity ?(check_min=true) env pos pos_def (arity:int)
    (exp_arity:fun_arity) =
  let exp_min = (Typing_defs.arity_min exp_arity) in
  if check_min && arity < exp_min then
    Errors.typing_too_few_args pos pos_def;
  match exp_arity with
    | Fstandard (_, exp_max) ->
      if (arity > exp_max) && (Env.get_mode env <> Ast.Mdecl)
      then Errors.typing_too_many_args pos pos_def;
    | Fvariadic _ | Fellipsis _ -> ()

(* The variadic capture argument is an array listing the passed
 * variable arguments for the purposes of the function body; callsites
 * should not unify with it *)
and variadic_param env ft =
  match ft.ft_arity with
    | Fvariadic (_, p_ty) -> env, Some p_ty
    | Fellipsis _ | Fstandard _ -> env, None

and call pos env fty el uel =
  let env, ty = call_ pos env fty el uel in
  (* We need to solve the constraints after every single function call.
   * The type-checker is control-flow sensitive, the same value could
   * have different type depending on the branch that we are in.
   * When this is the case, a call could violate one of the constraints
   * in a branch. *)
  let env = fold_fun_list env env.Env.todo in
  env, ty

and unpack_expr env e =
  let pos = fst e in
  let unpack_r = Reason.Runpack_param pos in
  let env, ty_elt = expr env e in
  let container_ty = (unpack_r, Tapply ((pos, SN.Collections.cContainer),
                                        [unpack_r, Tany])) in
  let env = Type.sub_type pos Reason.URparam env container_ty ty_elt in
  env, ty_elt

and call_ pos env fty el uel =
  let env, efty = Env.expand_type env fty in
  (match efty with
  | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, fty = Typing_tdef.expand_typedef env r x argl in
      call_ pos env fty el uel
  | _, (Tany | Tunresolved []) ->
    let el = el @ uel in
    let env, _ = lmap expr env el in
    Typing_hooks.dispatch_fun_call_hooks [] (List.map fst (el @ uel)) env;
    env, (Reason.Rnone, Tany)
  | r, Tunresolved tyl ->
    let env, retl = lmap (fun env ty -> call pos env ty el uel) env tyl in
    TUtils.in_var env (r, Tunresolved retl)
  | r2, Tfun ft ->
    let pos_def = Reason.to_pos r2 in
    let () = check_arity ~check_min:(uel = [])
      env pos pos_def (List.length el + List.length uel) ft.ft_arity in
    let env, var_param = variadic_param env ft in
    let env, tyl = lmap expr env el in
    let pos_tyl = List.combine (List.map fst el) tyl in
    let todos = ref [] in
    let env = wfold_left_default (call_param todos) (env, var_param)
      ft.ft_params pos_tyl in
    let env, _ = lmap unpack_expr env uel in
    let env = fold_fun_list env !todos in
    Typing_hooks.dispatch_fun_call_hooks ft.ft_params (List.map fst (el @ uel)) env;
    env, ft.ft_ret
  | r2, Tanon (arity, id) when uel = [] ->
    let env, tyl = lmap expr env el in
    let anon = Env.get_anonymous env id in
    let fpos = Reason.to_pos r2 in
    (match anon with
      | None ->
        Errors.anonymous_recursive_call pos;
        env, (Reason.Rnone, Tany)
      | Some anon ->
        let () = check_arity env pos fpos (List.length tyl) arity in
        let tyl = List.map (fun x -> None, x) tyl in
        anon env tyl)
  | _, Tarray _ when not (Env.is_strict env) ->
    (* Relaxing call_user_func to work with an array in partial mode *)
    env, (Reason.Rnone, Tany)
  | _, ty ->
    bad_call pos ty;
    env, (Reason.Rnone, Tany)
  )

and call_param todos env (name, x) (pos, arg) =
  (match name with
  | None -> ()
  | Some name -> Typing_suggest.save_param name env x arg
  );
  (* We solve for Tanon types after all the other params because we want to
   * typecheck the lambda bodies with as much type information as possible. For
   * example, in array_map(fn, x), we might be able to use the type of x to
   * infer the type of fn, but if we call sub_type on fn first, we end up
   * typechecking its body without the benefit of knowing its full type. If
   * fn is typehinted but not x, we could use fn to infer the type of x, but
   * in practice the reverse situation is more likely. This rearrangement is
   * particularly useful since higher-order functions usually put fn before x.
   *)
  match arg with
  | _, Tanon _ ->
      todos := (fun env ->
                Type.sub_type pos Reason.URparam env x arg) :: !todos;
      env
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _
    | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) ->
      Type.sub_type pos Reason.URparam env x arg

and bad_call p ty =
  Errors.bad_call p (Typing_print.error ty)

and unop p env uop ty =
  match uop with
  | Ast.Unot ->
      (* !$x (logical not) works with any type, so we just return Tbool *)
      Typing_async.enforce_not_awaitable env p ty;
      env, (Reason.Rlogic_ret p, Tprim Tbool)
  | Ast.Utild ->
      (* ~$x (bitwise not) only works with int *)
      Type.unify p Reason.URnone env (Reason.Rarith p, Tprim Tint) ty
  | Ast.Uincr
  | Ast.Upincr
  | Ast.Updecr
  | Ast.Udecr
  | Ast.Uplus
  | Ast.Uminus ->
      (* math operators work with int or floats, so we call sub_type *)
      let env = Type.sub_type p Reason.URnone env (Reason.Rarith p, Tprim Tnum) ty in
      env, ty

and binop in_cond p env bop p1 ty1 p2 ty2 =
  let expand_num_type env p ty =
    let env, ty = TUtils.fold_unresolved env ty in
    let env = Type.sub_type p Reason.URnone env (Reason.Rarith p, Tprim Tnum) ty in
    let env, ety = Env.expand_type env ty in
    (env, ety) in
  match bop with
  | Ast.Plus ->
      let env, ty1 = TUtils.fold_unresolved env ty1 in
      let env, ty2 = TUtils.fold_unresolved env ty2 in
      let env, ety1 = Env.expand_type env ty1 in
      let env, ety2 = Env.expand_type env ty2 in
      (match ety1, ety2 with
      | (_, Tarray _), (_, Tarray _)
      | (_, Tany), (_, Tarray _)
      | (_, Tarray _), (_, Tany) ->
          let env, ty = Type.unify p Reason.URnone env ty1 ty2 in
          env, ty
      | (_, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _,_))
        ), _ -> binop in_cond p env Ast.Minus p1 ty1 p2 ty2
      )
  | Ast.Minus | Ast.Star ->
      let env, ty1 = TUtils.fold_unresolved env ty1 in
      let env, ty2 = TUtils.fold_unresolved env ty2 in
      let env = Type.sub_type p1 Reason.URnone env
        (Reason.Rarith p1, Tprim Tnum) ty1 in
      let env = Type.sub_type p2 Reason.URnone env
        (Reason.Rarith p2, Tprim Tnum) ty2 in
      let env, ety1 = Env.expand_type env ty1 in
      let env, ety2 = Env.expand_type env ty2 in
      (match ety1, ety2 with
      | (r, Tprim Tfloat), _ | _, (r, Tprim Tfloat) ->
          (* if either side is a float then float: 1.0 - 1 -> float *)
          env, (r, Tprim Tfloat)
      | (r, Tprim Tnum), _ | _, (r, Tprim Tnum) ->
          (* if either side is a num, then num: (3 / x) - 1 -> num *)
          env, (r, Tprim Tnum)
      | (_, Tprim Tint), (_, Tprim Tint) ->
          (* Both sides are integers, then integer: 1 - 1 -> int *)
          env, (Reason.Rarith_ret p, Tprim Tint)
      | (_, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _, _))
        ), _->
          (* Either side is unknown, unknown *)
          (* TODO um, what? This seems very wrong, particularly where "newtype
           * as" is concerned.
           * This also causes issues with primitive constraints on generics.
           * See test/typecheck/generic_primitive_invariant.php as an example *)
          env, ety1)
  | Ast.Slash ->
      let env, ety1 = expand_num_type env p1 ty1 in
      let env, ety2 = expand_num_type env p2 ty2 in
      (match ety1, ety2 with
      | (r, Tprim Tfloat), _ | _, (r, Tprim Tfloat) -> env, (r, Tprim Tfloat)
      | (_, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _, _))
        ), _ -> env, (Reason.Rret_div p, Tprim Tnum)
      )
  | Ast.Starstar ->
      let env, ety1 = expand_num_type env p1 ty1 in
      let env, ety2 = expand_num_type env p2 ty2 in
      (match ety1, ety2 with
      | (r, Tprim Tfloat), _ | _, (r, Tprim Tfloat) -> env, (r, Tprim Tfloat)
      | (_, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _, _))
        ), _ -> env, (Reason.Rarith_ret p, Tprim Tnum)
      )
  | Ast.Percent ->
      let env, ty1 = Type.unify p Reason.URnone env ty1 (Reason.Rarith p1, Tprim Tint) in
      let env, ty2 = Type.unify p Reason.URnone env ty2 (Reason.Rarith p1, Tprim Tint) in
      env, (Reason.Rarith_ret p, Tprim Tint)
  | Ast.Xor ->
      let env, ty1 = TUtils.fold_unresolved env ty1 in
      let env, ty2 = TUtils.fold_unresolved env ty2 in
      let env, ety1 = Env.expand_type env ty1 in
      let env, ety2 = Env.expand_type env ty2 in
      (match ety1, ety2 with
      | (_, Tprim Tbool), _ | _, (_, Tprim Tbool) ->
          let env, ty1 = Type.unify p Reason.URnone env ty1 (Reason.Rlogic_ret p1, Tprim Tbool) in
          let env, ty2 = Type.unify p Reason.URnone env ty2 (Reason.Rlogic_ret p1, Tprim Tbool) in
          env, (Reason.Rlogic_ret p, Tprim Tbool)
      | (_, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _, _))
        ), _ ->
          let env, ty1 = Type.unify p Reason.URnone env ty1 (Reason.Rarith p1, Tprim Tint) in
          let env, ty2 = Type.unify p Reason.URnone env ty2 (Reason.Rarith p1, Tprim Tint) in
          env, (Reason.Rarith_ret p, Tprim Tint)
      )
  | Ast.Eqeq  | Ast.Diff  ->
      env, (Reason.Rcomp p, Tprim Tbool)
  | Ast.EQeqeq | Ast.Diff2 ->
      if not in_cond
      then TypingEqualityCheck.assert_nontrivial p bop env ty1 ty2;
      env, (Reason.Rcomp p, Tprim Tbool)
  | Ast.Lt | Ast.Lte  | Ast.Gt  | Ast.Gte  ->
      let ty_num = (Reason.Rcomp p, Tprim Nast.Tnum) in
      let ty_string = (Reason.Rcomp p, Tprim Nast.Tstring) in
      let both_sub ty =
        SubType.is_sub_type env ty ty1 && SubType.is_sub_type env ty ty2 in
      if both_sub ty_num || both_sub ty_string
      then env, (Reason.Rcomp p, Tprim Tbool)
      else
        (* TODO this is questionable; PHP's semantics for conversions with "<"
         * are pretty crazy and we may want to just disallow this? *)
        let env, ty = Type.unify p Reason.URnone env ty1 ty2 in
        env, (Reason.Rcomp p, Tprim Tbool)
  | Ast.Dot ->
      let env = SubType.sub_string p1 env ty1 in
      let env = SubType.sub_string p2 env ty2 in
      env, (Reason.Rconcat_ret p, Tprim Tstring)
  | Ast.AMpamp
  | Ast.BArbar ->
      env, (Reason.Rlogic_ret p, Tprim Tbool)
  | Ast.Amp  | Ast.Bar  | Ast.Ltlt  | Ast.Gtgt ->
      let env = Type.sub_type p Reason.URnone env (Reason.Rbitwise p1, Tprim Tint) ty1 in
      let env = Type.sub_type p Reason.URnone env (Reason.Rbitwise p2, Tprim Tint) ty2 in
      env, (Reason.Rbitwise_ret p, Tprim Tint)
  | Ast.Eq _ ->
      assert false

and non_null ?expanded:(expanded=ISet.empty) env ty =
  let env, expanded, ty = Env.expand_type_recorded env expanded ty in
  match ty with
  | _, Toption ty ->
      let env, expanded, ty = Env.expand_type_recorded env expanded ty in
      (* When "??T" appears in the typing environment due to implicit
       * typing, the recursion here ensures that it's treated as
       * isomorphic to "?T"; that is, all nulls are created equal.
       *)
      non_null ~expanded env ty
  | r, Tunresolved tyl ->
      let env, tyl = lfold (non_null ~expanded) env tyl in
      (* We need to flatten the unresolved types, otherwise we could
       * end up with "Tunresolved[Tunresolved _]" which is not supposed
       * to happen.
       *)
      let tyl = List.fold_right begin fun ty tyl ->
        match ty with
        | _, Tunresolved l -> l @ tyl
        | x -> x :: tyl
      end tyl [] in
      env, (r, Tunresolved tyl)
  | r, Tapply ((_, x), argl) when Typing_env.is_typedef x ->
      let env, ty = Typing_tdef.expand_typedef env r x argl in
      non_null ~expanded env ty
  | _, Taccess (_, _, _) ->
      let env, ty = TAccess.expand env ty in
      non_null ~expanded env ty
  | r, Tgeneric (x, Some ty) ->
      let env, ty = non_null ~expanded env ty in
      env, (r, Tgeneric (x, Some ty))
  | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _) | Tvar _
    | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _) | Tfun _
    | Tobject | Tshape _) ->
      env, ty

and condition_var_non_null env = function
  | _, Lvar (p, x) ->
      let env, x_ty = Env.get_local env x in
      let env, x_ty = Env.expand_type env x_ty in
      let env, x_ty = non_null env x_ty in
      Env.set_local env x x_ty
  | p, Class_get (cname, (_, member_name)) as e ->
      let env, ty = expr env e in
      let env, local = Env.FakeMembers.make_static p env cname member_name in
      let env = Env.set_local env local ty in
      let local = p, Lvar (p, local) in
      condition_var_non_null env local
  | p, Obj_get ((_, This | _, Lvar _ as obj),
                (_, Id (_, member_name)),
                _) as e ->
      let env, ty = expr env e in
      let env, local = Env.FakeMembers.make p env obj member_name in
      let env = Env.set_local env local ty in
      let local = p, Lvar (p, local) in
      condition_var_non_null env local
  | _ -> env

and condition_isset env = function
  | _, Array_get (x, _) -> condition_isset env x
  | v -> condition_var_non_null env v

(**
 * Build an environment for the true or false branch of
 * conditional statements.
 *)
and condition env tparamet =
  let expr = raw_expr true in function
  | _, Expr_list [] -> env
  | _, Expr_list [x] ->
      let env, _ = expr env x in
      condition env tparamet x
  | r, Expr_list (x::xs) ->
      let env, _ = expr env x in
      condition env tparamet (r, Expr_list xs)
  | _, Call (Cnormal, (_, Id (_, func)), [param], [])
    when SN.PseudoFunctions.isset = func && tparamet &&
    not (Env.is_strict env) ->
      condition_isset env param
  | _, Call (Cnormal, (_, Id (_, func)), [e], [])
    when not tparamet && SN.StdlibFunctions.is_null = func ->
      condition_var_non_null env e
  | r, Binop ((Ast.Eqeq | Ast.EQeqeq as bop),
              (_, Null), e)
  | r, Binop ((Ast.Eqeq | Ast.EQeqeq as bop),
              e, (_, Null)) when not tparamet ->
                let env, x_ty = expr env e in
                let env, x_ty = Env.expand_type env x_ty in
                if bop == Ast.Eqeq
                then check_null_wtf env r x_ty;
                condition_var_non_null env e
  | (p, (Lvar _ | Obj_get _ | Class_get _) as e) ->
      let env, ty = expr env e in
      let env, ety = Env.expand_type env ty in
      (match ety with
      | _, Tarray (None, None)
      | _, Tprim Tbool -> env
      | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
        | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _)
        | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
        | Taccess (_, _, _)) ->
          condition env (not tparamet) (p, Binop (Ast.Eqeq, e, (p, Null))))
  | r, Binop (Ast.Eq None, var, e) when tparamet ->
      let env, e_ty = expr env e in
      let env, e_ty = Env.expand_type env e_ty in
      check_null_wtf env r e_ty;
      condition_var_non_null env var
  | p1, Binop (Ast.Eq None, (_, (Lvar _ | Obj_get _) as lv), (p2, _)) ->
      let env, ty = expr env (p1, Binop (Ast.Eq None, lv, (p2, Null))) in
      condition env tparamet lv
  | p, Binop ((Ast.Diff | Ast.Diff2 as op), e1, e2) ->
      let op = if op = Ast.Diff then Ast.Eqeq else Ast.EQeqeq in
      condition env (not tparamet) (p, Binop (op, e1, e2))
  | _, Binop (Ast.AMpamp, e1, e2) when tparamet ->
      let env = condition env true e1 in
      let env = condition env true e2 in
      env
  | _, Binop (Ast.BArbar, e1, e2) when not tparamet ->
      let env = condition env false e1 in
      let env = condition env false e2 in
      env
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_array ->
      is_array env lv
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_int ->
      is_type env lv Tint
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_bool ->
      is_type env lv Tbool
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_float ->
      is_type env lv Tfloat
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_string ->
      is_type env lv Tstring
  | _, Call (Cnormal, (_, Id (_, f)), [lv], [])
    when tparamet && f = Naming.is_resource ->
      is_type env lv Tresource
  | _, Unop (Ast.Unot, e) ->
      condition env (not tparamet) e
  | _, InstanceOf (ivar, e2) when tparamet && is_instance_var ivar ->
      let env, (p, x) = get_instance_var env ivar in
      let env, x_ty = Env.get_local env x in
      let env, x_ty = Env.expand_type env x_ty in (* We don't want to modify x *)
      begin match instanceof_naming e2 with
        | None ->
          let env, _ = expr env e2 in
          Env.set_local env x (Reason.Rwitness p, Tobject)
        | Some cid ->
          let env, obj_ty = static_class_id (fst e2) env cid in
          (match obj_ty with
            | _, Tgeneric (this, Some (_, Tapply _))
              when this = SN.Typehints.this ->
              let env = Env.set_local env x obj_ty in
              env
            | _, Tapply ((_, cid as _c), _) ->
              let class_ = Env.get_class env cid in
              (match class_ with
                | None -> Env.set_local env x (Reason.Rwitness p, Tobject)
                | Some class_ ->
                  if SubType.is_sub_type env obj_ty x_ty
                  then
                    (* If the right side of the `instanceof` object is
                     * a super type of what we already knew. In this case,
                     * since we already have a more specialized object, we
                     * don't touch the original object. Check out the unit
                     * test srecko.php if this is unclear.
                     *
                     * Note that if x_ty is Tany, no amount of subtype
                     * checking will be able to specify it
                     * further. This is arguably desirable to maintain
                     * the invariant that removing annotations gets rid
                     * of typing errors in partial mode (See also
                     * t3216948).  *)
                    env
                  else Env.set_local env x obj_ty
              )
            | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _
              | Tprim _ | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _
              | Tanon (_, _) | Tunresolved _ | Tobject | Taccess (_, _, _)
              | Tshape _) -> Env.set_local env x (Reason.Rwitness p, Tobject)
          )
      end
  | _, Binop ((Ast.Eqeq | Ast.EQeqeq), e, (_, Null))
  | _, Binop ((Ast.Eqeq | Ast.EQeqeq), (_, Null), e) ->
    let env, _ = expr env e in
    env
  | e ->
    let env, _ = expr env e in
    env

and is_instance_var = function
  | _, (Lvar _ | This) -> true
  | _, Obj_get ((_, This), (_, Id _), _) -> true
  | _, Obj_get ((_, Lvar _), (_, Id _), _) -> true
  | _, Class_get (_, _) -> true
  | _ -> false

and get_instance_var env = function
  | p, Class_get (cname, (_, member_name)) ->
    let env, local = Env.FakeMembers.make_static p env cname member_name in
    env, (p, local)
  | p, Obj_get ((_, This | _, Lvar _ as obj), (_, Id (_, member_name)), _) ->
    let env, local = Env.FakeMembers.make p env obj member_name in
    env, (p, local)
  | _, Lvar (p, x) -> env, (p, x)
  | p, This -> env, (p, this)
  | _ -> failwith "Should only be called when is_instance_var is true"

and check_null_wtf env p ty =
  if Env.is_strict env then
    match ty with
      | _, Toption ty ->
        let env, ty = Env.expand_type env ty in
        (match ty with
          | _, Tmixed
          | _, Tany ->
            Errors.sketchy_null_check p
          | _, Tprim _ ->
            Errors.sketchy_null_check_primitive p
          | _, (Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tvar _ | Tfun _
            | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _)
            | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) -> ())
      | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Tprim _ | Tvar _
        | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _ | Tanon (_, _)
        | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) -> ()

and is_type env e tprim =
  match e with
    | p, Class_get (cname, (_, member_name)) ->
      let env, local = Env.FakeMembers.make_static p env cname member_name in
      Env.set_local env local (Reason.Rwitness p, Tprim tprim)
    | p, Obj_get ((_, This | _, Lvar _ as obj), (_, Id (_, member_name)), _) ->
      let env, local = Env.FakeMembers.make p env obj member_name in
      Env.set_local env local (Reason.Rwitness p, Tprim tprim)
    | _, Lvar (p, x) ->
      Env.set_local env x (Reason.Rwitness p, Tprim tprim)
    | _ -> env

and is_array env = function
  | p, Class_get (cname, (_, member_name)) ->
      let env, local = Env.FakeMembers.make_static p env cname member_name in
      Env.set_local env local (Reason.Rwitness p, Tarray (None, None))
  | p, Obj_get ((_, This | _, Lvar _ as obj), (_, Id (_, member_name)), _) ->
      let env, local = Env.FakeMembers.make p env obj member_name in
      Env.set_local env local (Reason.Rwitness p, Tarray (None, None))
  | _, Lvar (p, x) ->
      Env.set_local env x (Reason.Rwitness p, Tarray (None, None))
  | _ -> env

and string2 env idl =
  List.fold_left (
  fun env x ->
    let env, ty = expr env x in
    let p = fst x in
    let env = SubType.sub_string p env ty in
    env
 ) env idl

and get_implements ~with_checks ~this (env: Typing_env.env) ht =
  let env, ht = Typing_hint.hint env ht in
  match ht with
  | _, Tapply ((p, c), paraml) ->
      let class_ = Env.get_class_dep env c in
      (match class_ with
      | None ->
          (* The class lives in PHP land *)
          env, (SMap.add c ht SMap.empty, SMap.empty)
      | Some class_ ->
          let size1 = List.length class_.tc_tparams in
          let size2 = List.length paraml in
          if size1 <> size2 then Errors.class_arity p class_.tc_pos c size1;
          let this_ty = fst this, Tgeneric (SN.Typehints.this, Some this) in
          let subst =
            Inst.make_subst_with_this ~this:this_ty class_.tc_tparams paraml in
          iter2_shortest begin fun (_, (p, x), cstr) ty ->
            if with_checks
            then match cstr with
            | None -> ()
            | Some cstr ->
                let cstr = snd (Inst.instantiate subst env cstr) in
                ignore (Type.sub_type p Reason.URnone env cstr ty)
            else ()
          end class_.tc_tparams paraml;
          let sub_implements =
            SMap.map
              (fun ty -> snd (Inst.instantiate subst env ty))
              class_.tc_ancestors
          in
          let sub_dimplements =
            SMap.map
              (fun ty -> snd (Inst.instantiate subst env ty))
              class_.tc_ancestors_checked_when_concrete
          in
          let this_ty = (fst this, Tgeneric (SN.Typehints.this, Some this)) in
          let env, ht = Inst.instantiate_this env ht this_ty in
          env, (SMap.add c ht sub_implements, sub_dimplements)
      )
  | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
    | Tvar _ | Tfun _ | Tabstract (_, _, _) | Ttuple _ | Tanon (_, _)
    | Tunresolved _ | Tobject | Tshape _ | Taccess (_, _, _)) ->
      (* The class lives in PHP land *)
      env, (SMap.empty, SMap.empty)

(* In order to type-check a class, we need to know what "parent"
 * refers to. Sometimes people write "parent::", when that happens,
 * we need to know the type of parent.
 *)
and class_def_parent env class_def class_type =
  match class_def.c_extends with
  | (_, Happly ((_, x), _) as parent_ty) :: _ ->
      let parent_type = Env.get_class_dep env x in
      (match parent_type with
      | Some parent_type -> check_parent env class_def class_type parent_type
      | None -> ());
      let env, parent_ty = Typing_hint.hint env parent_ty in
      env, parent_ty
  (* The only case where we have more than one parent class is when
   * dealing with interfaces and interfaces cannot use parent.
   *)
  | _ :: _
  | _ -> env, (Reason.Rnone, Tany)

and check_parent env class_def class_type parent_type =
  let position = fst class_def.c_name in
  (* Are all the parents in Hack? Do we know all their methods?
   * If so, let's check that the abstract methods have been implemented.
   *)
  if class_type.tc_members_fully_known
  then check_parent_abstract position parent_type class_type;
  if parent_type.tc_final
  then Errors.extend_final position parent_type.tc_pos parent_type.tc_name
  else ()

and check_parent_abstract position parent_type class_type =
  if parent_type.tc_kind = Ast.Cabstract &&
    class_type.tc_kind <> Ast.Cabstract
  then begin
    check_extend_abstract position class_type.tc_methods;
    check_extend_abstract position class_type.tc_smethods;
    check_extend_abstract_typeconsts position class_type.tc_typeconsts;
  end
  else ()

and class_def env_up _ c =
  if c.c_mode = Ast.Mdecl
  then ()
  else begin
    if not !auto_complete
    then begin
      NastCheck.class_ env_up c;
      NastInitCheck.class_ env_up c;
    end;
    let env_tmp = Env.set_root env_up (Dep.Class (snd c.c_name)) in
    let tc = Env.get_class env_tmp (snd c.c_name) in
    match tc with
    | None ->
        (* This can happen if there was an error during the declaration
         * of the class.
         *)
        ()
    | Some tc -> class_def_ env_up c tc
  end

and get_self_from_c env c =
  let env, tparams = lfold type_param env c.c_tparams in
  let tparams = List.map begin fun (_, (p, s), param) ->
    Reason.Rwitness p, Tgeneric (s, param)
  end tparams in
  let ret = Reason.Rwitness (fst c.c_name), Tapply (c.c_name, tparams) in
  ret

and class_def_ env_up c tc =
  Typing_variance.class_ (snd c.c_name) tc;
  let env = Env.set_self_id env_up (snd c.c_name) in
  let env = Env.set_mode env c.c_mode in
  let env = Env.set_root env (Dep.Class (snd c.c_name)) in
  let pc, cid = c.c_name in
  let impl = c.c_extends @ c.c_implements @ c.c_uses in
  let self = get_self_from_c env c in
  let env, impl_dimpl =
    lfold (get_implements ~with_checks:true ~this:self) env impl in
  let _, dimpl = List.split impl_dimpl in
  let dimpl = List.fold_right (SMap.fold SMap.add) dimpl SMap.empty in
  let env, parent = class_def_parent env c tc in
  if tc.tc_kind = Ast.Cnormal && tc.tc_members_fully_known
  then begin
    check_extend_abstract pc tc.tc_methods;
    check_extend_abstract pc tc.tc_smethods;
    check_extend_abstract_typeconsts pc tc.tc_typeconsts;
  end;
  let env = Env.set_self env self in
  let env = Env.set_parent env parent in
  if tc.tc_final then begin
    match c.c_kind with
    | Ast.Cinterface -> Errors.interface_final (fst c.c_name)
    | Ast.Cabstract -> ()
    | Ast.Ctrait -> Errors.trait_final (fst c.c_name)
    | Ast.Cenum -> (* the parser won't let enums be final *) assert false
    | Ast.Cnormal -> ()
  end;
  List.iter (class_implements env c) impl;
  SMap.iter (fun _ ty -> class_implements_type env c ty) dimpl;
  List.iter (class_var_def env false c) c.c_vars;
  List.iter (method_def env) c.c_methods;
  let const_types = List.map (class_const_def env) c.c_consts in
  let env = Typing_enum.enum_class_check env tc c.c_consts const_types in
  class_constr_def env c;
  let env = Env.set_static env in
  List.iter (class_var_def env true c) c.c_static_vars;
  List.iter (method_def env) c.c_static_methods;
  if DynamicYield.contains_dynamic_yield tc.tc_extends
  then DynamicYield.check_yield_visibility env c

and check_extend_abstract p smap =
  SMap.iter begin fun x ce ->
    match ce.ce_type with
    | r, Tfun { ft_abstract = true; _ } ->
        Errors.implement_abstract p (Reason.to_pos r) x
    | _, (Tany | Tmixed | Tarray (_, _) | Tgeneric (_,_) | Toption _ | Tprim _
      | Tvar _ | Tfun _ | Tabstract (_, _, _) | Tapply (_, _) | Ttuple _
      | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
      | Taccess (_, _, _)) -> ()
  end smap

(* Type constants must be bound to a concrete type for non-abstract classes.
 * A concrete type for our purposes means it is not a generic.
 *
 * We encode abstract type constants as Tgeneric. We can use this encoding
 * because it is not valid to assign a generic type parameter to a type
 * constant.
 *)
and check_extend_abstract_typeconsts p smap =
  SMap.iter begin fun x ce ->
    match ce.ce_type with
    | r, Tgeneric _ ->
        (* TODO(dreeves): provide better error message *)
        Errors.implement_abstract p (Reason.to_pos r) x
    | _ -> ()
  end smap

and class_const_def env (h, id, e) =
  let env, ty =
    match h with
    | None -> env, Env.fresh_type()
    | Some h -> Typing_hint.hint env h
  in
  let env, ty' = expr env e in
  ignore (Type.sub_type (fst id) Reason.URhint env ty ty');
  ty'

and class_constr_def env c =
  match c.c_constructor with
  | None -> ()
  | Some m -> method_def env m

and class_implements env c1 h =
  let env, ctype2 = Typing_hint.hint env h in
  class_implements_type env c1 ctype2

and class_implements_type env c1 ctype2 =
  let env, params = lfold begin fun env (_, (p, s), param) ->
    let env, param = opt Typing_hint.hint env param in
    env, (Reason.Rwitness p, Tgeneric (s, param))
  end env c1.c_tparams
  in
  let r = Reason.Rwitness (fst (c1.c_name)) in
  let ctype1 = r, Tapply (c1.c_name, params) in
  Typing_extends.check_implements env ctype2 ctype1;
  ()

and class_var_def env is_static c cv =
  let env, ty =
    match cv.cv_expr with
    | None -> env, Env.fresh_type()
    | Some e -> expr env e in
  match cv.cv_type with
  | None when Env.is_strict env ->
      Errors.add_a_typehint (fst cv.cv_id)
  | None ->
      let pos, name = cv.cv_id in
      let name = if is_static then "$"^name else name in
      let var_type = Reason.Rwitness pos, Tany in
      (match cv.cv_expr with
      | None ->
          Typing_suggest.uninitalized_member (snd c.c_name) name env var_type ty;
          ()
      | Some _ ->
          Typing_suggest.save_member name env var_type ty;
          ()
      )
  | Some (p, _ as cty) ->
      let env =
        (* If this is an XHP attribute and we're in strict mode,
           relax to partial mode to allow the use of the "array"
           annotation without specifying type parameters. Until
           recently HHVM did not allow "array" with type parameters
           in XHP attribute declarations, so this is a temporary
           hack to support existing code for now. *)
        (* Task #5815945: Get rid of this Hack *)
        if cv.cv_is_xhp && (Env.is_strict env)
          then Env.set_mode env Ast.Mpartial
          else env in
      let env, cty = Typing_hint.hint env cty in
      let _ = Type.sub_type p Reason.URhint env cty ty in ()

and method_def env m =
  let env = { env with Env.lenv = Env.empty_local } in
  let env = Env.set_local env this (Env.get_self env) in
  let env, ret = (match m.m_ret with
    | None -> env, (Reason.Rwitness (fst m.m_name), Tany)
    | Some ret -> Typing_hint.hint ~ensure_instantiable:true env ret) in
  let env = DynamicYield.method_def env m.m_name ret in
  let m_params = match m.m_variadic with
    | FVvariadicArg param -> param :: m.m_params
    | _ -> m.m_params
  in
  let env = Typing_hint.check_params_instantiable env m_params in
  let env = Typing_hint.check_tparams_instantiable env m.m_tparams in
  let env, params =
    lfold (make_param_type_ ~for_body:true Env.fresh_type) env m_params in
  if Env.is_strict env then begin
    List.iter2 (check_param env) m_params params;
  end;
  let env = List.fold_left2 bind_param env params m_params in
  let env = fun_ ~abstract:m.m_abstract env m.m_unsafe (m.m_ret <> None)
      ret (fst m.m_name) m.m_body m.m_fun_kind in
  let env = List.fold_left (fun env f -> f env) env (Env.get_todo env) in
  match m.m_ret with
    | None when Env.is_strict env && snd m.m_name <> SN.Members.__destruct ->
      (* if we are in strict mode, the only case where we don't want to enforce
       * a return type is when the method is a destructor
       *)
      suggest_return env (fst m.m_name) ret
    | None
    | Some _ -> ()

and typedef_def env_up x typedef =
  NastCheck.typedef env_up x typedef;
  match typedef with
    | _, _, (pos, Hshape fdm) ->
      ignore (check_shape_keys_validity env_up pos (ShapeMap.keys fdm))
    | _ -> ()
