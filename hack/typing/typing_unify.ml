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

module Env = Typing_env
module TUtils = Typing_utils
module TDef = Typing_tdef
module Inst = Typing_instantiate
module TUEnv = Typing_unification_env
module TAccess = Typing_taccess

(* Most code -- notably the cases in unify_ -- do *not* need to thread through
 * the uenv, since for example just because we know an array<foo, bar> can't
 * itself be null, that doesn't mean that foo and bar can't be null. *)
let rec unify env ty1 ty2 =
  unify_with_uenv env (TUEnv.empty, ty1) (TUEnv.empty, ty2)

and unify_with_uenv env (uenv1, ty1) (uenv2, ty2) =
  if ty1 == ty2 then env, ty1 else
  match ty1, ty2 with
  | (_, Tany), ty | ty, (_, Tany) -> env, ty
  | (r1, Tvar n1), (r2, Tvar n2) -> unify_var env (r1, uenv1, n1) (r2, uenv2, n2)
  | (r, Tvar n), ty2
  | ty2, (r, Tvar n) ->
      let env, ty1 = Env.get_type env n in
      let n' = Env.fresh() in
      let env = Env.rename env n n' in
      let env, ty = unify_with_uenv env (uenv1, ty1) (uenv2, ty2) in
      let env = Env.add env n ty in
      env, (r, Tvar n')
  | (r1, Tunresolved tyl1), (r2, Tunresolved tyl2) ->
      let r = unify_reason r1 r2 in
      (* TODO this should probably pass through the uenv *)
      let env, tyl = TUtils.normalize_inter env tyl1 tyl2 in
      env, (r, Tunresolved tyl)
  | (r, Tunresolved tyl), (_, ty_ as ty)
  | (_, ty_ as ty), (r, Tunresolved tyl) ->
      let p1 = TUtils.find_pos (Reason.to_pos r) tyl in
      let str_ty = Typing_print.error ty_ in
      let r = Reason.Rcoerced (p1, env.Env.pos, str_ty) in
      let env = List.fold_left (fun env x -> TUtils.sub_type env ty x) env tyl in
      env, (r, ty_)
  | (_, Toption ty1), _ when uenv1.TUEnv.non_null ->
      unify_with_uenv env (uenv1, ty1) (uenv2, ty2)
  | _, (_, Toption ty2) when uenv2.TUEnv.non_null ->
      unify_with_uenv env (uenv1, ty1) (uenv2, ty2)
  | (r1, Toption ty1), (r2, Toption ty2) ->
      let r = unify_reason r1 r2 in
      let uenv1 = { uenv1 with TUEnv.non_null = true } in
      let uenv2 = { uenv2 with TUEnv.non_null = true } in
      let env, ty = unify_with_uenv env (uenv1, ty1) (uenv2, ty2) in
      env, (r, Toption ty)
  (* Mixed is nullable and we want it to unify with both ?T and T at
   * the same time. If we try to unify mixed with an option,
   * we peel of the ? and unify mixed with the underlying type. *)
  | (r2, Tmixed), (_, Toption ty1)
  | (_, Toption ty1), (r2, Tmixed) ->
    unify_with_uenv env (TUEnv.empty, ty1) (TUEnv.empty, (r2, Tmixed))
  | (r1, ty1), (r2, ty2) ->
      let r = unify_reason r1 r2 in
      let env, ty = unify_ env r1 ty1 r2 ty2 in
      env, (r, ty)

and unify_var env (r1, uenv1, n1) (r2, uenv2, n2) =
  let r = unify_reason r1 r2 in
  let env, n1 = Env.get_var env n1 in
  let env, n2 = Env.get_var env n2 in
  if n1 = n2 then env, (r, Tvar n1) else
  let env, ty1 = Env.get_type_unsafe env n1 in
  let env, ty2 = Env.get_type_unsafe env n2 in
  let n' = Env.fresh() in
  let env = Env.rename env n1 n' in
  let env = Env.rename env n2 n' in
  let env, ty = unify_with_uenv env (uenv1, ty1) (uenv2, ty2) in
  (* I ALWAYS FORGET THIS! ALWAYS!!! *)
  (* The type of n' could have changed because of recursive types *)
  (* We need one more round *)
  let env, ty' = Env.get_type env n' in
  let env, ty = unify env ty ty' in
  let env = Env.add env n' ty in
  env, (r, Tvar n')

and unify_ env r1 ty1 r2 ty2 =
  match ty1, ty2 with
  | Tprim (Nast.Tclassname _), Tprim (Nast.Tclassname _) ->
    env, ty1
  | Tprim (Nast.Tclassname _), Tprim (Nast.Tstring)
  | Tprim (Nast.Tstring), Tprim (Nast.Tclassname _) ->
    (* Tclassname is created solely within class_class_decl; as such, treating
     * it as interchangeable with a string for the purposes of unification
     * should be fairly safe. A typical scenario for the unification is array
     * literals like "array('foo', 'bar', C1::class, ...)" *)
    env, Tprim (Nast.Tstring)
  | Tprim x, Tprim y ->
    if x == y then env, Tprim x
    else
      let () = TUtils.uerror r1 ty1 r2 ty2 in
      env, Tany
  | Tarray (None, None), (Tarray _ as ty)
  | (Tarray _ as ty), Tarray (None, None) ->
      env, ty
  | Tarray (Some ty1, None), Tarray (Some ty2, None) ->
      let env, ty = unify env ty1 ty2 in
      env, Tarray (Some ty, None)
  | Tarray (Some ty1, Some ty2), Tarray (Some ty3, Some ty4) ->
      let env, ty1 = unify env ty1 ty3 in
      let env, ty2 = unify env ty2 ty4 in
      env, Tarray (Some ty1, Some ty2)
  | Tfun ft1, Tfun ft2 ->
      let env, ft = unify_funs env r1 ft1 r2 ft2 in
      env, Tfun ft
  | Tclass (((p1, x1) as id), argl1),
      Tclass ((p2, x2), argl2) when String.compare x1 x2 = 0 ->
        (* We handle the case where a generic A<T> is used as A *)
        let argl1 =
          if argl1 = [] && not (Env.is_strict env)
          then List.map (fun _ -> (r1, Tany)) argl2
          else argl1
        in
        let argl2 =
          if argl2 = [] && not (Env.is_strict env)
          then List.map (fun _ -> (r1, Tany)) argl1
          else argl2
        in
        if List.length argl1 <> List.length argl2
        then begin
          let n1 = soi (List.length argl1) in
          let n2 = soi (List.length argl2) in
          Errors.type_arity_mismatch p1 n1 p2 n2;
          env, Tany
        end
        else
          let env, argl = lfold2 unify env argl1 argl2 in
          env, Tclass (id, argl)
  | Tabstract (AKnewtype (x1, argl1), tcstr1),
    Tabstract (AKnewtype (x2, argl2), tcstr2) when String.compare x1 x2 = 0 ->
        if List.length argl1 <> List.length argl2
        then begin
          let n1 = soi (List.length argl1) in
          let n2 = soi (List.length argl2) in
          let p1 = Reason.to_pos r1 in
          let p2 = Reason.to_pos r2 in
          Errors.type_arity_mismatch p1 n1 p2 n2;
          env, Tany
        end
        else
          let env, tcstr =
            match tcstr1, tcstr2 with
            | None, None -> env, None
            | Some x1, Some x2 ->
                let env, x = unify env x1 x2 in
                env, Some x
            | _ -> assert false
          in
          let env, argl = lfold2 unify env argl1 argl2 in
          env, Tabstract (AKnewtype (x1, argl), tcstr)
  | Tabstract (AKgeneric (x1, Some super1), tcstr1),
    Tabstract (AKgeneric (x2, Some super2), tcstr2)
    when x1 = x2 && (Option.is_none tcstr1 = Option.is_none tcstr2) ->
      let env, super = unify env super1 super2 in
      let env, tcstr = match Option.map2 tcstr1 tcstr2 ~f:(unify env) with
        | None -> env, None
        | Some (env, cstr) -> env, Some cstr in
      env, Tabstract (AKgeneric (x1, Some super), tcstr)
  | Tabstract (ak1, tcstr1), Tabstract (ak2, tcstr2)
    when ak1 = ak2 && (Option.is_none tcstr1 = Option.is_none tcstr2) ->
      let env, tcstr = match Option.map2 tcstr1 tcstr2 ~f:(unify env) with
        | None -> env, None
        | Some (env, cstr) -> env, Some cstr in
      env, Tabstract (ak1, tcstr)
  | Tabstract (AKdependent (expr_dep, _),
      Some (_, Tclass ((_, x) as id, _) as ty)), _ ->
      let class_ = Env.get_class env x in
      (* For final class C, there is no difference between abstract<X> and X.
       * The one exception is for new types, because it is considered a distinct
       * type from X.
       *)
      (match class_ with
      | Some {tc_final = true; _} ->
          let env, ty = unify env ty (r2, ty2) in
          env, snd ty
      | _ ->
          (Errors.try_when
             (fun () -> TUtils.simplified_uerror env (r1, ty1) (r2, ty2))
             ~when_: begin fun () ->
               match ty2 with
               | Tclass ((_, y), _) -> y = x
               | Tany | Tmixed | Tarray (_, _) | Tprim _
               | Toption _ | Tvar _ | Tabstract (_, _) | Ttuple _
               | Tanon (_, _) | Tfun _ | Tunresolved _ | Tobject
               | Tshape _ -> false
             end
             ~do_: begin fun error ->
               if expr_dep = `cls x then
                 Errors.exact_class_final id (Reason.to_pos r2) error
               else
                 Errors.this_final id (Reason.to_pos r2) error
             end
          );
          env, Tany
        )
  | _, Tabstract (AKdependent (_, _), Some (_, Tclass _)) ->
      unify_ env r2 ty2 r1 ty1
  | (Ttuple _ as ty), Tarray (None, None)
  | Tarray (None, None), (Ttuple _ as ty) ->
      env, ty
  | Ttuple tyl1, Ttuple tyl2 ->
      let size1 = List.length tyl1 in
      let size2 = List.length tyl2 in
      if size1 <> size2
      then
        let p1 = Reason.to_pos r1 in
        let p2 = Reason.to_pos r2 in
        let n1 = soi size1 in
        let n2 = soi size2 in
        Errors.tuple_arity_mismatch p1 n1 p2 n2;
        env, Tany
      else
        let env, tyl = lfold2 unify env tyl1 tyl2 in
        env, Ttuple tyl
  | Tmixed, Tmixed -> env, Tmixed
  | Tanon (_, id1), Tanon (_, id2) when id1 = id2 -> env, ty1
  | Tanon _, Tanon _ ->
      (* This could be smarter, but the only place where we currently compare
       * two anonymous functions is when trying to normalize intersection -
       * saying that they never unify will just keep the intersection
       * unchanged, which is always a valid option. *)
      TUtils.uerror r1 ty1 r2 ty2;
      env, Tany
  | Tfun ft, Tanon (anon_arity, id)
  | Tanon (anon_arity, id), Tfun ft ->
      (match Env.get_anonymous env id with
      | None ->
        Errors.anonymous_recursive_call (Reason.to_pos r1);
        env, Tany
      | Some anon ->
        let p1 = Reason.to_pos r1 in
        let p2 = Reason.to_pos r2 in
        if not (unify_arities ~ellipsis_is_variadic:true anon_arity ft.ft_arity)
        then Errors.fun_arity_mismatch p1 p2;
        let env, ret = anon env ft.ft_params in
        let env, _ = unify env ft.ft_ret ret in
        env, Tfun ft)
  | Tobject, Tobject
  | Tobject, Tclass _
  | Tclass _, Tobject -> env, Tobject
  | Tshape (fields_known1, fdm1), Tshape (fields_known2, fdm2)  ->
      let on_common_field (env, acc) name ty1 ty2 =
        let env, ty = unify env ty1 ty2 in
        env, Nast.ShapeMap.add name ty acc in
      let on_missing_optional_field (env, acc) name ty =
        env, Nast.ShapeMap.add name ty acc in
      (* We do it both directions to verify that no field is missing *)
      let res = Nast.ShapeMap.empty in
      let env, res = TUtils.apply_shape
        ~on_common_field
        ~on_missing_optional_field
        (env, res) (r1, fields_known1, fdm1) (r2, fields_known2, fdm2) in
      let env, res = TUtils.apply_shape
        ~on_common_field
        ~on_missing_optional_field
        (env, res) (r2, fields_known2, fdm2) (r1, fields_known1, fdm1) in
        (* After doing apply_shape in both directions we can be sure that
         * fields_known1 = fields_known2 *)
      env, Tshape (fields_known1, res)
  | (Tany | Tmixed | Tarray (_, _) | Tprim _ | Toption _
      | Tvar _ | Tabstract (_, _) | Tclass (_, _) | Ttuple _ | Tanon (_, _)
      | Tfun _ | Tunresolved _ | Tobject | Tshape _), _ ->
        (* Make sure to add a dependency on any classes referenced here, even if
         * we're in an error state (i.e., where we are right now). The need for
         * this is extremely subtle. Consider this function:
         *
         * function f(): blah {
         *   // ...
         * }
         *
         * Suppose that "blah" isn't currently defined, and we send the result
         * of f() into a function that expects an int. We'll hit a unification
         * error here, as we should. But, we might later define "blah" to be a
         * type alias, "type blah = int", in another file. In that case, f()
         * needs to be rechecked with the new definition of "blah" present.
         *
         * Normally this isn't a problem. The presence of the error in f() in
         * the first place will cause it to be rechecked when "blah" pops into
         * existance anyways. (And in strict mode, or with assume_php=false, you
         * can't refer to the undefined "blah" anyways.) But there's one
         * important case where this does matter: the JS cross-compile of the
         * typechecker. The JS driver code uses the presence of dependencies to
         * figure out what code to pull into the browser, and it's pretty aggro
         * about not pulling in things it doesn't need. If this dep is missing,
         * it will never pull in "blah" -- which actually does exist, but is
         * "undefined" as far as the typechecker is concerned because the JS
         * driver hasn't pulled it into the browser *yet*. The presence of this
         * dep causes that to happen.
         *
         * Another way to do this might be to look up blah and see if it's
         * defined (and doing this will add the dep for us), and suppress the
         * error if it isn't. We typically say that undefined classes could live
         * in PHP and thus be anything -- but the only way it could unify with
         * a non-class is if it's a type alias, which isn't a PHP feature, so
         * the strictness (and subtlety) is warranted here.
         *
         * And the dep is correct anyways: if there weren't a unification error
         * like this, we'd be pulling in the declaration of "blah" (and adding
         * the dep) anyways.
         *)
        let add env = function
          | Tclass ((_, cid), _) -> Env.add_wclass env cid
          | _ -> () in
        add env ty1;
        add env ty2;
        TUtils.simplified_uerror env (r1, ty1) (r2, ty2);
        env, Tany

and unify_arities ~ellipsis_is_variadic anon_arity func_arity : bool =
  match anon_arity, func_arity with
    | Fellipsis a_min, Fvariadic (f_min, _) when ellipsis_is_variadic ->
      (* we want to allow use the "..." syntax in the declaration of
       * anonymous function types to match named variadic arguments
       * of the "...$args" form as well as unnamed ones *)
      a_min = f_min
    | Fvariadic (a_min, _), Fvariadic (f_min, _)
    | Fellipsis a_min, Fellipsis f_min ->
      a_min = f_min
    | Fstandard (a_min, a_max), Fstandard (f_min, f_max) ->
      a_min = f_min && a_max = f_max
    | _, _ -> false

and unify_reason r1 r2 =
  if r1 = Reason.none then r2 else
    if r2 = Reason.none then r1 else
      let c = Reason.compare r1 r2 in
      if c <= 0 then r1
      else r2

and iunify env ty1 ty2 =
  let env, _ = unify env ty1 ty2 in
  env

(* This function is used to unify two functions *)
(* r1 is the reason for ft1 to have its type (witness) *)
and unify_funs env r1 ft1 r2 ft2 =
  let p = Reason.to_pos r2 in
  let p1 = Reason.to_pos r1 in
  if not (unify_arities ~ellipsis_is_variadic:false ft1.ft_arity ft2.ft_arity)
  then Errors.fun_arity_mismatch p p1;
  let env, var_opt, arity = match ft1.ft_arity, ft2.ft_arity with
    | Fvariadic (_, (n1, var_ty1)), Fvariadic (min, (_n2, var_ty2)) ->
      let env, var = unify env var_ty1 var_ty2 in
      env, Some (n1, var), Fvariadic (min, (n1, var))
    | ar1, _ar2 ->
      env, None, ar1
  in
  let env, params = unify_params env ft1.ft_params ft2.ft_params var_opt in
  let env, ret = unify env ft1.ft_ret ft2.ft_ret in
  env, { ft1 with
    ft_arity = arity;
    ft_params = params;
    ft_ret = ret;
  }

and unify_params env l1 l2 var1_opt =
  match l1, l2, var1_opt with
  | [], l, None -> env, l
  | [], (name2, x2) :: rl2, Some (name1, v1) ->
    let name = if name1 = name2 then name1 else None in
    let env = { env with Env.pos = Reason.to_pos (fst x2) } in
    let env, _ = unify env x2 v1 in
    let env, rl = unify_params env [] rl2 var1_opt in
    env, (name, x2) :: rl
  | l, [], _ -> env, l
  | (name1, x1) :: rl1, (name2, x2) :: rl2, _ ->
    let name = if name1 = name2 then name1 else None in
    let env = { env with Env.pos = Reason.to_pos (fst x1) } in
    let env, _ = unify env x2 x1 in
    let env, rl = unify_params env rl1 rl2 var1_opt in
    env, (name, x2) :: rl

(*****************************************************************************)
(* Exporting *)
(*****************************************************************************)

let () = Typing_utils.unify_ref := unify
