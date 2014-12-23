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
(* Module used to type DynamicYield
 * Each class that uses the DynamicYield trait or which extends a class that
 * uses the DynamicYield trait implicitly defines a few methods. If it
 * explicitly defines a yieldFoo method, then it implicitly also defines genFoo
 * (unless this method is explicitly defined).
 * It does this with __call().
 *)
(*****************************************************************************)
open Nast
open Utils
open Typing_defs

module Reason = Typing_reason
module Type   = Typing_ops
module Env    = Typing_env
module SN     = Naming_special_names

(* Classes that use the DynamicYield trait and implement yieldFoo also provide
 * genFoo()
 *)
let rec decl env methods =
  SMap.fold begin fun name ce (env, acc) ->
    match parse_yield_name name with
      | Some base ->
        let ce_r, ft = match ce.ce_type with
          | r, Tfun ft -> r, ft
          | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _)
            | Toption _ | Tvar _ | Tabstract (_, _, _) | Tapply (_, _)
            | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject | Tshape _
            | Taccess (_, _, _)) ->
              assert false in
        let r = fst ft.ft_ret in
        let p = Reason.to_pos r in
        (* Fail silently, since we're in an early stage. A later check will assert
         * that yieldFoo() has the right type *)
        let env, base_ty =
          Errors.try_
            (fun () -> check_yield_types env ft.ft_pos ft.ft_ret)
            (fun _ -> (env, (Reason.Rwitness ft.ft_pos, Tany)))
        in
        (* Define genFoo(), which is Awaitable<T> if yieldFoo() is Awaitable<T>
         * If yieldFoo() is Tany, then genFoo() is Awaitable<Tany> *)
        let gen_name = "gen"^base in
        let gen_r = Reason.Rdynamic_yield (p, ft.ft_pos, gen_name, name) in
        let gen_ty = ce_r, Tfun {ft with
          ft_ret =  gen_r, Tapply ((p, SN.Classes.cAwaitable), [base_ty])
        } in
        let acc = add gen_name {ce with ce_type = gen_ty} acc in
        env, acc
      | None ->
        (match parse_get_name name with
          | None -> env, acc
          | Some base ->
            let yield_name = "yield"^base in
            if SMap.mem yield_name acc then
              (* if there's a yield and a get defined, the gen from the yield wins *)
              env, acc
            else
              let gen_name = "gen"^base in
              (* Define genFoo(), which is Awaitable<T> if getFoo() is T *
               * If getFoo() is Tany, then genFoo() is Awaitable<Tany> *)
              let ce_r, ft = match ce.ce_type with
                | r, Tfun ft -> r, ft
                | _, (Tany | Tmixed | Tarray (_, _) | Tprim _ | Tgeneric (_, _)
                  | Toption _ | Tvar _ | Tabstract (_, _, _) | Tapply (_, _)
                  | Ttuple _ | Tanon (_, _) | Tunresolved _ | Tobject
                  | Tshape _ | Taccess (_, _, _)) -> assert false in
              let p = Reason.to_pos (fst ft.ft_ret) in
              let gen_r = Reason.Rdynamic_yield (p, ft.ft_pos, gen_name, name) in
              let gen_ty = ce_r, Tfun {ft with
                ft_ret = gen_r, Tapply ((p, SN.Classes.cAwaitable), [ft.ft_ret])
              } in
              let acc = add gen_name {ce with ce_type = gen_ty} acc in
              env, acc
        )
  end methods (env, methods)

and check_yield_types env p hret =
  let type_var = Env.fresh_type() in
  let r = Reason.Rwitness p in
  let expected_type = r, Tapply ((p, SN.Classes.cAwaitable), [type_var]) in
  let env = Type.sub_type p (Reason.URdynamic_yield) env expected_type hret in
  (* Fully expand to make doubly sure we don't leak any type variables *)
  env, Typing_expand.fully_expand env type_var

and contains_dynamic_yield = SSet.mem SN.FB.cDynamicYield
and contains_dynamic_yield_interface = SSet.mem SN.FB.cIUseDynamicYield
and implements_dynamic_yield_interface ancestors = SMap.mem SN.FB.cIUseDynamicYield ancestors
and is_dynamic_yield name = (name = SN.FB.cDynamicYield)

and remove_prefix prefix str =
  if str_starts_with str prefix
  then begin
    let prefix_len = String.length prefix in
    Some (String.sub str prefix_len ((String.length str) - prefix_len))
  end else None

and parse_yield_name name =
  remove_prefix "yield" name

and parse_get_name name =
  remove_prefix "get" name

and add name ce acc =
  match SMap.get name acc with
    (* In a perfect world, we could just always add to the map. This is not a
     * perfect world. It is filled with sinners and people who talk in theaters
     * and www engineers who override a synthesized gen/get/prepare method with
     * their own, physically provided in the file. Which since DY is implemented
     * with __call is the one that gets called at runtime. But you also need to
     * be able to override one DY method with another. So we need to do the
     * following:
     *
     * - If the method we're about to synthesize doesn't exist yet, just add it.
     * - If it does, check:
     * -- Is it abstract? If so, we are implementing that abstract method, we
     *    should go ahead and overwrite it.
     * -- Is its return type reason Rdynamic_yield? If so, assume that we were
     *    the ones that synthesized it in the first place and go ahead and
     *    overwrite, so you can write a more specific return type in a child
     *    class, for example. Checking the return type reason isn't terribly
     *    clean, but it's a nice convenient flag that we synthesized it. If you
     *    are reading this because it's causing problems, I'm sorry, and feel
     *    free to add something to the appropriate record type.
     * -- Otherwise, assume the version that is there was physically provided by
     *    the code author, and keep theirs instead of synthesizing our own.
     *
     * One day, there will be a Glorious Revolution and DY will be moved into
     * HHVM, and there will be perf wins and dancing angels and anti-patterns
     * like this will go away. But today is not that day. Today, there is only
     * tears and a deep, yearning hole my heart. *)
    | None
    | Some { ce_type = (_, Tfun { ft_ret = Reason.Rdynamic_yield _, _; _ }); _ }
    | Some { ce_type = (_, Tfun { ft_abstract = true; _ }); _ } ->
      SMap.add name ce acc
    | _ -> acc

let clean_dynamic_yield env methods =
  env, SMap.filter begin
    fun name _ -> name <> Naming_special_names.Members.__call
  end methods

let method_def env name hret =
  let class_ = Env.get_class env (Env.get_self_id env) in
  match class_, parse_yield_name (snd name) with
    | None, _
    | _, None -> env
    | Some c, Some base_name ->
      if contains_dynamic_yield c.tc_extends
      then fst (check_yield_types env (fst name) hret)
      else env

let check_yield_visibility env c =
  let uses_dy_directly = List.exists begin fun trait ->
    match trait with
      | (_, Happly ((pos, name), _)) -> begin
          (* Either you directly use DynamicYield, or something you directly
           * use itself uses DynamicYield. *)
          is_dynamic_yield name || match Env.get_class_dep env name with
            | Some parent_type -> contains_dynamic_yield parent_type.tc_extends
            | None -> false
          end
      | _ -> assert false
  end c.c_uses in
  if not uses_dy_directly then List.iter begin fun m ->
    match (parse_yield_name (snd m.m_name), m.m_visibility) with
      | (Some _, Private) ->
          Errors.dynamic_yield_private (fst m.m_name)
      | _ -> ()
  end c.c_methods
