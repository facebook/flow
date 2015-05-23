(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(* This module performs checks after the naming has been done.
   Basically any check that doesn't fall in the typing category. *)
(* Check of type application arity *)
(* Check no `new AbstractClass` (or trait, or interface) *)
(* Check no top-level break / continue *)

(* NOTE: since the typing environment does not generally contain
   information about non-Hack code, some of these checks can
   only be done in strict (i.e. "everything is Hack") mode. Use
   `if Env.is_strict env.tenv` style checks accordingly.
*)


open Utils
open Nast
open Typing_defs
open Typing_deps
open Autocomplete

module Env = Typing_env

module CheckFunctionType = struct
  let rec stmt f_type st = match f_type, st with
    | Ast.FSync, Return (_, None)
    | Ast.FAsync, Return (_, None) -> ()
    | Ast.FSync, Return (_, Some e)
    | Ast.FAsync, Return (_, Some e) ->
        expr f_type e;
        ()
    | (Ast.FGenerator | Ast.FAsyncGenerator), Return (p, e) ->
        (match e with
        None -> ()
        | Some _ -> Errors.return_in_gen p);
        ()

    | _, Throw (_, e)
    | _, Expr e ->
        expr f_type e;
        ()
    | _, Noop
    | _, Fallthrough
    | _, Break _ | _, Continue _ -> ()
    | _, Static_var _ -> ()
    | _, If (_, b1, b2) ->
        block f_type b1;
        block f_type b2;
        ()
    | _, Do (b, _) ->
        block f_type b;
        ()
    | _, While (_, b) ->
        block f_type b;
        ()
    | _, For (_, _, _, b) ->
        block f_type b;
        ()
    | _, Switch (_, cl) ->
        liter case f_type cl;
        ()
    | (Ast.FSync | Ast.FGenerator), Foreach (_, (Await_as_v (p, _) | Await_as_kv (p, _, _)), _) ->
        Errors.await_in_sync_function p;
        ()
    | _, Foreach (_, _, b) ->
        block f_type b;
        ()
    | _, Try (b, cl, fb) ->
        block f_type b;
        liter catch f_type cl;
        block f_type fb;
        ()

  and block f_type stl =
    liter stmt f_type stl

  and case f_type = function
    | Default b -> block f_type b
    | Case (_, b) ->
        block f_type b;
        ()

  and catch f_type (_, _, b) = block f_type b

  and afield f_type = function
    | AFvalue e -> expr f_type e
    | AFkvalue (e1, e2) -> expr2 f_type (e1, e2)

  and expr f_type (p, e) =
    expr_ p f_type e

  and expr2 f_type (e1, e2) =
    expr f_type e1;
    expr f_type e2;
    ()

  and expr_ p f_type exp = match f_type, exp with
    | _, Any -> ()
    | _, Fun_id _
    | _, Method_id _
    | _, Smethod_id _
    | _, Method_caller _
    | _, This
    | _, Id _
    | _, Class_get _
    | _, Class_const _
    | _, Lvar _
    | _, Lplaceholder _ -> ()
    | _, Array afl ->
        liter afield f_type afl;
        ()
    | _, ValCollection (_, el) ->
        liter expr f_type el;
        ()
    | _, KeyValCollection (_, fdl) ->
        liter expr2 f_type fdl;
        ()
    | _, Clone e -> expr f_type e; ()
    | _, Obj_get (e, (_, Id _s), _) ->
        expr f_type e;
        ()
    | _, Obj_get (e1, e2, _) ->
        expr2 f_type (e1, e2);
        ()
    | _, Array_get (e, eopt) ->
        expr f_type e;
        maybe expr f_type eopt;
        ()
    | _, Call (_, e, el, uel) ->
        expr f_type e;
        liter expr f_type el;
        liter expr f_type uel;
        ()
    | _, True | _, False | _, Int _
    | _, Float _ | _, Null | _, String _ -> ()
    | _, String2 (el, _) ->
        liter expr f_type el;
        ()
    | _, List el ->
      liter expr f_type el;
      ()
    | _, Pair (e1, e2) ->
      expr2 f_type (e1, e2);
      ()
    | _, Expr_list el ->
        liter expr f_type el;
        ()
    | _, Unop (_, e) -> expr f_type e
    | _, Binop (_, e1, e2) ->
        expr2 f_type (e1, e2);
        ()
    | _, Eif (e1, None, e3) ->
        expr2 f_type (e1, e3);
        ()
    | _, Eif (e1, Some e2, e3) ->
        liter expr f_type [e1; e2; e3];
      ()
    | _, New (_, el, uel) ->
      liter expr f_type el;
      liter expr f_type uel;
      ()
    | _, InstanceOf (e, _) ->
        expr f_type e;
        ()
    | _, Cast (_, e) ->
        expr f_type e;
        ()
    | _, Efun _ -> ()

    | Ast.FGenerator, Yield_break
    | Ast.FAsyncGenerator, Yield_break -> ()
    | Ast.FGenerator, Yield af
    | Ast.FAsyncGenerator, Yield af -> afield f_type af; ()

    (* Should never happen -- presence of yield should make us FGenerator or
     * FAsyncGenerator. *)
    | Ast.FSync, Yield_break
    | Ast.FAsync, Yield_break
    | Ast.FSync, Yield _
    | Ast.FAsync, Yield _ -> assert false

    | Ast.FGenerator, Await _
    | Ast.FSync, Await _ -> Errors.await_in_sync_function p

    | Ast.FAsync, Await e
    | Ast.FAsyncGenerator, Await e -> expr f_type e; ()

    | _, Special_func func ->
      (match func with
        | Gena e
        | Gen_array_rec e -> expr f_type e
        | Genva el
        | Gen_array_va_rec el -> liter expr f_type el);
      ()
    | _, Xml (_, attrl, el) ->
        List.iter (fun (_, e) -> expr f_type e) attrl;
        liter expr f_type el;
        ()
    | _, Assert (AE_assert e) ->
        expr f_type e;
        ()
    | _, Shape fdm ->
        ShapeMap.iter (fun _ v -> expr f_type v) fdm;
        ()

end

type control_context =
  | Toplevel
  | LoopContext
  | SwitchContext

type env = {
  t_is_finally: bool;
  class_name: string option;
  class_kind: Ast.class_kind option;
  imm_ctrl_ctx: control_context;
  tenv: Env.env;
}

let is_magic =
  let h = Hashtbl.create 23 in
  let a x = Hashtbl.add h x true in
  a Naming_special_names.Members.__set;
  a Naming_special_names.Members.__isset;
  a Naming_special_names.Members.__get;
  a Naming_special_names.Members.__unset;
  a Naming_special_names.Members.__call;
  a Naming_special_names.Members.__callStatic;
  fun (_, s) ->
    Hashtbl.mem h s

let rec fun_ tenv f named_body =
  if !auto_complete then ()
  else begin
    let tenv = Typing_env.set_root tenv (Dep.Fun (snd f.f_name)) in
    let env = { t_is_finally = false;
                class_name = None; class_kind = None;
                imm_ctrl_ctx = Toplevel;
                tenv = tenv } in
    func env f named_body
  end

and func env f named_body =
  let p, fname = f.f_name in
  if String.lowercase (strip_ns fname) = Naming_special_names.Members.__construct
  then Errors.illegal_function_name p fname;
  let env = { env with
    tenv = Env.set_mode env.tenv f.f_mode;
    t_is_finally = false;
  } in
  maybe hint env f.f_ret;
  List.iter (fun_param env) f.f_params;
  block env named_body.fnb_nast;
  CheckFunctionType.block f.f_fun_kind named_body.fnb_nast

and hint env (p, h) =
  hint_ env p h

and hint_ env p = function
  | Hany  | Hmixed  | Habstr _ | Hprim _  | Hthis | Haccess _ ->
      ()
  | Harray (ty1, ty2) ->
      maybe hint env ty1;
      maybe hint env ty2
  | Htuple hl -> List.iter (hint env) hl
  | Hoption h ->
      hint env h; ()
  | Hfun (hl,_, h) ->
      List.iter (hint env) hl;
      hint env h;
      ()
  | Happly ((_, x), hl) when Typing_env.is_typedef x ->
      let tdef = Typing_heap.Typedefs.find_unsafe x in
      let params =
        match tdef with
        | Typing_heap.Typedef.Error -> []
        | Typing_heap.Typedef.Ok (_, x, _, _, _) -> x
      in
      check_params env p x params hl
  | Happly ((_, x), hl) ->
      (match Env.get_class env.tenv x with
      | None -> ()
      | Some class_ ->
          check_params env p x class_.tc_tparams hl
      );
      ()
  | Hshape fdl ->
      ShapeMap.iter (fun _ v -> hint env v) fdl

and check_params env p x params hl =
  let arity = List.length params in
  check_arity env p x arity (List.length hl);
  List.iter (hint env) hl;

and check_arity env p tname arity size =
  if size = arity then () else
  if size = 0 && not (Typing_env.is_strict env.tenv) then () else
  let nargs = soi arity in
  Errors.type_arity p tname nargs

and class_ tenv c =
  if !auto_complete then () else begin
  let cname = Some (snd c.c_name) in
  let tenv = Typing_env.set_root tenv (Dep.Class (snd c.c_name)) in
  let env = { t_is_finally = false;
              class_name = cname;
              class_kind = Some c.c_kind;
              imm_ctrl_ctx = Toplevel;
              tenv = tenv } in
  let env = { env with tenv = Env.set_mode env.tenv c.c_mode } in
  if c.c_kind = Ast.Cinterface then begin
    interface c;
  end
  else begin
    maybe method_ (env, true) c.c_constructor;
  end;
  liter hint env c.c_extends;
  liter hint env c.c_implements;
  liter class_const env c.c_consts;
  liter typeconst (env, (fst c.c_tparams)) c.c_typeconsts;
  liter class_var env c.c_static_vars;
  liter class_var env c.c_vars;
  liter method_ (env, true) c.c_static_methods;
  liter method_ (env, false) c.c_methods;
  liter check_is_interface (env, "implement") c.c_implements;
  liter check_is_interface (env, "require implementation of") c.c_req_implements;
  liter check_is_class env c.c_req_extends;
  liter check_is_trait env c.c_uses;
  end;
  ()

(** Make sure that the given hint points to an interface *)
and check_is_interface (env, error_verb) (x : hint) =
  match (snd x) with
    | Happly (id, _) ->
      (match Env.get_class env.tenv (snd id) with
        | None ->
          (* in partial mode, we can fail to find the class if it's
             defined in PHP. *)
          (* in strict mode, we catch the unknown class error before
             even reaching here. *)
          ()
        | Some { tc_kind = Ast.Cinterface; _ } -> ()
        | Some { tc_name; _ } ->
          Errors.non_interface (fst x) tc_name error_verb
      )
    | _ -> failwith "assertion failure: interface isn't a Happly"

(** Make sure that the given hint points to a non-final class *)
and check_is_class env (x : hint) =
  match (snd x) with
    | Happly (id, _) ->
      (match Env.get_class env.tenv (snd id) with
        | None ->
          (* in partial mode, we can fail to find the class if it's
             defined in PHP. *)
          (* in strict mode, we catch the unknown class error before
             even reaching here. *)
          ()
        | Some { tc_kind = Ast.Cabstract; _ } -> ()
        | Some { tc_kind = Ast.Cnormal; _ } -> ()
        | Some { tc_kind; tc_name; _ } ->
          Errors.requires_non_class (fst x) tc_name (Ast.string_of_class_kind tc_kind)
      )
    | _ -> failwith "assertion failure: interface isn't a Happly"

(**
   * Make sure that all "use"s are with traits, and not
   * classes, interfaces, etc.
*)
and check_is_trait env (h : hint) =
  (* Second part of a hint contains Happly information *)
  (match (snd h) with
  (* An Happly contains identifying info (sid) and hint list (which we *)
  (* do not care about at this time *)
  | Happly (pos_and_name, _) ->
    (* Env.get_class will get the type info associated with the name *)
    let type_info = Env.get_class env.tenv (snd pos_and_name) in
    (match type_info with
      (* in partial mode, it's possible to not find the trait, because the *)
      (* trait may live in PHP land. In strict mode, we catch the unknown *)
      (* trait error before even reaching here. so it's ok to just return *)
      (* unit. *)
      | None -> ()
      (* tc_kind is part of the type_info. If we are a trait, all is good *)
      | Some { tc_kind = Ast.Ctrait; _ } -> ()
      (* Anything other than a trait we are going to throw an error *)
      (* using the tc_kind and tc_name fields of our type_info *)
      | Some { tc_kind; tc_name; _ } ->
        Errors.uses_non_trait (fst h) tc_name (Ast.string_of_class_kind tc_kind)
    )
  | _ -> failwith "assertion failure: trait isn't an Happly"
  )

and interface c =
  (* make sure that interfaces only have empty public methods *)
  List.iter begin fun m ->
    (match m.m_body with
      | UnnamedBody { fub_ast = [] ; _}
      | NamedBody { fnb_nast = [] ; _} ->
        if m.m_visibility <> Public
        then Errors.not_public_interface (fst m.m_name)
        else ()
      | _ -> Errors.abstract_body (fst m.m_name))
  end (c.c_static_methods @ c.c_methods);
  (* make sure that interfaces don't have any member variables *)
  match c.c_vars with
  | hd::_ ->
    let pos = fst (hd.cv_id) in
    Errors.interface_with_member_variable pos
  | _ -> ();
  (* make sure that interfaces don't have static variables *)
  match c.c_static_vars with
  | hd::_ ->
    let pos = fst (hd.cv_id) in
    Errors.interface_with_static_member_variable pos
  | _ -> ()

and class_const env (h, _, e) =
  maybe hint env h;
  maybe expr env e;
  ()

and typeconst (env, class_tparams) tconst =
  maybe hint env tconst.c_tconst_type;
  maybe hint env tconst.c_tconst_constraint;
  (* need to ensure that tconst.c_tconst_type is not Habstr *)
  maybe check_no_class_tparams class_tparams tconst.c_tconst_type;
  maybe check_no_class_tparams class_tparams tconst.c_tconst_constraint

(* Check to make sure we are not using class type params for type const decls *)
and check_no_class_tparams class_tparams (pos, ty)  =
  let check_tparams = check_no_class_tparams class_tparams in
  let maybe_check_tparams = maybe check_no_class_tparams class_tparams in
  let matches_class_tparam tp_name =
    List.iter begin fun (_, (c_tp_pos, c_tp_name), _) ->
      if c_tp_name = tp_name
      then Errors.typeconst_depends_on_external_tparam pos c_tp_pos c_tp_name
    end class_tparams in
  match ty with
    | Hany | Hmixed | Hprim _ | Hthis -> ()
    (* We have found a type parameter. Make sure its name does not match
     * a name in class_tparams *)
    | Habstr (tparam_name, cstr_opt) ->
        maybe_check_tparams (opt_map snd cstr_opt);
        matches_class_tparam tparam_name
    | Harray (ty1, ty2) ->
        maybe_check_tparams ty1;
        maybe_check_tparams ty2
    | Htuple tyl -> List.iter check_tparams tyl
    | Hoption ty_ -> check_tparams ty_
    | Hfun (tyl, _, ty_) ->
        List.iter check_tparams tyl;
        check_tparams ty_
    | Happly (_, tyl) -> List.iter check_tparams tyl
    | Hshape fdl -> ShapeMap.iter (fun _ v -> check_tparams v) fdl
    | Haccess (root_ty, _) ->
        check_tparams root_ty

and class_var env cv =
  let hint_env =
    (* If this is an XHP attribute and we're in strict mode,
       relax to partial mode to allow the use of generic
       classes without specifying type parameters. This is
       a temporary hack to support existing code for now. *)
    (* Task #5815945: Get rid of this Hack *)
    if cv.cv_is_xhp && (Typing_env.is_strict env.tenv)
      then { env with tenv = Typing_env.set_mode env.tenv FileInfo.Mpartial }
      else env in
  maybe hint hint_env cv.cv_type;
  maybe expr env cv.cv_expr;
  ()

and check__toString m is_static =
  if snd m.m_name = Naming_special_names.Members.__toString
  then begin
    if m.m_visibility <> Public || is_static
    then Errors.toString_visibility (fst m.m_name);
    match m.m_ret with
      | Some (_, Hprim Tstring) -> ()
      | Some (p, _) -> Errors.toString_returns_string p
      | None -> ()
  end

and method_ (env, is_static) m =
  let named_body = assert_named_body m.m_body in
  check__toString m is_static;
  liter fun_param env m.m_params;
  block env named_body.fnb_nast;
  maybe hint env m.m_ret;
  CheckFunctionType.block m.m_fun_kind named_body.fnb_nast;
  if m.m_abstract && named_body.fnb_nast <> []
  then Errors.abstract_with_body m.m_name;
  if not (Env.is_decl env.tenv) && not m.m_abstract && named_body.fnb_nast = []
  then Errors.not_abstract_without_body m.m_name;
  (match env.class_name with
  | Some cname ->
      let p, mname = m.m_name in
      if String.lowercase (strip_ns cname) = String.lowercase mname
          && env.class_kind <> Some Ast.Ctrait
      then Errors.dangerous_method_name p
      else ()
  | None -> assert false);
  ()

and fun_param env param =
  maybe hint env param.param_hint;
  maybe expr env param.param_expr;
  ()

and fun_param_opt env (h, _, e) =
  maybe hint env h;
  maybe expr env e;
  ()

and stmt env = function
  | Return (p, _) when env.t_is_finally ->
    Errors.return_in_finally p; ()
  | Return (_, None)
  | Noop
  | Fallthrough -> ()
  | Break p -> begin
    match env.imm_ctrl_ctx with
      | Toplevel -> Errors.toplevel_break p
      | _ -> ()
    end
  | Continue p -> begin
    match env.imm_ctrl_ctx with
      | Toplevel -> Errors.toplevel_continue p
      | SwitchContext -> Errors.continue_in_switch p
      | LoopContext -> ()
    end
  | Return (_, Some e)
  | Expr e | Throw (_, e) ->
    expr env e
  | Static_var el ->
    liter expr env el
  | If (e, b1, b2) ->
    expr env e;
    block env b1;
    block env b2;
    ()
  | Do (b, e) ->
    block { env with imm_ctrl_ctx = LoopContext } b;
    expr env e;
    ()
  | While (e, b) ->
      expr env e;
      block { env with imm_ctrl_ctx = LoopContext } b;
      ()
  | For (e1, e2, e3, b) ->
      expr env e1;
      expr env e2;
      expr env e3;
      block { env with imm_ctrl_ctx = LoopContext } b;
      ()
  | Switch (e, cl) ->
      expr env e;
      liter case { env with imm_ctrl_ctx = SwitchContext } cl;
      ()
  | Foreach (e1, ae, b) ->
      expr env e1;
      as_expr env ae;
      block { env with imm_ctrl_ctx = LoopContext } b;
      ()
  | Try (b, cl, fb) ->
      block env b;
      liter catch env cl;
      block { env with t_is_finally = true } fb;
      ()

and as_expr env = function
  | As_v e
  | Await_as_v (_, e) -> expr env e
  | As_kv (e1, e2)
  | Await_as_kv (_, e1, e2) ->
      expr env e1;
      expr env e2;
      ()

and afield env = function
  | AFvalue e -> expr env e
  | AFkvalue (e1, e2) -> expr env e1; expr env e2;

and block env stl =
  liter stmt env stl

and expr env (_, e) =
  expr_ env e

and expr_ env = function
  | Any
  | Fun_id _
  | Method_id _
  | Smethod_id _
  | Method_caller _
  | This
  | Id _
  | Class_get _
  | Class_const _
  | Lvar _ | Lplaceholder _ -> ()
  | Array afl ->
      liter afield env afl;
      ()
  | ValCollection (_, el) ->
      liter expr env el;
      ()
  | KeyValCollection (_, fdl) ->
      liter field env fdl;
      ()
  | Clone e -> expr env e; ()
  | Obj_get (e, (_, Id s), _) ->
      if is_magic s && Env.is_strict env.tenv
      then Errors.magic s;
      expr env e;
      ()
  | Obj_get (e1, e2, _) ->
      expr env e1;
      expr env e2;
      ()
  | Array_get (e, eopt) ->
      expr env e;
      maybe expr env eopt;
      ()
  | Call (_, e, el, uel) ->
      expr env e;
      liter expr env el;
      liter expr env uel;
      ()
  | True | False | Int _
  | Float _ | Null | String _ -> ()
  | String2 (el, _) ->
      liter expr env el;
      ()
  | Unop (_, e) -> expr env e
  | Yield_break -> ()
  | Special_func func ->
    (match func with
      | Gena e
      | Gen_array_rec e->
        expr env e
      | Genva el
      | Gen_array_va_rec el ->
        liter expr env el);
    ()
  | Yield e ->
      afield env e;
      ()
  | Await e ->
      expr env e;
      ()
  | List el ->
      liter expr env el;
      ()
  | Pair (e1, e2) ->
    expr env e1;
    expr env e2;
    ()
  | Expr_list el ->
      liter expr env el;
      ()
  | Cast (h, e) ->
      hint env h;
      expr env e;
      ()
  | Binop (_, e1, e2) ->
      expr env e1;
      expr env e2;
      ()
  | Eif (e1, None, e3) ->
      expr env e1;
      expr env e3;
      ()
  | Eif (e1, Some e2, e3) ->
      expr env e1;
      expr env e2;
      expr env e3;
      ()
  | Assert (AE_assert e)
  | InstanceOf (e, _) ->
      expr env e;
      ()
  | New (_, el, uel) ->
      liter expr env el;
      liter expr env uel;
      ()
  | Efun (f, _) ->
    let body = Nast.assert_named_body f.f_body in
    func env f body; ()
  | Xml (_, attrl, el) ->
      liter attribute env attrl;
      liter expr env el;
      ()
  | Shape fdm ->
      ShapeMap.iter (fun _ v -> expr env v) fdm

and case env = function
  | Default b -> block env b
  | Case (e, b) ->
      expr env e;
      block env b;
      ()

and catch env (_, _, b) = block env b
and field env (e1, e2) =
  expr env e1;
  expr env e2;
  ()

and attribute env (_, e) =
  expr env e;
  ()

let typedef tenv t =
  let env = { t_is_finally = false;
              class_name = None; class_kind = None;
              imm_ctrl_ctx = Toplevel;
              tenv = tenv } in
  hint env t.t_kind
