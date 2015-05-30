(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(* module checking that all the class members are properly initialized *)
open Utils
open Nast
open Typing_defs

module SN = Naming_special_names

(* Exception raised when we hit a return statement and the initialization
 * is not over.
 * When that is the case, we bubble up back to the toplevel environment.
 * An example (right hand side is the set of things initialized):
 *
 *  $this->x = 0;  // { x }
 *  if(...) {
 *     $this->y = 1; // { x, y }
 *     if(...) {
 *        $this->z = 2; // { x, y, z }
 *        return; // raise InitReturn with set { x, y, z}
 *     } // exception caught, re-raise with { x, y }
 *  } // exception caught, re-reraise with { x }
 *
 *  What is effectively initialized: { x }
 *)
exception InitReturn of SSet.t

type prop_status =
  | Vnull (* The value is still potentially null *)
  | Vinit (* Yay! it has been initialized *)

let parent_init_prop = "parent::" ^ SN.Members.__construct

(* Module initializing the environment
   Originally, every class member has 2 possible states,
   Vok  ==> when it is declared as optional, it is the job of the
            typer to make sure it is always check for the null case
            not our problem here
   Vnull ==> The value is now null, it MUST be initialized,
             and cannot be used before it has been initialized.

   Concerning the methods, basically the information we are
   interested in is, which class members do they initialize?
   But we don't want to recompute it every time it is called.
   So we memoize the result: hence the type method status.
 *)
module Env = struct

  type method_status =
    (* We already computed this method *)
    | Done

    (* We have never computed this private method before *)
    | Todo of func_body

  type t = {
    methods : method_status ref SMap.t ;
    props   : SSet.t ;
  }

  (* If we need to call parent::__construct, we treat it as if it were
   * a class variable that needs to be initialized. It's a bit hacky
   * but it works. The idea here is that if the parent needs to be
   * initialized, we add a phony class variable. *)
  let add_parent_construct c tenv props parent_hint =
    match parent_hint with
      | (_, Happly ((_, parent), _)) ->
        let class_ = Typing_env.get_class tenv parent in
        (match class_ with
          | Some class_ when
              class_.Typing_defs.tc_need_init && c.c_constructor <> None
              -> SSet.add parent_init_prop props
          | _ -> props
        )
      | _ -> props

  let parent tenv props c =
    if c.c_mode = FileInfo.Mdecl then props
    else
      if c.c_kind = Ast.Ctrait
      then List.fold_left (add_parent_construct c tenv) props c.c_req_extends
      else match c.c_extends with
      | [] -> props
      | parent_hint :: _ -> add_parent_construct c tenv props parent_hint

  let prop_needs_init add_to_acc acc cv =
    if cv.cv_is_xhp then acc else begin
      match cv.cv_type with
        | None
        | Some (_, Hoption _)
        | Some (_, Hmixed) -> acc
        | Some _ when cv.cv_expr = None -> add_to_acc cv acc
        | Some _ -> acc
    end

  let parent_props tenv acc c =
    List.fold_left begin fun acc parent ->
      match parent with _, Happly ((_, parent), _) ->
        let tc = Typing_env.get_class tenv parent in
        (match tc with
          | None -> acc
          | Some { tc_deferred_init_members = members; _ } -> SSet.union members acc)
        | _ -> acc
    end acc (c.c_extends @ c.c_uses)

  (* return a tuple of the private init-requiring props of the class
   * and the other init-requiring props of the class and its ancestors *)
  let classify_props_for_decl tenv c =
    let tenv = Typing_env.set_root tenv (Typing_deps.Dep.Class (snd c.c_name)) in
    let adder = begin fun cv (private_props, hierarchy_props) ->
      let cname = snd cv.cv_id in
      if cv.cv_visibility = Private then
        (SSet.add cname private_props), hierarchy_props
      else
        private_props, (SSet.add cname hierarchy_props)
    end in
    let acc = (SSet.empty, SSet.empty) in
    let priv_props, props = List.fold_left (prop_needs_init adder) acc c.c_vars in
    let props = parent_props tenv props c in
    let props = parent tenv props c in
    priv_props, props

  let rec make tenv c =
    let tenv = Typing_env.set_root tenv (Typing_deps.Dep.Class (snd c.c_name)) in
    let methods = List.fold_left method_ SMap.empty c.c_methods in
    let adder = begin fun cv acc -> SSet.add (snd cv.cv_id) acc end in
    let props =
      List.fold_left (prop_needs_init adder) SSet.empty c.c_vars in
    let props = parent_props tenv props c in
    let props = parent tenv props c in
    { methods = methods; props = props }

  and method_ acc m =
    if m.m_visibility <> Private then acc else
      let name = snd m.m_name in
      let acc = SMap.add name (ref (Todo m.m_body)) acc in
      acc

  let get_method env m =
    SMap.get m env.methods

end

open Env

(*****************************************************************************)
(* List of functions that can use '$this' before the initialization is
 * over.
 *)
(*****************************************************************************)

let is_whitelisted = function
  | x when x = SN.StdlibFunctions.get_class -> true
  | _ -> false

let rec class_decl ~has_own_cstr tenv c =
  if not has_own_cstr && (c.c_kind = Ast.Ctrait || c.c_kind = Ast.Cabstract)
  then
    (* private properties cannot be initialized without a constructor *)
    let priv_props, props = Env.classify_props_for_decl tenv c in
    if priv_props <> SSet.empty && (c.c_kind = Ast.Cabstract) then
      (* XXX: should priv_props be checked for a trait?
       *  see chown_privates in typing_inherit *)
      Errors.constructor_required c.c_name priv_props;
    SSet.union priv_props props
  else SSet.empty

and class_ tenv c =
  if c.c_mode = FileInfo.Mdecl then () else
  match c.c_constructor with
  | _ when c.c_kind = Ast.Cinterface -> ()
  | Some { m_body = NamedBody { fnb_unsafe = true; _ }; _ } -> ()
  | _ -> (
    let p = match c.c_constructor with
      | Some m -> fst m.m_name
      | None -> fst c.c_name
    in
    let env = Env.make tenv c in
    let inits = constructor env c.c_constructor in

    let check_inits = begin fun () ->
      let uninit_props = SSet.diff env.props inits in
      if SSet.empty <> uninit_props then begin
        if SSet.mem parent_init_prop uninit_props then
          Errors.no_construct_parent p
        else
          Errors.not_initialized (p, snd c.c_name) uninit_props
      end
    end in

    Typing_suggest.save_initialized_members (snd c.c_name) inits;
    if c.c_kind = Ast.Ctrait || c.c_kind = Ast.Cabstract
    then begin
      let has_constructor = match c.c_constructor with
        | None -> false
        | Some m when m.m_abstract -> false
        | Some _ -> true in
      if has_constructor then check_inits () else ()
    end
    else check_inits ()
  )

and constructor env cstr =
  match cstr with
    | None -> SSet.empty
    | Some cstr ->
      let b = Nast.assert_named_body cstr.m_body in
      toplevel env SSet.empty b.fnb_nast

and assign _env acc x =
  SSet.add x acc

and assign_expr env acc e1 =
  match e1 with
    | _, Obj_get ((_, This), (_, Id (_, y)), _) ->
      assign env acc y
    | _, List el ->
      List.fold_left (assign_expr env) acc el
    | _ -> acc

and stmt env acc st =
  let expr = expr env in
  let block = block env in
  let catch = catch env in
  let case = case env in
  match st with
    | Expr (_, Call (Cnormal, (_, Class_const (CIparent, (_, m))), el, _uel))
        when m = SN.Members.__construct ->
      let acc = List.fold_left expr acc el in
      assign env acc parent_init_prop
    | Expr e -> expr acc e
    | Break _ -> acc
    | Continue _ -> acc
    | Throw (_, e) -> expr acc e
    | Return (_, None) ->
      if are_all_init env acc
      then acc
      else raise (InitReturn acc)
    | Return (_, Some x) ->
      let acc = expr acc x in
      if are_all_init env acc
      then acc
      else raise (InitReturn acc)
    | Static_var el -> List.fold_left expr acc el
    | If (e1, b1, b2) ->
      let acc = expr acc e1 in
      let is_term1 = Nast_terminality.Terminal.block b1 in
      let is_term2 = Nast_terminality.Terminal.block b2 in
      let b1 = block acc b1 in
      let b2 = block acc b2 in
      if is_term1
      then SSet.union acc b2
      else if is_term2
      then SSet.union acc b1
      else SSet.union acc (SSet.inter b1 b2)
    | Do (b, e) ->
      let acc = block acc b in
      expr acc e
    | While (e, _) ->
      expr acc e
    | For (e1, _, _, _) ->
      expr acc e1
    | Switch (e, cl) ->
      let acc = expr acc e in
      let _ = List.map (case acc) cl in
      let cl = List.filter (function c -> not (Nast_terminality.Terminal.case c)) cl in
      let cl = List.map (case acc) cl in
      let c = inter_list cl in
      SSet.union acc c
    | Foreach (e, _, _) ->
      let acc = expr acc e in
      acc
    | Try (b, cl, fb) ->
      let c = block acc b in
      let f = block acc fb in
      let _ = List.map (catch acc) cl in
      let cl = List.filter (fun (_, _, b) -> not (Nast_terminality.Terminal.block b)) cl in
      let cl = List.map (catch acc) cl in
      let c = inter_list (c :: cl) in
      (* the finally block executes even if *none* of try and catch do *)
      let acc = SSet.union acc f in
      SSet.union acc c
  | Fallthrough
  | Noop -> acc

and toplevel env acc l =
  try List.fold_left (stmt env) acc l
  with InitReturn acc -> acc

and block env acc l =
  let acc_before_block = acc in
  try
    List.fold_left (stmt env) acc l
  with InitReturn _ ->
    (* The block has a return statement, forget what was initialized in it *)
    raise (InitReturn acc_before_block)

and are_all_init env set =
  SSet.fold (fun cv acc -> SSet.mem cv set && acc) env.props true

and check_all_init p env acc =
  SSet.iter begin fun cv ->
    if not (SSet.mem cv acc)
    then Errors.call_before_init p cv
  end env.props

and exprl env acc l = List.fold_left (expr env) acc l
and expr env acc (p, e) = expr_ env acc p e
and expr_ env acc p e =
  let expr = expr env in
  let exprl = exprl env in
  let field = field env in
  let afield = afield env in
  let fun_paraml = fun_paraml env in
  match e with
  | Any -> acc
  | Array fdl -> List.fold_left afield acc fdl
  | ValCollection (_, el) -> exprl acc el
  | KeyValCollection (_, fdl) -> List.fold_left field acc fdl
  | This -> check_all_init p env acc; acc
  | Fun_id _
  | Method_id _
  | Smethod_id _
  | Method_caller _
  | Id _ -> acc
  | Lvar _ | Lplaceholder _ -> acc
  | Obj_get ((_, This), (_, Id (_, vx as v)), _) ->
      if SSet.mem vx env.props && not (SSet.mem vx acc)
      then (Errors.read_before_write v; acc)
      else acc
  | Clone e -> expr acc e
  | Obj_get (e1, e2, _) ->
      let acc = expr acc e1 in
      expr acc e2
  | Array_get (e, eo) ->
      let acc = expr acc e in
      (match eo with
      | None -> acc
      | Some e -> expr acc e)
  | Class_const _
  | Class_get _ -> acc
  | Call (Cnormal, (p, Obj_get ((_, This), (_, Id (_, f)), _)), _, _) ->
      let method_ = Env.get_method env f in
      (match method_ with
      | None ->
          check_all_init p env acc;
          acc
      | Some method_ ->
          (match !method_ with
          | Done -> acc
          | Todo b ->
            method_ := Done;
            let fb = Nast.assert_named_body b in
            toplevel env acc fb.fnb_nast
          )
      )
  | Call (_, e, el, uel) ->
    let el = el @ uel in
    let el =
      match e with
        | _, Id (_, fun_name) when is_whitelisted fun_name ->
          List.filter begin function
            | _, This -> false
            | _ -> true
          end el
        | _ -> el
    in
    let acc = List.fold_left expr acc el in
    expr acc e
  | True
  | False
  | Int _
  | Float _
  | Null
  | String _
  | String2 _ -> acc
  | Assert (AE_assert e) -> expr acc e
  | Yield e -> afield acc e
  | Yield_break -> acc
  | Await e -> expr acc e
  | List _ ->
      (* List is always an lvalue *)
      acc
  | Expr_list el ->
      exprl acc el
  | Special_func (Gena e)
  | Special_func (Gen_array_rec e) ->
      expr acc e
  | Special_func (Genva el)
  | Special_func (Gen_array_va_rec el) ->
      exprl acc el
  | New (_, el, uel) ->
      exprl acc (el @ uel)
  | Pair (e1, e2) ->
    let acc = expr acc e1 in
    expr acc e2
  | Cast (_, e)
  | Unop (_, e) -> expr acc e
  | Binop (Ast.Eq None, e1, e2) ->
      let acc = expr acc e2 in
      assign_expr env acc e1
  | Binop (Ast.AMpamp, e, _)
  | Binop (Ast.BArbar, e, _) ->
      expr acc e
  | Binop (_, e1, e2) ->
      let acc = expr acc e1 in
      expr acc e2
  | Eif (e1, None, e3) ->
      let acc = expr acc e1 in
      expr acc e3
  | Eif (e1, Some e2, e3) ->
      let acc = expr acc e1 in
      let acc = expr acc e2 in
      expr acc e3
  | InstanceOf (e, _) -> expr acc e
  | Efun (f, _) ->
      let acc = fun_paraml acc f.f_params in
      (* We don't need to analyze the body of closures *)
      acc
  | Xml (_, l, el) ->
      let l = List.map snd l in
      let acc = exprl acc l in
      exprl acc el
  | Shape fdm ->
      ShapeMap.fold begin fun _ v acc ->
        expr acc v
      end fdm acc

and case env acc = function
  | Default b -> block env acc b
  | Case (_, e) -> block env acc e

and catch env acc (_, _, b) = block env acc b

and field env acc (e1, e2) =
  let acc = expr env acc e1 in
  let acc = expr env acc e2 in
  acc

and afield env acc = function
  | AFvalue e ->
      expr env acc e
  | AFkvalue (e1, e2) ->
      let acc = expr env acc e1 in
      let acc = expr env acc e2 in
      acc

and fun_param env acc param =
  match param.param_expr with
  | None -> acc
  | Some x -> expr env acc x

and fun_paraml env acc l = List.fold_left (fun_param env) acc l
