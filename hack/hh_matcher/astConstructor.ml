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
(* This module defines a visitor-like class that can reconstruct a node in the
 * AST with some modifications based on an environment and some overridden
 * methods.
 *
 * To use it you must inherit the generic object and redefine the appropriate
 * methods for replacing the nodes you want to replace.
 *)
(*****************************************************************************)

open Ast

class type ['a] ast_constructor_type =
object
  (* core reconstruction methods *)
  method on_stmt : 'a -> stmt -> stmt list

  method on_pos : 'a -> Pos.t -> Pos.t
  method on_block : 'a -> block -> block
  method on_case : 'a -> case -> case
  method on_catch : 'a -> catch -> catch
  method on_as_expr : 'a -> as_expr -> as_expr

  method on_id : 'a -> id -> id
  method on_afield : 'a -> afield -> afield
  method on_fun_ : 'a -> fun_ -> fun_
  method on_shape : 'a -> (shape_field_name * expr) -> (shape_field_name * expr)
  method on_shape_field_name : 'a -> shape_field_name -> shape_field_name
  method on_pstring : 'a -> pstring -> pstring
  method on_hint : 'a -> hint -> hint
  method on_hint_ : 'a -> hint_ -> hint_
  method on_shape_field : 'a -> shape_field -> shape_field
  method on_expr : 'a -> expr -> expr list
  method on_expr_ : 'a -> expr_ -> expr_
end


(* Helpers for factoring out code to handle options, lists *)
let handle_option
      (type b) (fn : 'a -> b -> b) (env : 'a) (elem : b option) : b option =
  match elem with
  | None -> elem
  | Some elem -> Some (fn env elem)

let handle_list
      (type b) (fn : 'a -> b -> b) (env : 'a) (elems : b list) : b list =
  List.map (fun x -> fn env x) elems

(* for handling lists where the function returns a list *)
let handle_list_list
      (type b) (fn : 'a -> b -> b list) (env : 'a) (elems : b list) : b list =
  List.map (fn env) elems |>
    List.concat

(* assumption is that we shouldn't be replacing a single node with a list of
   nodes where that results in an invalid AST, so if we're converting
   to a single node we should be converting a single node *)
let list_to_single
      (type b) (fn : 'a -> b -> b list) (env : 'a) (elem : b) : b =
  match fn env elem with
  | [ele] -> ele
  | _ -> failwith ("cannot convert an empty list or list of multiple" ^
                     "elements to a single element.")

class virtual ['a] ast_constructor: ['a] ast_constructor_type =
object(this)

  method on_stmt env s =
    (match s with
    | Expr e                  -> Expr (list_to_single this#on_expr env e)
    | Break p                 -> Break (this#on_pos env p)
    | Block b                 -> Block (this#on_block env b)
    | Continue p              -> Continue (this#on_pos env p)
    | Throw   (e)             -> Throw (list_to_single this#on_expr env e)
    | Return  (p, eopt)       ->
       Return (this#on_pos env p,
               handle_option (list_to_single @@ this#on_expr) env eopt)
    | If      (e, b1, b2)     ->
       If (list_to_single this#on_expr env e,
           this#on_block env b1, this#on_block env b2)
    | Do      (b, e)          -> Do (this#on_block env b,
                                     list_to_single this#on_expr env e)
    | While   (e, b)          -> While (list_to_single this#on_expr env e,
                                        this#on_block env b)
    | For     (e1, e2, e3, b) ->
       For (list_to_single this#on_expr env e1,
            list_to_single this#on_expr env e2,
            list_to_single this#on_expr env e3,
            this#on_block env b)
    | Switch  (e, cl)         ->
       Switch (list_to_single this#on_expr env e,
               handle_list this#on_case env cl)
    | Foreach (e, popt, ae, b)->
       Foreach (list_to_single this#on_expr env e,
                handle_option this#on_pos env popt,
                this#on_as_expr env ae, this#on_block env b)
    | Try     (b, cl, fb)     ->
       Try (this#on_block env b, handle_list this#on_catch env cl,
            this#on_block env fb)
    | Static_var el           ->
       Static_var (handle_list_list this#on_expr env el)
    | Unsafe | Noop | Fallthrough -> s) |>
      function stm -> [stm]

  method on_pos _env p = p

  method on_block env b = handle_list_list this#on_stmt env b

  method on_case env = function
    | Default b -> Default (this#on_block env b)
    | Case (e, b) -> Case (list_to_single this#on_expr env e,
                           this#on_block env b)

  method on_catch env (i1, i2, b) =
    this#on_id env i1, this#on_id env i2, this#on_block env b

  method on_as_expr env = function
    | As_v e -> As_v (list_to_single this#on_expr env e)
    | As_kv (e1, e2) -> As_kv (list_to_single this#on_expr env e1,
                               list_to_single this#on_expr env e2)

  method on_expr env (p, e) = [(p, this#on_expr_ env e)]

  method on_expr_ env e =
    match e with
   | Unsafeexpr e-> Unsafeexpr (list_to_single this#on_expr env e)
   | Collection (i, afl) ->
      Collection (this#on_id env i, handle_list this#on_afield env afl)
   | Lfun f          -> Lfun (this#on_fun_ env f)
   | Import (ifv, e) -> Import (ifv, list_to_single this#on_expr env e)
   | Array afl   -> Array (handle_list this#on_afield env afl)
   | Shape sh    -> Shape (handle_list this#on_shape env sh)
   | True        -> True
   | False       -> False
   | Int n       -> Int (this#on_pstring env n)
   | Float n     -> Float (this#on_pstring env n)
   | String s    -> String (this#on_pstring env s)
   | Id id       -> Id (this#on_id env id)
   | Lvar id     -> Lvar (this#on_id env id)
   | Yield af    -> Yield (this#on_afield env af)
   | Await e     -> Await (list_to_single this#on_expr env e)
   | List el     -> List (handle_list_list this#on_expr env el)
   | Clone e     -> Clone (list_to_single this#on_expr env e)
   | Expr_list el    -> Expr_list (handle_list_list this#on_expr env el)
   | Obj_get     (e1, e2, onf) ->
      Obj_get (list_to_single this#on_expr env e1,
               list_to_single this#on_expr env e2, onf)
   | Array_get   (e1, e2o)    ->
      Array_get (list_to_single this#on_expr env e1,
                 handle_option (list_to_single @@ this#on_expr) env e2o)
   | Class_get   (id, pstr)   ->
      Class_get (this#on_id env id, this#on_pstring env pstr)
   | Class_const (id, pstr)   ->
      Class_const (this#on_id env id, this#on_pstring env pstr)
   | Call        (e, el, uel) ->
      Call (list_to_single this#on_expr env e,
            handle_list_list this#on_expr env el,
           handle_list_list this#on_expr env uel)
   | String2     el           -> String2 (handle_list_list this#on_expr env el)
   | Cast        (hint, e)   -> Cast (this#on_hint env hint,
                                      list_to_single this#on_expr env e)
   | Unop        (uop, e)         ->
      Unop (uop, list_to_single this#on_expr env e)
   | Binop       (bop, e1, e2)    ->
      Binop (bop, list_to_single this#on_expr env e1,
             list_to_single this#on_expr env e2)
   | Eif         (e1, e2o, e3)     ->
      Eif (list_to_single this#on_expr env e1,
           handle_option (list_to_single @@ this#on_expr) env e2o,
           list_to_single this#on_expr env e3)
   | NullCoalesce (e1, e2) ->
      NullCoalesce (list_to_single this#on_expr env e1,
                    list_to_single this#on_expr env e2)
   | InstanceOf  (e1, e2)         ->
      InstanceOf (list_to_single this#on_expr env e1,
                  list_to_single this#on_expr env e2)
   | New         (e, el, uel)   ->
      New (list_to_single this#on_expr env e,
           handle_list_list this#on_expr env el,
           handle_list_list this#on_expr env uel)
   | Efun        (f, idbl)         ->
      Efun (this#on_fun_ env f,
            handle_list (fun env (id, bo) -> this#on_id env id, bo) env idbl)
   | Xml         (id, attrl, el) ->
      (* TODO make xml work *)
      Xml (id, attrl, el)
   | Yield_break | Null        -> e

  method on_id _ i = i
  method on_pstring _ pstr = pstr

  method on_afield env = function
    | AFvalue e -> AFvalue (list_to_single this#on_expr env e)
    | AFkvalue (e1, e2) -> AFkvalue (list_to_single this#on_expr env e1,
                                     list_to_single this#on_expr env e2)

  (* TODO make fun_params, tparams work
     also everything else *)
  method on_fun_ env f =
    { f with
      f_ret = handle_option this#on_hint env f.f_ret;
      f_name = this#on_id env f.f_name;
      f_body = this#on_block env f.f_body }

  method on_shape env (sfn, e) =
    (this#on_shape_field_name env sfn, list_to_single this#on_expr env e)

  method  on_shape_field_name env = function
    | SFlit pstr -> SFlit (this#on_pstring env pstr)
    | SFclass_const (i, pstr) ->
       SFclass_const (this#on_id env i, this#on_pstring env pstr)

  method on_hint env h =
    (this#on_pos env (fst h), this#on_hint_ env (snd h))

  method on_hint_ env = function
    | Hoption h -> Hoption (this#on_hint env h)
    | Hfun (hl, bo, h) ->
       Hfun (handle_list this#on_hint env hl, bo, this#on_hint env h)
    | Htuple hl -> Htuple (handle_list this#on_hint env hl)
    | Happly (i, hl) ->
       Happly (this#on_id env i, handle_list this#on_hint env hl)
    | Hshape sfl -> Hshape (handle_list this#on_shape_field env sfl)
    | Haccess (i1, i2, il) ->
       Haccess (this#on_id env i1, this#on_id env i2,
                handle_list this#on_id env il)

  method on_shape_field env sf =
    (this#on_shape_field_name env (fst sf), this#on_hint env (snd sf))
end
