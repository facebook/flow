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
(* This module defines a visitor class on the Nast data structure.
 * To use it you must inherit the generic object and redefine the appropriate
 * methods.
 *)
(*****************************************************************************)

open Nast

(*****************************************************************************)
(* The signature of the visitor. *)
(*****************************************************************************)

class type ['a] nast_visitor_type = object
  method on_block : 'a -> Nast.block -> 'a
  method on_break : 'a -> Pos.t -> 'a
  method on_case : 'a -> Nast.case -> 'a
  method on_catch : 'a -> Nast.catch -> 'a
  method on_continue : 'a -> Pos.t -> 'a
  method on_do : 'a -> Nast.block -> Nast.expr -> 'a
  method on_expr : 'a -> Nast.expr -> 'a
  method on_expr_ : 'a -> Nast.expr_ -> 'a
  method on_for :
      'a -> Nast.expr -> Nast.expr -> Nast.expr -> Nast.block -> 'a
  method on_foreach :
      'a -> Nast.expr -> Nast.as_expr -> Nast.block -> 'a
  method on_if : 'a -> Nast.expr -> Nast.block -> Nast.block -> 'a
  method on_noop : 'a -> 'a
  method on_fallthrough : 'a -> 'a
  method on_return : 'a -> Pos.t -> Nast.expr option -> 'a
  method on_static_var : 'a -> Nast.expr list -> 'a
  method on_stmt : 'a -> Nast.stmt -> 'a
  method on_switch : 'a -> Nast.expr -> Nast.case list -> 'a
  method on_throw : 'a -> Nast.is_terminal -> Nast.expr -> 'a
  method on_try : 'a -> Nast.block -> Nast.catch list -> Nast.block -> 'a
  method on_while : 'a -> Nast.expr -> Nast.block -> 'a
  method on_as_expr : 'a -> as_expr -> 'a
  method on_array : 'a -> afield list -> 'a
  method on_shape : 'a -> expr ShapeMap.t -> 'a
  method on_valCollection : 'a -> string -> expr list -> 'a
  method on_keyValCollection : 'a -> string -> field list -> 'a
  method on_this : 'a -> 'a
  method on_id : 'a -> sid -> 'a
  method on_lvar : 'a -> id -> 'a
  method on_fun_id : 'a -> sid -> 'a
  method on_method_id : 'a -> expr -> pstring -> 'a
  method on_smethod_id : 'a -> sid -> pstring -> 'a
  method on_method_caller : 'a -> sid -> pstring -> 'a
  method on_obj_get : 'a -> expr -> expr -> 'a
  method on_array_get : 'a -> expr -> expr option -> 'a
  method on_class_get : 'a -> class_id -> pstring -> 'a
  method on_class_const : 'a -> class_id -> pstring -> 'a
  method on_call : 'a -> call_type -> expr -> expr list -> expr list -> 'a
  method on_true : 'a -> 'a
  method on_false : 'a -> 'a
  method on_int : 'a -> pstring -> 'a
  method on_float : 'a -> pstring -> 'a
  method on_null : 'a -> 'a
  method on_string : 'a -> pstring -> 'a
  method on_string2 : 'a -> expr list -> string -> 'a
  method on_special_func : 'a -> special_func -> 'a
  method on_yield_break : 'a -> 'a
  method on_yield : 'a -> afield -> 'a
  method on_await : 'a -> expr -> 'a
  method on_list : 'a -> expr list -> 'a
  method on_pair : 'a -> expr -> expr -> 'a
  method on_expr_list : 'a -> expr list -> 'a
  method on_cast : 'a -> hint -> expr -> 'a
  method on_unop : 'a -> Ast.uop -> expr -> 'a
  method on_binop : 'a -> Ast.bop -> expr -> expr -> 'a
  method on_eif : 'a -> expr -> expr option -> expr -> 'a
  method on_instanceOf : 'a -> expr -> expr -> 'a
  method on_new : 'a -> class_id -> expr list -> expr list -> 'a
  method on_efun : 'a -> fun_ -> id list -> 'a
  method on_xml : 'a -> sid -> (pstring * expr) list -> expr list -> 'a
  method on_assert : 'a -> assert_expr -> 'a
  method on_clone : 'a -> expr -> 'a
  method on_field: 'a -> field -> 'a
  method on_afield: 'a -> afield -> 'a

end

(*****************************************************************************)
(* The generic visitor ('a is the type of the accumulator). *)
(*****************************************************************************)

class virtual ['a] nast_visitor: ['a] nast_visitor_type = object(this)

  method on_break acc _ = acc
  method on_continue acc _ = acc
  method on_noop acc = acc
  method on_fallthrough acc = acc

  method on_throw acc _ e =
    let acc = this#on_expr acc e in
    acc

  method on_return acc _ eopt =
    match eopt with
    | None -> acc
    | Some e -> this#on_expr acc e

  method on_static_var acc el = List.fold_left this#on_expr acc el

  method on_if acc e b1 b2 =
    let acc = this#on_expr acc e in
    let acc = this#on_block acc b1 in
    let acc = this#on_block acc b2 in
    acc

  method on_do acc b e =
    let acc = this#on_block acc b in
    let acc = this#on_expr acc e in
    acc

  method on_while acc e b =
    let acc = this#on_expr acc e in
    let acc = this#on_block acc b in
    acc

  method on_for acc e1 e2 e3 b =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    let acc = this#on_expr acc e3 in
    let acc = this#on_block acc b in
    acc

  method on_switch acc e cl =
    let acc = this#on_expr acc e in
    let acc = List.fold_left this#on_case acc cl in
    acc

  method on_foreach acc e ae b =
    let acc = this#on_expr acc e in
    let acc = this#on_as_expr acc ae in
    let acc = this#on_block acc b in
    acc

  method on_try acc b cl fb =
    let acc = this#on_block acc b in
    let acc = List.fold_left this#on_catch acc cl in
    let acc = this#on_block acc fb in
    acc

  method on_block acc b =
    List.fold_left this#on_stmt acc b

  method on_case acc = function
    | Default b ->
        let acc = this#on_block acc b in
        acc
    | Case (e, b) ->
        let acc = this#on_expr acc e in
        let acc = this#on_block acc b in
        acc

  method on_as_expr acc = function
   | As_v e
   | Await_as_v (_, e) ->
       let acc = this#on_expr acc e in
       acc
   | As_kv (e1, e2)
   | Await_as_kv (_, e1, e2) ->
       let acc = this#on_expr acc e1 in
       let acc = this#on_expr acc e2 in
       acc

  method on_catch acc (_, _, b) = this#on_block acc b

  method on_stmt acc = function
    | Expr e                  -> this#on_expr acc e
    | Break p                 -> this#on_break acc p
    | Continue p              -> this#on_continue acc p
    | Throw   (is_term, e)    -> this#on_throw acc is_term e
    | Return  (p, eopt)       -> this#on_return acc p eopt
    | If      (e, b1, b2)     -> this#on_if acc e b1 b2
    | Do      (b, e)          -> this#on_do acc b e
    | While   (e, b)          -> this#on_while acc e b
    | For     (e1, e2, e3, b) -> this#on_for acc e1 e2 e3 b
    | Switch  (e, cl)         -> this#on_switch acc e cl
    | Foreach (e, ae, b)      -> this#on_foreach acc e ae b
    | Try     (b, cl, fb)     -> this#on_try acc b cl fb
    | Noop                    -> this#on_noop acc
    | Fallthrough             -> this#on_fallthrough acc
    | Static_var el           -> this#on_static_var acc el

  method on_expr acc (_, e) =
    this#on_expr_ acc e

  method on_expr_ acc e =
    match e with
   | Any         -> acc
   | Array afl   -> this#on_array acc afl
   | Shape sh    -> this#on_shape acc sh
   | True        -> this#on_true acc
   | False       -> this#on_false acc
   | Int n       -> this#on_int acc n
   | Float n     -> this#on_float acc n
   | Null        -> this#on_null acc
   | String s    -> this#on_string acc s
   | This        -> this#on_this acc
   | Id sid      -> this#on_id acc sid
   | Lplaceholder _sid -> acc
   | Lvar id     -> this#on_lvar acc id
   | Fun_id sid  -> this#on_fun_id acc sid
   | Method_id (expr, pstr) -> this#on_method_id acc expr pstr
   | Method_caller (sid, pstr) -> this#on_method_caller acc sid pstr
   | Smethod_id (sid, pstr) -> this#on_smethod_id acc sid pstr
   | Yield_break -> this#on_yield_break acc
   | Yield e     -> this#on_yield acc e
   | Await e     -> this#on_await acc e
   | List el     -> this#on_list acc el
   | Assert ae   -> this#on_assert acc ae
   | Clone e     -> this#on_clone acc e
   | Expr_list el    -> this#on_expr_list acc el
   | Special_func sf -> this#on_special_func acc sf
   | Obj_get     (e1, e2, _) -> this#on_obj_get acc e1 e2
   | Array_get   (e1, e2)    -> this#on_array_get acc e1 e2
   | Class_get   (cid, id)   -> this#on_class_get acc cid id
   | Class_const (cid, id)   -> this#on_class_const acc cid id
   | Call        (ct, e, el, uel) -> this#on_call acc ct e el uel
   | String2     (el, s)     -> this#on_string2 acc el s
   | Pair        (e1, e2)    -> this#on_pair acc e1 e2
   | Cast        (hint, e)   -> this#on_cast acc hint e
   | Unop        (uop, e)         -> this#on_unop acc uop e
   | Binop       (bop, e1, e2)    -> this#on_binop acc bop e1 e2
   | Eif         (e1, e2, e3)     -> this#on_eif acc e1 e2 e3
   | InstanceOf  (e1, e2)         -> this#on_instanceOf acc e1 e2
   | New         (cid, el, uel)   -> this#on_new acc cid el uel
   | Efun        (f, idl)         -> this#on_efun acc f idl
   | Xml         (sid, attrl, el) -> this#on_xml acc sid attrl el
   | ValCollection    (s, el)     ->
       this#on_valCollection acc s el
   | KeyValCollection (s, fl)     ->
       this#on_keyValCollection acc s fl

  method on_array acc afl =
    List.fold_left this#on_afield acc afl

  method on_shape acc sm =
    ShapeMap.fold begin fun _ e acc ->
      let acc = this#on_expr acc e in
      acc
    end sm acc

  method on_valCollection acc _ el =
    List.fold_left this#on_expr acc el

  method on_keyValCollection acc _ fieldl =
    List.fold_left this#on_field acc fieldl

  method on_this acc = acc
  method on_id acc _ = acc
  method on_lvar acc _ = acc
  method on_fun_id acc _ = acc
  method on_method_id acc _ _ = acc
  method on_smethod_id acc _ _ = acc
  method on_method_caller acc _ _ = acc

  method on_obj_get acc e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_array_get acc e e_opt =
    let acc = this#on_expr acc e in
    let acc =
      match e_opt with
      | None -> acc
      | Some e -> this#on_expr acc e
    in
    acc

  method on_class_get acc _ _ = acc
  method on_class_const acc _ _ = acc

  method on_call acc _ e el uel =
    let acc = this#on_expr acc e in
    let acc = List.fold_left this#on_expr acc el in
    let acc = List.fold_left this#on_expr acc uel in
    acc

  method on_true acc = acc
  method on_false acc = acc
  method on_int acc _ = acc
  method on_float acc _ = acc
  method on_null acc = acc
  method on_string acc _ = acc

  method on_string2 acc el _ =
    let acc = List.fold_left this#on_expr acc el in
    acc

  method on_special_func acc = function
    | Gena e
    | Gen_array_rec e -> this#on_expr acc e
    | Genva el
    | Gen_array_va_rec el -> List.fold_left this#on_expr acc el

  method on_yield_break acc = acc
  method on_yield acc e = this#on_afield acc e
  method on_await acc e = this#on_expr acc e
  method on_list acc el = List.fold_left this#on_expr acc el

  method on_pair acc e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_expr_list acc el =
    let acc = List.fold_left this#on_expr acc el in
    acc

  method on_cast acc _ e = this#on_expr acc e
  method on_unop acc _ e = this#on_expr acc e

  method on_binop acc _ e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_eif acc e1 e2 e3 =
    let acc = this#on_expr acc e1 in
    let acc =
      match e2 with
      | None -> acc
      | Some e -> this#on_expr acc e
    in
    let acc = this#on_expr acc e3 in
    acc

  method on_instanceOf acc e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_new acc _ el uel =
    let acc = List.fold_left this#on_expr acc el in
    let acc = List.fold_left this#on_expr acc uel in
    acc

  method on_efun acc f _ = match f.f_body with
    | UnnamedBody _ ->
      failwith "lambdas expected to be named in the context of the surrounding function"
    | NamedBody { fnb_nast ; _ } -> this#on_block acc fnb_nast

  method on_xml acc _ attrl el =
    let acc = List.fold_left begin fun acc (_, e) ->
      this#on_expr acc e
    end acc attrl in
    let acc = List.fold_left this#on_expr acc el in
    acc

  method on_assert acc = function
    | AE_assert e -> this#on_expr acc e

  method on_clone acc e = this#on_expr acc e

  method on_field acc (e1, e2) =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_afield acc = function
    | AFvalue e -> this#on_expr acc e
    | AFkvalue (e1, e2) ->
        let acc = this#on_expr acc e1 in
        let acc = this#on_expr acc e2 in
        acc
end

(*****************************************************************************)
(* Returns true if a block has a return statement. *)
(*****************************************************************************)

module HasReturn: sig
  val block: block -> bool
end = struct

  let visitor =
    object
      inherit [bool] nast_visitor
      method! on_expr acc _ = acc
      method! on_return _ _ _ = true
    end

  let block b = visitor#on_block false b

end

(* Used by HasBreak and HasContinue. Does not traverse nested loops, since the
 * breaks / continues in those loops do not affect the control flow of the
 * outermost loop. *)

class loop_visitor =
  object
    inherit [bool] nast_visitor
    method! on_expr acc _ = acc
    method! on_for acc _ _ _ _ = acc
    method! on_foreach acc _ _ _ = acc
    method! on_do acc _ _ = acc
    method! on_while acc _ _ = acc
    method! on_switch acc _ _ = acc
  end

(*****************************************************************************)
(* Returns true if a block has a continue statement.
 * It is necessary to properly handle the type of locals.
 * When a block statement has a continue statement, the control flow graph
 * could be interrupted. When that is the case, the types of locals has to
 * be more conservative. Locals can have different types depending on their
 * position in a block. In the presence of constructions that can interrupt
 * the control flow (exceptions, continue), the type of the local becomes:
 * "any type that the local had, regardless of its position".
 *)
(*****************************************************************************)

module HasContinue: sig
  val block: block -> bool
end = struct

  let visitor =
    object
      inherit loop_visitor
      method! on_continue _ _ = true
    end

  let block b = visitor#on_block false b

end

(*****************************************************************************)
(* Returns true if a block has a continue statement.
 * Useful for checking if a while(true) {...} loop is non-terminating.
 *)
(*****************************************************************************)

module HasBreak: sig
  val block: block -> bool
end = struct

  let visitor =
    object
      inherit loop_visitor
      method! on_break _ _ = true
    end

  let block b = visitor#on_block false b

end
