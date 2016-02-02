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
(* This module defines a visitor class on the Ast data structure.
 * To use it you must inherit the generic object and redefine the appropriate
 * methods.
 *)
(*****************************************************************************)

open Ast

(*****************************************************************************)
(* The signature of the visitor. *)
(*****************************************************************************)

class type ['a] ast_visitor_type = object
  method on_afield: 'a -> afield -> 'a
  method on_array : 'a -> afield list -> 'a
  method on_array_get : 'a -> expr -> expr option -> 'a
  method on_as_expr : 'a -> as_expr -> 'a
  method on_await : 'a -> expr -> 'a
  method on_binop : 'a -> bop -> expr -> expr -> 'a
  method on_block : 'a -> block -> 'a
  method on_break : 'a -> Pos.t -> 'a
  method on_call : 'a -> expr -> expr list -> expr list -> 'a
  method on_case : 'a -> case -> 'a
  method on_cast : 'a -> hint -> expr -> 'a
  method on_catch : 'a -> catch -> 'a
  method on_class_const : 'a -> id -> pstring -> 'a
  method on_class_get : 'a -> id -> pstring -> 'a
  method on_clone : 'a -> expr -> 'a
  method on_collection: 'a -> id -> afield list -> 'a
  method on_continue : 'a -> Pos.t -> 'a
  method on_do : 'a -> block -> expr -> 'a
  method on_efun : 'a -> fun_ -> (id * bool) list -> 'a
  method on_eif : 'a -> expr -> expr option -> expr -> 'a
  method on_nullCoalesce : 'a -> expr -> expr -> 'a
  method on_expr : 'a -> expr -> 'a
  method on_expr_ : 'a -> expr_ -> 'a
  method on_expr_list : 'a -> expr list -> 'a
  method on_fallthrough : 'a -> 'a
  method on_false : 'a -> 'a
  method on_field: 'a -> field -> 'a
  method on_float : 'a -> pstring -> 'a
  method on_for : 'a -> expr -> expr -> expr -> block -> 'a
  method on_foreach : 'a -> expr -> Pos.t option -> as_expr -> block -> 'a
  method on_hint: 'a -> hint -> 'a
  method on_id : 'a -> id -> 'a
  method on_if : 'a -> expr -> block -> block -> 'a
  method on_import: 'a -> import_flavor -> expr -> 'a
  method on_import_flavor: 'a -> import_flavor -> 'a
  method on_include: 'a -> 'a
  method on_includeOnce: 'a -> 'a
  method on_instanceOf : 'a -> expr -> expr -> 'a
  method on_int : 'a -> pstring -> 'a
  method on_lfun: 'a -> fun_ -> 'a
  method on_list : 'a -> expr list -> 'a
  method on_lvar : 'a -> id -> 'a
  method on_new : 'a -> expr -> expr list -> expr list -> 'a
  method on_noop : 'a -> 'a
  method on_null : 'a -> 'a
  method on_obj_get : 'a -> expr -> expr -> 'a
  method on_pstring : 'a -> pstring -> 'a
  method on_require: 'a -> 'a
  method on_requireOnce: 'a -> 'a
  method on_return : 'a -> Pos.t -> expr option -> 'a
  method on_sfclass_const: 'a -> id -> pstring -> 'a
  method on_sflit: 'a -> pstring -> 'a
  method on_shape : 'a -> (shape_field_name * expr) list -> 'a
  method on_shape_field_name: 'a -> shape_field_name -> 'a
  method on_static_var : 'a -> expr list -> 'a
  method on_stmt : 'a -> stmt -> 'a
  method on_string2 : 'a -> expr list -> 'a
  method on_string : 'a -> pstring -> 'a
  method on_switch : 'a -> expr -> case list -> 'a
  method on_throw : 'a -> expr -> 'a
  method on_true : 'a -> 'a
  method on_try : 'a -> block -> catch list -> block -> 'a
  method on_unop : 'a -> uop -> expr -> 'a
  method on_unsafe: 'a -> 'a
  method on_while : 'a -> expr -> block -> 'a
  method on_xml : 'a -> id -> (pstring * expr) list -> expr list -> 'a
  method on_yield : 'a -> afield -> 'a
  method on_yield_break : 'a -> 'a


  (* traversal for top-level parts of the AST *)
  (* may not be exactly what you want for all implementations*)
  method on_absConst: 'a -> hint option -> id -> 'a
  method on_attributes: 'a -> class_attr list -> 'a
  method on_class_: 'a -> class_ -> 'a
  method on_class_elt: 'a -> class_elt -> 'a
  method on_classTraitRequire: 'a -> trait_req_kind -> hint -> 'a
  method on_classUse: 'a -> hint -> 'a
  method on_classVars: 'a-> kind list -> hint option -> class_var list -> 'a
  method on_const: 'a -> hint option -> (id * expr) list -> 'a
  method on_constant: 'a -> gconst -> 'a
  method on_def: 'a -> def -> 'a
  method on_fun_: 'a -> fun_ -> 'a
  method on_fun_param: 'a -> fun_param -> 'a
  method on_gconst: 'a -> gconst -> 'a
  method on_method_: 'a -> method_ -> 'a
  method on_namespace: 'a -> id -> program -> 'a
  method on_namespaceUse: 'a -> (Ast.ns_kind * id * id) list -> 'a
  method on_program: 'a -> program -> 'a
  method on_tparam: 'a -> tparam -> 'a
  method on_typeConst: 'a -> typeconst -> 'a
  method on_typedef: 'a -> typedef -> 'a
  method on_user_attribute: 'a -> user_attribute -> 'a
  method on_xhpAttr: 'a-> kind list -> hint option -> class_var list -> bool ->
                     ((Pos.t * expr list) option) -> 'a
  method on_xhpAttrUse: 'a -> hint -> 'a
  method on_xhpCategory: 'a -> pstring list -> 'a

end

(*****************************************************************************)
(* The generic visitor ('a is the type of the accumulator). *)
(*****************************************************************************)

class virtual ['a] ast_visitor: ['a] ast_visitor_type = object(this)

  method on_break acc _ = acc
  method on_continue acc _ = acc
  method on_noop acc = acc
  method on_fallthrough acc = acc
  method on_unsafe acc = acc
  method on_include acc = acc
  method on_require acc = acc
  method on_includeOnce acc = acc
  method on_requireOnce acc = acc

  method on_hint acc _ = acc

  method on_throw acc e =
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

  method on_foreach acc  e _ ae b =
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
   | As_v e ->
       let acc = this#on_expr acc e in
       acc
   | As_kv (e1, e2) ->
       let acc = this#on_expr acc e1 in
       let acc = this#on_expr acc e2 in
       acc

  method on_catch acc (i1, i2, b) =
    let acc = this#on_id acc i1 in
    let acc = this#on_id acc i2 in
    let acc = this#on_block acc b in
    acc

  method on_stmt acc = function
    | Unsafe                  -> this#on_unsafe acc
    | Expr e                  -> this#on_expr acc e
    | Break p                 -> this#on_break acc p
    | Block b                 -> this#on_block acc b
    | Continue p              -> this#on_continue acc p
    | Throw   (e)             -> this#on_throw acc e
    | Return  (p, eopt)       -> this#on_return acc p eopt
    | If      (e, b1, b2)     -> this#on_if acc e b1 b2
    | Do      (b, e)          -> this#on_do acc b e
    | While   (e, b)          -> this#on_while acc e b
    | For     (e1, e2, e3, b) -> this#on_for acc e1 e2 e3 b
    | Switch  (e, cl)         -> this#on_switch acc e cl
    | Foreach (e, popt, ae, b)-> this#on_foreach acc e popt ae b
    | Try     (b, cl, fb)     -> this#on_try acc b cl fb
    | Noop                    -> this#on_noop acc
    | Fallthrough             -> this#on_fallthrough acc
    | Static_var el           -> this#on_static_var acc el

  method on_expr acc (_, e) =
    this#on_expr_ acc e

  method on_expr_ acc e =
    match e with
   | Unsafeexpr e-> this#on_expr acc e
   | Collection (i, afl) -> this#on_collection acc i afl
   | Lfun f          -> this#on_lfun acc f
   | Import (ifv, e) -> this#on_import acc ifv e
   | Array afl   -> this#on_array acc afl
   | Shape sh    -> this#on_shape acc sh
   | True        -> this#on_true acc
   | False       -> this#on_false acc
   | Int n       -> this#on_int acc n
   | Float n     -> this#on_float acc n
   | Null        -> this#on_null acc
   | String s    -> this#on_string acc s
   | Id id       -> this#on_id acc id
   | Lvar id     -> this#on_lvar acc id
   | Yield_break -> this#on_yield_break acc
   | Yield e     -> this#on_yield acc e
   | Await e     -> this#on_await acc e
   | List el     -> this#on_list acc el
   | Clone e     -> this#on_clone acc e
   | Expr_list el    -> this#on_expr_list acc el
   | Obj_get     (e1, e2, _) -> this#on_obj_get acc e1 e2
   | Array_get   (e1, e2)    -> this#on_array_get acc e1 e2
   | Class_get   (id, pstr)   -> this#on_class_get acc id pstr
   | Class_const (id, pstr)   -> this#on_class_const acc id pstr
   | Call        (e, el, uel) -> this#on_call acc e el uel
   | String2     el           -> this#on_string2 acc el
   | Cast        (hint, e)   -> this#on_cast acc hint e
   | Unop        (uop, e)         -> this#on_unop acc uop e
   | Binop       (bop, e1, e2)    -> this#on_binop acc bop e1 e2
   | Eif         (e1, e2, e3)     -> this#on_eif acc e1 e2 e3
   | NullCoalesce (e1, e2)     -> this#on_nullCoalesce acc e1 e2
   | InstanceOf  (e1, e2)         -> this#on_instanceOf acc e1 e2
   | New         (e, el, uel)   -> this#on_new acc e el uel
   | Efun        (f, idl)         -> this#on_efun acc f idl
   | Xml         (id, attrl, el) -> this#on_xml acc id attrl el

  method on_array acc afl =
    List.fold_left this#on_afield acc afl

  method on_shape acc sfnel =
    List.fold_left begin fun acc (sfn, e) ->
      let acc = this#on_shape_field_name acc sfn in
      let acc = this#on_expr acc e in
      acc
    end  acc sfnel

  method on_id acc _ = acc
  method on_lvar acc _ = acc

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

  method on_class_get acc id pstr =
    let acc = this#on_id acc id in
    let acc = this#on_pstring acc pstr in
    acc

  method on_class_const acc id pstr =
    let acc = this#on_id acc id in
    let acc = this#on_pstring acc pstr in
    acc

  method on_call acc e el uel =
    let acc = this#on_expr acc e in
    let acc = List.fold_left this#on_expr acc el in
    let acc = List.fold_left this#on_expr acc uel in
    acc

  method on_true acc = acc
  method on_false acc = acc
  method on_int acc pstr =
    let acc = this#on_pstring acc pstr in
    acc

  method on_float acc pstr =
    let acc = this#on_pstring acc pstr in
    acc

  method on_null acc = acc
  method on_string acc pstr =
    let acc = this#on_pstring acc pstr in
    acc


  method on_string2 acc el =
    let acc = List.fold_left this#on_expr acc el in
    acc

  method on_yield_break acc = acc
  method on_yield acc e = this#on_afield acc e
  method on_await acc e = this#on_expr acc e
  method on_list acc el = List.fold_left this#on_expr acc el

  method on_expr_list acc el =
    let acc = List.fold_left this#on_expr acc el in
    acc

  method on_cast acc h e =
    let acc = this#on_expr acc e in
    let acc = this#on_hint acc h in
    acc

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

  method on_nullCoalesce acc e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_instanceOf acc e1 e2 =
    let acc = this#on_expr acc e1 in
    let acc = this#on_expr acc e2 in
    acc

  method on_new acc e el uel =
    let acc = this#on_expr acc e in
    let acc = List.fold_left this#on_expr acc el in
    let acc = List.fold_left this#on_expr acc uel in
    acc

  method on_efun acc f _ = this#on_fun_ acc f

  method on_xml acc pstr attrl el =
    let acc = this#on_pstring acc pstr in
    let acc = List.fold_left begin fun acc (_, e) ->
      this#on_expr acc e
    end acc attrl in
    let acc = List.fold_left this#on_expr acc el in
    acc

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

  method on_shape_field_name acc = function
    | SFlit pstr -> this#on_sflit acc pstr
    | SFclass_const (id, pstr) -> this#on_sfclass_const acc id pstr

  method on_sflit acc pstr = this#on_pstring acc pstr
  method on_sfclass_const acc id c = this#on_class_const acc id c

  method on_collection acc i afl =
    let acc = this#on_id acc i in
    let acc = List.fold_left this#on_afield acc afl in
    acc

  method on_import acc ifv e =
    let acc = this#on_import_flavor acc ifv in
    let acc = this#on_expr acc e in
    acc

  method on_import_flavor acc = function
    | Include     -> this#on_include acc
    | Require     -> this#on_require acc
    | IncludeOnce -> this#on_includeOnce acc
    | RequireOnce -> this#on_requireOnce acc

  method on_lfun acc l = this#on_fun_ acc l



  method on_fun_ acc f =
    let acc = this#on_id acc f.f_name in
    let acc = List.fold_left this#on_tparam acc f.f_tparams in
    let acc = List.fold_left this#on_fun_param acc f.f_params in
    let acc = this#on_block acc f.f_body in
    let acc = match f.f_ret with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc

  method on_program acc p =
    let acc = List.fold_left begin fun acc d ->
      this#on_def acc d end acc p in
    acc

  method on_def acc = function
    | Fun f -> this#on_fun_ acc f
    | Class c -> this#on_class_ acc c
    | Stmt s -> this#on_stmt acc s
    | Typedef t -> this#on_typedef acc t
    | Constant g -> this#on_constant acc g
    | Namespace (i, p) -> this#on_namespace acc i p
    | NamespaceUse idl -> this#on_namespaceUse acc idl


  method on_class_ acc c =
    let acc = this#on_id acc c.c_name in
    let acc = List.fold_left this#on_tparam acc c.c_tparams in
    let acc = List.fold_left this#on_hint acc c.c_extends in
    let acc = List.fold_left this#on_hint acc c.c_implements in
    let acc = List.fold_left this#on_class_elt acc c.c_body in
    acc

  method on_typedef acc t =
    let acc = this#on_id acc t.t_id in
    let acc = match t.t_kind with
      | Alias h | NewType h -> this#on_hint acc h in
    let acc = List.fold_left this#on_tparam acc t.t_tparams in
    let acc = List.fold_left this#on_user_attribute acc t.t_user_attributes in
    acc

  method on_constant acc g =
    this#on_gconst acc g

  method on_namespace acc i p =
    let acc = this#on_id acc i in
    let acc = this#on_program acc p in
    acc

  method on_namespaceUse acc il =
    List.fold_left begin fun acc (_, i1, i2) ->
      let acc = this#on_id acc i1 in
      let acc = this#on_id acc i2 in
      acc end acc il

  method on_tparam acc t =
    let (_, i, c_h_opt) = t in
    let acc = this#on_id acc i in
    let acc = match c_h_opt with
      | Some (_, h) -> this#on_hint acc h
      | None -> acc in
    acc

  method on_fun_param acc f =
    let acc = this#on_id acc f.param_id in
    let acc = match f.param_expr with
      | None -> acc
      | Some expr -> this#on_expr acc expr in
    let acc = match f.param_hint with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc

  method on_user_attribute acc u =
    let acc = this#on_id acc u.ua_name in
    let acc = List.fold_left this#on_expr acc u.ua_params in
    acc

  method on_gconst acc g =
    let acc = this#on_id acc g.cst_name in
    let acc = this#on_expr acc g.cst_value in
    let acc = match g.cst_type with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc

  method on_class_elt acc = function
    | Const (hopt, iel) -> this#on_const acc hopt iel
    | AbsConst (h, a) -> this#on_absConst acc h a
    | Attributes cl -> this#on_attributes acc cl
    | TypeConst t -> this#on_typeConst acc t
    | ClassUse h -> this#on_classUse acc h
    | XhpAttrUse h -> this#on_xhpAttrUse acc h
    | XhpCategory cs -> this#on_xhpCategory acc cs
    | ClassTraitRequire (t, h) -> this#on_classTraitRequire acc t h
    | ClassVars (c,v,l) -> this#on_classVars acc c v l
    | XhpAttr (t,h,i,n,g) -> this#on_xhpAttr acc t h i n g
    | Method m -> this#on_method_ acc m

  method on_const acc h_opt _ =
    let acc = match h_opt with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc
  method on_absConst acc h_opt _ =
    let acc = match h_opt with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc
  method on_attributes acc _ = acc
  method on_typeConst acc t =
    let acc = this#on_id acc t.tconst_name in
    let acc = match t.tconst_constraint with
      | Some h -> this#on_hint acc h
      | None -> acc in
    let acc = match t.tconst_type with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc
  method on_classUse acc h =
    let acc = this#on_hint acc h in
    acc
  method on_xhpAttrUse acc h =
    let acc = this#on_hint acc h in
    acc
  method on_classTraitRequire acc _ h =
    let acc = this#on_hint acc h in
    acc
  method on_classVars acc _ h_opt _ =
    let acc = match h_opt with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc
  method on_xhpAttr acc _ h_opt _ _ _ =
    let acc = match h_opt with
      | Some h -> this#on_hint acc h
      | None -> acc in
    acc
  method on_xhpCategory acc cs =
    let acc = List.fold_left this#on_pstring acc cs in
    acc

  method on_method_ acc m =
    let acc = this#on_id acc m.m_name in
    let acc = List.fold_left this#on_tparam acc m.m_tparams in
    let acc = List.fold_left this#on_fun_param acc m.m_params in
    let acc = List.fold_left this#on_user_attribute acc m.m_user_attributes in
    let acc = match m.m_ret with
      | Some h -> this#on_hint acc h
      | None -> acc in
    let acc = this#on_block acc m.m_body in
    acc

  method on_pstring acc _ = acc

end
