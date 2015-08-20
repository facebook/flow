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
(* Constants *)
(*****************************************************************************)

type cst_kind =
  (* The constant was introduced with: define('X', ...); *)
  | Cst_define
  (* The constant was introduced with: const X = ...; *)
  | Cst_const

(*****************************************************************************)
(* The Abstract Syntax Tree *)
(*****************************************************************************)

type id = Pos.t * string
type pstring = Pos.t * string

type variance =
  | Covariant
  | Contravariant
  | Invariant

type program = def list

and def =
  | Fun of fun_
  | Class of class_
  | Stmt of stmt
  | Typedef of typedef
  | Constant of gconst
  | Namespace of id * program
  | NamespaceUse of (id * id) list

and typedef = {
  t_id: id;
  t_tparams: tparam list;
  t_constraint: tconstraint;
  t_kind: typedef_kind;
  t_user_attributes: user_attribute list;
  t_namespace: Namespace_env.env;
  t_mode: FileInfo.mode;
}

and gconst = {
  cst_mode: FileInfo.mode;
  cst_kind: cst_kind;
  cst_name: id;
  cst_type: hint option;
  cst_value: expr;
  cst_namespace: Namespace_env.env;
}

and constraint_kind =
  | Constraint_as
  | Constraint_super

and tparam = variance * id * (constraint_kind * hint) option

and tconstraint = hint option

and typedef_kind =
  | Alias of hint
  | NewType of hint

and class_ = {
  c_mode: FileInfo.mode;
  c_user_attributes: user_attribute list;
  c_final: bool;
  c_kind: class_kind;
  c_is_xhp: bool;
  c_name: id;
  c_tparams: tparam list;
  c_extends: hint list;
  c_implements: hint list;
  c_body: class_elt list;
  c_namespace: Namespace_env.env;
  c_enum: enum_ option;
}

and enum_ = {
  e_base       : hint;
  e_constraint : hint option;
}

and user_attribute = {
  ua_name: id;
  ua_params: expr list (* user attributes are restricted to scalar values *)
}

and class_kind =
  | Cabstract
  | Cnormal
  | Cinterface
  | Ctrait
  | Cenum

and trait_req_kind =
  | MustExtend
  | MustImplement

and class_elt =
  | Const of hint option * (id * expr) list
  | AbsConst of hint option * id
  | Attributes of class_attr list
  | TypeConst of typeconst
  | ClassUse of hint
  | XhpAttrUse of hint
  | ClassTraitRequire of trait_req_kind * hint
  | ClassVars of kind list * hint option * class_var list
  | XhpAttr of kind list * hint option * class_var list * bool *
               ((Pos.t * expr list) option)
  | Method of method_

and class_attr =
  | CA_name of id
  | CA_field of ca_field

and ca_field = {
  ca_type: ca_type;
  ca_id: id;
  ca_value: expr option;
  ca_required: bool;
}

and ca_type =
  | CA_hint of hint
  | CA_enum of string list

and kind =
  | Final
  | Static
  | Abstract
  | Private
  | Public
  | Protected

and og_null_flavor =
  | OG_nullthrows
  | OG_nullsafe

(* id without $ *)
and class_var = id * expr option

and method_ = {
  m_kind: kind list ;
  m_tparams: tparam list;
  m_name: id;
  m_params: fun_param list;
  m_body: block;
  m_user_attributes : user_attribute list;
  m_ret: hint option;
  m_ret_by_ref: bool;
  m_fun_kind: fun_kind;
}

and typeconst = {
  tconst_abstract: bool;
  tconst_name: id;
  tconst_constraint: hint option;
  tconst_type: hint option;
}

and is_reference = bool
and is_variadic = bool

and fun_param = {
  param_hint : hint option;
  param_is_reference : is_reference;
  param_is_variadic : is_variadic;
  param_id : id;
  param_expr : expr option;
  (* implicit field via constructor parameter.
   * This is always None except for constructors and the modifier
   * can be only Public or Protected or Private.
   *)
  param_modifier: kind option;
  param_user_attributes: user_attribute list;
}

and fun_ = {
  f_mode            : FileInfo.mode;
  f_tparams         : tparam list;
  f_ret             : hint option;
  f_ret_by_ref      : bool;
  f_name            : id;
  f_params          : fun_param list;
  f_body            : block;
  f_user_attributes : user_attribute list;
  f_mtime           : float;
  f_fun_kind        : fun_kind;
  f_namespace       : Namespace_env.env;
}

and fun_decl_kind =
  | FDeclAsync
  | FDeclSync

and fun_kind =
  | FSync
  | FAsync
  | FGenerator
  | FAsyncGenerator

and hint = Pos.t * hint_
and hint_ =
  | Hoption of hint
  | Hfun of hint list * bool * hint
  | Htuple of hint list
  | Happly of id * hint list
  | Hshape of shape_field list
 (* This represents the use of a type const. Type consts are accessed like
  * regular consts in Hack, i.e.
  *
  * Class::TypeConst
  *
  * Type const access can be chained such as
  *
  * Class::TC1::TC2::TC3
  *
  * This will result in the following representation
  *
  * Haccess ("Class", "TC1", ["TC2", "TC3"])
  *)
  | Haccess of id * id * id list

and shape_field_name =
  | SFlit of pstring
  | SFclass_const of id * pstring

and shape_field = shape_field_name * hint

and stmt =
  | Unsafe
  | Fallthrough
  | Expr of expr
  | Block of stmt list
  | Break of Pos.t
  | Continue of Pos.t
  | Throw of expr
  | Return of Pos.t * expr option
  | Static_var of expr list
  | If of expr * block * block
  | Do of block * expr
  | While of expr * block
  | For of expr * expr * expr * block
  | Switch of expr * case list
  | Foreach of expr * Pos.t option (* await as *) * as_expr * block
  | Try of block * catch list * block
  | Noop

and as_expr =
  | As_v of expr
  | As_kv of expr * expr

and block = stmt list

and expr = Pos.t * expr_
and expr_ =
  | Array of afield list
  | Shape of (shape_field_name * expr) list
  | Collection of id * afield list
  | Null
  | True
  | False
  | Id of id
  | Lvar of id
  | Clone of expr
  | Obj_get of expr * expr * og_null_flavor
  | Array_get of expr * expr option
  | Class_get of id * pstring
  | Class_const of id * pstring
  | Call of expr * expr list * expr list
  | Int of pstring
  | Float of pstring
  | String of pstring
  | String2 of expr list * pstring
  | Yield of afield
  | Yield_break
  | Await of expr
  | List of expr list
  | Expr_list of expr list
  | Cast of hint * expr
  | Unop of uop * expr
  | Binop of bop * expr * expr
  | Eif of expr * expr option * expr
  | InstanceOf of expr * expr
  | New of expr * expr list * expr list
  (* Traditional PHP-style closure with a use list. Each use element is
    a name and a bool indicating if its a reference or value *)
  | Efun of fun_ * (id * bool) list
  (*
   * Hack-style lambda expressions (no id list, we'll find the captures
   * during name resolution).
   *)
  | Lfun of fun_
  | Xml of id * (id * expr) list * expr list
  | Unsafeexpr of expr
  | Import of import_flavor * expr
  | Ref of expr

and import_flavor =
  | Include
  | Require
  | IncludeOnce
  | RequireOnce

and afield =
  | AFvalue of expr
  | AFkvalue of expr * expr

and bop =
| Plus
| Minus | Star | Slash | Eqeq | EQeqeq | Starstar
| Diff | Diff2 | AMpamp | BArbar | Lt
| Lte | Gt | Gte | Dot | Amp | Bar | Ltlt
| Gtgt | Percent | Xor
| Eq of bop option

and uop =
| Utild
| Unot | Uplus | Uminus | Uincr
| Udecr | Upincr | Updecr

and case =
| Default of block
| Case of expr * block

and catch = id * id * block

and field = expr * expr

and attr = id * expr

 (* with tarzan *)

type any =
  | AHint of hint
  | AExpr of expr
  | AStmt of stmt
  | ADef of def
  | AProgram of program

 (* with tarzan *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let string_of_class_kind = function
  | Cabstract -> "an abstract class"
  | Cnormal -> "a class"
  | Cinterface -> "an interface"
  | Ctrait -> "a trait"
  | Cenum -> "an enum"
