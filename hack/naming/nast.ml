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

module SN = Naming_special_names

type id = Pos.t * Ident.t
type sid = Pos.t * string
type pstring = Pos.t * string

type is_terminal = bool

type call_type =
  | Cnormal    (* when the call looks like f() *)
  | Cuser_func (* when the call looks like call_user_func(...) *)

type shape_field_name =
  | SFlit of pstring
  | SFclass_const of sid * pstring

module ShapeField = struct
  type t = shape_field_name
  (* We include span information in shape_field_name to improve error
   * messages, but we don't want it being used in the comparison, so
   * we have to write our own compare. *)
  let compare x y =
    match x, y with
      | SFlit _, SFclass_const _ -> -1
      | SFclass_const _, SFlit _ -> 1
      | SFlit (_, s1), SFlit (_, s2) -> Pervasives.compare s1 s2
      | SFclass_const ((_, s1), (_, s1')), SFclass_const ((_, s2), (_, s2')) ->
        Pervasives.compare (s1, s1') (s2, s2')

end

module ShapeMap = MyMap(ShapeField)

type hint = Pos.t * hint_
and hint_ =
  | Hany
  | Hmixed
  | Htuple of hint list
  | Habstr of string * (Ast.constraint_kind * hint) option
  | Harray of hint option * hint option
  | Hprim of tprim
  | Hoption of hint
  | Hfun of hint list * bool * hint
  | Happly of sid * hint list
  | Hshape of hint ShapeMap.t
  | Hthis

 (* This represents the use of a type const. Type consts are accessed like
  * regular consts in Hack, i.e.
  *
  * [self | static | Class]::TypeConst
  *
  * Class  => Happly "Class"
  * self   => Happly of the class of definition
  * static => Habstr ("static",
  *           Habstr ("this", (Constraint_as, Happly of class of definition)))
  * Type const access can be chained such as
  *
  * Class::TC1::TC2::TC3
  *
  * We resolve the root of the type access chain as a type as follows.
  *
  * This will result in the following representation
  *
  * Haccess (Happly "Class", ["TC1", "TC2", "TC3"])
  *)
  | Haccess of hint * sid list

and tprim =
  | Tvoid
  | Tint
  | Tbool
  | Tfloat
  | Tstring
  | Tclassname of string (* C::class *)
  | Tresource
  | Tnum
  | Tarraykey
  | Tnoreturn

and class_ = {
  c_mode           : FileInfo.mode    ;
  c_final          : bool             ;
  c_is_xhp         : bool;
  c_kind           : Ast.class_kind   ;
  c_name           : sid              ;
  (* The type parameters of a class A<T> (T is the parameter) *)
  c_tparams :
    tparam list *
    (* keeping around the ast version of the constraint only
     * for the purposes of Naming.class_meth_bodies *)
    ((Ast.constraint_kind * Ast.hint) option SMap.t);
  c_extends        : hint list        ;
  c_uses           : hint list        ;
  c_xhp_attr_uses  : hint list        ;
  c_req_extends    : hint list        ;
  c_req_implements : hint list        ;
  c_implements     : hint list        ;
  c_consts         : class_const list ;
  c_typeconsts     : class_typeconst list   ;
  c_static_vars    : class_var list   ;
  c_vars           : class_var list   ;
  c_constructor    : method_ option   ;
  c_static_methods : method_ list     ;
  c_methods        : method_ list     ;
  c_user_attributes : user_attribute list;
  c_enum           : enum_ option     ;
}

and enum_ = {
  e_base       : hint;
  e_constraint : hint option;
}

and user_attribute = {
  ua_name: sid;
  ua_params: expr list (* user attributes are restricted to scalar values *)
}

and tparam = Ast.variance * sid * (Ast.constraint_kind * hint) option

(* expr = None indicates an abstract const *)
and class_const = hint option * sid * expr option

(* This represents a type const definition. If a type const is abstract then
 * then the type hint acts as a constraint. Any concrete definition of the
 * type const must satisfy the constraint.
 *
 * If the type const is not abstract then a type must be specified.
 *)
and class_typeconst = {
  c_tconst_name : sid;
  c_tconst_constraint : hint option;
  c_tconst_type : hint option;
}

and class_var = {
  cv_final      : bool        ;
  cv_is_xhp     : bool        ;
  cv_visibility : visibility  ;
  cv_type       : hint option ;
  cv_id         : sid         ;
  cv_expr       : expr option ;
}

and method_ = {
  m_final           : bool                ;
  m_abstract        : bool                ;
  m_visibility      : visibility          ;
  m_name            : sid                 ;
  m_tparams         : tparam list         ;
  m_variadic        : fun_variadicity     ;
  m_params          : fun_param list      ;
  m_body            : func_body           ;
  m_fun_kind        : Ast.fun_kind;
  m_user_attributes : user_attribute list ;
  m_ret             : hint option         ;
}

and visibility =
  | Private
  | Public
  | Protected

and og_null_flavor =
  | OG_nullthrows
  | OG_nullsafe

and is_reference = bool
and is_variadic = bool
and fun_param = {
  param_hint : hint option;
  param_is_reference : is_reference;
  param_is_variadic : is_variadic;
  param_id : id;
  param_name : string;
  param_expr : expr option;
}

and fun_variadicity = (* does function take varying number of args? *)
  | FVvariadicArg of fun_param (* PHP5.6 ...$args finishes the func declaration *)
  | FVellipsis    (* HH ... finishes the declaration; deprecate for ...$args? *)
  | FVnonVariadic (* standard non variadic function *)

and fun_ = {
  f_mode     : FileInfo.mode;
  f_ret      : hint option;
  f_name     : sid;
  f_tparams  : tparam list;
  f_variadic : fun_variadicity;
  f_params   : fun_param list;
  f_body     : func_body;
  f_fun_kind : Ast.fun_kind;
  f_user_attributes : user_attribute list;
}

and typedef = {
  t_tparams : tparam list;
  t_constraint : hint option;
  t_kind : hint;
  t_user_attributes : user_attribute list;
}

and gconst = {
  cst_mode: FileInfo.mode;
  cst_name: Ast.id;
  cst_type: hint option;
  cst_value: expr option;
}

and func_body =
  | UnnamedBody of func_unnamed_body
  | NamedBody of func_named_body

and func_unnamed_body = {
  (* Unnamed AST for the function body *)
  fub_ast       : Ast.block;
  (* Unnamed AST for the function type params *)
  fub_tparams   : Ast.tparam list;
  (* Namespace info *)
  fub_namespace : Namespace_env.env;
}

and func_named_body = {
  (* Named AST for the function body *)
  fnb_nast     : block;
  (* True if there are any UNSAFE blocks; the presence of any unsafe
   * block in the function makes comparing the function body to the
   * declared return type impossible, since that block could return;
   * functions declared in Mdecl are by definition UNSAFE
   *)
  fnb_unsafe   : bool;
}

and stmt =
  | Expr of expr
  | Break of Pos.t
  | Continue of Pos.t
  | Throw of is_terminal * expr
  | Return of Pos.t * expr option
  | Static_var of expr list
  | If of expr * block * block
  | Do of block * expr
  | While of expr * block
  | For of expr * expr * expr * block
  | Switch of expr * case list
  | Foreach of expr * as_expr * block
  | Try of block * catch list * block
  | Noop
  | Fallthrough

and as_expr =
  | As_v of expr
  | As_kv of expr * expr
  | Await_as_v of Pos.t * expr
  | Await_as_kv of Pos.t * expr * expr

and block = stmt list

and class_id =
  | CIparent
  | CIself
  | CIstatic
  | CIvar of expr
  | CI of sid

and expr = Pos.t * expr_
and expr_ =
  | Any
  | Array of afield list
  | Shape of expr ShapeMap.t
  | ValCollection of string * expr list
  | KeyValCollection of string * field list
  | This
  | Id of sid
  | Lvar of id
  | Lplaceholder of sid
  | Fun_id of sid
  | Method_id of expr * pstring
  (* meth_caller('Class name', 'method name') *)
  | Method_caller of sid * pstring
  | Smethod_id of sid * pstring
  | Obj_get of expr * expr * og_null_flavor
  | Array_get of expr * expr option
  | Class_get of class_id * pstring
  | Class_const of class_id * pstring
  | Call of call_type
    * expr (* function *)
    * expr list (* positional args *)
    * expr list (* unpacked args *)
  | True
  | False
  | Int of pstring
  | Float of pstring
  | Null
  | String of pstring
  | String2 of expr list * string
  | Special_func of special_func
  | Yield_break
  | Yield of afield
  | Await of expr
  | List of expr list
  | Pair of expr * expr
  | Expr_list of expr list
  | Cast of hint * expr
  | Unop of Ast.uop * expr
  | Binop of Ast.bop * expr * expr
  | Eif of expr * expr option * expr
  | InstanceOf of expr * expr
  | New of class_id * expr list * expr list
  | Efun of fun_ * id list
  | Xml of sid * (pstring * expr) list * expr list
  | Assert of assert_expr
  | Clone of expr

(* These are "very special" constructs that we look for in, among
 * other places, terminality checks. invariant does not appear here
 * because it gets rewritten to If + AE_invariant_violation.
 *
 * TODO: get rid of assert_expr entirely in favor of rewriting to if
 * and noreturn *)
and assert_expr =
  | AE_assert of expr

and case =
  | Default of block
  | Case of expr * block

and catch = sid * id * block

and field = expr * expr
and afield =
  | AFvalue of expr
  | AFkvalue of expr * expr

and special_func =
  | Gena of expr
  | Genva of expr list
  | Gen_array_rec of expr
  | Gen_array_va_rec of expr list

type def =
  | Fun of fun_
  | Class of class_
  | Typedef of typedef

type program = def list

(* Expecting that Naming.func_body / Naming.class_meth_bodies has been
 * allowed at the AST. Ideally this would be enforced by the compiler,
 * a la the typechecking decl vs local phases *)
let assert_named_body = function
  | NamedBody b -> b
  | UnnamedBody _ -> failwith "Expecting a named function body"

let class_id_to_str = function
  | CIparent -> SN.Classes.cParent
  | CIself -> SN.Classes.cSelf
  | CIstatic -> SN.Classes.cStatic
  | CIvar (_, This) -> SN.SpecialIdents.this
  | CIvar (_, Lvar (_, x)) -> "$"^string_of_int(x)
  | CIvar _ -> assert false
  | CI (_, x) -> x
