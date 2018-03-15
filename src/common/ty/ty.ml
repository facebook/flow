(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type identifier = string

type t =
  | Generic of identifier * bool (* structural *) * t list option
  | TVar of tvar
  | Any | AnyObj | AnyFun
  | Top | Bot
  | Void | Null
  | Num | Str | Bool
  | NumLit of string
  | StrLit of string
  | BoolLit of bool
  | Fun of fun_t
  | Obj of obj_t
  | Arr of t
  | Tup of t list
  | Union of t * t * t list
  | Inter of t * t * t list
  | TypeAlias of type_alias
  | TypeOf of identifier
  | Class of identifier * bool (* structural *) * type_param list option
  | This
  | Exists
  | Mu of int * t

and tvar =
  | RVar of int             (* Recursive variable *)
  | TParam of identifier    (* Type parameter *)

and fun_t = {
  fun_params: (identifier option * t * fun_param) list;
  fun_rest_param: (identifier option * t) option;
  fun_return: t;
  fun_type_params: type_param list option;
}

and obj_t = {
  obj_exact: bool;
  obj_props: prop list;
}

and type_alias = {
  ta_name: identifier;
  ta_imported: identifier option;
  ta_tparams: type_param list option;
  ta_type: t option
}

and fun_param = {
  prm_optional: bool;
}

and prop =
  | NamedProp of identifier * named_prop
  | IndexProp of dict
  | CallProp of fun_t

and named_prop =
  | Field of t * field
  | Method of fun_t
  | Get of t
  | Set of t

and field = {
  fld_polarity: polarity;
  fld_optional: opt;
}

and dict = {
  dict_polarity: polarity;
  dict_name: identifier option;
  dict_key: t;
  dict_value: t;
}

and type_param = {
  tp_name: identifier;
  tp_bound: t option;
  tp_polarity: polarity;
  tp_default: t option;
}

and opt = bool

and polarity = Positive | Negative | Neutral


(* Type descructors *)

let rec bk_union = function
  | Union (t1,t2,ts) -> Core_list.concat_map ~f:bk_union (t1::t2::ts)
  | t -> [t]

let rec bk_inter = function
  | Inter (t1,t2,ts) -> Core_list.concat_map ~f:bk_inter (t1::t2::ts)
  | t -> [t]


(* Type constructors *)

let mk_union ts =
  let ts = List.concat (List.map bk_union ts) in
  match ts with
  | [] -> Bot
  | [t] -> t
  | t1::t2::ts -> Union (t1, t2, ts)

let mk_inter ts =
  let ts = List.concat (List.map bk_inter ts) in
  match ts with
  | [] -> Top
  | [t] -> t
  | t1::t2::ts -> Inter (t1, t2, ts)

let mk_maybe t =
  mk_union [Null; Void; t]
