(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Ty_symbol
include Ty_ancestors

type t =
  | TVar of tvar * t list option
  | Bound of symbol
  | Generic of symbol * bool (* structural *) * t list option
  | Any | AnyObj | AnyFun
  | Top | Bot
  | Void | Null
  | Num | Str | Bool
  | NumLit of string
  | StrLit of string
  | BoolLit of bool
  | Fun of fun_t
  | Obj of obj_t
  | Arr of arr_t
  | Tup of t list
  | Union of t * t * t list
  | Inter of t * t * t list
  | TypeAlias of type_alias
  | TypeOf of symbol
  | ClassDecl of symbol * type_param list option
  | InterfaceDecl of symbol * type_param list option
  | ClassUtil of t
  | Exists
  | Module of symbol
  | Mu of int * t

and tvar = RVar of int [@@unboxed]            (* Recursive variable *)

and fun_t = {
  fun_params: (string option * t * fun_param) list;
  fun_rest_param: (string option * t) option;
  fun_return: t;
  fun_type_params: type_param list option;
}

and obj_t = {
  obj_exact: bool;
  obj_frozen: bool;
  obj_props: prop list;
}

and arr_t = {
  arr_readonly: bool;
  arr_elt_t: t;
}

and type_alias = {
  ta_name: symbol;
  ta_tparams: type_param list option;
  ta_type: t option
}

and fun_param = {
  prm_optional: bool;
}

and prop =
  | NamedProp of string * named_prop
  | IndexProp of dict
  | CallProp of fun_t
  | SpreadProp of t

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
  dict_name: string option;
  dict_key: t;
  dict_value: t;
}

and type_param = {
  tp_name: string;
  tp_bound: t option;
  tp_polarity: polarity;
  tp_default: t option;
}

and opt = bool

and polarity = Positive | Negative | Neutral
[@@deriving visitors {
  name="iter_ty";
  nude=true;
  variety = "iter";
  visit_prefix="on_";
  ancestors=["iter_ty_base"];
}, visitors {
  name="reduce_ty";
  variety = "reduce";
  nude = true;
  visit_prefix = "on_";
  ancestors = ["reduce_ty_base"];
}, visitors {
  name="map_ty";
  variety = "map";
  nude = true;
  visit_prefix = "on_";
  ancestors = ["map_ty_base"];
}, visitors {
  name="endo_ty";
  variety = "endo";
  nude = true;
  visit_prefix = "on_";
  ancestors = ["endo_ty_base"];
}]


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

let mk_field_props prop_list =
  List.map (fun (id, t, opt) -> NamedProp (id,
    Field (t, { fld_polarity = Neutral; fld_optional = opt })
  )) prop_list

let mk_object ?(obj_exact=false) ?(obj_frozen=false) obj_props =
  Obj { obj_exact; obj_frozen; obj_props }

let named_t symbol =
  Generic (symbol, false, None)

let builtin_t name =
  named_t (builtin_symbol name)

let generic_t ?(structural=false) symbol targs =
  Generic (symbol, structural, targs)

let generic_builtin_t name targs =
  generic_t (builtin_symbol name) (Some targs)

let rec mk_exact ty =
  match ty with
  | Obj o -> Obj { o with obj_exact=true }
  | TypeAlias a ->
    let ta_type = Option.map ~f:mk_exact a.ta_type in
    TypeAlias { a with ta_type }
  | Mu (i, t) -> Mu (i, mk_exact t)
  (* Do not nest $Exact *)
  | Generic ({ provenance = Builtin; name = "$Exact"; _ }, _, Some [_]) -> ty
  (* Not applicable *)
  | Any | AnyObj | AnyFun | Top | Bot | Void | Null
  | Num | Str | Bool | NumLit _ | StrLit _ | BoolLit _
  | Fun _ | Arr _ | Tup _ -> ty
  (* Wrap in $Exact<...> *)
  | Generic _ | TVar _ | Bound _ | Union _ | Inter _
  | TypeOf _ | ClassDecl _ | InterfaceDecl _ | ClassUtil _
  | Exists | Module _ ->
    generic_builtin_t "$Exact" [ty]

let named_alias ?ta_tparams ?ta_type name =
  TypeAlias { ta_name=name; ta_tparams; ta_type }

let string_of_provenance_ctor = function
  | Local -> "Local"
  | Imported -> "Imported"
  | Remote -> "Remote"
  | Library -> "Library"
  | Builtin -> "Builtin"

let string_of_symbol { provenance; loc; name; _ } =
  Utils_js.spf "%s (%s:%s)" name (string_of_provenance_ctor provenance)
    (Reason.string_of_loc loc)
