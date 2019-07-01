(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include Ty_symbol
include Ty_ancestors

type aloc = ALoc.t

type t =
  | TVar of tvar * t list option
  | Bound of aloc * string
  | Generic of symbol * gen_kind * t list option
  | Any of any_kind
  | Top
  | Bot of bot_kind
  | Void | Null
  | Num of string option
  | Str of string option
  | Bool of bool option
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
  | TypeOf of path * string
  | ClassDecl of symbol * type_param list option
  | InterfaceDecl of symbol * type_param list option
  | Utility of utility
  | Module of symbol
  | Mu of int * t

and tvar = RVar of int [@@unboxed]            (* Recursive variable *)

and path = string list

and any_kind =
  | Implicit
  | Explicit

(* The purpose of adding this distinction is to enable normalized types to mimic
 * the behavior of the signature optimizer when exporting types that contain
 * tvars with no lower bounds.
 *)
and upper_bound_kind =
  (* No upper bounds are exported as `any` *)
  | NoUpper
  (* If there is some upper bound (use), this is exported as `MergedT use`. This
   * type is not helpful in a normalized form. So instead we attempt to normalize
   * the use to a type `t`. If this succeeds then we create `SomeKnownUpper t`.
   *)
  | SomeKnownUpper of t
  (* If the above case fails we resort to this last case. *)
  | SomeUnknownUpper of string

and bot_kind =
  (* Type.Empty *)
  | EmptyType
  (* Type.MatchingPropT *)
  | EmptyMatchingPropT
  (* Type.TypeDestructorTriggerT *)
  | EmptyTypeDestructorTriggerT of aloc
  (* A tvar with no lower bounds *)
  | NoLowerWithUpper of upper_bound_kind

and gen_kind =
  | ClassKind
  | InterfaceKind
  | TypeAliasKind

and fun_t = {
  fun_params: (string option * t * fun_param) list;
  fun_rest_param: (string option * t) option;
  fun_return: t;
  fun_type_params: type_param list option;
}

and obj_t = {
  obj_exact: bool;
  obj_frozen: bool;
  obj_literal: bool;
  obj_props: prop list;
}

and arr_t = {
  arr_readonly: bool;
  arr_literal: bool;
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

and utility =
  (* https://flow.org/en/docs/types/utilities/ *)
  | Keys of t
  | Values of t
  | ReadOnly of t
  | Exact of t
  | Diff of t * t
  | Rest of t * t
  | PropertyType of t * t
  | ElementType of t * t
  | NonMaybeType of t
  | Distribute of t * t
  | ObjMap of t * t
  | ObjMapi of t * t
  | TupleMap of t * t
  | Call of t * t list
  | Class of t
  | Shape of t
  | Supertype of t
  | Subtype of t
  | Exists
  (* React utils *)
  | ReactElementPropsType of t
  | ReactElementConfigType of t
  | ReactElementRefType of t
  | ReactConfigType of t * t

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
}, visitors {
  name="mapreduce_ty";
  variety = "mapreduce";
  nude = true;
  visit_prefix = "on_";
  ancestors = ["mapreduce_ty_base"];
}]

(* Type destructors *)

let rec bk_union ?(flattened=false) = function
  | Union (t1,t2,ts) when flattened -> (t1, t2::ts)
  | Union (t1,t2,ts) -> Nel.map_concat bk_union (t1, t2::ts)
  | t -> (t, [])

let rec bk_inter ?(flattened=false) = function
  | Inter (t1,t2,ts) when flattened -> (t1, t2::ts)
  | Inter (t1,t2,ts) -> Nel.map_concat bk_inter (t1, t2::ts)
  | t -> (t, [])


(* Type constructors *)

let mk_union ?(flattened=false) nel_ts =
  let (t, ts) = Nel.map_concat (bk_union ~flattened) nel_ts in
  match ts with
  | [] -> t
  | hd::tl -> Union (t, hd, tl)

let mk_inter ?(flattened=false) nel_ts =
  let (t, ts) = Nel.map_concat (bk_inter ~flattened) nel_ts in
  match ts with
  | [] -> t
  | hd::tl -> Inter (t, hd, tl)

let explicit_any = Any Explicit
let implicit_any = Any Implicit
let is_dynamic = function Any _ -> true | _ -> false
let mk_maybe t =
  mk_union (Null, [Void; t])

let mk_field_props prop_list =
  Core_list.map ~f:(fun (id, t, opt) -> NamedProp (id,
    Field (t, { fld_polarity = Neutral; fld_optional = opt })
  )) prop_list

let mk_object ?(obj_exact=false) ?(obj_frozen=false) ?(obj_literal = false) obj_props =
  Obj { obj_exact; obj_frozen; obj_literal; obj_props }

let mk_generic_class symbol targs =
  Generic (symbol, ClassKind, targs)

let mk_generic_interface symbol targs =
  Generic (symbol, InterfaceKind, targs)

let mk_generic_talias symbol targs =
  Generic (symbol, TypeAliasKind, targs)

let rec mk_exact ty =
  match ty with
  | Obj o -> Obj { o with obj_exact=true }
  | TypeAlias a ->
    let ta_type = Option.map ~f:mk_exact a.ta_type in
    TypeAlias { a with ta_type }
  | Mu (i, t) -> Mu (i, mk_exact t)
  (* Not applicable *)
  | Any _ | Top | Bot _ | Void | Null
  | Num _ | Str _ | Bool _ | NumLit _ | StrLit _ | BoolLit _
  | Fun _ | Arr _ | Tup _ -> ty
  (* Do not nest $Exact *)
  | Utility (Exact _) -> ty
  (* Wrap in $Exact<...> *)
  | Generic _ | TVar _ | Bound _ | Union _ | Inter _
  | TypeOf _ | ClassDecl _ | InterfaceDecl _
  | Utility _ | Module _ ->
    Utility (Exact ty)

let mk_array ~readonly ~literal t =
  Arr { arr_readonly = readonly; arr_literal = literal; arr_elt_t = t }

let named_alias ?ta_tparams ?ta_type name =
  TypeAlias { ta_name=name; ta_tparams; ta_type }

let debug_string_of_provenance_ctor = function
  | Local -> "Local"
  | Remote { imported_as = Some _ } -> "Imported"
  | Remote { imported_as = None } -> "Remote"
  | Library -> "Library"
  | Builtin -> "Builtin"

let debug_string_of_symbol { provenance; def_loc; name; _ } =
  Utils_js.spf "%s (%s:%s)" name (debug_string_of_provenance_ctor provenance)
    (Reason.string_of_aloc def_loc)

let debug_string_of_generic_kind = function
  | ClassKind -> "class"
  | InterfaceKind -> "interface"
  | TypeAliasKind -> "type alias"

let string_of_utility_ctor = function
  | Keys _ -> "$Keys"
  | Values _ -> "$Values"
  | ReadOnly _ -> "$ReadOnly"
  | Exact _ -> "$Exact"
  | Diff _ -> "$Diff"
  | Rest _ -> "$Rest"
  | PropertyType _ -> "$PropertyType"
  | ElementType _ -> "$ElementType"
  | NonMaybeType _ -> "$NonMaybeType"
  | Distribute _ -> "$Distribute"
  | ObjMap _ -> "$ObjMap"
  | ObjMapi _ -> "$ObjMapi"
  | TupleMap _ -> "$TupleMap"
  | Call _ -> "$Call"
  | Class _ -> "Class"
  | Shape _ -> "$Shape"
  | Supertype _ -> "$Supertype"
  | Subtype _ -> "$Subtype"
  | Exists -> "*"
  | ReactElementPropsType _ -> "React$ElementProps"
  | ReactElementConfigType _ -> "React$ElementConfig"
  | ReactElementRefType _ -> "React$ElementRef"
  | ReactConfigType _  -> "React$Config"

let types_of_utility = function
  | Keys t -> Some [t]
  | Values t -> Some [t]
  | ReadOnly t -> Some [t]
  | Exact t -> Some [t]
  | Diff (t1, t2) -> Some [t1; t2]
  | Rest (t1, t2) -> Some [t1; t2]
  | PropertyType (t1, t2) -> Some [t1; t2]
  | ElementType (t1, t2) -> Some [t1; t2]
  | NonMaybeType t -> Some [t]
  | Distribute (t1, t2) -> Some [t1; t2]
  | ObjMap (t1, t2) -> Some [t1; t2]
  | ObjMapi (t1, t2) -> Some [t1; t2]
  | TupleMap (t1, t2) -> Some [t1; t2]
  | Call (t, ts) -> Some (t::ts)
  | Class t -> Some [t]
  | Shape t -> Some [t]
  | Supertype t -> Some [t]
  | Subtype t -> Some [t]
  | Exists -> None
  | ReactElementPropsType t -> Some [t]
  | ReactElementConfigType t -> Some [t]
  | ReactElementRefType t -> Some [t]
  | ReactConfigType (t1, t2) -> Some [t1; t2]
