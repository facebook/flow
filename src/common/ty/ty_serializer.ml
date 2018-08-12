(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core_result
open Ty
module T = Ast.Type

let mapM f ts = all (List.map f ts)

let opt f t =
  match t with
  | Some t -> f t >>| fun t -> Some t
  | _ -> return None

let identifier x = (Loc.none, x)

let builtin x =
  return (Loc.none, T.Generic {
    T.Generic.
    id = T.Generic.Identifier.Unqualified (identifier x);
    targs = None;
  })

let generic_builtin x targs =
  return (Loc.none, T.Generic {
    T.Generic.
    id = T.Generic.Identifier.Unqualified (identifier x);
    targs = Some targs;
  })

let tvar (RVar _) = Error "Unsupported recursive variables."

let variance_ = function
  | Neutral -> None
  | Positive -> Some (Loc.none, Ast.Variance.Plus)
  | Negative -> Some (Loc.none, Ast.Variance.Minus)

let rec type_ t =
  let just t = return (Loc.none, t) in
  match t with
  | TVar (v, _) -> tvar v
  | Bound (Symbol (_, s)) -> builtin s
  | Generic (x, _, ts) -> generic x ts
  | Any -> just T.Any
  | AnyObj -> builtin "Object"
  | AnyFun -> builtin "Function"
  | Top -> just T.Mixed
  | Bot -> just T.Empty
  | Void -> just T.Void
  | Null -> just T.Null
  | Num -> just T.Number
  | Str -> just T.String
  | Bool -> just T.Boolean
  | NumLit lit -> just (T.NumberLiteral (num_lit lit))
  | StrLit lit -> just (T.StringLiteral (str_lit lit))
  | BoolLit lit -> just (T.BooleanLiteral lit)
  | Fun f -> function_ f >>| fun f -> (Loc.none, T.Function f)
  | Obj o -> obj_ o
  | Arr a -> arr a
  | Tup ts -> mapM type_ ts >>| fun ts -> (Loc.none, T.Tuple ts)
  | Union (t0, t1, ts) as t -> union t (t0,t1,ts)
  | Inter (t0, t1, ts) -> intersection (t0,t1,ts)
  | Class (s, _, _) -> class_ s

  | TypeOf _
  | TypeAlias _
  | Exists
  | Mu _
  | Module _
    ->
    Error (Utils_js.spf "Unsupported type constructor `%s`."
      (Ty_debug.string_of_ctor t))

and generic (Symbol (_, id)) targs =
  opt type_arguments targs >>| fun targs ->
  (Loc.none, T.Generic {
    T.Generic.
    id = T.Generic.Identifier.Unqualified (identifier id);
    targs
  })

and union t (t0, t1, rest) =
  let ts = bk_union t in
  if List.mem Null ts && List.mem Void ts then
    let ts = List.filter (fun t -> not (t = Null || t = Void)) ts in
    type_ (mk_union ts) >>| fun ts ->
    (Loc.none, T.Nullable ts)
  else
    type_ t0 >>= fun t0 ->
    type_ t1 >>= fun t1 ->
    mapM type_ rest >>| fun rest ->
    (Loc.none, T.Union (t0, t1, rest))

and intersection (t0, t1, rest) =
  type_ t0 >>= fun t0 ->
  type_ t1 >>= fun t1 ->
  mapM type_ rest >>| fun rest ->
  (Loc.none, T.Intersection (t0, t1, rest))

and function_ f =
  type_ f.fun_return >>= fun return ->
  fun_params f.fun_params f.fun_rest_param >>= fun params ->
  opt type_params f.fun_type_params >>| fun tparams -> {
    T.Function.
    params;
    return;
    tparams;
  }

and fun_params params rest_param =
  mapM fun_param params >>= fun params ->
  opt fun_rest_param rest_param >>| fun rest ->
  (Loc.none, {
    T.Function.Params.
    params;
    rest;
  })

and fun_param (name, t, {prm_optional}) =
  type_ t >>| fun annot ->
  (Loc.none, {
    T.Function.Param.
    name = Option.map ~f:identifier name;
    annot;
    optional = prm_optional;
  })

and fun_rest_param (name, t) =
  fun_param (name, t, {prm_optional = false}) >>| fun argument ->
  Loc.none, {
    T.Function.RestParam.
    argument;
  }

and obj_ o =
  mapM obj_prop o.obj_props >>| fun properties ->
  (Loc.none, T.Object {
    T.Object.
    exact = o.obj_exact;
    properties;
  })

and obj_prop = function
  | NamedProp (x, p) ->
    obj_named_prop x p >>| fun p -> T.Object.Property (Loc.none, p)
  | IndexProp d ->
    obj_index_prop d >>| fun p -> T.Object.Indexer (Loc.none, p)
  | CallProp f ->
    obj_call_prop f >>| fun p -> T.Object.CallProperty (Loc.none, p)

and obj_named_prop x =
  let key = Ast.Expression.Object.Property.Identifier (Loc.none, x) in
  function
  | Field (t, fld) -> type_ t >>| fun t -> {
      T.Object.Property.key;
      value = T.Object.Property.Init t;
      optional = fld.fld_optional;
      static = false;
      proto = false;
      _method = false;
      variance = variance_ fld.fld_polarity;
    }
  | Method f -> function_ f >>| fun fun_t -> {
      T.Object.Property.key;
      value = T.Object.Property.Init (Loc.none, T.Function fun_t);
      optional = false;
      static = false;
      proto = false;
      _method = true;
      variance = None;
    }
  | Get t -> getter t >>| fun t -> {
      T.Object.Property.key;
      value = T.Object.Property.Get (Loc.none, t);
      optional = false;
      static = false;
      proto = false;
      _method = false;
      variance = None;
    }
  | Set t -> setter t >>| fun t -> {
      T.Object.Property.key;
      value = T.Object.Property.Set (Loc.none, t);
      optional = false;
      static = false;
      proto = false;
      _method = false;
      variance = None;
    }

and obj_index_prop d =
  type_ d.dict_key >>= fun key ->
  type_ d.dict_value >>| fun value -> {
    T.Object.Indexer.
    id = Option.map ~f:identifier d.dict_name;
    key;
    value;
    static = false;
    variance = variance_ d.dict_polarity;
  }

and obj_call_prop f =
  function_ f >>| fun value -> {
  T.Object.CallProperty.
  value = (Loc.none, value);
  static = false;
}

and arr { arr_readonly; arr_elt_t } =
  type_ arr_elt_t >>= fun t ->
  if arr_readonly
  then generic_builtin "$ReadOnlyArray" (Loc.none, [t])
  else return (Loc.none, T.Array t)

and type_params ts =
  mapM type_param ts >>| fun ts -> (Loc.none, ts)

and type_param tp =
  opt annotation tp.tp_bound >>= fun bound ->
  opt type_ tp.tp_default >>| fun default ->
  (Loc.none, {
    T.ParameterDeclaration.TypeParam.
    name = Loc.none, tp.tp_name;
    bound;
    variance = variance_ tp.tp_polarity;
    default;
  })

and type_arguments ts =
  mapM type_ ts >>| fun ts -> (Loc.none, ts)

and str_lit lit =
  let quote = Js_layout_generator.better_quote lit in
  let raw_lit = Js_layout_generator.utf8_escape ~quote lit in
  let raw = quote ^ raw_lit ^ quote in
  { Ast.StringLiteral.value = lit; raw }

and num_lit lit = {
  Ast.NumberLiteral.
  value = (try Pervasives.float_of_string lit with Failure _ -> 0.);
  raw = lit;
}

and getter t = function_ {
  fun_params = [];
  fun_rest_param = None;
  fun_return = t;
  fun_type_params = None;
}

and setter t = function_ {
  fun_params = [(None, t, {prm_optional = false})];
  fun_rest_param = None;
  fun_return = Void;
  fun_type_params = None;
}

and class_ t =
  generic t None >>| fun t ->
  (Loc.none, T.Generic {
    T.Generic.
    id = T.Generic.Identifier.Unqualified (identifier "Class");
    targs = Some (Loc.none, [t])
  })

and annotation t = type_ t >>| fun t -> (Loc.none, t)
