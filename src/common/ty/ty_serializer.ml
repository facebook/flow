(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Base.Result
open Ty
module T = Flow_ast.Type

let mapM f ts = all (Base.List.map ~f ts)

let opt f t =
  match t with
  | Some t -> f t >>| fun t -> Some t
  | _ -> return None

let id_from_string x = Flow_ast_utils.ident_of_source (Loc.none, x)

let id_from_symbol x =
  let { sym_name; sym_anonymous; _ } = x in
  if sym_anonymous then
    Error (Utils_js.spf "Cannot output anonymous elements.")
  else
    Ok (id_from_string sym_name)

let mk_generic x targs =
  {
    T.Generic.id = T.Generic.Identifier.Unqualified x;
    targs;
    comments = Flow_ast_utils.mk_comments_opt ();
  }

let mk_targs arguments =
  (Loc.none, { T.TypeArgs.arguments; comments = Flow_ast_utils.mk_comments_opt () })

let mk_generic_type x targs = (Loc.none, T.Generic (mk_generic x targs))

let builtin_from_string ?targs x =
  let x = id_from_string x in
  mk_generic_type x targs

let tvar (RVar _) = Error "Unsupported recursive variables."

let variance_ = function
  | Neutral -> None
  | Positive ->
    Some
      ( Loc.none,
        { Ast.Variance.kind = Ast.Variance.Plus; comments = Flow_ast_utils.mk_comments_opt () } )
  | Negative ->
    Some
      ( Loc.none,
        { Ast.Variance.kind = Ast.Variance.Minus; comments = Flow_ast_utils.mk_comments_opt () } )

let rec type_ t =
  let just t = return (Loc.none, t) in
  match t with
  | TVar (v, _) -> tvar v
  | Bound (_, name) -> Ok (builtin_from_string name)
  | Generic (x, _, ts) -> generic_type x ts
  | Any _ -> just (T.Any (Flow_ast_utils.mk_comments_opt ()))
  | Top -> just (T.Mixed (Flow_ast_utils.mk_comments_opt ()))
  | Bot _ -> just (T.Empty (Flow_ast_utils.mk_comments_opt ()))
  | Void -> just (T.Void (Flow_ast_utils.mk_comments_opt ()))
  | Null -> just (T.Null (Flow_ast_utils.mk_comments_opt ()))
  | Symbol -> just (T.Symbol (Flow_ast_utils.mk_comments_opt ()))
  | Num (Some lit) ->
    return
      (builtin_from_string
         "$TEMPORARY$number"
         ~targs:(mk_targs [(Loc.none, T.NumberLiteral (num_lit lit))]))
  | Num None -> just (T.Number (Flow_ast_utils.mk_comments_opt ()))
  | Str (Some lit) ->
    return
      (builtin_from_string
         "$TEMPORARY$string"
         ~targs:(mk_targs [(Loc.none, T.StringLiteral (str_lit lit))]))
  | Str None -> just (T.String (Flow_ast_utils.mk_comments_opt ()))
  | Bool (Some lit) ->
    return
      (builtin_from_string
         "$TEMPORARY$boolean"
         ~targs:(mk_targs [(Loc.none, T.BooleanLiteral (bool_lit lit))]))
  | Bool None -> just (T.Boolean (Flow_ast_utils.mk_comments_opt ()))
  | NumLit lit -> just (T.NumberLiteral (num_lit lit))
  | StrLit lit -> just (T.StringLiteral (str_lit lit))
  | BoolLit lit -> just (T.BooleanLiteral (bool_lit lit))
  | Fun f -> function_ f >>| fun f -> (Loc.none, T.Function f)
  | Obj o -> obj_ o
  | Arr a -> arr a
  | Tup ts ->
    mapM type_ ts >>| fun ts ->
    (Loc.none, T.Tuple { T.Tuple.types = ts; comments = Flow_ast_utils.mk_comments_opt () })
  | Union (t0, t1, ts) as t -> union t (t0, t1, ts)
  | Inter (t0, t1, ts) -> intersection (t0, t1, ts)
  | Utility s -> utility s
  | InlineInterface i -> inline_interface i
  | CharSet s ->
    let id = id_from_string "CharSet" in
    return (mk_generic_type id (Some (mk_targs [(Loc.none, T.StringLiteral (str_lit s))])))
  | TypeOf (TSymbol name) ->
    id_from_symbol name >>= fun id ->
    just
      (T.Typeof
         {
           T.Typeof.argument = mk_generic_type id None;
           internal = false;
           comments = Flow_ast_utils.mk_comments_opt ();
         })
  | TypeOf _
  | Mu _ ->
    Error (Utils_js.spf "Unsupported type constructor `%s`." (Ty_debug.string_of_ctor_t t))

and generic x targs =
  id_from_symbol x >>= fun id ->
  opt type_arguments targs >>| fun targs -> mk_generic id targs

and generic_type x targs =
  id_from_symbol x >>= fun id ->
  opt type_arguments targs >>| fun targs -> mk_generic_type id targs

and union t (t0, t1, rest) =
  let ts = bk_union t |> Nel.to_list in
  if List.mem Null ts && List.mem Void ts then
    match List.filter (fun t -> not (t = Null || t = Void)) ts with
    | [] ->
      return
        ( Loc.none,
          T.Union
            {
              T.Union.types =
                ( (Loc.none, T.Null (Flow_ast_utils.mk_comments_opt ())),
                  (Loc.none, T.Void (Flow_ast_utils.mk_comments_opt ())),
                  [] );
              comments = Flow_ast_utils.mk_comments_opt ();
            } )
    | hd :: tl ->
      type_ (mk_union (hd, tl)) >>| fun ts ->
      ( Loc.none,
        T.Nullable { T.Nullable.argument = ts; comments = Flow_ast_utils.mk_comments_opt () } )
  else
    type_ t0 >>= fun t0 ->
    type_ t1 >>= fun t1 ->
    mapM type_ rest >>| fun rest ->
    ( Loc.none,
      T.Union { T.Union.types = (t0, t1, rest); comments = Flow_ast_utils.mk_comments_opt () } )

and intersection (t0, t1, rest) =
  type_ t0 >>= fun t0 ->
  type_ t1 >>= fun t1 ->
  mapM type_ rest >>| fun rest ->
  ( Loc.none,
    T.Intersection
      { T.Intersection.types = (t0, t1, rest); comments = Flow_ast_utils.mk_comments_opt () } )

and function_ f =
  type_ f.fun_return >>= fun return ->
  fun_params f.fun_params f.fun_rest_param >>= fun params ->
  opt type_params f.fun_type_params >>| fun tparams ->
  { T.Function.params; return; tparams; comments = Flow_ast_utils.mk_comments_opt () }

and fun_params params rest_param =
  mapM fun_param params >>= fun params ->
  opt fun_rest_param rest_param >>| fun rest ->
  (Loc.none, { T.Function.Params.params; rest; comments = Flow_ast_utils.mk_comments_opt () })

and fun_param (name, t, { prm_optional }) =
  let name = Base.Option.map ~f:id_from_string name in
  type_ t >>| fun annot -> (Loc.none, { T.Function.Param.name; annot; optional = prm_optional })

and fun_rest_param (name, t) =
  fun_param (name, t, { prm_optional = false }) >>| fun argument ->
  (Loc.none, { T.Function.RestParam.argument; comments = Flow_ast_utils.mk_comments_opt () })

and obj_ o =
  mapM obj_prop o.obj_props >>| fun properties ->
  ( Loc.none,
    T.Object
      {
        T.Object.exact = o.obj_exact;
        inexact = false;
        properties;
        comments = Flow_ast_utils.mk_comments_opt ();
      } )

and obj_prop = function
  | NamedProp { name; prop; _ } ->
    obj_named_prop name prop >>| fun p -> T.Object.Property (Loc.none, p)
  | IndexProp d -> obj_index_prop d >>| fun p -> T.Object.Indexer (Loc.none, p)
  | CallProp f -> obj_call_prop f >>| fun p -> T.Object.CallProperty (Loc.none, p)
  | SpreadProp t -> obj_spread_prop t >>| fun p -> T.Object.SpreadProperty p

and obj_named_prop =
  let to_key x =
    if Ty_printer.property_key_quotes_needed x then
      let raw = x in
      let value = Ast.Literal.String raw in
      Ast.Expression.Object.Property.Literal
        (Loc.none, { Ast.Literal.value; raw; comments = Flow_ast_utils.mk_comments_opt () })
    else
      Ast.Expression.Object.Property.Identifier (id_from_string x)
  in
  fun x prop ->
    match prop with
    | Field { t; polarity; optional } ->
      type_ t >>| fun t ->
      {
        T.Object.Property.key = to_key x;
        value = T.Object.Property.Init t;
        optional;
        static = false;
        proto = false;
        _method = false;
        variance = variance_ polarity;
      }
    | Method f ->
      function_ f >>| fun fun_t ->
      {
        T.Object.Property.key = to_key x;
        value = T.Object.Property.Init (Loc.none, T.Function fun_t);
        optional = false;
        static = false;
        proto = false;
        _method = true;
        variance = None;
      }
    | Get t ->
      getter t >>| fun t ->
      {
        T.Object.Property.key = to_key x;
        value = T.Object.Property.Get (Loc.none, t);
        optional = false;
        static = false;
        proto = false;
        _method = false;
        variance = None;
      }
    | Set t ->
      setter t >>| fun t ->
      {
        T.Object.Property.key = to_key x;
        value = T.Object.Property.Set (Loc.none, t);
        optional = false;
        static = false;
        proto = false;
        _method = false;
        variance = None;
      }

and obj_index_prop d =
  let id = Base.Option.map ~f:id_from_string d.dict_name in
  type_ d.dict_key >>= fun key ->
  type_ d.dict_value >>| fun value ->
  {
    T.Object.Indexer.id;
    key;
    value;
    static = false;
    variance = variance_ d.dict_polarity;
    comments = Flow_ast_utils.mk_comments_opt ();
  }

and obj_call_prop f =
  function_ f >>| fun value -> { T.Object.CallProperty.value = (Loc.none, value); static = false }

and obj_spread_prop t =
  type_ t >>| fun t ->
  (Loc.none, { T.Object.SpreadProperty.argument = t; comments = Flow_ast_utils.mk_comments_opt () })

and arr { arr_readonly; arr_elt_t; _ } =
  type_ arr_elt_t >>| fun t ->
  if arr_readonly then
    builtin_from_string "$ReadOnlyArray" ~targs:(mk_targs [t])
  else
    builtin_from_string "Array" ~targs:(mk_targs [t])

and type_params ts =
  mapM type_param ts >>| fun ts ->
  (Loc.none, { T.TypeParams.params = ts; comments = Flow_ast_utils.mk_comments_opt () })

and type_param tp =
  opt annotation tp.tp_bound >>= fun bound ->
  opt type_ tp.tp_default >>| fun default ->
  ( Loc.none,
    {
      T.TypeParam.name = id_from_string tp.tp_name;
      bound =
        (match bound with
        | Some t -> T.Available t
        | None -> T.Missing Loc.none);
      variance = variance_ tp.tp_polarity;
      default;
    } )

and type_arguments ts = mapM type_ ts >>| fun ts -> mk_targs ts

and str_lit lit =
  let quote = Js_layout_generator.better_quote lit in
  let raw_lit = Js_layout_generator.utf8_escape ~quote lit in
  let raw = quote ^ raw_lit ^ quote in
  { Ast.StringLiteral.value = lit; raw; comments = Flow_ast_utils.mk_comments_opt () }

and num_lit lit =
  {
    Ast.NumberLiteral.value = (try Pervasives.float_of_string lit with Failure _ -> 0.);
    raw = lit;
    comments = Flow_ast_utils.mk_comments_opt ();
  }

and bool_lit lit = { Ast.BooleanLiteral.value = lit; comments = Flow_ast_utils.mk_comments_opt () }

and getter t =
  function_
    {
      fun_params = [];
      fun_rest_param = None;
      fun_return = t;
      fun_type_params = None;
      fun_static = Ty.Top;
    }

and setter t =
  function_
    {
      fun_params = [(None, t, { prm_optional = false })];
      fun_rest_param = None;
      fun_return = Void;
      fun_type_params = None;
      fun_static = Ty.Top;
    }

and interface_extends e =
  let (x, _, ts) = e in
  generic x ts >>| fun gen -> (Loc.none, gen)

and inline_interface i =
  let { if_extends; if_body } = i in
  let { obj_props; _ } = if_body in
  mapM interface_extends if_extends >>= fun extends ->
  mapM obj_prop obj_props >>| fun properties ->
  let body =
    ( Loc.none,
      {
        T.Object.exact = false;
        inexact = false;
        properties;
        comments = Flow_ast_utils.mk_comments_opt ();
      } )
  in
  (Loc.none, T.Interface { T.Interface.body; extends; comments = Flow_ast_utils.mk_comments_opt () })

and utility u =
  let ctor = Ty.string_of_utility_ctor u in
  let ts = Ty.types_of_utility u in
  let id = id_from_string ctor in
  opt type_arguments ts >>| fun ts -> mk_generic_type id ts

and annotation t = type_ t >>| fun t -> (Loc.none, t)
