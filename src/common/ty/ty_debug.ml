(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty
open Utils_js

let cut_off ?(limit=1000) str =
  let len = String.length str in
  if len > limit
    then String.sub str 0 (len - 1) ^ " ..."
    else str

let rec dump_opt (f: 'a -> string) (o: 'a option) = match o with
  | Some t -> f t
  | None -> ""

and dump_list : 'a . ('a -> string) -> ?sep:string -> 'a list -> string =
  fun f ?(sep=", ") ls ->
    List.map f ls |> String.concat sep

and dump_param_opt = function
  | { prm_optional = true } -> "?"
  | _ -> ""

and dump_param ~depth (p: string option * t * fun_param) = match p with
  | Some s, t, o ->
    spf "%s%s: %s" s (dump_param_opt o) (dump_t ~depth t)
  | _, t, o -> spf "%s%s" (dump_param_opt o) (dump_t ~depth t)

and dump_rest_params ~depth = function
  | Some (o, t) ->
    spf "...%s" (dump_param ~depth (o, t, { prm_optional = false }))
  | _ -> ""

and dump_bound ~depth = function
  | Some t -> spf " <: %s" (dump_t ~depth t)
  | _ -> ""

and dump_polarity = function
  | Positive -> "+"
  | Negative -> "-"
  | Neutral -> ""

and dump_type_param ~depth { tp_name; tp_bound; tp_polarity; _ } =
  spf "%s%s%s" (dump_polarity tp_polarity) tp_name
    (dump_bound ~depth tp_bound)

and dump_type_params ~depth = function
  | Some [] -> ""
  | Some ps -> spf "<%s>" (dump_list (dump_type_param ~depth) ps)
  | _ -> ""

and dump_fun_t ~depth { fun_params; fun_rest_param; fun_return; fun_type_params } =
  spf "Fun(%s(%s%s) => %s)"
    (dump_type_params ~depth fun_type_params)
    (dump_list (dump_param ~depth) fun_params)
    (dump_rest_params ~depth  fun_rest_param)
    (dump_t ~depth fun_return)

and dump_field { fld_polarity; fld_optional } =
  spf "%s%s" (dump_polarity fld_polarity) (if fld_optional then "?" else "")

and dump_prop ~depth = function
  | NamedProp (s, p) -> spf "%s: %s" s (dump_named_prop ~depth p)
  | IndexProp d -> dump_dict ~depth d
  | CallProp f -> dump_fun_t ~depth f

and dump_named_prop ~depth = function
  | Field (t, field) -> spf "%s: %s" (dump_t ~depth t) (dump_field field)
  | Method t -> dump_fun_t ~depth t
  | Get t -> spf "get %s" (dump_t ~depth t)
  | Set t -> spf "get %s" (dump_t ~depth t)

and dump_dict ~depth { dict_polarity; dict_name; dict_key; dict_value } =
  spf "%s[%s%s]: %s"
    (dump_polarity dict_polarity)
    (match dict_name with
      | Some n -> spf "%s: " n
      | _ -> "")
    (dump_t ~depth dict_key)
    (dump_t ~depth dict_value)

and dump_obj ~depth { obj_exact; obj_props } =
  if obj_exact
    then spf "{|%s|}" (dump_list (dump_prop ~depth) obj_props)
    else spf "{%s}"   (dump_list (dump_prop ~depth) obj_props)

and dump_generics ~depth = function
  | Some ts -> "<" ^ dump_list (dump_t ~depth) ts ^ ">"
  | _ -> ""

and dump_tvar (TVar i) = spf "T_%d" i

and dump_t ?(depth = 10) t =
  if depth < 0 then "..." else
  let depth = depth - 1 in
  match t with
  | ID v -> dump_tvar v
  | Generic (s, st, ts) ->
    spf "Generic(%s, struct= %b, params=%s)" s st (dump_generics ~depth ts)
  | Any -> "Any"
  | AnyObj -> "AnyObj"
  | AnyFun -> "AnyFun"
  | Top -> "Top"
  | Bot -> "Bot"
  | Void -> "Void"
  | Null -> "Null"
  | Num -> "Num"
  | NumLit s -> spf "\"%s\"" s
  | Str -> "Str"
  | StrLit s -> spf "\"%s\"" s
  | Bool -> "Bool"
  | BoolLit b ->  spf "\"%b\"" b
  | Fun f -> dump_fun_t ~depth f
  | Obj o -> dump_obj ~depth o
  | Arr t -> spf "Array<%s>" (dump_t ~depth t)
  | This -> "this"
  | Tup ts ->
    spf "Tup(%s)" (dump_list (dump_t ~depth) ~sep:"," ts)
  | Union (t1,t2,ts) ->
    spf "Union(%s)" (dump_list (dump_t ~depth) ~sep:", " (ListUtils.first_n 10 (t1::t2::ts)))
  | Inter (t1,t2,ts) ->
    spf "Inter(%s)" (dump_list (dump_t ~depth) ~sep:", " (t1::t2::ts))
  | TypeAlias { ta_name; ta_imported=None; ta_tparams; ta_type } ->
    spf "TypeAlias(%s, params=[%s], body=%s)"
      ta_name
      (dump_type_params ~depth ta_tparams)
      (Option.value_map ta_type ~default:"" ~f:(fun t -> cut_off (dump_t ~depth t)))
  | TypeAlias { ta_name; ta_imported=Some file; _ } ->
    spf "Import(%s, %s)" ta_name file
  | TypeOf n ->
    spf "Typeof(%s)" n
  | Exists -> "*"
  | Class (n, s, ps) ->
    spf "%s(%s, params= %s)" (if s then "Interface" else "Class") n
      (dump_type_params ~depth ps)
  | Mu (v, t) -> spf "Mu(%s, %s)" (dump_tvar v) (dump_t ~depth t)

let dump_binding (v, ty) =
  Utils_js.spf "type %s = %s" (dump_tvar v) (dump_t ty)

let dump_env_t s = List.map dump_binding s |> String.concat "\n"

let json_of_ty _ = Hh_json.JSON_String "TODO Ty_debug.json_of_ty"
