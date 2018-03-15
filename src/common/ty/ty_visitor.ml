(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty

module type Env = sig
  type t
  val descend : Ty.t -> t -> t
end

module Make(M: Monoid.S)(E: Env) = struct

  include Writer.Make(M)

  let mapM f ts = all (List.map f ts)
  let map_snd : 'a 'b 'c . ('b -> 'c t) -> 'a * 'b -> ('a * 'c) t =
    fun f (x, t) -> f t >>| fun t -> (x, t)

  let opt : 'a . ('a -> 'a t) -> 'a option -> 'a option t = fun f t ->
    match t with
    | Some t -> f t >>| fun t -> Some t
    | _ -> return None

  class c = object(self)
    method type_ (env: E.t) t =
      let env = E.descend t env in
      match t with
      | Generic (n, st, Some ts) ->
        mapM (self#type_ env) ts >>| fun ts -> Generic (n, st, Some ts)
      | Generic (_, _, None) -> return t
      | Fun ft -> self#fun_t env ft >>| fun ft -> Fun ft
      | Obj ot -> self#obj_t env ot >>| fun ot -> Obj ot
      | Arr t -> self#type_ env t >>| fun a -> Arr a
      | Tup ts -> mapM (self#type_ env) ts >>| fun t -> Tup t
      | Union (t1, t2, ts) -> mapM (self#type_ env) (t1::t2::ts) >>| mk_union
      | Inter (t1, t2, ts) -> mapM (self#type_ env) (t1::t2::ts) >>| mk_inter
      | TypeAlias { ta_name; ta_imported; ta_tparams; ta_type } ->
        opt (mapM (self#param_t env)) ta_tparams >>= fun ta_tparams ->
        opt (self#type_ env) ta_type >>| fun ta_type ->
        TypeAlias { ta_name; ta_imported; ta_tparams; ta_type }
      | Class (n, s, ps) ->
        opt (mapM (self#param_t env)) ps >>| fun ps -> Class (n, s, ps)
      | Mu (v, t) -> self#type_ env t >>| fun t -> Mu (v, t)
      | (This|Any|AnyObj|AnyFun|Top|Bot|Void|Null|Num|Str|Bool|Exists
        |TVar _|NumLit _|StrLit _|BoolLit _|TypeOf _) ->
        return t

    method private fun_t env { fun_params; fun_rest_param; fun_return; fun_type_params } =
      mapM (self#fun_param env) fun_params >>= fun fun_params ->
      opt (map_snd (self#type_ env)) fun_rest_param >>= fun fun_rest_param ->
      self#type_ env fun_return >>= fun fun_return ->
      opt (mapM (self#param_t env)) fun_type_params >>| fun fun_type_params ->
      { fun_params; fun_rest_param; fun_return; fun_type_params }

    method private fun_param env (s, t, f) =
      self#type_ env t >>| fun t -> (s, t, f)

    method private obj_t env { obj_exact; obj_props } =
      mapM (self#prop_t env) obj_props >>| fun obj_props ->
      { obj_exact; obj_props }

    method private prop_t env = function
      | NamedProp (n, p) -> self#named_prop_t env p >>| fun p -> NamedProp (n,p)
      | IndexProp d -> self#dict_t env d >>| fun d -> IndexProp d
      | CallProp ft -> self#fun_t env ft >>| fun ft -> CallProp ft

    method private named_prop_t env = function
      | Field (t, f) -> self#type_ env t >>| fun t -> Field (t,f)
      | Method ft -> self#fun_t env ft >>| fun ft -> Method ft
      | Get t -> self#type_ env t >>| fun t -> Get t
      | Set t -> self#type_ env t >>| fun t -> Set t

    method private dict_t env { dict_polarity; dict_name; dict_key; dict_value } =
      self#type_ env dict_key >>= fun dict_key ->
      self#type_ env dict_value >>| fun dict_value ->
      { dict_polarity; dict_name; dict_key; dict_value }

    method param_t env { tp_name; tp_bound; tp_polarity; tp_default } =
      opt (self#type_ env) tp_bound >>= fun tp_bound ->
      opt (self#type_ env) tp_default >>| fun tp_default ->
      { tp_name; tp_bound; tp_polarity; tp_default }
  end

end

module MakeUnitEnv(M: Monoid.S) = Make(M)(struct
  type t = unit
  let descend _ () = ()
end)

module MakeAny(E: Env) = Make(Monoid.Any)(E)
module UnitVisitor = MakeUnitEnv(Monoid.Unit)
module AnyVisitor = MakeUnitEnv(Monoid.Any)
