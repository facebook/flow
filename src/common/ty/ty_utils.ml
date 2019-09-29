(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FreeVars = struct
  type env = {
    is_toplevel: bool;
    skip: ISet.t;
  }

  let searcher =
    object (self)
      inherit [_] Ty.reduce_ty as super

      method zero = ISet.empty

      method plus = ISet.union

      method! on_t env t =
        Ty.(
          match t with
          | TVar (RVar i, _) when not (ISet.mem i env.skip) -> ISet.singleton i
          | TypeAlias { ta_tparams; ta_type = Some t_body; _ } ->
            let env' = { env with is_toplevel = false } in
            let acc = self#on_option (self#on_list self#on_type_param) env' ta_tparams in
            (* If the type alias is the top-level constructor, then the body of the alias
             * will be useful and so we descend into that type expression and collect
             * variables. Otherwise we avoid collecting variables from the body as it is
             * typically not be exposed through a type query. *)
            if env.is_toplevel then
              self#plus acc (self#on_t env' t_body)
            else
              acc
          | Mu (v, t) ->
            let env = { env with skip = ISet.add v env.skip } in
            super#on_t env t
          | t -> super#on_t env t)
    end

  (* Computes the set of variables appearing free in the input. *)
  let of_type ~is_toplevel t =
    let env = { is_toplevel; skip = ISet.empty } in
    searcher#on_t env t
end

let tvar_appears_in_type ~is_toplevel v t =
  let (Ty.RVar v) = v in
  ISet.mem v (FreeVars.of_type ~is_toplevel t)

module Size = struct
  exception SizeCutOff

  type bounded_int =
    | Exactly of int
    | GreaterThan of int

  let _size_to_string = function
    | Exactly x -> Utils_js.spf "%d" x
    | GreaterThan x -> Utils_js.spf "(Greater than %d)" x

  let of_type =
    Ty.(
      let size = ref 0 in
      let o =
        object
          inherit [_] iter_ty as super

          method! on_t max (t : Ty.t) =
            size := !size + 1;
            if !size > max then raise SizeCutOff;
            super#on_t max t
        end
      in
      fun ~max t ->
        size := 0;
        match o#on_t max t with
        | exception SizeCutOff -> GreaterThan max
        | () -> Exactly !size)
end

let size_of_type ?(max = 10000) t =
  match Size.of_type ~max t with
  | Size.GreaterThan _ -> None
  | Size.Exactly s -> Some s

let symbols_of_type =
  Ty.(
    let o =
      object (_self)
        inherit [_] reduce_ty as _super

        method zero = []

        method plus = List.rev_append

        method! on_symbol _env s = [s]
      end
    in
    (fun t -> o#on_t () t))

module Simplify = struct
  type config = {
    is_bot: Ty.t -> bool;
    is_top: Ty.t -> bool;
    compare: Ty.t -> Ty.t -> int;
    sort: bool;
  }

  (* When merge_kinds is set to true then all kinds of Any (resp. Bot) types
   * are considered equivalent when comparing types. Specifically for the Bot type
   * we implement a predicate 'is_bot' that determines when the type should be
   * considered the empty element. *)
  let mk_config ~merge_kinds ~sort =
    let is_top = function
      | Ty.Top -> true
      | _ -> false
    in
    let is_bot =
      if merge_kinds then
        function
      | Ty.Bot _ -> true
      | _ -> false
      else
        let is_bot_upper_kind = function
          | Ty.NoUpper -> true
          | Ty.SomeKnownUpper _
          | Ty.SomeUnknownUpper _ ->
            false
        in
        let is_bot_kind = function
          | Ty.EmptyType -> true
          | Ty.EmptyTypeDestructorTriggerT _ -> false
          | Ty.EmptyMatchingPropT -> false
          | Ty.NoLowerWithUpper kind -> is_bot_upper_kind kind
        in
        function
        | Ty.Bot kind -> is_bot_kind kind
        | _ -> false
    in
    let compare =
      let comparator =
        if merge_kinds then
          object
            inherit [unit] Ty.comparator_ty

            (* All Bot kinds are equivalent *)
            method! private on_bot_kind () _ _ = ()

            method! private on_any_kind () _ _ = ()
          end
        else
          new Ty.comparator_ty
      in
      comparator#compare ()
    in
    { is_top; is_bot; compare; sort }

  (* Simplify union/intersection types, by
   * A. removing equal nodes from union and intersection types, and
   * B. removing the neutral element for union (resp. intersection) types,
   *    which is the bottom (resp. top) type.
   *
   *  WARNING: This visitor will do a deep type traversal.
   *)
  let run =
    let rec simplify_list ~is_zero ~is_one acc = function
      | [] -> acc
      | t :: ts ->
        if is_zero t then
          [t]
        else if is_one t then
          simplify_list ~is_zero ~is_one acc ts
        else
          simplify_list ~is_zero ~is_one (t :: acc) ts
    in
    let simplify_nel ~is_zero ~is_one (t, ts) =
      match simplify_list [] ~is_zero ~is_one (t :: ts) with
      | [] -> (t, [])
      | t :: ts -> (t, ts)
    in
    let mapper =
      object (self)
        inherit [_] Ty.endo_ty

        method private on_nel f env nel =
          let (hd, tl) = nel in
          let hd' = f env hd in
          let tl' = self#on_list f env tl in
          if hd == hd' && tl == tl' then
            nel
          else
            (hd', tl')

        method private simplify config ~break ~is_zero ~is_one ~make ~default ts0 =
          let { compare; sort; _ } = config in
          let ts1 = self#on_nel self#on_t config ts0 in
          let len1 = Nel.length ts1 in
          let ts2 = Nel.map_concat break ts1 in
          let len2 = Nel.length ts2 in
          let (ts2, len2) =
            if len1 <> len2 then
              (ts2, len2)
            else
              (ts1, len1)
          in
          let ts3 = ts2 |> simplify_nel ~is_zero ~is_one |> Nel.dedup ~compare in
          (* Note we are currently giving up on pointer equality when we are sorting types *)
          let ts3 =
            if sort || len2 <> Nel.length ts3 then
              ts3
            else
              ts2
          in
          if ts0 == ts3 then
            default
          else
            make ts3

        method! on_Union config u t0 t1 ts =
          let { is_top; is_bot; _ } = config in
          self#simplify
            ~break:Ty.bk_union
            ~make:Ty.mk_union
            ~is_zero:is_top
            ~is_one:is_bot
            ~default:u
            config
            (t0, t1 :: ts)

        method! on_Inter config i t0 t1 ts =
          let { is_top; is_bot; _ } = config in
          self#simplify
            ~break:Ty.bk_inter
            ~make:Ty.mk_inter
            ~is_zero:is_bot
            ~is_one:is_top
            ~default:i
            config
            (t0, t1 :: ts)
      end
    in
    fun ~merge_kinds ~sort ->
      let config = mk_config ~merge_kinds ~sort in
      mapper#on_t config
end

let simplify_type ~merge_kinds ?(sort = false) = Simplify.run ~merge_kinds ~sort
