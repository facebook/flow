(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason
open Loc_collections

let check_ref_use cx rrid in_hook var_reason kind t =
  let rec recur_id seen t =
    let recur = recur_id seen in
    let open Type in
    match t with
    | DefT
        ( _,
          ObjT
            {
              flags = { react_dro = Some (_, (HookReturn | HookArg | Props)); _ };
              call_t = None;
              props_tmap;
              _;
            }
        )
      when kind = Error_message.Access ->
      let props = Context.find_props cx props_tmap in
      if NameUtils.Map.cardinal props = 1 then
        (* Catch only cases that look like { current: T } *)
        Flow_js_utils.add_output
          cx
          (Error_message.EReactRefInRender { usage = var_reason; kind; in_hook })
    | OpaqueT (_, { opaque_id; _ })
      when Base.Option.value_map ~default:false ~f:(( = ) opaque_id) rrid ->
      Flow_js_utils.add_output
        cx
        (Error_message.EReactRefInRender { usage = var_reason; kind; in_hook })
    | OpaqueT (_, { underlying_t; super_t; _ }) ->
      Base.Option.iter ~f:recur underlying_t;
      Base.Option.iter ~f:recur super_t
    | OpenT (_, id) when ISet.mem id seen -> ()
    | OpenT (_, id) ->
      Flow_js_utils.possible_types cx id |> Base.List.iter ~f:(recur_id (ISet.add id seen))
    | UnionT (_, rep) ->
      let (_ : UnionRep.t) =
        UnionRep.ident_map
          (fun t ->
            recur t;
            t)
          rep
      in
      ()
    | IntersectionT (_, rep) ->
      let (_ : InterRep.t) =
        InterRep.ident_map
          (fun t ->
            recur t;
            t)
          rep
      in
      ()
    | MaybeT (_, t)
    | OptionalT { type_ = t; _ }
    | ExactT (_, t)
    | AnnotT (_, t, _)
    | TypeAppT { type_ = t; _ }
    | GenericT { bound = t; _ }
    | DefT (_, PolyT { t_out = t; _ })
    | DefT (_, TypeT (_, t)) ->
      recur t
    | _ -> ()
  in
  recur_id ISet.empty t

type permissiveness =
  | Permissive
  | Strict
  | Pattern

type hook_result =
  | HookCallee of ALocFuzzySet.t
  | MaybeHookCallee of {
      hooks: ALocFuzzySet.t;
      non_hooks: ALocFuzzySet.t;
    }
  | NotHookCallee of ALocFuzzySet.t
  | AnyCallee

let hook_callee cx t =
  let merge l r =
    match (l, r) with
    | (AnyCallee, other)
    | (other, AnyCallee) ->
      other
    | (NotHookCallee l, NotHookCallee r) -> NotHookCallee (ALocFuzzySet.union l r)
    | (HookCallee l, HookCallee r) -> HookCallee (ALocFuzzySet.union l r)
    | (MaybeHookCallee l, MaybeHookCallee r) ->
      MaybeHookCallee
        {
          hooks = ALocFuzzySet.union l.hooks r.hooks;
          non_hooks = ALocFuzzySet.union l.non_hooks r.non_hooks;
        }
    | (MaybeHookCallee m, HookCallee h)
    | (HookCallee h, MaybeHookCallee m) ->
      MaybeHookCallee { m with hooks = ALocFuzzySet.union m.hooks h }
    | (MaybeHookCallee m, NotHookCallee h)
    | (NotHookCallee h, MaybeHookCallee m) ->
      MaybeHookCallee { m with non_hooks = ALocFuzzySet.union m.non_hooks h }
    | (HookCallee h, NotHookCallee n)
    | (NotHookCallee n, HookCallee h) ->
      MaybeHookCallee { non_hooks = n; hooks = h }
  in
  let set_of_reason r = def_loc_of_reason r |> ALocFuzzySet.singleton in
  let rec recur_id seen t =
    let recur = recur_id seen in
    let open Type in
    match t with
    | DefT (r, FunT (_, { hook = HookDecl _ | HookAnnot; _ })) -> HookCallee (set_of_reason r)
    | DefT (_, FunT (_, { hook = AnyHook; _ })) -> AnyCallee
    | DefT (r, FunT (_, { hook = NonHook; _ })) -> NotHookCallee (set_of_reason r)
    | OpaqueT (_, { underlying_t; super_t; _ }) -> begin
      match (underlying_t, super_t) with
      | (Some t, _)
      | (None, Some t) ->
        recur t
      | _ -> AnyCallee
    end
    | OpenT (_, id) when ISet.mem id seen -> AnyCallee
    | OpenT (_, id) ->
      Flow_js_utils.possible_types cx id
      |> Base.List.map ~f:(recur_id (ISet.add id seen))
      |> Base.List.fold ~init:AnyCallee ~f:merge
    | UnionT (_, rep) ->
      UnionRep.members rep |> Base.List.fold ~init:AnyCallee ~f:(fun acc t -> merge (recur t) acc)
    | IntersectionT _ when Context.hooklike_functions cx ->
      (* We can't easily handle intersections with the HooklikeT destructor, so if we're
         in compatibility mode, let's just punt on enforcement *)
      AnyCallee
    | IntersectionT (rs, rep) ->
      (* Not tracking locations of hooks through unions *)
      let inv_merge l r =
        match (l, r) with
        | (AnyCallee, other)
        | (other, AnyCallee) ->
          other
        | _ when l = r -> l
        | (HookCallee _, _)
        | (_, HookCallee _) ->
          HookCallee (set_of_reason rs)
        | (MaybeHookCallee _, _)
        | (_, MaybeHookCallee _) ->
          MaybeHookCallee { hooks = set_of_reason rs; non_hooks = set_of_reason rs }
        | (NotHookCallee _, NotHookCallee _) -> NotHookCallee (set_of_reason rs)
      in
      InterRep.members rep
      |> Base.List.fold ~init:AnyCallee ~f:(fun acc t -> inv_merge (recur t) acc)
    | MaybeT (_, t)
    | OptionalT { type_ = t; _ }
    | ExactT (_, t)
    | AnnotT (_, t, _)
    | TypeAppT { type_ = t; _ }
    | GenericT { bound = t; _ }
    | DefT (_, PolyT { t_out = t; _ })
    | DefT (_, TypeT (_, t)) ->
      recur t
    | DefT (_, ObjT { call_t = Some id; _ }) -> recur (Context.find_call cx id)
    | _ -> AnyCallee
  in
  recur_id ISet.empty t

let hook_error cx ~call_loc ~callee_loc kind =
  if Context.react_rule_enabled cx Options.RulesOfHooks then
    Flow_js_utils.add_output
      cx
      (Error_message.EHookRuleViolation { call_loc; callee_loc; hook_rule = kind })

let compatibility_call call =
  match call with
  | {
   Ast.Expression.Call.callee =
     ( _,
       Ast.Expression.(
         ( Identifier (_, { Ast.Identifier.name; _ })
         | Member
             {
               Member._object = (_, Identifier (_, { Ast.Identifier.name = "React"; _ }));
               property = Member.PropertyIdentifier (_, { Ast.Identifier.name; _ });
               _;
             } ))
     );
   _;
  }
    when name = "forwardRef" || name = "memo" ->
    true
  | {
   Ast.Expression.Call.callee =
     (_, Ast.Expression.(Identifier (_, { Ast.Identifier.name = "renderHook"; _ })));
   _;
  } ->
    true
  | _ -> false

let componentlike_name name =
  let rec cln n =
    if String.length name > n && String.sub name n (n + 1) = "_" then
      cln (n + 1)
    else
      String.length name > n
      &&
      let fst = String.sub name n (n + 1) in
      fst = String.uppercase_ascii fst
  in
  cln 0

let bare_use { Ast.Expression.Call.callee; _ } =
  let open Ast.Expression in
  match callee with
  | (_, Identifier (_, { Flow_ast.Identifier.name = "use"; _ }))
  | ( _,
      Member
        {
          Member.property = Member.PropertyIdentifier (_, { Flow_ast.Identifier.name = "use"; _ });
          _;
        }
    ) ->
    true
  | _ -> false

module ConditionalState : sig
  type t

  type saved_cond

  type saved_switch

  type saved_try

  val init : t

  val conditional : t -> bool

  val enter_conditional : t -> (t -> unit) -> saved_cond

  val reset_conditional : t -> saved_cond -> t

  val return : t -> t

  val enter_label : t -> (t -> unit) -> string -> saved_cond

  val reset_label : t -> string -> saved_cond -> t

  val enter_switch : t -> (t -> unit) -> saved_cond * saved_switch

  val reset_switch : t -> saved_cond -> saved_switch -> t

  val break : t -> string option -> t

  val merge : t -> t -> t

  val enter_try : t -> (t -> unit) -> saved_cond * saved_try

  val reset_try : t -> saved_cond -> saved_try -> t

  val throwable : t -> t
end = struct
  type try_state =
    | NotInTry
    | InTry
    | InTryPostCall

  type t = {
    conditional_context: bool;
    broken: bool;
    return_seen: bool;
    label_scopes: bool SMap.t;
    switch_scope: bool;
    try_state: try_state;
  }

  type saved_cond = bool

  type saved_switch = bool

  type saved_try = try_state

  let max_try t1 t2 =
    match (t1, t2) with
    | (InTryPostCall, _)
    | (_, InTryPostCall) ->
      InTryPostCall
    | (InTry, _)
    | (_, InTry) ->
      InTry
    | (NotInTry, NotInTry) -> NotInTry

  let init =
    {
      conditional_context = false;
      broken = false;
      return_seen = false;
      label_scopes = SMap.empty;
      switch_scope = false;
      try_state = NotInTry;
    }

  let conditional { conditional_context; _ } = conditional_context

  let enter_conditional ({ conditional_context; _ } as t) setter =
    setter { t with conditional_context = true };
    conditional_context

  let reset_conditional
      ( {
          broken;
          return_seen;
          try_state;
          label_scopes = _;
          switch_scope = _;
          conditional_context = _;
        } as t
      )
      conditional =
    {
      t with
      conditional_context = broken || return_seen || conditional || try_state = InTryPostCall;
    }

  let return t = { t with conditional_context = true; return_seen = true }

  let enter_label ({ conditional_context; label_scopes; _ } as t) setter s =
    setter { t with label_scopes = SMap.add s false label_scopes };
    conditional_context

  let reset_broken ({ label_scopes; switch_scope; _ } as t) =
    { t with broken = SMap.fold (fun _ -> ( || )) label_scopes switch_scope }

  let reset_label ({ label_scopes; _ } as t) s conditional =
    let label_broken = SMap.find s label_scopes in
    let t = { t with label_scopes = SMap.remove s label_scopes } in
    if label_broken then
      let t = reset_broken t in
      reset_conditional t conditional
    else
      t

  let enter_switch ({ conditional_context; switch_scope; _ } as t) setter =
    setter { t with switch_scope = false };
    (conditional_context, switch_scope)

  let reset_switch ({ switch_scope; _ } as t) conditional cur_switch =
    let t = { t with switch_scope = cur_switch } in
    if switch_scope then
      let t = reset_broken t in
      reset_conditional t conditional
    else
      t

  let break t s =
    let ({ label_scopes; _ } as t) = { t with broken = true; conditional_context = true } in
    match s with
    | None -> { t with switch_scope = true }
    | Some s -> { t with label_scopes = SMap.add s true label_scopes }

  let enter_try ({ conditional_context; try_state; _ } as t) setter =
    setter { t with try_state = max_try try_state InTry };
    (conditional_context, try_state)

  let reset_try ({ try_state; _ } as t) conditional cur_try_state =
    let t = { t with try_state = cur_try_state } in
    if try_state = InTryPostCall then
      reset_conditional t conditional
    else
      t

  let throwable ({ try_state; _ } as t) =
    if try_state = InTry then
      { t with conditional_context = true; try_state = InTryPostCall }
    else
      t

  let merge
      {
        conditional_context = c1;
        broken = b1;
        return_seen = r1;
        label_scopes = l1;
        switch_scope = s1;
        try_state = t1;
      }
      {
        conditional_context = c2;
        broken = b2;
        return_seen = r2;
        label_scopes = l2;
        switch_scope = s2;
        try_state = t2;
      } =
    {
      conditional_context = c1 || c2;
      broken = b1 || b2;
      label_scopes =
        SMap.merge
          (fun _ a b ->
            match (a, b) with
            | (Some a, Some b) -> Some (a || b)
            | (Some a, _)
            | (_, Some a) ->
              Some a
            | (None, None) -> None)
          l1
          l2;
      switch_scope = s1 || s2;
      try_state = max_try t1 t2;
      return_seen = r1 || r2;
    }
end

let rec whole_ast_visitor ~under_component cx rrid =
  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable in_function_component = false

    val mutable declaring_function_component = false

    method on_loc_annot l = l

    method on_type_annot l = l

    method! component_declaration = (component_ast_visitor cx false rrid)#component_declaration

    method! function_ fn =
      let {
        Ast.Function.id;
        params = (_, { Ast.Function.Params.params = params_list; rest; _ }) as params;
        body;
        async;
        generator;
        hook;
        predicate;
        return;
        tparams;
        sig_loc;
        comments;
      } =
        fn
      in
      let is_probably_function_component =
        (* Capitalized letter initial name *)
        Base.Option.value_map
          ~f:(fun (_, { Ast.Identifier.name; _ }) -> componentlike_name name)
          ~default:false
          id
        && List.length params_list <= 2 (* Props and ref *)
        && Base.Option.is_none rest
      in
      if (Context.react_rules_always cx && is_probably_function_component) || hook then
        let ident' = Base.Option.map ~f:this#function_identifier id in
        this#type_params_opt tparams (fun tparams' ->
            let params' = (component_ast_visitor cx hook rrid)#function_params params in
            let return' = this#function_return_annotation return in
            let body' = (component_ast_visitor cx hook rrid)#function_component_body body in
            let predicate' =
              Base.Option.map ~f:(component_ast_visitor cx hook rrid)#predicate predicate
            in
            let sig_loc' = this#on_loc_annot sig_loc in
            let comments' = this#syntax_opt comments in
            {
              Ast.Function.id = ident';
              params = params';
              return = return';
              body = body';
              async;
              generator;
              hook;
              predicate = predicate';
              tparams = tparams';
              sig_loc = sig_loc';
              comments = comments';
            }
        )
      else begin
        let cur_in_function_component = in_function_component in
        let next_in_function_component =
          (declaring_function_component
          || is_probably_function_component
          || Base.Option.is_some (Flow_ast_utils.hook_function fn)
          )
          && Context.hooklike_functions cx
        in
        in_function_component <- next_in_function_component;
        let res = super#function_ fn in
        in_function_component <- cur_in_function_component;
        res
      end

    method! call ((call_loc, _) as annot) expr =
      let { Ast.Expression.Call.callee = ((callee_loc, callee_ty), callee_exp); _ } = expr in
      let callee_ty =
        match callee_exp with
        | Ast.Expression.Identifier (_, { Ast.Identifier.name; _ })
          when Context.hooklike_functions cx && name <> "require" ->
          (* If we're in compatibility mode, we want to bail on intersections. But
             the typed AST records the type of the overload we've selected, so we
             never see the intersection to realize we need to bail! Instead in this
             case we read from the environment. *)
          Type_env.var_ref cx (Reason.OrdinaryName name) callee_loc
        | _ -> callee_ty
      in
      begin
        match hook_callee cx callee_ty with
        | HookCallee _
        | MaybeHookCallee _
          when (not in_function_component)
               && not (Flow_ast_utils.hook_call expr && bare_use expr && under_component) ->
          hook_error cx ~callee_loc ~call_loc Error_message.HookNotInComponentOrHook
        | _ -> ()
      end;
      let cur_declaring = declaring_function_component in
      declaring_function_component <- compatibility_call expr || cur_declaring;
      let expr = super#call annot expr in
      declaring_function_component <- cur_declaring;
      expr

    method! export_default_declaration_decl decl =
      let cur_declaring = declaring_function_component in
      let filename = Context.file cx |> File_key.to_string in
      let next_declaring =
        if componentlike_name filename || Flow_ast_utils.hook_name filename then
          match decl with
          | Ast.Statement.ExportDefaultDeclaration.Expression
              (_, Ast.Expression.(ArrowFunction _ | Function _)) ->
            true
          | Ast.Statement.ExportDefaultDeclaration.Declaration
              (_, Ast.Statement.FunctionDeclaration _) ->
            true
          | _ -> false
        else
          false
      in
      declaring_function_component <- next_declaring || cur_declaring;
      let decl = super#export_default_declaration_decl decl in
      declaring_function_component <- cur_declaring;
      decl

    method! object_property (loc, prop) =
      let cur_declaring = declaring_function_component in
      let next_declaring =
        match prop with
        | Ast.Expression.Object.Property.(
            Method { key = Identifier (_, { Ast.Identifier.name; _ }); _ })
        | Ast.Expression.Object.Property.(
            Init
              {
                key = Identifier (_, { Ast.Identifier.name; _ });
                value = (_, Ast.Expression.(ArrowFunction _ | Function _));
                _;
              }) ->
          Flow_ast_utils.hook_name name || name = "render"
        | _ -> false
      in
      declaring_function_component <- next_declaring;
      let res = super#object_property (loc, prop) in
      declaring_function_component <- cur_declaring;
      res

    method! return ({ Ast.Statement.Return.argument; _ } as r) =
      let cur_declaring = declaring_function_component in
      let next_declaring =
        match argument with
        | Some (_, Ast.Expression.(ArrowFunction _ | Function _)) -> Context.hooklike_functions cx
        | _ -> false
      in
      declaring_function_component <- next_declaring;
      let res = super#return r in
      declaring_function_component <- cur_declaring;
      res

    method! body_expression ((_, argument) as r) =
      let cur_declaring = declaring_function_component in
      let next_declaring =
        match argument with
        | Ast.Expression.(ArrowFunction _ | Function _) -> Context.hooklike_functions cx
        | _ -> false
      in
      declaring_function_component <- next_declaring;
      let res = super#body_expression r in
      declaring_function_component <- cur_declaring;
      res

    method! variable_declarator ~kind decl =
      let (_, { Ast.Statement.VariableDeclaration.Declarator.id; init }) = decl in
      let (_ : (_, _) Ast.Pattern.t) = this#variable_declarator_pattern ~kind id in
      let cur_declaring = declaring_function_component in
      let next_declaring =
        match (id, init) with
        | ( ( _,
              Ast.Pattern.Identifier
                { Ast.Pattern.Identifier.name = (_, { Ast.Identifier.name; _ }); _ }
            ),
            Some (_, Ast.Expression.(ArrowFunction _ | Function _))
          ) ->
          Context.hooklike_functions cx && (Flow_ast_utils.hook_name name || componentlike_name name)
        | _ -> false
      in
      declaring_function_component <- next_declaring;
      let (_ : _ option) = Base.Option.map ~f:this#expression init in
      declaring_function_component <- cur_declaring;
      decl
  end

and component_ast_visitor cx is_hook rrid =
  let { Loc_env.var_info = { Env_api.env_values; _ } as var_info; _ } = Context.environment cx in
  let is_initializing loc =
    match Env_api.write_locs_of_read_loc env_values loc with
    | exception Not_found -> false
    | [] -> false
    | _ :: _ as write_locs ->
      let open Env_api in
      let open Refi in
      let is_nullish t =
        let rec recur_id seen t =
          let recur = recur_id seen in
          let open Type in
          match t with
          | OpenT (_, id) when ISet.mem id seen -> false
          | OpenT (_, id) ->
            let possible = Flow_js_utils.possible_types cx id in
            Base.List.for_all possible ~f:(recur_id (ISet.add id seen)) && List.length possible > 0
          | UnionT (_, rep) -> UnionRep.members rep |> Base.List.for_all ~f:recur
          | DefT (_, (NullT | VoidT)) -> true
          | _ -> false
        in
        recur_id ISet.empty t
      in
      let rec refines_safe refi =
        match refi with
        | AndR (l, r) -> refines_safe l || refines_safe r
        | OrR (l, r) -> refines_safe l && refines_safe r
        | NotR r -> not (not_refines_safe r)
        | SentinelR ("current", loc) ->
          is_nullish
            (Type_env.find_write cx Env_api.ExpressionLoc (Reason.mk_reason (RCustom "") loc))
        | PropNullishR { propname = "current"; _ } -> true
        | _ -> false
      and not_refines_safe refi =
        match refi with
        | AndR (l, r) -> not_refines_safe l && not_refines_safe r
        | OrR (l, r) -> not_refines_safe l || not_refines_safe r
        | NotR r -> not (refines_safe r)
        | PropExistsR { propname = "current"; _ } -> false
        | _ -> true
      in

      Base.List.for_all write_locs ~f:(fun write ->
          let refis = Env_api.refinements_of_write_loc var_info write in
          Base.List.exists ~f:refines_safe refis
      )
  in

  object (this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    val mutable conditional_state = ConditionalState.init

    val mutable calling_nonhook = false

    method on_loc_annot l = l

    method on_type_annot l = l

    method! member _ = failwith "Call visit_member"

    method! optional_member _ = failwith "Call visit_optional_member"

    method! expression ((_, expr') as expr) =
      match expr' with
      | Ast.Expression.Member mem ->
        this#visit_member mem;
        expr
      | Ast.Expression.OptionalMember mem ->
        this#visit_optional_member mem;
        expr
      | _ -> super#expression expr

    method! pattern_expression ((_, expr') as expr) =
      match expr' with
      | Ast.Expression.Member mem ->
        this#visit_member ~permissive:Pattern mem;
        expr
      | Ast.Expression.OptionalMember mem ->
        this#visit_optional_member ~permissive:Pattern mem;
        expr
      | _ -> super#expression expr

    method! predicate_expression = this#permissive_expression

    method permissive_expression ((_, expr') as expr) =
      match expr' with
      | Ast.Expression.Member mem ->
        this#visit_member ~permissive:Permissive mem;
        expr
      | Ast.Expression.OptionalMember mem ->
        this#visit_optional_member ~permissive:Permissive mem;
        expr
      | Ast.Expression.Unary
          { Ast.Expression.Unary.operator = Ast.Expression.Unary.Not; argument; comments = _ } ->
        let (_ : _ Ast.Expression.t) = this#permissive_expression argument in
        expr
      | Ast.Expression.Binary
          {
            Ast.Expression.Binary.operator =
              Ast.Expression.Binary.(Equal | StrictEqual | NotEqual | StrictNotEqual);
            left;
            right;
            comments = _;
          } ->
        let (_ : _ Ast.Expression.t) = this#permissive_expression left in
        let (_ : _ Ast.Expression.t) = this#permissive_expression right in
        expr
      | Ast.Expression.Logical { Ast.Expression.Logical.operator = _; left; right; comments = _ } ->
        let (_ : _ Ast.Expression.t) = this#permissive_expression left in
        let (_ : _ Ast.Expression.t) = this#permissive_expression right in
        expr
      | _ -> this#expression expr

    method target_expression (((loc, ty), exp) as expr) err_kind =
      let reason =
        match exp with
        | Ast.Expression.Identifier ((loc, _), { Ast.Identifier.name; _ }) ->
          mk_reason (RIdentifier (OrdinaryName name)) loc
        | _ -> mk_reason (RCustom "expression") loc
      in
      if Context.react_rule_enabled cx Options.ValidateRefAccessDuringRender then
        check_ref_use cx rrid is_hook reason err_kind ty;
      this#expression expr

    method! arg_list (annot, args) =
      let open Ast.Expression.ArgList in
      let { arguments; _ } = args in
      let (_ : _ list) =
        Base.List.map
          ~f:(function
            | Ast.Expression.Expression exp when calling_nonhook ->
              Ast.Expression.Expression (this#target_expression exp Error_message.Argument)
            | Ast.Expression.Expression exp -> Ast.Expression.Expression (this#expression exp)
            | Ast.Expression.Spread spread -> Ast.Expression.Spread (this#spread_element spread))
          arguments
      in
      (annot, args)

    method visit_optional_member ?permissive { Ast.Expression.OptionalMember.member; _ } =
      this#visit_member ?permissive member

    method visit_member ?(permissive = Strict) expr =
      let { Ast.Expression.Member._object = ((loc, _), _) as _object; property; comments = _ } =
        expr
      in
      let (_ : (_, _) Ast.Expression.Member.property) = this#member_property property in
      let (_ : (_, _) Ast.Expression.t) =
        match property with
        | Ast.Expression.Member.PropertyIdentifier (_, { Ast.Identifier.name = "current"; _ })
          when permissive = Strict || (permissive = Pattern && not (is_initializing loc)) ->
          this#target_expression _object Error_message.Access
        | _ -> this#expression _object
      in
      ()

    method! call ((call_loc, _) as annot) expr =
      let { Ast.Expression.Call.callee = ((callee_loc, callee_ty), callee_exp); _ } = expr in
      let callee_ty =
        match callee_exp with
        | Ast.Expression.Identifier (_, { Ast.Identifier.name; _ })
          when Context.hooklike_functions cx && name <> "require" ->
          (* If we're in compatibility mode, we want to bail on intersections. But
             the typed AST records the type of the overload we've selected, so we
             never see the intersection to realize we need to bail! Instead in this
             case we read from the environment. *)
          Type_env.var_ref cx (Reason.OrdinaryName name) callee_loc
        | _ -> callee_ty
      in
      let hook_error = hook_error cx ~call_loc ~callee_loc in
      let check_args_for_refs =
        match hook_callee cx callee_ty with
        | HookCallee _ ->
          begin
            if Flow_ast_utils.hook_call expr then begin
              if ConditionalState.conditional conditional_state && not (bare_use expr) then
                hook_error Error_message.ConditionalHook
            end else
              hook_error Error_message.HookHasIllegalName
          end;
          false
        | MaybeHookCallee { hooks; non_hooks } ->
          hook_error
            Error_message.(
              MaybeHook
                { hooks = ALocFuzzySet.elements hooks; non_hooks = ALocFuzzySet.elements non_hooks }
            );
          false
        | NotHookCallee _ ->
          if Flow_ast_utils.hook_call expr then hook_error Error_message.NonHookHasIllegalName;
          true
        | AnyCallee -> false
      in
      let cur_calling = calling_nonhook in
      calling_nonhook <- check_args_for_refs;
      let res = super#call annot expr in
      calling_nonhook <- cur_calling;
      conditional_state <- ConditionalState.throwable conditional_state;
      res

    method! new_ expr =
      let res = super#new_ expr in
      conditional_state <- ConditionalState.throwable conditional_state;
      res

    method! throw stmt =
      let res = super#throw stmt in
      conditional_state <- ConditionalState.throwable conditional_state;
      res

    method in_conditional : 'a 'b. ('a -> 'b) -> 'a -> 'b =
      fun f n ->
        let cur =
          ConditionalState.enter_conditional conditional_state (fun state ->
              conditional_state <- state
          )
        in
        let res = f n in
        conditional_state <- ConditionalState.reset_conditional conditional_state cur;
        res

    method! return r =
      let res = super#return r in
      conditional_state <- ConditionalState.return conditional_state;
      res

    method! labeled_statement
        ({ Ast.Statement.Labeled.label = (_, { Ast.Identifier.name; _ }); _ } as stmt) =
      let cur =
        ConditionalState.enter_label
          conditional_state
          (fun state -> conditional_state <- state)
          name
      in
      let res = super#labeled_statement stmt in
      conditional_state <- ConditionalState.reset_label conditional_state name cur;
      res

    method! switch stmt =
      let (cur, cur_switch) =
        ConditionalState.enter_switch conditional_state (fun state -> conditional_state <- state)
      in

      let { Ast.Statement.Switch.discriminant; cases; comments = _; exhaustive_out = _ } = stmt in
      let (_ : _ Ast.Expression.t) = this#expression discriminant in
      let (_ : _ list) =
        Base.List.mapi
          ~f:(fun i (_, case) -> this#visit_switch_case ~is_last:(i = List.length cases - 1) case)
          cases
      in

      conditional_state <- ConditionalState.reset_switch conditional_state cur cur_switch;
      stmt

    method visit_switch_case ~is_last ({ Ast.Statement.Switch.Case.test; _ } as case) =
      if Base.Option.is_some test || not is_last then
        this#in_conditional this#switch_case case
      else
        this#switch_case case

    method! break ({ Ast.Statement.Break.label; comments = _ } as stmt) =
      let s = Base.Option.map ~f:(fun (_, { Ast.Identifier.name; _ }) -> name) label in
      conditional_state <- ConditionalState.break conditional_state s;
      super#break stmt

    method try_block ({ Ast.Statement.Block.body; comments = _ } as block) =
      let (cur, cur_try) =
        ConditionalState.enter_try conditional_state (fun state -> conditional_state <- state)
      in
      let (_ : _ list) = this#statement_list body in
      conditional_state <- ConditionalState.reset_try conditional_state cur cur_try;
      block

    method! try_catch stmt =
      let { Ast.Statement.Try.block = (_, block); handler; finalizer; comments = _ } = stmt in
      let pre_cond = conditional_state in
      let (_ : (_, _) Ast.Statement.Block.t) = this#try_block block in
      let (_ : _ option) =
        Base.Option.map
          ~f:(fun (_, handler) -> this#in_conditional this#catch_clause handler)
          handler
      in
      let post_cond = conditional_state in
      conditional_state <- pre_cond;
      let (_ : _ option) =
        Base.Option.map ~f:(fun (_, finalizer) -> this#block finalizer) finalizer
      in
      conditional_state <- ConditionalState.merge post_cond conditional_state;
      stmt

    method! if_consequent_statement ~has_else =
      this#in_conditional (super#if_consequent_statement ~has_else)

    method! if_alternate_statement = this#in_conditional super#if_alternate_statement

    method! conditional expr =
      let { Ast.Expression.Conditional.test; consequent; alternate; _ } = expr in
      let _test' : _ Ast.Expression.t = this#predicate_expression test in
      let _consequent' : _ Ast.Expression.t = this#in_conditional this#expression consequent in
      let _alternate' : _ Ast.Expression.t = this#in_conditional this#expression alternate in
      expr

    method! logical expr =
      let { Ast.Expression.Logical.left; right; _ } = expr in
      let _left' : _ Ast.Expression.t = this#expression left in
      let _right' : _ Ast.Expression.t = this#in_conditional this#expression right in
      expr

    method! for_in_statement stmt =
      let { Ast.Statement.ForIn.left; right; body; _ } = stmt in
      let _left' : _ Ast.Statement.ForIn.left = this#for_in_statement_lhs left in
      let _right' : _ Ast.Expression.t = this#expression right in
      let _body' : _ Ast.Statement.t = this#in_conditional this#statement body in
      stmt

    method! for_of_statement stmt =
      let { Ast.Statement.ForOf.left; right; body; _ } = stmt in
      let _left' : _ Ast.Statement.ForOf.left = this#for_of_statement_lhs left in
      let _right' : _ Ast.Expression.t = this#expression right in
      let _body' : _ Ast.Statement.t = this#in_conditional this#statement body in
      stmt

    method! for_statement stmt =
      let { Ast.Statement.For.init; test; update; body; _ } = stmt in
      let _init' : _ option = Base.Option.map ~f:this#for_statement_init init in
      let _test' : _ option = Base.Option.map ~f:this#predicate_expression test in
      let _update' : _ option = Base.Option.map ~f:(this#in_conditional this#expression) update in
      let _body' : _ Ast.Statement.t = this#in_conditional this#statement body in
      stmt

    method! while_ stmt =
      let { Ast.Statement.While.test; body; _ } = stmt in
      let _test' : _ Ast.Expression.t = this#predicate_expression test in
      let _body' : _ Ast.Statement.t = this#in_conditional this#statement body in
      stmt

    method function_component_body = super#function_body_any

    method! function_body_any = (whole_ast_visitor ~under_component:true cx rrid)#function_body_any

    method! class_body = (whole_ast_visitor ~under_component:true cx rrid)#class_body
  end

let check_react_rules cx ast =
  let rrid =
    let open Type in
    let get_t cx =
      let no_lowers _cx r = Type.Unsoundness.merged_any r in
      function
      | OpenT (r, id) -> Flow_js_utils.merge_tvar ~no_lowers cx r id
      | t -> t
    in

    let builtins = Context.builtins cx in
    let lhs = Builtins.get_builtin_type_opt builtins "React$RefObject" in
    match Base.Option.map ~f:(get_t cx) lhs with
    | Some (DefT (_, PolyT { t_out = DefT (_, TypeT (_, OpaqueT (_, { opaque_id; _ }))); _ })) ->
      Some opaque_id
    | _ -> None
  in
  let _ = (whole_ast_visitor ~under_component:false cx rrid)#program ast in
  ()
