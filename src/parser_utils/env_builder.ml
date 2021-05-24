(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* These functions are adapted from typing/refinement.ml. Eventually, this will be the only place
 * where refinement logic lives, so jmbrown is ok with this temporary duplication while he is
 * fleshing out the refinement features of EnvBuilder
 *
 * The purpose of these functions is to extract _what_ is being refined when we have something like
 * expr != null. What in expr does this refine? *)
let rec key =
  let open Flow_ast.Expression in
  function
  | (_, Identifier id) -> key_of_identifier id
  | _ ->
    (* other LHSes unsupported currently/here *)
    None

and key_of_identifier (_, { Flow_ast.Identifier.name; comments = _ }) =
  if name = "undefined" then
    None
  else
    Some name

let is_number_literal node =
  let open Flow_ast in
  match node with
  | Expression.Literal { Literal.value = Literal.Number _; _ }
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.Literal { Literal.value = Literal.Number _; _ });
        comments = _;
      } ->
    true
  | _ -> false

let extract_number_literal node =
  let open Flow_ast in
  match node with
  | Expression.Literal { Literal.value = Literal.Number lit; raw; comments = _ } -> (lit, raw)
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.Literal { Literal.value = Literal.Number lit; raw; _ });
        comments = _;
      } ->
    (-.lit, "-" ^ raw)
  | _ -> Utils_js.assert_false "not a number literal"

module type S = sig
  module L : Loc_sig.S

  module Ssa_api : Ssa_api.S with module L = L

  module Scope_api : Scope_api_sig.S with module L = L

  module Provider_api : Provider_api.S with module L = L

  type refinement_kind =
    | And of refinement_kind * refinement_kind
    | Or of refinement_kind * refinement_kind
    | Not of refinement_kind
    | Truthy of L.t
    | Null
    | Undefined
    | Maybe
    | InstanceOf of L.t
    | IsArray
    | BoolR of L.t
    | FunctionR
    | NumberR of L.t
    | ObjectR
    | StringR of L.t
    | SymbolR of L.t
    | SingletonBoolR of {
        loc: L.t;
        sense: bool;
        lit: bool;
      }
    | SingletonStrR of {
        loc: L.t;
        sense: bool;
        lit: string;
      }
    | SingletonNumR of {
        loc: L.t;
        sense: bool;
        lit: float * string;
      }
  [@@deriving show { with_path = false }]

  type refinement = L.LSet.t * refinement_kind

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: Ssa_api.values;
    refinements: refinement L.LMap.t;
    providers: Provider_api.info;
  }

  val program_with_scope : ?ignore_toplevel:bool -> (L.t, L.t) Flow_ast.Program.t -> env_info

  val program : (L.t, L.t) Flow_ast.Program.t -> refinement L.LMap.t

  val sources_of_use : env_info -> L.t -> L.LSet.t

  val source_bindings : env_info -> L.LSet.t L.LMap.t
end

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_api : Scope_api_sig.S with module L = Ssa_api.L) :
  S with module L = L and module Ssa_api = Ssa_api and module Scope_api = Scope_api = struct
  module L = L
  module Ssa_api = Ssa_api
  module Scope_api = Scope_api
  module Scope_builder = Scope_builder.Make (L) (Scope_api)
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)
  module Invalidation_api = Invalidation_api.Make (L) (Scope_api) (Ssa_api)
  module Provider_api = Provider_api.Make (L)

  type refinement_kind =
    | And of refinement_kind * refinement_kind
    | Or of refinement_kind * refinement_kind
    | Not of refinement_kind
    | Truthy of L.t
    | Null
    | Undefined
    | Maybe
    | InstanceOf of L.t
    | IsArray
    | BoolR of L.t
    | FunctionR
    | NumberR of L.t
    | ObjectR
    | StringR of L.t
    | SymbolR of L.t
    | SingletonBoolR of {
        loc: L.t;
        sense: bool;
        lit: bool;
      }
    | SingletonStrR of {
        loc: L.t;
        sense: bool;
        lit: string;
      }
    | SingletonNumR of {
        loc: L.t;
        sense: bool;
        lit: float * string;
      }
  [@@deriving show { with_path = false }]

  type refinement = L.LSet.t * refinement_kind

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: Ssa_api.values;
    refinements: refinement L.LMap.t;
    providers: Provider_api.info;
  }

  type refinement_chain =
    | BASE of refinement
    | SUCCESSIVE of refinement_chain * int
    | AND of int * int
    | OR of int * int
    | NOT of int

  let merge_and ref1 ref2 = AND (ref1, ref2)

  let merge_or ref1 ref2 = OR (ref1, ref2)

  class env_builder (prepass_info, prepass_values) provider_info =
    object (this)
      inherit Ssa_builder.ssa_builder as super

      val invalidation_caches = Invalidation_api.mk_caches ()

      (* Maps SSA ids to refinement ids. This mapping contains _all_ the refinements reachable at
       * any point in the code. The latest_refinement maps keep track of which entries to read. *)
      val mutable refinement_heap = IMap.empty

      (* Stack mapping SSA ids to refinement ids. TODO: Use a ValMap.t list instead of an IMap.t list so that
       * ocaml warns us if we conflate ssa and refinement ids. 
       * We push new entries onto the stack when we enter a new refinement scope. This lets us
       * easily do things like negate refinements. It is critical that refinements in one layer of this stack
       * do not refer to refinements in other layers of this stack so that negations do not propagate to
       * refinements that appeared before the new scope. This invariant is maintained by add_refinement. *)
      val mutable latest_refinements = []

      val mutable refined_reads = L.LMap.empty

      val mutable globals_env = SMap.empty

      method refined_reads : refinement L.LMap.t = refined_reads

      val mutable curr_id = 0

      method private new_id () =
        let new_id = curr_id in
        curr_id <- curr_id + 1;
        new_id

      method private push_refinement_scope new_latest_refinements =
        latest_refinements <- new_latest_refinements :: latest_refinements

      method private pop_refinement_scope () = latest_refinements <- List.tl latest_refinements

      method private peek_new_refinements () = List.hd latest_refinements

      method private negate_new_refinements () =
        (* For each new ssa_id -> refinement_id we need to make a new_refinement_id and map 
         * ssa_id -> new_refinement_id  and new_refinement_id -> NOT refinement_id *)
        let head = List.hd latest_refinements in
        let new_latest_refinements =
          IMap.map
            (fun ref_id ->
              let new_id = this#new_id () in
              let new_ref = NOT ref_id in
              refinement_heap <- IMap.add new_id new_ref refinement_heap;
              new_id)
            head
        in
        latest_refinements <- new_latest_refinements :: List.tl latest_refinements

      method private merge_self_refinement_scope new_refinements =
        let head = List.hd latest_refinements in
        let head' =
          IMap.merge
            (fun _ latest1 latest2 ->
              match (latest1, latest2) with
              | (_, None) -> latest1
              | (_, Some _) -> latest2)
            head
            new_refinements
        in
        latest_refinements <- head' :: List.tl latest_refinements

      method private find_refinement_ids ssa_id =
        List.fold_left
          (fun acc refs ->
            match IMap.find_opt ssa_id refs with
            | None -> acc
            | Some id -> id :: acc)
          []
          latest_refinements

      method private find_refinement name =
        let writes =
          match SMap.find_opt name this#ssa_env with
          | Some v -> Some (Ssa_builder.Val.id_of_val v)
          | None -> SMap.find_opt name globals_env
        in
        match writes with
        | None -> None
        | Some ssa_id ->
          let refinement_ids = this#find_refinement_ids ssa_id in
          List.fold_left
            (fun acc id ->
              match (acc, IMap.find_opt id refinement_heap) with
              | (None, x) -> x
              | (x, None) -> x
              | (Some x, Some _) -> Some (SUCCESSIVE (x, id)))
            None
            (List.rev refinement_ids)

      method private add_refinement name refinement =
        let ssa_id =
          match SMap.find_opt name this#ssa_env with
          | Some writes_to_loc -> Ssa_builder.Val.id_of_val writes_to_loc
          | None -> this#add_global name
        in
        let new_id = this#new_id () in
        let head = List.hd latest_refinements in
        (* Note we are not using find_refinement_ids. We do not want this refinement to
         * refer to refinements in previous scopes. This is an important invariant to maintain:
         * refinements in one scope should not refer to refinements in another scope. If there
         * was a refinement in a previous scope, then when we _read_ the refinement we will combine
         * the refinement from the previous scope into the refinement in the current scope via SUCCESSIVE.
         * TODO: assert this invariant at runtime
         *)
        let old_id = IMap.find_opt ssa_id head in
        latest_refinements <- IMap.add ssa_id new_id head :: List.tl latest_refinements;
        let new_refinement =
          match old_id with
          | Some old_id -> SUCCESSIVE (BASE refinement, old_id)
          | _ -> BASE refinement
        in
        let new_expression_refinements = IMap.add new_id new_refinement refinement_heap in
        refinement_heap <- new_expression_refinements

      method add_global name =
        match SMap.find_opt name globals_env with
        | Some id -> id
        | None ->
          let id = Ssa_builder.Val.new_id () in
          globals_env <- SMap.add name id globals_env;
          id

      method! havoc_current_ssa_env ~all =
        SMap.iter
          (fun _x { Ssa_builder.val_ref; havoc = { Ssa_builder.Havoc.unresolved; locs } } ->
            match locs with
            | loc :: _
              when Invalidation_api.should_invalidate
                     ~all
                     invalidation_caches
                     prepass_info
                     prepass_values
                     loc ->
              (* NOTE: havoc_env should already have all writes to x, so the only
               additional thing that could come from ssa_env is "uninitialized." On
               the other hand, we *dont* want to include "uninitialized" if it's no
               longer in ssa_env, since that means that x has been initialized (and
               there's no going back). *)
              val_ref := unresolved
            | [] ->
              (* If we haven't yet seen a write to this variable, we always havoc *)
              val_ref := unresolved
            | _ -> ())
          ssa_env;
        globals_env <- SMap.empty

      method! havoc_uninitialized_ssa_env =
        super#havoc_uninitialized_ssa_env;
        globals_env <- SMap.empty

      method identifier_refinement ((loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        this#add_refinement name (L.LSet.singleton loc, Truthy loc)

      method assignment_refinement loc assignment =
        ignore @@ this#assignment loc assignment;
        let open Flow_ast.Expression.Assignment in
        match assignment.left with
        | ( id_loc,
            Flow_ast.Pattern.Identifier
              { Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ } ) ->
          this#add_refinement name (L.LSet.singleton loc, Truthy id_loc)
        | _ -> ()

      method private merge_refinement_scopes ~merge lhs_latest_refinements rhs_latest_refinements =
        let new_latest_refinements =
          IMap.merge
            (fun _ ref1 ref2 ->
              match (ref1, ref2) with
              | (None, None) -> None
              | (Some ref, None) -> Some ref
              | (None, Some ref) -> Some ref
              | (Some ref1, Some ref2) ->
                let new_ref = merge ref1 ref2 in
                let new_id = this#new_id () in
                refinement_heap <- IMap.add new_id new_ref refinement_heap;
                Some new_id)
            lhs_latest_refinements
            rhs_latest_refinements
        in
        this#merge_self_refinement_scope new_latest_refinements

      method logical_refinement expr =
        let { Flow_ast.Expression.Logical.operator; left = (loc, _) as left; right; comments = _ } =
          expr
        in
        this#push_refinement_scope IMap.empty;
        let (lhs_latest_refinements, rhs_latest_refinements, env1) =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.And ->
            ignore @@ this#expression_refinement left;
            let lhs_latest_refinements = this#peek_new_refinements () in
            (match operator with
            | Flow_ast.Expression.Logical.Or -> this#negate_new_refinements ()
            | _ -> ());
            let env1 = this#ssa_env in
            this#push_refinement_scope IMap.empty;
            ignore @@ this#expression_refinement right;
            let rhs_latest_refinements = this#peek_new_refinements () in
            (* Pop LHS refinement scope *)
            this#pop_refinement_scope ();
            (* Pop RHS refinement scope *)
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1)
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            (* If this overall expression is truthy, then either the LHS or the RHS has to be truthy.
               If it's because the LHS is truthy, then the LHS also has to be non-maybe (this is of course
               true by definition, but it's also true because of the nature of ??).
               But if we're evaluating the RHS, the LHS doesn't have to be truthy, it just has to be
               non-maybe. As a result, we do this weird dance of refinements so that when we traverse the
               RHS we have done the null-test but the overall result of this expression includes both the
               truthy and non-maybe qualities. *)
            ignore (this#null_test ~strict:false ~sense:false loc left);
            let env1 = this#ssa_env in
            let nullish = this#peek_new_refinements () in
            this#negate_new_refinements ();
            this#push_refinement_scope IMap.empty;
            ignore (this#expression_refinement right);
            let rhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#pop_refinement_scope ();
            this#push_refinement_scope nullish;
            (match key left with
            | None -> ()
            | Some name -> this#add_refinement name (L.LSet.singleton loc, Truthy loc));
            let lhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1)
        in
        let merge =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            merge_or
          | Flow_ast.Expression.Logical.And -> merge_and
        in
        this#merge_refinement_scopes merge lhs_latest_refinements rhs_latest_refinements;
        this#merge_self_ssa_env env1

      method null_test ~strict ~sense loc expr =
        ignore @@ this#expression expr;
        match key expr with
        | None -> ()
        | Some name ->
          let refinement =
            if strict then
              Null
            else
              Maybe
          in
          let refinement =
            if sense then
              refinement
            else
              Not refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)

      method void_test ~sense ~strict ~check_for_bound_undefined loc expr =
        ignore @@ this#expression expr;
        match key expr with
        | None -> ()
        | Some name ->
          (* Only add the refinement if undefined is not re-bound *)
          if (not check_for_bound_undefined) || SMap.find_opt "undefined" this#ssa_env = None then
            let refinement =
              if strict then
                Undefined
              else
                Maybe
            in
            let refinement =
              if sense then
                refinement
              else
                Not refinement
            in
            this#add_refinement name (L.LSet.singleton loc, refinement)

      method typeof_test loc arg typename sense =
        ignore @@ this#expression arg;
        let refinement =
          match typename with
          | "boolean" -> Some (BoolR loc)
          | "function" -> Some FunctionR
          | "number" -> Some (NumberR loc)
          | "object" -> Some ObjectR
          | "string" -> Some (StringR loc)
          | "symbol" -> Some (SymbolR loc)
          | "undefined" -> Some Undefined
          | _ -> None
        in
        match (refinement, key arg) with
        | (Some ref, Some name) ->
          let refinement =
            if sense then
              ref
            else
              Not ref
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method literal_test ~strict ~sense loc expr refinement =
        ignore @@ this#expression expr;
        match key expr with
        | Some name when strict ->
          let refinement =
            if sense then
              refinement
            else
              Not refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method eq_test ~strict ~sense loc left right =
        let open Flow_ast in
        match (left, right) with
        (* typeof expr ==/=== string *)
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ),
            (_, Expression.Literal { Literal.value = Literal.String s; _ }) )
        | ( (_, Expression.Literal { Literal.value = Literal.String s; _ }),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ) )
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ),
            ( _,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = s; _ };
                          _;
                        } );
                    ];
                  expressions = [];
                  comments = _;
                } ) )
        | ( ( _,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = s; _ };
                          _;
                        } );
                    ];
                  expressions = [];
                  comments = _;
                } ),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ) )
          ->
          this#typeof_test loc argument s sense
        (* bool equality *)
        | ((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }), expr)
        | (expr, (lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ })) ->
          this#literal_test ~strict ~sense loc expr (SingletonBoolR { loc = lit_loc; sense; lit })
        (* string equality *)
        | ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }), expr)
        | (expr, (lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }))
        | ( expr,
            ( lit_loc,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = lit; _ };
                          _;
                        } );
                    ];
                  _;
                } ) )
        | ( ( lit_loc,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = lit; _ };
                          _;
                        } );
                    ];
                  _;
                } ),
            expr ) ->
          this#literal_test ~strict ~sense loc expr (SingletonStrR { loc = lit_loc; sense; lit })
        (* number equality *)
        | ((lit_loc, number_literal), expr) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
        | (expr, (lit_loc, number_literal)) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
        (* expr op null *)
        | ((_, Expression.Literal { Literal.value = Literal.Null; _ }), expr)
        | (expr, (_, Expression.Literal { Literal.value = Literal.Null; _ })) ->
          this#null_test ~sense ~strict loc expr
        (* expr op undefined *)
        | ( ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined ),
            expr )
        | ( expr,
            ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined ) ) ->
          ignore @@ this#expression undefined;
          this#void_test ~sense ~strict ~check_for_bound_undefined:true loc expr
        (* expr op void(...) *)
        | ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }), expr)
        | (expr, (_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ })) ->
          this#void_test ~sense ~strict ~check_for_bound_undefined:false loc expr
        | _ ->
          ignore @@ this#expression left;
          ignore @@ this#expression right

      method instance_test loc expr instance =
        ignore @@ this#expression expr;
        ignore @@ this#expression instance;
        match key expr with
        | None -> ()
        | Some name ->
          let (inst_loc, _) = instance in
          this#add_refinement name (L.LSet.singleton loc, InstanceOf inst_loc)

      method binary_refinement loc expr =
        let open Flow_ast.Expression.Binary in
        let { operator; left; right; comments = _ } = expr in
        match operator with
        (* == and != refine if lhs or rhs is an ident and other side is null *)
        | Equal -> this#eq_test ~strict:false ~sense:true loc left right
        | NotEqual -> this#eq_test ~strict:false ~sense:false loc left right
        | StrictEqual -> this#eq_test ~strict:true ~sense:true loc left right
        | StrictNotEqual -> this#eq_test ~strict:true ~sense:false loc left right
        | Instanceof -> this#instance_test loc left right
        | LessThan
        | LessThanEqual
        | GreaterThan
        | GreaterThanEqual
        | In
        | LShift
        | RShift
        | RShift3
        | Plus
        | Minus
        | Mult
        | Exp
        | Div
        | Mod
        | BitOr
        | Xor
        | BitAnd ->
          ignore @@ this#binary loc expr

      method call_refinement loc call =
        match call with
        | {
         Flow_ast.Expression.Call.callee =
           ( _,
             Flow_ast.Expression.Member
               {
                 Flow_ast.Expression.Member._object =
                   ( _,
                     Flow_ast.Expression.Identifier
                       (_, { Flow_ast.Identifier.name = "Array"; comments = _ }) );
                 property =
                   Flow_ast.Expression.Member.PropertyIdentifier
                     (_, { Flow_ast.Identifier.name = "isArray"; comments = _ });
                 comments = _;
               } );
         targs = _;
         arguments =
           ( _,
             {
               Flow_ast.Expression.ArgList.arguments = [Flow_ast.Expression.Expression arg];
               comments = _;
             } );
         comments = _;
        } ->
          ignore @@ this#expression arg;
          (match key arg with
          | None -> ()
          | Some name -> this#add_refinement name (L.LSet.singleton loc, IsArray))
        | _ -> ignore @@ this#call loc call

      method unary_refinement
          loc ({ Flow_ast.Expression.Unary.operator; argument; comments = _ } as unary) =
        match operator with
        | Flow_ast.Expression.Unary.Not ->
          this#push_refinement_scope IMap.empty;
          ignore @@ this#expression_refinement argument;
          this#negate_new_refinements ();
          let negated_refinements = this#peek_new_refinements () in
          this#pop_refinement_scope ();
          this#merge_self_refinement_scope negated_refinements
        | _ -> ignore @@ this#unary_expression loc unary

      method expression_refinement ((loc, expr) as expression) =
        let open Flow_ast.Expression in
        match expr with
        | Identifier ident ->
          this#identifier_refinement ident;
          expression
        | Logical logical ->
          this#logical_refinement logical;
          expression
        | Assignment assignment ->
          this#assignment_refinement loc assignment;
          expression
        | Binary binary ->
          this#binary_refinement loc binary;
          expression
        | Call call ->
          this#call_refinement loc call;
          expression
        | Unary unary ->
          this#unary_refinement loc unary;
          expression
        | Array _
        | ArrowFunction _
        | Class _
        | Comprehension _
        | Conditional _
        | Function _
        | Generator _
        | Import _
        | JSXElement _
        | JSXFragment _
        | Literal _
        | MetaProperty _
        | Member _
        | New _
        | Object _
        | OptionalCall _
        | OptionalMember _
        | Sequence _
        | Super _
        | TaggedTemplate _
        | TemplateLiteral _
        | TypeCast _
        | This _
        | Update _
        | Yield _ ->
          this#expression expression

      method! logical _loc (expr : (L.t, L.t) Flow_ast.Expression.Logical.t) =
        let open Flow_ast.Expression.Logical in
        let { operator; left = (loc, _) as left; right; comments = _ } = expr in
        this#push_refinement_scope IMap.empty;
        (match operator with
        | Flow_ast.Expression.Logical.Or
        | Flow_ast.Expression.Logical.And ->
          ignore (this#expression_refinement left)
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          ignore (this#null_test ~strict:false ~sense:false loc left));
        let env1 = this#ssa_env in
        (match operator with
        | Flow_ast.Expression.Logical.NullishCoalesce
        | Flow_ast.Expression.Logical.Or ->
          this#negate_new_refinements ()
        | Flow_ast.Expression.Logical.And -> ());
        ignore @@ this#expression right;
        this#pop_refinement_scope ();
        this#merge_self_ssa_env env1;
        expr

      method! pattern_identifier ?kind ident =
        let open Ssa_builder in
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) loc in
        begin
          match SMap.find_opt x ssa_env with
          | Some { val_ref; havoc } ->
            val_ref := Val.one reason;
            Havoc.(
              havoc.locs <- Base.Option.value_exn (Provider_api.providers_of_def provider_info loc))
          | _ -> ()
        end;
        super#super_pattern_identifier ?kind ident

      method private chain_to_refinement =
        function
        | BASE refinement -> refinement
        | SUCCESSIVE (refinement_chain, id) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement refinement_chain in
          (L.LSet.union locs1 locs2, And (ref1, ref2))
        | AND (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 refinement_heap) in
          (L.LSet.union locs1 locs2, And (ref1, ref2))
        | OR (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 refinement_heap) in
          (L.LSet.union locs1 locs2, Or (ref1, ref2))
        | NOT id ->
          let (locs, ref) = this#chain_to_refinement (IMap.find id refinement_heap) in
          (locs, Not ref)

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read *)
      method! any_identifier loc name =
        super#any_identifier loc name;
        match this#find_refinement name with
        | None -> ()
        | Some refinement_chain ->
          let refinement = this#chain_to_refinement refinement_chain in
          refined_reads <- L.LMap.add loc refinement refined_reads
    end

  let program_with_scope ?(ignore_toplevel = false) program =
    let open Hoister in
    let (loc, _) = program in
    let ((scopes, ssa_values) as prepass) =
      Ssa_builder.program_with_scope ~ignore_toplevel program
    in
    let providers = Provider_api.find_providers program in
    let ssa_walk = new env_builder prepass providers in
    let bindings =
      if ignore_toplevel then
        Bindings.empty
      else
        let hoist = new hoister ~with_types:true in
        hoist#eval hoist#program program
    in
    ignore @@ ssa_walk#with_bindings loc bindings ssa_walk#program program;
    {
      scopes;
      ssa_values;
      env_values = ssa_walk#values;
      refinements = ssa_walk#refined_reads;
      providers;
    }

  let program program =
    let { refinements; _ } = program_with_scope ~ignore_toplevel:false program in
    refinements

  let sources_of_use { env_values = vals; refinements = refis; _ } loc =
    let write_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map
           ~f:
             (Fn.compose
                L.LSet.of_list
                (Base.List.filter_map ~f:(function
                    | Ssa_api.Uninitialized -> None
                    | Ssa_api.Write r -> Some (Reason.poly_loc_of_reason r))))
           ~default:L.LSet.empty
    in
    let refi_locs =
      L.LMap.find_opt loc refis |> Base.Option.value_map ~f:fst ~default:L.LSet.empty
    in
    L.LSet.union refi_locs write_locs

  let source_bindings ({ env_values = vals; refinements = refis; _ } as info) =
    let keys = L.LSet.of_list (L.LMap.keys vals @ L.LMap.keys refis) in
    L.LSet.fold (fun k acc -> L.LMap.add k (sources_of_use info k) acc) keys L.LMap.empty
end

module With_Loc = Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc)
include With_ALoc
