(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let extract_number_literal node =
  match Flow_ast_utils.extract_number_literal node with
  | Some lit -> lit
  | None -> raise Env_api.(Env_invariant (None, Impossible "not a number literal"))

let extract_bigint_literal node =
  match Flow_ast_utils.extract_bigint_literal node with
  | Some lit -> lit
  | None -> raise Env_api.(Env_invariant (None, Impossible "not a bigint literal"))

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  val jsx_attributes_possible_sentinel_refinements :
    (ALoc.t, ALoc.t) Ast.JSX.Opening.attribute list -> Hint.sentinel_refinement SMap.t

  val object_properties_possible_sentinel_refinements :
    (ALoc.t, ALoc.t) Ast.Expression.Object.property list -> Hint.sentinel_refinement SMap.t

  val visit_eq_test :
    on_type_of_test:
      (ALoc.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      string ->
      bool ->
      'a
      ) ->
    on_literal_test:
      (strict:bool ->
      sense:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      Env_api.Refi.refinement_kind ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      'a
      ) ->
    on_null_test:
      (sense:bool ->
      strict:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      'a
      ) ->
    on_void_test:
      (sense:bool ->
      strict:bool ->
      check_for_bound_undefined:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      (ALoc.t, ALoc.t) Ast.Expression.t ->
      'a
      ) ->
    on_member_eq_other:((ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> 'a) ->
    on_other_eq_member:((ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> 'a) ->
    is_switch_cond_context:bool ->
    on_other_eq_test:((ALoc.t, ALoc.t) Ast.Expression.t -> (ALoc.t, ALoc.t) Ast.Expression.t -> 'a) ->
    strict:bool ->
    sense:bool ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Ast.Expression.t ->
    (ALoc.t, ALoc.t) Ast.Expression.t ->
    'a
end

module Make
    (Scope_api : Scope_api_sig.S with module L = Loc_sig.ALocS)
    (Ssa_api : Ssa_api.S with module L = Loc_sig.ALocS)
    (Env_api : Env_api.S
                 with module L = Loc_sig.ALocS
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api) : S with module Env_api = Env_api = struct
  module Env_api = Env_api
  module RefinementKey = Refinement_key.Make (Loc_sig.ALocS)
  open Env_api.Refi
  open Hint

  let literal_check_of_expr ((_loc, expr) as e) =
    match expr with
    | Ast.Expression.StringLiteral { Ast.StringLiteral.value; _ } -> Some (SingletonStr value)
    | Ast.Expression.NumberLiteral { Ast.NumberLiteral.value; _ } -> Some (SingletonNum value)
    | Ast.Expression.BooleanLiteral { Ast.BooleanLiteral.value; _ } -> Some (SingletonBool value)
    | Ast.Expression.Member mem ->
      if Base.Option.is_some @@ RefinementKey.lookup_of_member ~allow_optional:false mem then
        Some (Member (Reason.mk_expression_reason e))
      else
        None
    | _ -> None

  let jsx_attributes_possible_sentinel_refinements =
    let open Ast.JSX in
    Base.List.fold ~init:SMap.empty ~f:(fun acc -> function
      | Opening.Attribute
          ( _,
            {
              Attribute.name =
                Ast.JSX.Attribute.Identifier (_, { Ast.JSX.Identifier.name; comments = _ });
              value;
            }
          ) ->
        let check =
          match value with
          | None -> Some (SingletonBool true)
          | Some (Attribute.StringLiteral (_, { Ast.StringLiteral.value; _ })) ->
            Some (SingletonStr value)
          | Some
              (Attribute.ExpressionContainer
                (_, { ExpressionContainer.expression = ExpressionContainer.Expression e; _ })
                ) ->
            literal_check_of_expr e
          | Some
              (Attribute.ExpressionContainer
                (_, { ExpressionContainer.expression = ExpressionContainer.EmptyExpression; _ })
                ) ->
            None
        in
        Base.Option.value_map check ~default:acc ~f:(fun check -> SMap.add name check acc)
      | _ -> acc
    )

  let object_properties_possible_sentinel_refinements =
    Base.List.fold ~init:SMap.empty ~f:(fun acc -> function
      | Ast.Expression.Object.Property p ->
        let open Ast.Expression.Object.Property in
        (match p with
        | ( _,
            Init
              {
                key =
                  ( Ast.Expression.Object.Property.StringLiteral
                      (_, { Ast.StringLiteral.value = name; _ })
                  | Ast.Expression.Object.Property.Identifier
                      (_, { Ast.Identifier.name; comments = _ }) );
                value;
                shorthand = _;
              }
          ) ->
          literal_check_of_expr value
          |> Base.Option.value_map ~default:acc ~f:(fun check -> SMap.add name check acc)
        | _ -> acc)
      | Ast.Expression.Object.SpreadProperty _ -> acc
    )

  let visit_eq_test
      ~on_type_of_test
      ~on_literal_test
      ~on_null_test
      ~on_void_test
      ~on_member_eq_other
      ~on_other_eq_member
      ~is_switch_cond_context
      ~on_other_eq_test
      ~strict
      ~sense
      loc
      left
      right =
    let open Ast in
    match (left, right) with
    (* typeof expr ==/=== string *)
    | ( ( _,
          Expression.Unary
            { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
        ),
        ((_, Expression.StringLiteral { StringLiteral.value = s; _ }) as other)
      )
    | ( ((_, Expression.StringLiteral { StringLiteral.value = s; _ }) as other),
        ( _,
          Expression.Unary
            { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
        )
      )
    | ( ( _,
          Expression.Unary
            { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
        ),
        ( ( _,
            Expression.TemplateLiteral
              {
                Expression.TemplateLiteral.quasis =
                  [
                    ( _,
                      {
                        Expression.TemplateLiteral.Element.value =
                          { Expression.TemplateLiteral.Element.cooked = s; _ };
                        _;
                      }
                    );
                  ];
                expressions = [];
                comments = _;
              }
          ) as other
        )
      )
    | ( ( ( _,
            Expression.TemplateLiteral
              {
                Expression.TemplateLiteral.quasis =
                  [
                    ( _,
                      {
                        Expression.TemplateLiteral.Element.value =
                          { Expression.TemplateLiteral.Element.cooked = s; _ };
                        _;
                      }
                    );
                  ];
                expressions = [];
                comments = _;
              }
          ) as other
        ),
        ( _,
          Expression.Unary
            { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
        )
      ) ->
      on_type_of_test loc argument other s sense
    (* bool equality *)
    | (((lit_loc, Expression.BooleanLiteral { BooleanLiteral.value = lit; _ }) as other), expr)
    | (expr, ((lit_loc, Expression.BooleanLiteral { BooleanLiteral.value = lit; _ }) as other)) ->
      on_literal_test ~strict ~sense loc expr (SingletonBoolR { loc = lit_loc; sense; lit }) other
    (* string equality *)
    | (((lit_loc, Expression.StringLiteral { StringLiteral.value = lit; _ }) as other), expr)
    | (expr, ((lit_loc, Expression.StringLiteral { StringLiteral.value = lit; _ }) as other))
    | ( expr,
        ( ( lit_loc,
            Expression.TemplateLiteral
              {
                Expression.TemplateLiteral.quasis =
                  [
                    ( _,
                      {
                        Expression.TemplateLiteral.Element.value =
                          { Expression.TemplateLiteral.Element.cooked = lit; _ };
                        _;
                      }
                    );
                  ];
                _;
              }
          ) as other
        )
      )
    | ( ( ( lit_loc,
            Expression.TemplateLiteral
              {
                Expression.TemplateLiteral.quasis =
                  [
                    ( _,
                      {
                        Expression.TemplateLiteral.Element.value =
                          { Expression.TemplateLiteral.Element.cooked = lit; _ };
                        _;
                      }
                    );
                  ];
                _;
              }
          ) as other
        ),
        expr
      ) ->
      on_literal_test ~strict ~sense loc expr (SingletonStrR { loc = lit_loc; sense; lit }) other
    (* number equality *)
    | (((lit_loc, number_literal) as other), expr)
      when Flow_ast_utils.is_number_literal number_literal ->
      let raw = extract_number_literal number_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonNumR { loc = lit_loc; sense; lit = raw })
        other
    | (expr, ((lit_loc, number_literal) as other))
      when Flow_ast_utils.is_number_literal number_literal ->
      let raw = extract_number_literal number_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonNumR { loc = lit_loc; sense; lit = raw })
        other
    (* bigint equality *)
    | (((lit_loc, bigint_literal) as other), expr)
      when Flow_ast_utils.is_bigint_literal bigint_literal ->
      let raw = extract_bigint_literal bigint_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonBigIntR { loc = lit_loc; sense; lit = raw })
        other
    | (expr, ((lit_loc, bigint_literal) as other))
      when Flow_ast_utils.is_bigint_literal bigint_literal ->
      let raw = extract_bigint_literal bigint_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonBigIntR { loc = lit_loc; sense; lit = raw })
        other
    (* expr op null *)
    | (((_, Expression.NullLiteral _) as other), expr)
    | (expr, ((_, Expression.NullLiteral _) as other)) ->
      on_null_test ~sense ~strict loc expr other
    (* expr op undefined *)
    | ( ( (_, Expression.Identifier (_, { Ast.Identifier.name = "undefined"; comments = _ })) as
        undefined
        ),
        expr
      )
    | ( expr,
        ( (_, Expression.Identifier (_, { Ast.Identifier.name = "undefined"; comments = _ })) as
        undefined
        )
      ) ->
      on_void_test ~sense ~strict ~check_for_bound_undefined:true loc expr undefined
    (* expr op void(...) *)
    | ( ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }) as other),
        expr
      )
    | ( expr,
        ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }) as other)
      ) ->
      on_void_test ~sense ~strict ~check_for_bound_undefined:false loc expr other
    | (((_, Expression.Member _) as expr), other) -> on_member_eq_other expr other
    | (other, ((_, Expression.Member _) as expr)) when not is_switch_cond_context ->
      on_other_eq_member other expr
    | _ -> on_other_eq_test left right
end
