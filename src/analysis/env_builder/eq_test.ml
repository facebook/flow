(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  val visit_eq_test :
    on_type_of_test:
      (ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      string ->
      bool ->
      'a
      ) ->
    on_literal_test:
      (strict:bool ->
      sense:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      Env_api.Refi.refinement_kind ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_null_test:
      (sense:bool ->
      strict:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_void_test:
      (sense:bool ->
      strict:bool ->
      check_for_bound_undefined:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_member_eq_other:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    on_other_eq_member:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    is_switch_cond_context:bool ->
    on_other_eq_test:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    strict:bool ->
    sense:bool ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
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
  open Env_api.Refi

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
    let open Flow_ast in
    match (left, right) with
    (* typeof expr ==/=== string *)
    | ( ( _,
          Expression.Unary
            { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
        ),
        ((_, Expression.Literal { Literal.value = Literal.String s; _ }) as other)
      )
    | ( ((_, Expression.Literal { Literal.value = Literal.String s; _ }) as other),
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
    | (((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }) as other), expr)
    | (expr, ((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }) as other)) ->
      on_literal_test ~strict ~sense loc expr (SingletonBoolR { loc = lit_loc; sense; lit }) other
    (* string equality *)
    | (((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }) as other), expr)
    | (expr, ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }) as other))
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
    | (((lit_loc, number_literal) as other), expr) when is_number_literal number_literal ->
      let raw = extract_number_literal number_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonNumR { loc = lit_loc; sense; lit = raw })
        other
    | (expr, ((lit_loc, number_literal) as other)) when is_number_literal number_literal ->
      let raw = extract_number_literal number_literal in
      on_literal_test
        ~strict
        ~sense
        loc
        expr
        (SingletonNumR { loc = lit_loc; sense; lit = raw })
        other
    (* expr op null *)
    | (((_, Expression.Literal { Literal.value = Literal.Null; _ }) as other), expr)
    | (expr, ((_, Expression.Literal { Literal.value = Literal.Null; _ }) as other)) ->
      on_null_test ~sense ~strict loc expr other
    (* expr op undefined *)
    | ( ( (_, Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ }))
        as undefined
        ),
        expr
      )
    | ( expr,
        ( (_, Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ }))
        as undefined
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
