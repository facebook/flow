open Utils

module Ast = Spider_monkey_ast

let parse json filename =
  let (ast, errors) = Parser_flow.program_file
    ~fail:false
    (spf "(%s)" json)
    (Some filename)
  in
  let ast_expr = Ast.(match ast with
    | (_, [(_, Statement.Expression({
        Statement.Expression.expression;
      }))], _) -> Some(expression)
    | _ -> None
  ) in
  (ast_expr, errors)

let parse_object json_obj filename =
  let (ast, errors) = parse json_obj filename in
  let ast = Ast.(Expression.(match ast with
    | Some(_, Object({Object.properties})) ->
      let extract_property map = Expression.Object.(function
        | Property(_, {
            Property.key = Property.Literal(_, {Literal.raw; _;});
            value;
            _;
          }) -> SMap.add raw value map
        | _ -> SMap.empty
      ) in
      List.fold_left extract_property SMap.empty properties

    | _ -> SMap.empty
  )) in
  (ast, errors)
