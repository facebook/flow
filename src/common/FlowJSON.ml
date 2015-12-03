open Utils

module Ast = Spider_monkey_ast

let parse json filename =
  Parser_flow.json_file ~fail:false json (Some filename)

let parse_object json_obj filename =
  let open Ast in
  let open Expression.Object in
  let ((_, {properties}), errors) =
    parse json_obj filename in
  let extract_property map = function
    | Property(_, {
        Property.key = Property.Literal(_, {Literal.raw; _;});
        value;
        _;
      }) -> SMap.add raw value map
    | _ -> SMap.empty
  in
  let prop_map = List.fold_left extract_property SMap.empty properties in
  (prop_map, errors)
