(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Literals = struct
  open Ast.Literal

  let string value =
    { value = String value; raw = Printf.sprintf "%S" value; }

  let number value raw =
    { value = Number value; raw; }

  let bool is_true =
    { value = Boolean is_true; raw = if is_true then "true" else "false" }
end

module Patterns = struct
  open Ast.Pattern

  let identifier str =
    Loc.none, Identifier { Identifier.
      name = Loc.none, str;
      annot = None;
      optional = false;
    }

  let array str =
    Loc.none, Array { Array.
      elements = [Some (Array.Element (identifier str))];
      annot = None;
    }

  let assignment str expr =
    Loc.none, Assignment { Assignment.
      left = identifier str;
      right = expr;
    }

  let object_ str =
    let open Object in
    Loc.none, Object {
      properties = [Property (Loc.none, { Property.
        key = Property.Identifier (Loc.none, str);
        pattern = identifier str;
        shorthand = true;
      })];
      annot = None;
    }
end

module Functions = struct
  open Ast.Function

  let body_block stmts =
    BodyBlock (Loc.none, { Ast.Statement.Block.
      body = stmts;
    })

  let body_expression expr =
    BodyExpression expr

  let make ~id ~expression ~params ?(generator=false) ?(async=false) ?body () =
    let body = match body with
    | Some body -> body
    | None -> body_block []
    in
    { id;
      params = Loc.none, { Ast.Function.Params.params; rest = None };
      body;
      async;
      generator = generator;
      predicate = None;
      expression;
      return = None;
      tparams = None;
    }
end

module Classes = struct
  open Ast.Class

  (* TODO: add method_ and property *)
  let make ?super ?id elements =
    { id;
      body = Loc.none, { Body.body = elements };
      tparams = None;
      super;
      super_targs = None;
      implements = [];
      classDecorators = [];
    }
end

module JSXs = struct
  open Ast.JSX

  let identifier name = Identifier (Loc.none, { Identifier.name })

  let attr_identifier name = Attribute.Identifier (Loc.none, { Identifier.name })

  let attr_literal lit = Attribute.Literal (Loc.none, lit)

  let attr name value = Opening.Attribute (Loc.none, { Attribute.name; value })

  let element ?selfclosing:(selfClosing=false) ?attrs:(attributes=[]) ?(children=[]) name =
    { openingElement = Loc.none, { Opening.name; selfClosing; attributes };
      closingElement = if selfClosing then None else Some (Loc.none, { Closing.name });
      children;
    }

  let child_element ?selfclosing ?attrs ?children name =
    Loc.none, Element (element ?selfclosing ?attrs ?children name)
end

module Statements = struct
  open Ast.Statement

  let empty () = Loc.none, Empty

  let block children =
    Loc.none, Block { Block.body = children }

  let while_ test body =
    Loc.none, While { While.test; body }

  let do_while body test =
    Loc.none, DoWhile { DoWhile.body; test }

  let for_ init test update body =
    Loc.none, For { For.
      init = Some (For.InitExpression init);
      test;
      update;
      body;
    }

  let expression ?directive expression =
    Loc.none, Expression { Expression.expression; directive }

  let labeled label body =
    Loc.none, Labeled { Labeled.label; body }

  let variable_declarator_generic id init =
    Loc.none, { VariableDeclaration.Declarator.
      id;
      init;
    }

  let variable_declarator ?init str =
    Loc.none, { VariableDeclaration.Declarator.
      id = Patterns.identifier str;
      init;
    }

  let variable_declaration
      ?(kind = Ast.Statement.VariableDeclaration.Var)
      declarations =
    Loc.none, VariableDeclaration { VariableDeclaration.kind; declarations; }

  let function_declaration ?(loc=Loc.none) ?(params=[]) ?body id =
    let body = match body with
    | Some stmts -> Some (Functions.body_block stmts)
    | None -> None
    in
    let fn = Functions.make ~params ~id:(Some id) ?body ~expression:false () in
    loc, FunctionDeclaration fn

  let class_declaration ?super ?id elements =
    Loc.none, ClassDeclaration (Classes.make ?super ?id elements)

  let if_ test consequent alternate =
    Loc.none, If { If.test; consequent; alternate }

  let return expr =
    Loc.none, Return { Return.argument = expr }

  let directive txt =
    let expr = Loc.none, Ast.Expression.Literal (Literals.string txt) in
    expression ~directive:txt expr
end

module Expressions = struct
  open Ast.Expression

  let identifier name =
    Loc.none, Identifier (Loc.none, name)

  let call_node ?(args=[]) callee = { Call.callee; arguments = args }

  let call ?(args=[]) callee =
    Loc.none, Call (call_node ~args callee)

  let optional_call ~optional ?(args=[]) callee =
    Loc.none, OptionalCall { OptionalCall.
      call = call_node ~args callee;
      optional;
    }

  let function_ ?(generator=false) ?(params=[]) ?body () =
    let fn = Functions.make ~generator ~params ~id:None ?body ~expression:true () in
    Loc.none, Function fn

  let arrow_function ?(params=[]) ?body () =
    let fn = Functions.make ~params ~id:None ?body ~expression:true () in
    Loc.none, ArrowFunction fn

  let class_ ?super ?id elements =
    Loc.none, Class (Classes.make ?super ?id elements)

  let literal lit =
    Loc.none, Literal lit

  let assignment left ?(operator=Ast.Expression.Assignment.Assign) right =
    Loc.none, Assignment { Assignment.operator; left; right; }

  let binary ~op left right =
    Loc.none, Binary { Binary.operator = op; left; right }

  let conditional test consequent alternate =
    Loc.none, Conditional { Conditional.test; consequent; alternate }

  let logical ~op left right =
    Loc.none, Logical { Logical.operator = op; left; right }

  let unary ~op argument =
    Loc.none, Unary { Unary.
      operator = op;
      prefix = true;
      argument;
    }

  let update ~op ~prefix argument =
    Loc.none, Update { Update.
      operator = op;
      prefix;
      argument;
    }

  let object_property_key k =
    Object.Property.Identifier (Loc.none, k)

  let object_property_key_literal k =
    Object.Property.Literal (Loc.none, k)

  let object_method ?body ?(params=[]) ?(generator=false) ?(async=false) key =
    let fn = Functions.make ~id:None ~expression:true ~params ~generator ~async ?body () in
    let prop = Object.Property.Method { key; value = (Loc.none, fn) } in
    Object.Property (Loc.none, prop)

  let object_property ?(shorthand=false) key value =
    let module Prop = Object.Property in
    let prop = Prop.Init { key; value; shorthand } in
    Object.Property (Loc.none, prop)

  let object_property_with_literal ?(shorthand=false) k v =
    object_property ~shorthand (object_property_key_literal k) v

  let object_ properties =
    Loc.none, Object { Object.properties }

  (* _object.property *)
  let member ~property _object =
    { Member.
      _object;
      property = Member.PropertyIdentifier (Loc.none, property);
      computed = false;
    }

  (* _object[property] *)
  let member_computed ~property _object =
    { Member.
      _object;
      property = Member.PropertyIdentifier (Loc.none, property);
      computed = true;
    }

  let member_computed_expr ~property _object =
    { Member.
      _object;
      property = Member.PropertyExpression property;
      computed = true;
    }

  let member_expression expr =
    Loc.none, Ast.Expression.Member expr

  let optional_member_expression ~optional expr =
    Loc.none, OptionalMember { OptionalMember.
      member = expr;
      optional;
    }

  let new_ ?(args=[]) callee =
    Loc.none, New { New.callee; arguments = args }

  let sequence exprs =
    Loc.none, Sequence { Sequence.expressions = exprs }

  let spread expr =
    Spread (Loc.none, { SpreadElement.argument = expr })

  let jsx_element elem =
    Loc.none, JSXElement elem

  let true_ () =
    literal (Literals.bool true)

  let false_ () =
    literal (Literals.bool false)

  let logical_and (l: Loc.t Ast.Expression.t) r = logical ~op:Logical.And l r
  let logical_or (l: Loc.t Ast.Expression.t) r = logical ~op:Logical.Or l r
  let unary_not (b: Loc.t Ast.Expression.t) = unary ~op:Unary.Not b
end

let mk_program stmts =
  Loc.none, stmts, []

let ast_of_string ~parser str =
  let parse_options = Some Parser_env.({
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    esproposal_optional_chaining = true;
    esproposal_nullish_coalescing = true;
    types = true;
    use_strict = false;
  }) in
  let env = Parser_env.init_env ~token_sink:None ~parse_options None str in
  let (ast, _) = Parser_flow.do_parse
    env parser false in
  ast

let expression_of_string str =
  ast_of_string ~parser:Parser_flow.Parse.expression str

let statement_of_string str =
  let ast_list = ast_of_string
    ~parser:(Parser_flow.Parse.module_body ~term_fn:(fun _ -> false)) str in
  match ast_list with
  | [ast] -> ast
  | _ -> failwith "Multiple statements found"
