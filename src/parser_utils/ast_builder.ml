(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module Identifiers = struct
  let identifier ?(loc=Loc.none) name = Flow_ast_utils.ident_of_source (loc, name)
end

module Types = struct
  module Functions = struct
    let params ?(loc=Loc.none) ?rest params = loc, { Ast.Type.Function.Params.params; rest }
    let make ?tparams params return = { Ast.Type.Function.tparams; params; return }
  end

  module Objects = struct
    let make ?(exact=true) ?(inexact=false) properties =
      { Ast.Type.Object.exact; inexact; properties }

    let property
        ?(loc=Loc.none)
        ?(optional=false)
        ?(static=false)
        ?(proto=false)
        ?(_method=false)
        ?(variance=None)
        key
        value
    =
      loc, { Ast.Type.Object.Property.key; value; optional; static; proto; _method; variance }

    let getter ?(loc=Loc.none) ?optional ?static ?proto ?_method ?variance key func =
      let value = Ast.Type.Object.Property.Get (loc, func) in
      let prop = property ~loc ?optional ?static ?proto ?_method ?variance key value in
      Ast.Type.Object.Property prop

    let setter ?(loc=Loc.none) ?optional ?static ?proto ?_method ?variance key func =
      let value = Ast.Type.Object.Property.Set (loc, func) in
      let prop = property ~loc ?optional ?static ?proto ?_method ?variance key value in
      Ast.Type.Object.Property prop
  end

  let mixed = Loc.none, Ast.Type.Mixed
  let annotation t = Loc.none, t

  let object_ ?(loc=Loc.none) ?exact ?inexact properties =
    loc, Ast.Type.Object (Objects.make ?exact ?inexact properties)
end

let string_literal value =
  {Ast.StringLiteral.value; raw = Printf.sprintf "%S" value}

let number_literal value raw =
  {Ast.NumberLiteral.value; raw}

module Literals = struct
  open Ast.Literal

  let string ?(comments=None) value =
    { value = String value; raw = Printf.sprintf "%S" value; comments }

  let number ?(comments=None) value raw =
    { value = Number value; raw; comments }

  let int value =
    number (float_of_int value) (Printf.sprintf "%d" value)

  let bool ?(comments=None) is_true =
    { value = Boolean is_true; raw = if is_true then "true" else "false"; comments }
end

module Patterns = struct
  open Ast.Pattern

  let identifier ?(loc=Loc.none) str =
    loc, Identifier { Identifier.
      name = Flow_ast_utils.ident_of_source (loc, str);
      annot = Ast.Type.Missing loc;
      optional = false;
    }

  let array elements =
    let elements = Core_list.map ~f:(function
      | Some i -> Some (Array.Element (Loc.none, { Array.Element.argument = i; default = None }))
      | None -> None
    ) elements in
    Loc.none, Array { Array.
      elements;
      annot = Ast.Type.Missing Loc.none;
    }

  let object_ str =
    let open Object in
    Loc.none, Object {
      properties = [Property (Loc.none, { Property.
        key = Property.Identifier (Flow_ast_utils.ident_of_source (Loc.none, str));
        pattern = identifier str;
        default = None;
        shorthand = true;
      })];
      annot = Ast.Type.Missing Loc.none;
    }
end

module Functions = struct
  open Ast.Function

  let params ?(loc=Loc.none) ?rest ps =
    loc, { Ast.Function.Params.params = ps; rest }

  let param ?(loc=Loc.none) ?default argument =
    loc, { Ast.Function.Param.argument; default }

  let body ?(loc=Loc.none) stmts =
    BodyBlock (loc, { Ast.Statement.Block.
      body = stmts;
    })

  let body_expression expr =
    BodyExpression expr

  let make ~id ?params:params_ ?(generator=false) ?(async=false) ?body:body_ () =
    let params = match params_ with
    | Some ps -> ps
    | None -> params []
    in
    let body = match body_ with
    | Some body_ -> body_
    | None -> body []
    in
    { id;
      params;
      body;
      async;
      generator;
      predicate = None;
      return = Ast.Type.Missing Loc.none;
      tparams = None;
      sig_loc = Loc.none;
    }
end

module Classes = struct
  open Ast.Class

  let implements ?targs id =
    Loc.none, { Implements.id; targs; }

  (* TODO: add method_ and property *)
  let make ?super ?(implements = []) ?id elements =
    let extends = match super with
    | None -> None
    | Some expr -> Some (Loc.none, { Extends.expr; targs = None })
    in
    { id;
      body = Loc.none, { Body.body = elements };
      tparams = None;
      extends;
      implements;
      classDecorators = [];
    }
end

module JSXs = struct
  open Ast.JSX

  let identifier name = Identifier (Loc.none, { Identifier.name })

  let attr_identifier ?(loc=Loc.none) name = Attribute.Identifier (loc, { Identifier.name })

  let attr_literal lit = Attribute.Literal (Loc.none, lit)

  let attr ?(loc=Loc.none) name value = Opening.Attribute (loc, { Attribute.name; value })

  let element ?selfclosing:(selfClosing=false) ?attrs:(attributes=[]) ?(children=[]) name =
    { openingElement = Loc.none, { Opening.name; selfClosing; attributes };
      closingElement = if selfClosing then None else Some (Loc.none, { Closing.name });
      children = (Loc.none, children);
    }

  let child_element ?(loc=Loc.none) ?selfclosing ?attrs ?children name =
    loc, Element (element ?selfclosing ?attrs ?children name)
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

  let for_in ?(each=false) left right body =
    Loc.none, ForIn { ForIn.left; right; body; each }

  let for_in_declarator ?(kind = Ast.Statement.VariableDeclaration.Var) declarations =
    ForIn.LeftDeclaration (Loc.none, { VariableDeclaration.declarations; kind })

  let for_in_pattern patt =
    ForIn.LeftPattern patt

  let for_of ?(async=false) left right body =
    Loc.none, ForOf { ForOf.left; right; body; async }

  let for_of_declarator ?(kind = Ast.Statement.VariableDeclaration.Var) declarations =
    ForOf.LeftDeclaration (Loc.none, { VariableDeclaration.declarations; kind })

  let for_of_pattern patt =
    ForOf.LeftPattern patt

  let expression ?(loc = Loc.none) ?directive expression =
    loc, Expression { Expression.expression; directive }

  let labeled label body =
    Loc.none, Labeled { Labeled.label; body }

  let variable_declarator_generic id init =
    Loc.none, { VariableDeclaration.Declarator.
      id;
      init;
    }

  let variable_declarator ?init ?(loc=Loc.none) str =
    loc, { VariableDeclaration.Declarator.
      id = Patterns.identifier ~loc str;
      init;
    }

  let variable_declaration
      ?(kind = Ast.Statement.VariableDeclaration.Var)
      ?(loc=Loc.none)
      declarations =
    loc, VariableDeclaration { VariableDeclaration.kind; declarations; }

  let let_declaration declarations =
    variable_declaration ~kind:Ast.Statement.VariableDeclaration.Let declarations

  let const_declaration declarations =
    variable_declaration ~kind:Ast.Statement.VariableDeclaration.Const declarations

  let function_declaration
    ?(loc=Loc.none) ?(async=false) ?(generator=false) ?params ?body
    id
  =
    let fn = Functions.make ?params ~id:(Some id) ~async ~generator ?body () in
    loc, FunctionDeclaration fn

  let class_declaration ?super ?implements ?id elements =
    Loc.none, ClassDeclaration (Classes.make ?super ?implements ?id elements)

  let if_ test consequent alternate =
    Loc.none, If { If.test; consequent; alternate }

  let return ?(loc=Loc.none) ?comments expr =
    loc, Return { Return.argument = expr; comments }

  let directive txt =
    let expr = Loc.none, Ast.Expression.Literal (Literals.string txt) in
    expression ~directive:txt expr

  let switch discriminant cases =
    Loc.none, Switch { Switch.discriminant; cases }

  let switch_case ?(loc=Loc.none) ?test consequent =
    loc, { Switch.Case.test; consequent }

  let break ?label () =
    Loc.none, Break { Break.label }

  let with_ _object body =
    Loc.none, With { With._object; body }

  let enum_declaration ?(loc=Loc.none) id body =
    loc, EnumDeclaration {EnumDeclaration.id; body}

  module EnumDeclarations = struct
    open EnumDeclaration

    let initialized_member ?(loc=Loc.none) id init_value =
      loc, {InitializedMember.id; init = Loc.none, init_value}

    let defaulted_member ?(loc=Loc.none) id =
      loc, {DefaultedMember.id}

    let boolean_body ?(explicit_type=false) members =
      BooleanBody {BooleanBody.members; explicitType = explicit_type}

    let number_body ?(explicit_type=false) members =
      NumberBody {NumberBody.members; explicitType = explicit_type}

    let string_defaulted_body ?(explicit_type=false) members =
      let members = StringBody.Defaulted members in
      StringBody {StringBody.members; explicitType = explicit_type}

    let string_initialized_body ?(explicit_type=false) members =
      let members = StringBody.Initialized members in
      StringBody {StringBody.members; explicitType = explicit_type}

    let symbol_body members =
      SymbolBody {SymbolBody.members}
  end
end

module Expressions = struct
  open Ast.Expression

  let identifier ?(loc=Loc.none) ?(comments=None) name =
    loc, Identifier (loc, { Ast.Identifier.name; comments })

  let array ?comments elements =
    Loc.none, Array { Array.elements; comments }

  let call_node ?targs ?(args=[]) callee = { Call.callee; targs; arguments = args }

  let call ?(args=[]) callee =
    Loc.none, Call (call_node ~args callee)

  let optional_call ~optional ?(args=[]) callee =
    Loc.none, OptionalCall { OptionalCall.
      call = call_node ~args callee;
      optional;
    }

  let function_ ?(loc=Loc.none) ?(async=false) ?(generator=false) ?params ?id ?body () =
    let fn = Functions.make ~async ~generator ?params ~id ?body () in
    loc, Function fn

  let arrow_function ?(loc=Loc.none) ?(async=false) ?params ?body () =
    let fn = Functions.make ~async ~generator:false ?params ~id:None ?body () in
    loc, ArrowFunction fn

  let class_ ?super ?id elements =
    Loc.none, Class (Classes.make ?super ?id elements)

  let literal ?(loc=Loc.none) lit =
    loc, Literal lit

  let assignment left ?operator right =
    Loc.none, Assignment { Assignment.operator; left; right; }

  let binary ~op left right =
    Loc.none, Binary { Binary.operator = op; left; right }

  let plus left right = binary ~op:Binary.Plus left right
  let minus left right = binary ~op:Binary.Minus left right
  let mult left right = binary ~op:Binary.Mult left right
  let instanceof left right = binary ~op:Binary.Instanceof left right
  let in_ left right = binary ~op:Binary.In left right
  let equal left right = binary ~op:Binary.Equal left right

  let conditional test consequent alternate =
    Loc.none, Conditional { Conditional.test; consequent; alternate }

  let logical ~op left right =
    Loc.none, Logical { Logical.operator = op; left; right }

  let unary ?(comments=None) ~op argument =
    Loc.none, Unary { Unary.
      operator = op;
      argument;
      comments;
    }

  let unary_plus (b: (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Plus b
  let unary_minus (b: (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Minus b
  let unary_not (b: (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Not b

  let update ~op ~prefix argument =
    Loc.none, Update { Update.
      operator = op;
      prefix;
      argument;
    }

  let increment ~prefix argument =
    update ~op:Update.Increment ~prefix argument

  let decrement ~prefix argument =
    update ~op:Update.Decrement ~prefix argument

  let object_property_key ?(loc=Loc.none) (k: string) =
    Object.Property.Identifier (Flow_ast_utils.ident_of_source(loc, k))

  let object_property_key_literal ?(loc=Loc.none) k =
    Object.Property.Literal (loc, k)

  let object_property_key_literal_from_string ?(loc=Loc.none) (k: string) =
    Object.Property.Literal (loc, Literals.string k)

  let object_property_computed_key k =
    Object.Property.Computed k

  let object_method ?body ?params ?(generator=false) ?(async=false) key =
    let fn = Functions.make ~id:None ?params ~generator ~async ?body () in
    let prop = Object.Property.Method { key; value = (Loc.none, fn) } in
    Object.Property (Loc.none, prop)

  let object_property ?(shorthand=false) ?(loc=Loc.none) key value =
    let module Prop = Object.Property in
    let prop = Prop.Init { key; value; shorthand } in
    Object.Property (loc, prop)

  let object_property_with_literal ?(shorthand=false) ?(loc=Loc.none) k v =
    object_property ~shorthand ~loc (object_property_key_literal ~loc k) v

  let object_ ?comments ?(loc=Loc.none) properties =
    loc, Object { Object.properties; comments }

  (* _object.property *)
  let member ~property _object =
    { Member.
      _object;
      property = Member.PropertyIdentifier (Flow_ast_utils.ident_of_source(Loc.none, property));
    }

  (* _object[property] *)
  let member_computed ~property _object =
    { Member.
      _object;
      property = Member.PropertyExpression property;
    }

  let member_expression expr =
    Loc.none, Ast.Expression.Member expr

  let member_expression_ident_by_name obj (name: string) =
    member_expression (member obj ~property: name)

  let member_expression_computed_string obj (str: string) =
    member_expression (member_computed obj
      ~property:(literal (Literals.string str)))

  let optional_member_expression ~optional expr =
    Loc.none, OptionalMember { OptionalMember.
      member = expr;
      optional;
    }

  let new_ ?targs ?(args=[]) callee =
    Loc.none, New { New.callee; targs; arguments = args }

  let sequence exprs =
    Loc.none, Sequence { Sequence.expressions = exprs }

  let expression expr = Expression expr

  let spread expr =
    Spread (Loc.none, { SpreadElement.argument = expr })

  let jsx_element ?(loc=Loc.none) elem =
    loc, JSXElement elem

  let true_ () =
    literal (Literals.bool true)

  let false_ () =
    literal (Literals.bool false)

  let logical_and (l: (Loc.t, Loc.t) Ast.Expression.t) r = logical ~op:Logical.And l r
  let logical_or (l: (Loc.t, Loc.t) Ast.Expression.t) r = logical ~op:Logical.Or l r

  let typecast expression annotation =
    Loc.none, TypeCast { TypeCast.
      expression;
      annot = Types.annotation annotation;
    }

  module Literals = struct
    let string ?loc value = literal ?loc (Literals.string value)
    let number ?loc value raw = literal ?loc (Literals.number value raw)
    let int ?loc value = literal ?loc (Literals.int value)
    let bool ?loc is_true = literal ?loc (Literals.bool is_true)
  end
end

module Comments = struct
  let block ?(loc = Loc.none) txt = loc, Ast.Comment.Block txt
  let line ?(loc = Loc.none) txt = loc, Ast.Comment.Line txt
end

let mk_program ?(comments=[]) stmts =
  Loc.none, stmts, comments

let ast_of_string ~parser str =
  let parse_options = Some Parser_env.({
    enums = true;
    esproposal_class_instance_fields = true;
    esproposal_class_static_fields = true;
    esproposal_decorators = true;
    esproposal_export_star_as = true;
    esproposal_optional_chaining = true;
    esproposal_nullish_coalescing = true;
    esproposal_fsharp_pipeline_operator = true;
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

let program_of_string str =
  let stmts = ast_of_string
    ~parser:(Parser_flow.Parse.module_body ~term_fn:(fun _ -> false)) str in
  (Loc.none, stmts, [])
