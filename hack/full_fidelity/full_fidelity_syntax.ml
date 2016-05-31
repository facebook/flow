(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(**
 * This module contains the code describing the structure of a syntax tree.
 *
 * The relationships between the various functors and signatures here needs
 * some explanation.
 *
 * First off, the structure of the syntax tree is described by the collection
 * of recursive types that makes up the bulk of this file. The type "t" is
 * the type of a node in the syntax tree; each node has associated with it
 * an arbitrary value of type SyntaxValue.t, and syntax node proper, which
 * has structure given by the "syntax" type.
 *
 * Note that every child in the syntax tree is of type t, except for the
 * "Token" type. This should be the *only* child of a type other than t.
 * We are explicitly NOT attempting to impose a type structure on the parse
 * tree beyond what is already implied by the types here. For example,
 * we are not attempting to put into the type system here the restriction that
 * the children of a binary operator must be expressions. The reason for this
 * is because we are potentially parsing code as it is being typed, and we
 * do not want to restrict our ability to make good error recovery by imposing
 * a restriction that will only be valid in correct program text.
 *
 * That said, it would of course be ideal if the only children of a compound
 * statement were statements, and so on. But those invariants should be
 * imposed by the design of the parser, not by the type system of the syntax
 * tree code.
 *
 * We want to be able to use different kinds of tokens, with different
 * performance characteristics. Therefore this module is first functorized by
 * token type.
 *
 * We wish to associate arbitrary values with the syntax nodes so that we can
 * construct syntax trees with various properties -- trees that only know
 * their widths and are thereby cheap to serialize, trees that have full
 * position data for each node, trees where the tokens know their text and
 * can therefore be edited, trees that have name annotations or type
 * annotations, and so on.
 *
 * Therefore this module is functorized again with the value type to be
 * associated with the node.
 *
 * We also wish to provide factory methods, so that nodes can be built up
 * from their child nodes. A factory method must not just know all the
 * children and the kind of node it is constructing; it also must know how
 * to construct the value that this node is going to be tagged with. For
 * that reason, a third, optional functor is provided. This functor requires
 * that methods be provided to construct the values associated with a token
 * or with any arbitrary node, given its children. If this functor is used
 * then the resulting module contains factory methods.
 *
 * This module also provides some useful helper functions, like an iterator,
 * a rewriting visitor, and so on.
 *
 *)

module SyntaxKind = Full_fidelity_syntax_kind
module Operator = Full_fidelity_operator

module type TokenType = sig
  type t
  val kind: t -> Full_fidelity_token_kind.t
end

module type SyntaxValueType = sig
  type t
end

(* These functors describe the shape of a parse tree that has a particular
   kind of token in the leaves, and a particular kind of value associated
   with each node. *)
module WithToken(Token: TokenType) = struct
  module WithSyntaxValue(SyntaxValue: SyntaxValueType) = struct

    type script_header = {
      header_less_than : t;
      header_question : t;
      header_language : t
    }
    and script = {
      script_header : t;
      script_declarations : t
    }
    and function_declaration = {
      function_attr : t;
      function_async : t;
      function_token : t;
      function_name : t;
      function_type_params : t;
      function_left_paren : t;
      function_params : t;
      function_right_paren : t;
      function_colon : t;
      function_type : t;
      function_body : t
    }
    and parameter_declaration = {
      param_attr : t;
      param_type : t;
      param_name : t;
      param_default : t
    }
    and default_argument_specifier = {
      default_equal : t;
      default_value : t
    }
    and compound_statement = {
      compound_left_brace : t;
      compound_statements : t;
      compound_right_brace : t
    }
    and expression_statement = {
      expr_statement_expr : t;
      expr_statement_semicolon : t
    }
    and while_statement = {
      while_keyword : t;
      while_left_paren : t;
      while_condition_expr : t;
      while_right_paren: t;
      while_body: t
    }
    and if_statement = {
      if_keyword: t;
      if_left_paren: t;
      if_condition_expr: t;
      if_right_paren: t;
      if_statement: t;
      if_elseif_clauses: t;
      if_else_clause: t;
    }
    and elseif_clause ={
      elseif_keyword: t;
      elseif_left_paren: t;
      elseif_condition_expr: t;
      elseif_right_paren: t;
      elseif_statement: t
    }
    and else_clause = {
      else_keyword: t;
      else_statement: t;
    }
    and do_statement = {
      do_keyword: t;
      do_statement: t;
      do_while_keyword: t;
      do_left_paren: t;
      do_condition_expr: t;
      do_right_paren: t;
      do_semicolon: t
    }
    and switch_statement = {
      switch_keyword: t;
      switch_left_paren: t;
      switch_expr: t;
      switch_right_paren: t;
      switch_compound_statement: t
    }
    and unary_operator = {
      unary_operator : t;
      unary_operand : t
    }
    and binary_operator = {
      binary_left_operand : t;
      binary_operator : t;
      binary_right_operand : t
    }
    and parenthesized_expression = {
      paren_expr_left_paren : t;
      paren_expr : t;
      paren_expr_right_paren : t
    }
    and braced_expression = {
      braced_expr_left_brace : t;
      braced_expr : t;
      braced_expr_right_brace : t
    }
    and xhp_attribute = {
      xhp_attr_name : t;
      xhp_attr_equal : t;
      xhp_attr_expr : t;
    }
    and xhp_open = {
      xhp_open_name : t;
      xhp_open_attrs : t;
      xhp_open_right_angle : t;
    }
    and xhp_embedded_expression = {
      xhp_embed_open_brace : t;
      xhp_embed_expr : t;
      xhp_embed_close_brace : t;
    }
    and xhp_expression = {
      xhp_open : t;
      xhp_body : t;
      xhp_close : t
    }
    and xhp_close = {
      xhp_close_left_angle : t;
      xhp_close_name : t;
      xhp_close_right_angle : t;
    }
    and type_constant = {
      type_constant_left_type : t;
      type_constant_separator : t;
      type_constant_right_type : t
    }
    and generic_type = {
      generic_class_type : t;
      generic_arguments : t
    }
    and type_arguments = {
      type_arguments_left_angle : t;
      type_arguments : t;
      type_arguments_right_angle : t
    }
    and syntax =
    | Token of Token.t
    | Error of t list
    | Missing
    | SyntaxList of t list
    | ScriptHeader of script_header
    | Script of script

    | FunctionDeclaration of function_declaration
    | ParameterDeclaration of parameter_declaration
    | DefaultArgumentSpecifier of default_argument_specifier

    | CompoundStatement of compound_statement
    | ExpressionStatement of expression_statement
    | WhileStatement of while_statement
    | IfStatement of if_statement
    | ElseifClause of elseif_clause
    | ElseClause of else_clause
    | DoStatement of do_statement
    | SwitchStatement of switch_statement

    | LiteralExpression of t
    | VariableExpression of t
    | QualifiedNameExpression of t
    | PrefixUnaryOperator of unary_operator
    | PostfixUnaryOperator of unary_operator
    | BinaryOperator of binary_operator
    | ParenthesizedExpression of parenthesized_expression
    | BracedExpression of braced_expression
    | XHPExpression of xhp_expression
    | XHPOpen of xhp_open
    | XHPAttribute of xhp_attribute

    | SimpleTypeSpecifier of t
    | TypeConstant of type_constant
    | GenericTypeSpecifier of generic_type
    | TypeArguments of type_arguments

    and t = { syntax : syntax ; value : SyntaxValue.t}

    let make syntax value =
      { syntax; value }

    let syntax node =
      node.syntax

    let value node =
      node.value

    let to_kind syntax =
      match syntax with
      | Missing -> SyntaxKind.Missing
      | Token _  -> SyntaxKind.Token
      | LiteralExpression _ -> SyntaxKind.LiteralExpression
      | VariableExpression _ -> SyntaxKind.VariableExpression
      | QualifiedNameExpression _ -> SyntaxKind.QualifiedNameExpression
      | Error _ -> SyntaxKind.Error
      | SyntaxList _ -> SyntaxKind.SyntaxList
      | ScriptHeader _ -> SyntaxKind.ScriptHeader
      | Script _ -> SyntaxKind.Script
      | FunctionDeclaration _ -> SyntaxKind.FunctionDeclaration
      | ParameterDeclaration _ -> SyntaxKind.ParameterDeclaration
      | DefaultArgumentSpecifier _ -> SyntaxKind.DefaultArgumentSpecifier
      | CompoundStatement _ -> SyntaxKind.CompoundStatement
      | ExpressionStatement _ -> SyntaxKind.ExpressionStatement
      | WhileStatement _ -> SyntaxKind.WhileStatement
      | IfStatement _ -> SyntaxKind.IfStatement
      | ElseifClause _ -> SyntaxKind.ElseifClause
      | ElseClause _ -> SyntaxKind.ElseClause
      | DoStatement _ -> SyntaxKind.DoStatement
      | SwitchStatement _ -> SyntaxKind.SwitchStatement
      | PrefixUnaryOperator _ -> SyntaxKind.PrefixUnaryOperator
      | PostfixUnaryOperator _ -> SyntaxKind.PostfixUnaryOperator
      | BinaryOperator _ -> SyntaxKind.BinaryOperator
      | ParenthesizedExpression _ -> SyntaxKind.ParenthesizedExpression
      | BracedExpression _ -> SyntaxKind.BracedExpression
      | XHPExpression _ -> SyntaxKind.XHPExpression
      | XHPOpen _ -> SyntaxKind.XHPOpen
      | XHPAttribute _ -> SyntaxKind.XHPAttribute
      | TypeConstant _ ->  SyntaxKind.TypeConstant
      | SimpleTypeSpecifier _ -> SyntaxKind.SimpleTypeSpecifier
      | GenericTypeSpecifier _ -> SyntaxKind.GenericTypeSpecifier
      | TypeArguments _ -> SyntaxKind.TypeArguments

    let kind node =
      to_kind (syntax node)

    let children node =
      match node.syntax with
      | Missing -> []
      | Token _ -> []
      | LiteralExpression x -> [x]
      | VariableExpression x -> [x]
      | QualifiedNameExpression x -> [x]
      | Error x -> x
      | SyntaxList x -> x
      | ScriptHeader x ->
        [ x.header_less_than; x.header_question; x.header_language ]
      | Script x -> [ x.script_header; x.script_declarations ]
      | FunctionDeclaration x -> [ x.function_attr; x.function_async;
        x.function_token; x.function_name; x.function_type_params;
        x.function_left_paren; x.function_params; x.function_right_paren;
        x.function_colon; x.function_type; x.function_body]
      | ParameterDeclaration x ->
        [ x.param_attr; x.param_type; x.param_name; x.param_default ]
      | DefaultArgumentSpecifier x -> [ x.default_equal; x.default_value ]
      | CompoundStatement x ->
        [ x.compound_left_brace; x.compound_statements; x.compound_right_brace ]
      | ExpressionStatement x -> [ x.expr_statement_expr;
        x.expr_statement_semicolon ]
      | WhileStatement x -> [ x.while_keyword; x.while_left_paren;
        x.while_condition_expr; x.while_right_paren; x.while_body ]
      | IfStatement x -> [ x.if_keyword; x.if_left_paren; x.if_condition_expr;
        x.if_right_paren; x.if_statement; x.if_elseif_clauses;
        x.if_else_clause ]
      | ElseifClause x -> [ x.elseif_keyword; x.elseif_left_paren;
        x.elseif_condition_expr; x.elseif_right_paren; x.elseif_statement ]
      | ElseClause x -> [ x.else_keyword; x.else_statement ]
      | DoStatement x -> [ x.do_keyword; x.do_statement;
      x.do_while_keyword; x.do_left_paren; x.do_condition_expr;
      x.do_right_paren; x.do_semicolon ]
      | SwitchStatement x -> [ x.switch_keyword; x.switch_left_paren;
        x.switch_expr; x.switch_right_paren; x.switch_compound_statement ]
      | PrefixUnaryOperator x -> [ x.unary_operator; x.unary_operand ]
      | PostfixUnaryOperator x -> [ x.unary_operand; x.unary_operator ]
      | BinaryOperator x ->
        [ x.binary_left_operand; x.binary_operator; x.binary_right_operand ]
      | ParenthesizedExpression x ->
        [ x.paren_expr_left_paren; x.paren_expr; x.paren_expr_right_paren ]
      | BracedExpression x ->
        [ x.braced_expr_left_brace; x.braced_expr; x.braced_expr_right_brace ]
      | XHPExpression x ->
        [ x.xhp_open; x.xhp_body; x.xhp_close ]
      | XHPOpen x ->
        [ x.xhp_open_name; x.xhp_open_attrs; x.xhp_open_right_angle ]
      | XHPAttribute x ->
        [ x.xhp_attr_name; x.xhp_attr_equal; x.xhp_attr_expr ]
      | TypeConstant x ->
        [ x.type_constant_left_type; x.type_constant_separator;
        x.type_constant_right_type ]
      | SimpleTypeSpecifier x -> [x]
      | GenericTypeSpecifier x -> [ x.generic_class_type; x.generic_arguments ]
      | TypeArguments x -> [ x.type_arguments_left_angle;
        x.type_arguments; x.type_arguments_right_angle ]

    let script_header x = x.script_header
    let script_declarations x = x.script_declarations
    let header_less_than x = x.header_less_than
    let header_question x = x.header_question
    let header_language x = x.header_language
    let function_attr x = x.function_attr
    let function_async x = x.function_async
    let function_token x = x.function_token
    let function_name x = x.function_name
    let function_type_params x = x.function_type_params
    let function_left_paren x = x.function_left_paren
    let function_params x = x.function_params
    let function_right_paren x = x.function_right_paren
    let function_colon x = x.function_colon
    let function_type x = x.function_type
    let function_body x = x.function_body
    let param_attr x = x.param_attr
    let param_type x = x.param_type
    let param_name x = x.param_name
    let param_default x = x.param_default
    let default_equal x = x.default_equal
    let default_value x = x.default_value
    let compound_left_brace x = x.compound_left_brace
    let compound_statements x = x.compound_statements
    let compound_right_brace x = x.compound_right_brace
    let while_keyword x = x.while_keyword
    let while_left_paren x = x.while_left_paren
    let while_condition_expr x = x.while_condition_expr
    let while_right_paren x = x.while_right_paren
    let while_body x = x.while_body
    let if_keyword x = x.if_keyword
    let if_left_paren x = x.if_left_paren
    let if_condition_expr x = x.if_condition_expr
    let if_right_paren x = x.if_right_paren
    let if_statement x = x.if_statement
    let if_elseif_clauses x = x.if_elseif_clauses
    let if_else_clause x = x.if_else_clause
    let elseif_keyword x = x.elseif_keyword
    let elseif_left_paren x = x.elseif_left_paren
    let elseif_condition_expr x = x.elseif_condition_expr
    let elseif_right_paren x = x.elseif_right_paren
    let elseif_statement x = x.elseif_statement
    let else_keyword x = x.else_keyword
    let else_statement x = x.else_statement
    let do_keyword x = x.do_keyword
    let do_statement x = x.do_statement
    let do_while_keyword x = x.do_while_keyword
    let do_left_paren x = x.do_left_paren
    let do_condition_expr x = x.do_condition_expr
    let do_right_paren x = x.do_right_paren
    let do_semicolon x = x.do_semicolon
    let switch_keyword x = x.switch_keyword
    let switch_left_paren x = x.switch_left_paren
    let switch_expr x = x.switch_expr
    let switch_right_paren x = x.switch_right_paren
    let switch_compound_statement x = x.switch_compound_statement
    let expr_statement_expr x = x.expr_statement_expr
    let expr_statement_semicolon x = x.expr_statement_semicolon
    let unary_operator x = x.unary_operator
    let unary_operand x = x.unary_operand
    let binary_left_operand b = b.binary_left_operand
    let binary_operator b = b.binary_operator
    let binary_right_operand b = b.binary_right_operand
    let paren_expr_left_paren x = x.paren_expr_left_paren
    let paren_expr x = x.paren_expr
    let paren_expr_right_paren x = x.paren_expr_right_paren
    let braced_expr_left_brace x = x.braced_expr_left_brace
    let braced_expr x = x.braced_expr
    let braced_expr_right_brace x = x.braced_expr_right_brace
    let xhp_open x = x.xhp_open
    let xhp_body x = x.xhp_body
    let xhp_close x = x.xhp_close
    let xhp_open_name x = x.xhp_open_name
    let xhp_open_attrs x = x.xhp_open_attrs
    let xhp_open_right_angle x = x.xhp_open_right_angle
    let xhp_attr_name x = x.xhp_attr_name
    let xhp_attr_equal x = x.xhp_attr_equal
    let xhp_attr_expr x = x.xhp_attr_expr
    let type_constant_left_type x = x.type_constant_left_type
    let type_constant_separator x = x.type_constant_separator
    let type_constant_right_type x = x.type_constant_right_type
    let generic_class_type x = x.generic_class_type
    let generic_arguments x = x.generic_arguments
    let type_arguments_left_angle x = x.type_arguments_left_angle
    let type_arguments x = x.type_arguments
    let type_arguments_right_angle x = x.type_arguments_right_angle

    let binary_operator_kind b =
      match syntax b.binary_operator with
      | Token token ->
        let kind = Token.kind token in
        if Operator.is_trailing_operator_token kind then
          Some (Operator.trailing_from_token kind)
        else
          None
      | _ -> None

    let get_token node =
      match (syntax node) with
      | Token token -> Some token
      | _ -> None

    let rec leading_token nodes =
      match nodes with
      | [] -> None
      | h :: t ->
        let token = get_token h in
        if token = None then
          let result = leading_token (children h) in
          if result = None then leading_token t else result
        else
          token

    let rec trailing_token nodes =
      let rec aux nodes =
        match nodes with
        | [] -> None
        | h :: t ->
          let token = get_token h in
          if token = None then
            let result = trailing_token (children h) in
            if result = None then aux t else result
          else
            token in
      aux (List.rev nodes)

    let syntax_from_children kind ts =
      match kind, ts with
      | (SyntaxKind.Missing, []) -> Missing
      | (SyntaxKind.SyntaxList, x) -> SyntaxList x
      | (SyntaxKind.Error, x) -> Error x
      | (SyntaxKind.LiteralExpression, [x]) -> LiteralExpression x
      | (SyntaxKind.VariableExpression, [x]) -> VariableExpression x
      | (SyntaxKind.QualifiedNameExpression,[x]) -> QualifiedNameExpression x
      | (SyntaxKind.SimpleTypeSpecifier, [x]) -> SimpleTypeSpecifier x
      | (SyntaxKind.ScriptHeader,
        [ header_less_than; header_question; header_language ]) ->
        ScriptHeader { header_less_than; header_question; header_language }
      | (SyntaxKind.Script, [ script_header; script_declarations ]) ->
        Script { script_header; script_declarations }
      | (SyntaxKind.FunctionDeclaration, [ function_attr; function_async;
        function_token; function_name; function_type_params;
        function_left_paren; function_params; function_right_paren;
        function_colon; function_type; function_body]) ->
            FunctionDeclaration { function_attr; function_async;
              function_token; function_name; function_type_params;
              function_left_paren; function_params; function_right_paren;
              function_colon; function_type; function_body }
      | (SyntaxKind.ParameterDeclaration, [ param_attr; param_type; param_name;
        param_default ]) ->
        ParameterDeclaration { param_attr; param_type; param_name;
          param_default }
      | (SyntaxKind.DefaultArgumentSpecifier, [ default_equal;
        default_value ]) ->
        DefaultArgumentSpecifier { default_equal; default_value }
      | (SyntaxKind.CompoundStatement, [ compound_left_brace;
        compound_statements; compound_right_brace ]) ->
        CompoundStatement { compound_left_brace; compound_statements;
          compound_right_brace }
      | (SyntaxKind.ExpressionStatement, [ expr_statement_expr;
        expr_statement_semicolon ]) ->
        ExpressionStatement { expr_statement_expr; expr_statement_semicolon }
      | (SyntaxKind.WhileStatement, [ while_keyword; while_left_paren;
        while_condition_expr; while_right_paren; while_body ]) ->
        WhileStatement{ while_keyword; while_left_paren;
          while_condition_expr; while_right_paren; while_body }
      | (SyntaxKind.IfStatement, [ if_keyword; if_left_paren; if_condition_expr;
        if_right_paren; if_statement; if_elseif_clauses; if_else_clause ]) ->
        IfStatement { if_keyword; if_left_paren; if_condition_expr;
          if_right_paren; if_statement; if_elseif_clauses; if_else_clause }
      | (SyntaxKind.ElseifClause, [ elseif_keyword; elseif_left_paren;
        elseif_condition_expr; elseif_right_paren; elseif_statement ]) ->
        ElseifClause { elseif_keyword; elseif_left_paren;
          elseif_condition_expr; elseif_right_paren; elseif_statement }
      | (SyntaxKind.ElseClause, [ else_keyword; else_statement ]) ->
        ElseClause { else_keyword; else_statement }
      | (SyntaxKind.DoStatement, [ do_keyword; do_statement;
        do_while_keyword; do_left_paren; do_condition_expr;
        do_right_paren; do_semicolon ]) ->
        DoStatement { do_keyword; do_statement;
          do_while_keyword; do_left_paren; do_condition_expr;
          do_right_paren; do_semicolon }
      | (SyntaxKind.SwitchStatement, [ switch_keyword; switch_left_paren;
        switch_expr; switch_right_paren; switch_compound_statement ]) ->
        SwitchStatement{ switch_keyword; switch_left_paren;
          switch_expr; switch_right_paren; switch_compound_statement }
      | (SyntaxKind.PrefixUnaryOperator, [ unary_operator; unary_operand ]) ->
        PrefixUnaryOperator { unary_operator; unary_operand }
      | (SyntaxKind.PostfixUnaryOperator, [ unary_operand; unary_operator ]) ->
        PostfixUnaryOperator { unary_operand; unary_operator }
      | (SyntaxKind.BinaryOperator, [ binary_left_operand; binary_operator;
        binary_right_operand ]) ->
        BinaryOperator { binary_left_operand; binary_operator;
        binary_right_operand }
      | (SyntaxKind.ParenthesizedExpression, [ paren_expr_left_paren;
        paren_expr; paren_expr_right_paren ]) ->
        ParenthesizedExpression { paren_expr_left_paren; paren_expr;
          paren_expr_right_paren }
      | (SyntaxKind.BracedExpression, [ braced_expr_left_brace;
        braced_expr; braced_expr_right_brace ]) ->
        BracedExpression { braced_expr_left_brace; braced_expr;
          braced_expr_right_brace }
      | (SyntaxKind.XHPExpression, [ xhp_open; xhp_body; xhp_close ]) ->
        XHPExpression { xhp_open; xhp_body; xhp_close }
      | (SyntaxKind.XHPOpen, [ xhp_open_name; xhp_open_attrs;
          xhp_open_right_angle ]) ->
        XHPOpen { xhp_open_name; xhp_open_attrs; xhp_open_right_angle }
      | (SyntaxKind.XHPAttribute, [ xhp_attr_name; xhp_attr_equal;
          xhp_attr_expr ]) ->
        XHPAttribute { xhp_attr_name; xhp_attr_equal; xhp_attr_expr }
      | (SyntaxKind.TypeConstant, [ type_constant_left_type;
          type_constant_separator; type_constant_right_type ]) ->
        TypeConstant { type_constant_left_type; type_constant_separator;
          type_constant_right_type }
      | (SyntaxKind.GenericTypeSpecifier, [ generic_class_type;
          generic_arguments ]) ->
        GenericTypeSpecifier { generic_class_type; generic_arguments }
      | (SyntaxKind.TypeArguments, [ type_arguments_left_angle;
          type_arguments; type_arguments_right_angle ]) ->
        TypeArguments { type_arguments_left_angle;
            type_arguments; type_arguments_right_angle }

      | _ -> failwith "with_children called with wrong number of children"

    let all_tokens node =
      let rec aux acc nodes =
        match nodes with
        | [] -> acc
        | h :: t ->
          begin
            match syntax h with
            | Token token -> aux (token :: acc) t
            | _ -> aux (aux acc (children h)) t
          end in
      List.rev (aux [] [node])

    module type ValueBuilderType = sig
      val value_from_children:
        Full_fidelity_syntax_kind.t -> t list -> SyntaxValue.t
      val value_from_token: Token.t -> SyntaxValue.t
    end

    module WithValueBuilder(ValueBuilder: ValueBuilderType) = struct
      let from_children kind ts =
        let syntax = syntax_from_children kind ts in
        let value = ValueBuilder.value_from_children kind ts in
        make syntax value

      let make_token token =
        let syntax = Token token in
        let value = ValueBuilder.value_from_token token in
        make syntax value

      let make_missing () =
        from_children SyntaxKind.Missing []

      let make_prefix_unary_operator unary_operator unary_operand =
        from_children SyntaxKind.PrefixUnaryOperator
          [ unary_operator; unary_operand ]

      let make_postfix_unary_operator unary_operator unary_operand =
        from_children SyntaxKind.PostfixUnaryOperator
          [ unary_operator; unary_operand ]

      let make_binary_operator
        binary_left_operand binary_operator binary_right_operand =
          from_children SyntaxKind.BinaryOperator
            [ binary_right_operand; binary_operator; binary_right_operand ]

      let make_parenthesized_expression
        paren_expr_left_paren paren_expr paren_expr_right_paren =
          from_children SyntaxKind.ParenthesizedExpression
            [ paren_expr_left_paren; paren_expr; paren_expr_right_paren ]

      let make_braced_expression
        braced_expr_left_brace braced_expr braced_expr_right_brace =
          from_children SyntaxKind.BracedExpression
            [ braced_expr_left_brace; braced_expr; braced_expr_right_brace ]

      let make_xhp xhp_open xhp_body xhp_close =
        from_children SyntaxKind.XHPExpression [xhp_open; xhp_body; xhp_close ]

      let make_xhp_open xhp_open_name xhp_open_attrs xhp_open_right_angle =
        from_children SyntaxKind.XHPOpen
          [xhp_open_name; xhp_open_attrs; xhp_open_right_angle ]

      let make_xhp_attr xhp_attr_name xhp_attr_equal xhp_attr_expr =
        from_children SyntaxKind.XHPAttribute
          [ xhp_attr_name; xhp_attr_equal; xhp_attr_expr ]

      let make_list items =
        from_children SyntaxKind.SyntaxList items

      let make_error items =
        from_children SyntaxKind.Error items

      let make_script_header header_less_than header_question header_language =
        from_children SyntaxKind.ScriptHeader
          [ header_less_than; header_question; header_language ]

      let make_script script_header script_declarations =
        from_children SyntaxKind.Script [ script_header; script_declarations ]

      let make_function function_attr function_async function_token
        function_name function_type_params function_left_paren function_params
        function_right_paren function_colon function_type function_body =
      from_children SyntaxKind.FunctionDeclaration [
        function_attr; function_async; function_token; function_name;
        function_type_params; function_left_paren; function_params;
        function_right_paren; function_colon; function_type; function_body ]

      let make_parameter_declaration
        param_attr param_type param_name param_default =
        from_children SyntaxKind.ParameterDeclaration
          [ param_attr; param_type; param_name; param_default ]

      let make_default_argument_specifier default_equal default_value =
        from_children SyntaxKind.DefaultArgumentSpecifier
          [ default_equal; default_value ]

      let make_compound_statement
        compound_left_brace compound_statements compound_right_brace =
        from_children SyntaxKind.CompoundStatement
          [ compound_left_brace; compound_statements; compound_right_brace ]

      let make_expression_statement expr_statement_expr
        expr_statement_semicolon =
        from_children SyntaxKind.ExpressionStatement
          [ expr_statement_expr; expr_statement_semicolon ]

      let make_while_statement
        while_keyword while_left_paren while_condition_expr
        while_right_paren while_body =
        from_children SyntaxKind.WhileStatement
          [ while_keyword; while_left_paren; while_condition_expr;
          while_right_paren; while_body ]

      let make_if_statement
        if_keyword if_left_paren if_condition_expr if_right_paren if_statement
        if_elseif_clauses if_else_clause =
        from_children SyntaxKind.IfStatement
          [ if_keyword; if_left_paren; if_condition_expr; if_right_paren;
          if_statement; if_elseif_clauses; if_else_clause ]
      let make_elseif_clause
        elseif_keyword elseif_left_paren elseif_condition_expr
        elseif_right_paren elseif_statement =
        from_children SyntaxKind.ElseifClause
          [ elseif_keyword; elseif_left_paren; elseif_condition_expr;
          elseif_right_paren; elseif_statement ]
      let make_else_clause else_keyword else_statement =
        from_children SyntaxKind.ElseClause
          [ else_keyword; else_statement ]
      let make_do_statement
        do_keyword do_statement do_while_keyword do_left_paren
        do_condition_expr do_right_paren do_semicolon =
        from_children SyntaxKind.DoStatement
        [ do_keyword; do_statement; do_while_keyword; do_left_paren;
        do_condition_expr; do_right_paren; do_semicolon ]

      let make_switch_statement
        switch_keyword switch_left_paren switch_expr
        switch_right_paren switch_compound_statement =
        from_children SyntaxKind.SwitchStatement
          [ switch_keyword; switch_left_paren; switch_expr;
          switch_right_paren; switch_compound_statement ]

      let make_type_constant type_constant_left_type type_constant_separator
          type_constant_right_type =
        from_children SyntaxKind.TypeConstant
          [ type_constant_left_type; type_constant_separator;
          type_constant_right_type ]

      let make_simple_type_specifier simple_type =
        from_children SyntaxKind.SimpleTypeSpecifier [ simple_type ]

      let make_generic_type_specifier generic_class_type generic_arguments =
        from_children SyntaxKind.GenericTypeSpecifier
          [ generic_class_type; generic_arguments ]

      let make_type_arguments left items right =
        from_children SyntaxKind.TypeArguments [ left; items; right ]

      let make_literal_expression literal =
        from_children SyntaxKind.LiteralExpression [ literal ]

      let make_variable_expression variable =
        from_children SyntaxKind.VariableExpression [ variable ]

      let make_qualified_name_expression name =
        from_children SyntaxKind.QualifiedNameExpression [ name ]

    end (* WithValueBuilder *)
  end (* WithSyntaxValue *)
end (* WithToken *)
