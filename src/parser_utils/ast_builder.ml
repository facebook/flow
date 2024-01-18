(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module Identifiers = struct
  let identifier ?(loc = Loc.none) name = Flow_ast_utils.ident_of_source (loc, name)
end

module Types = struct
  module Functions = struct
    let param ?(loc = Loc.none) ?(optional = false) name annot =
      (loc, { Ast.Type.Function.Param.name; annot; optional })

    let params ?(loc = Loc.none) ?rest ?this ?comments params =
      (loc, { Ast.Type.Function.Params.params; rest; this_ = this; comments })

    let make ?tparams ?comments ?(hook = false) params return =
      { Ast.Type.Function.tparams; params; return; hook; comments }
  end

  module Objects = struct
    let make ?(exact = true) ?(inexact = false) ?comments properties =
      { Ast.Type.Object.exact; inexact; properties; comments }

    let property
        ?(loc = Loc.none)
        ?(optional = false)
        ?(static = false)
        ?(proto = false)
        ?(_method = false)
        ?(variance = None)
        ?comments
        key
        value =
      ( loc,
        {
          Ast.Type.Object.Property.key;
          value;
          optional;
          static;
          proto;
          _method;
          variance;
          comments;
        }
      )

    let getter ?(loc = Loc.none) ?optional ?static ?proto ?_method ?variance key func =
      let value = Ast.Type.Object.Property.Get (loc, func) in
      let prop = property ~loc ?optional ?static ?proto ?_method ?variance key value in
      Ast.Type.Object.Property prop

    let setter ?(loc = Loc.none) ?optional ?static ?proto ?_method ?variance key func =
      let value = Ast.Type.Object.Property.Set (loc, func) in
      let prop = property ~loc ?optional ?static ?proto ?_method ?variance key value in
      Ast.Type.Object.Property prop
  end

  let mixed ?(loc = Loc.none) ?comments () = (loc, Ast.Type.Mixed comments)

  let number ?(loc = Loc.none) ?comments () = (loc, Ast.Type.Number comments)

  let empty ?(loc = Loc.none) ?comments () = (loc, Ast.Type.Empty comments)

  let void ?(loc = Loc.none) ?comments () = (loc, Ast.Type.Void comments)

  let annotation t = (Loc.none, t)

  let object_ ?(loc = Loc.none) ?exact ?inexact properties =
    (loc, Ast.Type.Object (Objects.make ?exact ?inexact properties))

  let type_param
      ?(loc = Loc.none)
      ?(bound = Flow_ast.Type.Missing Loc.none)
      ?(bound_kind = Ast.Type.TypeParam.Colon)
      ?variance
      ?default
      name =
    ( loc,
      {
        Ast.Type.TypeParam.name = Identifiers.identifier name;
        bound;
        bound_kind;
        variance;
        default;
      }
    )

  let return_type_annotation t = Ast.Type.Function.TypeAnnotation t

  let component_renders_annotation ?(loc = Loc.none) variant argument =
    Ast.Type.AvailableRenders
      (loc, { Ast.Type.Renders.operator_loc = Loc.none; comments = None; variant; argument })

  let return_type_guard_annotation ?(loc = Loc.none) ?comments x t =
    Ast.Type.Function.TypeGuard
      (loc, { Ast.Type.TypeGuard.asserts = false; guard = (x, Some t); comments })

  let type_params ?comments ?(loc = Loc.none) params =
    (loc, { Ast.Type.TypeParams.params; comments })

  let type_args ?comments ?(loc = Loc.none) arguments =
    (loc, { Ast.Type.TypeArgs.arguments; comments })

  let unqualified_generic ?comments ?(loc = Loc.none) ?targs name =
    ( loc,
      Ast.Type.Generic
        {
          Ast.Type.Generic.id = Ast.Type.Generic.Identifier.Unqualified (Identifiers.identifier name);
          targs;
          comments;
        }
    )
end

let string_literal ?comments value =
  { Ast.StringLiteral.value; raw = Printf.sprintf "%S" value; comments }

let number_literal ?comments value raw = { Ast.NumberLiteral.value; raw; comments }

let int_literal ?comments value =
  number_literal ?comments (float_of_int value) (Printf.sprintf "%d" value)

let boolean_literal ?comments value = { Ast.BooleanLiteral.value; comments }

module Literals = struct
  let string = string_literal

  let number = number_literal

  let int = int_literal

  let boolean = boolean_literal
end

let string_literal_expression ?(loc = Loc.none) ?comments value =
  (loc, Ast.Expression.StringLiteral (string_literal ?comments value))

let number_literal_expression ?(loc = Loc.none) ?comments value raw =
  (loc, Ast.Expression.NumberLiteral (number_literal ?comments value raw))

let int_literal_expression ?(loc = Loc.none) ?comments value =
  (loc, Ast.Expression.NumberLiteral (int_literal ?comments value))

let boolean_literal_expression ?(loc = Loc.none) ?comments value =
  (loc, Ast.Expression.BooleanLiteral (boolean_literal ?comments value))

module Patterns = struct
  open Ast.Pattern

  let identifier ?(loc = Loc.none) ?annot str =
    let annot = Base.Option.value ~default:(Ast.Type.Missing loc) annot in
    ( loc,
      Identifier
        { Identifier.name = Flow_ast_utils.ident_of_source (loc, str); annot; optional = false }
    )

  let array elements =
    let elements =
      Base.List.map
        ~f:(function
          | Some i -> Array.Element (Loc.none, { Array.Element.argument = i; default = None })
          | None -> Array.Hole Loc.none)
        elements
    in
    (Loc.none, Array { Array.elements; annot = Ast.Type.Missing Loc.none; comments = None })

  let object_ str =
    let open Object in
    ( Loc.none,
      Object
        {
          properties =
            [
              Property
                ( Loc.none,
                  {
                    Property.key =
                      Property.Identifier (Flow_ast_utils.ident_of_source (Loc.none, str));
                    pattern = identifier str;
                    default = None;
                    shorthand = true;
                  }
                );
            ];
          annot = Ast.Type.Missing Loc.none;
          comments = None;
        }
    )
end

module Functions = struct
  open Ast.Function

  let params ?(loc = Loc.none) ?rest ?this_ ?comments ps =
    (loc, { Ast.Function.Params.params = ps; rest; comments; this_ })

  let param ?(loc = Loc.none) ?default argument = (loc, { Ast.Function.Param.argument; default })

  let rest_param ?(loc = Loc.none) ?comments argument =
    (loc, { Ast.Function.RestParam.argument; comments })

  let body ?(loc = Loc.none) ?comments stmts =
    BodyBlock (loc, { Ast.Statement.Block.body = stmts; comments })

  let body_expression expr = BodyExpression expr

  let make
      ~id
      ?params:params_
      ?tparams
      ?(return = Ast.Function.ReturnAnnot.Missing Loc.none)
      ?(generator = false)
      ?(hook = false)
      ?(async = false)
      ?body:body_
      () =
    let params =
      match params_ with
      | Some ps -> ps
      | None -> params []
    in
    let body =
      match body_ with
      | Some body_ -> body_
      | None -> body []
    in
    {
      id;
      params;
      body;
      async;
      generator;
      hook;
      predicate = None;
      return;
      tparams;
      sig_loc = Loc.none;
      comments = None;
    }

  let pattern_of_param param =
    let (_, { Ast.Type.Function.Param.name; annot; optional }) = param in
    Base.Option.map name ~f:(fun name ->
        ( Loc.none,
          Ast.Pattern.Identifier
            { Ast.Pattern.Identifier.name; annot = Ast.Type.Available (Loc.none, annot); optional }
        )
    )

  let param_of_type param =
    let argument = pattern_of_param param in
    Base.Option.map argument ~f:(fun argument ->
        (Loc.none, { Ast.Function.Param.argument; default = None })
    )

  let rest_param_of_type rest =
    let (_, { Ast.Type.Function.RestParam.argument; comments }) = rest in
    let argument = pattern_of_param argument in
    Base.Option.map argument ~f:(fun argument ->
        (Loc.none, { Ast.Function.RestParam.argument; comments })
    )

  let this_param_of_type (loc, { Ast.Type.Function.ThisParam.annot; comments }) =
    (loc, { Ast.Function.ThisParam.annot; comments })

  let params_of_type (loc, { Ast.Type.Function.Params.this_; params; rest; comments }) =
    params
    |> Base.List.map ~f:param_of_type
    |> Base.Option.all
    |> Base.Option.map ~f:(fun params ->
           ( loc,
             {
               Ast.Function.Params.this_ = Base.Option.map ~f:this_param_of_type this_;
               params;
               rest = Base.Option.bind ~f:rest_param_of_type rest;
               comments;
             }
           )
       )

  let of_type
      ?id
      ?(generator = false)
      ?(async = false)
      ?(hook = false)
      ?body:body_
      { Ast.Type.Function.tparams; params; return; _ } =
    let params = params_of_type params in
    let return =
      match return with
      | Ast.Type.Function.TypeAnnotation t -> Ast.Function.ReturnAnnot.Available (Loc.none, t)
      | Ast.Type.Function.TypeGuard g -> Ast.Function.ReturnAnnot.TypeGuard (Loc.none, g)
    in
    Base.Option.map params ~f:(fun params ->
        let body =
          match body_ with
          | Some body_ -> body_
          | None -> body []
        in
        {
          id;
          params;
          body;
          async;
          generator;
          hook;
          predicate = None;
          return;
          tparams;
          sig_loc = Loc.none;
          comments = None;
        }
    )
end

module Classes = struct
  open Ast.Class

  module Methods = struct
    let make ?comments ?(decorators = []) ?(static = false) ~id function_ =
      ( Loc.none,
        {
          Method.kind = Method.Method;
          key = Ast.Expression.Object.Property.Identifier (Identifiers.identifier id);
          value = (Loc.none, function_);
          static;
          decorators;
          comments;
        }
      )

    let with_body method_ ~body =
      let (loc, method_) = method_ in
      let (value_loc, fun_) = method_.Flow_ast.Class.Method.value in
      let fun_ = { fun_ with Flow_ast.Function.body } in
      (loc, { method_ with Flow_ast.Class.Method.value = (value_loc, fun_) })

    let with_docs method_ ~docs =
      let open Flow_ast.Expression.Object.Property in
      let (loc, method_) = method_ in
      let key =
        match method_.Flow_ast.Class.Method.key with
        | StringLiteral (t, lit) -> StringLiteral (t, { lit with Ast.StringLiteral.comments = docs })
        | NumberLiteral (t, lit) -> NumberLiteral (t, { lit with Ast.NumberLiteral.comments = docs })
        | BigIntLiteral (t, lit) -> BigIntLiteral (t, { lit with Ast.BigIntLiteral.comments = docs })
        | Identifier (t, id) -> Identifier (t, { id with Flow_ast.Identifier.comments = docs })
        | PrivateName (loc, pn) ->
          PrivateName (loc, { pn with Flow_ast.PrivateName.comments = docs })
        | Computed (loc, ck) -> Computed (loc, { ck with Flow_ast.ComputedKey.comments = docs })
      in
      (loc, { method_ with Flow_ast.Class.Method.key })
  end

  let implements ?targs id = (Loc.none, { Implements.Interface.id; targs })

  let property
      ?comments
      ?(annot = Ast.Type.Missing Loc.none)
      ?(static = false)
      ?variance
      ?(decorators = [])
      ~id
      value =
    Body.Property
      ( Loc.none,
        {
          Property.key = Ast.Expression.Object.Property.Identifier (Identifiers.identifier id);
          value = Ast.Class.Property.Initialized value;
          annot;
          static;
          variance;
          decorators;
          comments;
        }
      )

  let method_ ?comments ?(decorators = []) ?(static = false) ~id function_ =
    Body.Method (Methods.make ?comments ~decorators ~static ~id function_)

  let make ?comments ?super ?(implements = []) ?id elements =
    let extends =
      match super with
      | None -> None
      | Some expr -> Some (Loc.none, { Extends.expr; targs = None; comments = None })
    in
    let implements =
      match implements with
      | [] -> None
      | _ -> Some (Loc.none, { Implements.interfaces = implements; comments = None })
    in
    {
      id;
      body = (Loc.none, { Body.body = elements; comments = None });
      tparams = None;
      extends;
      implements;
      class_decorators = [];
      comments;
    }
end

module JSXs = struct
  open Ast.JSX

  let identifier ?comments name = Identifier (Loc.none, { Identifier.name; comments })

  let attr_identifier ?(loc = Loc.none) ?comments name =
    Attribute.Identifier (loc, { Identifier.name; comments })

  let attr_literal lit = Attribute.StringLiteral (Loc.none, lit)

  let attr ?(loc = Loc.none) name value = Opening.Attribute (loc, { Attribute.name; value })

  let element
      ?selfclosing:(self_closing = false)
      ?targs
      ?attrs:(attributes = [])
      ?(children = [])
      ?comments
      name =
    {
      opening_element = (Loc.none, { Opening.name; targs; self_closing; attributes });
      closing_element =
        ( if self_closing then
          None
        else
          Some (Loc.none, { Closing.name })
        );
      children = (Loc.none, children);
      comments;
    }

  let child_element ?(loc = Loc.none) ?selfclosing ?attrs ?children name =
    (loc, Element (element ?selfclosing ?attrs ?children name))

  let closing ?(loc = Loc.none) name = (loc, { Closing.name })
end

module Statements = struct
  open Ast.Statement

  let empty ?comments () = (Loc.none, Empty { Empty.comments })

  let block ?comments children = (Loc.none, Block { Block.body = children; comments })

  let while_ test ?comments body = (Loc.none, While { While.test; body; comments })

  let do_while body ?comments test = (Loc.none, DoWhile { DoWhile.body; test; comments })

  let for_raw ?comments init test update body =
    (Loc.none, For { For.init; test; update; body; comments })

  let for_ ?comments init test update body =
    (Loc.none, For { For.init = Some (For.InitExpression init); test; update; body; comments })

  let for_in ?(each = false) ?comments left right body =
    (Loc.none, ForIn { ForIn.left; right; body; each; comments })

  let for_in_declarator ?(kind = Ast.Variable.Var) ?comments declarations =
    ForIn.LeftDeclaration (Loc.none, { VariableDeclaration.declarations; kind; comments })

  let for_in_pattern patt = ForIn.LeftPattern patt

  let for_of ?(await = false) ?comments left right body =
    (Loc.none, ForOf { ForOf.left; right; body; await; comments })

  let for_of_declarator ?(kind = Ast.Variable.Var) ?comments declarations =
    ForOf.LeftDeclaration (Loc.none, { VariableDeclaration.declarations; kind; comments })

  let for_of_pattern patt = ForOf.LeftPattern patt

  let expression ?(loc = Loc.none) ?directive ?comments expression =
    (loc, Expression { Expression.expression; directive; comments })

  let labeled ?comments label body = (Loc.none, Labeled { Labeled.label; body; comments })

  let variable_declarator_generic ?(loc = Loc.none) id init =
    (loc, { VariableDeclaration.Declarator.id; init })

  let variable_declarator ?loc ?init ?annot str =
    variable_declarator_generic ?loc (Patterns.identifier ?loc ?annot str) init

  let variable_declaration ?(kind = Ast.Variable.Var) ?(loc = Loc.none) ?comments declarations =
    (loc, VariableDeclaration { VariableDeclaration.kind; declarations; comments })

  let let_declaration ?loc declarations =
    variable_declaration ~kind:Ast.Variable.Let ?loc declarations

  let const_declaration ?loc ?comments declarations =
    variable_declaration ~kind:Ast.Variable.Const ?loc ?comments declarations

  let function_declaration ?(loc = Loc.none) ?(async = false) ?(generator = false) ?params ?body id
      =
    let fn = Functions.make ?params ~id:(Some id) ~async ~generator ?body () in
    (loc, FunctionDeclaration fn)

  let class_declaration ?super ?implements ?id elements =
    (Loc.none, ClassDeclaration (Classes.make ?super ?implements ?id elements))

  let import_declaration ?(loc = Loc.none) ?comments import_kind source default specifiers =
    (loc, ImportDeclaration { ImportDeclaration.import_kind; source; default; specifiers; comments })

  let named_import_specifier ?kind ?local remote =
    { ImportDeclaration.kind; local; remote; remote_name_def_loc = None }

  let named_import_declaration ?loc ?comments import_kind source specifiers =
    let default = None in
    import_declaration
      ?loc
      ?comments
      import_kind
      source
      default
      (Some (ImportDeclaration.ImportNamedSpecifiers specifiers))

  let if_ ?(loc = Loc.none) ?comments test consequent alternate =
    (loc, If { If.test; consequent; alternate; comments })

  let if_alternate ?(loc = Loc.none) ?comments body = (loc, { If.Alternate.body; comments })

  let return ?(loc = Loc.none) ?comments expr =
    (loc, Return { Return.argument = expr; comments; return_out = loc })

  let directive ?(loc = Loc.none) txt =
    expression ~loc ~directive:txt (string_literal_expression ~loc txt)

  let switch ?(loc = Loc.none) ?comments discriminant cases =
    (loc, Switch { Switch.discriminant; cases; comments; exhaustive_out = fst discriminant })

  let switch_case ?(loc = Loc.none) ?test ?comments consequent =
    (loc, { Switch.Case.test; consequent; comments })

  let break ?(loc = Loc.none) ?comments ?label () = (loc, Break { Break.label; comments })

  let try_ ?(loc = Loc.none) ?comments ?handler ?finalizer stmts =
    let block = (loc, { Block.body = stmts; comments = None }) in
    (loc, Try { Try.block; handler; finalizer; comments })

  let type_alias ?(loc = Loc.none) ?comments ?tparams ~name right =
    (loc, TypeAlias { TypeAlias.id = Identifiers.identifier name; tparams; right; comments })

  let catch ?(loc = Loc.none) ?comments ?param stmts =
    let body = (loc, { Block.body = stmts; comments = None }) in
    (loc, { Try.CatchClause.param; body; comments })

  let with_ ?(loc = Loc.none) ?comments _object body = (loc, With { With._object; body; comments })

  let enum_declaration ?(loc = Loc.none) ?comments id body =
    (loc, EnumDeclaration { EnumDeclaration.id; body; comments })

  module EnumDeclarations = struct
    open EnumDeclaration

    let initialized_member ?(loc = Loc.none) id init_value =
      (loc, { InitializedMember.id; init = (loc, init_value) })

    let defaulted_member ?(loc = Loc.none) id = (loc, { DefaultedMember.id })

    let boolean_body
        ?(loc = Loc.none) ?(explicit_type = false) ?(has_unknown_members = false) ?comments members
        =
      (loc, BooleanBody { BooleanBody.members; explicit_type; has_unknown_members; comments })

    let number_body
        ?(loc = Loc.none) ?(explicit_type = false) ?(has_unknown_members = false) ?comments members
        =
      (loc, NumberBody { NumberBody.members; explicit_type; has_unknown_members; comments })

    let string_defaulted_body
        ?(loc = Loc.none) ?(explicit_type = false) ?(has_unknown_members = false) ?comments members
        =
      let members = StringBody.Defaulted members in
      (loc, StringBody { StringBody.members; explicit_type; has_unknown_members; comments })

    let string_initialized_body
        ?(loc = Loc.none) ?(explicit_type = false) ?(has_unknown_members = false) ?comments members
        =
      let members = StringBody.Initialized members in
      (loc, StringBody { StringBody.members; explicit_type; has_unknown_members; comments })

    let symbol_body ?(loc = Loc.none) ?(has_unknown_members = false) ?comments members =
      (loc, SymbolBody { SymbolBody.members; has_unknown_members; comments })
  end

  let component_id_param ?(loc = Loc.none) ?default ?local name =
    let open Ast.Statement.ComponentDeclaration.Param in
    let local' = Base.Option.value ~default:(Patterns.identifier name) local in
    ( loc,
      {
        name = Identifier (Identifiers.identifier name);
        local = local';
        default;
        shorthand = Base.Option.is_none local;
      }
    )

  let component_string_param ?(loc = Loc.none) ?default name local =
    let open Ast.Statement.ComponentDeclaration.Param in
    ( loc,
      { name = StringLiteral (Loc.none, string_literal name); local; default; shorthand = false }
    )

  let component_params ?(loc = Loc.none) ?rest ?comments params =
    (loc, { Ast.Statement.ComponentDeclaration.Params.params; rest; comments })

  let component_declaration ?(loc = Loc.none) ?tparams ?params ?renders ?comments id body =
    let params' = Base.Option.value ~default:(component_params []) params in
    let renders' = Base.Option.value ~default:(Ast.Type.MissingRenders Loc.none) renders in
    ( loc,
      ComponentDeclaration
        {
          Ast.Statement.ComponentDeclaration.id = Identifiers.identifier id;
          body;
          tparams;
          params = params';
          renders = renders';
          comments;
          sig_loc = Loc.none;
        }
    )

  let component_type_param ?(loc = Loc.none) ?(optional = false) name annot =
    (loc, { Ast.Type.Component.Param.name; annot; optional })

  let component_type_params ?(loc = Loc.none) ?rest ?comments params =
    (loc, { Ast.Type.Component.Params.params; rest; comments })

  let declare_component
      ?(loc = Loc.none) ?tparams ?params ?comments ?(renders = Ast.Type.MissingRenders Loc.none) id
      =
    let params' = Base.Option.value ~default:(component_type_params []) params in
    ( loc,
      DeclareComponent
        {
          Ast.Statement.DeclareComponent.id = Identifiers.identifier id;
          tparams;
          params = params';
          renders;
          comments;
        }
    )
end

module Expressions = struct
  open Ast.Expression

  let identifier ?(loc = Loc.none) ?(comments = None) name =
    (loc, Identifier (loc, { Ast.Identifier.name; comments }))

  let array ?(loc = Loc.none) ?comments elements = (loc, Array { Array.elements; comments })

  let array_expression expr = Array.Expression expr

  let array_hole ?(loc = Loc.none) () = Array.Hole loc

  let arg_list ?(loc = Loc.none) ?comments arguments : (Loc.t, 'a) ArgList.t =
    (loc, { Ast.Expression.ArgList.arguments; comments })

  let call_node ?targs ?args ?comments callee =
    let arguments =
      match args with
      | Some args -> args
      | None -> arg_list []
    in
    { Call.callee; targs; arguments; comments }

  let call ?(loc = Loc.none) ?args callee = (loc, Call (call_node ?args callee))

  let optional_call ?(loc = Loc.none) ~optional ?args callee =
    (loc, OptionalCall { OptionalCall.call = call_node ?args callee; optional; filtered_out = loc })

  let function_ ?(loc = Loc.none) ?(async = false) ?(generator = false) ?params ?id ?body () =
    let fn = Functions.make ~async ~generator ?params ~id ?body () in
    (loc, Function fn)

  let arrow_function ?(loc = Loc.none) ?(async = false) ?params ?body () =
    let fn = Functions.make ~async ~generator:false ?params ~id:None ?body () in
    (loc, ArrowFunction fn)

  let class_ ?(loc = Loc.none) ?super ?id elements = (loc, Class (Classes.make ?super ?id elements))

  let assignment ?(loc = Loc.none) ?comments left ?operator right =
    (loc, Assignment { Assignment.operator; left; right; comments })

  let binary ?(loc = Loc.none) ?comments ~op left right =
    (loc, Binary { Binary.operator = op; left; right; comments })

  let plus left right = binary ~op:Binary.Plus left right

  let minus left right = binary ~op:Binary.Minus left right

  let mult left right = binary ~op:Binary.Mult left right

  let instanceof left right = binary ~op:Binary.Instanceof left right

  let in_ left right = binary ~op:Binary.In left right

  let equal left right = binary ~op:Binary.Equal left right

  let conditional ?(loc = Loc.none) ?comments test consequent alternate =
    (loc, Conditional { Conditional.test; consequent; alternate; comments })

  let logical ?(loc = Loc.none) ?comments ~op left right =
    (loc, Logical { Logical.operator = op; left; right; comments })

  let unary ?(loc = Loc.none) ?(comments = None) ~op argument =
    (loc, Unary { Unary.operator = op; argument; comments })

  let unary_plus (b : (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Plus b

  let unary_minus (b : (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Minus b

  let unary_not (b : (Loc.t, Loc.t) Ast.Expression.t) = unary ~op:Unary.Not b

  let update ?(loc = Loc.none) ?comments ~op ~prefix argument =
    (loc, Update { Update.operator = op; prefix; argument; comments })

  let increment ~prefix argument = update ~op:Update.Increment ~prefix argument

  let decrement ~prefix argument = update ~op:Update.Decrement ~prefix argument

  let object_property_key ?(loc = Loc.none) (k : string) =
    Object.Property.Identifier (Flow_ast_utils.ident_of_source (loc, k))

  let object_property_key_string_literal ?(loc = Loc.none) (k : string) =
    Object.Property.StringLiteral (loc, string_literal k)

  let object_property_key_number_literal ?(loc = Loc.none) (value : float) (raw : string) =
    Object.Property.NumberLiteral (loc, number_literal value raw)

  let object_property_computed_key ?comments ?(loc = Loc.none) expr =
    Object.Property.Computed (loc, { Ast.ComputedKey.expression = expr; comments })

  let object_method ?(loc = Loc.none) ?body ?params ?(generator = false) ?(async = false) key =
    let fn = Functions.make ~id:None ?params ~generator ~async ?body () in
    let prop = Object.Property.Method { key; value = (Loc.none, fn) } in
    Object.Property (loc, prop)

  let object_property ?(shorthand = false) ?(loc = Loc.none) key value =
    let module Prop = Object.Property in
    let prop = Prop.Init { key; value; shorthand } in
    Object.Property (loc, prop)

  let object_property_with_string_literal ?(shorthand = false) ?(loc = Loc.none) k v =
    object_property ~shorthand ~loc (object_property_key_string_literal ~loc k) v

  let object_ ?comments ?(loc = Loc.none) properties = (loc, Object { Object.properties; comments })

  module Members = struct
    (* _object.property *)
    let identifier ?comments ~property _object =
      { Member._object; property = Member.PropertyIdentifier property; comments }

    let identifier_by_name ?comments ~name _object =
      identifier ?comments ~property:(Identifiers.identifier name) _object

    (* _object.#property *)
    let private_name ?comments ~property _object =
      { Member._object; property = Member.PropertyPrivateName property; comments }

    (* _object[property] *)
    let expression ?comments ~property _object =
      { Member._object; property = Member.PropertyExpression property; comments }

    let expression_by_string ?comments ~str _object =
      let expr = (Loc.none, Ast.Expression.StringLiteral (string_literal str)) in
      expression ?comments ~property:expr _object
  end

  let member ?(loc = Loc.none) expr = (loc, Ast.Expression.Member expr)

  let optional_member_expression ?(loc = Loc.none) ~optional expr =
    (loc, OptionalMember { OptionalMember.member = expr; optional; filtered_out = loc })

  let new_ ?(loc = Loc.none) ?comments ?targs ?args callee =
    (loc, New { New.callee; targs; arguments = args; comments })

  let sequence ?(loc = Loc.none) ?comments exprs =
    (loc, Sequence { Sequence.expressions = exprs; comments })

  let expression expr = Expression expr

  let spread ?(loc = Loc.none) ?comments expr =
    Spread (loc, { SpreadElement.argument = expr; comments })

  let jsx_element ?(loc = Loc.none) elem = (loc, JSXElement elem)

  let true_ () = boolean_literal_expression true

  let false_ () = boolean_literal_expression false

  let parenthesis_hint () = string_literal_expression "_flowmin_paren_"

  let logical_and (l : (Loc.t, Loc.t) Ast.Expression.t) r = logical ~op:Logical.And l r

  let logical_or (l : (Loc.t, Loc.t) Ast.Expression.t) r = logical ~op:Logical.Or l r

  let typecast ?(loc = Loc.none) ?comments expression annotation =
    (loc, TypeCast { TypeCast.expression; annot = Types.annotation annotation; comments })

  let as_expression ?(loc = Loc.none) ?comments expression annotation =
    (loc, AsExpression { AsExpression.expression; annot = Types.annotation annotation; comments })

  let yield ?(loc = Loc.none) ?comments ~delegate expr =
    (loc, Yield { Yield.argument = expr; comments; delegate; result_out = loc })

  let this ?(loc = Loc.none) ?comments () = (loc, This { This.comments })

  let super ?(loc = Loc.none) ?comments () = (loc, Super { Super.comments })

  module Literals = struct
    let string ?loc ?comments value = string_literal_expression ?loc ?comments value

    let number ?loc ?comments value raw = number_literal_expression ?loc ?comments value raw

    let int ?loc ?comments value = int_literal_expression ?loc ?comments value

    let bool ?loc ?comments is_true = boolean_literal_expression ?loc ?comments is_true
  end
end

module Comments = struct
  let block ?(loc = Loc.none) ?(on_newline = false) text =
    let open Ast.Comment in
    (loc, { kind = Block; text; on_newline })

  let line ?(loc = Loc.none) ?(on_newline = false) text =
    let open Ast.Comment in
    (loc, { kind = Line; text; on_newline })
end

let mk_program ?(loc = Loc.none) ?(interpreter = None) ?(comments = None) ?(all_comments = []) stmts
    =
  (loc, { Ast.Program.statements = stmts; interpreter; comments; all_comments })

let test_ast_of_string ~parser str =
  let parse_options =
    Some
      {
        Parser_env.components = true;
        enums = true;
        esproposal_decorators = true;
        types = true;
        use_strict = false;
        module_ref_prefix = None;
        module_ref_prefix_LEGACY_INTEROP = None;
      }
  in

  let env = Parser_env.init_env ~token_sink:None ~parse_options None str in
  let (ast, _) = Parser_flow.do_parse env parser false in
  ast

let test_expression_of_string str = test_ast_of_string ~parser:Parser_flow.Parse.expression str

let test_statement_of_string str =
  let ast_list =
    test_ast_of_string ~parser:(Parser_flow.Parse.module_body ~term_fn:(fun _ -> false)) str
  in
  match ast_list with
  | [ast] -> ast
  | _ -> failwith "Multiple statements found"

let test_program_of_string str =
  let stmts =
    test_ast_of_string ~parser:(Parser_flow.Parse.module_body ~term_fn:(fun _ -> false)) str
  in
  ( Loc.none,
    { Ast.Program.statements = stmts; interpreter = None; comments = None; all_comments = [] }
  )
