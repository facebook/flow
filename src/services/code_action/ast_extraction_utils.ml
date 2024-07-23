(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module InsertionPointCollectors = struct
  type function_insertion_point = {
    function_name: string;
    body_loc: Loc.t;
    is_method: bool;
    tparams_rev: Type.typeparam list;
  }

  type class_insertion_point = {
    class_name: string option;
    body_loc: Loc.t;
    tparams_rev: Type.typeparam list;
  }

  let not_this_typeparam { Type.is_this; _ } = not is_this

  class virtual ['M, 'N] function_and_method_insertion_point_visitor =
    object (this)
      inherit ['M, 'N, 'M, 'N] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot x = x

      method on_type_annot x = x

      method virtual visit_named_function
          : is_method:bool -> function_name:string -> body_loc:'M -> unit

      method private function_with_name ?name ?(is_method = false) function_declaration =
        let open Flow_ast in
        match function_declaration with
        | { Function.id; body = Function.BodyBlock (block_loc, _); tparams; _ } ->
          (match (id, name) with
          | (None, None) -> ()
          | (Some (_, { Identifier.name; _ }), _)
          | (None, Some (_, { Identifier.name; _ })) ->
            this#type_params_opt tparams (fun _ ->
                this#visit_named_function ~is_method ~function_name:name ~body_loc:block_loc
            ))
        | _ -> ()

      method! function_ function_declaration =
        let () = this#function_with_name function_declaration in
        super#function_ function_declaration

      method! variable_declarator ~kind decl =
        let open Flow_ast in
        let (_, { Statement.VariableDeclaration.Declarator.id; init }) = decl in
        match (id, init) with
        | ( (_, Pattern.Identifier { Pattern.Identifier.name; _ }),
            Some (_, (Expression.ArrowFunction f | Expression.Function f))
          ) ->
          let () = this#function_with_name ~name f in
          super#variable_declarator ~kind decl
        | _ -> super#variable_declarator ~kind decl

      method! class_property property =
        let open Flow_ast in
        let { Class.Property.key; value; _ } = property in
        let () =
          match (key, value) with
          | ( Expression.Object.Property.Identifier name,
              Class.Property.Initialized (_, (Expression.ArrowFunction f | Expression.Function f))
            ) ->
            this#function_with_name ~name ~is_method:true f
          | _ -> ()
        in
        super#class_property property

      method! class_method meth =
        let open Flow_ast in
        let { Class.Method.key; value = (_, f); _ } = meth in
        let () =
          match key with
          | Expression.Object.Property.Identifier name ->
            this#function_with_name ~name ~is_method:true f
          | _ -> ()
        in
        super#class_method meth
    end

  class function_and_method_insertion_point_collector ~loc_of_aloc extracted_loc =
    object (this)
      inherit Typed_ast_finder.type_parameter_mapper as type_parameter_mapper

      inherit! [ALoc.t, ALoc.t * Type.t] function_and_method_insertion_point_visitor

      (* Override the following methods from type_parameter_mapper back *)

      method! type_param = type_parameter_mapper#type_param

      method! type_params_opt = type_parameter_mapper#type_params_opt

      method! class_ = type_parameter_mapper#class_

      val mutable acc = []

      method visit_named_function ~is_method ~function_name ~body_loc =
        let body_loc = loc_of_aloc body_loc in
        if Loc.contains body_loc extracted_loc then
          this#collect_name_and_loc ~is_method function_name body_loc |> this#annot_with_tparams

      method collect_name_and_loc ~tparams_rev ?(is_method = false) function_name body_loc =
        acc <-
          {
            function_name;
            body_loc;
            is_method;
            (* Flow models `this` parameter as a generic parameter with bound, so it will appear in
               the typeparam list. However, that is not necessary for the refactor purpose, since we
               will never produce `this` annotations. *)
            tparams_rev = List.filter not_this_typeparam tparams_rev;
          }
          :: acc

      method function_inserting_locs_with_typeparams typed_ast =
        let _ = this#program typed_ast in
        acc
    end

  let collect_function_method_inserting_points ~typed_ast ~loc_of_aloc ~extracted_loc =
    let collector = new function_and_method_insertion_point_collector ~loc_of_aloc extracted_loc in
    collector#function_inserting_locs_with_typeparams typed_ast

  class class_insertion_point_collector ~loc_of_aloc extracted_loc =
    object (this)
      inherit Typed_ast_finder.type_parameter_mapper as super

      val mutable acc = None

      method private collect_name_and_loc ~tparams_rev class_name body_loc =
        acc <-
          Some { class_name; body_loc; tparams_rev = List.filter not_this_typeparam tparams_rev }

      method closest_enclosing_class_scope typed_ast =
        let _ = this#program typed_ast in
        acc

      method! class_ class_declaration =
        let open Flow_ast in
        let { Class.id; body = (body_aloc, _); tparams; _ } = class_declaration in
        let body_loc = loc_of_aloc body_aloc in
        if Loc.contains extracted_loc body_loc then
          (* When the class is nested inside the extracted statements, we stop recursing down. *)
          class_declaration
        else if Loc.contains body_loc extracted_loc then
          let id =
            match id with
            | None -> None
            | Some (_, { Identifier.name; _ }) -> Some name
          in
          let () =
            this#type_params_opt tparams (fun _ ->
                this#collect_name_and_loc id body_loc |> this#annot_with_tparams
            )
          in
          super#class_ class_declaration
        else
          super#class_ class_declaration
    end

  let find_closest_enclosing_class ~typed_ast ~loc_of_aloc ~extracted_loc =
    let collector = new class_insertion_point_collector ~loc_of_aloc extracted_loc in
    collector#closest_enclosing_class_scope typed_ast
end

module AstExtractor = struct
  type constant_insertion_point = {
    title: string;
    function_body_loc: Loc.t option;
    statement_loc: Loc.t;
  }
  [@@deriving show]

  type expression_with_constant_insertion_points = {
    constant_insertion_points: constant_insertion_point Nel.t;
    expression: (Loc.t, Loc.t) Flow_ast.Expression.t;
  }

  type type_with_statement_loc = {
    directly_containing_statement_loc: Loc.t;
    type_: (Loc.t, Loc.t) Flow_ast.Type.t;
  }

  type extracted = {
    extracted_statements: (Loc.t, Loc.t) Flow_ast.Statement.t list option;
    extracted_expression: expression_with_constant_insertion_points option;
    extracted_type: type_with_statement_loc option;
  }

  let tokens ?parse_options filename file_contents =
    let rev_tokens = ref [] in
    let token_sink = Some (fun token_data -> rev_tokens := token_data :: !rev_tokens) in
    ignore @@ Parser_flow.program_file ~token_sink ~fail:false ~parse_options file_contents filename;
    !rev_tokens

  (* Collect all statements that are completely within the selection.
     Collect a single expression that is completely within the selection. *)
  class collector tokens (extract_range : Loc.t) =
    object (this)
      inherit
        [Loc.t, Loc.t] InsertionPointCollectors.function_and_method_insertion_point_visitor as super

      val mutable constant_insertion_points =
        Nel.one
          {
            title = "Extract to constant in module scope";
            function_body_loc = None;
            statement_loc = Loc.none;
          }

      val mutable collect_statement = true

      val mutable collected_expression = None

      val mutable collected_type = None

      method visit_named_function ~is_method ~function_name ~body_loc =
        if Loc.contains body_loc extract_range then
          constant_insertion_points <-
            Nel.cons
              {
                title =
                  Printf.sprintf
                    "Extract to constant in %s '%s'"
                    ( if is_method then
                      "method"
                    else
                      "function"
                    )
                    function_name;
                function_body_loc = Some body_loc;
                statement_loc = Loc.none;
              }
              constant_insertion_points

      (* `Some collected_statements`
         `None` when extraction is not allowed based on user selection. *)
      val mutable _collected_statements = Some []

      method extracted =
        let extracted_statements =
          match _collected_statements with
          | None -> None
          | Some [] -> None
          | Some collected_statements -> Some (List.rev collected_statements)
        in
        {
          extracted_statements;
          extracted_expression = collected_expression;
          extracted_type = collected_type;
        }

      method private collect_statement stmt =
        _collected_statements <-
          (match _collected_statements with
          | None -> None
          | Some acc -> Some (stmt :: acc))

      method! statement stmt =
        let (statement_loc, stmt') = stmt in
        let saved_constant_insertion_points = constant_insertion_points in
        let () =
          constant_insertion_points <-
            (let ({ title; function_body_loc; _ }, constant_insertion_points) =
               constant_insertion_points
             in
             ({ title; function_body_loc; statement_loc }, constant_insertion_points)
            )
        in
        let result =
          if Loc.contains extract_range statement_loc then
            let () = this#collect_statement stmt in
            (* If the statement is already completely contained in the range,
               do not recursve deeper to collect more nested ones,
               except for the special case when the statement is an expression statement. *)
            let () =
              match stmt' with
              | Flow_ast.Statement.Expression { Flow_ast.Statement.Expression.expression; _ } ->
                if fst expression = statement_loc then
                  collected_expression <- Some { constant_insertion_points; expression }
              | _ -> ()
            in
            stmt
          else if Loc.contains statement_loc extract_range then
            (* If the range is completely contained in the statement,
               we should recursve deeper to find smaller nested statements/expressions that are
               contained in the range. *)
            super#statement stmt
          else if Loc.intersects extract_range statement_loc then
            (* When there is intersection, it means that the selection is not allowed for extraction. *)
            let () = _collected_statements <- None in
            stmt
          else
            (* If disjoint, the statement and nested ones do not need to be collected. *)
            stmt
        in
        let () = constant_insertion_points <- saved_constant_insertion_points in
        result

      method! expression expr =
        let (expression_loc, _) = expr in
        if this#valid_single_selection expression_loc then
          let () = collected_expression <- Some { constant_insertion_points; expression = expr } in
          expr
        else if Loc.contains expression_loc extract_range then
          (* If the range is completely contained in the expression,
             we should recursve deeper to find smaller nested expressions that are contained in the
             range. *)
          super#expression expr
        else
          (* In all other cases, selection is illegal. *)
          expr

      method! jsx_child child =
        let open Flow_ast.JSX in
        let (loc, child') = child in
        ( loc,
          match child' with
          | Element elem -> Element (this#visit_jsx_element loc elem)
          | Fragment frag -> Fragment (this#visit_jsx_fragment loc frag)
          | ExpressionContainer expr -> ExpressionContainer (this#jsx_expression expr)
          | SpreadChild spread -> SpreadChild (this#jsx_spread_child spread)
          | Text _ as child' -> child'
        )

      (* We treat jsx_element like an expression, and follow similar extraction logic. *)
      method visit_jsx_element loc elem =
        if this#valid_single_selection loc then
          let () =
            collected_expression <-
              Some
                {
                  constant_insertion_points;
                  expression = (loc, Flow_ast.Expression.JSXElement elem);
                }
          in
          elem
        else if Loc.contains loc extract_range then
          super#jsx_element loc elem
        else
          elem

      (* We treat jsx_fragment like an expression, and follow similar extraction logic. *)
      method visit_jsx_fragment loc elem =
        if this#valid_single_selection loc then
          let () =
            collected_expression <-
              Some
                {
                  constant_insertion_points;
                  expression = (loc, Flow_ast.Expression.JSXFragment elem);
                }
          in
          elem
        else if Loc.contains loc extract_range then
          super#jsx_fragment elem
        else
          elem

      method! type_ t =
        let (type_loc, _) = t in
        if this#valid_single_selection type_loc then
          let ({ statement_loc; _ }, _) = constant_insertion_points in
          let () =
            collected_type <- Some { directly_containing_statement_loc = statement_loc; type_ = t }
          in
          t
        else if Loc.contains type_loc extract_range then
          (* If the range is completely contained in the type,
             we should recursve deeper to find smaller nested types that are contained in the
             range. *)
          super#type_ t
        else
          (* In all other cases, selection is illegal. *)
          t

      (* Check whether the node at node_loc is a valid single selection. *)
      method private valid_single_selection node_loc =
        (* Fast path: exact selection is always valid *)
        Loc.equal extract_range node_loc
        || (* If we selected more than the current node, we need to ensure that the selection
              doesn't touch any other token outside of node_loc *)
        Loc.contains extract_range node_loc
        && Base.List.for_all tokens ~f:(fun { Parser_env.token_loc; _ } ->
               Loc.contains node_loc token_loc
               || Loc.(pos_cmp extract_range._end token_loc.start) <= 0
               || Loc.(pos_cmp token_loc._end extract_range.start) <= 0
           )
    end

  let extract tokens ast extract_range =
    let collector = new collector tokens extract_range in
    let _ = collector#program ast in
    collector#extracted
end
