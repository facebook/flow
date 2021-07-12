(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Refactor_extract_utils

let union_loc acc loc =
  match acc with
  | None -> Some loc
  | Some existing_loc -> Some (Loc.btwn existing_loc loc)

let create_extracted_function
    ~undefined_variables
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~name
    ~extracted_statements =
  let open Ast_builder in
  let id = Some (Identifiers.identifier name) in
  let params =
    undefined_variables
    |> List.map (fun v -> v |> Patterns.identifier |> Functions.param)
    |> Functions.params
  in
  let returned_variables =
    escaping_definitions.VariableAnalysis.escaping_variables
    @ vars_with_shadowed_local_reassignments
  in
  let body_statements =
    match returned_variables with
    | [] -> extracted_statements
    | [only_returned_variable] ->
      extracted_statements
      @ [Statements.return (Some (Expressions.identifier only_returned_variable))]
    | _ ->
      extracted_statements
      @ [
          Statements.return
            (Some
               (Expressions.object_
                  ( returned_variables
                  |> List.map (fun def ->
                         Expressions.object_property
                           ~shorthand:true
                           (Expressions.object_property_key def)
                           (Expressions.identifier def)) )));
        ]
  in
  let body = Functions.body body_statements in
  Functions.make ~id ~params ~async:async_function ~body ()

let create_extracted_function_call
    ~undefined_variables
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~is_method
    ~extracted_statements_loc =
  let open Ast_builder in
  let call =
    let caller =
      if is_method then
        (Loc.none, Flow_ast.Expression.(This { This.comments = None }))
        |> Expressions.member ~property:"newMethod"
        |> Expressions.member_expression
      else
        Expressions.identifier "newFunction"
    in
    Expressions.call
      ~loc:extracted_statements_loc
      ~args:
        ( undefined_variables
        |> List.map (fun v -> v |> Expressions.identifier |> Expressions.expression)
        |> Expressions.arg_list )
      caller
  in
  let call =
    if async_function then
      Expressions.unary ~op:Flow_ast.Expression.Unary.Await call
    else
      call
  in
  let has_vars_with_shadowed_local_reassignments = vars_with_shadowed_local_reassignments <> [] in
  let let_declarations =
    if has_vars_with_shadowed_local_reassignments then
      List.map
        (fun v -> Statements.let_declaration [Statements.variable_declarator v])
        escaping_definitions.VariableAnalysis.escaping_variables
    else
      []
  in
  let returned_variables =
    escaping_definitions.VariableAnalysis.escaping_variables
    @ vars_with_shadowed_local_reassignments
  in
  let function_call_statement_with_collector =
    match returned_variables with
    | [] -> Statements.expression ~loc:extracted_statements_loc call
    | [only_returned_variable] ->
      if has_vars_with_shadowed_local_reassignments then
        Statements.expression
          ~loc:extracted_statements_loc
          (Expressions.assignment (Patterns.identifier only_returned_variable) call)
      else
        let declarations = [Statements.variable_declarator ~init:call only_returned_variable] in
        if escaping_definitions.VariableAnalysis.has_external_writes then
          Statements.let_declaration ~loc:extracted_statements_loc declarations
        else
          Statements.const_declaration ~loc:extracted_statements_loc declarations
    | _ ->
      let pattern =
        Flow_ast.Pattern.
          ( Loc.none,
            Object
              {
                Object.properties =
                  returned_variables
                  |> List.map (fun def ->
                         Object.Property
                           ( Loc.none,
                             {
                               Object.Property.key =
                                 Object.Property.Identifier (Identifiers.identifier def);
                               pattern = Patterns.identifier def;
                               default = None;
                               shorthand = true;
                             } ));
                annot = Flow_ast.Type.Missing Loc.none;
                comments = None;
              } )
      in
      if has_vars_with_shadowed_local_reassignments then
        Statements.expression ~loc:extracted_statements_loc (Expressions.assignment pattern call)
      else
        let declarations = [Statements.variable_declarator_generic pattern (Some call)] in
        if escaping_definitions.VariableAnalysis.has_external_writes then
          Statements.let_declaration ~loc:extracted_statements_loc declarations
        else
          Statements.const_declaration ~loc:extracted_statements_loc declarations
  in
  let_declarations @ [function_call_statement_with_collector]

let insert_function_to_toplevel
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let (program_loc, _) = ast in
  let undefined_variables =
    VariableAnalysis.undefined_variables_after_extraction
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~new_function_target_scope_loc:program_loc
      ~extracted_statements_loc
  in
  let new_ast =
    RefactorProgramMappers.extract_to_function
      ~target_body_loc:program_loc
      ~extracted_statements_loc
      ~function_call_statements:
        (create_extracted_function_call
           ~undefined_variables
           ~escaping_definitions
           ~vars_with_shadowed_local_reassignments
           ~async_function
           ~is_method:false
           ~extracted_statements_loc)
      ~function_declaration_statement:
        ( Loc.none,
          Flow_ast.Statement.FunctionDeclaration
            (create_extracted_function
               ~undefined_variables
               ~escaping_definitions
               ~vars_with_shadowed_local_reassignments
               ~async_function
               ~name:"newFunction"
               ~extracted_statements) )
      ast
  in
  ("Extract to function in module scope", new_ast)

let insert_function_as_inner_functions
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let create_refactor (title, target_function_body_loc) =
    let undefined_variables =
      VariableAnalysis.undefined_variables_after_extraction
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc:target_function_body_loc
        ~extracted_statements_loc
    in
    let new_ast =
      RefactorProgramMappers.extract_to_function
        ~target_body_loc:target_function_body_loc
        ~extracted_statements_loc
        ~function_call_statements:
          (create_extracted_function_call
             ~undefined_variables
             ~escaping_definitions
             ~vars_with_shadowed_local_reassignments
             ~async_function
             ~is_method:false
             ~extracted_statements_loc)
        ~function_declaration_statement:
          ( Loc.none,
            Flow_ast.Statement.FunctionDeclaration
              (create_extracted_function
                 ~undefined_variables
                 ~escaping_definitions
                 ~vars_with_shadowed_local_reassignments
                 ~async_function
                 ~name:"newFunction"
                 ~extracted_statements) )
        ast
    in
    (Printf.sprintf "Extract to inner function in function '%s'" title, new_ast)
  in
  LocationCollectors.collect_function_inserting_locs ~ast ~extracted_statements_loc
  |> List.map create_refactor

let insert_method
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  match LocationCollectors.find_closest_enclosing_class_scope ~ast ~extracted_statements_loc with
  | None -> []
  | Some (id, target_body_loc) ->
    let undefined_variables =
      VariableAnalysis.undefined_variables_after_extraction
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc:target_body_loc
        ~extracted_statements_loc
    in
    let new_ast =
      RefactorProgramMappers.extract_to_method
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements:
          (create_extracted_function_call
             ~undefined_variables
             ~escaping_definitions
             ~vars_with_shadowed_local_reassignments
             ~async_function
             ~is_method:true
             ~extracted_statements_loc)
        ~method_declaration:
          (Ast_builder.Classes.method_
             ~id:"newMethod"
             (create_extracted_function
                ~undefined_variables
                ~escaping_definitions
                ~vars_with_shadowed_local_reassignments
                ~async_function
                ~name:"newMethod"
                ~extracted_statements))
        ast
    in
    let title =
      match id with
      | None -> "Extract to method in anonymous class declaration"
      | Some id -> Printf.sprintf "Extract to method in class '%s'" id
    in
    [(title, new_ast)]

let provide_available_refactors ~ast ~typed_ast:_ ~parsing_heap_reader:_ ~extract_range =
  match StatementsExtractor.extract ast extract_range with
  | None -> []
  | Some extracted_statements ->
    let { InformationCollectors.has_unwrapped_control_flow; async_function; in_class } =
      InformationCollectors.collect_statements_information extracted_statements
    in
    if has_unwrapped_control_flow then
      []
    else
      let extracted_statements_locations = List.map fst extracted_statements in
      (match extracted_statements_locations with
      | [] -> []
      | insert_new_function_call_loc :: rest_statements_locations ->
        let rest_statements_loc_union = List.fold_left union_loc None rest_statements_locations in
        let extracted_statements_loc =
          match rest_statements_loc_union with
          | None -> insert_new_function_call_loc
          | Some loc -> Loc.btwn insert_new_function_call_loc loc
        in
        let (scope_info, ssa_values) = Ssa_builder.program_with_scope ast in
        let {
          VariableAnalysis.defs_with_scopes_of_local_uses;
          vars_with_shadowed_local_reassignments;
        } =
          VariableAnalysis.collect_relevant_defs_with_scope
            ~scope_info
            ~ssa_values
            ~extracted_statements_loc
        in
        let escaping_definitions =
          VariableAnalysis.collect_escaping_local_defs
            ~scope_info
            ~ssa_values
            ~extracted_statements_loc
        in
        let extract_to_method_refactors =
          insert_method
            ~scope_info
            ~defs_with_scopes_of_local_uses
            ~escaping_definitions
            ~vars_with_shadowed_local_reassignments
            ~async_function
            ~ast
            ~extracted_statements
            ~extracted_statements_loc
        in
        if in_class then
          extract_to_method_refactors
        else
          extract_to_method_refactors
          @ insert_function_to_toplevel
              ~scope_info
              ~defs_with_scopes_of_local_uses
              ~escaping_definitions
              ~vars_with_shadowed_local_reassignments
              ~async_function
              ~ast
              ~extracted_statements
              ~extracted_statements_loc
            :: insert_function_as_inner_functions
                 ~scope_info
                 ~defs_with_scopes_of_local_uses
                 ~escaping_definitions
                 ~vars_with_shadowed_local_reassignments
                 ~async_function
                 ~ast
                 ~extracted_statements
                 ~extracted_statements_loc)
