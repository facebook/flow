(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
open Loc_collections

(* Collect all statements that are completely within the selection. *)
class statements_collector (extract_range : Loc.t) =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable _collected_statements = Some []

    method private collect_statement stmt =
      _collected_statements <-
        (match _collected_statements with
        | None -> None
        | Some acc -> Some (stmt :: acc))

    (* `Some collected_statements`, None` when extraction is not allowed based on user selection. *)
    method collected_statements () =
      match _collected_statements with
      | Some collected_statements -> Some (List.rev collected_statements)
      | None -> None

    method! statement stmt =
      let (statement_loc, _) = stmt in
      if Loc.contains extract_range statement_loc then
        let () = this#collect_statement stmt in
        (* If the statement is already completely contained in the range, do not recursve deeper
           to collect more nested ones. *)
        stmt
      else if Loc.contains statement_loc extract_range then
        (* If the range is completely contained in the statement,
           we should recursve deeper to find smaller nested statements that are contained in the range. *)
        super#statement stmt
      else if Loc.intersects extract_range statement_loc then
        (* When there is intersection, it means that the selection is not allowed for extraction. *)
        let () = _collected_statements <- None in
        stmt
      else
        (* If disjoint, the statement and nested ones do not need to be collected. *)
        stmt
  end

let union_loc acc loc =
  match acc with
  | None -> Some loc
  | Some existing_loc -> Some (Loc.btwn existing_loc loc)

class extracted_statements_information_collector =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable _async = false

    val mutable _has_unwrapped_control_flow = false

    method is_async = _async

    method has_unwrapped_control_flow = _has_unwrapped_control_flow

    method! function_ loc function_node =
      if Flow_ast.Function.(function_node.async) then
        (* Do not recurse down to look for await if the function is async. *)
        function_node
      else
        super#function_ loc function_node

    method! for_of_statement loc statement =
      if Flow_ast.Statement.ForOf.(statement.await) then _async <- true;
      super#for_of_statement loc statement

    method! return loc statement =
      _has_unwrapped_control_flow <- true;
      super#return loc statement

    method! yield loc statement =
      _has_unwrapped_control_flow <- true;
      super#yield loc statement

    method! break loc statement =
      _has_unwrapped_control_flow <- true;
      super#break loc statement

    method! continue loc statement =
      _has_unwrapped_control_flow <- true;
      super#continue loc statement

    method! labeled_statement loc statement =
      _has_unwrapped_control_flow <- true;
      super#labeled_statement loc statement

    method! unary_expression loc unary_expr =
      let open Flow_ast.Expression.Unary in
      (match unary_expr with
      | { operator = Await; _ } -> _async <- true
      | _ -> ());
      super#unary_expression loc unary_expr
  end

class insertion_function_body_loc_collector extracted_statements_loc =
  object (_this)
    inherit [(string * Loc.t) list, Loc.t] Flow_ast_visitor.visitor ~init:[] as super

    method! function_ loc function_declaration =
      let open Flow_ast in
      match function_declaration with
      | {
       Function.id = Some (_, { Identifier.name; _ });
       body = Function.BodyBlock (block_loc, _);
       _;
      }
        when Loc.contains block_loc extracted_statements_loc ->
        let () = super#set_acc ((name, block_loc) :: acc) in
        super#function_ loc function_declaration
      | _ -> super#function_ loc function_declaration
  end

class insertion_class_body_loc_collector extracted_statements_loc =
  object
    inherit [(string option * Loc.t) option, Loc.t] Flow_ast_visitor.visitor ~init:None as super

    method! class_ loc class_declaration =
      if Loc.contains extracted_statements_loc loc then
        (* When the class is nested inside the extracted statements, we stop recursing down. *)
        class_declaration
      else if Loc.contains loc extracted_statements_loc then (
        let open Flow_ast in
        let { Class.id; body = (body_loc, _); _ } = class_declaration in
        let id =
          match id with
          | None -> None
          | Some (_, { Identifier.name; _ }) -> Some name
        in
        super#set_acc (Some (id, body_loc));
        super#class_ loc class_declaration
      ) else
        super#class_ loc class_declaration
  end

class refactor_mapper
  ~target_body_loc
  ~extracted_statements_loc
  ~function_call_statement
  ~function_declaration_statement =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method! program program =
      let (program_loc, program_body) = program in
      if Loc.equal program_loc target_body_loc then
        ( program_loc,
          Flow_ast.Program.
            {
              program_body with
              statements =
                super#statement_list program_body.statements @ [function_declaration_statement];
            } )
      else
        super#program program

    method! function_body block =
      let open Flow_ast.Statement.Block in
      let (body_loc, body) = block in
      if Loc.equal body_loc target_body_loc then
        ( body_loc,
          { body with body = super#statement_list body.body @ [function_declaration_statement] } )
      else
        super#function_body block

    method! statement_fork_point stmt =
      let (statement_loc, _) = stmt in
      if Loc.contains extracted_statements_loc statement_loc then
        if Loc.equal (Loc.start_loc extracted_statements_loc) (Loc.start_loc statement_loc) then
          [function_call_statement]
        else
          []
      else
        super#statement_fork_point stmt
  end

let extract_statements ast extract_range =
  let collector = new statements_collector extract_range in
  let _ = collector#program ast in
  collector#collected_statements ()

let find_closest_enclosing_class_scope ~ast ~extracted_statements_loc =
  let collector = new insertion_class_body_loc_collector extracted_statements_loc in
  collector#eval collector#program ast

let collect_relevant_defs_with_scope ~scope_info ~extracted_statements_loc =
  let used_defs_within_extracted_statements =
    IMap.fold
      (fun _ { Scope_api.Scope.locals; _ } acc ->
        LocMap.fold
          (fun use def acc ->
            if Loc.contains extracted_statements_loc use then
              Scope_api.DefMap.add def () acc
            else
              acc)
          locals
          acc)
      scope_info.Scope_api.scopes
      Scope_api.DefMap.empty
  in
  IMap.fold
    (fun _ scope acc ->
      let { Scope_api.Scope.defs; _ } = scope in
      SMap.fold
        (fun _ def acc ->
          if Scope_api.DefMap.mem def used_defs_within_extracted_statements then
            (def, scope) :: acc
          else
            acc)
        defs
        acc)
    scope_info.Scope_api.scopes
    []

let undefined_variables_after_extraction
    ~scope_info ~relevant_defs_with_scope ~new_function_target_scope_loc ~extracted_statements_loc =
  let new_function_target_scopes =
    Scope_api.scope_of_loc scope_info new_function_target_scope_loc
  in
  let to_undefined_variable (def, def_scope) =
    let { Scope_api.Def.locs = (def_loc, _); actual_name; _ } = def in
    if Loc.contains extracted_statements_loc def_loc then
      (* Variables defined inside the extracted statements are locally defined. *)
      None
    else
      (* If a definition is completely nested within the scope of the function to put `newFunction`
         definition, then the definition will be unusable when the statements are moving to this
         higher function scope that does not have the definition.
         This is the indicator that the variable will be undefined. *)
      let def_scope_is_within_function_scope function_scope =
        Scope_api.scope_within scope_info function_scope def_scope
      in
      (* Some of the nodes like functions might have two scopes, one for name and one for body with
         the relation name scope > body scope.
         We must check using `List.for_all` instead of `List.exists`, since a def might be exactly
         in the body scope, and `def_scope_is_within_function_scope name_scope body_scope` will be
         true, which incorrectly decides that a variable is undefined. *)
      if List.for_all def_scope_is_within_function_scope new_function_target_scopes then
        Some actual_name
      else
        None
  in
  List.filter_map to_undefined_variable relevant_defs_with_scope |> List.rev

let collect_escaping_local_defs ~scope_info ~extracted_statements_loc =
  SSet.empty
  |> IMap.fold
       (fun _ { Scope_api.Scope.locals; _ } acc ->
         LocMap.fold
           (fun use { Scope_api.Def.locs = (def_loc, _); actual_name; _ } acc ->
             if
               Loc.contains extracted_statements_loc def_loc
               && not (Loc.contains extracted_statements_loc use)
             then
               SSet.add actual_name acc
             else
               acc)
           locals
           acc)
       scope_info.Scope_api.scopes
  |> SSet.elements

let create_extracted_function
    ~undefined_variables ~escaping_definitions ~async_function ~extracted_statements =
  let open Ast_builder in
  let id = Some (Identifiers.identifier "newFunction") in
  let params =
    undefined_variables
    |> List.map (fun v -> v |> Patterns.identifier |> Functions.param)
    |> Functions.params
  in
  let body_statements =
    match escaping_definitions with
    | [] -> extracted_statements
    | [only_escaping_definition] ->
      extracted_statements
      @ [Statements.return (Some (Expressions.identifier only_escaping_definition))]
    | _ ->
      extracted_statements
      @ [
          Statements.return
            (Some
               (Expressions.object_
                  ( escaping_definitions
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
    ~undefined_variables ~escaping_definitions ~async_function ~extracted_statements_loc =
  let open Ast_builder in
  let call =
    Expressions.call
      ~loc:extracted_statements_loc
      ~args:
        ( undefined_variables
        |> List.map (fun v -> v |> Expressions.identifier |> Expressions.expression)
        |> Expressions.arg_list )
      (Expressions.identifier "newFunction")
  in
  let call =
    if async_function then
      Expressions.unary ~op:Flow_ast.Expression.Unary.Await call
    else
      call
  in
  match escaping_definitions with
  | [] -> Statements.expression ~loc:extracted_statements_loc call
  | [only_escaping_definition] ->
    Statements.const_declaration
      ~loc:extracted_statements_loc
      [Statements.variable_declarator ~init:call only_escaping_definition]
  | _ ->
    Statements.const_declaration
      ~loc:extracted_statements_loc
      [
        Statements.variable_declarator_generic
          Flow_ast.Pattern.
            ( Loc.none,
              Object
                {
                  Object.properties =
                    escaping_definitions
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
          (Some call);
      ]

let insert_function_to_toplevel
    ~scope_info
    ~relevant_defs_with_scope
    ~escaping_definitions
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let (program_loc, _) = ast in
  let undefined_variables =
    undefined_variables_after_extraction
      ~scope_info
      ~relevant_defs_with_scope
      ~new_function_target_scope_loc:program_loc
      ~extracted_statements_loc
  in
  (* Put extracted function to two lines after the end of program to have nice format. *)
  let new_function_loc = Loc.(cursor program_loc.source (program_loc._end.line + 2) 0) in
  let mapper =
    new refactor_mapper
      ~target_body_loc:program_loc
      ~extracted_statements_loc
      ~function_call_statement:
        (create_extracted_function_call
           ~undefined_variables
           ~escaping_definitions
           ~async_function
           ~extracted_statements_loc)
      ~function_declaration_statement:
        ( new_function_loc,
          Flow_ast.Statement.FunctionDeclaration
            (create_extracted_function
               ~undefined_variables
               ~escaping_definitions
               ~async_function
               ~extracted_statements) )
  in
  ("Extract to function in module scope", mapper#program ast)

let insert_function_as_inner_functions
    ~scope_info
    ~relevant_defs_with_scope
    ~escaping_definitions
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let collector = new insertion_function_body_loc_collector extracted_statements_loc in
  let create_refactor (title, target_function_body_loc) =
    let undefined_variables =
      undefined_variables_after_extraction
        ~scope_info
        ~relevant_defs_with_scope
        ~new_function_target_scope_loc:target_function_body_loc
        ~extracted_statements_loc
    in
    let mapper =
      new refactor_mapper
        ~target_body_loc:target_function_body_loc
        ~extracted_statements_loc
        ~function_call_statement:
          (create_extracted_function_call
             ~undefined_variables
             ~escaping_definitions
             ~async_function
             ~extracted_statements_loc)
        ~function_declaration_statement:
          ( Loc.none,
            Flow_ast.Statement.FunctionDeclaration
              (create_extracted_function
                 ~undefined_variables
                 ~escaping_definitions
                 ~async_function
                 ~extracted_statements) )
    in
    (Printf.sprintf "Extract to inner function in function '%s'" title, mapper#program ast)
  in
  collector#eval collector#program ast |> List.map create_refactor

let provide_available_refactors ast extract_range =
  match extract_statements ast extract_range with
  | None -> []
  | Some extracted_statements ->
    let information_collector = new extracted_statements_information_collector in
    let () =
      List.iter
        (fun statement -> information_collector#statement statement |> ignore)
        extracted_statements
    in
    if information_collector#has_unwrapped_control_flow then
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
        let async_function = information_collector#is_async in
        let scope_info = Scope_builder.program ~with_types:false ast in
        let relevant_defs_with_scope =
          collect_relevant_defs_with_scope ~scope_info ~extracted_statements_loc
        in
        let escaping_definitions =
          collect_escaping_local_defs ~scope_info ~extracted_statements_loc
        in
        insert_function_to_toplevel
          ~scope_info
          ~relevant_defs_with_scope
          ~escaping_definitions
          ~async_function
          ~ast
          ~extracted_statements
          ~extracted_statements_loc
        :: insert_function_as_inner_functions
             ~scope_info
             ~relevant_defs_with_scope
             ~escaping_definitions
             ~async_function
             ~ast
             ~extracted_statements
             ~extracted_statements_loc)
