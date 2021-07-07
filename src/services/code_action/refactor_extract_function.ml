(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
module Ssa_api = Ssa_api.With_Loc
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
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable _in_class = false

    val mutable _async = false

    val mutable _has_unwrapped_control_flow = false

    val mutable _inside_loop = false

    val mutable _inside_switch = false

    method private inside_loop : 'a. (Loc.t -> 'a -> 'a) -> Loc.t -> 'a -> 'a =
      fun f loc statement ->
        let old_inside_loop = _inside_loop in
        _inside_loop <- true;
        let statement = f loc statement in
        _inside_loop <- old_inside_loop;
        statement

    method in_class = _in_class

    method is_async = _async

    method has_unwrapped_control_flow = _has_unwrapped_control_flow

    (* Do not recurse down into nested classes. *)
    method! class_ _loc class_node = class_node

    method! function_ loc function_node =
      if Flow_ast.Function.(function_node.async) then
        (* Do not recurse down to look for await if the function is async. *)
        function_node
      else
        super#function_ loc function_node

    method! for_of_statement loc statement =
      if Flow_ast.Statement.ForOf.(statement.await) then _async <- true;
      this#inside_loop super#for_of_statement loc statement

    method! for_in_statement loc statement = this#inside_loop super#for_in_statement loc statement

    method! do_while loc statement = this#inside_loop super#do_while loc statement

    method! while_ loc statement = this#inside_loop super#while_ loc statement

    method! switch loc statement =
      let old_inside_switch = _inside_switch in
      _inside_switch <- true;
      let statement = super#switch loc statement in
      _inside_switch <- old_inside_switch;
      statement

    method! return loc statement =
      _has_unwrapped_control_flow <- true;
      super#return loc statement

    method! yield loc statement =
      _has_unwrapped_control_flow <- true;
      super#yield loc statement

    method! break loc statement =
      let open Flow_ast.Statement.Break in
      if ((not _inside_loop) && not _inside_switch) || statement.label != None then
        _has_unwrapped_control_flow <- true;
      super#break loc statement

    method! continue loc statement =
      let open Flow_ast.Statement.Continue in
      if (not _inside_loop) || statement.label != None then _has_unwrapped_control_flow <- true;
      super#continue loc statement

    method! labeled_statement loc statement =
      _has_unwrapped_control_flow <- true;
      super#labeled_statement loc statement

    method! this_expression loc this_expr =
      _in_class <- true;
      super#this_expression loc this_expr

    method! super_expression loc super_expr =
      _in_class <- true;
      super#super_expression loc super_expr

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

class replace_original_statements_mapper ~extracted_statements_loc ~function_call_statement =
  object (_this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

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

class extract_to_function_refactor_mapper
  ~target_body_loc
  ~extracted_statements_loc
  ~function_call_statement
  ~function_declaration_statement =
  object (_this)
    inherit
      replace_original_statements_mapper ~extracted_statements_loc ~function_call_statement as super

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
  end

class extract_to_method_refactor_mapper
  ~target_body_loc ~extracted_statements_loc ~function_call_statement ~method_declaration =
  object (_this)
    inherit
      replace_original_statements_mapper ~extracted_statements_loc ~function_call_statement as super

    method! class_body block =
      let open Flow_ast.Class.Body in
      let (body_loc, body) = block in
      if Loc.equal body_loc target_body_loc then
        ( body_loc,
          {
            body with
            body = Flow_ast_mapper.map_list super#class_element body.body @ [method_declaration];
          } )
      else
        super#class_body block
  end

let extract_statements ast extract_range =
  let collector = new statements_collector extract_range in
  let _ = collector#program ast in
  collector#collected_statements ()

let find_closest_enclosing_class_scope ~ast ~extracted_statements_loc =
  let collector = new insertion_class_body_loc_collector extracted_statements_loc in
  collector#eval collector#program ast

type relevant_defs = {
  defs_with_scopes_of_local_uses: (Scope_api.Def.t * Scope_api.Scope.t) list;
  vars_with_shadowed_local_reassignments: string list;
}

let collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_statements_loc =
  let ( used_defs_within_extracted_statements,
        shadowed_local_reassignments_within_extracted_statements ) =
    IMap.fold
      (fun _ { Scope_api.Scope.locals; _ } acc ->
        LocMap.fold
          (fun use def ((used_def_acc, shadowed_local_reassignment_acc) as acc) ->
            if Loc.contains extracted_statements_loc use then
              (Scope_api.DefMap.add def () used_def_acc, shadowed_local_reassignment_acc)
            else
              (* We do not need to worry about a local reassignment if the variable is only used
                 within extracted statements, since all uses will still read the correct modified
                 value within extracted statements.

                 e.g. We have

                 ```
                 // extracted statements start
                 let a = 3;
                 a = 4;
                 console.log(a);
                 // extracted statements end
                 // no more uses of `a`
                 ```

                 Then refactor it into

                 ```
                 newFunction();

                 function newFunction() {
                   let a = 3;
                   a = 4;
                   console.log(a);
                 }
                 ```

                 does not change the semantics.
                 *)
              let def_loc = fst def.Scope_api.Def.locs in
              if not (Loc.contains extracted_statements_loc def_loc) then
                let has_local_reassignment =
                  (* Find whether there is a local write within the selected statements,
                     while there is already a def outside of them.
                     If there is a local write, we know the variable has been mutated locally. *)
                  match LocMap.find_opt use ssa_values with
                  | None -> false
                  | Some writes ->
                    List.exists
                      (function
                        | Ssa_api.Uninitialized -> false
                        | Ssa_api.Write reason ->
                          let write_loc = Reason.poly_loc_of_reason reason in
                          Loc.contains extracted_statements_loc write_loc)
                      writes
                in
                if has_local_reassignment then
                  ( used_def_acc,
                    SSet.add def.Scope_api.Def.actual_name shadowed_local_reassignment_acc )
                else
                  acc
              else
                acc)
          locals
          acc)
      scope_info.Scope_api.scopes
      (Scope_api.DefMap.empty, SSet.empty)
  in
  {
    defs_with_scopes_of_local_uses =
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
        [];
    vars_with_shadowed_local_reassignments =
      SSet.elements shadowed_local_reassignments_within_extracted_statements;
  }

let undefined_variables_after_extraction
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~new_function_target_scope_loc
    ~extracted_statements_loc =
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
  List.filter_map to_undefined_variable defs_with_scopes_of_local_uses |> List.rev

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
    ~undefined_variables ~escaping_definitions ~async_function ~name ~extracted_statements =
  let open Ast_builder in
  let id = Some (Identifiers.identifier name) in
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
    ~undefined_variables ~escaping_definitions ~async_function ~is_method ~extracted_statements_loc
    =
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
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let (program_loc, _) = ast in
  let undefined_variables =
    undefined_variables_after_extraction
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~new_function_target_scope_loc:program_loc
      ~extracted_statements_loc
  in
  (* Put extracted function to two lines after the end of program to have nice format. *)
  let new_function_loc = Loc.(cursor program_loc.source (program_loc._end.line + 2) 0) in
  let mapper =
    new extract_to_function_refactor_mapper
      ~target_body_loc:program_loc
      ~extracted_statements_loc
      ~function_call_statement:
        (create_extracted_function_call
           ~undefined_variables
           ~escaping_definitions
           ~async_function
           ~is_method:false
           ~extracted_statements_loc)
      ~function_declaration_statement:
        ( new_function_loc,
          Flow_ast.Statement.FunctionDeclaration
            (create_extracted_function
               ~undefined_variables
               ~escaping_definitions
               ~async_function
               ~name:"newFunction"
               ~extracted_statements) )
  in
  ("Extract to function in module scope", mapper#program ast)

let insert_function_as_inner_functions
    ~scope_info
    ~defs_with_scopes_of_local_uses
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
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc:target_function_body_loc
        ~extracted_statements_loc
    in
    let mapper =
      new extract_to_function_refactor_mapper
        ~target_body_loc:target_function_body_loc
        ~extracted_statements_loc
        ~function_call_statement:
          (create_extracted_function_call
             ~undefined_variables
             ~escaping_definitions
             ~async_function
             ~is_method:false
             ~extracted_statements_loc)
        ~function_declaration_statement:
          ( Loc.none,
            Flow_ast.Statement.FunctionDeclaration
              (create_extracted_function
                 ~undefined_variables
                 ~escaping_definitions
                 ~async_function
                 ~name:"newFunction"
                 ~extracted_statements) )
    in
    (Printf.sprintf "Extract to inner function in function '%s'" title, mapper#program ast)
  in
  collector#eval collector#program ast |> List.map create_refactor

let insert_method
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~async_function
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  match find_closest_enclosing_class_scope ~ast ~extracted_statements_loc with
  | None -> []
  | Some (id, target_body_loc) ->
    let undefined_variables =
      undefined_variables_after_extraction
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~new_function_target_scope_loc:target_body_loc
        ~extracted_statements_loc
    in
    let mapper =
      new extract_to_method_refactor_mapper
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statement:
          (create_extracted_function_call
             ~undefined_variables
             ~escaping_definitions
             ~async_function
             ~is_method:true
             ~extracted_statements_loc)
        ~method_declaration:
          (Ast_builder.Classes.method_
             ~id:"newMethod"
             (create_extracted_function
                ~undefined_variables
                ~escaping_definitions
                ~async_function
                ~name:"newMethod"
                ~extracted_statements))
    in
    let title =
      match id with
      | None -> "Extract to method in anonymous class declaration"
      | Some id -> Printf.sprintf "Extract to method in class '%s'" id
    in
    [(title, mapper#program ast)]

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
        let in_class = information_collector#in_class in
        let (scope_info, ssa_values) = Ssa_builder.program_with_scope ast in
        let { defs_with_scopes_of_local_uses; _ } =
          collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_statements_loc
        in
        let escaping_definitions =
          collect_escaping_local_defs ~scope_info ~extracted_statements_loc
        in
        let extract_to_method_refactors =
          insert_method
            ~scope_info
            ~defs_with_scopes_of_local_uses
            ~escaping_definitions
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
              ~async_function
              ~ast
              ~extracted_statements
              ~extracted_statements_loc
            :: insert_function_as_inner_functions
                 ~scope_info
                 ~defs_with_scopes_of_local_uses
                 ~escaping_definitions
                 ~async_function
                 ~ast
                 ~extracted_statements
                 ~extracted_statements_loc)
