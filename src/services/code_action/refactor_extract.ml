(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Refactor_extract_utils
open Loc_collections

let union_loc acc loc =
  match acc with
  | None -> Some loc
  | Some existing_loc -> Some (Loc.btwn existing_loc loc)

let create_unique_name ~support_experimental_snippet_text_edit ~used_names prefix =
  let unique_name =
    if SSet.mem prefix used_names then
      let rec number_suffixed_name i =
        let name = prefix ^ string_of_int i in
        if SSet.mem name used_names then
          number_suffixed_name (i + 1)
        else
          name
      in
      number_suffixed_name 1
    else
      prefix
  in
  if support_experimental_snippet_text_edit then
    Printf.sprintf "${0:%s}" unique_name
  else
    unique_name

module TypeParamSet = struct
  include Flow_set.Make (struct
    type t = Type.typeparam

    let compare = Stdlib.compare
  end)

  let add_all list set = List.fold_left (fun acc t -> add t acc) set list
end

let create_extracted_function
    ~type_param_synthesizer
    ~type_synthesizer
    ~target_tparams_rev
    ~undefined_variables
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~name
    ~extracted_statements =
  let open Ast_builder in
  let open Base.Result.Let_syntax in
  let id = Some (Identifiers.identifier name) in
  let%bind (params, used_tparam_set) =
    Base.List.fold_result
      ~f:(fun (params, used_tparam_set) (v, loc) ->
        let%map (annot, used_tparam_set) =
          match%map type_synthesizer loc with
          | None -> (Flow_ast.Type.Missing Loc.none, used_tparam_set)
          | Some (tparams_rev, type_) ->
            ( Flow_ast.Type.Available (Loc.none, type_),
              TypeParamSet.add_all tparams_rev used_tparam_set )
        in
        ((v |> Patterns.identifier ~annot |> Functions.param) :: params, used_tparam_set))
      (Base.List.rev undefined_variables)
      ~init:([], TypeParamSet.empty)
  in
  let params = Functions.params params in
  let%bind (returned_variables, used_tparam_set) =
    Base.List.fold_result
      ~f:(fun (returned_variables, used_tparam_set) (v, loc) ->
        let%map (type_, used_tparam_set) =
          match%map type_synthesizer loc with
          | None -> ((Loc.none, Flow_ast.Type.Any None), used_tparam_set)
          | Some (tparams_rev, type_) -> (type_, TypeParamSet.add_all tparams_rev used_tparam_set)
        in
        ((v, type_) :: returned_variables, used_tparam_set))
      (Base.List.rev
         (escaping_definitions.VariableAnalysis.escaping_variables
         @ vars_with_shadowed_local_reassignments))
      ~init:([], used_tparam_set)
  in
  let%map tparams =
    let unbound_tparams =
      target_tparams_rev
      |> TypeParamSet.of_list
      |> TypeParamSet.diff used_tparam_set
      |> TypeParamSet.elements
    in
    (* We need not only the unbound tparams for synthesize types, but also bound ones, since
       unbound ones might have constraints on the bound ones.

       e.g. `B` is unbound in scope and `A` is bound, but we have `B: A`, so `A` must be included
            for synthesizing types. *)
    let tparams_env = unbound_tparams @ target_tparams_rev in
    match%map
      unbound_tparams |> List.map (type_param_synthesizer tparams_env) |> Base.Result.all
    with
    | [] -> None
    | tparams -> Some (Types.type_params tparams)
  in
  let (body_statements, return_type) =
    match returned_variables with
    | [] -> (extracted_statements, (Loc.none, Flow_ast.Type.Void None))
    | [(only_returned_variable, return_type)] ->
      ( extracted_statements
        @ [Statements.return (Some (Expressions.identifier only_returned_variable))],
        return_type )
    | _ ->
      ( extracted_statements
        @ [
            Statements.return
              (Some
                 (Expressions.object_
                    (returned_variables
                    |> List.map (fun (def, _) ->
                           Expressions.object_property
                             ~shorthand:true
                             (Expressions.object_property_key def)
                             (Expressions.identifier def)))));
          ],
        ( Loc.none,
          Flow_ast.Type.Object
            (returned_variables
            |> List.map (fun (v, type_) ->
                   Flow_ast.Type.Object.Property
                     (Types.Objects.property
                        (Expressions.object_property_key v)
                        (Flow_ast.Type.Object.Property.Init type_)))
            |> Types.Objects.make) ) )
  in
  let return_type =
    if async_function then
      Flow_ast.Type.Available
        (Types.unqualified_generic ~targs:(Types.type_args [return_type]) "Promise"
        |> Types.annotation)
    else
      Flow_ast.Type.Available (Types.annotation return_type)
  in
  let body = Functions.body body_statements in
  Functions.make ~id ~params ?tparams ~return:return_type ~async:async_function ~body ()

let create_extracted_function_call
    ~undefined_variables
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~async_function
    ~is_method
    ~new_function_name
    ~extracted_statements_loc =
  let open Ast_builder in
  let call =
    let caller =
      if is_method then
        (Loc.none, Flow_ast.Expression.(This { This.comments = None }))
        |> Expressions.member ~property:new_function_name
        |> Expressions.member_expression
      else
        Expressions.identifier new_function_name
    in
    Expressions.call
      ~loc:extracted_statements_loc
      ~args:
        (undefined_variables
        |> List.map (fun (v, _) -> v |> Expressions.identifier |> Expressions.expression)
        |> Expressions.arg_list)
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
        (fun (v, _) -> Statements.let_declaration [Statements.variable_declarator v])
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
    | [(only_returned_variable, _)] ->
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
                  |> List.map (fun (def, _) ->
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

let create_refactor_for_statements
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~type_synthesizer_context
    ~async_function
    ~is_method
    ~new_function_name
    ~ast
    ~extracted_statements
    ~extracted_statements_loc
    ~target_body_loc
    ~target_tparams_rev =
  let undefined_variables =
    let new_function_target_scope_loc =
      if target_body_loc = fst ast then
        (* Do not pass in target loc if the target is toplevel. *)
        None
      else
        Some target_body_loc
    in
    VariableAnalysis.undefined_variables_after_extraction
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~new_function_target_scope_loc
      ~extracted_loc:extracted_statements_loc
  in
  let { TypeSynthesizer.type_param_synthesizer; type_synthesizer; added_imports } =
    TypeSynthesizer.create_type_synthesizer_with_import_adder type_synthesizer_context
  in
  let function_call_statements =
    create_extracted_function_call
      ~undefined_variables
      ~escaping_definitions
      ~vars_with_shadowed_local_reassignments
      ~async_function
      ~is_method
      ~new_function_name
      ~extracted_statements_loc
  in
  let open Base.Result.Let_syntax in
  let%map new_ast =
    if is_method then
      let%map extracted_function =
        create_extracted_function
          ~type_param_synthesizer
          ~type_synthesizer
          ~target_tparams_rev
          ~undefined_variables
          ~escaping_definitions
          ~vars_with_shadowed_local_reassignments
          ~async_function
          ~name:new_function_name
          ~extracted_statements
      in
      let method_declaration =
        Ast_builder.Classes.method_ ~id:new_function_name extracted_function
      in
      RefactorProgramMappers.extract_to_method
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~method_declaration
        ast
    else
      let%map extracted_function =
        create_extracted_function
          ~type_param_synthesizer
          ~type_synthesizer
          ~target_tparams_rev
          ~undefined_variables
          ~escaping_definitions
          ~vars_with_shadowed_local_reassignments
          ~async_function
          ~name:new_function_name
          ~extracted_statements
      in
      let function_declaration_statement =
        (Loc.none, Flow_ast.Statement.FunctionDeclaration extracted_function)
      in
      RefactorProgramMappers.extract_to_function
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~function_declaration_statement
        ast
  in
  (new_ast, added_imports ())

type refactor = {
  title: string;
  new_ast: (Loc.t, Loc.t) Flow_ast.Program.t;
  added_imports: (string * Autofix_imports.bindings) list;
}

let available_refactors_for_statements
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~escaping_definitions
    ~vars_with_shadowed_local_reassignments
    ~type_synthesizer_context
    ~async_function
    ~has_this_super
    ~typed_ast
    ~reader
    ~create_unique_name
    ~ast
    ~extracted_statements
    ~extracted_statements_loc =
  let create_refactor =
    create_refactor_for_statements
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~escaping_definitions
      ~vars_with_shadowed_local_reassignments
      ~type_synthesizer_context
      ~async_function
      ~ast
      ~extracted_statements
      ~extracted_statements_loc
  in
  let extract_to_method_refactors =
    match
      InsertionPointCollectors.find_closest_enclosing_class
        ~typed_ast
        ~reader
        ~extracted_loc:extracted_statements_loc
    with
    | None -> []
    | Some { InsertionPointCollectors.class_name; body_loc = target_body_loc; tparams_rev } ->
      (match
         create_refactor
           ~is_method:true
           ~new_function_name:(create_unique_name "newMethod")
           ~target_body_loc
           ~target_tparams_rev:tparams_rev
       with
      | Ok (new_ast, added_imports) ->
        let title =
          match class_name with
          | None -> "Extract to method in anonymous class declaration"
          | Some class_name -> Printf.sprintf "Extract to method in class '%s'" class_name
        in
        [{ title; new_ast; added_imports }]
      | Error _ -> [])
  in
  if has_this_super then
    extract_to_method_refactors
  else
    let new_function_name = create_unique_name "newFunction" in
    let open Base.Result.Let_syntax in
    let create_inner_function_refactor
        {
          InsertionPointCollectors.function_name;
          is_method;
          body_loc = target_body_loc;
          tparams_rev;
        } =
      let title =
        Printf.sprintf
          "Extract to inner function in %s '%s'"
          (if is_method then
            "method"
          else
            "function")
          function_name
      in
      let%map (new_ast, added_imports) =
        create_refactor
          ~is_method:false
          ~new_function_name
          ~target_body_loc
          ~target_tparams_rev:tparams_rev
      in
      { title; new_ast; added_imports }
    in
    let top_level_function_refactor =
      let%map (new_ast, added_imports) =
        create_refactor
          ~is_method:false
          ~new_function_name
          ~target_body_loc:(fst ast)
          ~target_tparams_rev:[]
      in
      { title = "Extract to function in module scope"; new_ast; added_imports }
    in
    let extract_to_functions_refactor_results =
      top_level_function_refactor
      ::
      List.map
        create_inner_function_refactor
        (InsertionPointCollectors.collect_function_method_inserting_points
           ~typed_ast
           ~reader
           ~extracted_loc:extracted_statements_loc)
    in
    let extract_to_functions_refactors =
      Base.List.filter_map extract_to_functions_refactor_results ~f:Base.Result.ok
    in
    extract_to_method_refactors @ extract_to_functions_refactors

let extract_from_statements_refactors
    ~ast ~full_cx ~file ~file_sig ~typed_ast ~reader ~create_unique_name extracted_statements =
  let { InformationCollectors.has_unwrapped_control_flow; async_function; has_this_super } =
    InformationCollectors.collect_statements_information extracted_statements
  in
  if has_unwrapped_control_flow then
    []
  else
    let extracted_statements_locations =
      List.map Flow_ast_differ.expand_statement_comment_bounds extracted_statements
    in
    match extracted_statements_locations with
    | [] -> []
    | insert_new_function_call_loc :: rest_statements_locations ->
      let rest_statements_loc_union = List.fold_left union_loc None rest_statements_locations in
      let extracted_statements_loc =
        match rest_statements_loc_union with
        | None -> insert_new_function_call_loc
        | Some loc -> Loc.btwn insert_new_function_call_loc loc
      in
      let (_ssa_abnormal_completion_state, (scope_info, ssa_values, _possible_globals)) =
        Ssa_builder.program_with_scope ast
      in
      let {
        VariableAnalysis.defs_with_scopes_of_local_uses;
        vars_with_shadowed_local_reassignments;
      } =
        VariableAnalysis.collect_relevant_defs_with_scope
          ~scope_info
          ~ssa_values
          ~extracted_loc:extracted_statements_loc
      in
      let escaping_definitions =
        VariableAnalysis.collect_escaping_local_defs
          ~scope_info
          ~ssa_values
          ~extracted_statements_loc
      in
      let type_synthesizer_context =
        let locs =
          defs_with_scopes_of_local_uses
          |> List.map (fun ({ Scope_api.Def.locs = (def_loc, _); _ }, _) -> def_loc)
          |> LocSet.of_list
        in
        let locs =
          List.fold_left
            (fun acc (_, loc) -> LocSet.add loc acc)
            locs
            (vars_with_shadowed_local_reassignments
            @ escaping_definitions.VariableAnalysis.escaping_variables)
        in
        TypeSynthesizer.create_synthesizer_context ~full_cx ~file ~file_sig ~typed_ast ~reader ~locs
      in
      available_refactors_for_statements
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~escaping_definitions
        ~vars_with_shadowed_local_reassignments
        ~type_synthesizer_context
        ~async_function
        ~has_this_super
        ~typed_ast
        ~reader
        ~create_unique_name
        ~ast
        ~extracted_statements
        ~extracted_statements_loc

let create_extract_to_class_field_refactors
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~extracted_expression_loc
    ~expression
    ~new_property_name
    ~ast
    { InsertionPointCollectors.class_name; body_loc; _ } =
  let undefined_variables =
    VariableAnalysis.undefined_variables_after_extraction
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~new_function_target_scope_loc:(Some body_loc)
      ~extracted_loc:extracted_expression_loc
  in
  match undefined_variables with
  | _ :: _ -> []
  | [] ->
    let title =
      match class_name with
      | None -> "Extract to field in anonymous class declaration"
      | Some class_name -> Printf.sprintf "Extract to field in class '%s'" class_name
    in
    let new_ast =
      let open Ast_builder in
      let expression_replacement =
        (Loc.none, Flow_ast.Expression.(This { This.comments = None }))
        |> Expressions.member ~property:new_property_name
        |> Expressions.member_expression
      in
      let field_definition = Ast_builder.Classes.property ~id:new_property_name expression in
      RefactorProgramMappers.extract_to_class_field
        ~class_body_loc:body_loc
        ~expression_loc:extracted_expression_loc
        ~expression_replacement
        ~field_definition
        ast
    in
    [{ title; new_ast; added_imports = [] }]

let create_extract_to_constant_refactor
    ~scope_info
    ~defs_with_scopes_of_local_uses
    ~extracted_expression_loc
    ~expression
    ~new_local_name
    ~ast
    { AstExtractor.title; function_body_loc; statement_loc } =
  let undefined_variables =
    VariableAnalysis.undefined_variables_after_extraction
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~new_function_target_scope_loc:function_body_loc
      ~extracted_loc:extracted_expression_loc
  in
  match undefined_variables with
  | _ :: _ -> None
  | [] ->
    Some
      {
        title;
        new_ast =
          RefactorProgramMappers.extract_to_constant
            ~statement_loc
            ~expression_loc:extracted_expression_loc
            ~expression_replacement:(Ast_builder.Expressions.identifier new_local_name)
            ~constant_definition:
              (Ast_builder.Statements.const_declaration
                 [Ast_builder.Statements.variable_declarator ~init:expression new_local_name])
            ast;
        added_imports = [];
      }

let extract_from_expression_refactors
    ~ast
    ~typed_ast
    ~reader
    ~create_unique_name
    { AstExtractor.constant_insertion_points; expression } =
  let { InformationCollectors.has_this_super; _ } =
    InformationCollectors.collect_expression_information expression
  in
  let extracted_expression_loc = fst expression in
  let (_abnormal_completion_state, (scope_info, ssa_values, _possible_globals)) =
    Ssa_builder.program_with_scope ast
  in
  let { VariableAnalysis.defs_with_scopes_of_local_uses; _ } =
    VariableAnalysis.collect_relevant_defs_with_scope
      ~scope_info
      ~ssa_values
      ~extracted_loc:extracted_expression_loc
  in
  let constant_insertion_points = Nel.to_list constant_insertion_points in
  let class_insertion_point =
    InsertionPointCollectors.find_closest_enclosing_class
      ~typed_ast
      ~reader
      ~extracted_loc:extracted_expression_loc
  in
  let create_extract_to_constant_refactor =
    create_extract_to_constant_refactor
      ~scope_info
      ~defs_with_scopes_of_local_uses
      ~extracted_expression_loc
      ~expression
      ~new_local_name:(create_unique_name "newLocal")
      ~ast
  in
  match class_insertion_point with
  | None ->
    if has_this_super then
      []
    else
      List.filter_map create_extract_to_constant_refactor constant_insertion_points
  | Some ({ InsertionPointCollectors.body_loc = class_body_loc; _ } as class_insertion_point) ->
    let extract_to_class_field_refactors =
      create_extract_to_class_field_refactors
        ~scope_info
        ~defs_with_scopes_of_local_uses
        ~extracted_expression_loc
        ~expression
        ~new_property_name:(create_unique_name "newProperty")
        ~ast
        class_insertion_point
    in
    if has_this_super then
      extract_to_class_field_refactors
      @ List.filter_map
          (fun constant_insertion_point ->
            if Loc.contains class_body_loc constant_insertion_point.AstExtractor.statement_loc then
              (* As long as the statement is still inside the class, we allow extraction to constant,
                 since `this`/`super` is still bound. *)
              create_extract_to_constant_refactor constant_insertion_point
            else
              None)
          constant_insertion_points
    else
      extract_to_class_field_refactors
      @ List.filter_map create_extract_to_constant_refactor constant_insertion_points

let extract_to_type_alias_refactors
    ~ast
    ~full_cx
    ~file
    ~file_sig
    ~typed_ast
    ~reader
    ~create_unique_name
    { AstExtractor.directly_containing_statement_loc; type_ } =
  let type_loc = fst type_ in
  let available_tparams =
    match
      InsertionPointCollectors.collect_function_method_inserting_points
        ~typed_ast
        ~reader
        ~extracted_loc:type_loc
    with
    | [] -> []
    | { InsertionPointCollectors.tparams_rev; _ } :: _ -> tparams_rev
  in
  let new_ast_result =
    let open Base.Result.Let_syntax in
    let%bind (type_params_to_add, type_param_synthesizer) =
      let type_synthesizer_context =
        TypeSynthesizer.create_synthesizer_context
          ~full_cx
          ~file
          ~file_sig
          ~typed_ast
          ~reader
          ~locs:(LocSet.singleton type_loc)
      in
      let { TypeSynthesizer.type_param_synthesizer; type_synthesizer; _ } =
        TypeSynthesizer.create_type_synthesizer_with_import_adder type_synthesizer_context
      in
      let%map used_tparams =
        let%map ast = type_synthesizer type_loc in
        ast |> Base.Option.value_map ~default:[] ~f:fst |> TypeParamSet.of_list
      in
      let type_params_to_add =
        available_tparams
        |> TypeParamSet.of_list
        |> TypeParamSet.diff used_tparams
        |> TypeParamSet.elements
      in
      (type_params_to_add, type_param_synthesizer (available_tparams @ type_params_to_add))
    in
    let open Ast_builder in
    let new_type_name = create_unique_name "NewType" in
    let type_replacement =
      let targs =
        match type_params_to_add with
        | [] -> None
        | _ ->
          Some
            (type_params_to_add
            |> List.map (fun { Type.name; _ } -> Types.unqualified_generic name)
            |> Types.type_args)
      in
      Types.unqualified_generic ?targs new_type_name
    in
    let%map type_alias =
      let%map tparams =
        match type_params_to_add with
        | [] -> return None
        | _ ->
          let%map synthesized =
            type_params_to_add |> List.map type_param_synthesizer |> Base.Result.all
          in
          Some (Types.type_params synthesized)
      in
      Statements.type_alias ?tparams ~name:new_type_name type_
    in
    RefactorProgramMappers.extract_to_type_alias
      ~statement_loc:directly_containing_statement_loc
      ~type_loc
      ~type_replacement
      ~type_alias
      ast
  in
  match new_ast_result with
  | Ok new_ast -> [{ title = "Extract to type alias"; new_ast; added_imports = [] }]
  | Error _ -> []

let provide_available_refactors
    ~ast
    ~full_cx
    ~file
    ~file_sig
    ~typed_ast
    ~reader
    ~support_experimental_snippet_text_edit
    ~extract_range =
  let { AstExtractor.extracted_statements; extracted_expression; extracted_type } =
    AstExtractor.extract ast extract_range
  in
  let used_names = VariableAnalysis.collect_used_names ast in
  let create_unique_name = create_unique_name ~support_experimental_snippet_text_edit ~used_names in
  let extract_from_statements_refactors =
    Base.Option.value_map
      ~default:[]
      ~f:
        (extract_from_statements_refactors
           ~ast
           ~full_cx
           ~file
           ~file_sig
           ~typed_ast
           ~reader
           ~create_unique_name)
      extracted_statements
  in
  let extract_from_expression_refactors =
    Base.Option.value_map
      ~default:[]
      ~f:(extract_from_expression_refactors ~ast ~typed_ast ~reader ~create_unique_name)
      extracted_expression
  in
  let extract_to_type_alias_refactors =
    Base.Option.value_map
      ~default:[]
      ~f:
        (extract_to_type_alias_refactors
           ~ast
           ~full_cx
           ~file
           ~file_sig
           ~typed_ast
           ~reader
           ~create_unique_name)
      extracted_type
  in
  extract_from_statements_refactors
  @ extract_from_expression_refactors
  @ extract_to_type_alias_refactors
