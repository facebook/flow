(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
module Ssa_api = Ssa_api.With_Loc
open Loc_collections

module InformationCollectors = struct
  class extracted_information_collector =
    object (this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      val mutable _has_this_super = false

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

      method has_this_super = _has_this_super

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
        _has_this_super <- true;
        super#this_expression loc this_expr

      method! super_expression loc super_expr =
        _has_this_super <- true;
        super#super_expression loc super_expr

      method! unary_expression loc unary_expr =
        let open Flow_ast.Expression.Unary in
        (match unary_expr with
        | { operator = Await; _ } -> _async <- true
        | _ -> ());
        super#unary_expression loc unary_expr
    end

  type t = {
    has_unwrapped_control_flow: bool;
    async_function: bool;
    has_this_super: bool;
  }

  let collect_statements_information extracted_statements =
    let information_collector = new extracted_information_collector in
    let () =
      List.iter
        (fun statement -> information_collector#statement statement |> ignore)
        extracted_statements
    in
    {
      has_unwrapped_control_flow = information_collector#has_unwrapped_control_flow;
      async_function = information_collector#is_async;
      has_this_super = information_collector#has_this_super;
    }

  let collect_expression_information extracted_expression =
    let information_collector = new extracted_information_collector in
    let _ = information_collector#expression extracted_expression in
    {
      has_unwrapped_control_flow = information_collector#has_unwrapped_control_flow;
      async_function = information_collector#is_async;
      has_this_super = information_collector#has_this_super;
    }
end

module RefactorProgramMappers = struct
  class replace_original_statements_mapper ~extracted_statements_loc ~function_call_statements =
    object (_this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      method! statement_fork_point stmt =
        let statement_loc = Flow_ast_differ.expand_statement_comment_bounds stmt in
        if Loc.contains extracted_statements_loc statement_loc then
          if Loc.equal (Loc.start_loc extracted_statements_loc) (Loc.start_loc statement_loc) then
            function_call_statements
          else
            []
        else
          super#statement_fork_point stmt
    end

  class extract_statements_to_function_refactor_mapper
    ~target_body_loc
    ~extracted_statements_loc
    ~function_call_statements
    ~function_declaration_statement =
    object (_this)
      inherit
        replace_original_statements_mapper ~extracted_statements_loc ~function_call_statements as super

      method! program program =
        let (program_loc, program_body) = program in
        if Loc.equal program_loc target_body_loc then
          let open Flow_ast.Program in
          ( program_loc,
            {
              program_body with
              statements =
                super#statement_list program_body.statements @ [function_declaration_statement];
            }
          )
        else
          super#program program

      method! function_body block =
        let open Flow_ast.Statement.Block in
        let (body_loc, body) = block in
        if Loc.equal body_loc target_body_loc then
          ( body_loc,
            { body with body = super#statement_list body.body @ [function_declaration_statement] }
          )
        else
          super#function_body block
    end

  let extract_statements_to_function
      ~target_body_loc
      ~extracted_statements_loc
      ~function_call_statements
      ~function_declaration_statement
      ast =
    let mapper =
      new extract_statements_to_function_refactor_mapper
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~function_declaration_statement
    in
    mapper#program ast

  class extract_statements_to_method_refactor_mapper
    ~target_body_loc ~extracted_statements_loc ~function_call_statements ~method_declaration =
    object (_this)
      inherit
        replace_original_statements_mapper ~extracted_statements_loc ~function_call_statements as super

      method! class_body block =
        let open Flow_ast.Class.Body in
        let (body_loc, body) = block in
        if Loc.equal body_loc target_body_loc then
          ( body_loc,
            {
              body with
              body = Flow_ast_mapper.map_list super#class_element body.body @ [method_declaration];
            }
          )
        else
          super#class_body block
    end

  let extract_statements_to_method
      ~target_body_loc ~extracted_statements_loc ~function_call_statements ~method_declaration ast =
    let mapper =
      new extract_statements_to_method_refactor_mapper
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~method_declaration
    in

    mapper#program ast

  class replace_original_expression_mapper ~expression_loc ~expression_replacement =
    object (_this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      method! expression ((expr_loc, _) as expr) =
        if Loc.equal expr_loc expression_loc then
          expression_replacement
        else
          super#expression expr

      method! jsx_child child =
        let open Flow_ast.JSX in
        let (loc, child') = child in
        if Loc.equal loc expression_loc then
          ( loc,
            match child' with
            | Element _
            | Fragment _ ->
              (match expression_replacement with
              | (_, Flow_ast.Expression.JSXElement e) -> Element e
              | (_, Flow_ast.Expression.JSXFragment f) -> Fragment f
              | _ ->
                ExpressionContainer
                  {
                    ExpressionContainer.expression =
                      ExpressionContainer.Expression expression_replacement;
                    comments = None;
                  })
            | ExpressionContainer _
            | SpreadChild _
            | Text _ ->
              child'
          )
        else
          child
    end

  class extract_expression_to_react_component_refactor_mapper
    ~expression_loc ~expression_replacement ~component_declaration_statement =
    object (_this)
      inherit replace_original_expression_mapper ~expression_loc ~expression_replacement as super

      method! program program =
        let open Flow_ast.Program in
        let (program_loc, program_body) = program in
        ( program_loc,
          {
            program_body with
            statements =
              super#statement_list program_body.statements @ [component_declaration_statement];
          }
        )
    end

  let extract_expression_to_react_component
      ~expression_loc ~expression_replacement ~component_declaration_statement ast =
    let mapper =
      new extract_expression_to_react_component_refactor_mapper
        ~expression_loc
        ~expression_replacement
        ~component_declaration_statement
    in
    mapper#program ast

  class extract_expression_to_constant_refactor_mapper
    ~statement_loc ~expression_loc ~expression_replacement ~constant_definition =
    object (this)
      inherit replace_original_expression_mapper ~expression_loc ~expression_replacement as super

      method! statement_fork_point stmt =
        let (stmt_loc, _) = stmt in
        if Loc.equal stmt_loc statement_loc then
          [constant_definition; this#statement stmt]
        else
          super#statement_fork_point stmt
    end

  let extract_expression_to_constant
      ~statement_loc ~expression_loc ~expression_replacement ~constant_definition ast =
    let mapper =
      new extract_expression_to_constant_refactor_mapper
        ~statement_loc
        ~expression_loc
        ~expression_replacement
        ~constant_definition
    in
    mapper#program ast

  class extract_expression_to_class_field_refactor_mapper
    ~class_body_loc ~expression_loc ~expression_replacement ~field_definition =
    object (_this)
      inherit replace_original_expression_mapper ~expression_loc ~expression_replacement as super

      method! class_body block =
        let open Flow_ast.Class.Body in
        let (body_loc, body) = block in
        if Loc.equal body_loc class_body_loc then
          ( body_loc,
            {
              body with
              body = field_definition :: Flow_ast_mapper.map_list super#class_element body.body;
            }
          )
        else
          super#class_body block
    end

  let extract_expression_to_class_field
      ~class_body_loc ~expression_loc ~expression_replacement ~field_definition ast =
    let mapper =
      new extract_expression_to_class_field_refactor_mapper
        ~class_body_loc
        ~expression_loc
        ~expression_replacement
        ~field_definition
    in
    mapper#program ast

  class extract_type_to_type_alias_refactor_mapper
    ~statement_loc ~type_loc ~type_replacement ~type_alias =
    object (this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      method! type_ ((loc, _) as t) =
        if Loc.equal loc type_loc then
          type_replacement
        else
          super#type_ t

      method! statement_fork_point stmt =
        let (stmt_loc, _) = stmt in
        if Loc.equal stmt_loc statement_loc then
          [type_alias; this#statement stmt]
        else
          super#statement_fork_point stmt
    end

  let extract_type_to_type_alias ~statement_loc ~type_loc ~type_replacement ~type_alias ast =
    let mapper =
      new extract_type_to_type_alias_refactor_mapper
        ~statement_loc
        ~type_loc
        ~type_replacement
        ~type_alias
    in
    mapper#program ast
end

module VariableAnalysis = struct
  class identifier_collector =
    object (_this)
      inherit [SSet.t, Loc.t] Flow_ast_visitor.visitor ~init:SSet.empty as super

      method! identifier id =
        let (_, { Flow_ast.Identifier.name; _ }) = id in
        let () = acc |> SSet.add name |> super#set_acc in
        id
    end

  let collect_used_names ast =
    let collector = new identifier_collector in
    collector#eval collector#program ast

  type relevant_defs = {
    defs_with_scopes_of_local_uses: (Scope_api.Def.t * Scope_api.Scope.t) list;
    vars_with_shadowed_local_reassignments: (string * Loc.t) list;
  }

  let collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_loc =
    let ( used_defs_within_extracted_statements,
          shadowed_local_reassignments_within_extracted_statements
        ) =
      IMap.fold
        (fun _ { Scope_api.Scope.locals; _ } acc ->
          LocMap.fold
            (fun use def ((used_def_acc, shadowed_local_reassignment_acc) as acc) ->
              if Loc.contains extracted_loc use then
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
                if not (Loc.contains extracted_loc def_loc) then
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
                            let write_loc = Reason.loc_of_reason reason in
                            Loc.contains extracted_loc write_loc)
                        writes
                  in
                  if has_local_reassignment then
                    ( used_def_acc,
                      SMap.add def.Scope_api.Def.actual_name def_loc shadowed_local_reassignment_acc
                    )
                  else
                    acc
                else
                  acc)
            locals
            acc)
        scope_info.Scope_api.scopes
        (Scope_api.DefMap.empty, SMap.empty)
    in
    let defs_with_scopes_of_local_uses =
      IMap.fold
        (fun _ scope acc ->
          let { Scope_api.Scope.defs; _ } = scope in
          SMap.fold
            (fun _ def acc ->
              if Scope_api.DefMap.mem def used_defs_within_extracted_statements then
                Scope_api.DefMap.add def (def, scope) acc
              else
                acc)
            defs
            acc)
        scope_info.Scope_api.scopes
        Scope_api.DefMap.empty
      |> Scope_api.DefMap.values
    in
    let vars_with_shadowed_local_reassignments =
      shadowed_local_reassignments_within_extracted_statements |> SMap.elements |> List.rev
    in
    { defs_with_scopes_of_local_uses; vars_with_shadowed_local_reassignments }

  let undefined_variables_after_extraction
      ~scope_info ~defs_with_scopes_of_local_uses ~new_function_target_scope_loc ~extracted_loc =
    let new_function_target_scopes =
      match new_function_target_scope_loc with
      | Some scope_loc -> Scope_api.scope_of_loc scope_info scope_loc
      | None -> Scope_api.toplevel_scopes
    in
    let to_undefined_variable (def, def_scope) =
      let { Scope_api.Def.locs = (def_loc, _); actual_name; _ } = def in
      if Loc.contains extracted_loc def_loc then
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
          Some (actual_name, def_loc)
        else
          None
    in
    List.filter_map to_undefined_variable defs_with_scopes_of_local_uses |> List.rev

  type escaping_definitions = {
    escaping_variables: (string * Loc.t) list;
    has_external_writes: bool;
  }

  let collect_escaping_local_defs ~scope_info ~ssa_values ~extracted_statements_loc =
    let (escaping_variables, has_external_writes) =
      IMap.fold
        (fun _ { Scope_api.Scope.locals; _ } acc ->
          LocMap.fold
            (fun use def acc ->
              let { Scope_api.Def.locs = (def_loc, _); actual_name; _ } = def in
              if
                Loc.contains extracted_statements_loc def_loc
                && not (Loc.contains extracted_statements_loc use)
              then
                let (variables, has_external_writes) = acc in
                ( SMap.add actual_name def_loc variables,
                  has_external_writes
                  ||
                  match LocMap.find_opt use ssa_values with
                  | None ->
                    (* use is a write. *)
                    (* Since we already know the use is outside of extracted statements,
                       we know this is an external write *)
                    true
                  | Some write_locs ->
                    (* use is a read *)
                    (* find writes pointed to by the read, modulo initialization *)
                    List.exists
                      (function
                        | Ssa_api.Uninitialized -> false
                        | Ssa_api.Write reason ->
                          let write_loc = Reason.loc_of_reason reason in
                          not (Loc.contains extracted_statements_loc write_loc))
                      write_locs
                )
              else
                acc)
            locals
            acc)
        scope_info.Scope_api.scopes
        (SMap.empty, false)
    in
    { escaping_variables = escaping_variables |> SMap.elements |> List.rev; has_external_writes }
end

module TypeSynthesizer = struct
  type synthesizer_context = {
    cx: Context.t;
    file: File_key.t;
    file_sig: File_sig.t;
    typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
    loc_of_aloc: ALoc.t -> Loc.t;
    get_ast_from_shared_mem: File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option;
    get_haste_module_info: File_key.t -> Haste_module_info.t option;
    get_type_sig: File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option;
    type_at_loc_map: (Type.typeparam list * Type.t) LocMap.t;
  }

  class type_collector ~loc_of_aloc (locs : LocSet.t) =
    object (this)
      inherit Typed_ast_finder.type_parameter_mapper as super

      val mutable acc = LocMap.empty

      method! on_loc_annot x = x

      method! on_type_annot x = x

      method collected_types = acc

      method private collect_type_at_loc ~tparams_rev loc t =
        acc <- LocMap.add loc (tparams_rev, t) acc

      method! t_identifier (((aloc, t), _) as ident) =
        let loc = loc_of_aloc aloc in
        if LocSet.mem loc locs then this#annot_with_tparams (this#collect_type_at_loc loc t);
        ident

      method! expression (((aloc, type_), _) as e) =
        let loc = loc_of_aloc aloc in
        if LocSet.mem loc locs then this#annot_with_tparams (this#collect_type_at_loc loc type_);
        super#expression e

      method! type_ (((aloc, type_), _) as ast_type) =
        let loc = loc_of_aloc aloc in
        if LocSet.mem loc locs then this#annot_with_tparams (this#collect_type_at_loc loc type_);
        super#type_ ast_type
    end

  class generic_name_collector =
    object
      inherit [Subst_name.Set.t] Type_visitor.t as super

      method! type_ cx pole acc ty =
        let open Type in
        match ty with
        | GenericT { id; _ } ->
          Generic.fold_ids ~f:(fun _ name acc -> Subst_name.Set.add name acc) ~acc id
        | _ -> super#type_ cx pole acc ty
    end

  let keep_used_tparam_rev ~cx ~tparams_rev ~type_ =
    let generic_name_collector = new generic_name_collector in
    let collect_used_generic_names t acc = generic_name_collector#type_ cx Polarity.Neutral acc t in
    let appeared_generic_names = collect_used_generic_names type_ Subst_name.Set.empty in
    (* It's not enough to only collect used generic names from the type, but also the generic
       parameters themselves, since constraints on generic parameter may cause them to refer
       to each other. e.g. <A, B: A, C: A = B>.

       The following fold starts from the end and folds to the first, adding in names
       used in the constraints along the way. *)
    tparams_rev
    |> List.fold_left
         (fun (tparams, appeared_generic_names) ({ Type.name; bound; default; _ } as tparam) ->
           if Subst_name.Set.mem name appeared_generic_names then
             let appeared_generic_names = collect_used_generic_names bound appeared_generic_names in
             let appeared_generic_names =
               match default with
               | None -> appeared_generic_names
               | Some default -> collect_used_generic_names default appeared_generic_names
             in
             (tparam :: tparams, appeared_generic_names)
           else
             (tparams, appeared_generic_names))
         ([], appeared_generic_names)
    |> fst
    |> List.rev

  let create_synthesizer_context
      ~cx
      ~file
      ~file_sig
      ~typed_ast
      ~loc_of_aloc
      ~get_ast_from_shared_mem
      ~get_haste_module_info
      ~get_type_sig
      ~locs =
    let collector = new type_collector ~loc_of_aloc locs in
    ignore (collector#program typed_ast);
    let type_at_loc_map = collector#collected_types in
    {
      cx;
      file;
      file_sig;
      typed_ast;
      loc_of_aloc;
      get_ast_from_shared_mem;
      get_haste_module_info;
      get_type_sig;
      type_at_loc_map;
    }

  type type_synthesizer_with_import_adder = {
    type_param_synthesizer:
      Type.typeparam -> ((Loc.t, Loc.t) Flow_ast.Type.TypeParam.t, Insert_type.expected) result;
    type_synthesizer:
      Loc.t ->
      ((Type.typeparam list * (Loc.t, Loc.t) Flow_ast.Type.t) option, Insert_type.expected) result;
    added_imports: unit -> (string * Autofix_imports.bindings) list;
  }

  let create_type_synthesizer_with_import_adder
      {
        cx;
        file;
        file_sig;
        typed_ast;
        loc_of_aloc;
        get_ast_from_shared_mem;
        get_haste_module_info;
        get_type_sig;
        type_at_loc_map;
      } =
    let remote_converter =
      new Insert_type_imports.ImportsHelper.remote_converter
        ~loc_of_aloc
        ~file_options:(Context.file_options cx)
        ~get_haste_module_info
        ~get_type_sig
        ~iteration:0
        ~file
        ~reserved_names:SSet.empty
    in
    let synth_type loc t =
      try
        Insert_type.synth_type
          ~cx
          ~loc_of_aloc
          ~get_ast_from_shared_mem
          ~file_sig
          ~typed_ast
          ~omit_targ_defaults:false
          ~remote_converter
          loc
          t
      with
      | Insert_type.(FailedToInsertType (Expected expected)) -> Error expected
    in
    let open Base.Result.Let_syntax in
    let type_param_synthesizer { Type.name; bound; polarity; default; _ } =
      let%bind bound =
        match%map bound |> synth_type Loc.none with
        | (_, (_, Flow_ast.Type.Mixed _))
        | (_, (_, Flow_ast.Type.Unknown _)) ->
          Flow_ast.Type.Missing Loc.none
        | bound -> Flow_ast.Type.Available bound
      in
      let variance =
        match polarity with
        | Polarity.Neutral -> None
        | Polarity.Positive -> Some (Loc.none, Flow_ast.Variance.{ kind = Plus; comments = None })
        | Polarity.Negative -> Some (Loc.none, Flow_ast.Variance.{ kind = Minus; comments = None })
      in
      let%map default =
        match default with
        | None -> return None
        | Some default ->
          let%map (_, ast) = default |> synth_type Loc.none in
          Some ast
      in
      Ast_builder.Types.type_param ~bound ?variance ?default (Subst_name.string_of_subst_name name)
    in
    let type_synthesizer loc =
      match LocMap.find_opt loc type_at_loc_map with
      | None -> return None
      | Some (tparams_rev, type_) ->
        let%map (_, ast_type) = synth_type loc type_ in
        Some (keep_used_tparam_rev ~cx ~tparams_rev ~type_, ast_type)
    in
    {
      type_param_synthesizer;
      type_synthesizer;
      added_imports = (fun () -> remote_converter#to_import_bindings);
    }
end
