(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
module Ssa_api = Ssa_api.With_Loc
open Loc_collections

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

  class function_and_method_insertion_point_collector reader extracted_loc =
    object (this)
      inherit Typed_ast_utils.type_parameter_mapper as super

      val mutable acc = []

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

      method private function_with_name ?name ?(is_method = false) function_declaration =
        let open Flow_ast in
        match function_declaration with
        | { Function.id; body = Function.BodyBlock (block_aloc, _); tparams; _ } ->
          (match (id, name) with
          | (None, None) -> ()
          | (Some (_, { Identifier.name; _ }), _)
          | (None, Some (_, { Identifier.name; _ })) ->
            let block_loc = Parsing_heaps.Reader.loc_of_aloc ~reader block_aloc in
            if Loc.contains block_loc extracted_loc then
              this#type_params_opt tparams (fun _ ->
                  this#collect_name_and_loc ~is_method name block_loc |> this#annot_with_tparams))
        | _ -> ()

      method! function_ function_declaration =
        let () = this#function_with_name function_declaration in
        super#function_ function_declaration

      method! variable_declarator ~kind decl =
        let open Flow_ast in
        let (_, { Statement.VariableDeclaration.Declarator.id; init }) = decl in
        match (id, init) with
        | ( (_, Pattern.Identifier { Pattern.Identifier.name; _ }),
            Some (_, (Expression.ArrowFunction f | Expression.Function f)) ) ->
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

  let collect_function_method_inserting_points ~typed_ast ~reader ~extracted_loc =
    let collector = new function_and_method_insertion_point_collector reader extracted_loc in
    collector#function_inserting_locs_with_typeparams typed_ast

  class class_insertion_point_collector reader extracted_statements_loc =
    object (this)
      inherit Typed_ast_utils.type_parameter_mapper as super

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
        let body_loc = Parsing_heaps.Reader.loc_of_aloc ~reader body_aloc in
        if Loc.contains extracted_statements_loc body_loc then
          (* When the class is nested inside the extracted statements, we stop recursing down. *)
          class_declaration
        else if Loc.contains body_loc extracted_statements_loc then
          let id =
            match id with
            | None -> None
            | Some (_, { Identifier.name; _ }) -> Some name
          in
          let () =
            this#type_params_opt tparams (fun _ ->
                this#collect_name_and_loc id body_loc |> this#annot_with_tparams)
          in
          super#class_ class_declaration
        else
          super#class_ class_declaration
    end

  let find_closest_enclosing_class ~typed_ast ~reader ~extracted_statements_loc =
    let collector = new class_insertion_point_collector reader extracted_statements_loc in
    collector#closest_enclosing_class_scope typed_ast
end

module AstExtractor = struct
  type expression_with_statement_loc = {
    containing_statement_locs: Loc.t list;
    expression: (Loc.t, Loc.t) Flow_ast.Expression.t;
  }

  type type_with_statement_loc = {
    directly_containing_statement_loc: Loc.t;
    type_: (Loc.t, Loc.t) Flow_ast.Type.t;
  }

  type extracted = {
    extracted_statements: (Loc.t, Loc.t) Flow_ast.Statement.t list option;
    extracted_expression: expression_with_statement_loc option;
    extracted_type: type_with_statement_loc option;
  }

  (* Collect all statements that are completely within the selection.
     Collect a single expression that is completely within the selection. *)
  class collector (extract_range : Loc.t) =
    object (this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      val mutable containing_statement_locs = []

      val mutable collect_statement = true

      val mutable collected_expression = None

      val mutable collected_type = None

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
        let saved_containing_statement_locs = containing_statement_locs in
        let () = containing_statement_locs <- statement_loc :: containing_statement_locs in
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
                  collected_expression <- Some { containing_statement_locs; expression }
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
        let () = containing_statement_locs <- saved_containing_statement_locs in
        result

      method! expression expr =
        let (expression_loc, _) = expr in
        if Loc.equal extract_range expression_loc then
          (* Only collect expression when the selection is an exact match. *)
          let () = collected_expression <- Some { containing_statement_locs; expression = expr } in
          expr
        else if Loc.contains expression_loc extract_range then
          (* If the range is completely contained in the expression,
             we should recursve deeper to find smaller nested expressions that are contained in the
             range. *)
          super#expression expr
        else
          (* In all other cases, selection is illegal. *)
          expr

      method! type_ t =
        let (type_loc, _) = t in
        if Loc.equal extract_range type_loc then
          (* Only collect type when the selection is an exact match. *)
          let () =
            match containing_statement_locs with
            | [] -> ()
            | directly_containing_statement_loc :: _ ->
              collected_type <- Some { directly_containing_statement_loc; type_ = t }
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
    end

  let extract ast extract_range =
    let collector = new collector extract_range in
    let _ = collector#program ast in
    collector#extracted
end

module InformationCollectors = struct
  class extracted_statements_information_collector =
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
    let information_collector = new extracted_statements_information_collector in
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
end

module RefactorProgramMappers = struct
  class replace_original_statements_mapper ~extracted_statements_loc ~function_call_statements =
    object (_this)
      inherit [Loc.t] Flow_ast_mapper.mapper as super

      method! statement_fork_point stmt =
        let (statement_loc, _) = stmt in
        if Loc.contains extracted_statements_loc statement_loc then
          if Loc.equal (Loc.start_loc extracted_statements_loc) (Loc.start_loc statement_loc) then
            function_call_statements
          else
            []
        else
          super#statement_fork_point stmt
    end

  class extract_to_function_refactor_mapper
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
            { body with body = super#statement_list body.body @ [function_declaration_statement] }
          )
        else
          super#function_body block
    end

  let extract_to_function
      ~target_body_loc
      ~extracted_statements_loc
      ~function_call_statements
      ~function_declaration_statement
      ast =
    let mapper =
      new extract_to_function_refactor_mapper
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~function_declaration_statement
    in
    mapper#program ast

  class extract_to_method_refactor_mapper
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
            } )
        else
          super#class_body block
    end

  let extract_to_method
      ~target_body_loc ~extracted_statements_loc ~function_call_statements ~method_declaration ast =
    let mapper =
      new extract_to_method_refactor_mapper
        ~target_body_loc
        ~extracted_statements_loc
        ~function_call_statements
        ~method_declaration
    in

    mapper#program ast

  class extract_to_type_alias_refactor_mapper ~statement_loc ~type_loc ~type_replacement ~type_alias
    =
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

  let extract_to_type_alias ~statement_loc ~type_loc ~type_replacement ~type_alias ast =
    let mapper =
      new extract_to_type_alias_refactor_mapper
        ~statement_loc
        ~type_loc
        ~type_replacement
        ~type_alias
    in
    mapper#program ast
end

module VariableAnalysis = struct
  type relevant_defs = {
    defs_with_scopes_of_local_uses: (Scope_api.Def.t * Scope_api.Scope.t) list;
    vars_with_shadowed_local_reassignments: (string * Loc.t) list;
  }

  let collect_relevant_defs_with_scope ~scope_info ~ssa_values ~extracted_loc =
    let ( used_defs_within_extracted_statements,
          shadowed_local_reassignments_within_extracted_statements ) =
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
                            let write_loc = Reason.poly_loc_of_reason reason in
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
        shadowed_local_reassignments_within_extracted_statements |> SMap.elements |> List.rev;
    }

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
                          let write_loc = Reason.poly_loc_of_reason reason in
                          not (Loc.contains extracted_statements_loc write_loc))
                      write_locs )
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
    full_cx: Context.t;
    file: File_key.t;
    file_sig: File_sig.With_ALoc.t;
    typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
    type_at_loc_map: Type.TypeScheme.t LocMap.t;
  }

  class type_collector reader (locs : LocSet.t) =
    object (this)
      inherit Typed_ast_utils.type_parameter_mapper as super

      val mutable acc = LocMap.empty

      method! on_loc_annot x = x

      method! on_type_annot x = x

      method collected_types = acc

      method private collect_type_at_loc ~tparams_rev loc t =
        acc <- LocMap.add loc { Type.TypeScheme.tparams_rev; type_ = t } acc

      method! t_identifier (((aloc, t), _) as ident) =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        if LocSet.mem loc locs then this#annot_with_tparams (this#collect_type_at_loc loc t);
        ident

      method! type_ (((aloc, type_), _) as ast_type) =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        if LocSet.mem loc locs then this#annot_with_tparams (this#collect_type_at_loc loc type_);
        super#type_ ast_type
    end

  class generic_name_collector =
    object
      inherit [SSet.t] Type_visitor.t as super

      method! type_ cx pole acc ty =
        let open Type in
        match ty with
        | BoundT (_, name) -> SSet.add name acc
        | GenericT { id; _ } -> Generic.fold_ids ~f:(fun _ name acc -> SSet.add name acc) ~acc id
        | _ -> super#type_ cx pole acc ty
    end

  let keep_used_tparam_rev ~cx ~tparams_rev ~type_ =
    let generic_name_collector = new generic_name_collector in
    let collect_used_generic_names t acc = generic_name_collector#type_ cx Polarity.Neutral acc t in
    let appeared_generic_names = collect_used_generic_names type_ SSet.empty in
    (* It's not enough to only collect used generic names from the type, but also the generic
       parameters themselves, since constraints on generic parameter may cause them to refer
       to each other. e.g. <A, B: A, C: A = B>.

       The following fold starts from the end and folds to the first, adding in names
       used in the constraints along the way. *)
    tparams_rev
    |> List.fold_left
         (fun (tparams, appeared_generic_names) ({ Type.name; bound; default; _ } as tparam) ->
           if SSet.mem name appeared_generic_names then
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

  let create_synthesizer_context ~full_cx ~file ~file_sig ~typed_ast ~reader ~locs =
    let collector = new type_collector reader locs in
    ignore (collector#program typed_ast);
    let type_at_loc_map = collector#collected_types in
    { full_cx; file; file_sig; typed_ast; type_at_loc_map }

  type type_synthesizer_with_import_adder = {
    type_param_synthesizer:
      Type.typeparam list -> Type.typeparam -> (Loc.t, Loc.t) Flow_ast.Type.TypeParam.t;
    type_synthesizer: Loc.t -> (Type.typeparam list * (Loc.t, Loc.t) Flow_ast.Type.t) option;
    added_imports: unit -> (string * Autofix_imports.bindings) list;
  }

  let create_type_synthesizer_with_import_adder
      { full_cx; file; file_sig; typed_ast; type_at_loc_map } =
    let remote_converter =
      new Insert_type_imports.ImportsHelper.remote_converter
        ~iteration:0
        ~file
        ~reserved_names:SSet.empty
    in
    let synth_type =
      Insert_type.synth_type
        ~full_cx
        ~file_sig
        ~typed_ast
        ~expand_aliases:true
        ~omit_targ_defaults:false
        ~ambiguity_strategy:Autofix_options.Generalize
        ~remote_converter
    in
    let type_param_synthesizer tparams_rev { Type.name; bound; polarity; default; _ } =
      let type_scheme_of_type type_ = { Type.TypeScheme.tparams_rev; type_ } in
      let bound =
        match bound |> type_scheme_of_type |> synth_type Loc.none with
        | (_, (_, Flow_ast.Type.Mixed _)) -> Flow_ast.Type.Missing Loc.none
        | bound -> Flow_ast.Type.Available bound
      in
      let variance =
        match polarity with
        | Polarity.Neutral -> None
        | Polarity.Positive -> Some (Loc.none, Flow_ast.Variance.{ kind = Plus; comments = None })
        | Polarity.Negative -> Some (Loc.none, Flow_ast.Variance.{ kind = Minus; comments = None })
      in
      let default =
        match default with
        | None -> None
        | Some default -> Some (default |> type_scheme_of_type |> synth_type Loc.none |> snd)
      in
      Ast_builder.Types.type_param ~bound ?variance ?default name
    in
    let type_synthesizer loc =
      match LocMap.find_opt loc type_at_loc_map with
      | None -> None
      | Some ({ Type.TypeScheme.tparams_rev; type_ } as type_scheme) ->
        let (_, ast_type) = synth_type loc type_scheme in
        Some (keep_used_tparam_rev ~cx:full_cx ~tparams_rev ~type_, ast_type)
    in
    {
      type_param_synthesizer;
      type_synthesizer;
      added_imports = (fun () -> remote_converter#to_import_bindings);
    }
end
