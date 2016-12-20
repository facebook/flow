open Utils_js
open Reason
open Type

module Ast = Graphql_ast
module Flow = Flow_js
module Schema = Graphql_schema

type context = {
  mutable vars: Schema.Type.t SMap.t;
  infer_vars: bool;
  relay_op: bool;
}


let rec doc cx doc =
  let schema =
    match Context.graphql_config cx with
    | Some inst -> inst.Graphql_config.schema
    | None -> failwith "impossible"
  in
  match doc.Ast.Document.definitions with
  | [] -> VoidT.at Loc.none
  | def :: rest ->
    List.iter (fun d -> ignore (do_definition cx schema d)) rest;
    do_definition cx schema def

and do_definition cx schema def =
  match def with
  | Ast.Definition.Fragment { Ast.FragmentDef.
      typeCondition = (type_loc, type_name);
      selectionSet;
      loc;
      _
    } ->
    let reason = mk_reason
        (RCustom (spf "fragment on `%s`" type_name)) loc in
    if Graphql_flow.check_frag_type cx schema type_name type_loc then
      let gcx = {vars = SMap.empty; infer_vars = true; relay_op = false} in
      let selection =
        mk_selection cx gcx schema type_name selectionSet in
      let frag = { Graphql.
        frag_schema = schema;
        frag_type = type_name;
        frag_selection = selection;
      } in
      GraphqlFragT (reason, frag)
    else
      EmptyT.why reason
  | Ast.Definition.Operation op_def ->
    let { Ast.OperationDef.
      operation = (op_loc, operation);
      selectionSet;
      variableDefs;
      loc;
      _;
    } = op_def in
    let op_type = Ast.OperationType.(match operation with
      | Query -> Schema.Query
      | Mutation -> Schema.Mutation
      | Subscription -> Schema.Subscription
    ) in
    let (type_name, op_name) = Schema.(
      match op_type with
      | Query -> Some schema.Schema.query_name, "query"
      | Mutation -> schema.Schema.mutation_name, "mutation"
      | Subscription -> schema.Schema.subscription_name, "subscription"
    ) in
    let reason = mk_reason (RCustom (spf "GraphQL `%s`" op_name)) loc in
    (match type_name with
    | Some type_name ->
      (* Let's just hardcode Relay behavior for now. If operation contains only
         one field with no args and no selections, we do the following:
         - use args of this field as operation variables
         - do not require selection for this field even if it's object type
       *)
      let relay_op = is_relay_op op_def in
      let (vars, infer_vars) = match variableDefs with
        | Some var_defs -> (do_vars_decl cx schema var_defs, false)
        | None -> (SMap.empty, true)
      in
      let gcx = {vars; infer_vars; relay_op} in
      let selection = mk_selection cx gcx schema type_name selectionSet in
      let vars = gcx.vars in
      let operation = { Graphql.
        op_schema = schema;
        op_type;
        op_vars = vars;
        op_selection = selection;
      } in
      GraphqlOpT (reason, operation)
    | None ->
      let loc = Option.value op_loc ~default:Loc.none in
      Flow_error.(add_output cx (EGraphqlUndefOp (loc, op_name)));
      EmptyT.why reason
    )
  | _ ->
    (* TODO: Report an error *)
    VoidT.at Loc.none

and is_relay_op op = Ast.(
  let check_field = function
    | Selection.Field {Field.selectionSet = None; _} -> true
    | _ -> false
  in
  List.for_all check_field op.OperationDef.selectionSet.SelectionSet.selections
)

and do_vars_decl cx schema var_defs =
  let rec conv t =
    match t with
    | Ast.Type.Named (loc, name) ->
      if Schema.type_exists schema name
      then match Schema.type_def schema name with
        | Schema.Type.Scalar _
        | Schema.Type.Enum _
        | Schema.Type.InputObj _
          -> Some (Schema.Type.Named name)
        | _ ->
          Flow_error.(add_output cx (EGraphqlNotInputType (loc, name)));
          None
      else (
        Flow_error.(add_output cx (EGraphqlTypeNotFound (loc, name)));
        None
      )
    | Ast.Type.List (_, t) ->
      Option.map ~f:(fun t -> Schema.Type.List t) (conv t)
    | Ast.Type.NonNull (loc, Ast.Type.NonNull _) ->
      (* `Type!!` *)
      Flow_error.(add_output cx (EGraphqlDoubleNonNull loc));
      None
    | Ast.Type.NonNull (_, t) ->
      Option.map ~f:(fun t -> Schema.Type.NonNull t) (conv t)
  in
  let vars =
    List.fold_left (fun vars var_def ->
      let {
        Ast.VariableDef.variable = (loc, (_, name));
        type_;
        _;
      } = var_def in
      match conv type_ with
      | Some t ->
        if SMap.mem name vars then (
          Flow_error.(add_output cx (EGraphqlVarRedef (loc, name)));
          vars
        ) else SMap.add name t vars
      | None -> vars
    ) SMap.empty var_defs
  in
  vars

and mk_selection cx gcx schema type_name selection_set =
  let {Ast.SelectionSet.selections; loc} = selection_set in
  let reason = mk_reason (RCustom (spf "selection on `%s`" type_name)) loc in
  let s_selections = SMap.from_keys
    (Schema.get_possible_types schema type_name)
    (fun _ -> SMap.empty)
  in
  let selection = { Graphql.
    s_schema = schema;
    s_on = type_name;
    s_selections;
  } in
  select
    cx gcx schema (GraphqlSelectionT (reason, selection)) type_name selections

and select cx gcx schema selection type_name selections =
  List.fold_left (fun selection item ->
    match item with
    | Ast.Selection.Field { Ast.Field.
        alias;
        name = (floc, fname);
        args;
        selectionSet;
        _
      } ->
      if Type_inference_hooks_js.dispatch_graphql_field_hook cx fname floc selection
      then selection
      else begin
        (* TODO *)
        let reason = mk_reason (RCustom (spf "field `%s`" fname)) floc in
        if Graphql_flow.check_field cx schema type_name fname floc then (
          let field_type =
            Schema.find_field_type schema type_name fname in
          let _fields_args =
            let args = Option.value args ~default:[] in
            do_field_args cx gcx schema
              (Schema.find_field schema type_name fname) floc args
          in
          let field_selection = do_field_selection
              cx gcx schema
              (Schema.name_of_type field_type)
              floc selectionSet in
          let field = {
            Graphql.sf_schema = schema;
            sf_alias = Option.value_map alias ~default:fname ~f:(fun (_, x) -> x);
            sf_name = fname;
            sf_type = field_type;
            sf_selection = field_selection;
          } in
          let field = GraphqlFieldT (reason, field) in
          Hashtbl.replace (Context.type_table cx) floc field;
          Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
            let field = Graphql.SelectField field in
            Flow.flow cx (selection, GraphqlSelectT (reason, field, t))
          )
        ) else (
          Option.iter ~f:(skim_selection_set cx gcx schema) selectionSet;
          selection
        )
      end
    | Ast.Selection.InlineFragment { Ast.InlineFragment.
        typeCondition;
        selectionSet;
        loc = frag_loc;
        _;
      }
      ->
      let (frag_type_loc, frag_type) =
        Option.value typeCondition ~default:(Loc.none, type_name) in
      if Graphql_flow.check_frag_type cx schema frag_type frag_type_loc then
        let reason = mk_reason (RCustom "inline fragment") frag_loc in
        let frag = { Graphql.
          frag_schema = schema;
          frag_type;
          frag_selection = mk_selection cx gcx schema frag_type selectionSet;
        } in
        Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
          let frag = Graphql.SelectFrag (GraphqlFragT (reason, frag)) in
          Flow.flow cx (selection, GraphqlSelectT (reason, frag, t))
        )
      else selection
    | Ast.Selection.FragmentSpread { Ast.FragmentSpread.
        name = (_loc, _name);
        _;
      }
      ->
      Flow.mk_tvar_where cx (reason_of_t selection) (fun t ->
        Flow.flow_t cx (selection, t)
      )
    | Ast.Selection.JS _ -> selection
  ) selection selections

and skim_selection_set cx gcx schema selection_set =
  List.iter (fun item ->
    match item with
    | Ast.Selection.InlineFragment {
        Ast.InlineFragment.typeCondition = Some (type_loc, type_name);
        selectionSet;
        _;
      } ->
      if Graphql_flow.check_frag_type cx schema type_name type_loc
      then mk_selection cx gcx schema type_name selectionSet |> ignore
    | _ -> ()
  ) selection_set.Ast.SelectionSet.selections

and do_field_selection cx gcx schema type_name loc selection_set =
  let need_selection = Schema.Type.(
    match Schema.type_def schema type_name with
    | Obj _ | Interface _ | Union _ -> true
    | _ -> false
  ) in
  match selection_set with
  (* obj { ... } *)
  | Some selection_set when need_selection ->
    Some (mk_selection cx gcx schema type_name selection_set)
  (* scalar { ... } *)
  | Some {Ast.SelectionSet.loc; _} ->
    Flow_error.(add_output cx (EGraphqlNonObjSelect (loc, type_name)));
    None
  (* obj *)
  | None when (need_selection && (not gcx.relay_op)) ->
    Flow_error.(add_output cx (EGraphqlObjNeedSelect (loc, type_name)));
    None
  (* scalar *)
  | None -> None

and do_field_args cx gcx schema field field_loc args =
  let args_map = List.fold_left (fun map arg ->
    let {Ast.Argument.name = (_, name); _} = arg in
    SMap.add name arg map
  ) SMap.empty args in
  SMap.iter (fun _ {Schema.InputVal.name; type_} ->
    match type_ with
    | Schema.Type.NonNull _ when not (SMap.mem name args_map) ->
      err cx field_loc (spf "Missings required argument `%s`" name)
    | _ -> ()
  ) field.Schema.Field.args;
  List.fold_left (fun map {Ast.Argument.name = (loc, name); value; _} ->
    match Schema.get_arg_type schema field name with
    | Some arg_type ->
      do_value cx gcx schema arg_type value;
      map
    | None ->
      err cx loc (spf "Field does not have argument `%s`" name);
      map
  ) SMap.empty args

and do_value cx gcx schema type_ value =
  let open Ast.Value in
  let module Ast = Ast in
  let type_name = Schema.type_name schema type_ in
  let check_scalar loc input =
    let err () =
      err cx loc
        (spf "This value expected to be of type `%s`" type_name)
    in
    match Schema.type_def schema type_name with
    | Schema.Type.Scalar (_, kind) ->
      if not (Schema.can_convert_to input kind) then err ()
    | _ -> err ()
  in
  match value with
  | Variable (loc, (_, name)) -> do_var cx gcx loc name type_
  | IntValue (loc, str) ->
    begin
      try
        Int32.of_string str |> Int32.to_int |> ignore;
        check_scalar loc Schema.Type.Int
      with _ -> err cx loc "This is not a valid int32 value"
    end
  | FloatValue (loc, _) -> check_scalar loc Schema.Type.Float
  | StringValue (loc, _) -> check_scalar loc Schema.Type.Str
  | BooleanValue (loc, _) -> check_scalar loc Schema.Type.Bool
  | NullValue loc ->
    begin match type_ with
    | Schema.Type.NonNull _ -> err cx loc "can't be null"
    | _ -> ()
    end
  | EnumValue (loc, value) ->
    begin match Schema.type_def schema type_name with
    | Schema.Type.Enum (_, values) ->
      if not (List.exists ((=) value) values) then
        err cx loc
          (spf "This value expected to be of enum type `%s`" type_name)
    | _ ->
      err cx loc
        (spf "This value expected to be of type `%s`" type_name)
    end
  | ListValue (loc, items) ->
    let rec check_list = (function
      | Schema.Type.Named _ ->
        err cx loc
          (spf "This value expected to be of type `%s`, found list instead"
             type_name)
      | Schema.Type.List t ->
        List.iter (do_value cx gcx schema t) items
      | Schema.Type.NonNull t -> check_list t
    ) in
    check_list type_
  | ObjectValue {Ast.ObjectValue.fields; loc} ->
    begin match Schema.type_def schema type_name with
    | Schema.Type.InputObj (_, field_defs) ->
      let fields_map = List.fold_left (fun map field ->
        let {Ast.ObjectField.name = (_, name); value; _} = field in
        SMap.add name value map
      ) SMap.empty fields in
      SMap.iter (fun field_name field_type ->
        let missing =
          match field_type.Schema.InputVal.type_ with
          | Schema.Type.NonNull _ ->
            not (SMap.mem field_name fields_map)
          | _ -> false
        in
        if missing then
          err cx loc
            (spf "Missing required field `%s` in object of type `%s`"
               field_name type_name)
      ) field_defs;
      List.iter (fun {Ast.ObjectField.name = (loc, name); value; _} ->
        match SMap.get name field_defs with
        | Some {Schema.InputVal.type_ = field_type; _} ->
          do_value cx gcx schema field_type value
        | None ->
          err cx loc
            (spf "Field `%s` is not found in `%s`" name type_name)
      ) fields
    | _ -> err cx loc "This value expected to be an input object"
    end

and do_var cx gcx loc name type_ =
  if SMap.mem name gcx.vars
  then vars_compatible cx loc (SMap.find name gcx.vars) type_
  else if gcx.infer_vars then (
    let new_vars = SMap.add name type_ gcx.vars in
    gcx.vars <- new_vars
  ) else (err cx loc (spf "Variable `%s` in not declared" name))

and vars_compatible cx loc t1 t2 =
  let module T = Schema.Type in
  let rec check t1 t2 =
    match t1, t2 with
    | T.Named n1, T.Named n2 -> n1 = n2
    | T.List t1, T.List t2 -> check t1 t2
    | T.NonNull t1, T.NonNull t2 -> check t1 t2
    | _, _ -> false
  in
  let valid = check t1 t2 in
  if not valid then (
    let t_exp = Schema.string_of_type_ref t1 in
    let t_real = Schema.string_of_type_ref t2 in
    err cx loc
      (spf "Expected variable of type `%s`, but got `%s`" t_exp t_real);
  );
  ()

and err cx loc msg =
  Flow_error.(add_output cx (EGraphqlCustom (loc, msg)))
