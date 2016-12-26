open Type

module FlowError = Flow_error

type flow = {
  flow: Context.t -> ?trace: Trace.t -> Type.t * Type.use_t -> unit;
  flow_t: Context.t -> ?trace: Trace.t -> Type.t * Type.t -> unit;
  add_output: Context.t -> ?trace: Trace.t -> Flow_error.error_message -> unit;
  mk_tvar: Context.t -> Reason.t -> Type.t;
  mk_tvar_where: Context.t -> Reason.t -> (Type.t -> unit) -> Type.t;
}

(****************)
(* Constructing *)
(****************)

let check_frag_type flow cx schema type_name loc = Graphql_schema.Type.(
  if not (Graphql_schema.type_exists schema type_name) then (
    flow.add_output cx (FlowError.EGraphqlTypeNotFound (loc, type_name));
    false
  ) else
    match Graphql_schema.type_def schema type_name with
    | Obj _ | Interface _ | Union _ -> true
    | Scalar _ | Enum _ | InputObj _ ->
      flow.add_output cx (FlowError.EGraphqlFragOnNonComposite (loc, type_name));
      false
)

let check_field flow cx schema parent_type name loc =
  match Graphql_schema.type_def schema parent_type with
  | Graphql_schema.Type.Union _ when name <> "__typename" ->
    flow.add_output cx (FlowError.EGraphqlUnionSelect (loc, parent_type));
    false
  | _ ->
    if Graphql_schema.get_field_type schema parent_type name <> None then true
    else (
      flow.add_output cx (FlowError.EGraphqlFieldNotFound (loc, parent_type));
      false
    )

(**************)
(* Converting *)
(**************)

let rec conv_val_ ?non_null:(non_null=true) mk_obj schema reason value =
  let module T = Graphql_schema.Type in
  let wrap v = if non_null then MaybeT v else v in
  match value with
  | T.Named name ->
    let value = match Graphql_schema.type_def schema name with
      | T.Scalar (_, kind) -> (match kind with
          | T.Str -> StrT.why reason
          | T.Bool -> BoolT.why reason
          | T.Float
          | T.Int -> NumT.why reason
        )
      | T.InputObj (_, vars) ->
        let vars = SMap.map (fun iv -> iv.Graphql_schema.InputVal.type_) vars in
        conv_values_map mk_obj schema reason vars
      | _ -> MixedT.why reason
    in
    wrap value
  | T.List t ->
    let t = conv_val_ mk_obj schema reason t in
    wrap (ArrT (reason, t, []))
  | T.NonNull t -> conv_val_ ~non_null:false mk_obj schema reason t
  

and conv_values_map mk_obj schema reason vars =
  let props = SMap.map (fun type_ ->
    let t = conv_val_ mk_obj schema reason type_ in
    Property.field Neutral t
  ) vars in
  mk_obj reason props

(***********)
(* Merging *)
(***********)

let check_field_selection _field = true

let merge_field flow cx ?trace f1 f2 = Graphql.(
  (* TODO: validate args *)
  match f1.sf_selection, f2.sf_selection with
  | Some s1, Some s2 ->
    let reason = reason_of_t s2 in
    let new_s = flow.mk_tvar cx (reason_of_t s1) in
    flow.flow cx ?trace (s1, GraphqlSelectT (reason, SelectFrag s2, new_s));
    { f1 with sf_selection = Some new_s }
  | _ -> f1
)

let merge_fun flow cx ?trace _ f1 f2 =
  match (f1, f2) with
  | Some f1, Some f2 -> Some (merge_field flow cx ?trace f1 f2)
  | Some _, None -> f1
  | None, Some _ -> f2
  | None, None -> None

let add_field flow cx ?trace fields f =
  let alias = f.Graphql.sf_alias in
  let f =
    if SMap.mem alias fields
    then merge_field flow cx ?trace (SMap.find alias fields) f
    else f
  in
  SMap.add alias f fields

let merge_fields flow cx ?trace fields1 fields2 =
  SMap.merge (merge_fun flow cx ?trace) fields1 fields2
