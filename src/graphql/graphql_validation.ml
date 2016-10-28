open Utils_js
module Schema = Graphql_schema

let rec check_type schema t errors =
  match t with
  | Schema.Type.Named name ->
    if not (Graphql_schema.type_exists schema name)
    then
      (spf "Referenced type `%s` is not defined." name) :: errors
    else errors
  | Schema.Type.List t -> check_type schema t errors
  | Schema.Type.NonNull t -> check_type schema t errors

let check_map check schema map errors =
  SMap.fold (fun _ item errors -> check schema item errors) map errors

let check_input_val schema value errors =
  check_type schema value.Schema.InputVal.type_ errors

let check_field schema field errors =
  errors
    |> check_type schema field.Schema.Field.type_
    |> check_map check_input_val schema field.Schema.Field.args

let check_type_refs schema errors =
  SMap.fold (fun _ type_def errors ->
    Schema.Type.(match type_def with
      | Obj (_, fields, interfaces) ->
        let errors = check_map check_field schema fields errors in
        let errors = List.fold_left (fun errors interface ->
          if not (Schema.type_exists schema interface)
          then (spf "Interface `%s` is not defined." interface) :: errors
          else errors
        ) errors interfaces in
        errors
      | Interface (_, fields) ->
        check_map check_field schema fields errors
      | InputObj (_, values) ->
        check_map check_input_val schema values errors
      | Scalar _
      | Union _
      | Enum _
        -> errors
    )
  ) schema.Schema.type_map errors

let check_op_types schema errors =
  let check_type op_name type_name errors = match type_name with
    | Some n when not (Schema.type_exists schema n) ->
      (spf "%s type `%s` not defined." op_name n) :: errors
    | _ -> errors
  in
  errors
    |> check_type "Query" (Some schema.Schema.query_name)
    |> check_type "Mutation" schema.Schema.mutation_name
    |> check_type "Subscription" schema.Schema.subscription_name

(**
 * Check that all referenced in schema types are defined.
 * TODO: Would be better to validate ast so we can add locations into errors.
 *)
let validate_schema schema =
  []
    |> check_type_refs schema
    |> check_op_types schema
