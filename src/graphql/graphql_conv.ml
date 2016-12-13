module Ast = Graphql_ast
module Schema = Graphql_schema

let names_to_strings names = List.map (fun (_, name) -> name) names

let rec conv_type t =
  match t with
  | Ast.Type.Named (_, name) -> Schema.Type.Named name
  | Ast.Type.List (_, t) -> Schema.Type.List (conv_type t)
  | Ast.Type.NonNull (_, t) -> Schema.Type.NonNull (conv_type t)

let conv_input_vals values =
  List.fold_left (fun map value ->
    let (_, name) = value.Ast.InputValueDef.name in
    let type_ = conv_type value.Ast.InputValueDef.type_ in
    let value = { Schema.InputVal.
      name;
      type_;
    } in
    SMap.add name value map
  ) SMap.empty values

let conv_fields fields =
  List.fold_left (fun map field ->
    let (_, name) = field.Ast.FieldDef.name in
    let type_ = conv_type field.Ast.FieldDef.type_ in
    let field = {
      Schema.Field.name = name;
      args = conv_input_vals field.Ast.FieldDef.args;
      type_;
    } in
    SMap.add name field map
  ) SMap.empty fields

let schema_from_ast scalar_kinds doc =
  let query = ref None in
  let mutation = ref None in
  let subscription = ref None in
  let type_map = ref SMap.empty in

  let add_type name type_ = type_map := SMap.add name type_ !type_map in

  let consume_def def =
    let module Def = Ast.Definition in
    match def with
    | Def.ScalarType scalar ->
        let (_, name) = scalar.Ast.ScalarTypeDef.name in
        let kind =
          Option.value (SMap.get name scalar_kinds) ~default:Schema.Type.Str in
        add_type name (Schema.Type.Scalar (name, kind))
    | Def.ObjectType {
        Ast.ObjectTypeDef.name = (_, name);
        fields;
        interfaces;
        directives = _;
        loc = _;
      } ->
        let interfaces = names_to_strings interfaces in
        add_type name (Schema.Type.Obj (name, conv_fields fields, interfaces))
    | Def.InterfaceType {
        Ast.InterfaceTypeDef.name = (_, name);
        fields;
        directives = _;
        loc = _;
      } ->
        add_type name (Schema.Type.Interface (name, conv_fields fields))
    | Def.UnionType {
        Ast.UnionTypeDef.name = (_, name);
        types;
        directives = _;
        loc = _;
      } ->
        let types = names_to_strings types in
        add_type name (Schema.Type.Union (name, types))
    | Def.EnumType {
        Ast.EnumTypeDef.name = (_, name);
        values;
        directives = _;
        loc = _;
      } ->
        let values = List.map (fun v -> v.Ast.EnumValueDef.name) values in
        let values = names_to_strings values in
        add_type name (Schema.Type.Enum (name, values))
    | Def.InputObjectType {
        Ast.InputObjectTypeDef.name = (_, name);
        fields;
        directives = _;
        loc = _;
      } ->
        add_type name (Schema.Type.InputObj (name, conv_input_vals fields))
    | Def.TypeExtension _ -> () (* TODO *)
    | Def.Directive _ -> () (* TODO *)
    | Def.Schema obj ->
        List.iter (fun t ->
          let typeRef =
            match t.Ast.OperationTypeDef.operation with
            | _, Ast.OperationType.Query -> query
            | _, Ast.OperationType.Mutation -> mutation
            | _, Ast.OperationType.Subscription -> subscription
          in
          let (_, type_) = t.Ast.OperationTypeDef.type_ in
          typeRef := Some type_
        ) obj.Ast.SchemaDef.operationTypes
    | Def.Operation _
    | Def.Fragment _ ->
        failwith "Schema definition can not contain operation/fragment definitions"
  in

  let builtins_ast = Graphql_builtins.get_ast () in
  List.iter consume_def builtins_ast.Ast.Document.definitions;

  List.iter consume_def doc.Ast.Document.definitions;

  let query_name = match !query with Some x -> x | None -> "Query" in
  Schema.Type.(
    match SMap.get query_name !type_map with
    | Some (Obj (name, fields, interfaces)) ->
      let add_field name type_ args fields =
        SMap.add name {Schema.Field.name; args; type_} fields
      in
      let fields =
        add_field "__schema" (NonNull (Named "__Schema")) SMap.empty fields
      in
      let type_args = SMap.add "name" { Schema.InputVal.
        name = "name";
        type_ = NonNull (Named "String");
      } SMap.empty in
      let fields = add_field "__type" (Named "__Type") type_args fields in
      type_map := SMap.add name (Obj (name, fields, interfaces)) !type_map
    | _ -> ()
  );

  Schema.({
    query_name;
    mutation_name = !mutation;
    subscription_name = !subscription;
    type_map = !type_map;
  })
