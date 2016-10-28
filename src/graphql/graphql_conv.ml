module Ast = Graphql_ast
module Schema = Graphql_schema
module SMap = Schema.SMap

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

let schema_from_ast doc =
  let query = ref None in
  let mutation = ref None in
  let subscription = ref None in
  let type_map = ref SMap.empty in

  let add_type name type_ = type_map := SMap.add name type_ !type_map in

  List.iter (fun stmt ->
    let module Def = Ast.Definition in
    match stmt with
    | Def.ScalarType scalar ->
        let (_, name) = scalar.Ast.ScalarTypeDef.name in
        add_type name (Schema.Type.Scalar name)
    | Def.ObjectType {
        Ast.ObjectTypeDef.name = (_, name);
        fields;
        interfaces;
        directives = _;
      } ->
        let interfaces = names_to_strings interfaces in
        add_type name (Schema.Type.Obj (name, conv_fields fields, interfaces))
    | Def.InterfaceType {
        Ast.InterfaceTypeDef.name = (_, name);
        fields;
        directives = _;
      } ->
        add_type name (Schema.Type.Interface (name, conv_fields fields))
    | Def.UnionType {
        Ast.UnionTypeDef.name = (_, name);
        types;
        directives = _;
      } ->
        let types = names_to_strings types in
        add_type name (Schema.Type.Union (name, types))
    | Def.EnumType {
        Ast.EnumTypeDef.name = (_, name);
        values;
        directives = _;
      } ->
        let values = List.map (fun v -> v.Ast.EnumValueDef.name) values in
        let values = names_to_strings values in
        add_type name (Schema.Type.Enum (name, values))
    | Def.InputObjectType {
        Ast.InputObjectTypeDef.name = (_, name);
        fields;
        directives = _;
      } ->
        add_type name (Schema.Type.InputObj (name, conv_input_vals fields))
    | Def.Schema obj ->
        List.iter (fun t ->
          let typeRef =
            match t.Ast.OperationType.operation with
            | Ast.OperationType.Query -> query
            | Ast.OperationType.Mutation -> mutation
            | Ast.OperationType.Subscription -> subscription
          in
          let (_, type_) = t.Ast.OperationType.type_ in
          typeRef := Some type_
        ) obj.Ast.SchemaDef.operationTypes
    | Def.Operation _
    | Def.Fragment _ ->
        failwith "Schema definition can not contain operation/fragment definitions"
  ) doc.Ast.Document.definitions;

  Schema.{
    query_name = (match !query with Some x -> x | None -> "Query");
    mutation_name = !mutation;
    subscription_name = !subscription;
    type_map = !type_map;
  }
