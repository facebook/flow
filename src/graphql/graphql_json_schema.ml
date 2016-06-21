open Graphql_schema2
open Hh_json

let member key json =
  let pairs = get_object_exn json in
  let _, value = List.find (fun (k, _) -> key = k) pairs in
  value

let rec type_of_json type_json =
  match type_json |> member "kind" |> get_string_exn with
  | "SCALAR"
  | "OBJECT"
  | "INTERFACE"
  | "UNION"
  | "ENUM"
  | "INPUT_OBJECT" -> Type.Named (type_json |> member "name" |> get_string_exn)

  | "LIST" -> Type.List (type_of_json (type_json |> member "ofType"))

  | "NON_NULL" -> Type.NonNull (type_of_json (type_json |> member "ofType"))

  | x -> failwith ("Unknown type kind `" ^ x ^ "`")

let field_of_json field_json =
  Field.{
    name = field_json |> member "name" |> get_string_exn;
    args = [];
    _type = type_of_json (field_json |> member "type");
  }

let field_map_of_json fields_json =
  let fields = fields_json
    |> get_array_exn
    |> List.map field_of_json
  in
  List.fold_left (fun map field ->
    SMap.add field.Field.name field map
  ) SMap.empty fields

let load file =
  let json = json_of_file file in

  let query_type = Type.Named (json
    |> member "data"
    |> member "__schema"
    |> member "queryType"
    |> member "name"
    |> get_string_exn
  ) in

  let types = json
    |> member "data"
    |> member "__schema"
    |> member "types"
    |> get_array_exn
  in
  let types = List.fold_left (fun type_map type_json ->
    let name = type_json |> member "name" |> get_string_exn in
    let def = match type_json |> member "kind" |> get_string_exn with
      | "SCALAR" -> Type.Scalar name
      | "OBJECT" ->
        let interfaces = type_json
          |> member "interfaces"
          |> get_array_exn
          |> List.map (fun x -> x |> member "name" |> get_string_exn)
        in
        let field_map = field_map_of_json (type_json |> member "fields") in
        Type.Obj (name, field_map, interfaces)
      | "INTERFACE" ->
        let field_map = field_map_of_json (type_json |> member "fields") in
        Type.Interface (name, field_map)
      | "UNION" ->
        let type_names = type_json
          |> member "possibleTypes"
          |> get_array_exn
          |> List.map (fun x -> x |> member "name" |> get_string_exn)
        in
        Type.Union (name, type_names)
      | "ENUM" ->
        let values = type_json
          |> member "enumValues"
          |> get_array_exn
          |> List.map (fun x -> x |> member "name" |> get_string_exn)
        in
        Type.Enum (name, values)
      | "InputObject" as x
      | x -> failwith ("Unexpected type kind `" ^ x ^ "` in schema definition")
    in
    SMap.add name def type_map
  ) SMap.empty types in
  Schema.{query_type; types}

let global = load "/Users/pvolok/local/flow/tests/relay/schema.json";;
