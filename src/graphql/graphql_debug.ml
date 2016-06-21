open Graphql_ast

let rec print_document doc =
  let open Document in
  let open OperationDefinition in
  let print_definitions defs =
    match defs with
    | [] -> ()
    | def :: _ ->
      match def with
        | OperationDefinition x -> print_selection_set (Some x.selectionSet) ""
        | FragmentDefinition x ->
          print_selection_set (Some x.FragmentDefinition.selectionSet) ""
  in
  print_definitions doc.definitions

and print_selection_set selectionSet indent =
  let open SelectionSet in
  let rec print_selections selections =
    match selections with
    | [] -> ()
    | Field field :: rest ->
      print_field field indent;
      print_selections rest
    | Expression _ :: rest ->
      print_endline (indent ^ "expr");
      print_selections rest
  in
  match selectionSet with
  | Some selectionSet -> print_selections selectionSet.selections
  | None -> ()

and print_field field indent =
  let open Field in
  print_endline (indent ^ field.name);
  print_selection_set field.selectionSet (indent ^ "  ")

let dump_schema s =
  let open Graphql_schema2 in

  let rec dump_type _type =
    match _type with
    | Type.Named name -> name
    | Type.List t -> "[" ^ (dump_type t) ^ "]"
    | Type.NonNull t -> (dump_type t) ^ "!"
  in

  let dump_fields field_map =
    SMap.values field_map |> List.map (fun field ->
      "  " ^ field.Field.name ^ ": " ^ (dump_type field.Field._type)
    ) |> String.concat "\n"
  in

  let parts = SMap.values s.Schema.types |> List.map (fun type_def ->
    match type_def with
    | Type.Scalar name -> "scalar " ^ name ^ "\n"
    | Type.Obj (name, field_map, _) ->
      "type " ^ name ^ " {\n" ^ (dump_fields field_map) ^ "\n}\n"
    | Type.Interface (name, field_map) ->
      "interface " ^ name ^ " {\n" ^ (dump_fields field_map) ^ "\n}\n"
    | Type.Union (name, values) ->
      "union " ^ name ^ " = " ^ (String.concat " | " values) ^ "\n"
    | Type.Enum (name, names) ->
      let names = List.map (fun x -> "  " ^ x) names in
      "enum " ^ name ^ " {\n" ^ (String.concat "\n" names) ^ "\n}\n"
    | Type.InputObj (name, _) -> "input_obj " ^ name ^ "\n"
  ) in

  String.concat "\n" parts
