open Utils_js

type inst = {
  regex: Str.regexp option;
  tag: string list; (* Relay.QL -> ["QL", "Relay"] *)
  schema: Graphql_schema.t;
  schema_path: string;
}

type config = {
  instances: inst list;
}

type t = config

let obj_get json key =
  let rec seek = function
    | (k, v) :: rest -> if k = key then Some v else seek rest
    | [] -> None
  in
  seek (Hh_json.get_object_exn json)

let obj_find json key =
  match obj_get json key with
  | Some v -> v
  | None -> failwith (spf "Required key `%s` not not found" key)

let add_builtin_scalar_kinds kinds = Graphql_schema.Type.(
  kinds
  |> SMap.add "ID" Str
  |> SMap.add "String" Str
  |> SMap.add "Int" Int
  |> SMap.add "Float" Float
  |> SMap.add "Boolean" Bool
)

let load_instance root inst_config =
  let regex =
    obj_get inst_config "regex"
    |> Option.map ~f:(fun r -> r |> Hh_json.get_string_exn |> Str.regexp)
  in
  let tag =
    obj_find inst_config "tag"
    |> Hh_json.get_string_exn
    |> Str.split (Str.regexp "\\.")
    |> List.rev
  in
  let schema_path =
    obj_find inst_config "schema"
    |> Hh_json.get_string_exn
    |> Files.normalize_path root
  in
  let schema_src = Sys_utils.cat schema_path in
  let scalar_kinds = add_builtin_scalar_kinds SMap.empty in
  let schema =
    Graphql_parse.parse_file schema_src schema_path
    |> Graphql_conv.schema_from_ast scalar_kinds
  in
  let errors = Graphql_validation.validate_schema schema in
  if errors <> [] then begin
    List.iter (Flow_logger.log "[SCHEMA ERROR] %s") errors;
    failwith "Error loading schema"
  end;
  { regex; tag; schema; schema_path }

let parse_config root path =
  let config = Hh_json.json_of_file (Files.normalize_path root path) in
  let items = Hh_json.get_array_exn config in
  let instances = List.map (load_instance root) items in
  { instances }

let global = ref None

let get () = !global

let restore config = global := config

let init root path =
  let config = parse_config root path in
  global := Some config

let get_schema_paths () =
  match !global with
  | Some global -> List.map (fun inst -> inst.schema_path) global.instances
  | None -> []

let get_inst path =
  let rec find instances = match instances with
    | inst :: rest ->
      begin match inst.regex with
      | None -> Some inst
      | Some regex ->
        if Str.string_match regex path 0 then Some inst else find rest
      end
    | [] -> None
  in
  Option.value_map !global ~default:None ~f:(fun c -> find c.instances)

let tag inst = inst.tag
