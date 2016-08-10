module rec Schema: sig
  type t = {
    query_type: Type.t;
    types: Type.def SMap.t;
  }
end = Schema

and Type: sig
  type def =
    | Scalar of string
    | Obj of string * Field.t SMap.t * string list
    | Interface of string * Field.t SMap.t
    | Union of string * string list
    | Enum of string * string list
    | InputObj of string * Field.t SMap.t

  and t =
    | Named of string
    | List of t
    | NonNull of t
end = Type

and Field: sig
  type t = {
    name: string;
    args: InputVal.t list;
    _type: Type.t;
  }
end = Field

and InputVal: sig
  type t = {
    name: string;
    _type: Type.t;
  }
end = InputVal

type t = Schema.t

let rec name_of_type f = match f with
  | Type.Named name -> name
  | Type.List f
  | Type.NonNull f -> name_of_type f

let query_type_name s =
  name_of_type s.Schema.query_type

let type_def s type_name =
  let type_name = match type_name with
    | "$query" -> query_type_name s
    | x -> x
  in
  SMap.find_unsafe type_name s.Schema.types

let rec type_name s _type =
  match _type with
  | Type.Named name -> name
  | Type.List t
  | Type.NonNull t -> type_name s t
