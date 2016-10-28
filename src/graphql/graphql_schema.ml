module SMap = Map.Make(String)

module rec Type: sig
  type def =
    | Scalar of string
    | Obj of string * Field.t SMap.t * string list
    | Interface of string * Field.t SMap.t
    | Union of string * string list
    | Enum of string * string list
    | InputObj of string * InputVal.t SMap.t

  and t =
    | Named of string
    | List of t
    | NonNull of t
end = Type

and Field: sig
  type t = {
    name: string;
    args: InputVal.t SMap.t;
    type_: Type.t;
  }
end = Field

and InputVal: sig
  type t = {
    name: string;
    type_: Type.t;
  }
end = InputVal

type t = {
  query_name: string;
  mutation_name: string option;
  subscription_name: string option;
  type_map: Type.def SMap.t;
}

let rec name_of_type f = match f with
  | Type.Named name -> name
  | Type.List f
  | Type.NonNull f -> name_of_type f

let query_type_name s = s.mutation_name

let type_def s type_name =
  SMap.find type_name s.type_map

let rec type_name s _type =
  match _type with
  | Type.Named name -> name
  | Type.List t
  | Type.NonNull t -> type_name s t
