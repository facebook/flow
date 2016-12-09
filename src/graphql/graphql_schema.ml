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

let typename_field = { Field.
  name = "__typename";
  args = SMap.empty;
  type_ = Type.NonNull (Type.Named "String");
}

let rec name_of_type f = match f with
  | Type.Named name -> name
  | Type.List f
  | Type.NonNull f -> name_of_type f

let query_type_name s = s.mutation_name

let type_def s type_name =
  SMap.find type_name s.type_map

let type_exists s type_name =
  SMap.mem type_name s.type_map

let rec type_name s _type =
  match _type with
  | Type.Named name -> name
  | Type.List t
  | Type.NonNull t -> type_name s t

let rec print_type s type_ =
  match type_ with
  | Type.Named name -> name
  | Type.List t -> "[" ^ (print_type s t) ^ "]"
  | Type.NonNull t -> (print_type s t) ^ "!"

let get_field s type_name field_name =
  match type_def s type_name with
  | Type.Obj _ | Type.Interface _ | Type.Union _
    when field_name = "__typename"
    -> Some typename_field
  | Type.Obj (_, fmap, _) -> SMap.get field_name fmap
  | Type.Interface (_, fmap) -> SMap.get field_name fmap
  | _ -> failwith "Cannot get field type name of non object"

let get_field_type s type_name field_name =
  Option.map (get_field s type_name field_name) (fun f -> f.Field.type_)

let find_field_type s type_name field_name =
  match get_field s type_name field_name with
  | Some t -> t.Field.type_
  | None -> failwith "Field not found"

let obj_field_types s type_name =
  match type_def s type_name with
  | Type.Obj (_, fields, _) ->
    SMap.fold (fun name field acc -> (name, field.Field.type_) :: acc) fields []
  | _ -> []

let get_possible_types s type_name = Type.(
  match type_def s type_name with
  | Union (_, types) -> types
  | Interface _ ->
    SMap.fold (fun _ t acc ->
      match t with
      | Obj (n, _, interfaces) when List.mem type_name interfaces -> n :: acc
      | _ -> acc
    ) s.type_map []
  | _ -> [type_name]
)

let rec do_types_overlap s a b = Type.(
  if a = b then true
  else match type_def s b with
    | Interface _ | Union _ -> List.exists (do_types_overlap s a) (get_possible_types s b)
    | _ -> List.mem b (get_possible_types s a)
)

let rec is_subtype_name s sub sup =
  if sub = sup then true
  else Type.(match type_def s sub with
    | Obj (_, _, interfaces) -> List.exists (fun x -> sup = x) interfaces
    | Union (_, types) -> List.exists (is_subtype_name s sup) types
    | _ -> false
  )
