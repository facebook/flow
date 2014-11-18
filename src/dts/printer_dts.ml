open Format
open Dts_ast

let rec list ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "%a"
        f x
  | x::xs ->
      fprintf fmt "%a%s@,%a"
        f x
        sep
        (list ~sep f) xs
  | [] -> ()

let rec list_ ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "%a%s"
        f x
        sep
  | x::xs ->
      fprintf fmt "%a%s@,%a"
        f x
        sep
        (list_ ~sep f) xs
  | [] -> ()

let rec _list ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "@,%a%s"
        f x
        sep
  | x::xs ->
      fprintf fmt "@,%a%s%a"
        f x
        sep
        (_list ~sep f) xs
  | [] -> ()

let non_empty wrapper f fmt = function
  | [] -> ()
  | list -> fprintf fmt wrapper f list

let opt f fmt = function
  | None -> ()
  | Some x -> f fmt x

(* eventually make this assert false *)
let todo fmt =
  fprintf fmt "@[%s@]"
    "#######"

let rec program fmt (_, stmts, _) =
  Format.set_margin 80;
  fprintf fmt "@[<v>@,%a@]@."
    (list ~sep:";" statement) stmts

and statement fmt = Statement.(function
  | _, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, { Declarator.id; _ })] ->
          fprintf fmt "@[<hv>declare var %a@]"
            pattern id
      | _ -> assert false
    )

  | _, InterfaceDeclaration { Interface.
      id; body; extends; typeParameters;
    } ->
      fprintf fmt "@[<v>declare class %a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param))) typeParameters
        extends_ extends
        object_type (snd body)

  | _, ModuleDeclaration { Module.id; body; } ->
      fprintf fmt "@[<v>declare module %a {@;<0 2>@[<v>%a@]@,}@]"
        id_path id
        (list ~sep:";" statement) body

  | _, ExportModuleDeclaration { ExportModule.name; body; } ->
      fprintf fmt "@[<v>declare module %s {@;<0 2>@[<v>%a@]@,}@]"
        name
        (list ~sep:";" statement) body

  | _, ExportAssignment id ->
      fprintf fmt "@[<h>declare var exports: typeof %a@]"
        id_ id

  | _ ->
      todo fmt
)

and pattern fmt = Pattern.(function
  | _, Identifier id ->
      id_ fmt id
  | _ ->
      todo fmt
)

and id_ fmt = function
  | _, { Identifier.name; typeAnnotation; _ } ->
      fprintf fmt "%s%a"
        name
        (opt annot) typeAnnotation

and annot fmt =
  fprintf fmt ": %a" type_

and generic_type fmt = Type.(function
  | { Generic.id; typeArguments; } ->
      fprintf fmt "@[%a%a@]"
        id_path id
        (non_empty "<%a>" (list ~sep:"," type_)) typeArguments
)

(********************* TODO ************************)
and id_path fmt = function
  | _, { IdPath.ids; _ } ->
      fprintf fmt "@[<h>%a@]"
        (list ~sep:"." id_) ids

and type_ fmt = Type.(function
  | _, Any -> fprintf fmt "any"
  | _, Void -> fprintf fmt "void"
  | _, Number -> fprintf fmt "number"
  | _, String -> fprintf fmt "string"
  | _, Boolean -> fprintf fmt "boolean"
  | _, Function t -> function_type fmt t
  | _, Object t -> object_type fmt t
  | _, Array t -> array_type fmt t
  | _, Generic t -> generic_type fmt t
  | _ -> assert false
)

and type_param fmt = Type.(function
  | { Param.id; _ } ->
      id_ fmt id
)

and extends_ fmt = function
  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ extends %a@]"
        generic_type t
  | _ -> assert false

and object_type fmt = Type.(function
  | { Object.properties; indexers; } ->
      fprintf fmt "{@;<0 2>@[<v>%a%a@]@,}"
        (list_ ~sep:";" property) properties
        (list ~sep:";" indexer_) indexers
)

and property fmt = Type.Object.(function
  | _, { Property.key; value; _ } ->
    (match key, value with
      | (Expression.Object.Property.Identifier id, (_,Type.Function value)) ->
          fprintf fmt "@[<hv>%a%a@]"
            id_ id
            method_type value
      | (Expression.Object.Property.Identifier id, _) ->
          fprintf fmt "@[<hv>%a: %a@]"
            id_ id
            type_ value
      | _ -> assert false
    )
)

and indexer_ fmt = Type.Object.(function
  | _, { Indexer.id; key; value; } ->
      fprintf fmt "@[<hv>[%a: %a]: %a@]"
        id_ id
        type_ key
        type_ value
)

and array_type fmt t =
  fprintf fmt "Array<%a>"
    type_ t

and function_type fmt = Type.(function
  | { Function.typeParameters; params; rest; returnType; } ->
      fprintf fmt "%a(@;<0 2>@[<hv>%a%a@]@,) => %a"
        (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param)) typeParameters
        (list ~sep:", " param) params
        (opt (rest_ ~follows:(params <> []))) rest
        type_ returnType
)

and method_type fmt = Type.(function
  | { Function.typeParameters; params; rest; returnType; } ->
      fprintf fmt "%a(@;<0 2>@[<hv>%a%a@]@,): %a"
        (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param)) typeParameters
        (list ~sep:", " param) params
        (opt (rest_ ~follows:(params <> []))) rest
        type_ returnType
)

and param fmt = Type.Function.(function
  | _, { Param.name; typeAnnotation; optional } ->
    if optional
    then fprintf fmt "%a?: %a"
      id_ name
      type_ typeAnnotation
    else fprintf fmt "%a: %a"
      id_ name
      type_ typeAnnotation
)

and rest_ ?(follows=false) fmt = Type.Function.(function
  | _, { Param.name; typeAnnotation; _ } ->
    let sep = if follows then ", " else "" in
    fprintf fmt "%s...%a: %a"
      sep
        id_ name
      type_ typeAnnotation
)
