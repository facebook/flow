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


let fail_on_todo = true


let todo fmt =
    assert (not fail_on_todo);
    fprintf fmt "@[%s@]"
      "#######"


(* get_modules_in_scope computes the list of modules which are
   declared int he cuurent scope. In particular we shall use it to
   find a list of modules in global scope.

   declare module M {
       export class A extends B implements C { }
   }

   declare module N {
       var x : M.A
   }

   For the above d.ts file, the function will return the following
   list -> "M" :: "N" :: []
*)

let rec get_modules_in_scope = function
  | [] -> []
  | x :: xs ->
    extract_module (get_modules_in_scope xs) x

and extract_module tail = Statement.(function
  | _, ModuleDeclaration { Module.id; _; } ->
    get_names tail id
  | _ -> tail
)

and get_names tail = function
  | _ , { IdPath.ids; _} ->
    append_names tail ids

and append_names tail = function
  | [] -> tail
  | x :: xs -> (id_name x) :: tail

and id_name = function
  | _, { Identifier.name; _ } -> name

let rec program fmt (_, stmts, _) =
  Format.set_margin 80;
  let scope = get_modules_in_scope stmts in
  let () = printf "%d\n" (List.length scope) in
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
      | _ -> todo fmt
    )

  | _, InterfaceDeclaration { Interface.
      id; body; extends; typeParameters;
    } ->
      fprintf fmt "@[<v>declare class %a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param))) typeParameters
        extends_interface extends
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

  | _, AmbientClassDeclaration { AmbientClass.
      id; body; typeParameters; extends; implements;
    } ->
      fprintf fmt "@[<v>declare class %a%a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param))) typeParameters
        extends_class extends
        implements_class implements
        object_type (snd body)

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
  | _ -> todo fmt
)

and type_param fmt = Type.(function
  | { Param.id; _ } ->
      id_ fmt id
)

(* A class can only extend one class unlike interfaces which can
   extend multiple interfaces. Thus, the same function does not work
   for classes.

   declare module M {
       export class A extends B { }
       export class D { }
   }

   In the above .d.ts program we see that class A have "extends"
   property where as class D does not. Thus "extends" is
   an optional property. This is handled in the extends_class_ function.

*)
and extends_class fmt = function
  | None -> ()
  | Some (_, t) ->
      fprintf fmt "@[ extends %a@]"
        generic_type t

(* This helper function is for handling extend statement in an
   interface declration. Since an interface can extend more than one
   interfaces, we have a list of interfaces which are extended by the
   current interface.
*)
and extends_interface fmt = function
  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ extends %a@]"
        generic_type t
  | _ -> todo fmt

(* Implements_class is a helper function to print "implements"
   property in a class. A class can have more than one or zero
   interface so we have a list of interfaces which are implemented by
   he current class.

   Eg. :

   declare module M {
       export class A extends B implements C { }
   }

*)
and implements_class fmt = function
  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ implements %a@]"
        generic_type t
  | _ -> todo fmt

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
      | _ -> todo fmt
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
