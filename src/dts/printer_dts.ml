open Format
open Dts_ast

module SSet = Set.Make(String)

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
  | [] -> SSet.empty
  | x :: xs ->
    extract_module (get_modules_in_scope xs) x

and extract_module acc = Statement.(function
  | _, ModuleDeclaration { Module.id; _; } ->
    SSet.add (get_name id) acc
  | _ -> acc
)

and get_name = function
  | _ , { IdPath.ids; _} -> append_name ids

and append_name = function
  | [x] -> id_name x
  | _ -> failwith
    "FLow only supports module declaration with one identifier"

and id_name = function
  | _, { Identifier.name; _ } -> name



(* get_modules_used computes a list of possible use of modules in the
   given list of statements. What it does is find all instances of
   object notation and enlist them. For example consider the
   following code:

   declare module M {
       export class A extends B implements C { }
   }

   declare module O {
       export class D {}
   }

   declare module N {
       var x : M.A
       var y : W.A
       var z : O.D
   }

   For the above .d.ts file, get_modules_used will return
   "M" :: "W" :: "O" :: [] when ran on the body of "module N"
   However, we can see that only "W" might not be a reference to a
   module, possibly "W" is an object defined in another file.
*)

let rec get_modules_used = function
  | [] -> SSet.empty
  | x :: xs ->
    module_used_statement (get_modules_used xs) x

(* modules_used_statement walks the AST of a given statement to see
   if it contains any node in object notation.

   TODO: Currently it only considers the case where object notation is
   present in the type annotation of a variable declaration. Need to
   cover the rest of the cases like:

   export class P extends M.Q { }

   etc.
*)
and module_used_statement acc = Statement.(function
  | _, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, {Declarator.id; _})] ->
        module_used_pattern acc id
      | _ -> failwith "Only single declarator handled currently"
    )
  | _ -> acc
)

and module_used_pattern acc = Pattern.(function
  | _, Identifier id -> module_used_id acc id
  | _ -> failwith "Only identifier allowed in variable declaration"
)

and module_used_id acc = function
  | _, { Identifier. typeAnnotation; _ } ->
      match typeAnnotation with
      | None -> acc
      | Some x -> module_used_type acc x

and module_used_type acc = Type.(function
  | _, Generic t -> module_used_generic acc t
  | _ -> acc
)

and module_used_generic acc = Type.(function
  | { Generic.id; _ } -> match id with
    | _, {IdPath.ids; _} -> module_used_ids acc ids
)

and module_used_ids acc = function
  | x :: xs ->  ( match x with
    | _, {Identifier. name; _ } -> SSet.add name acc
  )
  | _ -> acc


let rec program fmt (_, stmts, _) =
  Format.set_margin 80;
  let scope = get_modules_in_scope stmts in
  fprintf fmt "@[<v>@,%a@]@."
    (list ~sep:"" (statement scope)) stmts

and statement scope fmt = Statement.(function
  | _, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, { Declarator.id; _ })] ->
          fprintf fmt "@[<hv>declare var %a;@]"
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
    (* First we compute all the possible instances of module
    references and then take an intersection with the list of modules
    in scope which are defined in the same file to get a subset of
    actual references to sibling modules.  *)

    let list_possible_modules = get_modules_used body in
    let list_modules_used = SSet.inter scope list_possible_modules in

    fprintf fmt "@[<v>declare module %a {@;<0 2>@[<v>%a%a@]@,}@]"
      id_path id
      (list_ ~sep:"" import_module) (SSet.elements list_modules_used)
      (_list ~sep:"" (statement scope)) body

  | _, ExportModuleDeclaration { ExportModule.name; body; } ->
      fprintf fmt "@[<v>declare module %s {@;<0 2>@[<v>%a@]@,}@]"
        name
        (list ~sep:"" (statement scope)) body

  | _, ExportAssignment id ->
      fprintf fmt "@[<h>declare var exports: typeof %a;@]"
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

(* This prints the import module statement. Since currently, the flow
   parser does not parse import statements inside a module
   declaration, we use this hack of $Exports
*)
and import_module fmt = function
  | x -> fprintf fmt "declare var %s: $Exports<'%s'>;" x x
