open Format
open Dts_ast
open Utils

module SSet = Utils.SSet

(* get_line_number returns a line number corresponding to a
   location *)

let get_line_number = Loc. (function
  | { start; _ } -> start.line
)

let strip_quotes = Str.global_replace (Str.regexp "'\\|\"") ""

let failwith_location loc str = failwith (
  String.concat "" [sprintf "Line %d: " (get_line_number loc); str] )

(* contents of an option type *)
let contents = function
  | Some x -> x
  | None -> failwith "Trying  access contents of None option"

(* returns identifier of an interface or variable declaration.
  returns None for anything else. *)
let rec get_identifier = Statement.(function
  | _, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
    match declarations with
    | [(_, { Declarator.id; _})] ->
      get_identifier_pattern id
    | _ -> None
  )
  | _, InterfaceDeclaration { Interface.
      id; _
    } ->
        get_identifier_id id
  | _ -> None
)

and get_identifier_pattern = Pattern.(function
  | _, Identifier id ->
    get_identifier_id id
  | _ -> None
)

and get_identifier_id = function
  | _, { Identifier.name; _ } ->
      Some name

(* returns argument of a require expression *)
and get_identifier_require = Expression.(function
  | _, Call {Call. arguments; _ } -> get_identifier_arguments arguments
  | loc, _ -> None
)

(* return argument from a singleton list of arguments *)
and get_identifier_arguments = function
  | h :: [] -> get_identifier_expression_or_spread h
  | _ -> None

and get_identifier_expression_or_spread = function
  | Expression.Expression e -> get_identifier_expression e
  | _ -> None

and get_identifier_expression = Expression.(function
  | _, Let l -> get_identifier_let l
  | _, Literal l -> get_identifier_literal l
  | _ -> None
)

and get_identifier_let = Expression.(function
  | {Let. body; _} -> get_identifier_expression body
)

and get_identifier_literal = function
  | {Literal. raw; _ } -> Some raw

let same_name x y =
  let x = get_identifier x in
  let y = get_identifier y in
  if x = None || y = None then false
  else x = y

(* returns body of an interface or type of a variable in object form.
   returns None for anything else. *)
let rec get_object = Statement.(function
  | _, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
    match declarations with
    | [(_, { Declarator.id; _})] ->
      get_object_pattern id
    | _ -> None
  )
  | _, InterfaceDeclaration { Interface.
      body; _
    } ->
        Some (snd body)
  | _ -> None
)

and get_object_pattern = Pattern.(function
  | _, Identifier id ->
    get_object_id id
  | _ -> None
)

and get_object_id = function
  | _, { Identifier.typeAnnotation; _ } ->
    get_object_annotation typeAnnotation

and get_object_annotation = Type.(function
  | Some (_, Object t) -> Some t
  | _ -> None
)

(* returns true if and only if x and y are VariableDeclaration or
   InterfaceDeclaration and they have same identifier associated
   with them *)
let same_name x y =
  let x = get_identifier x in
  let y = get_identifier y in
  if x = None || y = None then false
  else x = y


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
      fprintf fmt "%a%s@,"
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

(* This is a little modification of the list functions above.
   This does a one step look ahead to find variable and interface
   declarations which can be squashed together as a class. *)
let rec look_ahead ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "%a%s@,"
        f (x, None)
        sep
  | x::y::xs ->
    let samename = (same_name x y) in
      fprintf fmt "%a%s@,%a"
        f (x, if samename then Some y else None)
        sep
        (look_ahead ~sep f) (if samename then xs else y::xs)
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


(* generate_mangled_name generates a new name for an inner level
   module so that it can be moved to global scope without any
   collisions of names with any other module definition.
*)
let generate_mangled_name name prefix =
  String.concat "___" (List.append prefix [name])

(* inverse of generate_mangled_name *)
let clean_mangled_name mangled_name =
  Str.global_substitute (Str.regexp ".*___") (fun _ -> "") mangled_name

(* Function to convert an arbitrary module name to a valid javascript
   identifier. Change this later as need be.
   Right now it just convert "-" to "$HYPHEN$". *)

let module_to_identifier =
  Str.global_replace (Str.regexp "-") "$HYPHEN$" ;;

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

   In case of nested modules, it does more. It prepends a prefix
   which is names of its ancestors separated by "____".



   declare module M {
       declare module N {
           declare module P {

           }
           declare module Q {

           }
       }
   }

   when called for the body of module N, the function will return the
   following list --> "M___N___P" :: "M___N___Q" :: []
*)

let rec get_modules_in_scope acc prefix = function
  | [] -> acc
  | x :: xs ->
    extract_module (get_modules_in_scope acc prefix xs) prefix x

and extract_module acc prefix = Statement.(function
  | _, ModuleDeclaration { Module.id; _; } ->
    (get_name prefix id) :: acc
  | _, ExportModuleDeclaration { ExportModule.name; _; } ->
    (generate_mangled_name name prefix) :: acc
  | _ -> acc
)

and get_name prefix = function
  | _ , { IdPath.ids; _} -> append_name prefix ids

and append_name prefix = function
  | [x] -> id_name prefix x
  | _ -> failwith
    "FLow only supports module declaration with one identifier"

and id_name prefix = function
  | _, { Identifier.name; _ } -> generate_mangled_name name prefix

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

   TODO:  Have not covered all the cases where object notation might
   be present. Eg., union, intersection, array etc.

   etc.

   Note: All the child modules are by default imported by the
   parent. This is done so that they are accessbile in through the
   parent module via dot notation.

   Eg.
   declare module R {
       module T {
           export class E { }
       }
   }

   converts to
   declare module R {
     declare var T: $Exports<'R___T'>;
   }
   declare module R___T {
     declare class E {

     }
   }

   So that we can access class E as R.T.E
*)
and module_used_statement acc = Statement.(function
  (* This handles the following case:

     declare module M {
       export class A { }
     }
     declare module N {
       export var x: M.A
     }
     delcare module P {
       export var z: typeof N.x
     }
  *)
  | loc, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, {Declarator.id; _})] ->
        module_used_pattern acc id
      | _ -> failwith_location loc
        "Only single declarator handled currently"
    )
  (* This case means that the child modules are imported by default.*)
  | _, ModuleDeclaration { Module.id; body; } ->
    SSet.add (get_name [] id) acc
  (* This case check for possible module references in export
       assignments. Note this does not look for object notation. Eg.,

     declare module M { }
     declare module "N" {
       export = M // this is reference to a module
     }
  *)
  | _, ExportAssignment id -> (match id with
    | _, {Identifier. name; _} -> SSet.add name acc
  )
  (* This case checks for object notation in extend property of a
       class. Eg.,
     export class C extends M.D { }
  *)
  | _, AmbientClassDeclaration { AmbientClass. extends; body; _ } ->
    let acc = module_used_body acc body in
    (match extends with
    | Some (_, t) -> module_used_generic acc t
    | _ -> acc
    )
  (* This case checks for object notation in extend property of an
       interface. Eg.,
     export interface C extends M.D { }
  *)
  | _, InterfaceDeclaration { Interface. extends; body; _ } ->
    let acc = module_used_body acc body in
    let fold_intermediate x y = module_used_generic x (snd y) in
    List.fold_left fold_intermediate acc extends
  | _ -> acc
)

and module_used_pattern acc = Pattern.(function
  | _, Identifier id -> module_used_id acc id
  | loc, _ -> failwith_location loc
    "Only identifier allowed in variable declaration"
)

and module_used_id acc = function
  | _, { Identifier. typeAnnotation; _ } ->
      match typeAnnotation with
      | None -> acc
      | Some x -> module_used_type acc x

and module_used_type acc = Type.(function
  | _, Generic t -> module_used_generic acc t
  | _, Typeof x ->
    (match x with _, {IdPath.ids; _}-> module_used_ids acc ids)
  | loc, Object t -> module_used_body acc (loc, t)
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

(* This is for checking module use within an object body *)
and module_used_body acc = Type.(function
| _, {Object. properties; indexers; } ->
  let fold_intermediate x y = module_used_property x (snd y) in
  let acc = List.fold_left fold_intermediate acc properties in
  let fold_intermediate x y = module_used_indexer x (snd y) in
  List.fold_left fold_intermediate acc indexers
)

and module_used_property acc = Type.Object.(function
  | { Property.value; _ } -> module_used_type acc value
)

and module_used_indexer acc = Type.Object.(function
  | { Indexer.value; _ } -> module_used_type acc value
)

(*
  get_modules_to_import returns a list of modules that need to be
  imported. An element of this list is a tuple of two strings A and
  B. A refers to the name by which the module is to be referred to in the
  current scope and B refers to the name to which the module is be
  referred to in the global scope.
*)
let get_modules_to_import scope set =
  (* find_module matches name of a module with all the modules in
     scope by stripping of the prefix *)
  let rec find_module name = function
    | [] -> None
    | x :: xs ->
      if (clean_mangled_name x) = name
      then Some (name, x)
      else find_module name xs
  and fold_intermediate scope name acc =
    match (find_module name scope) with
    | None -> acc
    | Some x -> x :: acc
  in
  SSet.fold (fold_intermediate scope) set []



(*
  The following two functions filter out module and not_module
  statements from a list of statements respectively.
*)
let rec filter_modules prefix imports scope = Statement.(function
  | [] -> []
  |  x :: xs -> match x with
    | _, ModuleDeclaration _ ->
      (prefix, imports, scope, x) :: (filter_modules prefix imports scope xs)
    | _, ExportModuleDeclaration _ ->
      (prefix, imports, scope, x) :: (filter_modules prefix imports scope xs)
    | _ -> filter_modules prefix imports scope xs
)

let rec filter_not_modules = Statement.(function
  | [] -> []
  |  x :: xs -> match x with
    | _, ModuleDeclaration _ -> filter_not_modules xs
    | _, ExportModuleDeclaration _ -> filter_not_modules xs
    | _ -> x :: (filter_not_modules xs)
)

(*
  Find a list of modules declared within the current module. If you
  only consider the module nodes in the AST then, this is just the
  find_child function of a tree.
*)
let find_child_modules acc prefix imports scope = Statement.(function
  | _, ModuleDeclaration { Module.id; body; } ->
    let new_prefix = List.append prefix [get_name [] id] in
    let new_scope = get_modules_in_scope scope new_prefix body in
    List.append (filter_modules new_prefix imports new_scope body) acc
  | _, ExportModuleDeclaration { ExportModule.name; body; } ->
    let new_scope = get_modules_in_scope scope [name] body in
    List.append (filter_modules [name] imports new_scope body) acc
  | loc, _  -> failwith_location loc
    "Unexpected statement. A module declaration was expected"
)

(*
  Find a list of non module statements declared within the current module. And
  append it in imports.

  Cases handled:
    1. Variable Declaration
    2. Class Declaration
    3. Interference Declaration
    4, Enum Declaration

  TODO:
    1. Function Declaration

  "imports" is a list of pairs (a, b) where 'a' is the non module entity to be
  imported and 'b' is the of the module whose context 'a' is declared.

  Eg.

  module M {
    variable x : number
    class A { }
    module N {

    }
  }

  If run on module M, expand_imports returns ("x", "M") :: ("A", "M") :: []
*)

let rec expand_imports imports = Statement.(function
  | _, ModuleDeclaration{Module. body; id; _ } ->
    let name = get_name [] id in
    expand_imports_statements imports name body
  | _, ExportModuleDeclaration{ExportModule. body; name; _ } ->
    let name = module_to_identifier name in
    expand_imports_statements imports name body
  | loc, _ -> failwith_location loc
    "Unexpected statement. A module declaration was expexted"
)

and expand_imports_statements acc name = function
  | [] -> acc
  | x :: xs ->
    expand_imports_statement (expand_imports_statements acc name xs) name x

and expand_imports_statement acc name = Statement.(function
  | loc, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, {Declarator.id; _})] ->
        expand_imports_pattern acc name id
      | _ -> failwith_location loc
        "Only single declarator handled currently"
    )
  | _, AmbientClassDeclaration { AmbientClass. id; _ } ->
    expand_imports_id acc name id
  | _, InterfaceDeclaration { Interface. id; _ } ->
    expand_imports_id acc name id
  | _, EnumDeclaration { Enum. name = id; members } ->
    expand_imports_id acc name id
  | _ -> acc
)

and expand_imports_pattern acc name = Pattern.(function
  | _, Identifier id -> expand_imports_id acc name id
  | loc, _ -> failwith_location loc
    "Only identifier allowed in variable declaration"
)

and expand_imports_id acc name= function
  | _, { Identifier. name = import; _ } -> (import, name) :: acc



(*
  To handle flatten nested modules, we decopes the AST into modules
  and not_modules component. Note that the modules will form a tree
  and we just need to flatten that tree. We do this in DFS order for
  readability.
*)

let rec program fmt (_, stmts, _) =
  Format.set_margin 80;
  let prefix  = [] in
  let scope = get_modules_in_scope [] prefix stmts in
  let modules = filter_modules prefix [] scope stmts in
  let not_modules = filter_not_modules stmts in
  fprintf fmt "@[<v>%a%a@]@."
    print_modules modules
    (look_ahead ~sep:"" (statement scope prefix)) not_modules

(*
  print_modules calls print_module in a DFS order. It can easily be
  converted to BFS order by changing find_child_moduels.
*)

and print_modules fmt = function
  | [] -> ()
  | (prefix, imports, scope, x) :: xs ->
    fprintf fmt "%a@,%a"
      (print_module scope imports prefix) x
      print_modules
      (find_child_modules xs prefix (expand_imports imports x) scope x)

and print_module scope imports prefix fmt = Statement.(function

  (* 1. First we compute all the possible instances of module
     references and then take an intersection with the list of modules
     in scope which are defined in the same file to get a subset of
     actual references to other modules.

     2. Also, we import all the entities declared by any of the anscestors.
     3. And ofcourse we import all the parent modules. This is done to
        assist step 2.
  *)

  | _, ModuleDeclaration { Module.id; body; } ->
    let name = get_name [] id in
    let new_prefix = List.append prefix [name] in
    let new_scope = get_modules_in_scope scope new_prefix body in
    let list_possible_modules =
      SSet.union (get_modules_used body) (Utils.set_of_list prefix) in
    let list_modules_used =
      get_modules_to_import new_scope list_possible_modules in

    fprintf fmt "@[<v>declare module \"%s\" {@;<0 2>@[<v>%a%a%a@]@,}@]"
      (generate_mangled_name name prefix)
      (list_ ~sep:"" import_module) list_modules_used
      (list_ ~sep:"" import_members) imports
      (look_ahead ~sep:"" (statement new_scope new_prefix))
      (filter_not_modules body)

  | loc, ExportModuleDeclaration { ExportModule.name; body; } ->
    (* ExportModuleDeclaration is allowed only in global scope, that
    is the prefix when an ExportModuleDeclaration is encoutered is ""  *)
    if prefix <> [] then failwith_location loc
      "Export Module Declaration allowed only in the global scope"
    else
      let new_scope = get_modules_in_scope scope [name] body in
      let list_possible_modules = get_modules_used body in
      let list_modules_used =
        get_modules_to_import new_scope list_possible_modules in
      fprintf fmt "@[<v>declare module \"%s\" {@;<0 2>@[<v>%a%a@]@,}@]"
        name
        (list_ ~sep:"" import_module) list_modules_used
        (look_ahead ~sep:"" (statement scope prefix))
        (filter_not_modules body)

  | loc, _ -> failwith_location loc "Module declaration expected here"
)
and statement scope prefix fmt =
  Statement.(function
  (* This case sqashes a variable with the following interface
     with same name *)
  | ((_, VariableDeclaration _) as y), Some x -> (match x with
      | _, InterfaceDeclaration { Interface.
        id; extends; typeParameters; _
        } ->
        fprintf fmt "@[<v>declare class %a%a%a %a@]"
          id_ id
          (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param)))
            typeParameters
          extends_interface extends
          print_combined_class ((get_object x), (get_object y))
      | loc, _ -> failwith_location loc
        "Unsopprted elements with same name."
      )

  | (_, VariableDeclaration { VariableDeclaration.
      declarations; _
    }), _ -> VariableDeclaration.(
      match declarations with
      | [(_, { Declarator.id; _ })] ->
          fprintf fmt "@[<hv>declare var %a;@]"
            pattern id
      | _ -> todo fmt
    )
  (* This case squashes an interface with the following VariableDeclaration
     with same name *)
  | ((_, InterfaceDeclaration { Interface.
      id; extends; typeParameters; _
    }) as x), Some y ->
      fprintf fmt "@[<v>declare class %a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param)))
          typeParameters
        extends_interface extends
        print_combined_class ((get_object x), (get_object y))

  | (_, InterfaceDeclaration { Interface.
      id; body; extends; typeParameters;
    }), _ ->
      fprintf fmt "@[<v>declare class %a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param))) typeParameters
        extends_interface extends
        object_type (snd body)

  | (_, ExportAssignment id), _ ->
      fprintf fmt "@[<h>declare var exports: typeof %a;@]"
        id_ id

  | (_, AmbientClassDeclaration { AmbientClass.
      id; body; typeParameters; extends; implements;
    }), _ ->
      fprintf fmt "@[<v>declare class %a%a%a%a %a@]"
        id_ id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," type_param))) typeParameters
        extends_class extends
        implements_class implements
        object_type (snd body)

  (* The following case converts an enum declaration to a class with
      static members of type "number"

     Eg.

     export enum Color { R, G, B }

     converts to

     declare class Color {
       static R : number;
       static G : number;
       static B : number;
     }
  *)
  | (_, EnumDeclaration { Enum. name; members }), _ ->
    (* TODO: Write a doc explaining difference between enums and our
       approximation for the same *)
    let () = prerr_endline "Warning: The declaration file contains \
       enums.\nEnums are converted to class with static members. More\
       details here : <url for doc>\n" in
    fprintf fmt "@[<v>declare class %a {@;<0 2>@[<v>%a@]@,}@]"
      id_ name
      (list_ ~sep:";" enum_member) members

  (* The following case handles function declaration *)
  | (_, AmbientFunctionDeclaration {AmbientFunctionDeclaration.
      id; params; returnType; _ }), _ ->
    fprintf fmt "@[<hv>declare function %a(%a)%a;@]"
      id_ id
      (list ~sep:", " pattern) params
      annot returnType

  (* The following handles Typescript's commonjs import statements like:

     import M = require("M");

     TODO: handle other import notations as well.
  *)
  | (loc, ImportDeclaration {Import. id; entity; _} ), _ ->
      import_module fmt (
        contents (get_identifier_id id),
        strip_quotes (contents (get_identifier_require entity))
      )
  (* The following handles type aliases.
     Eg. : type T = number | string;
     TODO: walk through typealiases while computing module_used etc.
  *)
  | (loc, TypeAlias {TypeAlias.left; right } ), _ ->
    fprintf fmt "@[<hv>type %a = %a;@]"
      generic_type (snd left)
      type_ right

  | (loc, _), _ -> failwith_location loc "This is not supported yet"
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

(* convert a number identifier to a string *)
and id_no_num fmt = function
  | _, { Identifier.name; typeAnnotation; _ } ->
    let name = try ignore (int_of_string name); spf "%S" name
      with _ -> name in
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
  | _, Typeof x -> fprintf fmt "typeof %a"
    id_path x
  | _, Intersection l -> (list ~sep:" & " type_) fmt l
  | _, Union l -> (list ~sep:" | " type_) fmt l
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

(* implements_class currently does nothing, uncomment the commented
   lines when "implements" is supported on the flow side *)
and implements_class fmt = function
(*  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ implements %a@]"
        generic_type t
  | _ -> todo fmt *)
  | _ -> ()

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
            id_no_num id
            method_type value
      | (Expression.Object.Property.Identifier id, _) ->
          fprintf fmt "@[<hv>%a: %a@]"
            id_no_num id
            type_ value
      | _ -> todo fmt
    )
)

(* Appends static to all methods and data members apart from constructor *)
and property_static fmt = Type.Object.(function
  | _, { Property.key; value; _ } ->
    (match key, value with
      | (Expression.Object.Property.Identifier id, (_,Type.Function value)) ->
          (if get_identifier_id id = Some "new"
          then fprintf fmt "@[<hv>%a%a@]"
          else fprintf fmt "@[<hv>static %a%a@]")
            id_no_num id
            method_type value
      | (Expression.Object.Property.Identifier id, _) ->
          fprintf fmt "@[<hv>static %a: %a@]"
            id_no_num id
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
  | (x, y) -> fprintf fmt "declare var %s: $Exports<'%s'>;"
    (module_to_identifier x) y

(* This prints the import member from module. Since currently, the flow
   parser does not parse import statements inside a module
   declaration, we use this hack of "typeof"
*)
and import_members fmt = function
  | (x, y) -> fprintf fmt "declare var %s : typeof %s.%s;" x y x

(* The following helper function prints a member of enum as a static
   memeber of the corresponding class. *)
and enum_member fmt = Statement.Enum.(function
  | _, { Member. name; _ } -> fprintf fmt "static %a : number"
    enum_key name
)

(* We do not support computed members in enum. Although a TODO could
   be to strip off the computation part and just print the
   identifier. *)
and enum_key fmt = Expression.Object.Property.(function
  | Identifier id -> id_ fmt id
  (* TODO: Handle the next line as error rather than raising an exception*)
  | Literal (loc, _) -> failwith_location loc
    "Literals are not allowed in Enums"
  | Computed (loc, _) -> failwith_location loc
        "Computed members in enums are not supported by Flow"
)

(* This squash two objects and print them as one *)
and print_combined_class fmt = Type.Object.(function
  | (Some a, Some b) ->
      fprintf fmt "{@;<0 2>@[<v>%a%a%a%a@]@,}"
        (list_ ~sep:";" property) a.properties
        (list ~sep:";" indexer_) a.indexers
        (list_ ~sep:";" property_static) b.properties
        (list ~sep:";" indexer_) b.indexers
  | _ -> failwith "This type of same name interface and var not supported yet"
  )
