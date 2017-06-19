(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Format
open Dts_ast
open Utils_js

(* get_line_number returns a line number corresponding to a
   location *)

let get_line_number = Loc. (function
  | { start; _ } -> start.line
)

let strip_quotes = Str.global_replace (Str.regexp "'\\|\"") ""

let if_true = fun x y -> if x then y else []

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
  | _, _ -> None
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

let rec list_end ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "%a%s"
        f x
        sep
  | x::xs ->
      fprintf fmt "%a%s@,%a"
        f x
        sep
        (list_end ~sep f) xs
  | [] -> ()

let rec _list ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "@,%a%s"
        f x
        sep
  | x::xs ->
      fprintf fmt "@,%a%s@,%a"
        f x
        sep
        (_list ~sep f) xs
  | [] -> ()

(* This is a little modification of the list functions above.
   This does a one step look ahead to find variable and interface
   declarations which can be squashed together as a class. *)
let rec look_ahead ?(sep="") f fmt = function
  | [x] ->
      fprintf fmt "%a%s"
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

(* rename_global_module is used to rename modules defined globally while
   when they are used in global non module statements. Also these modules are
   imported in global using this name.
   This name isn't supposed to be used in implementation files. Purpose is just
   to mimic TypeScript within the declaration environment.
*)
let rename_global_module = fun x -> spf "G_%s" x

let rec rename_reference name = function
| [] -> name
| x :: xs -> if x = name then rename_global_module x else rename_reference x xs
(* Function to convert an arbitrary module name to a valid javascript
   identifier. Change this later as need be.
   Right now it just convert "-" to "$HYPHEN$". *)

let module_to_identifier =
  Str.global_replace (Str.regexp "-") "$HYPHEN$" ;;

(* get_modules_in_scope computes the list of modules which are
   declared in the cuurent scope. In particular we shall use it to
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

and append_name prefix =
  function x -> String.concat "." (List.map (id_name prefix) x)

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
  | _, ModuleDeclaration { Module.id; _ } ->
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
  (* This case checks for function declaration's parameters and returnType.
    Eg.,
          export function f(x: M.C): typeof P.x
    This would return "M" :: "P" :: []
  *)
  | _, AmbientFunctionDeclaration{ AmbientFunctionDeclaration. params;
      returnType; _ } ->
    let acc = module_used_type acc returnType in
    let fold_intermediate x y = module_used_pattern x y in
    List.fold_left fold_intermediate acc params
  (* This case checks for TypeAliases
    Eg.,
          type A = number | M.C
    This would return "M" :: []
  *)
  | _, TypeAlias {TypeAlias. right; _} ->
    module_used_type acc right

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
  | _, Intersection l ->
      let fold_intermediate x y = module_used_type x y in
      List.fold_left fold_intermediate acc l
  | _, Union l ->
      let fold_intermediate x y = module_used_type x y in
      List.fold_left fold_intermediate acc l
  | _ -> acc
)

and module_used_generic acc = Type.(function
  | { Generic.id; _ } -> match id with
    | _, {IdPath.ids; _} -> module_used_ids acc ids
)

and module_used_ids acc = function
  | (_, {Identifier. name; _ })::_ -> SSet.add name acc
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

(* get_modules_to_import returns a list of modules that need to be imported. An
   element of this list is a tuple of two strings A and B. A refers to the name
   by which the module is to be referred to in the current scope and B refers to
   the name to which the module is be referred to in the global
   scope. Ultimately we want to emit something like `import * as A from B`.
*)
let get_modules_to_import scope prefix stmts =
  (* set of possible module names M that appear as M.x or M.N in body *)
  let set_possible_modules =
    SSet.union (get_modules_used stmts) (SSet.of_list prefix) in
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
  SSet.fold (fold_intermediate scope) set_possible_modules []

(*
  The following two functions filter out module and not_module
  statements from a list of statements respectively.
*)

let is_module = (Statement.(function
  | _, (ModuleDeclaration _ | ExportModuleDeclaration _) -> true
  | _ -> false
))

let filter_modules =
  List.filter is_module

let filter_not_modules =
  List.filter (fun x -> not (is_module x))

(*
  Find a list of modules declared within the current module. If you
  only consider the module nodes in the AST then, this is just the
  find_child function of a tree.
*)
let find_child_modules prefix scope = Statement.(function
  | _, ModuleDeclaration { Module.id; body; } ->
    let new_prefix = List.append prefix [get_name [] id] in
    let new_scope = get_modules_in_scope scope new_prefix body in
    new_prefix, new_scope, filter_modules body
  | _, ExportModuleDeclaration { ExportModule.name; body; } ->
    let new_scope = get_modules_in_scope scope [name] body in
    [name], new_scope, filter_modules body
  | loc, _  -> failwith_location loc
    "Unexpected statement. A module declaration was expected"
)

let rec shadow x = function
| [] -> []
| (a, b) :: xs -> if x = a then shadow x xs else (a, b) :: (shadow x xs)

(*
  Find a list of non module statements declared within the current module.

  Cases handled:
    1. Variable Declaration
    2. Class Declaration
    3. Interference Declaration
    4, Enum Declaration

  TODO:
    1. Function Declaration
*)

let rec collect_names = Statement.(function
  | _, ModuleDeclaration{Module. body; id; _ } ->
    let name = get_name [] id in
    name, collect_names_statements body
  | _, ExportModuleDeclaration{ExportModule. body; name; _ } ->
    let name = module_to_identifier name in
    name, collect_names_statements body
  | loc, _ -> failwith_location loc
    "Unexpected statement. A module declaration was expexted"
)

and collect_names_statements xs =
  List.fold_left collect_names_statement [] xs

and collect_names_statement acc = Statement.(function
  | loc, VariableDeclaration { VariableDeclaration.
      declarations; _
    } -> VariableDeclaration.(
      match declarations with
      | [(_, {Declarator.id; _})] ->
        collect_names_pattern acc id
      | _ -> failwith_location loc
        "Only single declarator handled currently"
    )
  | _, AmbientClassDeclaration { AmbientClass. id; _ } ->
    collect_names_id acc id
  | _, InterfaceDeclaration { Interface. id; _ } ->
    collect_names_id acc id
  | _, EnumDeclaration { Enum.name; _ } ->
    collect_names_id acc name
  | _ -> acc
)

and collect_names_pattern acc = Pattern.(function
  | _, Identifier id -> collect_names_id acc id
  | loc, _ -> failwith_location loc
    "Only identifier allowed in variable declaration"
)

and collect_names_id acc = function
  | _, { Identifier. name; _ } -> name :: acc

(*
  To handle flatten nested modules, we decopes the AST into modules
  and not_modules component. Note that the modules will form a tree
  and we just need to flatten that tree. We do this in DFS order for
  readability.
*)

let rec program fmt (_, stmts, _) =
  Format.set_margin 80;
  (* list of ancestor modules *)
  let prefix  = [] in
  (* get modules in current scope, passing prefix *)
  let scope = get_modules_in_scope [] prefix stmts in
  (* Collect modules in current scope, attaching the current environment to each
     module. The current environment consists of prefix, imports (empty in the
     global scope), and scope. *)
  let modules, not_modules = List.partition is_module stmts in
  (* Final list of modules to import, filtering out names that are not module
     names in scope. Each entry is a pair; see comment on
     `get_modules_to_import` for details. *)
  let list_modules_used = get_modules_to_import scope prefix not_modules in

  fprintf fmt "@[<v>%a%a%a@]@."
    (print_modules prefix [] scope) modules
    (* We import all the top level module as global identifiers.
       But rename the rename/garble the identifier. Renaming serves two
       purposes:
       1. No name clashes due the fact that TS has two namespaces and Flow has
          only ony.
       2. In flow we do not wan't modules to be accessbile outside the module
          without explicitly importing them. Since we garble the names, it is
          somewhat hidden.
    *)
    (list_ ~sep:"" (import_module true)) list_modules_used
    (statements true scope) not_modules

(*
  print_modules calls print_module in a DFS order. It can easily be
  converted to BFS order by changing find_child_modules.
*)

(* TODO: fix comment

   And remove those identifier (shadow) from the list of imports "imports" is a
   list of pairs (a, b) where 'a' is the non module entity to be imported and 'b'
   is the of the module whose context 'a' is declared.

   Eg.
   module M {
   variable x : string
   variable y : number
   module N {
   variable x : number
   class A { }
   module N {

   }
   }

   If run on module N, collect_names returns [("y", "M")] instead of
   ("x", "M") :: ("y", "M") :: []

   And append it in imports.

   Eg.

   module M {
   variable x : number
   class A { }
   module N {

   }
   }

   If run on module M, expand_imports returns ("x", "M") :: ("A", "M") :: []

*)

and print_modules prefix imports scope fmt xs =
  list ~sep:"" (print_dfs_module prefix imports scope) fmt xs

and print_dfs_module prefix imports scope fmt x =
    let name, names = collect_names x in
    let set_names = SSet.of_list names in
    let imports =
      List.filter (fun (x, _) -> not (SSet.mem x set_names)) imports in
    let name_names = List.map (fun x -> x, name) names in
    let expanded_imports = List.rev_append name_names imports in
    let new_prefix, new_scope, child_modules =
      find_child_modules prefix scope x in
    fprintf fmt "%a@,%a"
      (print_module scope imports prefix) x
      (print_modules new_prefix expanded_imports new_scope) child_modules

and print_module scope imports prefix fmt = Statement.(function

  (* 1. First we compute all the possible instances of module
     references and then take an intersection with the list of modules
     in scope which are defined in the same file to get a subset of
     actual references to other modules.

     2. Also, we import all the entities declared by any of the anscestors.
     3. And ofcourse we import all the parent modules. This is done to
        assist step 2.
  *)

  | loc, ModuleDeclaration { Module.id; body; } ->
    let name = get_name [] id in
    let new_prefix = List.append prefix [name] in
    let new_scope = get_modules_in_scope scope new_prefix body in
    let list_modules_used = get_modules_to_import new_scope prefix body in

    let print_header fmt = function _ ->
      match list_modules_used, imports with
      | [], [] -> ()
      | _ -> fprintf fmt "%s@,%a%a%s@,"
        "// ** IMPORTS TO EMULATE NESTING **"
        (list_ ~sep:"" (import_module false)) list_modules_used
        (list_ ~sep:"" import_members) imports
        "// ** END OF IMPORTS SECTION **"
    in
    fprintf fmt "@[<v>%s@,declare module %S {@;<0 2>@[<v>%a%a@]@,}@]"
      (spf "// Module declared on line %d in .d.ts file" (get_line_number loc))
      (generate_mangled_name name prefix)
      print_header ()
      (statements false new_scope) (filter_not_modules body)

  | loc, ExportModuleDeclaration { ExportModule.name; body; } ->
    (* ExportModuleDeclaration is allowed only in global scope, that
    is the prefix when an ExportModuleDeclaration is encoutered is ""  *)
    if prefix <> [] then failwith_location loc
      "Export Module Declaration allowed only in the global scope"
    else
      let new_scope = get_modules_in_scope scope [name] body in
      let list_modules_used = get_modules_to_import new_scope prefix body in
      let print_header fmt = function _ ->
        match list_modules_used  with
        | [] -> ()
        | _ -> fprintf fmt "%s@,%a%s@,"
          "// ** IMPORTS TO EMULATE NESTING **"
          (list_ ~sep:"" (import_module false)) list_modules_used
          "// ** END OF IMPORTS SECTION **"
      in
      fprintf fmt "@[<v>%s@,declare module %S {@;<0 2>@[<v>%a%a@]@,}@]"
        (spf "// Module declared on line %d in .d.ts file"
          (get_line_number loc))
        name
        print_header ()
        (statements false scope) (filter_not_modules body)

  | loc, _ -> failwith_location loc "Module declaration expected here"
)

(*
  identifier "scope" contains all a list of modules which are in scope and are
  used in the following statements.
  "if_true is_global scope" retuns "scope" iff current scope is global.
*)

and squash x y is_global scope fmt =
  Statement.(match x, y with
  | (_, InterfaceDeclaration {
      Interface.id; extends; typeParameters; _
     }),
    (_, VariableDeclaration _) ->
    let scope = (if_true is_global scope) in
    fprintf fmt "@[<v>declare class %a%a%a %a@]"
      (id_ scope) id
      (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," (type_param scope))))
      typeParameters
      (extends_interface scope) extends
      (print_combined_class scope)
      ((get_object x), (get_object y))
  | (loc, _), _ -> failwith_location loc
    "Unsupported elements with same name."
  )

and statements is_global scope =
  look_ahead ~sep:"" (statement is_global scope)

and statement is_global scope fmt =
  Statement.(function
  (* This case sqashes a variable with the following interface
     with same name, or vice versa *)
  | ((_, VariableDeclaration _) as y), Some x
  | ((_, InterfaceDeclaration _) as x), Some y ->
    squash x y is_global scope fmt

  | (_, VariableDeclaration { VariableDeclaration.
      declarations; _
    }), _ -> VariableDeclaration.(
      match declarations with
      | [(_, { Declarator.id; _ })] ->
          fprintf fmt "@[<hv>declare var %a;@]"
            (pattern (if_true is_global scope)) id
      | _ -> todo fmt
    )

  | (_, InterfaceDeclaration { Interface.
      id; body; extends; typeParameters;
    }), _ ->
      let scope = (if_true is_global scope) in
      fprintf fmt "@[<v>declare interface %a%a%a %a@]"
        (id_ scope) id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," (type_param scope))))
          typeParameters
        (extends_interface scope) extends
        (object_type scope) (snd body)

  | (_, ExportAssignment id), _ ->
      fprintf fmt "@[<h>declare var exports: typeof %a;@]"
        (id_ scope) id

  | (_, AmbientClassDeclaration { AmbientClass.
      id; body; typeParameters; extends; implements;
    }), _ ->
      let scope = (if_true is_global scope) in
      fprintf fmt "@[<v>declare class %a%a%a%a %a@]"
        (id_ scope) id
        (opt (non_empty "<@[<h>%a@]>" (list ~sep:"," (type_param scope))))
          typeParameters
        (extends_class scope) extends
        (implements_class scope) implements
        (object_type scope) (snd body)

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
  | (loc, EnumDeclaration { Enum. name; members }), _ ->
    (* TODO: Write a doc explaining difference between enums and our
       approximation for the same *)
    let () = prerr_endline "Warning: The declaration file contains \
       enums.\nEnums are converted to class with static members. More\
       details here : <url for doc>\n" in
    fprintf fmt "@[<v>%s@,declare class %a {@;<0 2>@[<v>%a@]@,}@]"
      (spf "// This was originally an Enum. See line %d in .d.ts file."
        (get_line_number loc))
      (id_ []) name
      (list ~sep:"" enum_member) members

  (* The following case handles function declaration *)
  | (_, AmbientFunctionDeclaration {AmbientFunctionDeclaration.
      id; params; returnType; _ }), _ ->
    let scope = (if_true is_global scope) in
    fprintf fmt "@[<hv>declare function %a(%a)%a;@]"
      (id_ scope) id
      (list ~sep:", " (pattern scope)) params
      (annot scope) returnType

  (* The following handles Typescript's commonjs import statements like:

     import M = require("M");

     TODO: handle other import notations as well.
  *)
  | (_, ImportDeclaration {Import. id; entity; _} ), _ ->
      (import_module false) fmt (
        contents (get_identifier_id id),
        strip_quotes (contents (get_identifier_require entity))
      )
  (* The following handles type aliases.
     Eg. : type T = number | string;
  *)
  | (_, TypeAlias {TypeAlias.left; right } ), _ ->
    let scope = (if_true is_global scope) in
    fprintf fmt "@[<hv>declare type %a = %a;@]"
      (generic_type []) (snd left)
      (type_ scope) right

  | (loc, _), _ -> failwith_location loc "This is not supported yet"
)

(* From here on "scope" is [] unless its global scope *)

and pattern scope fmt = Pattern.(function
  | _, Identifier id ->
      (id_ scope) fmt id
  | _ ->
      todo fmt
)

(* id_scope is differnet from id_ in one respect. It renames the identifier
   if it matches with any of the modules in "scope".
*)
and id_scope scope fmt = function
  | _, { Identifier.name; typeAnnotation; _ } ->
      fprintf fmt "%s%a"
        (rename_reference name scope)
        (opt (annot scope)) typeAnnotation

and id_ scope fmt = function
  | _, { Identifier.name; typeAnnotation; _ } ->
      fprintf fmt "%s%a"
        name
        (opt (annot scope)) typeAnnotation

(* convert a number identifier to a string *)
and id_no_num scope fmt = function
  | _, { Identifier.name; typeAnnotation; _ } ->
    let name = try ignore (int_of_string name); spf "%S" name
      with _ -> name in
    fprintf fmt "%s%a"
        name
        (opt (annot scope)) typeAnnotation

and annot scope fmt =
  fprintf fmt ": %a" (type_ scope)

and generic_type scope fmt = Type.(function
  | { Generic.id; typeArguments; } ->
      fprintf fmt "@[%a%a@]"
        (id_path scope) id
        (non_empty "<%a>" (list ~sep:"," (type_ scope))) typeArguments
)

(* id_path renames the first identifier if its a list of length more than
    one. Ofcourse, renaming happens only if the identifier is present
    in "scope".
*)
and id_path scope fmt = function
  | _, { IdPath.ids; _ } ->
      (match ids with
        | [x] -> fprintf fmt "@[<h>%a@]" (id_ scope) x
        | (x :: xs) -> fprintf fmt "@[<h>%a.%a@]"
                        (id_scope scope) x
                        (list ~sep:"." (id_ scope)) xs
        | _ -> fprintf fmt "@[<h>%a@]" (list ~sep:"." (id_ scope)) ids
      )


and type_ scope fmt = Type.(function
  | _, Any -> fprintf fmt "any"
  | _, Void -> fprintf fmt "void"
  | _, Number -> fprintf fmt "number"
  | _, String -> fprintf fmt "string"
  | _, Boolean -> fprintf fmt "boolean"
  | _, Function t -> function_type scope fmt t
  | _, Object t -> object_type scope fmt t
  | _, Array t -> array_type scope fmt t
  | _, Generic t -> generic_type scope fmt t
  | _, Typeof x -> fprintf fmt "typeof %a"
    (id_path scope) x
  | _, Intersection l -> (list ~sep:" & " (type_ scope)) fmt l
  | _, Union l -> (list ~sep:" | " (type_ scope)) fmt l
  | _ -> todo fmt
)

and type_param scope fmt = Type.(function
  | { Param.id; _ } ->
      (id_ scope) fmt id
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
and extends_class scope fmt = function
  | None -> ()
  | Some (_, t) ->
      fprintf fmt "@[ extends %a@]"
        (generic_type scope) t

(* This helper function is for handling extend statement in an
   interface declration. Since an interface can extend more than one
   interfaces, we have a list of interfaces which are extended by the
   current interface.
*)

(* INC : WORKING HERE *)
and extends_interface scope fmt = function
  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ extends %a@]"
        (generic_type scope) t
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
and implements_class _scope _fmt = function
(*  | [] -> ()
  | [_, t] ->
      fprintf fmt "@[ implements %a@]"
        (generic_type scope) t
  | _ -> todo fmt *)
  | _ -> ()

and object_type scope fmt = Type.(function
  | { Object.properties; indexers; } ->
      fprintf fmt "{@;<0 2>@[<v>%a%a@]@,}"
        (list_end ~sep:";" (property scope)) properties
        (_list ~sep:";" (indexer_ scope)) indexers
)

and property scope fmt = Type.Object.(function
  | _, { Property.key; value; _ } ->
    (match key, value with
      | (Expression.Object.Property.Identifier id, (_,Type.Function value)) ->
          fprintf fmt "@[<hv>%a%a@]"
            (id_no_num scope) id
            (method_type scope) value
      | (Expression.Object.Property.Identifier id, _) ->
          fprintf fmt "@[<hv>%a: %a@]"
            (id_no_num scope) id
            (type_ scope) value
      | _ -> todo fmt
    )
)

(* Appends static to all methods and data members apart from constructor *)
and property_static scope fmt = Type.Object.(function
  | _, { Property.key; value; _ } ->
    (match key, value with
      | (Expression.Object.Property.Identifier id, (_,Type.Function value)) ->
          (if get_identifier_id id = Some "new"
          then fprintf fmt "@[<hv>%a%a@]"
          else fprintf fmt "@[<hv>static %a%a@]")
            (id_no_num scope) id
            (method_type scope) value
      | (Expression.Object.Property.Identifier id, _) ->
          fprintf fmt "@[<hv>static %a: %a@]"
            (id_no_num scope) id
            (type_ scope) value
      | _ -> todo fmt
    )
)

and indexer_ scope fmt = Type.Object.(function
  | _, { Indexer.id; key; value; } ->
      fprintf fmt "@[<hv>[%a: %a]: %a@]"
        (id_ scope) id
        (type_ scope) key
        (type_ scope) value
)

and array_type scope fmt t =
  fprintf fmt "Array<%a>"
    (type_ scope) t

and function_type scope fmt = Type.(function
  | { Function.typeParameters; params; rest; returnType; } ->
      fprintf fmt "%a(@;<0 2>@[<hv>%a%a@]@,) => %a"
        (non_empty "<@[<h>%a@]>" (list ~sep:"," (type_param scope)))
          typeParameters
        (list ~sep:", " (param scope)) params
        (opt (rest_ ~follows:(params <> []) scope)) rest
        (type_ scope) returnType
)

and method_type scope fmt = Type.(function
  | { Function.typeParameters; params; rest; returnType; } ->
      fprintf fmt "%a(@;<0 2>@[<hv>%a%a@]@,): %a"
        (non_empty "<@[<h>%a@]>" (list ~sep:"," (type_param scope)))
          typeParameters
        (list ~sep:", " (param scope)) params
        (opt (rest_ ~follows:(params <> []) scope)) rest
        (type_ scope) returnType
)

and param scope fmt = Type.Function.(function
  | _, { Param.name; typeAnnotation; optional } ->
    if optional
    then fprintf fmt "%a?: %a"
      (id_ scope) name
      (type_ scope) typeAnnotation
    else fprintf fmt "%a: %a"
      (id_ scope) name
      (type_ scope) typeAnnotation
)

and rest_ ?(follows=false) scope fmt = Type.Function.(function
  | _, { Param.name; typeAnnotation; _ } ->
    let sep = if follows then ", " else "" in
    fprintf fmt "%s...%a: %a"
      sep
        (id_ scope) name
      (type_ scope) typeAnnotation
)

(* This prints the import module statement. Since currently, the flow
   parser does not parse import statements inside a module
   declaration, we use this hack of $Exports
*)
and import_module is_global fmt = function
  | (x, y) -> fprintf fmt "declare var %s: $Exports<'%s'>;"
    ((if is_global then rename_global_module else fun x -> x)
       (module_to_identifier x)) y

(* This prints the import member from module. Since currently, the flow
   parser does not parse import statements inside a module
   declaration, we use this hack of "typeof"
*)
and import_members fmt = function
  | (x, y) -> fprintf fmt "declare var %s : typeof %s.%s;" x y x

(* The following helper function prints a member of enum as a static
   memeber of the corresponding class. *)
and enum_member fmt = Statement.Enum.(function
  | _, { Member. name; _ } -> fprintf fmt "static %a : number;"
    enum_key name
)

(* We do not support computed members in enum. Although a TODO could
   be to strip off the computation part and just print the
   identifier. *)
and enum_key fmt = Expression.Object.Property.(function
  | Identifier id -> (id_ []) fmt id
  (* TODO: Handle the next line as error rather than raising an exception*)
  | Literal (loc, _) -> failwith_location loc
    "Literals are not allowed in Enums"
  | Computed (loc, _) -> failwith_location loc
        "Computed members in enums are not supported by Flow"
)

(* This squash two objects and print them as one *)
and print_combined_class scope fmt = Type.Object.(function
  | (Some a, Some b) ->
      fprintf fmt "{@;<0 2>@[<v>%a%a@,%a%a@]@,}"
        (list_end ~sep:";" (property scope)) a.properties
        (_list ~sep:";" (indexer_ scope)) a.indexers
        (list_end ~sep:";" (property_static scope)) b.properties
        (_list ~sep:";" (indexer_ scope)) b.indexers
  | _ -> failwith "This type of same name interface and var not supported yet"
  )
