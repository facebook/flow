(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This file contains util functions*)
module S = Ast.Statement;;
module E = Ast.Expression;;
module T = Ast.Type;;
module P = Ast.Pattern;;

(*
class renamer(target : string,  = object(this)
  inherit Flow_ast_mapper.mapper as super
  method! expression (expr : Loc.t E.t) =
end
   *)
  

module StrSet = Set.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

(* This is a special "random" number generator used for code
   generation. It is special in a way that it ensures that it doesn't
   generate the same random number given the same history.

   For example, if it gives 0,0,0,0 during the first program
   generation, it will give 1,0,0,0 for the second program
   generation.

   The algorithm simply keeps track of histories when generating
   numbers. When we want to generate a number and the generator has
   already generated 0,1,0, the generator will return 0 for the first
   time because it is the first time it generates a number under the
   history 0,1,0. If later on it is asked to generate another number
   under the history 0,1,0, it will generate (0 + 1) % max.

   Essentially this random number generator turns a random walk
   algorithm into a combinatorial search algorithm without
   stopping. The reason why we need this is that we don't want to
   generate repetative programs.
*)
module FRandom = struct
  (* A hash table storing history counts *)
  let hist_tbl = Hashtbl.create 10000

  (* The current history for a codegen session *)
  let history = ref ""

  (* init functions *)
  let init_hist () = history := ""
  let init_all_hist ()  = Hashtbl.clear hist_tbl

  (* The most important function in this module *)
  let int (limit : int) : int =
    (* insert a new history if necessary *)
    if not (Hashtbl.mem hist_tbl !history) then
      Hashtbl.add hist_tbl !history 0;

    (* get the count of seeing this history *)
    let count = Hashtbl.find hist_tbl !history in

    (* increment the history count *)
    Hashtbl.replace hist_tbl !history (count + 1);

    (* return a number and update the current history *)
    let result = if limit = 0 then 0 else count mod limit in
    history := !history ^ (string_of_int result) ^ ",";
    result

  (* The rest are based on int function *)
  let bool () : bool = int 2 = 0
  let choice (arr : 'a array) : 'a =
    let index = int (Array.length arr) in
    Array.get arr index

  (* Real randomness here *)
  let rint (limit : int) : int =
    if limit = 0
    then 0
    else Random.int limit
  let rbool () : bool = Random.bool ()
  let rchoice (arr : 'a array) : 'a =
    let index = rint (Array.length arr) in
    Array.get arr index
end;;


(* Generate a sequence of numbers *)
let sequence i j =
  let rec helper n acc =
    if n < i then acc else helper (n - 1) (n :: acc) in
  helper j [];;

(* Read all lines from the in_channel *)
let read_all ic : string list =
  let lines = ref [] in
  try
    while true; do
      lines := input_line ic :: !lines
    done; !lines
  with End_of_file ->
    close_in ic;
    List.rev !lines

(* Read from a file into a string *)
let read_file filename =
  let ic = open_in filename in
  let lines = read_all ic in
  String.concat "\n" lines

(* convert an AST into a string for printing *)
    (*
let string_of_ast endline func =
  fun (ast) ->
    let layout = func (Loc.none, ast) in
    let open Layout_printer in
    let s = (Source.contents (PrettyPrinter.print layout)) in
    if endline then s
    else s |> Str.global_replace (Str.regexp "\n") "";;
let string_of_stmt =
  string_of_ast true Js_layout_generator.statement;;
let string_of_expr =
  string_of_ast false Js_layout_generator.expression;;
let string_of_type =
  string_of_ast false Js_layout_generator.type_;;
*)


(* A hacky version of string_of function for AST nodes.
   This will be replaced once we have a better solution.
*)
let rec string_of_pattern (pattern : Loc.t P.t') =
  match pattern with
  | P.Identifier id ->
    let open P.Identifier in
    (snd id.name) ^
    (if id.optional then "?" else "") ^ " " ^
    (match id.typeAnnotation with
     | Some (_, (_, t)) -> " : " ^ (string_of_type t)
     | None -> "")
  | P.Expression (_, e) -> string_of_expr e
  | P.Assignment assign ->
    let open P.Assignment in
    (string_of_pattern (snd assign.left)) ^
    " = " ^
    (string_of_expr (snd assign.right))
  | _ -> failwith "[string_of_pattern] unsupported pattern"

and string_of_expr (expr : Loc.t E.t') =
  let string_of_proplist plist =
    let helper prop = match prop with
      | E.Object.Property (_, p) ->
        let open E.Object.Property in
        (match p.key, p.value with
         | Identifier (_, name), Init (_, e) -> name ^ " : " ^ (string_of_expr e)
         | _ -> failwith "Unsupported expression")
      | _ -> failwith "Unsupported property" in
    String.concat ", " (List.map helper plist) in

  let string_of_assign_op op =
    let open E.Assignment in
    match op with
    | Assign -> "="
    | PlusAssign -> "+="
    | MinusAssign -> "-="
    | MultAssign -> "*="
    | ExpAssign -> "^="
    | DivAssign -> "/="
    | _ -> failwith "unsupported assign" in

  match expr with
  | E.Object o ->
    "{" ^ (string_of_proplist E.Object.(o.properties)) ^ "}"
  | E.Literal lit -> Ast.Literal.(lit.raw)
  | E.Assignment assign ->
    let open E.Assignment in
    [(string_of_pattern (snd assign.left));
     (string_of_assign_op assign.operator);
     (string_of_expr (snd assign.right))]
    |> String.concat " "
  | E.Call call ->
    let open E.Call in
    let callee_str = string_of_expr (snd call.callee) in
    let arglist_str =
      call.arguments
      |> List.map (fun a -> match a with
          | E.Expression (_, e) -> e
          | E.Spread _ -> failwith "[string_of_expr] call does not support spread argument.")
      |> List.map string_of_expr
      |> String.concat ", " in
    callee_str ^ "(" ^ arglist_str ^ ")"
  | E.Identifier (_, id) -> id
  | E.Member mem ->
    let open E.Member in
    let obj_str = string_of_expr (snd mem._object) in
    let prop_str = match mem.property with
      | PropertyIdentifier (_, id) -> id
      | PropertyExpression (_, e) -> string_of_expr e
      | PropertyPrivateName (_, (_, id)) -> id in
    obj_str ^ "." ^ prop_str
  | E.TypeCast cast ->
    let open E.TypeCast in
    (string_of_expr (snd cast.expression)) ^
    " : " ^
    (string_of_type (snd (snd cast.typeAnnotation)))
  | E.Array array ->
    let open E.Array in
    "[" ^
    (List.map (fun elt -> match elt with
         | Some (E.Expression (_, e)) -> string_of_expr e
         | Some (E.Spread (_, e)) -> string_of_expr (E.SpreadElement.((snd e.argument)))
         | None -> "") array.elements
     |> (String.concat ", ")) ^
      "]"
  | _ -> failwith "unknown expr"

and string_of_stmt (stmt : Loc.t S.t') =
  match stmt with
  | S.Block b ->
    S.Block.(b.body)
    |> List.map snd
    |> List.map string_of_stmt
    |> String.concat "\n"
  | S.Empty -> "\n"
  | S.FunctionDeclaration func ->
    let open Ast.Function in
    let fname = match func.id with
      | Some (_, n) -> n
      | None -> "" in
    let params_str =
      let (_, { Ast.Function.Params.params; rest = _ }) = func.params in
      params
      |> List.map snd
      |> List.map string_of_pattern
      |> String.concat ", " in
    let body_str = match func.body with
      | BodyBlock (_, s) -> string_of_stmt (S.Block s)
      | BodyExpression (_, e) -> string_of_expr e in
    let ret_type_str = match func.returnType with
      | Some (_, (_, t)) -> ": " ^ string_of_type t
      | None -> "" in
    "function " ^ fname ^ "(" ^ params_str ^ ") " ^ ret_type_str ^ " {\n" ^
    body_str ^
    "}\n"
  | S.Return r ->
    let open S.Return in
    (match r.argument with
     | Some (_, e) -> "return " ^ (string_of_expr e) ^ "\n;"
     | None -> "return;\n")
  | S.VariableDeclaration decl ->
    let open S.VariableDeclaration in
    let string_of_dtor dtor =
      let open S.VariableDeclaration.Declarator in
      let init_str = match dtor.init with
        | Some (_, e) -> "= " ^ (string_of_expr e)
        | None -> "" in
      (string_of_pattern (snd dtor.id)) ^ init_str in
    let kind_str = match decl.kind with
      | Var -> "var"
      | Let -> "let"
      | Const -> "const" in
    let dlist = List.map snd decl.declarations in
    let dlist_str = String.concat ", " (List.map string_of_dtor dlist) in
    kind_str ^ " " ^ dlist_str ^ "\n"
  | S.Expression e ->
    let open S.Expression in
    (string_of_expr (snd e.expression)) ^ ";\n"
  | _ -> failwith "[string_of_stmt] Unspported stmt"

and string_of_type (t : Loc.t T.t') =

  match t with
  | T.Any -> "any"
  | T.Mixed -> "mixed"
  | T.Empty -> "empty"
  | T.Void -> "void"
  | T.Null -> "null"
  | T.Number -> "number"
  | T.String -> "string"
  | T.Boolean -> "boolean"
  | T.Object ot ->
    let open T.Object in
    let string_of_prop prop = match prop with
      | T.Object.Property (_, p) ->
        let open T.Object.Property in
        let key_str = match p.key with
          | E.Object.Property.Literal (_, lit)  -> Ast.Literal.(lit.raw)
          | E.Object.Property.Identifier (_, name) -> name
          | E.Object.Property.PrivateName (_, (_, name)) -> name
          | E.Object.Property.Computed (_, e) -> string_of_expr e in
        let t_str = match p.value with
          | Init (_, init_t) -> string_of_type init_t
          | Get (_, ft) -> string_of_type (T.Function ft)
          | Set (_, ft) -> string_of_type (T.Function ft) in
        let opt = if p.optional then "?" else "" in
        key_str ^ opt ^ " : " ^ t_str
      | _ -> failwith "[string_of_prop] unsupported property" in
    let prop_str_list = ot.properties
      |> List.map string_of_prop
      |> String.concat ", " in
    if ot.exact then "{|" ^ prop_str_list ^ "|}"
    else "{" ^ prop_str_list ^ "}"
  | T.Union ((_, t1), (_, t2), trest) ->
    let t_strlist =
      [(string_of_type t1); (string_of_type t2)]
      @ (trest |> (List.map snd) |> (List.map string_of_type)) in
    String.concat " | " t_strlist
  | T.StringLiteral st -> T.StringLiteral.(st.raw)
  | T.NumberLiteral nt -> T.NumberLiteral.(nt.raw)
  | T.BooleanLiteral bt -> if bt then "true" else "false"
  | T.Function func ->
    let open T.Function in
    let string_of_param param =
      let open T.Function.Param in
      let opt_str = if param.optional then "?" else "" in
      let name_str = match param.name with
        | Some (_, id) -> id ^ opt_str ^ " : "
        | None -> "" in
      name_str ^ (string_of_type (snd param.typeAnnotation)) in
    let params_str =
      let (_, { T.Function.Params.params; rest = _ }) = func.params in
      params
      |> List.map snd
      |> List.map string_of_param
      |> String.concat ", " in
    let ret_type_str = (string_of_type (snd func.returnType)) in
    "(" ^ params_str ^ ") => " ^ ret_type_str
  | _ -> failwith "[string_of_type] unsupported type"


(* A generator function for creating functions that makes variables and
 * properties
 *)
let mk_gen (prefix : string) =
  let count = ref 0 in
  fun () ->
    let vname : string = prefix ^ (string_of_int !count) in
    count := !count + 1;
    vname;;

(* A function that makes unique names. *)
let mk_var = mk_gen "v_";;
let mk_prop = mk_gen "p_";;
let mk_func = mk_gen "f";;
let mk_obj_cons = mk_gen "Obj";;

(* Convert a code and its dependencies into a list
   CAUTION: This function will lose some independencies between
   codes *)
let list_of_code (code : Code.t) : Loc.t Ast.Statement.t list =
  let open Code in
  let rec helper acc lst = match lst with
    | [] -> acc
    | hd :: tl ->
      hd.stmt :: (helper (helper acc hd.stmt_deps) tl) in
  (code.stmt :: (helper [] code.stmt_deps)) |> List.rev


(* Convert a list of statements into a code object. Dependencies
   are based on the order of the statements. Thus, it will create
   unnecessary dependnecies. USE THIS WITH CAUTION. *)
let code_of_stmt_list (slist : Loc.t Ast.Statement.t list) : Code.t option =
  let open Code in

  let rec helper lst = match lst with
    | [] -> failwith "List is empty, but this cannot happen"
    | hd :: [] -> {stmt = hd; stmt_deps = []}
    | hd :: tl -> {stmt = hd; stmt_deps = [helper tl]} in

  if (List.length slist) = 0 then None
  else
    let rev_slist = List.rev slist in
    Some (helper rev_slist)

(* We also remove redundant assignment as well. Redundant
   assignments will appear after empty object init.
*)
let rm_prop_write
    (prop : Loc.t E.Member.t)
    (clist : Code.t list) : Code.t list =
  let open S.Expression in
  let open Code in
  let is_target (code : Code.t) : bool =
    match code.stmt with
    | (_, S.Expression {expression = (_, E.Assignment assign);
                        directive = _}) ->
      (match E.Assignment.(assign.left) with
       | (_, P.Expression (_, E.Member p)) when p = prop -> false
      | _ -> true)
    | _ -> true in
  List.filter is_target clist;;

(* Remove variable declaration from a list of code where vname is
 * defined
 *)
let rm_vardecl
    (vname : string)
    (clist : Code.t list) : Code.t list =
  let open S.VariableDeclaration.Declarator in
  let open S.VariableDeclaration in

  (* Check whether this declaration defines the target variable *)
  let is_target (decl : Loc.t S.VariableDeclaration.Declarator.t) =
    let decl' = (snd decl) in
    match decl'.id with
    | (_, P.Identifier { P.Identifier.name = (_, name); _;})
      -> name != vname
    | _ -> true in

  let open Code in
  List.fold_left (fun acc code -> match code.stmt with
      | (loc, S.VariableDeclaration {declarations = decls; kind = k;}) ->
        (* Remove vname's decls *)
        (match List.filter is_target decls with
         (* No declarators. We remove this statement *)
         | [] -> acc
          (* Create a new var decl statement *)
         | lst ->
           let new_stmt = {declarations = lst; kind = k} in
           let new_code =
             {stmt = (loc, (S.VariableDeclaration new_stmt));
              stmt_deps = code.stmt_deps} in
           new_code :: acc)
      | _ -> code :: acc) [] clist |> List.rev


(* This is the JSON config library. It loads a JSON file into a list
   of (name, value) pairs. Simply provide load_config a JSON filename
   and access the values through the get functions.

   This module provides simple and easy value retrieval by supporting
   accessing using strings as key names separated by dots, since
   config might be recursive. For example, if we have a config like
   this:

   {"n" : 100, "o" : {"foo" : "bar"}}

   we can just provide "o.foo" to access "foo" inside "o". Therefore,
   dots are not allowed inside key names.

   In order to benefit from type checking, it is highly recommended to
   convert the config into a domain-specific record type once the JSON
   config is loaded. Try to limit the number of get function calls,
   because it is very easy to get runtime error.
*)

module Config = struct

  (* config type *)
  type value =
      Int of int
    | Str of string
    | Bool of bool
    | Obj of t
  and t = (string * value) list;;

  (* Convert a JSON ast into a config *)
  let rec to_config (ast : Loc.t E.Object.t) : t =

    (* get config value from an expression *)
    let get_value (expr : Loc.t E.t') : value = match expr with
      | E.Object o -> Obj (to_config o)
      | E.Literal lit -> let open Ast.Literal in
        (match lit.value with
         | String s -> Str s
         | Boolean b -> Bool b
         | Number n -> Int (int_of_float n)
         | _ -> failwith "We only support string, bool, and int as config vals.")
      | _ -> failwith "Unknown AST type for config" in

    let open E.Object in

    (* get all the properties *)
    let prop_list = (List.map (fun p -> match p with
        | Property (_, prop) ->
          let k = E.Object.Property.(prop.key) in
          let k = (match k with
              | E.Object.Property.Literal (_, id) -> Ast.Literal.(match id.value with
                  | String s ->
                    if String.contains s '.' then
                      failwith ("Config key '" ^
                                s ^
                                "' contains dots which are not allowed");
                    s
                  | _ -> failwith "Config key can only be a string")
              | _ -> failwith "Config key can only be a string literal.") in
          let v = E.Object.Property.(prop.value) in
          let v = (match v with
              | E.Object.Property.Init (_, e) -> get_value e
              | _ -> failwith "Config values can only be expressions.") in
          (k, v)
        | _ -> failwith "Spread properties are not allowed") ast.properties) in
    prop_list

  (* Convert a config into an expression ast. Mainly used for printing *)
  let rec ast_of_config (c : t) : Loc.t E.Object.t =

    let expr_of_value (v : value) : Loc.t E.t' = let open Ast.Literal in
      match v with
      | Int i -> E.Literal {value = Number (float_of_int i); raw = string_of_int i}
      | Str s -> E.Literal {value = String s; raw = "\"" ^ s ^ "\""}
      | Bool b -> E.Literal {value = Boolean b; raw = string_of_bool b}
      | Obj o -> E.Object (ast_of_config o) in

    (* Convert all properties into object properties *)
    let open E.Object in
    let prop_list =
      let open E.Object.Property in
      List.map (fun (k, v) ->
          let key = Identifier (Loc.none, "\"" ^ k ^ "\"") in
          let value = Init (Loc.none, expr_of_value v) in
          Property (Loc.none, {key;
                               value;
                               _method = false;
                               shorthand = false})) c in
    {properties = prop_list};;

  (* Convert a config into string for printing *)
  let string_of_config (c : t) : string =
    let ast = ast_of_config c in
    string_of_expr (E.Object ast)

  (* Return an empty config *)
  let empty () : t =
    let open E.Object in
    to_config {properties = []};;

  (* Get a value from the config given a string.*)
  let get (conf : t) (prop_name : string) : value =
    let name_list = Str.split (Str.regexp "\\.") prop_name in

    let rec helper (c : t) (slist : string list) = match slist with
      | [] -> failwith "Config is empty"
      | hd :: [] -> List.assoc hd c
      | hd :: tl -> (match List.assoc hd c with
          | Obj o -> helper o tl
          | _ -> failwith "It has to be a config type") in
    try
      helper conf name_list
    with
      Not_found -> failwith ("No config value for '" ^ prop_name)

  (* Normal get function requires users to do type conversion. That's
     why we are creating these functions. It checks types as well.
  *)
  let get_int ?default (conf : t) (prop_name : string) : int =
    match get conf prop_name with
    | Int i -> i
    | _ -> (match default with
        | None -> failwith ("Config '" ^ prop_name ^ "' is not an int.")
        | Some i -> i);;
  let get_str ?default (conf : t) (prop_name : string) : string =
    match get conf prop_name with
    | Str s -> s
    | _ -> (match default with
        | None -> failwith ("Config '" ^ prop_name ^ "' is not a string.")
        | Some s -> s);;
  let get_bool ?default (conf : t) (prop_name : string) : bool =
    match get conf prop_name with
    | Bool b -> b
    | _ -> (match default with
        | None -> failwith ("Config '" ^ prop_name ^ "' is not a boolean.")
        | Some b -> b);;

  (* load a config from a string *)
  let load_json_config_string ?filename json_str : t =
    let expr_ast = Parser_flow.json_file
        json_str
        (match filename with
         | None -> None
         | Some f -> (Some (Loc.JsonFile f))) in
    to_config (match (fst expr_ast) with
        | (_, E.Object o) -> o
        | _ -> failwith "Can only be an object")

  (* Load a config into _config *)
  let load_json_config (filename : string) : t =
    let content = read_file filename in
    load_json_config_string ~filename content;;
end
