(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module S = Flow_ast.Statement
module E = Flow_ast.Expression
module T = Flow_ast.Type
module P = Flow_ast.Pattern
module Utils = Flowtestgen_utils
module Config = Flowtestgen_config
module FTypes = Flowtestgen_types
module FRandom = Utils.FRandom
open Code

(* Put a statement into a dead if branch *)
let dead_branch (s : S.t') : S.t' =
  let test = Ast.Literal.(Loc.none, E.Literal { value = Boolean false; raw = "false" }) in
  S.If.(S.If { test; consequent = (Loc.none, s); alternate = None })

let mk_prop_read (obj_name : string) (prop_name : string) : E.Member.t =
  E.Member.
    {
      _object = (Loc.none, E.Identifier (Loc.none, obj_name));
      property = PropertyIdentifier (Loc.none, prop_name);
      computed = false;
    }

(* Make a chain of property read *)
let rec prop_read_of_list (plist : string list) : E.Member.t =
  E.Member.(
    match plist with
    | [] -> failwith "prop_read_of_list: Cannot accept empty list"
    | [_] -> failwith "prop_read_of_list: Must have at least two elements"
    | [hd1; hd2] -> mk_prop_read hd1 hd2
    | hd :: tl ->
      {
        _object = (Loc.none, E.Identifier (Loc.none, hd));
        property = PropertyExpression (Loc.none, E.Member (prop_read_of_list tl));
        computed = false;
      })

(* Make an expression code into a statement code *)
let mk_expr_code (e : t') : t =
  let stmt = S.Expression.(S.Expression { expression = (Loc.none, e.expr); directive = None }) in
  { stmt = (Loc.none, stmt); stmt_deps = e.expr_deps }

(* Make a variable . It only makes a literal at this point. We will support
 * other expressions like property read.
 *)
let mk_vardecl_code (vname : string) (vtype : T.t' option) (expr : t' option) : t =
  (* We add type annotation based on random choice *)
  let type_annot =
    match vtype with
    | None -> None
    | Some t -> Some (Loc.none, (Loc.none, t))
  in
  (* Make an identifier *)
  let id =
    P.Identifier.
      ( Loc.none,
        P.Identifier { name = (Loc.none, vname); typeAnnotation = type_annot; optional = false } )
  in
  (* get the expression and its dependencies *)
  let (init, deps) =
    match expr with
    | Some e -> (Some (Loc.none, e.expr), e.expr_deps)
    | None -> (None, [])
  in
  (* Make a var declaration *)
  let decl : S.VariableDeclaration.Declarator.t =
    S.VariableDeclaration.Declarator.(Loc.none, { id; init })
  in
  let var_decl : S.VariableDeclaration.t =
    S.VariableDeclaration.{ declarations = [decl]; kind = Var }
  in
  let decl_stmt = S.(Loc.none, VariableDeclaration var_decl) in
  { stmt = decl_stmt; stmt_deps = deps }

(* Make an object initialization expression according to etype.
 * This only creates an object with a single property. Maybe
 * we could have more properties in the future.
 *)
let mk_obj_init_expr ?init_expr (prop_key : E.Object.Property.key) (etype : T.t') : t' =
  (* Make an init expression of the given type *)
  let init_expr =
    match init_expr with
    | None -> FTypes.mk_literal_expr etype
    | Some e -> e
  in
  (* Make the property *)
  let prop =
    E.Object.Property.
      {
        key = prop_key;
        value = Init (Loc.none, init_expr.expr);
        _method = false;
        shorthand = false;
      }
  in
  let content = E.Object.(E.Object { properties = [Property (Loc.none, prop)] }) in
  { expr = content; expr_deps = init_expr.expr_deps }

(* Make an object constructor. Return the constructor and its name *)
let mk_obj_constructor (prop_name : string) (t : T.t') : t * string =
  let fname = Utils.mk_obj_cons () in
  let param =
    P.Identifier.
      ( Loc.none,
        P.Identifier
          {
            name = (Loc.none, prop_name);
            typeAnnotation = Some (Loc.none, (Loc.none, t));
            optional = false;
          } )
  in
  let assign_expr =
    E.Assignment.(
      let left = P.Expression (Loc.none, E.Member (mk_prop_read "this" prop_name)) in
      let right = E.Identifier (Loc.none, prop_name) in
      E.Assignment { operator = Assign; left = (Loc.none, left); right = (Loc.none, right) })
  in
  (* Randomly put the assignment into a dead if statement *)
  let assign_stmt =
    S.Expression.(S.Expression { expression = (Loc.none, assign_expr); directive = None })
    |> FRandom.rchoice [|(fun id -> id); dead_branch|]
  in
  let body = S.Block.{ body = [(Loc.none, assign_stmt)] } in
  let func =
    Ast.Function.
      {
        id = Some (Loc.none, fname);
        params = ([param], None);
        body = Ast.Function.BodyBlock (Loc.none, body);
        async = false;
        generator = false;
        predicate = None;
        expression = false;
        returnType = None;
        typeParameters = None;
      }
  in
  ({ stmt = (Loc.none, S.FunctionDeclaration func); stmt_deps = [] }, fname)

(* Generate the code for object declaration given a property
   read.*)
let rec mk_objdecl_code ?init_expr (read : E.Member.t) (t : T.t') : t =
  E.Member.(
    (* Make a property *)
    let prop_id =
      match read.property with
      | PropertyIdentifier (_, id) -> id
      | _ -> failwith "Property name has to be an identifier."
    in
    let prop_key = E.Object.Property.(Identifier (Loc.none, prop_id)) in
    (* Get the object name according to the property read *)
    let obj_name =
      match read._object with
      | (_, E.Identifier id) -> id
      | _ -> failwith "Object name has to be an identifier."
    in
    (* Get the object type *)
    let obj_type = FTypes.mk_obj_type [(prop_id, t)] in
    let obj_init =
      match init_expr with
      | None -> mk_obj_init_expr prop_key t
      | Some e -> e
    in
    match FRandom.int 4 with
    | 0 ->
      (* We create normal object decl with init expr *)
      mk_vardecl_code
        (snd obj_name)
        Config.(
          match config.type_annot with
          | Force -> Some obj_type
          | Random -> FRandom.rchoice [|None; Some obj_type|]
          | No -> None)
        (Some obj_init)
    | 1 ->
      (* We create an empty object first and then assign
       the property to it *)
      let empty_init = E.Object.{ expr = E.Object { properties = [] }; expr_deps = [] } in
      let empty_decl =
        if FRandom.rbool () then
          mk_vardecl_code (snd obj_name) None (Some empty_init)
        else
          let empty_body = S.Block.{ body = [] } in
          let empty_func =
            Ast.Function.
              {
                id = Some obj_name;
                params = ([], None);
                body = Ast.Function.BodyBlock (Loc.none, empty_body);
                async = false;
                generator = false;
                predicate = None;
                expression = false;
                returnType = None;
                typeParameters = None;
              }
          in
          { stmt = (Loc.none, S.FunctionDeclaration empty_func); stmt_deps = [] }
      in
      let left = P.Expression (Loc.none, E.Member read) in
      let right = FTypes.mk_literal_expr t in
      let assign =
        E.Assignment.(
          E.Assignment
            { operator = Assign; left = (Loc.none, left); right = (Loc.none, right.expr) })
      in
      let assign_stmt =
        S.Expression.(S.Expression { expression = (Loc.none, assign); directive = None })
        |> FRandom.rchoice [|(fun id -> id); dead_branch|]
      in
      { stmt = (Loc.none, assign_stmt); stmt_deps = empty_decl :: right.expr_deps }
    | 2 ->
      (* create an object with a constructor *)
      let (con, fname) = mk_obj_constructor prop_id t in
      let new_call =
        let callee = (Loc.none, E.Identifier (Loc.none, fname)) in
        let init_expr = FTypes.mk_literal_expr t in
        let arguments = [E.Expression (Loc.none, init_expr.expr)] in
        let call_expr = E.New.(E.New { callee; arguments }) in
        { expr = call_expr; expr_deps = [con] }
      in
      mk_vardecl_code
        (snd obj_name)
        ( if FRandom.rbool () then
          Some obj_type
        else
          None )
        (Some new_call)
    | _ ->
      (* Create an object using Object.create() *)
      let create_call =
        let callee = (Loc.none, E.Member (mk_prop_read "Object" "create")) in
        let proto_name = Utils.mk_var () in
        let proto_id = E.Identifier (Loc.none, proto_name) in
        let proto_decl = mk_objdecl_code (mk_prop_read proto_name prop_id) t in
        let arguments = [E.Expression (Loc.none, proto_id)] in
        let call_expr = E.Call.(E.Call { callee; arguments }) in
        { expr = call_expr; expr_deps = [proto_decl] }
      in
      mk_vardecl_code
        (snd obj_name)
        ( if FRandom.rbool () then
          Some obj_type
        else
          None )
        (Some create_call))

(* Make an object mutatation statement which assigns
 * a new property to an object
 *)
let mk_mutation_assignment_code ?rhs (read : E.Member.t) (t : T.t') (deps : t list) : t =
  let left = P.Expression (Loc.none, E.Member read) in
  let right =
    match rhs with
    | None -> FTypes.mk_literal_expr t
    | Some e -> e
  in
  let assign =
    E.Assignment.(
      E.Assignment { operator = Assign; left = (Loc.none, left); right = (Loc.none, right.expr) })
  in
  (* Randomly put the assignment into a dead if statement *)
  let assign_stmt =
    S.Expression.(S.Expression { expression = (Loc.none, assign); directive = None })
    |> FRandom.rchoice [|(fun id -> id); dead_branch|]
  in
  { stmt = (Loc.none, assign_stmt); stmt_deps = deps @ right.expr_deps }

(* Define a function for function calls *)
let mk_obj_mutation_funcdecl_code (fname : Ast.Identifier.t) (prop_name : string) (t : T.t') : t =
  let obj_type = (Loc.none, (Loc.none, FTypes.mk_obj_type [(prop_name, t)])) in
  (* We add type annotation based on random choice *)
  let type_annot =
    Config.(
      match config.type_annot with
      | Force -> Some obj_type
      | Random -> FRandom.rchoice [|None; Some obj_type|]
      | No -> None)
  in
  let param_name = Utils.mk_var () in
  let param =
    P.Identifier.
      [
        ( Loc.none,
          P.Identifier
            { name = (Loc.none, param_name); typeAnnotation = type_annot; optional = false } );
      ]
  in
  let prop_read =
    if FRandom.rbool () then
      (* We use "arguments" *)
      let prop = E.Literal Ast.Literal.{ value = Number 0.0; raw = "0" } in
      let args =
        E.Member.
          {
            _object = (Loc.none, E.Identifier (Loc.none, "arguments"));
            property = PropertyExpression (Loc.none, prop);
            computed = true;
          }
      in
      E.Member.
        {
          _object = (Loc.none, E.Member args);
          property = PropertyIdentifier (Loc.none, prop_name);
          computed = false;
        }
    else
      E.Member.
        {
          _object = (Loc.none, E.Identifier (Loc.none, param_name));
          property = PropertyIdentifier (Loc.none, prop_name);
          computed = false;
        }
  in
  let assign_stmt = mk_mutation_assignment_code prop_read t [] in
  (* Generate the body *)
  let body = S.Block.{ body = assign_stmt |> Utils.list_of_code } in
  let func =
    Ast.Function.
      {
        id = Some fname;
        params = (param, None);
        body = Ast.Function.BodyBlock (Loc.none, body);
        async = false;
        generator = false;
        predicate = None;
        expression = false;
        returnType = None;
        typeParameters = None;
      }
  in
  { stmt = (Loc.none, S.FunctionDeclaration func); stmt_deps = [] }

(* Mutate the value using a single assignment *)
let mk_assignment_mutation (etype : T.t') (obj_name : string) (prop_name : string) (obj_decl : t) :
    t =
  (* Create a new object with the given type *)
  let new_obj = Utils.mk_var () in
  let prop_read = mk_prop_read new_obj prop_name in
  let init_expr = { expr = E.Identifier (Loc.none, obj_name); expr_deps = [obj_decl] } in
  let new_obj_decl =
    let obj_type = FTypes.mk_obj_type [(prop_name, etype)] in
    mk_vardecl_code
      new_obj
      Config.(
        match config.type_annot with
        | Force -> Some obj_type
        | Random -> FRandom.rchoice [|None; Some obj_type|]
        | No -> None)
      (Some init_expr)
  in
  mk_mutation_assignment_code prop_read etype [new_obj_decl]

(* Make an object mutation statement using function calls *)
let mk_func_mutation (etype : T.t') (obj_name : string) (prop_name : string) (obj_decl : t) : t =
  (* Get the object name according to the property read *)
  let funcname = (Loc.none, Utils.mk_func ()) in
  let obj_id = E.Identifier (Loc.none, obj_name) in
  let call =
    E.Call.(
      E.Call
        {
          callee = (Loc.none, E.Identifier funcname);
          arguments = [E.Expression (Loc.none, obj_id)];
        })
  in
  let func_def = mk_obj_mutation_funcdecl_code funcname prop_name etype in
  mk_expr_code { expr = call; expr_deps = [obj_decl; func_def] }

let mk_array_mutation (etype : T.t') (obj_name : string) (prop_name : string) (prev_stmt : t) : t =
  (* Create a new object with the given type *)
  let new_obj = Utils.mk_var () in
  let prop_read = mk_prop_read new_obj prop_name in
  let init_expr = { expr = E.Identifier (Loc.none, obj_name); expr_deps = [prev_stmt] } in
  let new_obj_decl =
    let obj_type = FTypes.mk_obj_type [(prop_name, etype)] in
    mk_vardecl_code
      new_obj
      Config.(
        match config.type_annot with
        | Force -> Some obj_type
        | Random -> FRandom.rchoice [|None; Some obj_type|]
        | No -> None)
      (Some init_expr)
  in
  match etype with
  | T.Array (_, atype) ->
    let fname = FRandom.choice [|"push"; "pop"; "shift"; "length"|] in
    let callee =
      E.Member.(
        E.Member
          {
            _object = (Loc.none, E.Member prop_read);
            property = PropertyIdentifier (Loc.none, fname);
            computed = false;
          })
    in
    let expr =
      match fname with
      | "push"
      | "pop"
      | "shift" ->
        let arg = FTypes.mk_literal_expr atype in
        let arguments =
          if fname = "push" then
            [E.Expression (Loc.none, arg.expr)]
          else
            []
        in
        if FRandom.rbool () then
          E.Call.(E.Call { callee = (Loc.none, callee); arguments })
        else
          (* Call the function from prototype *)
          let proto_call = prop_read_of_list ["Array"; "prototype"; fname; "apply"] in
          let proto_args = E.Expression (Loc.none, E.Member prop_read) :: arguments in
          E.Call.(E.Call { callee = (Loc.none, E.Member proto_call); arguments = proto_args })
      | "length" ->
        let left = callee in
        let right = E.Literal Ast.Literal.{ value = Number 1.0; raw = "1" } in
        E.Assignment.(
          E.Assignment
            {
              operator = Assign;
              left = (Loc.none, P.Expression (Loc.none, left));
              right = (Loc.none, right);
            })
      | _ -> failwith ("array mutation: '" ^ fname ^ "' is unsupported")
    in
    { expr; expr_deps = [new_obj_decl] } |> mk_expr_code
  | _ -> failwith "Can only accept array type here"
