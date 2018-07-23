(**
 * Copyright (c) 2014, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a change' =
  | Replace of 'a * 'a
  (* TODO add Insert and Delete as part of the implementation of a real diff algorithm *)

type 'a change = (Loc.t * 'a change')

(* Position in the list is necessary to figure out what Loc.t to assign to insertions. *)
type 'a diff_result = (int (* position *) * 'a change')

(* diffs based on identity *)
(* return None if no good diff was found (max edit distance exceeded, etc.) *)
let list_diff (old_list : 'a list) (new_list : 'a list) : ('a diff_result list) option =
  (* For now,
   * inspect the lists pairwise and record any items which are different as replacements. Give up if
   * the lists have different lengths.
   *
   * TODO implement a real diff algorithm, e.g. http://www.xmailserver.org/diff2.pdf. Note that this
   * diff algorithm provides a list of deletions and insertions. We will have to do a pass over
   * these results to convert a deletion and insertion at the same location into a replacement, in
   * order to allow us to recurse into the nodes and provide a more fine-grained diff.
   *)
  let rec helper i lst1 lst2 =
    match lst1, lst2 with
    | [], [] -> Some []
    | hd1::tl1, hd2::tl2 ->
      let rest = helper (i + 1) tl1 tl2 in
      if hd1 != hd2 then
        Option.map rest ~f:(List.cons (i, Replace (hd1, hd2)))
      else
        rest
    | _, []
    | [], _ ->
      None
  in
  if old_list == new_list then Some []
  else helper 0 old_list new_list

(* Runs `list_diff` and then recurses into replacements (using `f`) to get more granular diffs. *)
let diff_and_recurse
    (f: 'a -> 'a -> 'b change list option)
    (old_list: 'a list)
    (new_list: 'a list)
    : 'b change list option =
  let changes = list_diff old_list new_list in
  Option.bind changes begin fun changes ->
    changes
    |> List.map begin function
    | _, Replace (x1, x2) ->
      f x1 x2
    end
    |> Option.all
    |> Option.map ~f:List.concat
  end

(* We need a variant here for every node that we want to be able to store a diff for. The more we
 * have here, the more granularly we can diff. *)
type node =
  | Statement of Loc.t Ast.Statement.t
  | Program of Loc.t Ast.program
  | Expression of Loc.t Ast.Expression.t
  | Identifier of Loc.t Ast.Identifier.t

(* This is needed because all of the functions assume that if they are called, there is some
 * difference between their arguments and they will often report that even if no difference actually
 * exists. This allows us to easily avoid calling the diffing function if there is no difference. *)
let diff_if_changed f x1 x2 =
  if x1 == x2 then [] else f x1 x2

(* Outline:
* - There is a function for every AST node that we want to be able to recurse into.
* - Each function for an AST node represented in the `node` type above should return a list of
*   changes.
*   - If it cannot compute a more granular diff, it should return a list with a single element,
*     which records the replacement of `old_node` with `new_node` (where `old_node` and
*     `new_node` are the arguments passed to that function)
* - Every other function should do the same, except if it is unable to return a granular diff, it
*   should return `None` to indicate that its parent must be recorded as a replacement. This is
*   because there is no way to record a replacement for a node which does not appear in the
*   `node` type above.
* - We can add additional functions as needed to improve the granularity of the diffs.
* - We could eventually reach a point where no function would ever fail to generate a diff. That
*   would require us to implement a function here for every AST node, and add a variant to the
*   `node` type for every AST node as well. It would also likely require some tweaks to the AST.
*   For example, a function return type is optional. If it is None, it has no location attached.
*   What would we do if the original tree had no annotation, but the new tree did have one? We
*   would not know what Loc.t to give to the insertion.
*)

(* Entry point *)
let rec program (program1: Loc.t Ast.program) (program2: Loc.t Ast.program) : node change list =
  let (program_loc, statements1, _) = program1 in
  let (_, statements2, _) = program2 in
  statement_list statements1 statements2
  |> Option.value ~default:[(program_loc, Replace (Program program1, Program program2))]

and statement_list (stmts1: Loc.t Ast.Statement.t list) (stmts2: Loc.t Ast.Statement.t list)
    : node change list option =
  diff_and_recurse (fun x y -> Some (statement x y)) stmts1 stmts2

and statement (stmt1: Loc.t Ast.Statement.t) (stmt2: Loc.t Ast.Statement.t)
    : node change list =
  let open Ast.Statement in
  let changes = match stmt1, stmt2 with
  | (_, FunctionDeclaration func1), (_, FunctionDeclaration func2) ->
    function_declaration func1 func2
  | (_, ClassDeclaration class1), (_, ClassDeclaration class2) ->
    class_ class1 class2
  | (_, Ast.Statement.If if1), (_, Ast.Statement.If if2) ->
    if_statement if1 if2
  | (_, Ast.Statement.Expression expr1), (_, Ast.Statement.Expression expr2) ->
    expression_statement expr1 expr2
  | (_, Ast.Statement.Block block1), (_, Ast.Statement.Block block2) ->
    block block1 block2
  | _, _ ->
    None
  in
  let old_loc = Ast_utils.loc_of_statement stmt1 in
  Option.value changes ~default:[(old_loc, Replace (Statement stmt1, Statement stmt2))]

and function_declaration func1 func2 = function_ func1 func2

and function_ (func1: Loc.t Ast.Function.t) (func2: Loc.t Ast.Function.t)
    : node change list option =
  let open Ast.Function in
  let {
    id = id1; params = params1; body = body1; async = async1; generator = generator1;
    expression = expression1; predicate = predicate1; return = return1; tparams = tparams1;
  } = func1 in
  let {
    id = id2; params = params2; body = body2; async = async2; generator = generator2;
    expression = expression2; predicate = predicate2; return = return2; tparams = tparams2;
  } = func2 in

  if id1 != id2 || params1 != params2 || (* body handled below *) async1 != async2
      || generator1 != generator2 || expression1 != expression2 || predicate1 != predicate2
      || return1 != return2 || tparams1 != tparams2
  then
    None
  else
    (* just body changed *)
    match body1, body2 with
    | BodyExpression _, _
    | _, BodyExpression _ ->
      None
    | BodyBlock (_, block1), BodyBlock (_, block2) ->
      block block1 block2

and if_statement (if1: Loc.t Ast.Statement.If.t) (if2: Loc.t Ast.Statement.If.t)
    : node change list option =
  let open Ast.Statement.If in
  let {
    test = test1;
    consequent = consequent1;
    alternate = alternate1
  } = if1 in
  let {
    test = test2;
    consequent = consequent2;
    alternate = alternate2
  } = if2 in

  let expr_diff = Some (diff_if_changed expression test1 test2) in
  let cons_diff = Some (diff_if_changed statement consequent1 consequent2) in
  let alt_diff = match alternate1, alternate2 with
    | None, None -> Some ([])
    | Some _, None
    | None, Some _ -> None
    | Some a1, Some a2 -> Some (diff_if_changed statement a1 a2) in
  let result_list = [expr_diff; cons_diff; alt_diff] in

  Option.all result_list |> Option.map ~f:List.concat

and class_ (class1: Loc.t Ast.Class.t) (class2: Loc.t Ast.Class.t) =
  let open Ast.Class in
  let {
    id=id1; body=body1; tparams=tparams1; super=super1; super_targs=super_targs1;
    implements=implements1; classDecorators=classDecorators1;
  } = class1 in
  let {
    id=id2; body=body2; tparams=tparams2; super=super2; super_targs=super_targs2;
    implements=implements2; classDecorators=classDecorators2;
  } = class2 in
  if id1 != id2 || (* body handled below *) tparams1 != tparams2 || super1 != super2 ||
      super_targs1 != super_targs2 || implements1 != implements2 ||
      classDecorators1 != classDecorators2
  then
    None
  else
    (* just body changed *)
    class_body body1 body2

and class_body (class_body1: Loc.t Ast.Class.Body.t) (class_body2: Loc.t Ast.Class.Body.t)
    : node change list option =
  let open Ast.Class.Body in
  let _, { body=body1 } = class_body1 in
  let _, { body=body2 } = class_body2 in
  diff_and_recurse class_element body1 body2

and class_element (elem1: Loc.t Ast.Class.Body.element) (elem2: Loc.t Ast.Class.Body.element)
    : node change list option =
  let open Ast.Class.Body in
  match elem1, elem2 with
  | Method (_, m1), Method (_, m2) ->
    class_method m1 m2
  | _ -> None (* TODO *)

and class_method
    (m1: Loc.t Ast.Class.Method.t')
    (m2: Loc.t Ast.Class.Method.t')
    : node change list option =
  let open Ast.Class.Method in
  let { kind = kind1; key = key1; value = (_loc, value1); static = static1; decorators = decorators1 } =
    m1
  in
  let { kind = kind2; key = key2; value = (_loc, value2); static = static2; decorators = decorators2 } =
    m2
  in
  if kind1 != kind2 || key1 != key2 || (* value handled below *) static1 != static2 ||
      decorators1 != decorators2
  then
    None
  else
    function_ value1 value2

and block (block1: Loc.t Ast.Statement.Block.t) (block2: Loc.t Ast.Statement.Block.t)
    : node change list option =
  let open Ast.Statement.Block in
  let { body = body1 } = block1 in
  let { body = body2 } = block2 in
  statement_list body1 body2

and expression_statement
    (stmt1: Loc.t Ast.Statement.Expression.t)
    (stmt2: Loc.t Ast.Statement.Expression.t)
    : node change list option =
  let open Ast.Statement.Expression in
  let { expression = expr1; directive = dir1 } = stmt1 in
  let { expression = expr2; directive = dir2 } = stmt2 in
  if dir1 != dir2 then
    None
  else
    Some (expression expr1 expr2)

and expression (expr1: Loc.t Ast.Expression.t) (expr2: Loc.t Ast.Expression.t)
    : node change list =
  let changes =
    (* The open is here to avoid ambiguity with the use of the local `Expression` constructor
     * below *)
    let open Ast.Expression in
    match expr1, expr2 with
    | (_, Binary b1), (_, Binary b2) ->
      binary b1 b2
    | (_, Ast.Expression.Identifier id1), (_, Ast.Expression.Identifier id2) ->
      Some (identifier id1 id2)
    | (_, New new1), (_, New new2) ->
      new_ new1 new2
    | (_, Function f1), (_, Function f2) ->
      function_ f1 f2
    | _, _ ->
      None
  in
  let old_loc = Ast_utils.loc_of_expression expr1 in
  Option.value changes ~default:[(old_loc, Replace (Expression expr1, Expression expr2))]

and binary (b1: Loc.t Ast.Expression.Binary.t) (b2: Loc.t Ast.Expression.Binary.t): node change list option =
  let open Ast.Expression.Binary in
  let { operator = op1; left = left1; right = right1 } = b1 in
  let { operator = op2; left = left2; right = right2 } = b2 in
  if op1 != op2 then
    None
  else
    Some (diff_if_changed expression left1 left2 @ diff_if_changed expression right1 right2)

and identifier (id1: Loc.t Ast.Identifier.t) (id2: Loc.t Ast.Identifier.t): node change list =
  let (old_loc, _) = id1 in
  [(old_loc, Replace (Identifier id1, Identifier id2))]

and new_ (new1: Loc.t Ast.Expression.New.t) (new2: Loc.t Ast.Expression.New.t): node change list option =
  let open Ast.Expression.New in
  let { callee = callee1; targs = targs1; arguments = arguments1 } = new1 in
  let { callee = callee2; targs = targs2; arguments = arguments2 } = new2 in
  if targs1 != targs2 || arguments1 != arguments2 then
    (* TODO(nmote) recurse into targs and arguments *)
    None
  else
    Some (diff_if_changed expression callee1 callee2)
