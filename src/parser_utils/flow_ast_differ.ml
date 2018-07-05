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
      if hd1 <> hd2 then
        Option.map rest ~f:(List.cons (i, Replace (hd1, hd2)))
      else
        rest
    | _, []
    | [], _ ->
      None
  in
  if old_list == new_list then Some []
  else helper 0 old_list new_list

(* We need a variant here for every node that we want to be able to store a diff for. The more we
 * have here, the more granularly we can diff. *)
type node =
  | Statement of Loc.t Ast.Statement.t
  | Program of Loc.t Ast.program

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
  let changes = list_diff stmts1 stmts2 in
  Option.map changes ~f:begin fun changes ->
    changes
    |> List.map begin function
    | _, Replace (stmt1, stmt2) ->
      statement stmt1 stmt2
    end
    |> List.concat
  end

and statement (stmt1: Loc.t Ast.Statement.t) (stmt2: Loc.t Ast.Statement.t)
    : node change list =
  let open Ast.Statement in
  let changes = match stmt1, stmt2 with
  | (_, FunctionDeclaration func1), (_, FunctionDeclaration func2) ->
    function_declaration func1 func2
  | _, _ ->
    None
  in
  let old_loc = Ast_utils.loc_of_statement stmt1 in
  Option.value changes ~default:[(old_loc, Replace (Statement stmt1, Statement stmt2))]

and function_declaration (func1: Loc.t Ast.Function.t) (func2: Loc.t Ast.Function.t)
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

  if id1 <> id2 || params1 <> params2 || (* body handled below *) async1 <> async2
      || generator1 <> generator2 || expression1 <> expression2 || predicate1 <> predicate2
      || return1 <> return2 || tparams1 <> tparams2
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

and block (block1: Loc.t Ast.Statement.Block.t) (block2: Loc.t Ast.Statement.Block.t)
    : node change list option =
  let open Ast.Statement.Block in
  let { body = body1 } = block1 in
  let { body = body2 } = block2 in
  statement_list body1 body2
