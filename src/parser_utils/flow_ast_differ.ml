(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast_utils = Flow_ast_utils
module Ast = Flow_ast
open Utils_js

type 'a change' =
  | Replace of 'a * 'a
  | Insert of (* separator. Defaults to \n *) string option * 'a list
  | Delete of 'a

type 'a change = Loc.t * 'a change'

type diff_algorithm =
  | Trivial
  | Standard

(* Position in the list is necessary to figure out what Loc.t to assign to insertions. *)
type 'a diff_result = int (* position *) * 'a change'

(* Compares changes based on location. *)
let change_compare (pos1, chg1) (pos2, chg2) =
  if pos1 <> pos2 then
    compare pos1 pos2
  else
    (* Orders the change types alphabetically. This puts same-indexed inserts before deletes *)
    match (chg1, chg2) with
    | (Insert _, Delete _)
    | (Delete _, Replace _)
    | (Insert _, Replace _) ->
      -1
    | (Delete _, Insert _)
    | (Replace _, Delete _)
    | (Replace _, Insert _) ->
      1
    | _ -> 0

(* diffs based on identity *)
(* return None if no good diff was found (max edit distance exceeded, etc.) *)
let trivial_list_diff (old_list : 'a list) (new_list : 'a list) : 'a diff_result list option =
  (* inspect the lists pairwise and record any items which are different as replacements. Give up if
   * the lists have different lengths.*)
  let rec helper i lst1 lst2 =
    match (lst1, lst2) with
    | ([], []) -> Some []
    | (hd1 :: tl1, hd2 :: tl2) ->
      let rest = helper (i + 1) tl1 tl2 in
      if hd1 != hd2 then
        Base.Option.map rest ~f:(List.cons (i, Replace (hd1, hd2)))
      else
        rest
    | (_, [])
    | ([], _) ->
      None
  in
  if old_list == new_list then
    Some []
  else
    helper 0 old_list new_list

(* diffs based on http://www.xmailserver.org/diff2.pdf on page 6 *)
let standard_list_diff (old_list : 'a list) (new_list : 'a list) : 'a diff_result list option =
  (* Lots of acccesses in this algorithm so arrays are faster *)
  let (old_arr, new_arr) = (Array.of_list old_list, Array.of_list new_list) in
  let (n, m) = (Array.length old_arr, Array.length new_arr) in
  (* The shortest edit sequence problem is equivalent to finding the longest
     common subsequence, or equivalently the longest trace *)
  let longest_trace max_distance : (int * int) list option =
    (* adds the match points in this snake to the trace and produces the endpoint along with the
       new trace *)
    let rec follow_snake x y trace =
      if x >= n || y >= m then
        (x, y, trace)
      else if old_arr.(x) == new_arr.(y) then
        follow_snake (x + 1) (y + 1) ((x, y) :: trace)
      else
        (x, y, trace)
    in
    let rec build_trace dist frontier visited =
      if Hashtbl.mem visited (n, m) then
        ()
      else
        let new_frontier = Queue.create () in
        if dist > max_distance then
          ()
        else
          let follow_trace (x, y) : unit =
            let trace = Hashtbl.find visited (x, y) in
            let (x_old, y_old, advance_in_old_list) = follow_snake (x + 1) y trace in
            let (x_new, y_new, advance_in_new_list) = follow_snake x (y + 1) trace in
            (* if we have already visited this location, there is a shorter path to it, so we don't
           store this trace *)
            let () =
              if Hashtbl.mem visited (x_old, y_old) |> not then
                let () = Queue.add (x_old, y_old) new_frontier in
                Hashtbl.add visited (x_old, y_old) advance_in_old_list
            in
            if Hashtbl.mem visited (x_new, y_new) |> not then
              let () = Queue.add (x_new, y_new) new_frontier in
              Hashtbl.add visited (x_new, y_new) advance_in_new_list
          in
          Queue.iter follow_trace frontier;
          build_trace (dist + 1) new_frontier visited
    in
    (* Keep track of all visited string locations so we don't duplicate work *)
    let visited = Hashtbl.create (n * m) in
    let frontier = Queue.create () in
    (* Start with the basic trace, but follow a starting snake to a non-match point *)
    let (x, y, trace) = follow_snake 0 0 [] in
    Queue.add (x, y) frontier;
    Hashtbl.add visited (x, y) trace;
    build_trace 0 frontier visited;
    Hashtbl.find_opt visited (n, m)
  in
  (* Produces an edit script from a trace via the procedure described on page 4
     of the paper. Assumes the trace is ordered by the x coordinate *)
  let build_script_from_trace (trace : (int * int) list) : 'a diff_result list =
    (* adds inserts at position x_k for values in new_list from
       y_k + 1 to y_(k + 1) - 1 for k such that y_k + 1 < y_(k + 1) *)
    let rec add_inserts k script =
      let trace_len = List.length trace in
      let trace_array = Array.of_list trace in
      let gen_inserts first last =
        let len = last - first in
        Base.List.sub new_list ~pos:first ~len
      in
      if k > trace_len - 1 then
        script
      else
        (* The algorithm treats the trace as though (-1,-1) were the (-1)th match point
         in the list and (n,m) were the (len+1)th *)
        let first =
          if k = -1 then
            0
          else
            (trace_array.(k) |> snd) + 1
        in
        let last =
          if k = trace_len - 1 then
            m
          else
            trace_array.(k + 1) |> snd
        in
        if first < last then
          let start =
            if k = -1 then
              -1
            else
              trace_array.(k) |> fst
          in
          (start, Insert (None, gen_inserts first last)) :: script |> add_inserts (k + 1)
        else
          add_inserts (k + 1) script
    in
    (* Convert like-indexed deletes and inserts into a replacement. This relies
       on the fact that sorting the script with our change_compare function will order all
       Insert nodes before Deletes *)
    let rec convert_to_replace script =
      match script with
      | []
      | [_] ->
        script
      | (i1, Insert (_, [x])) :: (i2, Delete y) :: t when i1 = i2 - 1 ->
        (i2, Replace (y, x)) :: convert_to_replace t
      | (i1, Insert (break, x :: rst)) :: (i2, Delete y) :: t when i1 = i2 - 1 ->
        (* We are only removing the first element of the insertion *)
        (i2, Replace (y, x)) :: convert_to_replace ((i2, Insert (break, rst)) :: t)
      | h :: t -> h :: convert_to_replace t
    in
    (* Deletes are added for every element of old_list that does not have a
       match point with new_list *)
    let deletes =
      Base.List.map ~f:fst trace
      |> ISet.of_list
      |> ISet.diff (ListUtils.range 0 n |> ISet.of_list)
      |> ISet.elements
      |> Base.List.map ~f:(fun pos -> (pos, Delete old_arr.(pos)))
    in
    deletes |> add_inserts (-1) |> List.sort change_compare |> convert_to_replace
  in
  Base.Option.(
    longest_trace (n + m)
    >>| List.rev (* trace is built backwards for efficiency *)
    >>| build_script_from_trace)

let list_diff = function
  | Trivial -> trivial_list_diff
  | Standard -> standard_list_diff

(* We need a variant here for every node that we want to be able to store a diff for. The more we
 * have here, the more granularly we can diff. *)
type node =
  | Raw of string
  | Comment of Loc.t Flow_ast.Comment.t
  | NumberLiteralNode of Ast.NumberLiteral.t
  | Literal of Loc.t Ast.Literal.t
  | StringLiteral of Ast.StringLiteral.t
  | Statement of (Loc.t, Loc.t) Ast.Statement.t
  | Program of (Loc.t, Loc.t) Ast.program
  | Expression of (Loc.t, Loc.t) Ast.Expression.t
  | Pattern of (Loc.t, Loc.t) Ast.Pattern.t
  | Params of (Loc.t, Loc.t) Ast.Function.Params.t
  | Variance of Loc.t Ast.Variance.t
  | Type of (Loc.t, Loc.t) Flow_ast.Type.t
  | TypeParam of (Loc.t, Loc.t) Ast.Type.TypeParam.t
  | TypeAnnotation of (Loc.t, Loc.t) Flow_ast.Type.annotation
  | FunctionTypeAnnotation of (Loc.t, Loc.t) Flow_ast.Type.annotation
  | ClassProperty of (Loc.t, Loc.t) Flow_ast.Class.Property.t
  | ObjectProperty of (Loc.t, Loc.t) Flow_ast.Expression.Object.property
  | TemplateLiteral of (Loc.t, Loc.t) Ast.Expression.TemplateLiteral.t
  | JSXChild of (Loc.t, Loc.t) Ast.JSX.child
  | JSXIdentifier of (Loc.t, Loc.t) Ast.JSX.Identifier.t

(* This is needed because all of the functions assume that if they are called, there is some
 * difference between their arguments and they will often report that even if no difference actually
 * exists. This allows us to easily avoid calling the diffing function if there is no difference. *)
let diff_if_changed f x1 x2 =
  if x1 == x2 then
    []
  else
    f x1 x2

let diff_if_changed_ret_opt f x1 x2 =
  if x1 == x2 then
    Some []
  else
    f x1 x2

let diff_if_changed_opt f opt1 opt2 : node change list option =
  match (opt1, opt2) with
  | (Some x1, Some x2) ->
    if x1 == x2 then
      Some []
    else
      f x1 x2
  | (None, None) -> Some []
  | _ -> None

let diff_or_add_opt f add opt1 opt2 : node change list option =
  match (opt1, opt2) with
  | (Some x1, Some x2) ->
    if x1 == x2 then
      Some []
    else
      f x1 x2
  | (None, None) -> Some []
  | (None, Some x2) -> Some (add x2)
  | _ -> None

(* This is needed if the function f takes its arguments as options and produces an optional
   node change list (for instance, type annotation). In this case it is not sufficient just to
   give up and return None if only one of the options is present *)
let _diff_if_changed_opt_arg f opt1 opt2 : node change list option =
  match (opt1, opt2) with
  | (None, None) -> Some []
  | (Some x1, Some x2) when x1 == x2 -> Some []
  | _ -> f opt1 opt2

(* This is needed if the function for the given node returns a node change
* list instead of a node change list option (for instance, expression) *)
let diff_if_changed_nonopt_fn f opt1 opt2 : node change list option =
  match (opt1, opt2) with
  | (Some x1, Some x2) ->
    if x1 == x2 then
      Some []
    else
      Some (f x1 x2)
  | (None, None) -> Some []
  | _ -> None

(* Is an RHS expression an import expression? *)
let is_import_expr (expr : (Loc.t, Loc.t) Ast.Expression.t) =
  let open Ast.Expression.Call in
  match expr with
  | (_, Ast.Expression.Import _) -> true
  | ( _,
      Ast.Expression.Call
        { callee = (_, Ast.Expression.Identifier (_, { Ast.Identifier.name; comments = _ })); _ } )
    ->
    name = "require"
  | _ -> false

(* Guess whether a statement is an import or not *)
let is_import_or_directive_stmt (stmt : (Loc.t, Loc.t) Ast.Statement.t) =
  let open Ast.Statement.Expression in
  let open Ast.Statement.VariableDeclaration in
  let open Ast.Statement.VariableDeclaration.Declarator in
  match stmt with
  | (_, Ast.Statement.Expression { directive = Some _; _ })
  | (_, Ast.Statement.ImportDeclaration _) ->
    true
  | (_, Ast.Statement.Expression { expression = expr; _ }) -> is_import_expr expr
  | (_, Ast.Statement.VariableDeclaration { declarations = decs; _ }) ->
    List.exists
      (fun (_, { init; _ }) -> Base.Option.value_map init ~default:false ~f:is_import_expr)
      decs
  | _ -> false

let partition_imports (stmts : (Loc.t, Loc.t) Ast.Statement.t list) =
  let rec partition_import_helper rec_stmts top =
    match rec_stmts with
    | [] -> (List.rev top, [])
    | hd :: tl when is_import_or_directive_stmt hd -> partition_import_helper tl (hd :: top)
    | _ -> (List.rev top, rec_stmts)
  in
  partition_import_helper stmts []

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
let program
    (algo : diff_algorithm)
    (program1 : (Loc.t, Loc.t) Ast.program)
    (program2 : (Loc.t, Loc.t) Ast.program) : node change list =
  (* Assuming a diff has already been generated, recurse into it.
     This function is passed the old_list and index_offset parameters
     in order to correctly insert new statements WITHOUT assuming that
     the entire statement list is being processed with a single call
     to this function. When an Insert diff is detected, we need to find
     a Loc.t that represents where in the original program they will be inserted.
     To do so, we find the statement in the old statement list that they will
     be inserted after, and get its end_loc. The index_offset parameter represents how
     many statements in the old statement list are NOT represented in this diff--
     for example, if we separated the statement lists into a list of initial imports
     and a list of body statements and generated diffs for them separately
     (cf. toplevel_statement_list), when recursing into the body diffs, the
     length of the imports in the old statement list should be passed in to
     index_offset so that insertions into the body section are given the right index.
  *)
  let recurse_into_diff
      (type a b)
      (f : a -> a -> b change list option)
      (trivial : a -> (Loc.t * b) option)
      (old_list : a list)
      (index_offset : int)
      (diffs : a diff_result list) : b change list option =
    Base.Option.(
      let recurse_into_change = function
        | (_, Replace (x1, x2)) -> f x1 x2
        | (index, Insert (break, lst)) ->
          let index = index + index_offset in
          let loc =
            if List.length old_list = 0 then
              None
            else if
              (* To insert at the start of the list, insert before the first element *)
              index = -1
            then
              List.hd old_list |> trivial >>| fst >>| Loc.start_loc
            (* Otherwise insert it after the current element *)
            else
              List.nth old_list index |> trivial >>| fst >>| Loc.end_loc
          in
          Base.List.map ~f:trivial lst
          |> all
          >>| Base.List.map ~f:snd (* drop the loc *)
          >>| (fun x -> Insert (break, x))
          |> both loc
          >>| Base.List.return
        | (_, Delete x) -> trivial x >>| (fun (loc, y) -> (loc, Delete y)) >>| Base.List.return
      in
      let recurse_into_changes =
        Base.List.map ~f:recurse_into_change %> all %> map ~f:Base.List.concat
      in
      recurse_into_changes diffs)
  in
  (* Runs `list_diff` and then recurses into replacements (using `f`) to get more granular diffs.
     For inserts and deletes, it uses `trivial` to produce a Loc.t and a b for the change *)
  let diff_and_recurse
      (type a b)
      (f : a -> a -> b change list option)
      (trivial : a -> (Loc.t * b) option)
      (old_list : a list)
      (new_list : a list) : b change list option =
    Base.Option.(list_diff algo old_list new_list >>= recurse_into_diff f trivial old_list 0)
  in
  (* Same as diff_and_recurse but takes in a function `f` that doesn't return an option *)
  let diff_and_recurse_nonopt (type a b) (f : a -> a -> b change list) =
    diff_and_recurse (fun x y -> f x y |> Base.Option.return)
  in
  (* diff_and_recurse for when there is no way to get a trivial transformation from a to b*)
  let diff_and_recurse_no_trivial f = diff_and_recurse f (fun _ -> None) in
  let diff_and_recurse_nonopt_no_trivial f = diff_and_recurse_nonopt f (fun _ -> None) in
  let join_diff_list = Some [] |> List.fold_left (Base.Option.map2 ~f:List.append) in
  let rec syntax_opt
      (loc : Loc.t)
      (s1 : (Loc.t, unit) Ast.Syntax.t option)
      (s2 : (Loc.t, unit) Ast.Syntax.t option) =
    let add_comments { Ast.Syntax.leading; trailing; internal = _ } =
      Loc.(
        let fold_comment acc cmt = Comment cmt :: acc in
        let leading = List.fold_left fold_comment [] leading in
        let leading_inserts =
          match leading with
          | [] -> []
          | leading -> [({ loc with _end = loc.start }, Insert (None, List.rev leading))]
        in
        let trailing = List.fold_left fold_comment [] trailing in
        let trailing_inserts =
          match trailing with
          | [] -> []
          | trailing -> [({ loc with start = loc._end }, Insert (None, List.rev trailing))]
        in
        leading_inserts @ trailing_inserts)
    in
    diff_or_add_opt syntax add_comments s1 s2
  and syntax (s1 : (Loc.t, unit) Ast.Syntax.t) (s2 : (Loc.t, unit) Ast.Syntax.t) =
    let { Ast.Syntax.leading = leading1; trailing = trailing1; internal = _ } = s1 in
    let { Ast.Syntax.leading = leading2; trailing = trailing2; internal = _ } = s2 in
    let add_comment ((loc, _) as cmt) = Some (loc, Comment cmt) in
    let leading = diff_and_recurse comment add_comment leading1 leading2 in
    let trailing = diff_and_recurse comment add_comment trailing1 trailing2 in
    match (leading, trailing) with
    | (Some l, Some t) -> Some (l @ t)
    | (Some l, None) -> Some l
    | (None, Some t) -> Some t
    | (None, None) -> None
  and comment
      ((loc1, comment1) as cmt1 : Loc.t Ast.Comment.t)
      ((_loc2, comment2) as cmt2 : Loc.t Ast.Comment.t) =
    let open Ast.Comment in
    match (comment1, comment2) with
    | (Line _, Block _) -> Some [(loc1, Replace (Comment cmt1, Comment cmt2))]
    | (Block _, Line _) -> Some [(loc1, Replace (Comment cmt1, Comment cmt2))]
    | (Line c1, Line c2)
    | (Block c1, Block c2)
      when not (String.equal c1 c2) ->
      Some [(loc1, Replace (Comment cmt1, Comment cmt2))]
    | _ -> None
  and program' (program1 : (Loc.t, Loc.t) Ast.program) (program2 : (Loc.t, Loc.t) Ast.program) :
      node change list =
    let (program_loc, statements1, _) = program1 in
    let (_, statements2, _) = program2 in
    toplevel_statement_list statements1 statements2
    |> Base.Option.value ~default:[(program_loc, Replace (Program program1, Program program2))]
  and toplevel_statement_list
      (stmts1 : (Loc.t, Loc.t) Ast.Statement.t list) (stmts2 : (Loc.t, Loc.t) Ast.Statement.t list)
      =
    Base.Option.(
      let (imports1, body1) = partition_imports stmts1 in
      let (imports2, body2) = partition_imports stmts2 in
      let imports_diff = list_diff algo imports1 imports2 in
      let body_diff = list_diff algo body1 body2 in
      let whole_program_diff = list_diff algo stmts1 stmts2 in
      let split_len =
        all [imports_diff; body_diff]
        >>| Base.List.map ~f:List.length
        >>| List.fold_left ( + ) 0
        |> value ~default:max_int
      in
      let whole_len = value_map ~default:max_int whole_program_diff ~f:List.length in
      if split_len > whole_len then
        whole_program_diff
        >>= recurse_into_diff
              (fun x y -> Some (statement x y))
              (fun s -> Some (Ast_utils.loc_of_statement s, Statement s))
              stmts1
              0
      else
        imports_diff
        >>= recurse_into_diff
              (fun x y -> Some (statement x y))
              (fun s -> Some (Ast_utils.loc_of_statement s, Statement s))
              stmts1
              0
        >>= fun import_recurse ->
        body_diff
        >>= ( List.length imports1
            |> recurse_into_diff
                 (fun x y -> Some (statement x y))
                 (fun s -> Some (Ast_utils.loc_of_statement s, Statement s))
                 stmts1 )
        >>| fun body_recurse -> import_recurse @ body_recurse)
  and statement_list
      (stmts1 : (Loc.t, Loc.t) Ast.Statement.t list) (stmts2 : (Loc.t, Loc.t) Ast.Statement.t list)
      : node change list option =
    diff_and_recurse_nonopt
      statement
      (fun s -> Some (Ast_utils.loc_of_statement s, Statement s))
      stmts1
      stmts2
  and statement (stmt1 : (Loc.t, Loc.t) Ast.Statement.t) (stmt2 : (Loc.t, Loc.t) Ast.Statement.t) :
      node change list =
    let open Ast.Statement in
    let changes =
      match (stmt1, stmt2) with
      | ((loc, VariableDeclaration var1), (_, VariableDeclaration var2)) ->
        variable_declaration loc var1 var2
      | ((_, FunctionDeclaration func1), (_, FunctionDeclaration func2)) ->
        function_declaration func1 func2
      | ((_, ClassDeclaration class1), (_, ClassDeclaration class2)) -> class_ class1 class2
      | ((_, InterfaceDeclaration intf1), (_, InterfaceDeclaration intf2)) -> interface intf1 intf2
      | ((loc, If if1), (_, If if2)) -> if_statement loc if1 if2
      | ((_, Ast.Statement.Expression expr1), (_, Ast.Statement.Expression expr2)) ->
        expression_statement expr1 expr2
      | ((loc, Block block1), (_, Block block2)) -> block loc block1 block2
      | ((_, For for1), (_, For for2)) -> for_statement for1 for2
      | ((_, ForIn for_in1), (_, ForIn for_in2)) -> for_in_statement for_in1 for_in2
      | ((loc, While while1), (_, While while2)) -> Some (while_statement loc while1 while2)
      | ((_, ForOf for_of1), (_, ForOf for_of2)) -> for_of_statement for_of1 for_of2
      | ((loc, DoWhile do_while1), (_, DoWhile do_while2)) ->
        Some (do_while_statement loc do_while1 do_while2)
      | ((loc, Switch switch1), (_, Switch switch2)) -> switch_statement loc switch1 switch2
      | ((loc, Return return1), (_, Return return2)) -> return_statement loc return1 return2
      | ((loc, Debugger dbg1), (_, Debugger dbg2)) -> debugger_statement loc dbg1 dbg2
      | ((loc, Continue cont1), (_, Continue cont2)) -> continue_statement loc cont1 cont2
      | ((loc, Labeled labeled1), (_, Labeled labeled2)) ->
        Some (labeled_statement loc labeled1 labeled2)
      | ((loc, With with1), (_, With with2)) -> Some (with_statement loc with1 with2)
      | ((_, ExportDefaultDeclaration export1), (_, ExportDefaultDeclaration export2)) ->
        export_default_declaration export1 export2
      | ((_, DeclareExportDeclaration export1), (_, DeclareExportDeclaration export2)) ->
        declare_export export1 export2
      | ((_, ImportDeclaration import1), (_, ImportDeclaration import2)) ->
        import_declaration import1 import2
      | ((_, ExportNamedDeclaration export1), (_, ExportNamedDeclaration export2)) ->
        export_named_declaration export1 export2
      | ((loc, Try try1), (_, Try try2)) -> try_ loc try1 try2
      | ((loc, Throw throw1), (_, Throw throw2)) -> Some (throw_statement loc throw1 throw2)
      | ((_, DeclareTypeAlias d_t_alias1), (_, DeclareTypeAlias d_t_alias2)) ->
        type_alias d_t_alias1 d_t_alias2
      | ((_, TypeAlias t_alias1), (_, TypeAlias t_alias2)) -> type_alias t_alias1 t_alias2
      | ((_, OpaqueType o_type1), (_, OpaqueType o_type2)) -> opaque_type o_type1 o_type2
      | ((_, DeclareClass declare_class_t1), (_, DeclareClass declare_class_t2)) ->
        declare_class declare_class_t1 declare_class_t2
      | (_, _) -> None
    in
    let old_loc = Ast_utils.loc_of_statement stmt1 in
    Base.Option.value changes ~default:[(old_loc, Replace (Statement stmt1, Statement stmt2))]
  and export_named_declaration export1 export2 =
    let open Ast.Statement.ExportNamedDeclaration in
    let { declaration = decl1; specifiers = specs1; source = src1; exportKind = kind1 } = export1 in
    let { declaration = decl2; specifiers = specs2; source = src2; exportKind = kind2 } = export2 in
    if src1 != src2 || kind1 != kind2 then
      None
    else
      let decls = diff_if_changed_nonopt_fn statement decl1 decl2 in
      let specs = diff_if_changed_opt export_named_declaration_specifier specs1 specs2 in
      join_diff_list [decls; specs]
  and export_default_declaration
      (export1 : (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t)
      (export2 : (Loc.t, Loc.t) Ast.Statement.ExportDefaultDeclaration.t) : node change list option
      =
    let open Ast.Statement.ExportDefaultDeclaration in
    let { declaration = declaration1; default = default1 } = export1 in
    let { declaration = declaration2; default = default2 } = export2 in
    if default1 != default2 then
      None
    else
      match (declaration1, declaration2) with
      | (Declaration s1, Declaration s2) -> statement s1 s2 |> Base.Option.return
      | ( Ast.Statement.ExportDefaultDeclaration.Expression e1,
          Ast.Statement.ExportDefaultDeclaration.Expression e2 ) ->
        expression e1 e2 |> Base.Option.return
      | _ -> None
  and export_specifier
      (spec1 : Loc.t Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t)
      (spec2 : Loc.t Ast.Statement.ExportNamedDeclaration.ExportSpecifier.t) :
      node change list option =
    let open Ast.Statement.ExportNamedDeclaration.ExportSpecifier in
    let (_, { local = local1; exported = exported1 }) = spec1 in
    let (_, { local = local2; exported = exported2 }) = spec2 in
    let locals = diff_if_changed identifier local1 local2 |> Base.Option.return in
    let exporteds = diff_if_changed_nonopt_fn identifier exported1 exported2 in
    join_diff_list [locals; exporteds]
  and export_named_declaration_specifier
      (specs1 : Loc.t Ast.Statement.ExportNamedDeclaration.specifier)
      (specs2 : Loc.t Ast.Statement.ExportNamedDeclaration.specifier) =
    let open Ast.Statement.ExportNamedDeclaration in
    match (specs1, specs2) with
    | (ExportSpecifiers es1, ExportSpecifiers es2) ->
      diff_and_recurse_no_trivial export_specifier es1 es2
    | (ExportBatchSpecifier (_, ebs1), ExportBatchSpecifier (_, ebs2)) ->
      diff_if_changed_nonopt_fn identifier ebs1 ebs2
    | _ -> None
  and declare_export
      (export1 : (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.t)
      (export2 : (Loc.t, Loc.t) Ast.Statement.DeclareExportDeclaration.t) : node change list option
      =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default = default1; declaration = decl1; specifiers = specs1; source = src1 } = export1 in
    let { default = default2; declaration = decl2; specifiers = specs2; source = src2 } = export2 in
    if default1 != default2 || src1 != src2 || decl1 != decl2 then
      None
    else
      diff_if_changed_opt export_named_declaration_specifier specs1 specs2
  and import_default_specifier
      (ident1 : (Loc.t, Loc.t) Ast.Identifier.t option)
      (ident2 : (Loc.t, Loc.t) Ast.Identifier.t option) : node change list option =
    diff_if_changed_nonopt_fn identifier ident1 ident2
  and import_namespace_specifier
      (ident1 : (Loc.t, Loc.t) Ast.Identifier.t) (ident2 : (Loc.t, Loc.t) Ast.Identifier.t) :
      node change list option =
    diff_if_changed identifier ident1 ident2 |> Base.Option.return
  and import_named_specifier
      (nm_spec1 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier)
      (nm_spec2 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.named_specifier) :
      node change list option =
    let open Ast.Statement.ImportDeclaration in
    let { kind = kind1; local = local1; remote = remote1 } = nm_spec1 in
    let { kind = kind2; local = local2; remote = remote2 } = nm_spec2 in
    if kind1 != kind2 then
      None
    else
      let locals = diff_if_changed_nonopt_fn identifier local1 local2 in
      let remotes = diff_if_changed identifier remote1 remote2 |> Base.Option.return in
      join_diff_list [locals; remotes]
  and import_specifier
      (spec1 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.specifier)
      (spec2 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.specifier) : node change list option =
    let open Ast.Statement.ImportDeclaration in
    match (spec1, spec2) with
    | (ImportNamedSpecifiers nm_specs1, ImportNamedSpecifiers nm_specs2) ->
      diff_and_recurse_no_trivial import_named_specifier nm_specs1 nm_specs2
    | (ImportNamespaceSpecifier (_, ident1), ImportNamespaceSpecifier (_, ident2)) ->
      diff_if_changed_ret_opt import_namespace_specifier ident1 ident2
    | _ -> None
  and import_declaration
      (import1 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t)
      (import2 : (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t) : node change list option =
    let open Ast.Statement.ImportDeclaration in
    let { importKind = imprt_knd1; source = src1; default = dflt1; specifiers = spec1 } = import1 in
    let { importKind = imprt_knd2; source = src2; default = dflt2; specifiers = spec2 } = import2 in
    if imprt_knd1 != imprt_knd2 || src1 != src2 then
      None
    else
      let dflt_diff = import_default_specifier dflt1 dflt2 in
      let spec_diff = diff_if_changed_opt import_specifier spec1 spec2 in
      join_diff_list [dflt_diff; spec_diff]
  and function_declaration func1 func2 = function_ func1 func2
  and function_
      ?(is_arrow = false)
      (func1 : (Loc.t, Loc.t) Ast.Function.t)
      (func2 : (Loc.t, Loc.t) Ast.Function.t) : node change list option =
    let open Ast.Function in
    let {
      id = id1;
      params = params1;
      body = body1;
      async = async1;
      generator = generator1;
      predicate = predicate1;
      return = return1;
      tparams = tparams1;
      sig_loc = _;
    } =
      func1
    in
    let {
      id = id2;
      params = params2;
      body = body2;
      async = async2;
      generator = generator2;
      predicate = predicate2;
      return = return2;
      tparams = tparams2;
      sig_loc = _;
    } =
      func2
    in
    if async1 != async2 || generator1 != generator2 || predicate1 != predicate2 then
      None
    else
      let id = diff_if_changed_nonopt_fn identifier id1 id2 in
      let tparams = diff_if_changed_opt type_params tparams1 tparams2 in
      let params = diff_if_changed_ret_opt function_params params1 params2 in
      let params =
        match (is_arrow, params1, params2, params) with
        | ( true,
            (l, { Params.params = [_p1]; rest = None }),
            (_, { Params.params = [_p2]; rest = None }),
            Some [_] ) ->
          (* reprint the parameter if it's the single parameter of a lambda to add () *)
          Some [(l, Replace (Params params1, Params params2))]
        | _ -> params
      in
      let returns = diff_if_changed type_annotation_hint return1 return2 |> Base.Option.return in
      let fnbody = diff_if_changed_ret_opt function_body_any body1 body2 in
      join_diff_list [id; tparams; params; returns; fnbody]
  and function_params
      (params1 : (Loc.t, Loc.t) Ast.Function.Params.t)
      (params2 : (Loc.t, Loc.t) Ast.Function.Params.t) : node change list option =
    let open Ast.Function.Params in
    let (_, { params = param_lst1; rest = rest1 }) = params1 in
    let (_, { params = param_lst2; rest = rest2 }) = params2 in
    let params_diff = diff_and_recurse_no_trivial function_param param_lst1 param_lst2 in
    let rest_diff = diff_if_changed_nonopt_fn function_rest_param rest1 rest2 in
    join_diff_list [params_diff; rest_diff]
  and function_param
      (param1 : (Loc.t, Loc.t) Ast.Function.Param.t) (param2 : (Loc.t, Loc.t) Ast.Function.Param.t)
      : node change list option =
    let (_, { Ast.Function.Param.argument = arg1; default = def1 }) = param1 in
    let (_, { Ast.Function.Param.argument = arg2; default = def2 }) = param2 in
    let param_diff = diff_if_changed function_param_pattern arg1 arg2 |> Base.Option.return in
    let default_diff = diff_if_changed_nonopt_fn expression def1 def2 in
    join_diff_list [param_diff; default_diff]
  and function_body_any
      (body1 : (Loc.t, Loc.t) Ast.Function.body) (body2 : (Loc.t, Loc.t) Ast.Function.body) :
      node change list option =
    let open Ast.Function in
    match (body1, body2) with
    | (BodyExpression e1, BodyExpression e2) -> expression e1 e2 |> Base.Option.return
    | (BodyBlock (loc, block1), BodyBlock (_, block2)) -> block loc block1 block2
    | _ -> None
  and variable_declarator
      (decl1 : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t)
      (decl2 : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.Declarator.t) :
      node change list option =
    let open Ast.Statement.VariableDeclaration.Declarator in
    let (_, { id = id1; init = init1 }) = decl1 in
    let (_, { id = id2; init = init2 }) = decl2 in
    let id_diff = diff_if_changed pattern id1 id2 |> Base.Option.return in
    let expr_diff = diff_if_changed_nonopt_fn expression init1 init2 in
    join_diff_list [id_diff; expr_diff]
  and variable_declaration
      (loc : Loc.t)
      (var1 : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t)
      (var2 : (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t) : node change list option =
    let open Ast.Statement.VariableDeclaration in
    let { declarations = declarations1; kind = kind1; comments = comments1 } = var1 in
    let { declarations = declarations2; kind = kind2; comments = comments2 } = var2 in
    if kind1 != kind2 then
      None
    else
      let declarations_diff =
        if declarations1 != declarations2 then
          diff_and_recurse_no_trivial variable_declarator declarations1 declarations2
        else
          Some []
      in
      let comments_diff = syntax_opt loc comments1 comments2 in
      join_diff_list [declarations_diff; comments_diff]
  and if_statement
      loc (if1 : (Loc.t, Loc.t) Ast.Statement.If.t) (if2 : (Loc.t, Loc.t) Ast.Statement.If.t) :
      node change list option =
    let open Ast.Statement.If in
    let { test = test1; consequent = consequent1; alternate = alternate1; comments = comments1 } =
      if1
    in
    let { test = test2; consequent = consequent2; alternate = alternate2; comments = comments2 } =
      if2
    in
    let expr_diff = Some (diff_if_changed expression test1 test2) in
    let cons_diff = Some (diff_if_changed statement consequent1 consequent2) in
    let alt_diff =
      match (alternate1, alternate2) with
      | (None, None) -> Some []
      | (Some _, None)
      | (None, Some _) ->
        None
      | (Some a1, Some a2) -> Some (diff_if_changed statement a1 a2)
    in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [comments; expr_diff; cons_diff; alt_diff]
  and with_statement
      (loc : Loc.t)
      (with1 : (Loc.t, Loc.t) Ast.Statement.With.t)
      (with2 : (Loc.t, Loc.t) Ast.Statement.With.t) : node change list =
    let open Ast.Statement.With in
    let { _object = _object1; body = body1; comments = comments1 } = with1 in
    let { _object = _object2; body = body2; comments = comments2 } = with2 in
    let _object_diff = diff_if_changed expression _object1 _object2 in
    let body_diff = diff_if_changed statement body1 body2 in
    let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    List.concat [_object_diff; body_diff; comments_diff]
  and try_
      loc (try1 : (Loc.t, Loc.t) Ast.Statement.Try.t) (try2 : (Loc.t, Loc.t) Ast.Statement.Try.t) =
    let open Ast.Statement.Try in
    let {
      block = (block_loc, block1);
      handler = handler1;
      finalizer = finalizer1;
      comments = comments1;
    } =
      try1
    in
    let { block = (_, block2); handler = handler2; finalizer = finalizer2; comments = comments2 } =
      try2
    in
    let comments = syntax_opt loc comments1 comments2 in
    let block_diff = diff_if_changed_ret_opt (block block_loc) block1 block2 in
    let finalizer_diff =
      match (finalizer1, finalizer2) with
      | (Some (loc, finalizer1), Some (_, finalizer2)) ->
        diff_if_changed_ret_opt (block loc) finalizer1 finalizer2
      | (None, None) -> Some []
      | _ -> None
    in
    let handler_diff = diff_if_changed_opt handler handler1 handler2 in
    join_diff_list [comments; block_diff; finalizer_diff; handler_diff]
  and handler
      (hand1 : (Loc.t, Loc.t) Ast.Statement.Try.CatchClause.t)
      (hand2 : (Loc.t, Loc.t) Ast.Statement.Try.CatchClause.t) =
    let open Ast.Statement.Try.CatchClause in
    let (old_loc, { body = (block_loc, block1); param = param1; comments = comments1 }) = hand1 in
    let (_new_loc, { body = (_, block2); param = param2; comments = comments2 }) = hand2 in
    let comments = syntax_opt old_loc comments1 comments2 in
    let body_diff = diff_if_changed_ret_opt (block block_loc) block1 block2 in
    let param_diff = diff_if_changed_nonopt_fn pattern param1 param2 in
    join_diff_list [comments; body_diff; param_diff]
  and class_ (class1 : (Loc.t, Loc.t) Ast.Class.t) (class2 : (Loc.t, Loc.t) Ast.Class.t) =
    let open Ast.Class in
    let {
      id = id1;
      body = body1;
      tparams = tparams1;
      extends = extends1;
      implements = implements1;
      classDecorators = classDecorators1;
      comments = comments1;
    } =
      class1
    in
    let {
      id = id2;
      body = body2;
      tparams = tparams2;
      extends = extends2;
      implements = implements2;
      classDecorators = classDecorators2;
      comments = comments2;
    } =
      class2
    in
    if
      id1 != id2
      (* body handled below *)
      || tparams1 != tparams2
      (* extends handled below *)
      || implements1 != implements2
      || classDecorators1 != classDecorators2
      || comments1 != comments2
    then
      None
    else
      let extends_diff = diff_if_changed_opt class_extends extends1 extends2 in
      let body_diff = diff_if_changed_ret_opt class_body body1 body2 in
      join_diff_list [extends_diff; body_diff]
  and class_extends
      ((_loc, extends1) : (Loc.t, Loc.t) Ast.Class.Extends.t)
      ((_loc, extends2) : (Loc.t, Loc.t) Ast.Class.Extends.t) =
    let open Ast.Class.Extends in
    let { expr = expr1; targs = targs1 } = extends1 in
    let { expr = expr2; targs = targs2 } = extends2 in
    let expr_diff = diff_if_changed expression expr1 expr2 |> Base.Option.return in
    let targs_diff = diff_if_changed_opt type_args targs1 targs2 in
    join_diff_list [expr_diff; targs_diff]
  and interface
      (intf1 : (Loc.t, Loc.t) Ast.Statement.Interface.t)
      (intf2 : (Loc.t, Loc.t) Ast.Statement.Interface.t) : node change list option =
    let open Ast.Statement.Interface in
    let { id = id1; tparams = tparams1; extends = extends1; body = (_loc1, body1) } = intf1 in
    let { id = id2; tparams = tparams2; extends = extends2; body = (_loc2, body2) } = intf2 in
    let id_diff = diff_if_changed identifier id1 id2 |> Base.Option.return in
    let tparams_diff = diff_if_changed_opt type_params tparams1 tparams2 in
    let extends_diff = diff_and_recurse_no_trivial generic_type_with_loc extends1 extends2 in
    let body_diff = diff_if_changed_ret_opt object_type body1 body2 in
    join_diff_list [id_diff; tparams_diff; extends_diff; body_diff]
  and class_body
      (class_body1 : (Loc.t, Loc.t) Ast.Class.Body.t)
      (class_body2 : (Loc.t, Loc.t) Ast.Class.Body.t) : node change list option =
    let open Ast.Class.Body in
    let (_, { body = body1 }) = class_body1 in
    let (_, { body = body2 }) = class_body2 in
    diff_and_recurse_no_trivial class_element body1 body2
  and class_element
      (elem1 : (Loc.t, Loc.t) Ast.Class.Body.element)
      (elem2 : (Loc.t, Loc.t) Ast.Class.Body.element) : node change list option =
    let open Ast.Class.Body in
    match (elem1, elem2) with
    | (Method (_, m1), Method (_, m2)) -> class_method m1 m2
    | (Property p1, Property p2) -> class_property p1 p2 |> Base.Option.return
    | _ -> None
  (* TODO *)
  and class_property prop1 prop2 : node change list =
    let open Ast.Class.Property in
    let (loc1, { key = key1; value = val1; annot = annot1; static = s1; variance = var1 }) =
      prop1
    in
    let (_, { key = key2; value = val2; annot = annot2; static = s2; variance = var2 }) = prop2 in
    ( if key1 != key2 || s1 != s2 || var1 != var2 then
      None
    else
      let vals = diff_if_changed_ret_opt class_property_value val1 val2 in
      let annots = Some (diff_if_changed type_annotation_hint annot1 annot2) in
      join_diff_list [vals; annots] )
    |> Base.Option.value ~default:[(loc1, Replace (ClassProperty prop1, ClassProperty prop2))]
  and class_property_value val1 val2 : node change list option =
    let open Ast.Class.Property in
    match (val1, val2) with
    | (Declared, Declared) -> Some []
    | (Uninitialized, Uninitialized) -> Some []
    | (Initialized e1, Initialized e2) -> Some (diff_if_changed expression e1 e2)
    | _ -> None
  and class_method
      (m1 : (Loc.t, Loc.t) Ast.Class.Method.t') (m2 : (Loc.t, Loc.t) Ast.Class.Method.t') :
      node change list option =
    let open Ast.Class.Method in
    let {
      kind = kind1;
      key = key1;
      value = (_loc, value1);
      static = static1;
      decorators = decorators1;
    } =
      m1
    in
    let {
      kind = kind2;
      key = key2;
      value = (_loc, value2);
      static = static2;
      decorators = decorators2;
    } =
      m2
    in
    if
      kind1 != kind2
      || key1 != key2
      (* value handled below *)
      || static1 != static2
      || decorators1 != decorators2
    then
      None
    else
      function_ value1 value2
  and block
      (loc : Loc.t)
      (block1 : (Loc.t, Loc.t) Ast.Statement.Block.t)
      (block2 : (Loc.t, Loc.t) Ast.Statement.Block.t) : node change list option =
    let open Ast.Statement.Block in
    let { body = body1; comments = comments1 } = block1 in
    let { body = body2; comments = comments2 } = block2 in
    let body_diff = statement_list body1 body2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [body_diff; comments_diff]
  and expression_statement
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.Expression.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.Expression.t) : node change list option =
    let open Ast.Statement.Expression in
    let { expression = expr1; directive = dir1 } = stmt1 in
    let { expression = expr2; directive = dir2 } = stmt2 in
    if dir1 != dir2 then
      None
    else
      Some (expression expr1 expr2)
  and expression (expr1 : (Loc.t, Loc.t) Ast.Expression.t) (expr2 : (Loc.t, Loc.t) Ast.Expression.t)
      : node change list =
    let changes =
      (* The open is here to avoid ambiguity with the use of the local `Expression` constructor
       * below *)
      let open Ast.Expression in
      match (expr1, expr2) with
      | ((loc, Ast.Expression.Literal lit1), (_, Ast.Expression.Literal lit2)) ->
        Some (literal loc lit1 lit2)
      | ((loc, Binary b1), (_, Binary b2)) -> binary loc b1 b2
      | ((loc, Unary u1), (_, Unary u2)) -> unary loc u1 u2
      | ((_, Ast.Expression.Identifier id1), (_, Ast.Expression.Identifier id2)) ->
        identifier id1 id2 |> Base.Option.return
      | ((loc, Conditional c1), (_, Conditional c2)) -> conditional loc c1 c2 |> Base.Option.return
      | ((loc, New new1), (_, New new2)) -> new_ loc new1 new2
      | ((loc, Member member1), (_, Member member2)) -> member loc member1 member2
      | ((loc, Call call1), (_, Call call2)) -> call loc call1 call2
      | ((_, ArrowFunction f1), (_, ArrowFunction f2)) -> function_ ~is_arrow:true f1 f2
      | ((_, Function f1), (_, Function f2)) -> function_ f1 f2
      | ((_, Class class1), (_, Class class2)) -> class_ class1 class2
      | ((loc, Assignment assn1), (_, Assignment assn2)) -> assignment loc assn1 assn2
      | ((loc, Object obj1), (_, Object obj2)) -> object_ loc obj1 obj2
      | ((loc, TaggedTemplate t_tmpl1), (_, TaggedTemplate t_tmpl2)) ->
        Some (tagged_template loc t_tmpl1 t_tmpl2)
      | ((loc, Ast.Expression.TemplateLiteral t_lit1), (_, Ast.Expression.TemplateLiteral t_lit2))
        ->
        Some (template_literal loc t_lit1 t_lit2)
      | ((loc, JSXElement jsx_elem1), (_, JSXElement jsx_elem2)) ->
        jsx_element loc jsx_elem1 jsx_elem2
      | ((loc, JSXFragment frag1), (_, JSXFragment frag2)) -> jsx_fragment loc frag1 frag2
      | ((loc, TypeCast t1), (_, TypeCast t2)) -> Some (type_cast loc t1 t2)
      | ((loc, Logical l1), (_, Logical l2)) -> logical loc l1 l2
      | ((loc, Array arr1), (_, Array arr2)) -> array loc arr1 arr2
      | (expr, (loc, TypeCast t2)) -> Some (type_cast_added expr loc t2)
      | ((loc, Update update1), (_, Update update2)) -> update loc update1 update2
      | ((loc, Sequence seq1), (_, Sequence seq2)) -> sequence loc seq1 seq2
      | ((loc, This t1), (_, This t2)) -> this_expression loc t1 t2
      | ((loc, Super s1), (_, Super s2)) -> super_expression loc s1 s2
      | ((loc, MetaProperty m1), (_, MetaProperty m2)) -> meta_property loc m1 m2
      | ((loc, Import i1), (_, Import i2)) -> import_expression loc i1 i2
      | (_, _) -> None
    in
    let old_loc = Ast_utils.loc_of_expression expr1 in
    Base.Option.value changes ~default:[(old_loc, Replace (Expression expr1, Expression expr2))]
  and literal (loc : Loc.t) (lit1 : Loc.t Ast.Literal.t) (lit2 : Loc.t Ast.Literal.t) :
      node change list =
    [(loc, Replace (Literal lit1, Literal lit2))]
  and number_literal_type (loc : Loc.t) (nlit1 : Ast.NumberLiteral.t) (nlit2 : Ast.NumberLiteral.t)
      : node change list =
    [(loc, Replace (NumberLiteralNode nlit1, NumberLiteralNode nlit2))]
  and string_literal (loc : Loc.t) (lit1 : Ast.StringLiteral.t) (lit2 : Ast.StringLiteral.t) :
      node change list option =
    let { Ast.StringLiteral.value = val1; raw = raw1 } = lit1 in
    let { Ast.StringLiteral.value = val2; raw = raw2 } = lit2 in
    if String.equal val1 val2 && String.equal raw1 raw2 then
      Some []
    else
      Some [(loc, Replace (StringLiteral lit1, StringLiteral lit2))]
  and tagged_template
      (loc : Loc.t)
      (t_tmpl1 : (Loc.t, Loc.t) Ast.Expression.TaggedTemplate.t)
      (t_tmpl2 : (Loc.t, Loc.t) Ast.Expression.TaggedTemplate.t) : node change list =
    let open Ast.Expression.TaggedTemplate in
    let { tag = tag1; quasi = (quasi_loc, quasi1); comments = comments1 } = t_tmpl1 in
    let { tag = tag2; quasi = (_, quasi2); comments = comments2 } = t_tmpl2 in
    let tag_diff = diff_if_changed expression tag1 tag2 in
    let quasi_diff = diff_if_changed (template_literal quasi_loc) quasi1 quasi2 in
    let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    Base.List.concat [tag_diff; quasi_diff; comments_diff]
  and template_literal
      (loc : Loc.t)
      (* Need to pass in loc because TemplateLiteral doesn't have a loc attached *)
        (t_lit1 : (Loc.t, Loc.t) Ast.Expression.TemplateLiteral.t)
      (t_lit2 : (Loc.t, Loc.t) Ast.Expression.TemplateLiteral.t) : node change list =
    let open Ast.Expression.TemplateLiteral in
    let { quasis = quasis1; expressions = exprs1; comments = comments1 } = t_lit1 in
    let { quasis = quasis2; expressions = exprs2; comments = comments2 } = t_lit2 in
    let quasis_diff = diff_and_recurse_no_trivial template_literal_element quasis1 quasis2 in
    let exprs_diff = diff_and_recurse_nonopt_no_trivial expression exprs1 exprs2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    let result = join_diff_list [quasis_diff; exprs_diff; comments_diff] in
    Base.Option.value
      result
      ~default:[(loc, Replace (TemplateLiteral t_lit1, TemplateLiteral t_lit2))]
  and template_literal_element
      (tl_elem1 : Loc.t Ast.Expression.TemplateLiteral.Element.t)
      (tl_elem2 : Loc.t Ast.Expression.TemplateLiteral.Element.t) : node change list option =
    let open Ast.Expression.TemplateLiteral.Element in
    let (_, { value = value1; tail = tail1 }) = tl_elem1 in
    let (_, { value = value2; tail = tail2 }) = tl_elem2 in
    (* These are primitives, so structural equality is fine *)
    if value1.raw <> value2.raw || value1.cooked <> value2.cooked || tail1 <> tail2 then
      None
    else
      Some []
  and jsx_element
      (loc : Loc.t)
      (jsx_elem1 : (Loc.t, Loc.t) Ast.JSX.element)
      (jsx_elem2 : (Loc.t, Loc.t) Ast.JSX.element) : node change list option =
    let open Ast.JSX in
    let {
      openingElement = open_elem1;
      closingElement = close_elem1;
      children = (_, children1);
      comments = comments1;
    } =
      jsx_elem1
    in
    let {
      openingElement = open_elem2;
      closingElement = close_elem2;
      children = (_, children2);
      comments = comments2;
    } =
      jsx_elem2
    in
    let opening_diff = diff_if_changed_ret_opt jsx_opening_element open_elem1 open_elem2 in
    let children_diff = diff_and_recurse_nonopt_no_trivial jsx_child children1 children2 in
    let closing_diff = diff_if_changed_opt jsx_closing_element close_elem1 close_elem2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [opening_diff; children_diff; closing_diff; comments_diff]
  and jsx_fragment
      (loc : Loc.t)
      (frag1 : (Loc.t, Loc.t) Ast.JSX.fragment)
      (frag2 : (Loc.t, Loc.t) Ast.JSX.fragment) : node change list option =
    let open Ast.JSX in
    (* Opening and closing elements contain no information besides loc, so we
     * ignore them for the diff *)
    let {
      frag_openingElement = _;
      frag_children = (_, children1);
      frag_closingElement = _;
      frag_comments = frag_comments1;
    } =
      frag1
    in
    let {
      frag_openingElement = _;
      frag_children = (_, children2);
      frag_closingElement = _;
      frag_comments = frag_comments2;
    } =
      frag2
    in
    let children_diff = diff_and_recurse_nonopt_no_trivial jsx_child children1 children2 in
    let frag_comments_diff = syntax_opt loc frag_comments1 frag_comments2 in
    join_diff_list [children_diff; frag_comments_diff]
  and jsx_opening_element
      (elem1 : (Loc.t, Loc.t) Ast.JSX.Opening.t) (elem2 : (Loc.t, Loc.t) Ast.JSX.Opening.t) :
      node change list option =
    let open Ast.JSX.Opening in
    let (_, { name = name1; selfClosing = self_close1; attributes = attrs1 }) = elem1 in
    let (_, { name = name2; selfClosing = self_close2; attributes = attrs2 }) = elem2 in
    if self_close1 != self_close2 then
      None
    else
      let name_diff = diff_if_changed_ret_opt jsx_name name1 name2 in
      let attrs_diff = diff_and_recurse_no_trivial jsx_opening_attribute attrs1 attrs2 in
      join_diff_list [name_diff; attrs_diff]
  and jsx_name (name1 : (Loc.t, Loc.t) Ast.JSX.name) (name2 : (Loc.t, Loc.t) Ast.JSX.name) :
      node change list option =
    let open Ast.JSX in
    match (name1, name2) with
    | (Ast.JSX.Identifier id1, Ast.JSX.Identifier id2) ->
      Some (diff_if_changed jsx_identifier id1 id2)
    | (NamespacedName namespaced_name1, NamespacedName namespaced_name2) ->
      Some (diff_if_changed jsx_namespaced_name namespaced_name1 namespaced_name2)
    | (MemberExpression member_expr1, MemberExpression member_expr2) ->
      diff_if_changed_ret_opt jsx_member_expression member_expr1 member_expr2
    | _ -> None
  and jsx_identifier
      (id1 : (Loc.t, Loc.t) Ast.JSX.Identifier.t) (id2 : (Loc.t, Loc.t) Ast.JSX.Identifier.t) :
      node change list =
    let open Ast.JSX.Identifier in
    let (old_loc, { name = name1; comments = comments1 }) = id1 in
    let (_, { name = name2; comments = comments2 }) = id2 in
    let name_diff =
      if name1 = name2 then
        []
      else
        [(old_loc, Replace (JSXIdentifier id1, JSXIdentifier id2))]
    in
    let comments_diff = syntax_opt old_loc comments1 comments2 |> Base.Option.value ~default:[] in
    name_diff @ comments_diff
  and jsx_namespaced_name
      (namespaced_name1 : (Loc.t, Loc.t) Ast.JSX.NamespacedName.t)
      (namespaced_name2 : (Loc.t, Loc.t) Ast.JSX.NamespacedName.t) : node change list =
    let open Ast.JSX.NamespacedName in
    let (_, { namespace = namespace1; name = name1 }) = namespaced_name1 in
    let (_, { namespace = namespace2; name = name2 }) = namespaced_name2 in
    let namespace_diff = diff_if_changed jsx_identifier namespace1 namespace2 in
    let name_diff = diff_if_changed jsx_identifier name1 name2 in
    namespace_diff @ name_diff
  and jsx_member_expression
      (member_expr1 : (Loc.t, Loc.t) Ast.JSX.MemberExpression.t)
      (member_expr2 : (Loc.t, Loc.t) Ast.JSX.MemberExpression.t) : node change list option =
    let open Ast.JSX.MemberExpression in
    let (_, { _object = object1; property = prop1 }) = member_expr1 in
    let (_, { _object = object2; property = prop2 }) = member_expr2 in
    let obj_diff =
      match (object1, object2) with
      | (Ast.JSX.MemberExpression.Identifier id1, Ast.JSX.MemberExpression.Identifier id2) ->
        Some (diff_if_changed jsx_identifier id1 id2)
      | (MemberExpression member_expr1', MemberExpression member_expr2') ->
        diff_if_changed_ret_opt jsx_member_expression member_expr1' member_expr2'
      | _ -> None
    in
    let prop_diff = diff_if_changed jsx_identifier prop1 prop2 |> Base.Option.return in
    join_diff_list [obj_diff; prop_diff]
  and jsx_closing_element
      (elem1 : (Loc.t, Loc.t) Ast.JSX.Closing.t) (elem2 : (Loc.t, Loc.t) Ast.JSX.Closing.t) :
      node change list option =
    let open Ast.JSX.Closing in
    let (_, { name = name1 }) = elem1 in
    let (_, { name = name2 }) = elem2 in
    diff_if_changed_ret_opt jsx_name name1 name2
  and jsx_opening_attribute
      (jsx_attr1 : (Loc.t, Loc.t) Ast.JSX.Opening.attribute)
      (jsx_attr2 : (Loc.t, Loc.t) Ast.JSX.Opening.attribute) : node change list option =
    let open Ast.JSX.Opening in
    match (jsx_attr1, jsx_attr2) with
    | (Attribute attr1, Attribute attr2) -> diff_if_changed_ret_opt jsx_attribute attr1 attr2
    | (SpreadAttribute attr1, SpreadAttribute attr2) ->
      diff_if_changed jsx_spread_attribute attr1 attr2 |> Base.Option.return
    | _ -> None
  and jsx_spread_attribute
      (attr1 : (Loc.t, Loc.t) Ast.JSX.SpreadAttribute.t)
      (attr2 : (Loc.t, Loc.t) Ast.JSX.SpreadAttribute.t) : node change list =
    let open Flow_ast.JSX.SpreadAttribute in
    let (_, { argument = arg1 }) = attr1 in
    let (_, { argument = arg2 }) = attr2 in
    diff_if_changed expression arg1 arg2
  and jsx_attribute
      (attr1 : (Loc.t, Loc.t) Ast.JSX.Attribute.t) (attr2 : (Loc.t, Loc.t) Ast.JSX.Attribute.t) :
      node change list option =
    let open Ast.JSX.Attribute in
    let (_, { name = name1; value = value1 }) = attr1 in
    let (_, { name = name2; value = value2 }) = attr2 in
    let name_diff =
      match (name1, name2) with
      | (Ast.JSX.Attribute.Identifier id1, Ast.JSX.Attribute.Identifier id2) ->
        Some (diff_if_changed jsx_identifier id1 id2)
      | (NamespacedName namespaced_name1, NamespacedName namespaced_name2) ->
        Some (diff_if_changed jsx_namespaced_name namespaced_name1 namespaced_name2)
      | _ -> None
    in
    let value_diff =
      match (value1, value2) with
      | (Some (Ast.JSX.Attribute.Literal (loc, lit1)), Some (Ast.JSX.Attribute.Literal (_, lit2)))
        ->
        diff_if_changed (literal loc) lit1 lit2 |> Base.Option.return
      | (Some (ExpressionContainer (_, expr1)), Some (ExpressionContainer (_, expr2))) ->
        diff_if_changed_ret_opt jsx_expression expr1 expr2
      | _ -> None
    in
    join_diff_list [name_diff; value_diff]
  and jsx_child (child1 : (Loc.t, Loc.t) Ast.JSX.child) (child2 : (Loc.t, Loc.t) Ast.JSX.child) :
      node change list =
    let open Ast.JSX in
    let (old_loc, child1') = child1 in
    let (_, child2') = child2 in
    if child1' == child2' then
      []
    else
      let changes =
        match (child1', child2') with
        | (Element elem1, Element elem2) ->
          diff_if_changed_ret_opt (jsx_element old_loc) elem1 elem2
        | (Fragment frag1, Fragment frag2) ->
          diff_if_changed_ret_opt (jsx_fragment old_loc) frag1 frag2
        | (ExpressionContainer expr1, ExpressionContainer expr2) ->
          diff_if_changed_ret_opt jsx_expression expr1 expr2
        | (SpreadChild expr1, SpreadChild expr2) ->
          diff_if_changed expression expr1 expr2 |> Base.Option.return
        | (Text _, Text _) -> None
        | _ -> None
      in
      Base.Option.value changes ~default:[(old_loc, Replace (JSXChild child1, JSXChild child2))]
  and jsx_expression
      (jsx_expr1 : (Loc.t, Loc.t) Ast.JSX.ExpressionContainer.t)
      (jsx_expr2 : (Loc.t, Loc.t) Ast.JSX.ExpressionContainer.t) : node change list option =
    let open Ast.JSX in
    let { ExpressionContainer.expression = expr1 } = jsx_expr1 in
    let { ExpressionContainer.expression = expr2 } = jsx_expr2 in
    match (expr1, expr2) with
    | (ExpressionContainer.Expression expr1', ExpressionContainer.Expression expr2') ->
      Some (diff_if_changed expression expr1' expr2')
    | (ExpressionContainer.EmptyExpression, ExpressionContainer.EmptyExpression) -> Some []
    | _ -> None
  and assignment
      (loc : Loc.t)
      (assn1 : (Loc.t, Loc.t) Ast.Expression.Assignment.t)
      (assn2 : (Loc.t, Loc.t) Ast.Expression.Assignment.t) : node change list option =
    let open Ast.Expression.Assignment in
    let { operator = op1; left = pat1; right = exp1; comments = comments1 } = assn1 in
    let { operator = op2; left = pat2; right = exp2; comments = comments2 } = assn2 in
    if op1 != op2 then
      None
    else
      let pat_diff = diff_if_changed pattern pat1 pat2 in
      let exp_diff = diff_if_changed expression exp1 exp2 in
      let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
      Some (List.concat [pat_diff; exp_diff; comments_diff])
  and object_spread_property prop1 prop2 =
    let open Ast.Expression.Object.SpreadProperty in
    let { argument = arg1 } = prop1 in
    let { argument = arg2 } = prop2 in
    expression arg1 arg2
  and object_key key1 key2 =
    let module EOP = Ast.Expression.Object.Property in
    match (key1, key2) with
    | (EOP.Literal (loc, l1), EOP.Literal (_, l2)) ->
      diff_if_changed (literal loc) l1 l2 |> Base.Option.return
    | (EOP.Identifier i1, EOP.Identifier i2) ->
      diff_if_changed identifier i1 i2 |> Base.Option.return
    | (EOP.Computed e1, EOP.Computed e2) -> diff_if_changed expression e1 e2 |> Base.Option.return
    | (_, _) -> None
  and object_regular_property (_, prop1) (_, prop2) =
    let open Ast.Expression.Object.Property in
    match (prop1, prop2) with
    | ( Init { shorthand = sh1; value = val1; key = key1 },
        Init { shorthand = sh2; value = val2; key = key2 } ) ->
      if sh1 != sh2 then
        None
      else
        let values = diff_if_changed expression val1 val2 |> Base.Option.return in
        let keys = diff_if_changed_ret_opt object_key key1 key2 in
        join_diff_list [keys; values]
    | (Set { value = val1; key = key1 }, Set { value = val2; key = key2 })
    | (Method { value = val1; key = key1 }, Method { value = val2; key = key2 })
    | (Get { value = val1; key = key1 }, Get { value = val2; key = key2 }) ->
      let values = diff_if_changed_ret_opt function_ (snd val1) (snd val2) in
      let keys = diff_if_changed_ret_opt object_key key1 key2 in
      join_diff_list [keys; values]
    | _ -> None
  and object_property prop1 prop2 =
    let open Ast.Expression.Object in
    match (prop1, prop2) with
    | (Property (loc, p1), Property p2) ->
      object_regular_property (loc, p1) p2
      |> Base.Option.value ~default:[(loc, Replace (ObjectProperty prop1, ObjectProperty prop2))]
      |> Base.Option.return
    | (SpreadProperty (_, p1), SpreadProperty (_, p2)) ->
      object_spread_property p1 p2 |> Base.Option.return
    | _ -> None
  and object_ loc obj1 obj2 =
    let open Ast.Expression.Object in
    let { properties = properties1; comments = comments1 } = obj1 in
    let { properties = properties2; comments = comments2 } = obj2 in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [comments; diff_and_recurse_no_trivial object_property properties1 properties2]
  and binary
      (loc : Loc.t)
      (b1 : (Loc.t, Loc.t) Ast.Expression.Binary.t)
      (b2 : (Loc.t, Loc.t) Ast.Expression.Binary.t) : node change list option =
    let open Ast.Expression.Binary in
    let { operator = op1; left = left1; right = right1; comments = comments1 } = b1 in
    let { operator = op2; left = left2; right = right2; comments = comments2 } = b2 in
    if op1 != op2 then
      None
    else
      let left_diff = diff_if_changed expression left1 left2 in
      let right_diff = diff_if_changed expression right1 right2 in
      let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
      Some (List.concat [left_diff; right_diff; comments_diff])
  and unary
      loc (u1 : (Loc.t, Loc.t) Ast.Expression.Unary.t) (u2 : (Loc.t, Loc.t) Ast.Expression.Unary.t)
      : node change list option =
    let open Ast.Expression.Unary in
    let { operator = op1; argument = arg1; comments = comments1 } = u1 in
    let { operator = op2; argument = arg2; comments = comments2 } = u2 in
    let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    if op1 != op2 then
      None
    else
      Some (comments @ expression arg1 arg2)
  and identifier (id1 : (Loc.t, Loc.t) Ast.Identifier.t) (id2 : (Loc.t, Loc.t) Ast.Identifier.t) :
      node change list =
    let (old_loc, { Ast.Identifier.name = name1; comments = comments1 }) = id1 in
    let (_new_loc, { Ast.Identifier.name = name2; comments = comments2 }) = id2 in
    let name =
      if String.equal name1 name2 then
        []
      else
        [(old_loc, Replace (Raw name1, Raw name2))]
    in
    let comments = syntax_opt old_loc comments1 comments2 |> Base.Option.value ~default:[] in
    comments @ name
  and conditional
      (loc : Loc.t)
      (c1 : (Loc.t, Loc.t) Ast.Expression.Conditional.t)
      (c2 : (Loc.t, Loc.t) Ast.Expression.Conditional.t) : node change list =
    let open Ast.Expression.Conditional in
    let { test = test1; consequent = cons1; alternate = alt1; comments = comments1 } = c1 in
    let { test = test2; consequent = cons2; alternate = alt2; comments = comments2 } = c2 in
    let test_diff = diff_if_changed expression test1 test2 in
    let cons_diff = diff_if_changed expression cons1 cons2 in
    let alt_diff = diff_if_changed expression alt1 alt2 in
    let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    Base.List.concat [test_diff; cons_diff; alt_diff; comments_diff]
  and new_
      loc (new1 : (Loc.t, Loc.t) Ast.Expression.New.t) (new2 : (Loc.t, Loc.t) Ast.Expression.New.t)
      : node change list option =
    let open Ast.Expression.New in
    let { callee = callee1; targs = targs1; arguments = arguments1; comments = comments1 } = new1 in
    let { callee = callee2; targs = targs2; arguments = arguments2; comments = comments2 } = new2 in
    let comments = syntax_opt loc comments1 comments2 in
    let targs = diff_if_changed_ret_opt (diff_if_changed_opt call_type_args) targs1 targs2 in
    let args = diff_if_changed_ret_opt (diff_if_changed_opt call_args) arguments1 arguments2 in
    let callee = Some (diff_if_changed expression callee1 callee2) in
    join_diff_list [comments; targs; args; callee]
  and member
      (loc : Loc.t)
      (member1 : (Loc.t, Loc.t) Ast.Expression.Member.t)
      (member2 : (Loc.t, Loc.t) Ast.Expression.Member.t) : node change list option =
    let open Ast.Expression.Member in
    let { _object = obj1; property = prop1; comments = comments1 } = member1 in
    let { _object = obj2; property = prop2; comments = comments2 } = member2 in
    let obj = Some (diff_if_changed expression obj1 obj2) in
    let prop = diff_if_changed_ret_opt member_property prop1 prop2 in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [obj; prop; comments]
  and member_property
      (prop1 : (Loc.t, Loc.t) Ast.Expression.Member.property)
      (prop2 : (Loc.t, Loc.t) Ast.Expression.Member.property) : node change list option =
    let open Ast.Expression.Member in
    match (prop1, prop2) with
    | (PropertyExpression exp1, PropertyExpression exp2) ->
      Some (diff_if_changed expression exp1 exp2)
    | (PropertyIdentifier id1, PropertyIdentifier id2)
    | (PropertyPrivateName (_, id1), PropertyPrivateName (_, id2)) ->
      Some (diff_if_changed identifier id1 id2)
    | (_, _) -> None
  and call
      (loc : Loc.t)
      (call1 : (Loc.t, Loc.t) Ast.Expression.Call.t)
      (call2 : (Loc.t, Loc.t) Ast.Expression.Call.t) : node change list option =
    let open Ast.Expression.Call in
    let { callee = callee1; targs = targs1; arguments = arguments1; comments = comments1 } =
      call1
    in
    let { callee = callee2; targs = targs2; arguments = arguments2; comments = comments2 } =
      call2
    in
    let targs = diff_if_changed_ret_opt (diff_if_changed_opt call_type_args) targs1 targs2 in
    let args = diff_if_changed_ret_opt call_args arguments1 arguments2 in
    let callee = Some (diff_if_changed expression callee1 callee2) in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [targs; args; callee; comments]
  and call_type_arg
      (t1 : (Loc.t, Loc.t) Ast.Expression.CallTypeArg.t)
      (t2 : (Loc.t, Loc.t) Ast.Expression.CallTypeArg.t) : node change list option =
    let open Ast.Expression.CallTypeArg in
    match (t1, t2) with
    | (Explicit type1, Explicit type2) -> Some (diff_if_changed type_ type1 type2)
    | (Implicit (loc, type1), Implicit (_, type2)) ->
      let { Implicit.comments = comments1 } = type1 in
      let { Implicit.comments = comments2 } = type2 in
      syntax_opt loc comments1 comments2
    | _ -> None
  and call_type_args
      (pi1 : (Loc.t, Loc.t) Ast.Expression.CallTypeArgs.t)
      (pi2 : (Loc.t, Loc.t) Ast.Expression.CallTypeArgs.t) : node change list option =
    let (_, t_args1) = pi1 in
    let (_, t_args2) = pi2 in
    diff_and_recurse_no_trivial call_type_arg t_args1 t_args2
  and call_args
      (list1 : (Loc.t, Loc.t) Ast.Expression.ArgList.t)
      (list2 : (Loc.t, Loc.t) Ast.Expression.ArgList.t) : node change list option =
    let (_, args1) = list1 in
    let (_, args2) = list2 in
    diff_and_recurse_no_trivial expression_or_spread args1 args2
  and expression_or_spread
      (expr1 : (Loc.t, Loc.t) Ast.Expression.expression_or_spread)
      (expr2 : (Loc.t, Loc.t) Ast.Expression.expression_or_spread) : node change list option =
    match (expr1, expr2) with
    | (Ast.Expression.Expression e1, Ast.Expression.Expression e2) ->
      Some (diff_if_changed expression e1 e2)
    | (Ast.Expression.Spread spread1, Ast.Expression.Spread spread2) ->
      Some (diff_if_changed spread_element spread1 spread2)
    | (_, _) -> None
  and spread_element
      (spread1 : (Loc.t, Loc.t) Ast.Expression.SpreadElement.t)
      (spread2 : (Loc.t, Loc.t) Ast.Expression.SpreadElement.t) : node change list =
    let open Ast.Expression.SpreadElement in
    let (_, { argument = arg1 }) = spread1 in
    let (_, { argument = arg2 }) = spread2 in
    diff_if_changed expression arg1 arg2
  and logical loc expr1 expr2 =
    let open Ast.Expression.Logical in
    let { left = left1; right = right1; operator = operator1; comments = comments1 } = expr1 in
    let { left = left2; right = right2; operator = operator2; comments = comments2 } = expr2 in
    if operator1 == operator2 then
      let left = diff_if_changed expression left1 left2 in
      let right = diff_if_changed expression right1 right2 in
      let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
      Some (Base.List.concat [left; right; comments])
    else
      None
  and array loc arr1 arr2 : node change list option =
    let open Ast.Expression.Array in
    let { elements = elems1; comments = comments1 } = arr1 in
    let { elements = elems2; comments = comments2 } = arr2 in
    let comments = syntax_opt loc comments1 comments2 in
    let elements =
      diff_and_recurse_no_trivial (diff_if_changed_opt expression_or_spread) elems1 elems2
    in
    join_diff_list [comments; elements]
  and sequence loc seq1 seq2 : node change list option =
    let open Ast.Expression.Sequence in
    let { expressions = exps1; comments = comments1 } = seq1 in
    let { expressions = exps2; comments = comments2 } = seq2 in
    let expressions_diff = diff_and_recurse_nonopt_no_trivial expression exps1 exps2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [expressions_diff; comments_diff]
  and for_statement
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.For.t) (stmt2 : (Loc.t, Loc.t) Ast.Statement.For.t) :
      node change list option =
    let open Ast.Statement.For in
    let { init = init1; test = test1; update = update1; body = body1 } = stmt1 in
    let { init = init2; test = test2; update = update2; body = body2 } = stmt2 in
    let init = diff_if_changed_opt for_statement_init init1 init2 in
    let test = diff_if_changed_nonopt_fn expression test1 test2 in
    let update = diff_if_changed_nonopt_fn expression update1 update2 in
    let body = Some (diff_if_changed statement body1 body2) in
    join_diff_list [init; test; update; body]
  and for_statement_init
      (init1 : (Loc.t, Loc.t) Ast.Statement.For.init)
      (init2 : (Loc.t, Loc.t) Ast.Statement.For.init) : node change list option =
    let open Ast.Statement.For in
    match (init1, init2) with
    | (InitDeclaration (loc, decl1), InitDeclaration (_, decl2)) ->
      variable_declaration loc decl1 decl2
    | (InitExpression expr1, InitExpression expr2) -> Some (diff_if_changed expression expr1 expr2)
    | (InitDeclaration _, InitExpression _)
    | (InitExpression _, InitDeclaration _) ->
      None
  and for_in_statement
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.ForIn.t) (stmt2 : (Loc.t, Loc.t) Ast.Statement.ForIn.t)
      : node change list option =
    let open Ast.Statement.ForIn in
    let { left = left1; right = right1; body = body1; each = each1 } = stmt1 in
    let { left = left2; right = right2; body = body2; each = each2 } = stmt2 in
    let left =
      if left1 == left2 then
        Some []
      else
        for_in_statement_lhs left1 left2
    in
    let body = Some (diff_if_changed statement body1 body2) in
    let right = Some (diff_if_changed expression right1 right2) in
    let each =
      if each1 != each2 then
        None
      else
        Some []
    in
    join_diff_list [left; right; body; each]
  and for_in_statement_lhs
      (left1 : (Loc.t, Loc.t) Ast.Statement.ForIn.left)
      (left2 : (Loc.t, Loc.t) Ast.Statement.ForIn.left) : node change list option =
    let open Ast.Statement.ForIn in
    match (left1, left2) with
    | (LeftDeclaration (loc, decl1), LeftDeclaration (_, decl2)) ->
      variable_declaration loc decl1 decl2
    | (LeftPattern p1, LeftPattern p2) -> Some (pattern p1 p2)
    | (LeftDeclaration _, LeftPattern _)
    | (LeftPattern _, LeftDeclaration _) ->
      None
  and while_statement
      loc
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.While.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.While.t) : node change list =
    let open Ast.Statement.While in
    let { body = body1; test = test1; comments = comments1 } = stmt1 in
    let { body = body2; test = test2; comments = comments2 } = stmt2 in
    let test = diff_if_changed expression test1 test2 in
    let body = diff_if_changed statement body1 body2 in
    let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    test @ body @ comments
  and for_of_statement
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.ForOf.t) (stmt2 : (Loc.t, Loc.t) Ast.Statement.ForOf.t)
      : node change list option =
    let open Ast.Statement.ForOf in
    let { left = left1; right = right1; body = body1; async = async1 } = stmt1 in
    let { left = left2; right = right2; body = body2; async = async2 } = stmt2 in
    let left =
      if left1 == left2 then
        Some []
      else
        for_of_statement_lhs left1 left2
    in
    let body = Some (diff_if_changed statement body1 body2) in
    let right = Some (diff_if_changed expression right1 right2) in
    let async =
      if async1 != async2 then
        None
      else
        Some []
    in
    join_diff_list [left; right; body; async]
  and for_of_statement_lhs
      (left1 : (Loc.t, Loc.t) Ast.Statement.ForOf.left)
      (left2 : (Loc.t, Loc.t) Ast.Statement.ForOf.left) : node change list option =
    let open Ast.Statement.ForOf in
    match (left1, left2) with
    | (LeftDeclaration (loc, decl1), LeftDeclaration (_, decl2)) ->
      variable_declaration loc decl1 decl2
    | (LeftPattern p1, LeftPattern p2) -> Some (pattern p1 p2)
    | (LeftDeclaration _, LeftPattern _)
    | (LeftPattern _, LeftDeclaration _) ->
      None
  and do_while_statement
      loc
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.DoWhile.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.DoWhile.t) : node change list =
    let open Ast.Statement.DoWhile in
    let { body = body1; test = test1; comments = comments1 } = stmt1 in
    let { body = body2; test = test2; comments = comments2 } = stmt2 in
    let body = diff_if_changed statement body1 body2 in
    let test = diff_if_changed expression test1 test2 in
    let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    Base.List.concat [body; test; comments]
  and debugger_statement
      loc (stmt1 : Loc.t Ast.Statement.Debugger.t) (stmt2 : Loc.t Ast.Statement.Debugger.t) :
      node change list option =
    let open Ast.Statement.Debugger in
    let { comments = comments1 } = stmt1 in
    let { comments = comments2 } = stmt2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [comments_diff]
  and continue_statement
      loc (stmt1 : Loc.t Ast.Statement.Continue.t) (stmt2 : Loc.t Ast.Statement.Continue.t) :
      node change list option =
    let open Ast.Statement.Continue in
    let { comments = comments1; _ } = stmt1 in
    let { comments = comments2; _ } = stmt2 in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [comments_diff]
  and return_statement
      loc
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.Return.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.Return.t) : node change list option =
    let open Ast.Statement.Return in
    let { argument = argument1; comments = comments1 } = stmt1 in
    let { argument = argument2; comments = comments2 } = stmt2 in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [comments; diff_if_changed_nonopt_fn expression argument1 argument2]
  and throw_statement
      loc
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.Throw.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.Throw.t) : node change list =
    let open Ast.Statement.Throw in
    let { argument = argument1; comments = comments1 } = stmt1 in
    let { argument = argument2; comments = comments2 } = stmt2 in
    let argument = diff_if_changed expression argument1 argument2 in
    let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    argument @ comments
  and labeled_statement
      loc
      (labeled1 : (Loc.t, Loc.t) Ast.Statement.Labeled.t)
      (labeled2 : (Loc.t, Loc.t) Ast.Statement.Labeled.t) : node change list =
    let open Ast.Statement.Labeled in
    let { label = label1; body = body1; comments = comments1 } = labeled1 in
    let { label = label2; body = body2; comments = comments2 } = labeled2 in
    let label_diff = diff_if_changed identifier label1 label2 in
    let body_diff = diff_if_changed statement body1 body2 in
    let comments_diff = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    Base.List.concat [label_diff; body_diff; comments_diff]
  and switch_statement
      (loc : Loc.t)
      (stmt1 : (Loc.t, Loc.t) Ast.Statement.Switch.t)
      (stmt2 : (Loc.t, Loc.t) Ast.Statement.Switch.t) : node change list option =
    let open Ast.Statement.Switch in
    let { discriminant = discriminant1; cases = cases1; comments = comments1 } = stmt1 in
    let { discriminant = discriminant2; cases = cases2; comments = comments2 } = stmt2 in
    let discriminant = Some (diff_if_changed expression discriminant1 discriminant2) in
    let cases = diff_and_recurse_no_trivial switch_case cases1 cases2 in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [discriminant; cases; comments]
  and switch_case
      ((_, s1) : (Loc.t, Loc.t) Ast.Statement.Switch.Case.t)
      ((_, s2) : (Loc.t, Loc.t) Ast.Statement.Switch.Case.t) : node change list option =
    let open Ast.Statement.Switch.Case in
    let { test = test1; consequent = consequent1 } = s1 in
    let { test = test2; consequent = consequent2 } = s2 in
    let test = diff_if_changed_nonopt_fn expression test1 test2 in
    let consequent = statement_list consequent1 consequent2 in
    join_diff_list [test; consequent]
  and function_param_pattern
      (pat1 : (Loc.t, Loc.t) Ast.Pattern.t) (pat2 : (Loc.t, Loc.t) Ast.Pattern.t) : node change list
      =
    binding_pattern pat1 pat2
  and binding_pattern (pat1 : (Loc.t, Loc.t) Ast.Pattern.t) (pat2 : (Loc.t, Loc.t) Ast.Pattern.t) :
      node change list =
    pattern pat1 pat2
  and pattern (p1 : (Loc.t, Loc.t) Ast.Pattern.t) (p2 : (Loc.t, Loc.t) Ast.Pattern.t) :
      node change list =
    let changes =
      match (p1, p2) with
      | ((_, Ast.Pattern.Identifier i1), (_, Ast.Pattern.Identifier i2)) -> pattern_identifier i1 i2
      | ((loc, Ast.Pattern.Array a1), (_, Ast.Pattern.Array a2)) -> pattern_array loc a1 a2
      | ((_, Ast.Pattern.Object o1), (_, Ast.Pattern.Object o2)) -> pattern_object o1 o2
      | ((_, Ast.Pattern.Expression e1), (_, Ast.Pattern.Expression e2)) -> Some (expression e1 e2)
      | (_, _) -> None
    in
    let old_loc = Ast_utils.loc_of_pattern p1 in
    Base.Option.value changes ~default:[(old_loc, Replace (Pattern p1, Pattern p2))]
  and pattern_object
      (o1 : (Loc.t, Loc.t) Ast.Pattern.Object.t) (o2 : (Loc.t, Loc.t) Ast.Pattern.Object.t) :
      node change list option =
    let open Ast.Pattern.Object in
    let { properties = properties1; annot = annot1 } = o1 in
    let { properties = properties2; annot = annot2 } = o2 in
    let properties_diff =
      diff_and_recurse_no_trivial pattern_object_property properties1 properties2
    in
    let annot_diff = diff_if_changed type_annotation_hint annot1 annot2 |> Base.Option.return in
    join_diff_list [properties_diff; annot_diff]
  and pattern_object_property
      (p1 : (Loc.t, Loc.t) Ast.Pattern.Object.property)
      (p2 : (Loc.t, Loc.t) Ast.Pattern.Object.property) : node change list option =
    let open Ast.Pattern.Object in
    match (p1, p2) with
    | (Property (_, p3), Property (_, p4)) ->
      let open Ast.Pattern.Object.Property in
      let { key = key1; pattern = pattern1; default = default1; shorthand = shorthand1 } = p3 in
      let { key = key2; pattern = pattern2; default = default2; shorthand = shorthand2 } = p4 in
      let keys = diff_if_changed_ret_opt pattern_object_property_key key1 key2 in
      let pats = Some (diff_if_changed pattern pattern1 pattern2) in
      let defaults = diff_if_changed_nonopt_fn expression default1 default2 in
      (match (shorthand1, shorthand2) with
      | (false, false) -> join_diff_list [keys; pats; defaults]
      | (_, _) -> None)
    | (RestProperty (_, rp1), RestProperty (_, rp2)) ->
      let open Ast.Pattern.Object.RestProperty in
      let { argument = argument1 } = rp1 in
      let { argument = argument2 } = rp2 in
      Some (diff_if_changed pattern argument1 argument2)
    | (_, _) -> None
  and pattern_object_property_key
      (k1 : (Loc.t, Loc.t) Ast.Pattern.Object.Property.key)
      (k2 : (Loc.t, Loc.t) Ast.Pattern.Object.Property.key) : node change list option =
    let module POP = Ast.Pattern.Object.Property in
    match (k1, k2) with
    | (POP.Literal (loc, l1), POP.Literal (_, l2)) ->
      diff_if_changed (literal loc) l1 l2 |> Base.Option.return
    | (POP.Identifier i1, POP.Identifier i2) ->
      diff_if_changed identifier i1 i2 |> Base.Option.return
    | (POP.Computed e1, POP.Computed e2) -> diff_if_changed expression e1 e2 |> Base.Option.return
    | (_, _) -> None
  and pattern_array
      loc (a1 : (Loc.t, Loc.t) Ast.Pattern.Array.t) (a2 : (Loc.t, Loc.t) Ast.Pattern.Array.t) :
      node change list option =
    let open Ast.Pattern.Array in
    let { elements = elements1; annot = annot1; comments = comments1 } = a1 in
    let { elements = elements2; annot = annot2; comments = comments2 } = a2 in
    let elements_diff = diff_and_recurse_no_trivial pattern_array_e elements1 elements2 in
    let annot_diff = diff_if_changed type_annotation_hint annot1 annot2 |> Base.Option.return in
    let comments_diff = syntax_opt loc comments1 comments2 in
    join_diff_list [comments_diff; elements_diff; annot_diff]
  and pattern_array_e
      (eo1 : (Loc.t, Loc.t) Ast.Pattern.Array.element option)
      (eo2 : (Loc.t, Loc.t) Ast.Pattern.Array.element option) : node change list option =
    let open Ast.Pattern.Array in
    match (eo1, eo2) with
    | (Some (Element p1), Some (Element p2)) -> pattern_array_element p1 p2
    | (Some (RestElement re1), Some (RestElement re2)) -> Some (pattern_array_rest re1 re2)
    | (None, None) -> Some [] (* Both elements elided *)
    | (_, _) -> None
  (* one element is elided and another is not *)
  and pattern_array_element
      ((_, e1) : (Loc.t, Loc.t) Ast.Pattern.Array.Element.t)
      ((_, e2) : (Loc.t, Loc.t) Ast.Pattern.Array.Element.t) : node change list option =
    let open Ast.Pattern.Array.Element in
    let { argument = argument1; default = default1 } = e1 in
    let { argument = argument2; default = default2 } = e2 in
    let args = Some (diff_if_changed pattern argument1 argument2) in
    let defaults = diff_if_changed_nonopt_fn expression default1 default2 in
    join_diff_list [args; defaults]
  and pattern_array_rest
      ((_, r1) : (Loc.t, Loc.t) Ast.Pattern.Array.RestElement.t)
      ((_, r2) : (Loc.t, Loc.t) Ast.Pattern.Array.RestElement.t) : node change list =
    let open Ast.Pattern.Array.RestElement in
    let { argument = argument1 } = r1 in
    let { argument = argument2 } = r2 in
    pattern argument1 argument2
  and pattern_identifier
      (i1 : (Loc.t, Loc.t) Ast.Pattern.Identifier.t) (i2 : (Loc.t, Loc.t) Ast.Pattern.Identifier.t)
      : node change list option =
    let open Ast.Pattern.Identifier in
    let { name = name1; annot = annot1; optional = optional1 } = i1 in
    let { name = name2; annot = annot2; optional = optional2 } = i2 in
    if optional1 != optional2 then
      None
    else
      let ids = diff_if_changed identifier name1 name2 |> Base.Option.return in
      let annots = Some (diff_if_changed type_annotation_hint annot1 annot2) in
      join_diff_list [ids; annots]
  and function_rest_param
      (elem1 : (Loc.t, Loc.t) Ast.Function.RestParam.t)
      (elem2 : (Loc.t, Loc.t) Ast.Function.RestParam.t) : node change list =
    let open Ast.Function.RestParam in
    let (_, { argument = arg1 }) = elem1 in
    let (_, { argument = arg2 }) = elem2 in
    binding_pattern arg1 arg2
  and type_ ((loc1, type1) : (Loc.t, Loc.t) Ast.Type.t) ((_loc2, type2) : (Loc.t, Loc.t) Ast.Type.t)
      : node change list =
    let open Ast.Type in
    let type_diff =
      match (type1, type2) with
      | (NumberLiteral n1, NumberLiteral n2) ->
        Some (diff_if_changed (number_literal_type loc1) n1 n2)
      | (Function fn1, Function fn2) -> diff_if_changed_ret_opt function_type fn1 fn2
      | (Interface i1, Interface i2) -> interface_type i1 i2
      | (Generic g1, Generic g2) -> generic_type g1 g2
      | (Intersection (t0, t1, ts), Intersection (t0', t1', ts'))
      | (Union (t0, t1, ts), Union (t0', t1', ts')) ->
        diff_and_recurse_nonopt_no_trivial type_ (t0 :: t1 :: ts) (t0' :: t1' :: ts')
      | (Nullable (t1_loc, t1), Nullable (t2_loc, t2)) -> Some (type_ (t1_loc, t1) (t2_loc, t2))
      | (Object obj1, Object obj2) -> diff_if_changed_ret_opt object_type obj1 obj2
      | (Ast.Type.StringLiteral s1, Ast.Type.StringLiteral s2) -> (string_literal loc1) s1 s2
      | (Typeof (t1_loc, t1), Typeof (t2_loc, t2)) -> Some (type_ (t1_loc, t1) (t2_loc, t2))
      | (Tuple t1, Tuple t2) -> diff_if_changed_ret_opt tuple_type t1 t2
      | (Array t1, Array t2) -> Some (type_ t1 t2)
      | _ -> None
    in
    Base.Option.value type_diff ~default:[(loc1, Replace (Type (loc1, type1), Type (loc1, type2)))]
  and interface_type
      (it1 : (Loc.t, Loc.t) Ast.Type.Interface.t) (it2 : (Loc.t, Loc.t) Ast.Type.Interface.t) :
      node change list option =
    let open Ast.Type.Interface in
    let { extends = extends1; body = (_loc1, body1) } = it1 in
    let { extends = extends2; body = (_loc2, body2) } = it2 in
    let extends_diff = diff_and_recurse_no_trivial generic_type_with_loc extends1 extends2 in
    let body_diff = diff_if_changed_ret_opt object_type body1 body2 in
    join_diff_list [extends_diff; body_diff]
  and generic_type
      (gt1 : (Loc.t, Loc.t) Ast.Type.Generic.t) (gt2 : (Loc.t, Loc.t) Ast.Type.Generic.t) :
      node change list option =
    let open Ast.Type.Generic in
    let { id = id1; targs = targs1 } = gt1 in
    let { id = id2; targs = targs2 } = gt2 in
    let id_diff = diff_if_changed_ret_opt generic_identifier_type id1 id2 in
    let targs_diff = diff_if_changed_opt type_args targs1 targs2 in
    join_diff_list [id_diff; targs_diff]
  and generic_type_with_loc
      ((_loc1, gt1) : Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t)
      ((_loc2, gt2) : Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t) : node change list option =
    generic_type gt1 gt2
  and generic_identifier_type
      (git1 : (Loc.t, Loc.t) Ast.Type.Generic.Identifier.t)
      (git2 : (Loc.t, Loc.t) Ast.Type.Generic.Identifier.t) : node change list option =
    let open Ast.Type.Generic.Identifier in
    match (git1, git2) with
    | (Unqualified id1, Unqualified id2) -> diff_if_changed identifier id1 id2 |> Base.Option.return
    | ( Qualified (_loc1, { qualification = q1; id = id1 }),
        Qualified (_loc2, { qualification = q2; id = id2 }) ) ->
      let qualification_diff = diff_if_changed_ret_opt generic_identifier_type q1 q2 in
      let id_diff = diff_if_changed identifier id1 id2 |> Base.Option.return in
      join_diff_list [qualification_diff; id_diff]
    | _ -> None
  and object_type (ot1 : (Loc.t, Loc.t) Ast.Type.Object.t) (ot2 : (Loc.t, Loc.t) Ast.Type.Object.t)
      : node change list option =
    let open Ast.Type.Object in
    let { properties = props1; exact = exact1; inexact = inexact1 } = ot1 in
    let { properties = props2; exact = exact2; inexact = inexact2 } = ot2 in
    (* These are boolean literals, so structural equality is ok *)
    let exact_diff =
      if exact1 = exact2 then
        Some []
      else
        None
    in
    let inexact_diff =
      if inexact1 = inexact2 then
        Some []
      else
        None
    in
    let properties_diff = diff_and_recurse_no_trivial object_type_property props1 props2 in
    join_diff_list [exact_diff; inexact_diff; properties_diff]
  and object_type_property
      (prop1 : (Loc.t, Loc.t) Ast.Type.Object.property)
      (prop2 : (Loc.t, Loc.t) Ast.Type.Object.property) : node change list option =
    let open Ast.Type.Object in
    match (prop1, prop2) with
    | (Property p1, Property p2) -> diff_if_changed_ret_opt object_property_type p1 p2
    | (SpreadProperty _p1, SpreadProperty _p2) -> None (* TODO *)
    | (Indexer _p1, Indexer _p2) -> None (* TODO *)
    | (CallProperty _p1, CallProperty _p2) -> None (* TODO *)
    | (InternalSlot _s1, InternalSlot _s2) -> None (* TODO *)
    | _ -> None
  and object_property_type
      (optype1 : (Loc.t, Loc.t) Ast.Type.Object.Property.t)
      (optype2 : (Loc.t, Loc.t) Ast.Type.Object.Property.t) : node change list option =
    let open Ast.Type.Object.Property in
    let ( _loc1,
          {
            key = key1;
            value = value1;
            optional = opt1;
            static = static1;
            proto = proto1;
            _method = method1;
            variance = var1;
          } ) =
      optype1
    in
    let ( _loc2,
          {
            key = key2;
            value = value2;
            optional = opt2;
            static = static2;
            proto = proto2;
            _method = method2;
            variance = var2;
          } ) =
      optype2
    in
    if opt1 != opt2 || static1 != static2 || proto1 != proto2 || method1 != method2 then
      None
    else
      let variance_diff = diff_if_changed_ret_opt variance var1 var2 in
      let key_diff = diff_if_changed_ret_opt object_key key1 key2 in
      let value_diff = diff_if_changed_ret_opt object_property_value_type value1 value2 in
      join_diff_list [variance_diff; key_diff; value_diff]
  and object_property_value_type
      (opvt1 : (Loc.t, Loc.t) Ast.Type.Object.Property.value)
      (opvt2 : (Loc.t, Loc.t) Ast.Type.Object.Property.value) : node change list option =
    let open Ast.Type.Object.Property in
    match (opvt1, opvt2) with
    | (Init t1, Init t2) -> diff_if_changed type_ t1 t2 |> Base.Option.return
    | (Get (_loc1, ft1), Get (_loc2, ft2))
    | (Set (_loc1, ft1), Set (_loc2, ft2)) ->
      diff_if_changed_ret_opt function_type ft1 ft2
    | _ -> None
  and tuple_type (tp1 : (Loc.t, Loc.t) Ast.Type.t list) (tp2 : (Loc.t, Loc.t) Ast.Type.t list) :
      node change list option =
    diff_and_recurse_nonopt_no_trivial type_ tp1 tp2
  and type_args
      (pi1 : (Loc.t, Loc.t) Ast.Type.TypeArgs.t) (pi2 : (Loc.t, Loc.t) Ast.Type.TypeArgs.t) :
      node change list option =
    let (_, t_args1) = pi1 in
    let (_, t_args2) = pi2 in
    diff_and_recurse_nonopt_no_trivial type_ t_args1 t_args2
  and function_param_type
      (fpt1 : (Loc.t, Loc.t) Ast.Type.Function.Param.t)
      (fpt2 : (Loc.t, Loc.t) Ast.Type.Function.Param.t) : node change list option =
    let open Ast.Type.Function.Param in
    let (_loc1, { annot = annot1; name = name1; optional = opt1 }) = fpt1 in
    let (_loc2, { annot = annot2; name = name2; optional = opt2 }) = fpt2 in
    (* These are boolean literals, so structural equality is ok *)
    let optional_diff =
      if opt1 = opt2 then
        Some []
      else
        None
    in
    let name_diff = diff_if_changed_nonopt_fn identifier name1 name2 in
    let annot_diff = diff_if_changed type_ annot1 annot2 |> Base.Option.return in
    join_diff_list [optional_diff; name_diff; annot_diff]
  and function_rest_param_type
      (frpt1 : (Loc.t, Loc.t) Ast.Type.Function.RestParam.t)
      (frpt2 : (Loc.t, Loc.t) Ast.Type.Function.RestParam.t) : node change list option =
    let open Ast.Type.Function.RestParam in
    let (_loc1, { argument = arg1 }) = frpt1 in
    let (_loc2, { argument = arg2 }) = frpt2 in
    diff_if_changed_ret_opt function_param_type arg1 arg2
  and function_type
      (ft1 : (Loc.t, Loc.t) Ast.Type.Function.t) (ft2 : (Loc.t, Loc.t) Ast.Type.Function.t) :
      node change list option =
    let open Ast.Type.Function in
    let {
      params = (_params_loc1, { Params.params = params1; rest = rest1 });
      return = return1;
      tparams = tparams1;
    } =
      ft1
    in
    let {
      params = (_params_loc2, { Params.params = params2; rest = rest2 });
      return = return2;
      tparams = tparams2;
    } =
      ft2
    in
    let tparams_diff = diff_if_changed_opt type_params tparams1 tparams2 in
    let params_diff = diff_and_recurse_no_trivial function_param_type params1 params2 in
    let rest_diff = diff_if_changed_opt function_rest_param_type rest1 rest2 in
    let return_diff = diff_if_changed type_ return1 return2 |> Base.Option.return in
    join_diff_list [tparams_diff; params_diff; rest_diff; return_diff]
  and type_alias
      (t_alias1 : (Loc.t, Loc.t) Ast.Statement.TypeAlias.t)
      (t_alias2 : (Loc.t, Loc.t) Ast.Statement.TypeAlias.t) : node change list option =
    let open Ast.Statement.TypeAlias in
    let { id = id1; tparams = t_params1; right = right1 } = t_alias1 in
    let { id = id2; tparams = t_params2; right = right2 } = t_alias2 in
    let id_diff = diff_if_changed identifier id1 id2 |> Base.Option.return in
    let t_params_diff = diff_if_changed_opt type_params t_params1 t_params2 in
    let right_diff = diff_if_changed type_ right1 right2 |> Base.Option.return in
    join_diff_list [id_diff; t_params_diff; right_diff]
  and opaque_type
      (o_type1 : (Loc.t, Loc.t) Ast.Statement.OpaqueType.t)
      (o_type2 : (Loc.t, Loc.t) Ast.Statement.OpaqueType.t) : node change list option =
    let open Ast.Statement.OpaqueType in
    let { id = id1; tparams = t_params1; impltype = impltype1; supertype = supertype1 } = o_type1 in
    let { id = id2; tparams = t_params2; impltype = impltype2; supertype = supertype2 } = o_type2 in
    let id_diff = diff_if_changed identifier id1 id2 |> Base.Option.return in
    let t_params_diff = diff_if_changed_opt type_params t_params1 t_params2 in
    let supertype_diff = diff_if_changed_nonopt_fn type_ supertype1 supertype2 in
    let impltype_diff = diff_if_changed_nonopt_fn type_ impltype1 impltype2 in
    join_diff_list [id_diff; t_params_diff; supertype_diff; impltype_diff]
  and declare_class dclass1 dclass2 =
    let open Ast.Statement.DeclareClass in
    let {
      id = id1;
      tparams = tparams1;
      body = (_, body1);
      extends = extends1;
      mixins = mixins1;
      implements = implements1;
    } =
      dclass1
    in
    let {
      id = id2;
      tparams = tparams2;
      body = (_, body2);
      extends = extends2;
      mixins = mixins2;
      implements = implements2;
    } =
      dclass2
    in
    let id_diff = diff_if_changed identifier id1 id2 |> Base.Option.return in
    let t_params_diff = diff_if_changed_opt type_params tparams1 tparams2 in
    let body_diff = diff_if_changed_ret_opt object_type body1 body2 in
    let extends_diff = diff_if_changed_opt generic_type_with_loc extends1 extends2 in
    if mixins1 != mixins2 || implements1 != implements2 then
      None
    else
      join_diff_list [id_diff; t_params_diff; body_diff; extends_diff]
  and type_params
      (pd1 : (Loc.t, Loc.t) Ast.Type.TypeParams.t) (pd2 : (Loc.t, Loc.t) Ast.Type.TypeParams.t) :
      node change list option =
    let (_, t_params1) = pd1 in
    let (_, t_params2) = pd2 in
    diff_and_recurse_nonopt_no_trivial type_param t_params1 t_params2
  and type_param
      ((loc1, t_param1) : (Loc.t, Loc.t) Ast.Type.TypeParam.t)
      ((_, t_param2) : (Loc.t, Loc.t) Ast.Type.TypeParam.t) : node change list =
    let open Ast.Type.TypeParam in
    let { name = name1; bound = bound1; variance = variance1; default = default1 } = t_param1 in
    let { name = name2; bound = bound2; variance = variance2; default = default2 } = t_param2 in
    let variance_diff = diff_if_changed_ret_opt variance variance1 variance2 in
    let name_diff = diff_if_changed identifier name1 name2 |> Base.Option.return in
    let bound_diff = diff_if_changed type_annotation_hint bound1 bound2 |> Base.Option.return in
    let default_diff = diff_if_changed_nonopt_fn type_ default1 default2 in
    let result = join_diff_list [variance_diff; name_diff; bound_diff; default_diff] in
    Base.Option.value
      result
      ~default:[(loc1, Replace (TypeParam (loc1, t_param1), TypeParam (loc1, t_param2)))]
  and variance (var1 : Loc.t Ast.Variance.t option) (var2 : Loc.t Ast.Variance.t option) :
      node change list option =
    match (var1, var2) with
    | (Some (loc1, var1), Some (_, var2)) ->
      Some [(loc1, Replace (Variance (loc1, var1), Variance (loc1, var2)))]
    | (Some (loc1, var1), None) -> Some [(loc1, Delete (Variance (loc1, var1)))]
    | (None, None) -> Some []
    | _ -> None
  and type_annotation_hint
      (return1 : (Loc.t, Loc.t) Ast.Type.annotation_or_hint)
      (return2 : (Loc.t, Loc.t) Ast.Type.annotation_or_hint) : node change list =
    let open Ast.Type in
    let annot_change typ =
      match return2 with
      | Available (_, (_, Function _)) -> FunctionTypeAnnotation typ
      | _ -> TypeAnnotation typ
    in
    match (return1, return2) with
    | (Missing _, Missing _) -> []
    | (Available (loc1, typ), Missing _) -> [(loc1, Delete (TypeAnnotation (loc1, typ)))]
    | (Missing loc1, Available annot) -> [(loc1, Insert (None, [annot_change annot]))]
    | (Available annot1, Available annot2) -> type_annotation annot1 annot2
  and type_annotation
      ((loc1, typ1) : (Loc.t, Loc.t) Ast.Type.annotation)
      ((loc2, typ2) : (Loc.t, Loc.t) Ast.Type.annotation) : node change list =
    let open Ast.Type in
    match (typ1, typ2) with
    | (_, (_, Function _)) ->
      [(loc1, Replace (TypeAnnotation (loc1, typ1), FunctionTypeAnnotation (loc2, typ2)))]
    | (_, _) -> type_ typ1 typ2
  and type_cast
      (loc : Loc.t)
      (type_cast1 : (Loc.t, Loc.t) Flow_ast.Expression.TypeCast.t)
      (type_cast2 : (Loc.t, Loc.t) Flow_ast.Expression.TypeCast.t) : node change list =
    let open Flow_ast.Expression.TypeCast in
    let { expression = expr1; annot = annot1; comments = comments1 } = type_cast1 in
    let { expression = expr2; annot = annot2; comments = comments2 } = type_cast2 in
    let expr = diff_if_changed expression expr1 expr2 in
    let annot = diff_if_changed type_annotation annot1 annot2 in
    let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
    Base.List.concat [expr; annot; comments]
  and type_cast_added
      (expr : (Loc.t, Loc.t) Flow_ast.Expression.t)
      (loc : Loc.t)
      (type_cast : (Loc.t, Loc.t) Flow_ast.Expression.TypeCast.t) : node change list =
    let open Flow_ast.Expression.TypeCast in
    Loc.(
      let { expression = expr2; annot = annot2; comments = _ } = type_cast in
      let expr_diff_rev = diff_if_changed expression expr expr2 |> List.rev in
      let append_annot_rev =
        ({ loc with start = loc._end }, Insert (Some "", [TypeAnnotation annot2; Raw ")"]))
        :: expr_diff_rev
      in
      ({ loc with _end = loc.start }, Insert (Some "", [Raw "("])) :: List.rev append_annot_rev)
  and update
      loc
      (update1 : (Loc.t, Loc.t) Ast.Expression.Update.t)
      (update2 : (Loc.t, Loc.t) Ast.Expression.Update.t) : node change list option =
    let open Ast.Expression.Update in
    let { operator = op1; argument = arg1; prefix = p1; comments = comments1 } = update1 in
    let { operator = op2; argument = arg2; prefix = p2; comments = comments2 } = update2 in
    if op1 != op2 || p1 != p2 then
      None
    else
      let argument = expression arg1 arg2 in
      let comments = syntax_opt loc comments1 comments2 |> Base.Option.value ~default:[] in
      Some (argument @ comments)
  and this_expression
      (loc : Loc.t) (this1 : Loc.t Ast.Expression.This.t) (this2 : Loc.t Ast.Expression.This.t) :
      node change list option =
    let open Ast.Expression.This in
    let { comments = comments1 } = this1 in
    let { comments = comments2 } = this2 in
    syntax_opt loc comments1 comments2
  and super_expression
      (loc : Loc.t) (super1 : Loc.t Ast.Expression.Super.t) (super2 : Loc.t Ast.Expression.Super.t)
      : node change list option =
    let open Ast.Expression.Super in
    let { comments = comments1 } = super1 in
    let { comments = comments2 } = super2 in
    syntax_opt loc comments1 comments2
  and meta_property
      (loc : Loc.t)
      (meta1 : Loc.t Ast.Expression.MetaProperty.t)
      (meta2 : Loc.t Ast.Expression.MetaProperty.t) : node change list option =
    let open Ast.Expression.MetaProperty in
    let { meta = meta1; property = property1; comments = comments1 } = meta1 in
    let { meta = meta2; property = property2; comments = comments2 } = meta2 in
    let meta = Some (diff_if_changed identifier meta1 meta2) in
    let property = Some (diff_if_changed identifier property1 property2) in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [meta; property; comments]
  and import_expression
      (loc : Loc.t)
      (import1 : (Loc.t, Loc.t) Ast.Expression.Import.t)
      (import2 : (Loc.t, Loc.t) Ast.Expression.Import.t) : node change list option =
    let open Ast.Expression.Import in
    let { argument = argument1; comments = comments1 } = import1 in
    let { argument = argument2; comments = comments2 } = import2 in
    let argument = Some (diff_if_changed expression argument1 argument2) in
    let comments = syntax_opt loc comments1 comments2 in
    join_diff_list [argument; comments]
  in
  program' program1 program2 |> List.sort change_compare
