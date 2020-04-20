(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Layout

(* There are some cases where expressions must be wrapped in parens to eliminate
   ambiguity. We pass whether we're in one of these special cases down through
   the tree as we generate the layout. Note that these are only necessary as
   long as the ambiguity can exist, so emitting any wrapper (like a paren or
   bracket) is enough to reset the context back to Normal. *)
type expression_context = {
  left: expression_context_left;
  group: expression_context_group;
}

and expression_context_left =
  | Normal_left
  | In_expression_statement (* `(function x(){});` would become a declaration *)
  | In_tagged_template (* `(new a)``` would become `new (a``)` *)
  | In_plus_op (* `x+(+y)` would become `(x++)y` *)
  | In_minus_op

(* `x-(-y)` would become `(x--)y` *)
and expression_context_group =
  | Normal_group
  | In_arrow_func (* `() => ({a: b})` would become `() => {a: b}` *)
  | In_for_init

(* `for ((x in y);;);` would become a for-in *)

let normal_context = { left = Normal_left; group = Normal_group }

(* Some contexts only matter to the left-most token. If we output some other
   token, like an `=`, then we can reset the context. Note that all contexts
   reset when wrapped in parens, brackets, braces, etc, so we don't need to call
   this in those cases, we can just set it back to Normal. *)
let context_after_token ctxt = { ctxt with left = Normal_left }

(* JS layout helpers *)
let not_supported loc message = failwith (message ^ " at " ^ Loc.debug_to_string loc)

let with_semicolon node = fuse [node; Atom ";"]

let with_pretty_semicolon node = fuse [node; IfPretty (Atom ";", Empty)]

let wrap_in_parens item = group [Atom "("; item; Atom ")"]

let wrap_in_parens_on_break item =
  wrap_and_indent (IfBreak (Atom "(", Empty), IfBreak (Atom ")", Empty)) [item]

let option f = function
  | Some v -> f v
  | None -> Empty

let hint f = function
  | Ast.Type.Available v -> f v
  | Ast.Type.Missing _ -> Empty

let deoptionalize l =
  List.rev
    (List.fold_left
       (fun acc -> function
         | None -> acc
         | Some x -> x :: acc)
       []
       l)

(* See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence *)
let max_precedence = 22

let min_precedence = 1

(* 0 means always parenthesize, which is not a precedence decision *)

let precedence_of_assignment = 3

let precedence_of_expression expr =
  let module E = Ast.Expression in
  match expr with
  (* Expressions that don't involve operators have the highest priority *)
  | (_, E.Array _)
  | (_, E.Class _)
  | (_, E.Function _)
  | (_, E.Identifier _)
  | (_, E.JSXElement _)
  | (_, E.JSXFragment _)
  | (_, E.Literal _)
  | (_, E.Object _)
  | (_, E.Super _)
  | (_, E.TemplateLiteral _)
  | (_, E.This _) ->
    max_precedence
  (* Expressions involving operators *)
  | (_, E.Member _)
  | (_, E.OptionalMember _)
  | (_, E.MetaProperty _)
  | (_, E.Call _)
  | (_, E.OptionalCall _)
  | (_, E.New { E.New.arguments = Some _; _ }) ->
    21
  | (_, E.New { E.New.arguments = None; _ }) -> 20
  | (_, E.TaggedTemplate _)
  | (_, E.Import _) ->
    19
  | (_, E.Update { E.Update.prefix = false; _ }) -> 18
  | (_, E.Update { E.Update.prefix = true; _ })
  | (_, E.Unary _) ->
    17
  | (_, E.Binary { E.Binary.operator; _ }) ->
    begin
      match operator with
      | E.Binary.Exp -> 16
      | E.Binary.Mult -> 15
      | E.Binary.Div -> 15
      | E.Binary.Mod -> 15
      | E.Binary.Plus -> 14
      | E.Binary.Minus -> 14
      | E.Binary.LShift -> 13
      | E.Binary.RShift -> 13
      | E.Binary.RShift3 -> 13
      | E.Binary.LessThan -> 12
      | E.Binary.LessThanEqual -> 12
      | E.Binary.GreaterThan -> 12
      | E.Binary.GreaterThanEqual -> 12
      | E.Binary.In -> 12
      | E.Binary.Instanceof -> 12
      | E.Binary.Equal -> 11
      | E.Binary.NotEqual -> 11
      | E.Binary.StrictEqual -> 11
      | E.Binary.StrictNotEqual -> 11
      | E.Binary.BitAnd -> 10
      | E.Binary.Xor -> 9
      | E.Binary.BitOr -> 8
    end
  | (_, E.Logical { E.Logical.operator = E.Logical.And; _ }) -> 7
  | (_, E.Logical { E.Logical.operator = E.Logical.Or; _ }) -> 6
  | (_, E.Logical { E.Logical.operator = E.Logical.NullishCoalesce; _ }) -> 5
  | (_, E.Conditional _) -> 4
  | (_, E.Assignment _) -> precedence_of_assignment
  | (_, E.Yield _) -> 2
  (* not sure how low this _needs_ to be, but it can at least be higher than 0
     because it binds tighter than a sequence expression. it must be lower than
     a member expression, though, because `()=>{}.x` is invalid. *)
  | (_, E.ArrowFunction _) -> 1
  | (_, E.Sequence _) -> 0
  (* Expressions that always need parens (probably) *)
  | (_, E.Comprehension _)
  | (_, E.Generator _)
  | (_, E.TypeCast _) ->
    0

let definitely_needs_parens =
  let module E = Ast.Expression in
  let context_needs_parens ctxt expr =
    match ctxt with
    | { group = In_arrow_func; _ } ->
      (* an object body expression in an arrow function needs parens to not
         make it become a block with label statement. *)
      begin
        match expr with
        | (_, E.Object _) -> true
        | _ -> false
      end
    | { group = In_for_init; _ } ->
      (* an `in` binary expression in the init of a for loop needs parens to not
         make the for loop become a for-in loop. *)
      begin
        match expr with
        | (_, E.Binary { E.Binary.operator = E.Binary.In; _ }) -> true
        | _ -> false
      end
    | { left = In_expression_statement; _ } ->
      (* functions (including async functions, but not arrow functions) and
         classes must be wrapped in parens to avoid ambiguity with function and
         class declarations. objects must be also, to not be confused with
         blocks.

         https://tc39.github.io/ecma262/#prod-ExpressionStatement *)
      begin
        match expr with
        | (_, E.Class _)
        | (_, E.Function _)
        | (_, E.Object _)
        | (_, E.Assignment { E.Assignment.left = (_, Ast.Pattern.Object _); _ }) ->
          true
        | _ -> false
      end
    | { left = In_tagged_template; _ } ->
      begin
        match expr with
        | (_, E.Class _)
        | (_, E.Function _)
        | (_, E.New _)
        | (_, E.Import _)
        | (_, E.Object _) ->
          true
        | _ -> false
      end
    | { left = In_minus_op; _ } ->
      begin
        match expr with
        | (_, E.Unary { E.Unary.operator = E.Unary.Minus; _ })
        | (_, E.Update { E.Update.operator = E.Update.Decrement; prefix = true; _ }) ->
          true
        | _ -> false
      end
    | { left = In_plus_op; _ } ->
      begin
        match expr with
        | (_, E.Unary { E.Unary.operator = E.Unary.Plus; _ })
        | (_, E.Update { E.Update.operator = E.Update.Increment; prefix = true; _ }) ->
          true
        | _ -> false
      end
    | { left = Normal_left; group = Normal_group } -> false
  in
  fun ~precedence ctxt expr ->
    precedence_of_expression expr < precedence || context_needs_parens ctxt expr

(* TODO: this only needs to be shallow; we don't need to walk into function
   or class bodies, for example. *)
class contains_call_mapper result_ref =
  object
    inherit [Loc.t] Flow_ast_mapper.mapper

    method! call _loc expr =
      result_ref := true;
      expr
  end

let contains_call_expression expr =
  (* TODO: use a fold *)
  let result = ref false in
  let _ = (new contains_call_mapper result)#expression expr in
  !result

(* returns all of the comments that start before `loc`, and discards the rest *)
let comments_before_loc loc comments =
  let rec helper loc acc = function
    | ((c_loc, _) as comment) :: rest when Loc.compare c_loc loc < 0 ->
      helper loc (comment :: acc) rest
    | _ -> List.rev acc
  in
  helper loc [] comments

type statement_or_comment =
  | Statement of (Loc.t, Loc.t) Ast.Statement.t
  | Comment of Loc.t Ast.Comment.t

let better_quote =
  let rec count (double, single) str i =
    if i < 0 then
      (double, single)
    else
      let acc =
        match str.[i] with
        | '"' -> (succ double, single)
        | '\'' -> (double, succ single)
        | _ -> (double, single)
      in
      count acc str (pred i)
  in
  fun str ->
    let (double, single) = count (0, 0) str (String.length str - 1) in
    if double > single then
      "'"
    else
      "\""

let utf8_escape =
  (* a null character can be printed as \x00 or \0. but if the next character is an ASCII digit,
     then using \0 would create \01 (for example), which is a legacy octal 1. so, rather than simply
     fold over the codepoints, we have to look ahead at the next character as well. *)
  let lookahead_fold_wtf_8 :
      ?pos:int ->
      ?len:int ->
      (next:(int * Wtf8.codepoint) option -> 'a -> int -> Wtf8.codepoint -> 'a) ->
      'a ->
      string ->
      'a =
    let lookahead ~f (prev, buf) i cp =
      let next = Some (i, cp) in
      let buf =
        match prev with
        | Some (prev_i, prev_cp) -> f ~next buf prev_i prev_cp
        | None -> buf
      in
      (next, buf)
    in
    fun ?pos ?len f acc str ->
      str |> Wtf8.fold_wtf_8 ?pos ?len (lookahead ~f) (None, acc) |> fun (last, acc) ->
      match last with
      | Some (i, cp) -> f ~next:None acc i cp
      | None -> acc
  in
  let f ~quote ~next buf _i = function
    | Wtf8.Malformed -> buf
    | Wtf8.Point cp ->
      begin
        match cp with
        (* SingleEscapeCharacter: http://www.ecma-international.org/ecma-262/6.0/#table-34 *)
        | 0x0 ->
          let zero =
            match next with
            | Some (_i, Wtf8.Point n) when 0x30 <= n && n <= 0x39 -> "\\x00"
            | _ -> "\\0"
          in
          Buffer.add_string buf zero;
          buf
        | 0x8 ->
          Buffer.add_string buf "\\b";
          buf
        | 0x9 ->
          Buffer.add_string buf "\\t";
          buf
        | 0xA ->
          Buffer.add_string buf "\\n";
          buf
        | 0xB ->
          Buffer.add_string buf "\\v";
          buf
        | 0xC ->
          Buffer.add_string buf "\\f";
          buf
        | 0xD ->
          Buffer.add_string buf "\\r";
          buf
        | 0x22 when quote = "\"" ->
          Buffer.add_string buf "\\\"";
          buf
        | 0x27 when quote = "'" ->
          Buffer.add_string buf "\\'";
          buf
        | 0x5C ->
          Buffer.add_string buf "\\\\";
          buf
        (* printable ascii *)
        | n when 0x1F < n && n < 0x7F ->
          Buffer.add_char buf (Char.unsafe_chr cp);
          buf
        (* basic multilingual plane, 2 digits *)
        | n when n < 0x100 ->
          Printf.bprintf buf "\\x%02x" n;
          buf
        (* basic multilingual plane, 4 digits *)
        | n when n < 0x10000 ->
          Printf.bprintf buf "\\u%04x" n;
          buf
        (* supplemental planes *)
        | n ->
          (* ES5 does not support the \u{} syntax, so print surrogate pairs
         "\ud83d\udca9" instead of "\u{1f4A9}". if we add a flag to target
         ES6, we should change this. *)
          let n' = n - 0x10000 in
          let hi = 0xD800 lor (n' lsr 10) in
          let lo = 0xDC00 lor (n' land 0x3FF) in
          Printf.bprintf buf "\\u%4x" hi;
          Printf.bprintf buf "\\u%4x" lo;
          buf
      end
  in
  fun ~quote str ->
    str |> lookahead_fold_wtf_8 (f ~quote) (Buffer.create (String.length str)) |> Buffer.contents

let is_single_linebreak prev next = Loc.(next.start.line = prev._end.line + 1)

let is_multi_linebreak prev next = Loc.(next.start.line > prev._end.line + 1)

let layout_comment loc comment =
  let open Ast.Comment in
  SourceLocation
    ( loc,
      match comment with
      | Line text -> fuse [Atom "//"; Atom text; hardline]
      | Block text -> fuse [Atom "/*"; Atom text; Atom "*/"] )

let layout_from_leading_comment (loc, comment) next_loc =
  let open Ast.Comment in
  let is_single_linebreak = is_single_linebreak loc next_loc in
  let is_multi_linebreak = is_multi_linebreak loc next_loc in
  let layout_comment = layout_comment loc comment in
  match comment with
  | Line _ when is_multi_linebreak -> fuse [layout_comment; pretty_hardline]
  | Line _ -> layout_comment
  | Block _ when is_multi_linebreak -> fuse [layout_comment; pretty_hardline; pretty_hardline]
  | Block _ when is_single_linebreak -> fuse [layout_comment; pretty_hardline]
  | Block _ -> fuse [layout_comment; pretty_space]

let layout_from_trailing_comment (loc, comment) (prev_loc, prev_comment) =
  let open Ast.Comment in
  let is_single_linebreak = is_single_linebreak prev_loc loc in
  let is_multi_linebreak = is_multi_linebreak prev_loc loc in
  let layout_comment = layout_comment loc comment in
  match prev_comment with
  | Some (Line _) when is_multi_linebreak -> fuse [pretty_hardline; layout_comment]
  | Some (Line _) -> layout_comment
  | Some (Block _) when is_multi_linebreak ->
    fuse [pretty_hardline; pretty_hardline; layout_comment]
  | Some (Block _) when is_single_linebreak -> fuse [pretty_hardline; layout_comment]
  | Some (Block _) -> fuse [pretty_space; layout_comment]
  | None when is_multi_linebreak -> fuse [pretty_hardline; pretty_hardline; layout_comment]
  | None when is_single_linebreak -> fuse [pretty_hardline; layout_comment]
  | None -> fuse [pretty_space; layout_comment]

let layout_from_leading_comments comments node_loc =
  let rec inner = function
    | [] -> []
    | [comment] -> [layout_from_leading_comment comment node_loc]
    | comment :: ((next_loc, _) :: _ as rest) ->
      layout_from_leading_comment comment next_loc :: inner rest
  in
  inner comments

let layout_from_trailing_comments comments node_loc =
  let rec inner prev_comment = function
    | [] -> []
    | (loc, comment) :: rest ->
      layout_from_trailing_comment (loc, comment) prev_comment :: inner (loc, Some comment) rest
  in
  inner node_loc comments

let layout_node_with_comments current_loc comments layout_node =
  let { Ast.Syntax.leading; trailing; _ } = comments in
  let preceding = layout_from_leading_comments leading current_loc in
  let following = layout_from_trailing_comments trailing (current_loc, None) in
  Concat (List.concat [preceding; [layout_node]; following])

let layout_node_with_comments_opt current_loc comments layout_node =
  match comments with
  | Some c -> layout_node_with_comments current_loc c layout_node
  | None -> layout_node

let source_location_with_comments ?comments (current_loc, layout_node) =
  let layout = SourceLocation (current_loc, layout_node) in
  match comments with
  | Some comments -> layout_node_with_comments current_loc comments layout
  | None -> layout

let internal_comments = function
  | None
  | Some { Ast.Syntax.internal = []; _ } ->
    None
  | Some { Ast.Syntax.internal = (first_loc, _) :: _ as internal; _ } ->
    Some (Concat (layout_from_trailing_comments internal (first_loc, None)))

let identifier_with_comments current_loc name comments =
  let node = Identifier (current_loc, name) in
  match comments with
  | Some comments -> layout_node_with_comments current_loc comments node
  | None -> node

(* Generate JS layouts *)
let rec program ~preserve_docblock ~checksum (loc, statements, comments) =
  let nodes =
    if preserve_docblock && comments <> [] then
      let (directives, statements) = Flow_ast_utils.partition_directives statements in
      let comments =
        match statements with
        | [] -> comments
        | (loc, _) :: _ -> comments_before_loc loc comments
      in
      [combine_directives_and_comments directives comments; fuse (statement_list statements)]
    else
      [fuse (statement_list statements)]
  in
  let nodes = group [join pretty_hardline nodes] in
  let nodes = maybe_embed_checksum nodes checksum in
  let loc = { loc with Loc.start = { Loc.line = 1; column = 0 } } in
  source_location_with_comments (loc, nodes)

and program_simple (loc, statements, _) =
  let nodes = group [fuse (statement_list statements)] in
  let loc = { loc with Loc.start = { Loc.line = 1; column = 0 } } in
  source_location_with_comments (loc, nodes)

and combine_directives_and_comments directives comments : Layout.layout_node =
  let directives = Base.List.map ~f:(fun ((loc, _) as x) -> (loc, Statement x)) directives in
  let comments = Base.List.map ~f:(fun ((loc, _) as x) -> (loc, Comment x)) comments in
  let merged = List.merge (fun (a, _) (b, _) -> Loc.compare a b) directives comments in
  let nodes =
    Base.List.map
      ~f:(function
        | (loc, Statement s) -> (loc, Comment_attachment.statement_comment_bounds s, statement s)
        | (loc, Comment c) -> (loc, (None, None), comment c))
      merged
  in
  fuse (list_with_newlines nodes)

and maybe_embed_checksum nodes checksum =
  match checksum with
  | Some checksum ->
    let comment = Printf.sprintf "/* %s */" checksum in
    fuse [nodes; Newline; Atom comment]
  | None -> nodes

and comment (loc, comment) =
  let module C = Ast.Comment in
  source_location_with_comments
    ( loc,
      match comment with
      | C.Block txt -> fuse [Atom "/*"; Atom txt; Atom "*/"]
      | C.Line txt -> fuse [Atom "//"; Atom txt; Newline] )

(**
 * Renders a statement
 *
 * Set `pretty_semicolon` when a semicolon is only required in pretty mode. For example,
 * a semicolon is never required on the last statement of a statement list, so we can set
 * `~pretty_semicolon:true` to only print the unnecessary semicolon in pretty mode.
 *)
and statement ?(pretty_semicolon = false) (root_stmt : (Loc.t, Loc.t) Ast.Statement.t) =
  let (loc, stmt) = root_stmt in
  let module E = Ast.Expression in
  let module S = Ast.Statement in
  let with_semicolon =
    if pretty_semicolon then
      with_pretty_semicolon
    else
      with_semicolon
  in
  source_location_with_comments
    ( loc,
      match stmt with
      | S.Empty { S.Empty.comments } -> layout_node_with_comments_opt loc comments (Atom ";")
      | S.Debugger { S.Debugger.comments } ->
        layout_node_with_comments_opt loc comments @@ with_semicolon (Atom "debugger")
      | S.Block b -> block (loc, b)
      | S.Expression { S.Expression.expression = expr; directive = _; comments } ->
        let ctxt = { normal_context with left = In_expression_statement } in
        layout_node_with_comments_opt loc comments
        @@ with_semicolon (expression_with_parens ~precedence:0 ~ctxt expr)
      | S.If { S.If.test; consequent; alternate; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          begin
            match alternate with
            | Some { S.If.Alternate.body; comments } ->
              fuse
                [
                  group [statement_with_test "if" (expression test); statement_after_test consequent];
                  pretty_space;
                  layout_node_with_comments_opt loc comments
                  @@ fuse_with_space [Atom "else"; statement ~pretty_semicolon body];
                ]
            | None ->
              group
                [
                  statement_with_test "if" (expression test);
                  statement_after_test ~pretty_semicolon consequent;
                ]
          end
      | S.Labeled { S.Labeled.label; body; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (fuse [identifier label; Atom ":"; pretty_space; statement body])
      | S.Break { S.Break.label; comments } ->
        let s_break = Atom "break" in
        layout_node_with_comments_opt loc comments
        @@ with_semicolon
             (match label with
             | Some l -> fuse [s_break; space; identifier l]
             | None -> s_break)
      | S.Continue { S.Continue.label; comments } ->
        let s_continue = Atom "continue" in
        layout_node_with_comments_opt loc comments
        @@ with_semicolon
             (match label with
             | Some l -> fuse [s_continue; space; identifier l]
             | None -> s_continue)
      | S.With { S.With._object; body; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (fuse [statement_with_test "with" (expression _object); statement_after_test body])
      | S.Switch { S.Switch.discriminant; cases; comments } ->
        let case_nodes =
          let rec helper acc =
            let open Comment_attachment in
            function
            | [] -> List.rev acc
            | [((loc, _) as case)] ->
              let case = (loc, switch_case_comment_bounds case, switch_case ~last:true case) in
              List.rev (case :: acc)
            | ((loc, _) as case) :: rest ->
              let case = (loc, switch_case_comment_bounds case, switch_case ~last:false case) in
              helper (case :: acc) rest
          in
          helper [] cases |> list_with_newlines
        in
        let cases_node =
          wrap_and_indent ~break:pretty_hardline (Atom "{", Atom "}") [fuse case_nodes]
        in
        layout_node_with_comments_opt
          loc
          comments
          (fuse [statement_with_test "switch" (expression discriminant); pretty_space; cases_node])
      | S.Return { S.Return.argument; comments } ->
        let s_return = Atom "return" in
        layout_node_with_comments_opt loc comments
        @@ with_semicolon
             (match argument with
             | Some arg ->
               let arg =
                 match arg with
                 | (_, E.Logical _)
                 | (_, E.Binary _)
                 | (_, E.Sequence _)
                 | (_, E.JSXElement _) ->
                   group [wrap_in_parens_on_break (expression arg)]
                 | _ -> expression arg
               in
               fuse_with_space [s_return; arg]
             | None -> s_return)
      | S.Throw { S.Throw.argument; comments } ->
        layout_node_with_comments_opt loc comments
        @@ with_semicolon
             (fuse_with_space [Atom "throw"; group [wrap_in_parens_on_break (expression argument)]])
      | S.Try { S.Try.block = b; handler; finalizer; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (fuse
             [
               Atom "try";
               pretty_space;
               block b;
               (match handler with
               | Some (loc, { S.Try.CatchClause.param; body; comments }) ->
                 source_location_with_comments
                   ?comments
                   ( loc,
                     match param with
                     | Some p ->
                       fuse
                         [
                           pretty_space;
                           statement_with_test "catch" (pattern ~ctxt:normal_context p);
                           pretty_space;
                           block body;
                         ]
                     | None -> fuse [pretty_space; Atom "catch"; pretty_space; block body] )
               | None -> Empty);
               (match finalizer with
               | Some b -> fuse [pretty_space; Atom "finally"; pretty_space; block b]
               | None -> Empty);
             ])
      | S.While { S.While.test; body; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (fuse
             [
               statement_with_test "while" (expression test);
               statement_after_test ~pretty_semicolon body;
             ])
      | S.DoWhile { S.DoWhile.body; test; comments } ->
        layout_node_with_comments_opt loc comments
        @@ with_semicolon
             (fuse
                [
                  fuse_with_space [Atom "do"; statement body];
                  pretty_space;
                  Atom "while";
                  pretty_space;
                  group [wrap_and_indent (Atom "(", Atom ")") [expression test]];
                ])
      | S.For { S.For.init; test; update; body; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               statement_with_test
                 "for"
                 (join
                    (fuse [Atom ";"; pretty_line])
                    [
                      begin
                        match init with
                        | Some (S.For.InitDeclaration decl) ->
                          let ctxt = { normal_context with group = In_for_init } in
                          variable_declaration ~ctxt decl
                        | Some (S.For.InitExpression expr) ->
                          let ctxt = { normal_context with group = In_for_init } in
                          expression_with_parens ~precedence:0 ~ctxt expr
                        | None -> Empty
                      end;
                      begin
                        match test with
                        | Some expr -> expression expr
                        | None -> Empty
                      end;
                      begin
                        match update with
                        | Some expr -> expression expr
                        | None -> Empty
                      end;
                    ]);
               statement_after_test ~pretty_semicolon body;
             ]
      | S.ForIn { S.ForIn.left; right; body; each; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               Atom "for";
               ( if each then
                 fuse [space; Atom "each"]
               else
                 Empty );
               pretty_space;
               wrap_in_parens
                 (fuse_with_space
                    [
                      begin
                        match left with
                        | S.ForIn.LeftDeclaration decl -> variable_declaration decl
                        | S.ForIn.LeftPattern patt -> pattern patt
                      end;
                      Atom "in";
                      expression right;
                    ]);
               statement_after_test ~pretty_semicolon body;
             ]
      | S.FunctionDeclaration func -> function_ loc func
      | S.VariableDeclaration decl ->
        let semicolon =
          if pretty_semicolon then
            Some (IfPretty (Atom ";", Empty))
          else
            Some (Atom ";")
        in
        variable_declaration ?semicolon (loc, decl)
      | S.ClassDeclaration class_ -> class_base loc class_
      | S.EnumDeclaration enum -> enum_declaration loc enum
      | S.ForOf { S.ForOf.left; right; body; await; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               Atom "for";
               ( if await then
                 fuse [space; Atom "await"]
               else
                 Empty );
               pretty_space;
               wrap_in_parens
                 (fuse
                    [
                      begin
                        match left with
                        | S.ForOf.LeftDeclaration decl -> variable_declaration decl
                        | S.ForOf.LeftPattern patt -> pattern patt
                      end;
                      space;
                      Atom "of";
                      space;
                      expression right;
                    ]);
               statement_after_test ~pretty_semicolon body;
             ]
      | S.ImportDeclaration import -> import_declaration loc import
      | S.ExportNamedDeclaration export -> export_declaration loc export
      | S.ExportDefaultDeclaration export -> export_default_declaration loc export
      | S.TypeAlias typeAlias -> type_alias ~declare:false loc typeAlias
      | S.OpaqueType opaqueType -> opaque_type ~declare:false loc opaqueType
      | S.InterfaceDeclaration interface -> interface_declaration loc interface
      | S.DeclareClass interface -> declare_class loc interface
      | S.DeclareFunction func -> declare_function loc func
      | S.DeclareInterface interface -> declare_interface loc interface
      | S.DeclareVariable var -> declare_variable loc var
      | S.DeclareModuleExports exports -> declare_module_exports loc exports
      | S.DeclareModule m -> declare_module loc m
      | S.DeclareTypeAlias typeAlias -> type_alias ~declare:true loc typeAlias
      | S.DeclareOpaqueType opaqueType -> opaque_type ~declare:true loc opaqueType
      | S.DeclareExportDeclaration export -> declare_export_declaration loc export )

(* The beginning of a statement that does a "test", like `if (test)` or `while (test)` *)
and statement_with_test name test =
  fuse [Atom name; pretty_space; group [wrap_and_indent (Atom "(", Atom ")") [test]]]

(* A statement following a "test", like the `statement` in `if (expr) statement` or
   `for (...) statement`. Better names for this are welcome! *)
and statement_after_test ?pretty_semicolon = function
  | (_, Ast.Statement.Empty _) as stmt -> statement ?pretty_semicolon stmt
  | (_, Ast.Statement.Block _) as stmt -> fuse [pretty_space; statement ?pretty_semicolon stmt]
  | stmt -> Indent (fuse [pretty_line; statement ?pretty_semicolon stmt])

and expression ?(ctxt = normal_context) (root_expr : (Loc.t, Loc.t) Ast.Expression.t) =
  let (loc, expr) = root_expr in
  let module E = Ast.Expression in
  let precedence = precedence_of_expression (loc, expr) in
  source_location_with_comments
    ( loc,
      match expr with
      | E.This { E.This.comments } -> layout_node_with_comments_opt loc comments (Atom "this")
      | E.Super { E.Super.comments } -> layout_node_with_comments_opt loc comments (Atom "super")
      | E.Array { E.Array.elements; comments } ->
        let rev_elements =
          List.rev_map
            (function
              | Some expr -> expression_or_spread ~ctxt:normal_context expr
              | None -> Empty)
            elements
        in
        (* if the last element is a hole, then we need to manually insert a trailing `,`, even in
         ugly mode, and disable automatic trailing separators. *)
        let (trailing_sep, rev_elements) =
          match rev_elements with
          | Empty :: tl -> (false, Atom "," :: tl)
          | _ -> (true, rev_elements)
        in
        layout_node_with_comments_opt loc comments
        @@ group
             [
               new_list
                 ~wrap:(Atom "[", Atom "]")
                 ~sep:(Atom ",")
                 ~trailing_sep
                 (List.rev rev_elements);
             ]
      | E.Object { E.Object.properties; comments } ->
        let prop_loc = function
          | E.Object.Property (loc, _) -> loc
          | E.Object.SpreadProperty (loc, _) -> loc
        in
        let props =
          Base.List.map
            ~f:(fun prop ->
              ( prop_loc prop,
                Comment_attachment.object_property_comment_bounds prop,
                object_property prop ))
            properties
          |> list_with_newlines ~sep:(Atom ",") ~sep_linebreak:pretty_line
        in
        (* Add trailing comma to last property *)
        let props =
          if props = [] then
            props
          else
            [fuse props; if_break (Atom ",") Empty]
        in
        (* If first prop is on a different line then pretty print with line breaks *)
        let break =
          match properties with
          | [] -> None
          | first_prop :: _ ->
            if Loc.(loc.start.line < (prop_loc first_prop).start.line) then
              Some pretty_hardline
            else
              Some pretty_line
        in
        let props_layout = wrap_and_indent ?break (Atom "{", Atom "}") props in
        layout_node_with_comments_opt loc comments @@ group [props_layout]
      | E.Sequence { E.Sequence.expressions; comments } ->
        (* to get an AST like `x, (y, z)`, then there must've been parens
         around the right side. we can force that by bumping the minimum
         precedence. *)
        let precedence = precedence + 1 in
        let layouts = Base.List.map ~f:(expression_with_parens ~precedence ~ctxt) expressions in
        layout_node_with_comments_opt loc comments
        @@ group [join (fuse [Atom ","; pretty_line]) layouts]
      | E.Identifier ident -> identifier ident
      | E.Literal lit -> literal loc lit
      | E.Function func -> function_ loc func
      | E.ArrowFunction func -> arrow_function ~ctxt ~precedence loc func
      | E.Assignment { E.Assignment.operator; left; right; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               pattern ~ctxt left;
               pretty_space;
               begin
                 match operator with
                 | None -> Atom "="
                 | Some op -> Atom (Flow_ast_utils.string_of_assignment_operator op)
               end;
               pretty_space;
               begin
                 let ctxt = context_after_token ctxt in
                 expression_with_parens ~precedence ~ctxt right
               end;
             ]
      | E.Binary { E.Binary.operator; left; right; comments } ->
        let module B = E.Binary in
        layout_node_with_comments_opt loc comments
        @@ fuse_with_space
             [
               expression_with_parens ~precedence ~ctxt left;
               Atom (Flow_ast_utils.string_of_binary_operator operator);
               begin
                 match (operator, right) with
                 | (E.Binary.Plus, (_, E.Unary { E.Unary.operator = E.Unary.Plus; _ }))
                 | (E.Binary.Minus, (_, E.Unary { E.Unary.operator = E.Unary.Minus; _ }))
                 | ( E.Binary.Plus,
                     (_, E.Update { E.Update.prefix = true; operator = E.Update.Increment; _ }) )
                 | ( E.Binary.Minus,
                     (_, E.Update { E.Update.prefix = true; operator = E.Update.Decrement; _ }) ) ->
                   let ctxt = context_after_token ctxt in
                   fuse [ugly_space; expression ~ctxt right]
                 | _ ->
                   (* to get an AST like `x + (y - z)`, then there must've been parens
             around the right side. we can force that by bumping the minimum
             precedence to not have parens. *)
                   let precedence = precedence + 1 in
                   let ctxt =
                     {
                       ctxt with
                       left =
                         (match operator with
                         | E.Binary.Minus -> In_minus_op
                         | E.Binary.Plus -> In_plus_op
                         | _ -> Normal_left);
                     }
                   in
                   expression_with_parens ~precedence ~ctxt right
               end;
             ]
      | E.Call c -> call ~precedence ~ctxt c loc
      | E.OptionalCall { E.OptionalCall.call = c; optional } ->
        call ~optional ~precedence ~ctxt c loc
      | E.Conditional { E.Conditional.test; consequent; alternate; comments } ->
        let test_layout =
          (* increase precedence since conditionals are right-associative *)
          expression_with_parens ~precedence:(precedence + 1) ~ctxt test
        in
        layout_node_with_comments_opt loc comments
        @@ group
             [
               test_layout;
               Indent
                 (fuse
                    [
                      pretty_line;
                      Atom "?";
                      pretty_space;
                      expression_with_parens ~precedence:min_precedence ~ctxt consequent;
                      pretty_line;
                      Atom ":";
                      pretty_space;
                      expression_with_parens ~precedence:min_precedence ~ctxt alternate;
                    ]);
             ]
      | E.Logical { E.Logical.operator; left; right; comments } ->
        let expression_with_parens ~precedence ~ctxt expr =
          match (operator, expr) with
          (* Logical expressions inside a nullish coalese expression must be wrapped in parens *)
          | ( E.Logical.NullishCoalesce,
              (_, E.Logical { E.Logical.operator = E.Logical.And | E.Logical.Or; _ }) ) ->
            wrap_in_parens (expression expr)
          | _ -> expression_with_parens ~precedence ~ctxt expr
        in
        let left = expression_with_parens ~precedence ~ctxt left in
        let operator =
          match operator with
          | E.Logical.Or -> Atom "||"
          | E.Logical.And -> Atom "&&"
          | E.Logical.NullishCoalesce -> Atom "??"
        in
        let right = expression_with_parens ~precedence:(precedence + 1) ~ctxt right in
        (* if we need to wrap, the op stays on the first line, with the RHS on a
         new line and indented by 2 spaces *)
        layout_node_with_comments_opt loc comments
        @@ Group [left; pretty_space; operator; Indent (fuse [pretty_line; right])]
      | E.Member m -> member ~precedence ~ctxt m loc
      | E.OptionalMember { E.OptionalMember.member = m; optional } ->
        member ~optional ~precedence ~ctxt m loc
      | E.New { E.New.callee; targs; arguments; comments } ->
        let callee_layout =
          if definitely_needs_parens ~precedence ctxt callee || contains_call_expression callee then
            wrap_in_parens (expression ~ctxt callee)
          else
            expression ~ctxt callee
        in
        layout_node_with_comments_opt loc comments
        @@ group
             [
               fuse_with_space [Atom "new"; callee_layout];
               option call_type_args targs;
               option call_args arguments;
             ]
      | E.Unary { E.Unary.operator; argument; comments } ->
        let (s_operator, needs_space) =
          match operator with
          | E.Unary.Minus -> (Atom "-", false)
          | E.Unary.Plus -> (Atom "+", false)
          | E.Unary.Not -> (Atom "!", false)
          | E.Unary.BitNot -> (Atom "~", false)
          | E.Unary.Typeof -> (Atom "typeof", true)
          | E.Unary.Void -> (Atom "void", true)
          | E.Unary.Delete -> (Atom "delete", true)
          | E.Unary.Await -> (Atom "await", true)
        in
        let expr =
          let ctxt =
            {
              ctxt with
              left =
                (match operator with
                | E.Unary.Minus -> In_minus_op
                | E.Unary.Plus -> In_plus_op
                | _ -> Normal_left);
            }
          in
          expression_with_parens ~precedence ~ctxt argument
        in
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               s_operator;
               ( if needs_space then
                 match argument with
                 | (_, E.Sequence _) -> Empty
                 | _ -> space
               else
                 Empty );
               expr;
             ]
      | E.Update { E.Update.operator; prefix; argument; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (let s_operator =
             match operator with
             | E.Update.Increment -> Atom "++"
             | E.Update.Decrement -> Atom "--"
           in
           (* we never need to wrap `argument` in parens because it must be a valid
         left-hand side expression *)
           if prefix then
             fuse [s_operator; expression ~ctxt argument]
           else
             fuse [expression ~ctxt argument; s_operator])
      | E.Class class_ -> class_base loc class_
      | E.Yield { E.Yield.argument; delegate; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               Atom "yield";
               ( if delegate then
                 Atom "*"
               else
                 Empty );
               (match argument with
               | Some arg -> fuse [space; expression ~ctxt arg]
               | None -> Empty);
             ]
      | E.MetaProperty { E.MetaProperty.meta; property; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse [identifier meta; Atom "."; identifier property]
      | E.TaggedTemplate
          {
            E.TaggedTemplate.tag;
            quasi =
              (template_loc, ({ E.TemplateLiteral.comments = template_comments; _ } as template));
            comments;
          } ->
        let ctxt = { normal_context with left = In_tagged_template } in
        layout_node_with_comments_opt loc comments
        @@ fuse
             [
               expression_with_parens ~precedence ~ctxt tag;
               source_location_with_comments
                 ?comments:template_comments
                 (template_loc, template_literal template);
             ]
      | E.TemplateLiteral ({ E.TemplateLiteral.comments; _ } as template) ->
        layout_node_with_comments_opt loc comments (template_literal template)
      | E.JSXElement el -> jsx_element loc el
      | E.JSXFragment fr -> jsx_fragment loc fr
      | E.TypeCast { E.TypeCast.expression = expr; annot; comments } ->
        layout_node_with_comments_opt loc comments
        @@ wrap_in_parens (fuse [expression expr; type_annotation annot])
      | E.Import { E.Import.argument; comments } ->
        layout_node_with_comments_opt loc comments
        @@ fuse [Atom "import"; wrap_in_parens (expression argument)]
      (* Not supported *)
      | E.Comprehension _
      | E.Generator _ ->
        not_supported loc "Comprehension not supported" )

and call ?(optional = false) ~precedence ~ctxt call_node loc =
  let { Ast.Expression.Call.callee; targs; arguments; comments } = call_node in
  let (targs, lparen) =
    match targs with
    | None ->
      let lparen =
        if optional then
          "?.("
        else
          "("
      in
      (Empty, lparen)
    | Some (loc, { Ast.Expression.CallTypeArgs.arguments; comments }) ->
      let less_than =
        if optional then
          "?.<"
        else
          "<"
      in
      ( source_location_with_comments
          ?comments
          ( loc,
            group
              [
                new_list
                  ~wrap:(Atom less_than, Atom ">")
                  ~sep:(Atom ",")
                  (Base.List.map ~f:call_type_arg arguments);
              ] ),
        "(" )
  in
  layout_node_with_comments_opt
    loc
    comments
    (fuse [expression_with_parens ~precedence ~ctxt callee; targs; call_args ~lparen arguments])

and expression_with_parens ~precedence ~(ctxt : expression_context) expr =
  if definitely_needs_parens ~precedence ctxt expr then
    wrap_in_parens (expression ~ctxt:normal_context expr)
  else
    expression ~ctxt expr

and expression_or_spread ?(ctxt = normal_context) expr_or_spread =
  (* min_precedence causes operators that should always be parenthesized
     (they have precedence = 0) to be parenthesized. one notable example is
     the comma operator, which would be confused with additional arguments if
     not parenthesized. *)
  let precedence = min_precedence in
  match expr_or_spread with
  | Ast.Expression.Expression expr -> expression_with_parens ~precedence ~ctxt expr
  | Ast.Expression.Spread (loc, { Ast.Expression.SpreadElement.argument; comments }) ->
    source_location_with_comments
      ?comments
      (loc, fuse [Atom "..."; expression_with_parens ~precedence ~ctxt argument])

and identifier (loc, { Ast.Identifier.name; comments }) = identifier_with_comments loc name comments

and number_literal_type loc { Ast.NumberLiteral.value = _; raw; comments } =
  layout_node_with_comments_opt loc comments (Atom raw)

and number_literal ~in_member_object raw num =
  let str = Dtoa.shortest_string_of_float num in
  if in_member_object then
    (* `1.foo` is a syntax error, but `1.0.foo`, `1e0.foo` and even `1..foo` are all ok. *)
    let is_int x = not (String.contains x '.' || String.contains x 'e') in
    let if_pretty =
      if is_int raw then
        wrap_in_parens (Atom raw)
      else
        Atom raw
    in
    let if_ugly =
      if is_int str then
        fuse [Atom str; Atom "."]
      else
        Atom str
    in
    if if_pretty = if_ugly then
      if_pretty
    else
      IfPretty (if_pretty, if_ugly)
  else if String.equal raw str then
    Atom raw
  else
    IfPretty (Atom raw, Atom str)

and literal loc { Ast.Literal.raw; value; comments } =
  let open Ast.Literal in
  layout_node_with_comments_opt loc comments
  @@
  match value with
  | Number num -> number_literal ~in_member_object:false raw num
  | String str ->
    let quote = better_quote str in
    fuse [Atom quote; Atom (utf8_escape ~quote str); Atom quote]
  | RegExp { RegExp.pattern; flags } ->
    let flags = flags |> String_utils.to_list |> List.sort Char.compare |> String_utils.of_list in
    fuse [Atom "/"; Atom pattern; Atom "/"; Atom flags]
  | _ -> Atom raw

and string_literal_type loc { Ast.StringLiteral.value = _; raw; comments } =
  layout_node_with_comments_opt loc comments (Atom raw)

and bigint_literal_type loc { Ast.BigIntLiteral.approx_value = _; bigint; comments } =
  layout_node_with_comments_opt loc comments (Atom bigint)

and boolean_literal_type loc { Ast.BooleanLiteral.value; comments } =
  layout_node_with_comments_opt
    loc
    comments
    (Atom
       ( if value then
         "true"
       else
         "false" ))

and member ?(optional = false) ~precedence ~ctxt member_node loc =
  let { Ast.Expression.Member._object; property; comments } = member_node in
  let computed =
    match property with
    | Ast.Expression.Member.PropertyExpression _ -> true
    | Ast.Expression.Member.PropertyIdentifier _
    | Ast.Expression.Member.PropertyPrivateName _ ->
      false
  in
  let (ldelim, rdelim) =
    match (computed, optional) with
    | (false, false) -> (Atom ".", Empty)
    | (false, true) -> (Atom "?.", Empty)
    | (true, false) -> (Atom "[", Atom "]")
    | (true, true) -> (Atom "?.[", Atom "]")
  in
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         begin
           match _object with
           | (_, Ast.Expression.Call _) -> expression ~ctxt _object
           | ( loc,
               Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Number num; raw; comments }
             )
             when not computed ->
             (* 1.foo would be confused with a decimal point, so it needs parens *)
             source_location_with_comments
               ?comments
               (loc, number_literal ~in_member_object:true raw num)
           | _ -> expression_with_parens ~precedence ~ctxt _object
         end;
         ldelim;
         begin
           match property with
           | Ast.Expression.Member.PropertyIdentifier (loc, { Ast.Identifier.name = id; comments })
             ->
             source_location_with_comments ?comments (loc, Atom id)
           | Ast.Expression.Member.PropertyPrivateName
               ( loc,
                 {
                   Ast.PrivateName.id = (_, { Ast.Identifier.name = id; comments = id_comments });
                   comments = private_comments;
                 } ) ->
             let comments =
               Flow_ast_utils.merge_comments ~inner:id_comments ~outer:private_comments
             in
             source_location_with_comments ?comments (loc, Atom ("#" ^ id))
           | Ast.Expression.Member.PropertyExpression expr -> expression ~ctxt expr
         end;
         rdelim;
       ]

and string_literal (loc, { Ast.StringLiteral.value; comments; _ }) =
  let quote = better_quote value in
  source_location_with_comments
    ?comments
    (loc, fuse [Atom quote; Atom (utf8_escape ~quote value); Atom quote])

and pattern_object_property_key =
  let open Ast.Pattern.Object in
  function
  | Property.Literal (loc, lit) -> source_location_with_comments (loc, literal loc lit)
  | Property.Identifier ident -> identifier ident
  | Property.Computed (loc, { Ast.ComputedKey.expression = expr; comments }) ->
    layout_node_with_comments_opt loc comments
    @@ fuse [Atom "["; Sequence ({ seq with break = Break_if_needed }, [expression expr]); Atom "]"]

and pattern ?(ctxt = normal_context) ((loc, pat) : (Loc.t, Loc.t) Ast.Pattern.t) =
  let module P = Ast.Pattern in
  let rest_element loc { P.RestElement.argument; comments } =
    source_location_with_comments ?comments (loc, fuse [Atom "..."; pattern argument])
  in
  source_location_with_comments
    ( loc,
      match pat with
      | P.Object { P.Object.properties; annot } ->
        group
          [
            new_list
              ~wrap:(Atom "{", Atom "}")
              ~sep:
                (Atom ",")
                (* Object rest can have comma but most tooling still apply old
          pre-spec rules that disallow it so omit it to be safe *)
              ~trailing_sep:false
              (List.map
                 (function
                   | P.Object.Property
                       (loc, { P.Object.Property.key; pattern = pat; default; shorthand }) ->
                     let prop = pattern_object_property_key key in
                     let prop =
                       match shorthand with
                       | false -> fuse [prop; Atom ":"; pretty_space; pattern pat]
                       | true -> prop
                     in
                     let prop =
                       match default with
                       | Some expr -> fuse_with_default prop expr
                       | None -> prop
                     in
                     source_location_with_comments (loc, prop)
                   | P.Object.RestElement (loc, el) -> rest_element loc el)
                 properties);
            hint type_annotation annot;
          ]
      | P.Array { P.Array.elements; annot; comments } ->
        layout_node_with_comments_opt
          loc
          comments
          (group
             [
               new_list
                 ~wrap:(Atom "[", Atom "]")
                 ~sep:(Atom ",")
                 ~trailing_sep:false (* Array rest cannot have trailing *)
                 (List.map
                    (function
                      | None -> Empty
                      | Some (P.Array.Element (loc, { P.Array.Element.argument; default })) ->
                        let elem = pattern argument in
                        let elem =
                          match default with
                          | Some expr -> fuse_with_default elem expr
                          | None -> elem
                        in
                        source_location_with_comments (loc, elem)
                      | Some (P.Array.RestElement (loc, el)) -> rest_element loc el)
                    elements);
               hint type_annotation annot;
             ])
      | P.Identifier { P.Identifier.name; annot; optional } ->
        fuse
          [
            identifier name;
            ( if optional then
              Atom "?"
            else
              Empty );
            hint type_annotation annot;
          ]
      | P.Expression expr -> expression ~ctxt expr )

and fuse_with_default ?(ctxt = normal_context) node expr =
  fuse
    [
      node;
      pretty_space;
      Atom "=";
      pretty_space;
      expression_with_parens
        ~precedence:precedence_of_assignment
        ~ctxt:(context_after_token ctxt)
        expr;
    ]

and template_literal { Ast.Expression.TemplateLiteral.quasis; expressions; comments = _ } =
  let module T = Ast.Expression.TemplateLiteral in
  let template_element i (loc, { T.Element.value = { T.Element.raw; _ }; tail }) =
    fuse
      [
        source_location_with_comments
          ( loc,
            fuse
              [
                ( if i > 0 then
                  Atom "}"
                else
                  Empty );
                Atom raw;
                ( if not tail then
                  Atom "${"
                else
                  Empty );
              ] );
        ( if not tail then
          expression (List.nth expressions i)
        else
          Empty );
      ]
  in
  fuse [Atom "`"; fuse (List.mapi template_element quasis); Atom "`"]

and variable_declaration
    ?(ctxt = normal_context)
    ?(semicolon = Empty)
    (loc, { Ast.Statement.VariableDeclaration.declarations; kind; comments }) =
  let kind_layout =
    match kind with
    | Ast.Statement.VariableDeclaration.Var -> Atom "var"
    | Ast.Statement.VariableDeclaration.Let -> Atom "let"
    | Ast.Statement.VariableDeclaration.Const -> Atom "const"
  in
  let has_init =
    List.exists
      (fun var ->
        let open Ast.Statement.VariableDeclaration.Declarator in
        match var with
        | (_, { id = _; init = Some _ }) -> true
        | _ -> false)
      declarations
  in
  let sep =
    if has_init then
      pretty_hardline
    else
      pretty_line
  in
  let decls_layout =
    match declarations with
    | [] -> Empty (* impossible *)
    | [single_decl] -> variable_declarator ~ctxt single_decl
    | hd :: tl ->
      let hd = variable_declarator ~ctxt hd in
      let tl = Base.List.map ~f:(variable_declarator ~ctxt) tl in
      group [hd; Atom ","; Indent (fuse [sep; join (fuse [Atom ","; sep]) tl])]
  in
  source_location_with_comments
    ?comments
    (loc, fuse [fuse_with_space [kind_layout; decls_layout]; semicolon])

and variable_declarator ~ctxt (loc, { Ast.Statement.VariableDeclaration.Declarator.id; init }) =
  source_location_with_comments
    ( loc,
      match init with
      | Some expr ->
        fuse
          [
            pattern ~ctxt id;
            pretty_space;
            Atom "=";
            pretty_space;
            expression_with_parens ~precedence:precedence_of_assignment ~ctxt expr;
          ]
      | None -> pattern ~ctxt id )

and arrow_function
    ?(ctxt = normal_context)
    ~precedence
    loc
    {
      Ast.Function.params =
        (params_loc, { Ast.Function.Params.comments = params_comments; _ }) as params;
      body;
      async;
      predicate;
      return;
      tparams;
      comments;
      generator = _;
      id = _;
      (* arrows don't have ids and can't be generators *) sig_loc = _;
    } =
  let is_single_simple_param =
    match params with
    | ( _,
        {
          Ast.Function.Params.params =
            [
              ( _,
                {
                  Ast.Function.Param.argument =
                    ( _,
                      Ast.Pattern.Identifier
                        { Ast.Pattern.Identifier.optional = false; annot = Ast.Type.Missing _; _ }
                    );
                  default = None;
                } );
            ];
          rest = None;
          comments = _;
        } ) ->
      true
    | _ -> false
  in
  let params_and_stuff =
    match (is_single_simple_param, return, predicate, tparams) with
    | (true, Ast.Type.Missing _, None, None) ->
      layout_node_with_comments_opt
        params_loc
        params_comments
        (List.hd (function_params ~ctxt params))
    | (_, _, _, _) ->
      let params =
        layout_node_with_comments_opt params_loc params_comments (arrow_function_params params)
      in
      fuse [option type_parameter tparams; params; function_return ~arrow:true return predicate]
  in
  let body_separator =
    let open Comment_attachment in
    let (first_leading, _) =
      match body with
      | Ast.Function.BodyBlock block -> block_comment_bounds block
      | Ast.Function.BodyExpression expr -> expression_comment_bounds expr
    in
    (* If the body begins with a comment we must insert a newline before the body
       to ensure that comment will be attached to body after formatting. *)
    if first_leading = None then
      pretty_space
    else
      pretty_hardline
  in
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         fuse_with_space
           [
             ( if async then
               Atom "async"
             else
               Empty );
             params_and_stuff;
           ];
         (* Babylon does not parse ():*=>{}` because it thinks the `*=` is an
       unexpected multiply-and-assign operator. Thus, we format this with a
       space e.g. `():* =>{}`. *)
         begin
           match return with
           | Ast.Type.Available (_, (_, Ast.Type.Exists _)) -> space
           | _ -> pretty_space
         end;
         Atom "=>";
         body_separator;
         begin
           match body with
           | Ast.Function.BodyBlock b -> block b
           | Ast.Function.BodyExpression expr ->
             let ctxt = { normal_context with group = In_arrow_func } in
             expression_with_parens ~precedence ~ctxt expr
         end;
       ]

and has_rest_param = function
  | (_, { Ast.Function.Params.rest = Some _; _ }) -> true
  | _ -> false

and arrow_function_params params =
  group
    [
      new_list
        ~wrap:(Atom "(", Atom ")")
        ~sep:(Atom ",")
        ~trailing_sep:(not (has_rest_param params))
        (function_params ~ctxt:normal_context params);
    ]

and function_ loc func =
  let {
    Ast.Function.id;
    params;
    body;
    async;
    generator;
    predicate;
    return;
    tparams;
    sig_loc = _;
    comments;
  } =
    func
  in
  let prefix =
    let s_func =
      fuse
        [
          Atom "function";
          ( if generator then
            Atom "*"
          else
            Empty );
        ]
    in
    let id =
      match id with
      | Some id -> fuse [s_func; space; identifier id]
      | None -> s_func
    in
    if async then
      fuse [Atom "async"; space; id]
    else
      id
  in
  function_base ~prefix ~params ~body ~predicate ~return ~tparams ~loc ~comments

and function_base ~prefix ~params ~body ~predicate ~return ~tparams ~loc ~comments =
  let (params_loc, { Ast.Function.Params.comments = params_comments; _ }) = params in
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         prefix;
         option type_parameter tparams;
         layout_node_with_comments_opt params_loc params_comments
         @@ list
              ~wrap:(Atom "(", Atom ")")
              ~sep:(Atom ",")
              ~trailing:(not (has_rest_param params))
              (function_params ~ctxt:normal_context params);
         function_return ~arrow:false return predicate;
         pretty_space;
         begin
           match body with
           | Ast.Function.BodyBlock b -> block b
           | Ast.Function.BodyExpression _ -> failwith "Only arrows should have BodyExpressions"
         end;
       ]

and function_params ~ctxt (_, { Ast.Function.Params.params; rest; comments = _ }) =
  let s_params =
    Base.List.map
      ~f:(fun (loc, { Ast.Function.Param.argument; default }) ->
        let node = pattern ~ctxt argument in
        let node =
          match default with
          | Some expr -> fuse_with_default node expr
          | None -> node
        in
        source_location_with_comments (loc, node))
      params
  in
  match rest with
  | Some (loc, { Ast.Function.RestParam.argument; comments }) ->
    let s_rest =
      source_location_with_comments ?comments (loc, fuse [Atom "..."; pattern ~ctxt argument])
    in
    List.append s_params [s_rest]
  | None -> s_params

and function_return ~arrow return predicate =
  (* Function return types in arrow functions must be wrapped in parens or else arrow
     from arrow function is interpreted as part of the function type. *)
  let needs_parens = function
    | ( _,
        ( _,
          Ast.Type.Function
            { Ast.Type.Function.params = (_, { Ast.Type.Function.Params.params = _ :: _; _ }); _ }
        ) )
      when arrow ->
      true
    | _ -> false
  in
  match (return, predicate) with
  | (Ast.Type.Missing _, None) -> Empty
  | (Ast.Type.Missing _, Some pred) -> fuse [Atom ":"; pretty_space; type_predicate pred]
  | (Ast.Type.Available ret, Some pred) ->
    fuse [type_annotation ~parens:(needs_parens ret) ret; pretty_space; type_predicate pred]
  | (Ast.Type.Available ret, None) -> type_annotation ~parens:(needs_parens ret) ret

and block (loc, { Ast.Statement.Block.body; comments }) =
  let statements = statement_list ~pretty_semicolon:true body in
  source_location_with_comments
    ?comments
    ( loc,
      if statements <> [] then
        group [wrap_and_indent ~break:pretty_hardline (Atom "{", Atom "}") [fuse statements]]
      else
        match internal_comments comments with
        | None -> Atom "{}"
        | Some comments -> fuse [Atom "{"; comments; Atom "}"] )

and decorators_list decorators =
  if List.length decorators > 0 then
    let decorators =
      List.map
        (fun (loc, { Ast.Class.Decorator.expression = expr; comments }) ->
          source_location_with_comments
            ?comments
            ( loc,
              fuse
                [
                  Atom "@";
                  begin
                    (* Magic number, after `Call` but before `Update` *)
                    let precedence = 18 in
                    expression_with_parens ~precedence ~ctxt:normal_context expr
                  end;
                ] ))
        decorators
    in
    group [join pretty_line decorators; if_pretty hardline space]
  else
    Empty

and class_method
    (loc, { Ast.Class.Method.kind; key; value = (func_loc, func); static; decorators; comments }) =
  let module M = Ast.Class.Method in
  let {
    Ast.Function.params;
    body;
    async;
    generator;
    predicate;
    return;
    tparams;
    id = _;
    (* methods don't use id; see `key` *) sig_loc = _;
    comments = func_comments;
  } =
    func
  in
  source_location_with_comments
    ?comments
    ( loc,
      let s_key = object_property_key key in
      let s_key =
        if generator then
          fuse [Atom "*"; s_key]
        else
          s_key
      in
      let s_kind =
        match kind with
        | M.Constructor
        | M.Method ->
          Empty
        | M.Get -> Atom "get"
        | M.Set -> Atom "set"
      in
      (* TODO: getters/setters/constructors will never be async *)
      let s_async =
        if async then
          Atom "async"
        else
          Empty
      in
      let prefix = fuse_with_space [s_async; s_kind; s_key] in
      fuse
        [
          decorators_list decorators;
          ( if static then
            fuse [Atom "static"; space]
          else
            Empty );
          source_location_with_comments
            ( func_loc,
              function_base
                ~prefix
                ~params
                ~body
                ~predicate
                ~return
                ~tparams
                ~loc:func_loc
                ~comments:func_comments );
        ] )

and class_property_helper loc key value static annot variance_ comments =
  let (declare, value) =
    match value with
    | Ast.Class.Property.Declared -> (true, None)
    | Ast.Class.Property.Uninitialized -> (false, None)
    | Ast.Class.Property.Initialized expr -> (false, Some expr)
  in
  source_location_with_comments
    ?comments
    ( loc,
      with_semicolon
        (fuse
           [
             ( if declare then
               fuse [Atom "declare"; space]
             else
               Empty );
             ( if static then
               fuse [Atom "static"; space]
             else
               Empty );
             option variance variance_;
             key;
             hint type_annotation annot;
             begin
               match value with
               | Some v ->
                 fuse
                   [
                     pretty_space;
                     Atom "=";
                     pretty_space;
                     expression_with_parens ~precedence:min_precedence ~ctxt:normal_context v;
                   ]
               | None -> Empty
             end;
           ]) )

and class_property (loc, { Ast.Class.Property.key; value; static; annot; variance; comments }) =
  class_property_helper loc (object_property_key key) value static annot variance comments

and class_private_field
    ( loc,
      {
        Ast.Class.PrivateField.key =
          ( ident_loc,
            {
              Ast.PrivateName.id = (_, { Ast.Identifier.name; comments = id_comments });
              comments = private_comments;
            } );
        value;
        static;
        annot;
        variance;
        comments;
      } ) =
  let key_comments = Flow_ast_utils.merge_comments ~inner:id_comments ~outer:private_comments in
  let key =
    layout_node_with_comments_opt
      ident_loc
      key_comments
      (identifier (Flow_ast_utils.ident_of_source (ident_loc, "#" ^ name)))
  in
  class_property_helper loc key value static annot variance comments

and class_body (loc, { Ast.Class.Body.body; comments }) =
  let elements =
    Base.List.map
      ~f:
        (let open Comment_attachment in
        function
        | Ast.Class.Body.Method ((loc, _) as meth) ->
          let comment_bounds =
            comment_bounds loc meth (fun collector (loc, meth) -> collector#class_method loc meth)
          in
          (loc, comment_bounds, class_method meth)
        | Ast.Class.Body.Property ((loc, _) as prop) ->
          let comment_bounds =
            comment_bounds loc prop (fun collector (loc, prop) -> collector#class_property loc prop)
          in
          (loc, comment_bounds, class_property prop)
        | Ast.Class.Body.PrivateField ((loc, _) as field) ->
          let comment_bounds =
            comment_bounds loc field (fun collector (loc, field) ->
                collector#class_private_field loc field)
          in
          (loc, comment_bounds, class_private_field field))
      body
    |> list_with_newlines
  in
  if body <> [] then
    source_location_with_comments
      ?comments
      (loc, group [wrap_and_indent ~break:pretty_hardline (Atom "{", Atom "}") [fuse elements]])
  else
    source_location_with_comments ?comments (loc, Atom "{}")

and class_implements implements =
  match implements with
  | None -> None
  | Some (loc, { Ast.Class.Implements.interfaces; comments }) ->
    Some
      (source_location_with_comments
         ?comments
         ( loc,
           fuse
             [
               Atom "implements";
               space;
               fuse_list
                 ~sep:(Atom ",")
                 (List.map
                    (fun (loc, { Ast.Class.Implements.Interface.id; targs }) ->
                      source_location_with_comments
                        (loc, fuse [identifier id; option type_args targs]))
                    interfaces);
             ] ))

and class_base loc { Ast.Class.id; body; tparams; extends; implements; classDecorators; comments } =
  let decorator_parts = decorators_list classDecorators in
  let class_parts =
    [
      Atom "class";
      begin
        match id with
        | Some ident -> fuse [space; identifier ident; option type_parameter tparams]
        | None -> Empty
      end;
    ]
  in
  let extends_parts =
    let class_extends =
      [
        begin
          match extends with
          | Some (loc, { Ast.Class.Extends.expr; targs; comments }) ->
            Some
              (source_location_with_comments
                 ?comments
                 ( loc,
                   fuse
                     [
                       Atom "extends";
                       space;
                       source_location_with_comments
                         (loc, fuse [expression expr; option type_args targs]);
                     ] ))
          | None -> None
        end;
        class_implements implements;
      ]
    in
    match deoptionalize class_extends with
    | [] -> []
    | items -> [Layout.Indent (fuse [line; join line items])]
  in
  let parts =
    []
    |> List.rev_append class_parts
    |> List.rev_append extends_parts
    |> List.cons pretty_space
    |> List.cons (class_body body)
    |> List.rev
  in
  group [decorator_parts; source_location_with_comments ?comments (loc, group parts)]

and enum_declaration loc { Ast.Statement.EnumDeclaration.id; body; comments } =
  let open Ast.Statement.EnumDeclaration in
  let representation_type name explicit =
    if explicit then
      fuse [space; Atom "of"; space; Atom name]
    else
      Empty
  in
  let wrap_body members =
    wrap_and_indent ~break:pretty_hardline (Atom "{", Atom "}") [join pretty_hardline members]
  in
  let defaulted_member (_, { DefaultedMember.id }) = fuse [identifier id; Atom ","] in
  let initialized_member id value =
    fuse [identifier id; pretty_space; Atom "="; pretty_space; value; Atom ","]
  in
  let boolean_member
      ( _,
        { InitializedMember.id; init = (loc, { Ast.BooleanLiteral.value = init_value; comments }) }
      ) =
    initialized_member
      id
      (layout_node_with_comments_opt
         loc
         comments
         ( if init_value then
           Atom "true"
         else
           Atom "false" ))
  in
  let number_member
      (_, { InitializedMember.id; init = (loc, { Ast.NumberLiteral.raw; comments; _ }) }) =
    initialized_member id (layout_node_with_comments_opt loc comments (Atom raw))
  in
  let string_member
      (_, { InitializedMember.id; init = (loc, { Ast.StringLiteral.raw; comments; _ }) }) =
    initialized_member id (layout_node_with_comments_opt loc comments (Atom raw))
  in
  let body =
    match body with
    | (loc, BooleanBody { BooleanBody.members; explicitType; comments }) ->
      fuse
        [
          representation_type "boolean" explicitType;
          pretty_space;
          layout_node_with_comments_opt loc comments
          @@ wrap_body
          @@ Base.List.map ~f:boolean_member members;
        ]
    | (loc, NumberBody { NumberBody.members; explicitType; comments }) ->
      fuse
        [
          representation_type "number" explicitType;
          pretty_space;
          layout_node_with_comments_opt loc comments
          @@ wrap_body
          @@ Base.List.map ~f:number_member members;
        ]
    | (loc, StringBody { StringBody.members; explicitType; comments }) ->
      fuse
        [
          representation_type "string" explicitType;
          pretty_space;
          ( layout_node_with_comments_opt loc comments
          @@ wrap_body
          @@
          match members with
          | StringBody.Defaulted members -> Base.List.map ~f:defaulted_member members
          | StringBody.Initialized members -> Base.List.map ~f:string_member members );
        ]
    | (loc, SymbolBody { SymbolBody.members; comments }) ->
      fuse
        [
          representation_type "symbol" true;
          pretty_space;
          layout_node_with_comments_opt loc comments
          @@ wrap_body
          @@ Base.List.map ~f:defaulted_member members;
        ]
  in
  layout_node_with_comments_opt loc comments (fuse [Atom "enum"; space; identifier id; body])

(* given a list of (loc * layout node) pairs, insert newlines between the nodes when necessary *)
and list_with_newlines
    ?(sep = Empty)
    ?(sep_linebreak = pretty_hardline)
    (nodes :
      (Loc.t * (Loc.t Ast.Comment.t option * Loc.t Ast.Comment.t option) * Layout.layout_node) list)
    =
  let rec helper nodes acc prev_loc =
    match nodes with
    | [] -> acc
    | (loc, (first_leading, last_trailing), node) :: rest ->
      let open Loc in
      (* Expand node's loc to include its attached comments *)
      let loc =
        let start =
          match first_leading with
          | None -> loc
          | Some (first_leading_loc, _) -> first_leading_loc
        in
        let _end =
          match last_trailing with
          | None -> loc
          | Some (last_trailing_loc, _) -> last_trailing_loc
        in
        btwn start _end
      in
      let has_trailing_line_comment =
        match last_trailing with
        | Some (_, Ast.Comment.Line _) -> true
        | _ -> false
      in
      let sep =
        if rest = [] then
          Empty
        (* Line comments already end in a hard newline, so another linebreak is not needed *)
        else if has_trailing_line_comment then
          sep
        else
          fuse [sep; sep_linebreak]
      in
      let acc =
        match (prev_loc, node) with
        (* empty line, don't add anything *)
        | (_, Empty) -> acc
        (* Lines are offset by more than one, let's add a line break. *)
        | (Some { Loc._end; _ }, node) when _end.line + 1 < loc.start.line ->
          fuse [pretty_hardline; node; sep] :: acc
        (* Hasn't matched, just add the node *)
        | (_, node) -> fuse [node; sep] :: acc
      in
      helper rest acc (Some loc)
  in
  let nodes = helper nodes [] None in
  List.rev nodes

and statement_list ?(pretty_semicolon = false) statements =
  let rec mapper acc = function
    | [] -> List.rev acc
    | ((loc, _) as stmt) :: rest ->
      let pretty_semicolon = pretty_semicolon && rest = [] in
      let comment_bounds = Comment_attachment.statement_comment_bounds stmt in
      let acc = (loc, comment_bounds, statement ~pretty_semicolon stmt) :: acc in
      (mapper [@tailcall]) acc rest
  in
  mapper [] statements |> list_with_newlines

and object_property_key key =
  let module O = Ast.Expression.Object in
  match key with
  | O.Property.Literal (loc, lit) -> source_location_with_comments (loc, literal loc lit)
  | O.Property.Identifier ident -> identifier ident
  | O.Property.Computed (loc, { Ast.ComputedKey.expression = expr; comments }) ->
    layout_node_with_comments_opt loc comments
    @@ fuse [Atom "["; Layout.Indent (fuse [pretty_line; expression expr]); pretty_line; Atom "]"]
  | O.Property.PrivateName _ -> failwith "Internal Error: Found object prop with private name"

and object_property property =
  let module O = Ast.Expression.Object in
  match property with
  | O.Property (loc, O.Property.Init { key; value; shorthand }) ->
    source_location_with_comments
      ( loc,
        let s_key = object_property_key key in
        if shorthand then
          s_key
        else
          group
            [
              s_key;
              Atom ":";
              pretty_space;
              expression_with_parens ~precedence:min_precedence ~ctxt:normal_context value;
            ] )
  | O.Property (loc, O.Property.Method { key; value = (fn_loc, func) }) ->
    let s_key = object_property_key key in
    let {
      Ast.Function.id;
      params;
      body;
      async;
      generator;
      predicate;
      return;
      tparams;
      sig_loc = _;
      comments = fn_comments;
    } =
      func
    in
    assert (id = None);

    (* methods don't have ids, see `key` *)
    let prefix =
      fuse
        [
          ( if async then
            fuse [Atom "async"; space]
          else
            Empty );
          ( if generator then
            Atom "*"
          else
            Empty );
          s_key;
        ]
    in
    source_location_with_comments
      ( loc,
        source_location_with_comments
          ( fn_loc,
            function_base
              ~prefix
              ~params
              ~body
              ~predicate
              ~return
              ~tparams
              ~loc:fn_loc
              ~comments:fn_comments ) )
  | O.Property (loc, O.Property.Get { key; value = (fn_loc, func); comments }) ->
    let {
      Ast.Function.id;
      params;
      body;
      async;
      generator;
      predicate;
      return;
      tparams;
      sig_loc = _;
      comments = fn_comments;
    } =
      func
    in
    assert (id = None);

    (* getters don't have ids, see `key` *)
    assert (not async);

    (* getters can't be async *)
    assert (not generator);

    (* getters can't be generators *)
    let prefix = fuse [Atom "get"; space; object_property_key key] in
    source_location_with_comments
      ?comments
      ( loc,
        source_location_with_comments
          ( fn_loc,
            function_base
              ~prefix
              ~params
              ~body
              ~predicate
              ~return
              ~tparams
              ~loc:fn_loc
              ~comments:fn_comments ) )
  | O.Property (loc, O.Property.Set { key; value = (fn_loc, func); comments }) ->
    let {
      Ast.Function.id;
      params;
      body;
      async;
      generator;
      predicate;
      return;
      tparams;
      sig_loc = _;
      comments = fn_comments;
    } =
      func
    in
    assert (id = None);

    (* setters don't have ids, see `key` *)
    assert (not async);

    (* setters can't be async *)
    assert (not generator);

    (* setters can't be generators *)
    let prefix = fuse [Atom "set"; space; object_property_key key] in
    source_location_with_comments
      ?comments
      ( loc,
        source_location_with_comments
          ( fn_loc,
            function_base
              ~prefix
              ~params
              ~body
              ~predicate
              ~return
              ~tparams
              ~loc:fn_loc
              ~comments:fn_comments ) )
  | O.SpreadProperty (loc, { O.SpreadProperty.argument; comments }) ->
    source_location_with_comments ?comments (loc, fuse [Atom "..."; expression argument])

and jsx_element loc { Ast.JSX.openingElement; closingElement; children; comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         begin
           match openingElement with
           | (_, { Ast.JSX.Opening.selfClosing = false; _ }) -> jsx_opening openingElement
           | (_, { Ast.JSX.Opening.selfClosing = true; _ }) -> jsx_self_closing openingElement
         end;
         jsx_children loc children;
         begin
           match closingElement with
           | Some closing -> jsx_closing closing
           | _ -> Empty
         end;
       ]

and jsx_fragment
    loc { Ast.JSX.frag_openingElement; frag_closingElement; frag_children; frag_comments } =
  layout_node_with_comments_opt loc frag_comments
  @@ fuse
       [
         jsx_fragment_opening frag_openingElement;
         jsx_children loc frag_children;
         jsx_closing_fragment frag_closingElement;
       ]

and jsx_identifier (loc, { Ast.JSX.Identifier.name; comments }) =
  identifier_with_comments loc name comments

and jsx_namespaced_name (loc, { Ast.JSX.NamespacedName.namespace; name }) =
  source_location_with_comments (loc, fuse [jsx_identifier namespace; Atom ":"; jsx_identifier name])

and jsx_member_expression (loc, { Ast.JSX.MemberExpression._object; property }) =
  source_location_with_comments
    ( loc,
      fuse
        [
          begin
            match _object with
            | Ast.JSX.MemberExpression.Identifier ident -> jsx_identifier ident
            | Ast.JSX.MemberExpression.MemberExpression member -> jsx_member_expression member
          end;
          Atom ".";
          jsx_identifier property;
        ] )

and jsx_expression_container loc { Ast.JSX.ExpressionContainer.expression = expr; comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         Atom "{";
         begin
           match expr with
           | Ast.JSX.ExpressionContainer.Expression expr -> expression expr
           | Ast.JSX.ExpressionContainer.EmptyExpression -> Empty
         end;
         Atom "}";
       ]

and jsx_spread_child loc { Ast.JSX.SpreadChild.expression = expr; comments } =
  fuse
    [Atom "{"; layout_node_with_comments_opt loc comments (Atom "..."); expression expr; Atom "}"]

and jsx_attribute (loc, { Ast.JSX.Attribute.name; value }) =
  let module A = Ast.JSX.Attribute in
  source_location_with_comments
    ( loc,
      fuse
        [
          begin
            match name with
            | A.Identifier ident -> jsx_identifier ident
            | A.NamespacedName name -> jsx_namespaced_name name
          end;
          begin
            match value with
            | Some v ->
              fuse
                [
                  Atom "=";
                  begin
                    match v with
                    | A.Literal (loc, lit) -> source_location_with_comments (loc, literal loc lit)
                    | A.ExpressionContainer (loc, express) ->
                      source_location_with_comments (loc, jsx_expression_container loc express)
                  end;
                ]
            | None -> flat_ugly_space (* TODO we shouldn't do this for the last attr *)
          end;
        ] )

and jsx_spread_attribute (loc, { Ast.JSX.SpreadAttribute.argument; comments }) =
  layout_node_with_comments_opt loc comments
  @@ fuse [Atom "{"; Atom "..."; expression argument; Atom "}"]

and jsx_element_name = function
  | Ast.JSX.Identifier ident -> jsx_identifier ident
  | Ast.JSX.NamespacedName name -> jsx_namespaced_name name
  | Ast.JSX.MemberExpression member -> jsx_member_expression member

and jsx_opening_attr = function
  | Ast.JSX.Opening.Attribute attr -> jsx_attribute attr
  | Ast.JSX.Opening.SpreadAttribute attr -> jsx_spread_attribute attr

and jsx_opening (loc, { Ast.JSX.Opening.name; attributes; selfClosing = _ }) =
  jsx_opening_helper loc (Some name) attributes

and jsx_fragment_opening loc = jsx_opening_helper loc None []

and jsx_opening_helper loc nameOpt attributes =
  source_location_with_comments
    ( loc,
      group
        [
          Atom "<";
          (match nameOpt with
          | Some name -> jsx_element_name name
          | None -> Empty);
          ( if attributes <> [] then
            Layout.Indent
              (fuse [line; join pretty_line (Base.List.map ~f:jsx_opening_attr attributes)])
          else
            Empty );
          Atom ">";
        ] )

and jsx_self_closing (loc, { Ast.JSX.Opening.name; attributes; selfClosing = _ }) =
  let attributes = Base.List.map ~f:jsx_opening_attr attributes in
  source_location_with_comments
    ( loc,
      group
        [
          Atom "<";
          jsx_element_name name;
          ( if attributes <> [] then
            fuse [Layout.Indent (fuse [line; join pretty_line attributes]); pretty_line]
          else
            pretty_space );
          Atom "/>";
        ] )

and jsx_closing (loc, { Ast.JSX.Closing.name }) =
  source_location_with_comments (loc, fuse [Atom "</"; jsx_element_name name; Atom ">"])

and jsx_closing_fragment loc = source_location_with_comments (loc, fuse [Atom "</>"])

and jsx_children loc (_children_loc, children) =
  Loc.(
    let processed_children = deoptionalize (Base.List.map ~f:jsx_child children) in
    (* Check for empty children *)
    if List.length processed_children <= 0 then
      Empty
    (* If start and end lines don't match check inner breaks *)
    else if loc._end.line > loc.start.line then
      let (children_n, _) =
        List.fold_left
          (fun (children_n, last_line) (loc, child) ->
            let child_n = SourceLocation (loc, child) in
            let formatted_child_n =
              match last_line with
              (* First child, newlines will always be forced via the `pretty_hardline` below *)
              | None -> child_n
              (* If there is at least one blank line between the current and previous child,
                 output a single blank line separating the children. *)
              | Some last_line when loc.start.line > last_line + 1 ->
                fuse [pretty_hardline; Newline; child_n]
              (* If the current child and the previous child line positions are offset match
           this via forcing a newline *)
              | Some last_line when loc.start.line > last_line ->
                (* TODO: Remove the `Newline` hack, this forces newlines to exist
                   when using the compact printer *)
                fuse [Newline; child_n]
              (* Must be on the same line as the previous child *)
              | Some _ -> child_n
            in
            (formatted_child_n :: children_n, Some loc._end.line))
          ([], None)
          processed_children
      in
      fuse [Layout.Indent (fuse (pretty_hardline :: List.rev children_n)); pretty_hardline]
    (* Single line *)
    else
      fuse (Base.List.map ~f:(fun (loc, child) -> SourceLocation (loc, child)) processed_children))

and jsx_child (loc, child) =
  match child with
  | Ast.JSX.Element elem -> Some (loc, jsx_element loc elem)
  | Ast.JSX.Fragment frag -> Some (loc, jsx_fragment loc frag)
  | Ast.JSX.ExpressionContainer express -> Some (loc, jsx_expression_container loc express)
  | Ast.JSX.SpreadChild spread -> Some (loc, jsx_spread_child loc spread)
  | Ast.JSX.Text { Ast.JSX.Text.raw; _ } ->
    begin
      match Utils_jsx.trim_jsx_text loc raw with
      | Some (loc, txt) -> Some (loc, Atom txt)
      | None -> None
    end

and partition_specifiers default specifiers =
  let open Ast.Statement.ImportDeclaration in
  let (special, named) =
    match specifiers with
    | Some (ImportNamespaceSpecifier (loc, id)) -> ([import_namespace_specifier (loc, id)], None)
    | Some (ImportNamedSpecifiers named_specifiers) ->
      ([], Some (import_named_specifiers named_specifiers))
    | None -> ([], None)
  in
  match default with
  | Some default -> (identifier default :: special, named)
  | None -> (special, named)

and import_namespace_specifier (loc, id) =
  source_location_with_comments (loc, fuse [Atom "*"; pretty_space; Atom "as"; space; identifier id])

and import_named_specifier { Ast.Statement.ImportDeclaration.kind; local; remote } =
  fuse
    [
      (let open Ast.Statement.ImportDeclaration in
      match kind with
      | Some ImportType -> fuse [Atom "type"; space]
      | Some ImportTypeof -> fuse [Atom "typeof"; space]
      | Some ImportValue
      | None ->
        Empty);
      identifier remote;
      (match local with
      | Some id -> fuse [space; Atom "as"; space; identifier id]
      | None -> Empty);
    ]

and import_named_specifiers named_specifiers =
  group
    [
      new_list
        ~wrap:(Atom "{", Atom "}")
        ~sep:(Atom ",")
        (Base.List.map ~f:import_named_specifier named_specifiers);
    ]

and import_declaration
    loc { Ast.Statement.ImportDeclaration.importKind; source; specifiers; default; comments } =
  let s_from = fuse [Atom "from"; pretty_space] in
  let module I = Ast.Statement.ImportDeclaration in
  layout_node_with_comments_opt loc comments
  @@ with_semicolon
       (fuse
          [
            Atom "import";
            begin
              match importKind with
              | I.ImportType -> fuse [space; Atom "type"]
              | I.ImportTypeof -> fuse [space; Atom "typeof"]
              | I.ImportValue -> Empty
            end;
            begin
              match (partition_specifiers default specifiers, importKind) with
              (* No export specifiers *)
              (* `import 'module-name';` *)
              | (([], None), I.ImportValue) -> pretty_space
              (* `import type {} from 'module-name';` *)
              | (([], None), (I.ImportType | I.ImportTypeof)) ->
                fuse [pretty_space; Atom "{}"; pretty_space; s_from]
              (* Only has named specifiers *)
              | (([], Some named), _) -> fuse [pretty_space; named; pretty_space; s_from]
              (* Only has default or namedspaced specifiers *)
              | ((special, None), _) ->
                fuse [space; fuse_list ~sep:(Atom ",") special; space; s_from]
              (* Has both default or namedspaced specifiers and named specifiers *)
              | ((special, Some named), _) ->
                fuse [space; fuse_list ~sep:(Atom ",") (special @ [named]); pretty_space; s_from]
            end;
            string_literal source;
          ])

and export_source ~prefix = function
  | Some lit -> fuse [prefix; Atom "from"; pretty_space; string_literal lit]
  | None -> Empty

and export_specifier source =
  let open Ast.Statement.ExportNamedDeclaration in
  function
  | ExportSpecifiers specifiers ->
    fuse
      [
        group
          [
            new_list
              ~wrap:(Atom "{", Atom "}")
              ~sep:(Atom ",")
              (List.map
                 (fun (loc, { ExportSpecifier.local; exported }) ->
                   source_location_with_comments
                     ( loc,
                       fuse
                         [
                           identifier local;
                           begin
                             match exported with
                             | Some export -> fuse [space; Atom "as"; space; identifier export]
                             | None -> Empty
                           end;
                         ] ))
                 specifiers);
          ];
        export_source ~prefix:pretty_space source;
      ]
  | ExportBatchSpecifier (loc, Some ident) ->
    fuse
      [
        source_location_with_comments
          (loc, fuse [Atom "*"; pretty_space; Atom "as"; space; identifier ident]);
        export_source ~prefix:space source;
      ]
  | ExportBatchSpecifier (loc, None) ->
    fuse [source_location_with_comments (loc, Atom "*"); export_source ~prefix:pretty_space source]

and export_declaration
    loc
    { Ast.Statement.ExportNamedDeclaration.declaration; specifiers; source; exportKind; comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         Atom "export";
         begin
           match (declaration, specifiers) with
           | (Some decl, None) -> fuse [space; statement decl]
           | (None, Some specifier) ->
             with_semicolon
               (fuse
                  [
                    begin
                      match exportKind with
                      | Ast.Statement.ExportType -> fuse [space; Atom "type"]
                      | Ast.Statement.ExportValue -> Empty
                    end;
                    pretty_space;
                    export_specifier source specifier;
                  ])
           | (_, _) -> failwith "Invalid export declaration"
         end;
       ]

and export_default_declaration
    loc { Ast.Statement.ExportDefaultDeclaration.default = _; declaration; comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         Atom "export";
         space;
         Atom "default";
         space;
         (let open Ast.Statement.ExportDefaultDeclaration in
         match declaration with
         | Declaration stat -> statement stat
         | Expression expr -> with_semicolon (expression expr));
       ]

and variance (loc, { Ast.Variance.kind; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      match kind with
      | Ast.Variance.Plus -> Atom "+"
      | Ast.Variance.Minus -> Atom "-" )

and switch_case ~last (loc, { Ast.Statement.Switch.Case.test; consequent; comments }) =
  let case_left =
    match test with
    | Some expr -> fuse_with_space [Atom "case"; fuse [expression expr; Atom ":"]]
    | None -> Atom "default:"
  in
  source_location_with_comments
    ?comments
    ( loc,
      match consequent with
      | [] -> case_left
      | _ ->
        let statements = statement_list ~pretty_semicolon:last consequent in
        fuse [case_left; Indent (fuse [pretty_hardline; fuse statements])] )

and type_param
    ( _,
      {
        Ast.Type.TypeParam.name = (loc, { Ast.Identifier.name; comments });
        bound;
        variance = variance_;
        default;
      } ) =
  fuse
    [
      option variance variance_;
      source_location_with_comments ?comments (loc, Atom name);
      hint type_annotation bound;
      begin
        match default with
        | Some t -> fuse [pretty_space; Atom "="; pretty_space; type_ t]
        | None -> Empty
      end;
    ]

and type_parameter (loc, { Ast.Type.TypeParams.params; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      group
        [new_list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (Base.List.map ~f:type_param params)]
    )

and call_args ?(lparen = "(") (loc, { Ast.Expression.ArgList.arguments; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      group
        [
          new_list
            ~wrap:(Atom lparen, Atom ")")
            ~sep:(Atom ",")
            (Base.List.map ~f:expression_or_spread arguments);
        ] )

and call_type_args (loc, { Ast.Expression.CallTypeArgs.arguments = args; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      group
        [new_list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (Base.List.map ~f:call_type_arg args)]
    )

and call_type_arg (x : (Loc.t, Loc.t) Ast.Expression.CallTypeArg.t) =
  let open Ast.Expression.CallTypeArg in
  match x with
  | Implicit (loc, { Implicit.comments }) -> layout_node_with_comments_opt loc comments (Atom "_")
  | Explicit t -> type_ t

and type_args (loc, { Ast.Type.TypeArgs.arguments; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      group [new_list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (Base.List.map ~f:type_ arguments)]
    )

and type_alias ~declare loc { Ast.Statement.TypeAlias.id; tparams; right; comments } =
  layout_node_with_comments_opt loc comments
  @@ with_semicolon
       (fuse
          [
            ( if declare then
              fuse [Atom "declare"; space]
            else
              Empty );
            Atom "type";
            space;
            identifier id;
            option type_parameter tparams;
            pretty_space;
            Atom "=";
            pretty_space;
            type_ right;
          ])

and opaque_type ~declare loc { Ast.Statement.OpaqueType.id; tparams; impltype; supertype; comments }
    =
  layout_node_with_comments_opt loc comments
  @@ with_semicolon
       (fuse
          ( [
              ( if declare then
                fuse [Atom "declare"; space]
              else
                Empty );
              Atom "opaque type";
              space;
              identifier id;
              option type_parameter tparams;
            ]
          @ (match supertype with
            | Some t -> [Atom ":"; pretty_space; type_ t]
            | None -> [])
          @
          match impltype with
          | Some impltype -> [pretty_space; Atom "="; pretty_space; type_ impltype]
          | None -> [] ))

and type_annotation ?(parens = false) (loc, t) =
  source_location_with_comments
    ( loc,
      fuse
        [
          Atom ":";
          pretty_space;
          ( if parens then
            wrap_in_parens (type_ t)
          else
            type_ t );
        ] )

and type_predicate (loc, { Ast.Type.Predicate.kind; comments }) =
  source_location_with_comments
    ?comments
    ( loc,
      fuse
        [
          Atom "%checks";
          (let open Ast.Type.Predicate in
          match kind with
          | Declared expr -> wrap_in_parens (expression expr)
          | Inferred -> Empty);
        ] )

and type_union_or_intersection ~sep ts =
  let sep = fuse [sep; pretty_space] in
  list
    ~inline:(false, true)
    (List.mapi
       (fun i t ->
         fuse
           [
             ( if i = 0 then
               IfBreak (sep, Empty)
             else
               sep );
             type_with_parens t;
           ])
       ts)

and type_function_param (loc, { Ast.Type.Function.Param.name; annot; optional }) =
  source_location_with_comments
    ( loc,
      fuse
        [
          begin
            match name with
            | Some id ->
              fuse
                [
                  identifier id;
                  ( if optional then
                    Atom "?"
                  else
                    Empty );
                  Atom ":";
                  pretty_space;
                ]
            | None -> Empty
          end;
          type_ annot;
        ] )

and type_function
    ~sep
    loc
    {
      Ast.Type.Function.params =
        ( params_loc,
          { Ast.Type.Function.Params.params; rest = restParams; comments = params_comments } );
      return;
      tparams;
      comments = func_comments;
    } =
  let params = Base.List.map ~f:type_function_param params in
  let params =
    match restParams with
    | Some (loc, { Ast.Type.Function.RestParam.argument; comments }) ->
      params
      @ [
          source_location_with_comments
            ?comments
            (loc, fuse [Atom "..."; type_function_param argument]);
        ]
    | None -> params
  in
  layout_node_with_comments_opt loc func_comments
  @@ fuse
       [
         option type_parameter tparams;
         layout_node_with_comments_opt params_loc params_comments
         @@ group
              [
                new_list (* Calls should not allow a trailing comma *)
                  ~trailing_sep:false
                  ~wrap:(Atom "(", Atom ")")
                  ~sep:(Atom ",")
                  params;
              ];
         sep;
         pretty_space;
         type_ return;
       ]

and type_object_property =
  let open Ast.Type.Object in
  function
  | Property
      ( loc,
        { Property.key; value; optional; static; proto; variance = variance_; _method; comments } )
    ->
    let s_static =
      if static then
        fuse [Atom "static"; space]
      else
        Empty
    in
    let s_proto =
      if proto then
        fuse [Atom "proto"; space]
      else
        Empty
    in
    source_location_with_comments
      ?comments
      ( loc,
        match (value, _method, proto, optional) with
        (* Functions with no special properties can be rendered as methods *)
        | (Property.Init (loc, Ast.Type.Function func), true, false, false) ->
          source_location_with_comments
            (loc, fuse [s_static; object_property_key key; type_function ~sep:(Atom ":") loc func])
        (* Normal properties *)
        | (Property.Init t, _, _, _) ->
          fuse
            [
              s_static;
              s_proto;
              option variance variance_;
              object_property_key key;
              ( if optional then
                Atom "?"
              else
                Empty );
              Atom ":";
              pretty_space;
              type_ t;
            ]
        (* Getters/Setters *)
        | (Property.Get (loc, func), _, _, _) ->
          source_location_with_comments
            ( loc,
              fuse
                [Atom "get"; space; object_property_key key; type_function ~sep:(Atom ":") loc func]
            )
        | (Property.Set (loc, func), _, _, _) ->
          source_location_with_comments
            ( loc,
              fuse
                [Atom "set"; space; object_property_key key; type_function ~sep:(Atom ":") loc func]
            ) )
  | SpreadProperty (loc, { SpreadProperty.argument; comments }) ->
    source_location_with_comments ?comments (loc, fuse [Atom "..."; type_ argument])
  | Indexer (loc, { Indexer.id; key; value; static; variance = variance_; comments }) ->
    source_location_with_comments
      ?comments
      ( loc,
        fuse
          [
            ( if static then
              fuse [Atom "static"; space]
            else
              Empty );
            option variance variance_;
            Atom "[";
            begin
              match id with
              | Some id -> fuse [identifier id; Atom ":"; pretty_space]
              | None -> Empty
            end;
            type_ key;
            Atom "]";
            Atom ":";
            pretty_space;
            type_ value;
          ] )
  | CallProperty (loc, { CallProperty.value = (call_loc, func); static; comments }) ->
    source_location_with_comments
      ?comments
      ( loc,
        fuse
          [
            ( if static then
              fuse [Atom "static"; space]
            else
              Empty );
            source_location_with_comments (call_loc, type_function ~sep:(Atom ":") call_loc func);
          ] )
  | InternalSlot (loc, { InternalSlot.id; value; optional; static; _method = _; comments }) ->
    source_location_with_comments
      ?comments
      ( loc,
        fuse
          [
            ( if static then
              fuse [Atom "static"; space]
            else
              Empty );
            Atom "[[";
            identifier id;
            Atom "]]";
            ( if optional then
              Atom "?"
            else
              Empty );
            Atom ":";
            pretty_space;
            type_ value;
          ] )

and type_object ?(sep = Atom ",") loc { Ast.Type.Object.exact; properties; inexact; comments } =
  let s_exact =
    if exact then
      Atom "|"
    else
      Empty
  in
  let sep_linebreak = pretty_line in
  let prop_loc =
    let open Ast.Type.Object in
    function
    | Property (loc, _)
    | SpreadProperty (loc, _)
    | Indexer (loc, _)
    | CallProperty (loc, _)
    | InternalSlot (loc, _) ->
      loc
  in
  let props =
    Base.List.map
      ~f:(fun property ->
        ( prop_loc property,
          Comment_attachment.object_type_property_comment_bounds property,
          type_object_property property ))
      properties
    |> list_with_newlines ~sep ~sep_linebreak
  in
  let props =
    if inexact then
      let sep =
        if props = [] then
          Empty
        else
          fuse [sep; sep_linebreak]
      in
      props @ [sep; Atom "..."]
    else
      props
  in
  (* Add trailing comma to last property *)
  let props =
    if props = [] then
      props
    else
      [fuse props; if_break sep Empty]
  in
  (* If first prop is on a different line then pretty print with line breaks *)
  let break =
    match properties with
    | [] -> None
    | first_prop :: _ ->
      if Loc.(loc.start.line < (prop_loc first_prop).start.line) then
        Some pretty_hardline
      else
        None
  in
  let props_layout =
    wrap_and_indent ?break (fuse [Atom "{"; s_exact], fuse [s_exact; Atom "}"]) props
  in
  layout_node_with_comments_opt loc comments @@ group [props_layout]

and type_interface loc { Ast.Type.Interface.extends; body = (obj_loc, obj); comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         Atom "interface";
         interface_extends extends;
         pretty_space;
         source_location_with_comments (obj_loc, type_object ~sep:(Atom ",") obj_loc obj);
       ]

and interface_extends = function
  | [] -> Empty
  | xs ->
    fuse
      [
        space;
        Atom "extends";
        space;
        fuse_list
          ~sep:(Atom ",")
          (List.map
             (fun (loc, generic) -> source_location_with_comments (loc, type_generic loc generic))
             xs);
      ]

and type_generic loc { Ast.Type.Generic.id; targs; comments } =
  let rec generic_identifier =
    let open Ast.Type.Generic.Identifier in
    function
    | Unqualified id -> identifier id
    | Qualified (loc, { qualification; id }) ->
      source_location_with_comments
        (loc, fuse [generic_identifier qualification; Atom "."; identifier id])
  in
  layout_node_with_comments_opt loc comments @@ fuse [generic_identifier id; option type_args targs]

and type_nullable loc { Ast.Type.Nullable.argument; comments } =
  layout_node_with_comments_opt loc comments (fuse [Atom "?"; type_with_parens argument])

and type_typeof loc { Ast.Type.Typeof.argument; internal = _; comments } =
  layout_node_with_comments_opt loc comments (fuse [Atom "typeof"; space; type_ argument])

and type_tuple loc { Ast.Type.Tuple.types; comments } =
  layout_node_with_comments_opt
    loc
    comments
    (group [new_list ~wrap:(Atom "[", Atom "]") ~sep:(Atom ",") (Base.List.map ~f:type_ types)])

and type_array loc { Ast.Type.Array.argument; comments } =
  layout_node_with_comments_opt loc comments (fuse [Atom "Array<"; type_ argument; Atom ">"])

and type_union loc { Ast.Type.Union.types = (t0, t1, ts); comments } =
  layout_node_with_comments_opt
    loc
    comments
    (type_union_or_intersection ~sep:(Atom "|") (t0 :: t1 :: ts))

and type_intersection loc { Ast.Type.Intersection.types = (t0, t1, ts); comments } =
  layout_node_with_comments_opt
    loc
    comments
    (type_union_or_intersection ~sep:(Atom "&") (t0 :: t1 :: ts))

and type_with_parens t =
  let module T = Ast.Type in
  match t with
  | (_, T.Function _)
  | (_, T.Union _)
  | (_, T.Intersection _) ->
    wrap_in_parens (type_ t)
  | _ -> type_ t

and type_ ((loc, t) : (Loc.t, Loc.t) Ast.Type.t) =
  let module T = Ast.Type in
  source_location_with_comments
    ( loc,
      match t with
      | T.Any comments -> layout_node_with_comments_opt loc comments (Atom "any")
      | T.Mixed comments -> layout_node_with_comments_opt loc comments (Atom "mixed")
      | T.Empty comments -> layout_node_with_comments_opt loc comments (Atom "empty")
      | T.Void comments -> layout_node_with_comments_opt loc comments (Atom "void")
      | T.Null comments -> layout_node_with_comments_opt loc comments (Atom "null")
      | T.Symbol comments -> layout_node_with_comments_opt loc comments (Atom "symbol")
      | T.Number comments -> layout_node_with_comments_opt loc comments (Atom "number")
      | T.BigInt comments -> layout_node_with_comments_opt loc comments (Atom "bigint")
      | T.String comments -> layout_node_with_comments_opt loc comments (Atom "string")
      | T.Boolean comments -> layout_node_with_comments_opt loc comments (Atom "boolean")
      | T.Nullable t -> type_nullable loc t
      | T.Function func -> type_function ~sep:(fuse [pretty_space; Atom "=>"]) loc func
      | T.Object obj -> type_object loc obj
      | T.Interface i -> type_interface loc i
      | T.Array t -> type_array loc t
      | T.Generic generic -> type_generic loc generic
      | T.Union t -> type_union loc t
      | T.Intersection t -> type_intersection loc t
      | T.Typeof t -> type_typeof loc t
      | T.Tuple t -> type_tuple loc t
      | T.StringLiteral lit -> string_literal_type loc lit
      | T.NumberLiteral lit -> number_literal_type loc lit
      | T.BigIntLiteral lit -> bigint_literal_type loc lit
      | T.BooleanLiteral lit -> boolean_literal_type loc lit
      | T.Exists comments -> layout_node_with_comments_opt loc comments (Atom "*") )

and interface_declaration_base
    ~def loc { Ast.Statement.Interface.id; tparams; body = (body_loc, obj); extends; comments } =
  layout_node_with_comments_opt loc comments
  @@ fuse
       [
         def;
         identifier id;
         option type_parameter tparams;
         interface_extends extends;
         pretty_space;
         source_location_with_comments (body_loc, type_object ~sep:(Atom ",") body_loc obj);
       ]

and interface_declaration loc interface =
  interface_declaration_base ~def:(fuse [Atom "interface"; space]) loc interface

and declare_interface loc interface =
  interface_declaration_base
    ~def:(fuse [Atom "declare"; space; Atom "interface"; space])
    loc
    interface

and declare_class
    ?(s_type = Empty)
    loc
    {
      Ast.Statement.DeclareClass.id;
      tparams;
      body = (body_loc, obj);
      extends;
      mixins;
      implements;
      comments;
    } =
  let class_parts =
    [
      Atom "declare";
      space;
      s_type;
      Atom "class";
      space;
      identifier id;
      option type_parameter tparams;
    ]
  in
  let extends_parts =
    let class_extends =
      [
        begin
          match extends with
          | Some (loc, generic) ->
            Some
              (fuse
                 [
                   Atom "extends";
                   space;
                   source_location_with_comments (loc, type_generic loc generic);
                 ])
          | None -> None
        end;
        begin
          match mixins with
          | [] -> None
          | xs ->
            Some
              (fuse
                 [
                   Atom "mixins";
                   space;
                   fuse_list
                     ~sep:(Atom ",")
                     (List.map
                        (fun (loc, generic) ->
                          source_location_with_comments (loc, type_generic loc generic))
                        xs);
                 ])
        end;
        class_implements implements;
      ]
    in
    match deoptionalize class_extends with
    | [] -> Empty
    | items -> Layout.Indent (fuse [line; join line items])
  in
  let body = source_location_with_comments (body_loc, type_object ~sep:(Atom ",") body_loc obj) in
  let parts =
    []
    |> List.rev_append class_parts
    |> List.cons extends_parts
    |> List.cons pretty_space
    |> List.cons body
    |> List.rev
  in
  source_location_with_comments ?comments (loc, group parts)

and declare_function
    ?(s_type = Empty)
    loc
    { Ast.Statement.DeclareFunction.id; annot = (annot_lot, t); predicate; comments } =
  layout_node_with_comments_opt loc comments
  @@ with_semicolon
       (fuse
          [
            Atom "declare";
            space;
            s_type;
            Atom "function";
            space;
            identifier id;
            source_location_with_comments
              ( annot_lot,
                match t with
                | (loc, Ast.Type.Function func) ->
                  source_location_with_comments (loc, type_function ~sep:(Atom ":") loc func)
                | _ -> failwith "Invalid DeclareFunction" );
            begin
              match predicate with
              | Some pred -> fuse [pretty_space; type_predicate pred]
              | None -> Empty
            end;
          ])

and declare_variable ?(s_type = Empty) loc { Ast.Statement.DeclareVariable.id; annot; comments } =
  layout_node_with_comments_opt loc comments
  @@ with_semicolon
       (fuse
          [
            Atom "declare";
            space;
            s_type;
            Atom "var";
            space;
            identifier id;
            hint type_annotation annot;
          ])

and declare_module_exports loc { Ast.Statement.DeclareModuleExports.annot; comments } =
  layout_node_with_comments_opt loc comments
  @@ with_semicolon (fuse [Atom "declare"; space; Atom "module.exports"; type_annotation annot])

and declare_module loc { Ast.Statement.DeclareModule.id; body; kind = _; comments } =
  source_location_with_comments
    ?comments
    ( loc,
      fuse
        [
          Atom "declare";
          space;
          Atom "module";
          space;
          begin
            match id with
            | Ast.Statement.DeclareModule.Identifier id -> identifier id
            | Ast.Statement.DeclareModule.Literal lit -> string_literal lit
          end;
          pretty_space;
          block body;
        ] )

and declare_export_declaration
    loc
    { Ast.Statement.DeclareExportDeclaration.default; declaration; specifiers; source; comments } =
  let s_export =
    fuse
      [
        Atom "export";
        space;
        ( if Base.Option.is_some default then
          fuse [Atom "default"; space]
        else
          Empty );
      ]
  in
  match (declaration, specifiers) with
  | (Some decl, None) ->
    let open Ast.Statement.DeclareExportDeclaration in
    (match decl with
    (* declare export var *)
    | Variable (loc, var) ->
      source_location_with_comments ?comments (loc, declare_variable ~s_type:s_export loc var)
    (* declare export function *)
    | Function (loc, func) ->
      source_location_with_comments ?comments (loc, declare_function ~s_type:s_export loc func)
    (* declare export class *)
    | Class (loc, c) ->
      source_location_with_comments ?comments (loc, declare_class ~s_type:s_export loc c)
    (* declare export default [type]
     * this corresponds to things like
     * export default 1+1; *)
    | DefaultType t ->
      source_location_with_comments
        ?comments
        (loc, with_semicolon (fuse [Atom "declare"; space; s_export; type_ t]))
    (* declare export type *)
    | NamedType (loc, typeAlias) ->
      source_location_with_comments
        ?comments
        (loc, fuse [Atom "declare"; space; s_export; type_alias ~declare:false loc typeAlias])
    (* declare export opaque type *)
    | NamedOpaqueType (loc, opaqueType) ->
      source_location_with_comments
        ?comments
        (loc, fuse [Atom "declare"; space; s_export; opaque_type ~declare:false loc opaqueType])
    (* declare export interface *)
    | Interface (loc, interface) ->
      source_location_with_comments
        ?comments
        (loc, fuse [Atom "declare"; space; s_export; interface_declaration loc interface]))
  | (None, Some specifier) ->
    source_location_with_comments
      ?comments
      ( loc,
        fuse [Atom "declare"; space; Atom "export"; pretty_space; export_specifier source specifier]
      )
  | (_, _) -> failwith "Invalid declare export declaration"
