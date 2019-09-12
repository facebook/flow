(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Layout
module LocMap = Loc_collections.LocMap

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

type comment_attach =
  | Preceding
  | Enclosing
  | Following

type comment_map =
  (comment_attach * (Loc.t, Loc.t) Ast.Statement.t * Loc.t Ast.Comment.t) list LocMap.t

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
let max_precedence = 20

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
  | (_, E.Super)
  | (_, E.TemplateLiteral _)
  | (_, E.This) ->
    max_precedence
  (* Expressions involving operators *)
  | (_, E.Member _)
  | (_, E.OptionalMember _)
  | (_, E.MetaProperty _)
  | (_, E.New _) ->
    19
  | (_, E.Call _)
  | (_, E.OptionalCall _)
  | (_, E.TaggedTemplate _)
  | (_, E.Import _) ->
    18
  | (_, E.Update { E.Update.prefix = false; _ }) -> 17
  | (_, E.Update { E.Update.prefix = true; _ }) -> 16
  | (_, E.Unary _) -> 16
  | (_, E.Binary { E.Binary.operator; _ }) ->
    begin
      match operator with
      | E.Binary.Exp -> 15
      | E.Binary.Mult -> 14
      | E.Binary.Div -> 14
      | E.Binary.Mod -> 14
      | E.Binary.Plus -> 13
      | E.Binary.Minus -> 13
      | E.Binary.LShift -> 12
      | E.Binary.RShift -> 12
      | E.Binary.RShift3 -> 12
      | E.Binary.LessThan -> 11
      | E.Binary.LessThanEqual -> 11
      | E.Binary.GreaterThan -> 11
      | E.Binary.GreaterThanEqual -> 11
      | E.Binary.In -> 11
      | E.Binary.Instanceof -> 11
      | E.Binary.Equal -> 10
      | E.Binary.NotEqual -> 10
      | E.Binary.StrictEqual -> 10
      | E.Binary.StrictNotEqual -> 10
      | E.Binary.BitAnd -> 9
      | E.Binary.Xor -> 8
      | E.Binary.BitOr -> 7
    end
  | (_, E.Logical { E.Logical.operator = E.Logical.And; _ }) -> 6
  | (_, E.Logical { E.Logical.operator = E.Logical.Or; _ }) -> 5
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
      str
      |> Wtf8.fold_wtf_8 ?pos ?len (lookahead ~f) (None, acc)
      |> fun (last, acc) ->
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

let layout_from_comment anchor loc_node (loc_cm, comment) =
  Ast.Comment.(
    let comment_text =
      match comment with
      | Line txt -> Printf.sprintf "//%s\n" txt
      | Block txt ->
        (match (Loc.lines_intersect loc_node loc_cm, anchor) with
        | (false, Preceding) -> Printf.sprintf "\n/*%s*/" txt
        | (false, Following) -> Printf.sprintf "/*%s*/\n" txt
        | (false, Enclosing) -> Printf.sprintf "/*%s*/\n" txt
        | _ -> Printf.sprintf "/*%s*/" txt)
    in
    SourceLocation (loc_cm, Atom comment_text))

let with_attached_comments : comment_map option ref = ref None

let layout_node_with_comments current_loc layout_node =
  Layout.(
    match !with_attached_comments with
    | None -> layout_node
    | Some attached when LocMap.is_empty attached -> layout_node
    | Some attached ->
      (match LocMap.find_opt current_loc attached with
      | None
      | Some [] ->
        layout_node
      | Some comments ->
        with_attached_comments := Some (LocMap.remove current_loc attached);
        let matched =
          List.fold_left
            (fun nodes comment ->
              let (anchor, (loc_st, _), comment) = comment in
              match anchor with
              | Preceding -> nodes @ [layout_from_comment anchor loc_st comment]
              | Following -> [layout_from_comment anchor loc_st comment] @ nodes
              | Enclosing ->
                (* TODO(festevezga)(T29896911) print enclosing comments using full statement *)
                [layout_from_comment anchor loc_st comment] @ nodes)
            [layout_node]
            comments
        in
        Concat matched))

let layout_node_with_simple_comments current_loc comments layout_node =
  let { Ast.Syntax.leading; trailing; _ } = comments in
  let preceding = List.map (layout_from_comment Preceding current_loc) leading in
  let following = List.map (layout_from_comment Following current_loc) trailing in
  Concat (preceding @ [layout_node] @ following)

let layout_node_with_simple_comments_opt current_loc comments layout_node =
  match comments with
  | Some c -> layout_node_with_simple_comments current_loc c layout_node
  | None -> layout_node

let source_location_with_comments ?comments (current_loc, layout_node) =
  match comments with
  | Some comments ->
    layout_node_with_simple_comments
      current_loc
      comments
      (SourceLocation (current_loc, layout_node))
  | None -> layout_node_with_comments current_loc (SourceLocation (current_loc, layout_node))

let identifier_with_comments (current_loc, { Ast.Identifier.name; comments }) =
  let node = Identifier (current_loc, name) in
  match comments with
  | Some comments -> layout_node_with_simple_comments current_loc comments node
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
      combine_directives_and_comments directives comments :: statement_list statements
    else
      statement_list statements
  in
  let nodes = group [join pretty_hardline nodes] in
  let nodes = maybe_embed_checksum nodes checksum in
  let loc = { loc with Loc.start = { Loc.line = 1; column = 0 } } in
  source_location_with_comments (loc, nodes)

and program_simple (loc, statements, _) =
  let nodes = group [join pretty_hardline (statement_list statements)] in
  let loc = { loc with Loc.start = { Loc.line = 1; column = 0 } } in
  source_location_with_comments (loc, nodes)

and combine_directives_and_comments directives comments : Layout.layout_node =
  let directives = Core_list.map ~f:(fun ((loc, _) as x) -> (loc, Statement x)) directives in
  let comments = Core_list.map ~f:(fun ((loc, _) as x) -> (loc, Comment x)) comments in
  let merged = List.merge (fun (a, _) (b, _) -> Loc.compare a b) directives comments in
  let nodes =
    Core_list.map
      ~f:(function
        | (loc, Statement s) -> (loc, statement s)
        | (loc, Comment c) -> (loc, comment c))
      merged
  in
  join pretty_hardline (list_with_newlines nodes)

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
      | S.Empty -> Atom ";"
      | S.Debugger -> with_semicolon (Atom "debugger")
      | S.Block b -> block (loc, b)
      | S.Expression { S.Expression.expression = expr; _ } ->
        let ctxt = { normal_context with left = In_expression_statement } in
        with_semicolon (expression_with_parens ~precedence:0 ~ctxt expr)
      | S.If { S.If.test; consequent; alternate; comments } ->
        layout_node_with_simple_comments_opt
          loc
          comments
          begin
            match alternate with
            | Some alt ->
              fuse
                [
                  group
                    [statement_with_test "if" (expression test); statement_after_test consequent];
                  pretty_space;
                  fuse_with_space [Atom "else"; statement ~pretty_semicolon alt];
                ]
            | None ->
              group
                [
                  statement_with_test "if" (expression test);
                  statement_after_test ~pretty_semicolon consequent;
                ]
          end
      | S.Labeled { S.Labeled.label; body } ->
        fuse [identifier label; Atom ":"; pretty_space; statement body]
      | S.Break { S.Break.label; comments } ->
        let s_break = Atom "break" in
        with_semicolon
        @@ layout_node_with_simple_comments_opt
             loc
             comments
             (match label with
             | Some l -> fuse [s_break; space; identifier l]
             | None -> s_break)
      | S.Continue { S.Continue.label; comments } ->
        let s_continue = Atom "continue" in
        with_semicolon
        @@ layout_node_with_simple_comments_opt
             loc
             comments
             (match label with
             | Some l -> fuse [s_continue; space; identifier l]
             | None -> s_continue)
      | S.With { S.With._object; body } ->
        fuse [statement_with_test "with" (expression _object); statement_after_test body]
      | S.Switch { S.Switch.discriminant; cases } ->
        let case_nodes =
          let rec helper acc = function
            | [] -> List.rev acc
            | [case] -> List.rev (switch_case ~last:true case :: acc)
            | case :: next :: rest ->
              let case_node = switch_case ~last:false case in
              let next_node = switch_case ~last:(rest = []) next in
              let case_node =
                let (Loc.{ _end = { line = case_end; _ }; _ }, _) = case in
                let (Loc.{ start = { line = next_start; _ }; _ }, _) = next in
                if case_end + 1 < next_start then
                  fuse [case_node; pretty_hardline]
                else
                  case_node
              in
              helper (next_node :: case_node :: acc) rest
          in
          helper [] cases
        in
        let cases_node =
          wrap_and_indent
            ~break:pretty_hardline
            (Atom "{", Atom "}")
            [join pretty_hardline case_nodes]
        in
        fuse [statement_with_test "switch" (expression discriminant); pretty_space; cases_node]
      | S.Return { S.Return.argument; comments } ->
        let s_return = Atom "return" in
        with_semicolon
        @@ layout_node_with_simple_comments_opt
             loc
             comments
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
      | S.Throw { S.Throw.argument } ->
        with_semicolon
          (fuse_with_space [Atom "throw"; group [wrap_in_parens_on_break (expression argument)]])
      | S.Try { S.Try.block = b; handler; finalizer; comments } ->
        layout_node_with_simple_comments_opt
          loc
          comments
          (fuse
             [
               Atom "try";
               pretty_space;
               block b;
               (match handler with
               | Some (loc, { S.Try.CatchClause.param; body }) ->
                 source_location_with_comments
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
      | S.While { S.While.test; body } ->
        fuse
          [
            statement_with_test "while" (expression test);
            statement_after_test ~pretty_semicolon body;
          ]
      | S.DoWhile { S.DoWhile.body; test; comments } ->
        with_semicolon
        @@ layout_node_with_simple_comments_opt
             loc
             comments
             (fuse
                [
                  fuse_with_space [Atom "do"; statement body];
                  pretty_space;
                  Atom "while";
                  pretty_space;
                  group [wrap_and_indent (Atom "(", Atom ")") [expression test]];
                ])
      | S.For { S.For.init; test; update; body } ->
        fuse
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
      | S.ForIn { S.ForIn.left; right; body; each } ->
        fuse
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
      | S.FunctionDeclaration func -> function_ func
      | S.VariableDeclaration decl -> with_semicolon (variable_declaration (loc, decl))
      | S.ClassDeclaration class_ -> class_base class_
      | S.EnumDeclaration enum -> enum_declaration enum
      | S.ForOf { S.ForOf.left; right; body; async } ->
        fuse
          [
            Atom "for";
            ( if async then
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
      | S.ImportDeclaration import -> import_declaration import
      | S.ExportNamedDeclaration export -> export_declaration export
      | S.ExportDefaultDeclaration export -> export_default_declaration export
      | S.TypeAlias typeAlias -> type_alias ~declare:false typeAlias
      | S.OpaqueType opaqueType -> opaque_type ~declare:false opaqueType
      | S.InterfaceDeclaration interface -> interface_declaration interface
      | S.DeclareClass interface -> declare_class interface
      | S.DeclareFunction func -> declare_function func
      | S.DeclareInterface interface -> declare_interface interface
      | S.DeclareVariable var -> declare_variable var
      | S.DeclareModuleExports annot -> declare_module_exports annot
      | S.DeclareModule m -> declare_module m
      | S.DeclareTypeAlias typeAlias -> type_alias ~declare:true typeAlias
      | S.DeclareOpaqueType opaqueType -> opaque_type ~declare:true opaqueType
      | S.DeclareExportDeclaration export -> declare_export_declaration export )

(* The beginning of a statement that does a "test", like `if (test)` or `while (test)` *)
and statement_with_test name test =
  fuse [Atom name; pretty_space; group [wrap_and_indent (Atom "(", Atom ")") [test]]]

(* A statement following a "test", like the `statement` in `if (expr) statement` or
   `for (...) statement`. Better names for this are welcome! *)
and statement_after_test ?pretty_semicolon = function
  | (_, Ast.Statement.Empty) as stmt -> statement ?pretty_semicolon stmt
  | (_, Ast.Statement.Block _) as stmt -> fuse [pretty_space; statement ?pretty_semicolon stmt]
  | stmt -> Indent (fuse [pretty_line; statement ?pretty_semicolon stmt])

and expression ?(ctxt = normal_context) (root_expr : (Loc.t, Loc.t) Ast.Expression.t) =
  let (loc, expr) = root_expr in
  let module E = Ast.Expression in
  let precedence = precedence_of_expression (loc, expr) in
  source_location_with_comments
    ( loc,
      match expr with
      | E.This -> Atom "this"
      | E.Super -> Atom "super"
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
        layout_node_with_simple_comments_opt loc comments
        @@ group
             [
               new_list
                 ~wrap:(Atom "[", Atom "]")
                 ~sep:(Atom ",")
                 ~trailing_sep
                 (List.rev rev_elements);
             ]
      | E.Object { E.Object.properties; comments } ->
        layout_node_with_simple_comments_opt loc comments
        @@ group
             [
               new_list
                 ~wrap:(Atom "{", Atom "}")
                 ~sep:(Atom ",")
                 ~wrap_spaces:true
                 (object_properties_with_newlines properties);
             ]
      | E.Sequence { E.Sequence.expressions } ->
        (* to get an AST like `x, (y, z)`, then there must've been parens
         around the right side. we can force that by bumping the minimum
         precedence. *)
        let precedence = precedence + 1 in
        let layouts = Core_list.map ~f:(expression_with_parens ~precedence ~ctxt) expressions in
        group [join (fuse [Atom ","; pretty_line]) layouts]
      | E.Identifier ident -> identifier ident
      | E.Literal lit -> literal lit
      | E.Function func -> function_ func
      | E.ArrowFunction func -> arrow_function ~ctxt ~precedence func
      | E.Assignment { E.Assignment.operator; left; right } ->
        fuse
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
      | E.Binary { E.Binary.operator; left; right } ->
        let module B = E.Binary in
        fuse_with_space
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
      | E.Call c -> call ~precedence ~ctxt c
      | E.OptionalCall { E.OptionalCall.call = c; optional } -> call ~optional ~precedence ~ctxt c
      | E.Conditional { E.Conditional.test; consequent; alternate } ->
        let test_layout =
          (* increase precedence since conditionals are right-associative *)
          expression_with_parens ~precedence:(precedence + 1) ~ctxt test
        in
        group
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
      | E.Logical { E.Logical.operator; left; right } ->
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
        Group [left; pretty_space; operator; Indent (fuse [pretty_line; right])]
      | E.Member m -> member ~precedence ~ctxt m
      | E.OptionalMember { E.OptionalMember.member = m; optional } ->
        member ~optional ~precedence ~ctxt m
      | E.New { E.New.callee; targs; arguments } ->
        let callee_layout =
          if definitely_needs_parens ~precedence ctxt callee || contains_call_expression callee
          then
            wrap_in_parens (expression ~ctxt callee)
          else
            expression ~ctxt callee
        in
        group
          [
            fuse_with_space [Atom "new"; callee_layout];
            option type_parameter_instantiation_with_implicit targs;
            new_list
              ~wrap:(Atom "(", Atom ")")
              ~sep:(Atom ",")
              (Core_list.map ~f:expression_or_spread arguments);
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
        layout_node_with_simple_comments_opt loc comments
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
      | E.Update { E.Update.operator; prefix; argument } ->
        let s_operator =
          match operator with
          | E.Update.Increment -> Atom "++"
          | E.Update.Decrement -> Atom "--"
        in
        (* we never need to wrap `argument` in parens because it must be a valid
         left-hand side expression *)
        if prefix then
          fuse [s_operator; expression ~ctxt argument]
        else
          fuse [expression ~ctxt argument; s_operator]
      | E.Class class_ -> class_base class_
      | E.Yield { E.Yield.argument; delegate; comments } ->
        layout_node_with_simple_comments_opt loc comments
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
      | E.MetaProperty { E.MetaProperty.meta; property } ->
        fuse [identifier meta; Atom "."; identifier property]
      | E.TaggedTemplate { E.TaggedTemplate.tag; quasi = (loc, template) } ->
        let ctxt = { normal_context with left = In_tagged_template } in
        fuse
          [
            expression_with_parens ~precedence ~ctxt tag;
            source_location_with_comments (loc, template_literal template);
          ]
      | E.TemplateLiteral template -> template_literal template
      | E.JSXElement el -> jsx_element loc el
      | E.JSXFragment fr -> jsx_fragment loc fr
      | E.TypeCast { E.TypeCast.expression = expr; annot } ->
        wrap_in_parens (fuse [expression expr; type_annotation annot])
      | E.Import expr -> fuse [Atom "import"; wrap_in_parens (expression expr)]
      (* Not supported *)
      | E.Comprehension _
      | E.Generator _ ->
        not_supported loc "Comprehension not supported" )

and call ?(optional = false) ~precedence ~ctxt call_node =
  let { Ast.Expression.Call.callee; targs; arguments } = call_node in
  match (callee, targs, arguments) with
  (* __d hack, force parens around factory function.
    More details at: https://fburl.com/b1wv51vj
    TODO: This is FB only, find generic way to add logic *)
  | ( (_, Ast.Expression.Identifier (_, { Ast.Identifier.name = "__d"; comments = _ })),
      None,
      [a; b; c; d] ) ->
    let lparen =
      if optional then
        ".?("
      else
        "("
    in
    group
      [
        Atom "__d";
        new_list
          ~wrap:(Atom lparen, Atom ")")
          ~sep:(Atom ",")
          [
            expression_or_spread a;
            expression_or_spread b;
            wrap_in_parens (expression_or_spread c);
            expression_or_spread d;
          ];
      ]
  (* Standard call expression printing *)
  | _ ->
    let (targs, lparen) =
      match targs with
      | None ->
        let lparen =
          if optional then
            ".?("
          else
            "("
        in
        (Empty, lparen)
      | Some (loc, args) ->
        let less_than =
          if optional then
            "?.<"
          else
            "<"
        in
        ( source_location_with_comments
            ( loc,
              group
                [
                  new_list
                    ~wrap:(Atom less_than, Atom ">")
                    ~sep:(Atom ",")
                    (Core_list.map ~f:explicit_or_implicit args);
                ] ),
          "(" )
    in
    fuse
      [
        expression_with_parens ~precedence ~ctxt callee;
        targs;
        group
          [
            new_list
              ~wrap:(Atom lparen, Atom ")")
              ~sep:(Atom ",")
              (Core_list.map ~f:expression_or_spread arguments);
          ];
      ]

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
  | Ast.Expression.Spread (loc, { Ast.Expression.SpreadElement.argument }) ->
    source_location_with_comments
      (loc, fuse [Atom "..."; expression_with_parens ~precedence ~ctxt argument])

and identifier (loc, name) = identifier_with_comments (loc, name)

and number_literal_type { Ast.NumberLiteral.raw; _ } = Atom raw

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

and literal { Ast.Literal.raw; value; comments = _ (* handled by caller *) } =
  Ast.Literal.(
    match value with
    | Number num -> number_literal ~in_member_object:false raw num
    | String str ->
      let quote = better_quote str in
      fuse [Atom quote; Atom (utf8_escape ~quote str); Atom quote]
    | RegExp { RegExp.pattern; flags } ->
      let flags =
        flags |> String_utils.to_list |> List.sort Char.compare |> String_utils.of_list
      in
      fuse [Atom "/"; Atom pattern; Atom "/"; Atom flags]
    | _ -> Atom raw)

and string_literal_type { Ast.StringLiteral.raw; _ } = Atom raw

and member ?(optional = false) ~precedence ~ctxt member_node =
  let { Ast.Expression.Member._object; property } = member_node in
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
  fuse
    [
      begin
        match _object with
        | (_, Ast.Expression.Call _) -> expression ~ctxt _object
        | ( loc,
            Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Number num; raw; comments } )
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
        | Ast.Expression.Member.PropertyIdentifier (loc, { Ast.Identifier.name = id; comments = _ })
          ->
          source_location_with_comments (loc, Atom id)
        | Ast.Expression.Member.PropertyPrivateName
            (loc, (_, { Ast.Identifier.name = id; comments = _ })) ->
          source_location_with_comments (loc, Atom ("#" ^ id))
        | Ast.Expression.Member.PropertyExpression expr -> expression ~ctxt expr
      end;
      rdelim;
    ]

and string_literal (loc, { Ast.StringLiteral.value; _ }) =
  let quote = better_quote value in
  source_location_with_comments
    (loc, fuse [Atom quote; Atom (utf8_escape ~quote value); Atom quote])

and pattern_object_property_key =
  Ast.Pattern.Object.(
    function
    | Property.Literal (loc, lit) -> source_location_with_comments (loc, literal lit)
    | Property.Identifier ident -> identifier ident
    | Property.Computed expr ->
      fuse [Atom "["; Sequence ({ seq with break = Break_if_needed }, [expression expr]); Atom "]"])

and pattern ?(ctxt = normal_context) ((loc, pat) : (Loc.t, Loc.t) Ast.Pattern.t) =
  let module P = Ast.Pattern in
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
                   | P.Object.RestProperty (loc, { P.Object.RestProperty.argument }) ->
                     source_location_with_comments (loc, fuse [Atom "..."; pattern argument]))
                 properties);
            hint type_annotation annot;
          ]
      | P.Array { P.Array.elements; annot; comments } ->
        group
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
                   | Some (P.Array.RestElement (loc, { P.Array.RestElement.argument })) ->
                     source_location_with_comments
                       ?comments
                       (loc, fuse [Atom "..."; pattern argument]))
                 elements);
            hint type_annotation annot;
          ]
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

and template_literal { Ast.Expression.TemplateLiteral.quasis; expressions } =
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
    ?(ctxt = normal_context) (loc, { Ast.Statement.VariableDeclaration.declarations; kind }) =
  let kind_layout =
    match kind with
    | Ast.Statement.VariableDeclaration.Var -> Atom "var"
    | Ast.Statement.VariableDeclaration.Let -> Atom "let"
    | Ast.Statement.VariableDeclaration.Const -> Atom "const"
  in
  let has_init =
    List.exists
      (fun var ->
        Ast.Statement.VariableDeclaration.Declarator.(
          match var with
          | (_, { id = _; init = Some _ }) -> true
          | _ -> false))
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
      let tl = Core_list.map ~f:(variable_declarator ~ctxt) tl in
      group [hd; Atom ","; Indent (fuse [sep; join (fuse [Atom ","; sep]) tl])]
  in
  source_location_with_comments (loc, fuse_with_space [kind_layout; decls_layout])

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
    {
      Ast.Function.params;
      body;
      async;
      predicate;
      return;
      tparams;
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
        } ) ->
      true
    | _ -> false
  in
  let params_and_stuff =
    match (is_single_simple_param, return, predicate, tparams) with
    | (true, Ast.Type.Missing _, None, None) -> List.hd (function_params ~ctxt params)
    | (_, _, _, _) ->
      fuse
        [
          option type_parameter tparams;
          arrow_function_params params;
          function_return return predicate;
        ]
  in
  fuse
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
          | Ast.Type.Available (_, (_, Ast.Type.Exists)) -> space
          | _ -> pretty_space
        end;
      Atom "=>";
      pretty_space;
      begin
        match body with
        | Ast.Function.BodyBlock b -> block b
        | Ast.Function.BodyExpression expr ->
          let ctxt = { normal_context with group = In_arrow_func } in
          expression_with_parens ~precedence ~ctxt expr
      end;
    ]

and arrow_function_params params =
  group
    [
      new_list
        ~wrap:(Atom "(", Atom ")")
        ~sep:(Atom ",")
        (function_params ~ctxt:normal_context params);
    ]

and function_ func =
  let { Ast.Function.id; params; body; async; generator; predicate; return; tparams; sig_loc = _ }
      =
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
  function_base ~prefix ~params ~body ~predicate ~return ~tparams

and function_base ~prefix ~params ~body ~predicate ~return ~tparams =
  fuse
    [
      prefix;
      option type_parameter tparams;
      list ~wrap:(Atom "(", Atom ")") ~sep:(Atom ",") (function_params ~ctxt:normal_context params);
      function_return return predicate;
      pretty_space;
      begin
        match body with
        | Ast.Function.BodyBlock b -> block b
        | Ast.Function.BodyExpression _ -> failwith "Only arrows should have BodyExpressions"
      end;
    ]

and function_params ~ctxt (_, { Ast.Function.Params.params; rest }) =
  let s_params =
    Core_list.map
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
  | Some (loc, { Ast.Function.RestParam.argument }) ->
    let s_rest = source_location_with_comments (loc, fuse [Atom "..."; pattern ~ctxt argument]) in
    List.append s_params [s_rest]
  | None -> s_params

and function_return return predicate =
  match (return, predicate) with
  | (Ast.Type.Missing _, None) -> Empty
  | (Ast.Type.Missing _, Some pred) -> fuse [Atom ":"; pretty_space; type_predicate pred]
  | (Ast.Type.Available ret, Some pred) ->
    fuse [type_annotation ret; pretty_space; type_predicate pred]
  | (Ast.Type.Available ret, None) -> type_annotation ret

and block (loc, { Ast.Statement.Block.body }) =
  let statements = statement_list ~pretty_semicolon:true body in
  source_location_with_comments
    ( loc,
      if statements <> [] then
        group
          [
            wrap_and_indent
              ~break:pretty_hardline
              (Atom "{", Atom "}")
              [join pretty_hardline statements];
          ]
      else
        Atom "{}" )

and decorators_list decorators =
  if List.length decorators > 0 then
    let decorators =
      List.map
        (fun (_, { Ast.Class.Decorator.expression = expr }) ->
          fuse
            [
              Atom "@";
              begin
                (* Magic number, after `Call` but before `Update` *)
                let precedence = 18 in
                expression_with_parens ~precedence ~ctxt:normal_context expr
              end;
            ])
        decorators
    in
    group [join pretty_line decorators; if_pretty hardline space]
  else
    Empty

and class_method (loc, { Ast.Class.Method.kind; key; value = (func_loc, func); static; decorators })
    =
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
  } =
    func
  in
  source_location_with_comments
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
            (func_loc, function_base ~prefix ~params ~body ~predicate ~return ~tparams);
        ] )

and class_property_helper loc key value static annot variance_ =
  source_location_with_comments
    ( loc,
      with_semicolon
        (fuse
           [
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

and class_property (loc, { Ast.Class.Property.key; value; static; annot; variance }) =
  class_property_helper loc (object_property_key key) value static annot variance

and class_private_field
    ( loc,
      {
        Ast.Class.PrivateField.key = (ident_loc, (_, { Ast.Identifier.name; comments = _ }));
        value;
        static;
        annot;
        variance;
      } ) =
  class_property_helper
    loc
    (identifier (Flow_ast_utils.ident_of_source (ident_loc, "#" ^ name)))
    value
    static
    annot
    variance

and class_body (loc, { Ast.Class.Body.body }) =
  if body <> [] then
    source_location_with_comments
      ( loc,
        group
          [
            wrap_and_indent
              ~break:pretty_hardline
              (Atom "{", Atom "}")
              [
                join
                  pretty_hardline
                  (Core_list.map
                     ~f:(function
                       | Ast.Class.Body.Method meth -> class_method meth
                       | Ast.Class.Body.Property prop -> class_property prop
                       | Ast.Class.Body.PrivateField field -> class_private_field field)
                     body);
              ];
          ] )
  else
    Atom "{}"

and class_implements implements =
  match implements with
  | [] -> None
  | _ ->
    Some
      (fuse
         [
           Atom "implements";
           space;
           fuse_list
             ~sep:(Atom ",")
             (List.map
                (fun (loc, { Ast.Class.Implements.id; targs }) ->
                  source_location_with_comments
                    (loc, fuse [identifier id; option type_parameter_instantiation targs]))
                implements);
         ])

and class_base { Ast.Class.id; body; tparams; extends; implements; classDecorators } =
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
          | Some (loc, { Ast.Class.Extends.expr; targs }) ->
            Some
              (fuse
                 [
                   Atom "extends";
                   space;
                   source_location_with_comments
                     (loc, fuse [expression expr; option type_parameter_instantiation targs]);
                 ])
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
  group [decorator_parts; group parts]

and enum_declaration { Ast.Statement.EnumDeclaration.id; body } =
  Ast.Statement.EnumDeclaration.(
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
    let initialized_member id value_str =
      fuse [identifier id; pretty_space; Atom "="; pretty_space; Atom value_str; Atom ","]
    in
    let boolean_member (_, { InitializedMember.id; init = (_, init_value) }) =
      initialized_member
        id
        ( if init_value then
          "true"
        else
          "false" )
    in
    let number_member (_, { InitializedMember.id; init = (_, { Ast.NumberLiteral.raw; _ }) }) =
      initialized_member id raw
    in
    let string_member (_, { InitializedMember.id; init = (_, { Ast.StringLiteral.raw; _ }) }) =
      initialized_member id raw
    in
    let body =
      match body with
      | BooleanBody { BooleanBody.members; explicitType } ->
        fuse
          [
            representation_type "boolean" explicitType;
            pretty_space;
            wrap_body @@ Core_list.map ~f:boolean_member members;
          ]
      | NumberBody { NumberBody.members; explicitType } ->
        fuse
          [
            representation_type "number" explicitType;
            pretty_space;
            wrap_body @@ Core_list.map ~f:number_member members;
          ]
      | StringBody { StringBody.members; explicitType } ->
        fuse
          [
            representation_type "string" explicitType;
            pretty_space;
            ( wrap_body
            @@
            match members with
            | StringBody.Defaulted members -> Core_list.map ~f:defaulted_member members
            | StringBody.Initialized members -> Core_list.map ~f:string_member members );
          ]
      | SymbolBody { SymbolBody.members } ->
        fuse
          [
            representation_type "symbol" true;
            pretty_space;
            wrap_body @@ Core_list.map ~f:defaulted_member members;
          ]
    in
    fuse [Atom "enum"; space; identifier id; body])

(* given a list of (loc * layout node) pairs, insert newlines between the nodes when necessary *)
and list_with_newlines (nodes : (Loc.t * Layout.layout_node) list) =
  let (nodes, _) =
    List.fold_left
      (fun (acc, last_loc) (loc, node) ->
        Loc.(
          let acc =
            match (last_loc, node) with
            (* empty line, don't add anything *)
            | (_, Empty) -> acc
            (* Lines are offset by more than one, let's add a line break *)
            | (Some { Loc._end; _ }, node) when _end.line + 1 < loc.start.line ->
              fuse [pretty_hardline; node] :: acc
            (* Hasn't matched, just add the node *)
            | (_, node) -> node :: acc
          in
          (acc, Some loc)))
      ([], None)
      nodes
  in
  List.rev nodes

and statement_list ?(pretty_semicolon = false) statements =
  let rec mapper acc = function
    | [] -> List.rev acc
    | ((loc, _) as stmt) :: rest ->
      let pretty_semicolon = pretty_semicolon && rest = [] in
      let acc = (loc, statement ~pretty_semicolon stmt) :: acc in
      (mapper [@tailcall]) acc rest
  in
  mapper [] statements |> list_with_newlines

and object_properties_with_newlines properties =
  let module E = Ast.Expression in
  let module O = E.Object in
  let rec has_function_decl = function
    | O.Property (_, O.Property.Init { value = v; _ }) ->
      begin
        match v with
        | (_, E.Function _)
        | (_, E.ArrowFunction _) ->
          true
        | (_, E.Object { O.properties; comments = _ }) -> List.exists has_function_decl properties
        | _ -> false
      end
    | O.Property (_, O.Property.Get _)
    | O.Property (_, O.Property.Set _) ->
      true
    | _ -> false
  in
  let (property_labels, _) =
    List.fold_left
      (fun (acc, last_p) p ->
        match (last_p, p) with
        | (None, _) ->
          (* Never on first line *)
          (object_property p :: acc, Some (has_function_decl p))
        | (Some true, p) ->
          (fuse [pretty_hardline; object_property p] :: acc, Some (has_function_decl p))
        | (_, p) when has_function_decl p ->
          (fuse [pretty_hardline; object_property p] :: acc, Some true)
        | _ -> (object_property p :: acc, Some false))
      ([], None)
      properties
  in
  List.rev property_labels

and object_property_key key =
  let module O = Ast.Expression.Object in
  match key with
  | O.Property.Literal (loc, lit) -> source_location_with_comments (loc, literal lit)
  | O.Property.Identifier ident -> identifier ident
  | O.Property.Computed expr ->
    fuse [Atom "["; Layout.Indent (fuse [pretty_line; expression expr]); pretty_line; Atom "]"]
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
          (fn_loc, function_base ~prefix ~params ~body ~predicate ~return ~tparams) )
  | O.Property (loc, O.Property.Get { key; value = (fn_loc, func) }) ->
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
      ( loc,
        source_location_with_comments
          (fn_loc, function_base ~prefix ~params ~body ~predicate ~return ~tparams) )
  | O.Property (loc, O.Property.Set { key; value = (fn_loc, func) }) ->
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
      ( loc,
        source_location_with_comments
          (fn_loc, function_base ~prefix ~params ~body ~predicate ~return ~tparams) )
  | O.SpreadProperty (loc, { O.SpreadProperty.argument }) ->
    source_location_with_comments (loc, fuse [Atom "..."; expression argument])

and jsx_element loc { Ast.JSX.openingElement; closingElement; children } =
  fuse
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

and jsx_fragment loc { Ast.JSX.frag_openingElement; frag_closingElement; frag_children } =
  fuse
    [
      jsx_fragment_opening frag_openingElement;
      jsx_children loc frag_children;
      jsx_closing_fragment frag_closingElement;
    ]

and jsx_identifier (loc, { Ast.JSX.Identifier.name }) =
  identifier_with_comments @@ Flow_ast_utils.ident_of_source (loc, name)

and jsx_namespaced_name (loc, { Ast.JSX.NamespacedName.namespace; name }) =
  source_location_with_comments
    (loc, fuse [jsx_identifier namespace; Atom ":"; jsx_identifier name])

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

and jsx_expression_container { Ast.JSX.ExpressionContainer.expression = expr } =
  fuse
    [
      Atom "{";
      begin
        match expr with
        | Ast.JSX.ExpressionContainer.Expression expr -> expression expr
        | Ast.JSX.ExpressionContainer.EmptyExpression -> Empty
      end;
      Atom "}";
    ]

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
                    | A.Literal (loc, lit) -> source_location_with_comments (loc, literal lit)
                    | A.ExpressionContainer (loc, express) ->
                      source_location_with_comments (loc, jsx_expression_container express)
                  end;
                ]
            | None -> flat_ugly_space (* TODO we shouldn't do this for the last attr *)
          end;
        ] )

and jsx_spread_attribute (loc, { Ast.JSX.SpreadAttribute.argument }) =
  source_location_with_comments (loc, fuse [Atom "{"; Atom "..."; expression argument; Atom "}"])

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
              (fuse [line; join pretty_line (Core_list.map ~f:jsx_opening_attr attributes)])
          else
            Empty );
          Atom ">";
        ] )

and jsx_self_closing (loc, { Ast.JSX.Opening.name; attributes; selfClosing = _ }) =
  let attributes = Core_list.map ~f:jsx_opening_attr attributes in
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
    let processed_children = deoptionalize (Core_list.map ~f:jsx_child children) in
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
      fuse (Core_list.map ~f:(fun (loc, child) -> SourceLocation (loc, child)) processed_children))

and jsx_child (loc, child) =
  match child with
  | Ast.JSX.Element elem -> Some (loc, jsx_element loc elem)
  | Ast.JSX.Fragment frag -> Some (loc, jsx_fragment loc frag)
  | Ast.JSX.ExpressionContainer express -> Some (loc, jsx_expression_container express)
  | Ast.JSX.SpreadChild expr -> Some (loc, fuse [Atom "{..."; expression expr; Atom "}"])
  | Ast.JSX.Text { Ast.JSX.Text.raw; _ } ->
    begin
      match Utils_jsx.trim_jsx_text loc raw with
      | Some (loc, txt) -> Some (loc, Atom txt)
      | None -> None
    end

and partition_specifiers default specifiers =
  Ast.Statement.ImportDeclaration.(
    let (special, named) =
      match specifiers with
      | Some (ImportNamespaceSpecifier (loc, id)) -> ([import_namespace_specifier (loc, id)], None)
      | Some (ImportNamedSpecifiers named_specifiers) ->
        ([], Some (import_named_specifiers named_specifiers))
      | None -> ([], None)
    in
    match default with
    | Some default -> (identifier default :: special, named)
    | None -> (special, named))

and import_namespace_specifier (loc, id) =
  source_location_with_comments
    (loc, fuse [Atom "*"; pretty_space; Atom "as"; space; identifier id])

and import_named_specifier { Ast.Statement.ImportDeclaration.kind; local; remote } =
  fuse
    [
      Ast.Statement.ImportDeclaration.(
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
        (Core_list.map ~f:import_named_specifier named_specifiers);
    ]

and import_declaration { Ast.Statement.ImportDeclaration.importKind; source; specifiers; default }
    =
  let s_from = fuse [Atom "from"; pretty_space] in
  let module I = Ast.Statement.ImportDeclaration in
  with_semicolon
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
           | ((special, None), _) -> fuse [space; fuse_list ~sep:(Atom ",") special; space; s_from]
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
  Ast.Statement.ExportNamedDeclaration.(
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
      fuse
        [source_location_with_comments (loc, Atom "*"); export_source ~prefix:pretty_space source])

and export_declaration
    { Ast.Statement.ExportNamedDeclaration.declaration; specifiers; source; exportKind } =
  fuse
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

and export_default_declaration { Ast.Statement.ExportDefaultDeclaration.default = _; declaration }
    =
  fuse
    [
      Atom "export";
      space;
      Atom "default";
      space;
      Ast.Statement.ExportDefaultDeclaration.(
        match declaration with
        | Declaration stat -> statement stat
        | Expression expr -> with_semicolon (expression expr));
    ]

and variance (loc, var) =
  source_location_with_comments
    ( loc,
      match var with
      | Ast.Variance.Plus -> Atom "+"
      | Ast.Variance.Minus -> Atom "-" )

and switch_case ~last (loc, { Ast.Statement.Switch.Case.test; consequent }) =
  let case_left =
    match test with
    | Some expr -> fuse_with_space [Atom "case"; fuse [expression expr; Atom ":"]]
    | None -> Atom "default:"
  in
  source_location_with_comments
    ( loc,
      match consequent with
      | [] -> case_left
      | _ ->
        let statements = statement_list ~pretty_semicolon:last consequent in
        fuse [case_left; Indent (fuse [pretty_hardline; join pretty_hardline statements])] )

and type_param
    ( _,
      {
        Ast.Type.ParameterDeclaration.TypeParam.name = (loc, { Ast.Identifier.name; comments = _ });
        bound;
        variance = variance_;
        default;
      } ) =
  fuse
    [
      option variance variance_;
      source_location_with_comments (loc, Atom name);
      hint type_annotation bound;
      begin
        match default with
        | Some t -> fuse [pretty_space; Atom "="; pretty_space; type_ t]
        | None -> Empty
      end;
    ]

and type_parameter (loc, params) =
  source_location_with_comments
    ( loc,
      group
        [new_list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (Core_list.map ~f:type_param params)]
    )

and type_parameter_instantiation_with_implicit (loc, args) =
  source_location_with_comments
    ( loc,
      group
        [
          new_list
            ~wrap:(Atom "<", Atom ">")
            ~sep:(Atom ",")
            (Core_list.map ~f:explicit_or_implicit args);
        ] )

and type_parameter_instantiation (loc, args) =
  source_location_with_comments
    (loc, group [new_list ~wrap:(Atom "<", Atom ">") ~sep:(Atom ",") (Core_list.map ~f:type_ args)])

and type_alias ~declare { Ast.Statement.TypeAlias.id; tparams; right } =
  with_semicolon
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

and opaque_type ~declare { Ast.Statement.OpaqueType.id; tparams; impltype; supertype } =
  with_semicolon
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

and type_predicate (loc, pred) =
  source_location_with_comments
    ( loc,
      fuse
        [
          Atom "%checks";
          Ast.Type.Predicate.(
            match pred with
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
    {
      Ast.Type.Function.params = (_, { Ast.Type.Function.Params.params; rest = restParams });
      return;
      tparams;
    } =
  let params = Core_list.map ~f:type_function_param params in
  let params =
    match restParams with
    | Some (loc, { Ast.Type.Function.RestParam.argument }) ->
      params
      @ [source_location_with_comments (loc, fuse [Atom "..."; type_function_param argument])]
    | None -> params
  in
  fuse
    [
      option type_parameter tparams;
      group
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
  Ast.Type.Object.(
    function
    | Property
        (loc, { Property.key; value; optional; static; proto; variance = variance_; _method }) ->
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
        ( loc,
          match (value, _method, proto, optional) with
          (* Functions with no special properties can be rendered as methods *)
          | (Property.Init (loc, Ast.Type.Function func), true, false, false) ->
            source_location_with_comments
              (loc, fuse [s_static; object_property_key key; type_function ~sep:(Atom ":") func])
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
                  [Atom "get"; space; object_property_key key; type_function ~sep:(Atom ":") func]
              )
          | (Property.Set (loc, func), _, _, _) ->
            source_location_with_comments
              ( loc,
                fuse
                  [Atom "set"; space; object_property_key key; type_function ~sep:(Atom ":") func]
              ) )
    | SpreadProperty (loc, { SpreadProperty.argument }) ->
      source_location_with_comments (loc, fuse [Atom "..."; type_ argument])
    | Indexer (loc, { Indexer.id; key; value; static; variance = variance_ }) ->
      source_location_with_comments
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
    | CallProperty (loc, { CallProperty.value = (call_loc, func); static }) ->
      source_location_with_comments
        ( loc,
          fuse
            [
              ( if static then
                fuse [Atom "static"; space]
              else
                Empty );
              source_location_with_comments (call_loc, type_function ~sep:(Atom ":") func);
            ] )
    | InternalSlot (loc, { InternalSlot.id; value; optional; static; _method = _ }) ->
      source_location_with_comments
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
            ] ))

and type_object ?(sep = Atom ",") { Ast.Type.Object.exact; properties; inexact } =
  let s_exact =
    if exact then
      Atom "|"
    else
      Empty
  in
  let props = Core_list.map ~f:type_object_property properties in
  let props =
    if inexact then
      props @ [Atom "..."]
    else
      props
  in
  group [new_list ~wrap:(fuse [Atom "{"; s_exact], fuse [s_exact; Atom "}"]) ~sep props]

and type_interface { Ast.Type.Interface.extends; body = (loc, obj) } =
  fuse
    [
      Atom "interface";
      interface_extends extends;
      pretty_space;
      source_location_with_comments (loc, type_object ~sep:(Atom ",") obj);
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
             (fun (loc, generic) -> source_location_with_comments (loc, type_generic generic))
             xs);
      ]

and type_generic { Ast.Type.Generic.id; targs } =
  let rec generic_identifier =
    Ast.Type.Generic.Identifier.(
      function
      | Unqualified id -> identifier id
      | Qualified (loc, { qualification; id }) ->
        source_location_with_comments
          (loc, fuse [generic_identifier qualification; Atom "."; identifier id]))
  in
  fuse [generic_identifier id; option type_parameter_instantiation targs]

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
      | T.Any -> Atom "any"
      | T.Mixed -> Atom "mixed"
      | T.Empty -> Atom "empty"
      | T.Void -> Atom "void"
      | T.Null -> Atom "null"
      | T.Number -> Atom "number"
      | T.BigInt -> Atom "bigint"
      | T.String -> Atom "string"
      | T.Boolean -> Atom "boolean"
      | T.Nullable t -> fuse [Atom "?"; type_with_parens t]
      | T.Function func -> type_function ~sep:(fuse [pretty_space; Atom "=>"]) func
      | T.Object obj -> type_object obj
      | T.Interface i -> type_interface i
      | T.Array t -> fuse [Atom "Array<"; type_ t; Atom ">"]
      | T.Generic generic -> type_generic generic
      | T.Union (t1, t2, ts) -> type_union_or_intersection ~sep:(Atom "|") (t1 :: t2 :: ts)
      | T.Intersection (t1, t2, ts) -> type_union_or_intersection ~sep:(Atom "&") (t1 :: t2 :: ts)
      | T.Typeof t -> fuse [Atom "typeof"; space; type_ t]
      | T.Tuple ts ->
        group [new_list ~wrap:(Atom "[", Atom "]") ~sep:(Atom ",") (Core_list.map ~f:type_ ts)]
      | T.StringLiteral lit -> string_literal_type lit
      | T.NumberLiteral t -> number_literal_type t
      | T.BigIntLiteral { Ast.BigIntLiteral.bigint; _ } -> Atom bigint
      | T.BooleanLiteral value ->
        Atom
          ( if value then
            "true"
          else
            "false" )
      | T.Exists -> Atom "*" )

and explicit_or_implicit
    (x : (Loc.t, Loc.t) Ast.Expression.TypeParameterInstantiation.type_parameter_instantiation) =
  Ast.Expression.TypeParameterInstantiation.(
    match x with
    | Implicit _ -> Atom "_"
    | Explicit t -> type_ t)

and interface_declaration_base
    ~def { Ast.Statement.Interface.id; tparams; body = (loc, obj); extends } =
  fuse
    [
      def;
      identifier id;
      option type_parameter tparams;
      interface_extends extends;
      pretty_space;
      source_location_with_comments (loc, type_object ~sep:(Atom ",") obj);
    ]

and interface_declaration interface =
  interface_declaration_base ~def:(fuse [Atom "interface"; space]) interface

and declare_interface interface =
  interface_declaration_base ~def:(fuse [Atom "declare"; space; Atom "interface"; space]) interface

and declare_class
    ?(s_type = Empty)
    { Ast.Statement.DeclareClass.id; tparams; body = (loc, obj); extends; mixins; implements } =
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
                 [Atom "extends"; space; source_location_with_comments (loc, type_generic generic)])
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
                          source_location_with_comments (loc, type_generic generic))
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
  let body = source_location_with_comments (loc, type_object ~sep:(Atom ",") obj) in
  let parts =
    []
    |> List.rev_append class_parts
    |> List.cons extends_parts
    |> List.cons pretty_space
    |> List.cons body
    |> List.rev
  in
  group parts

and declare_function
    ?(s_type = Empty) { Ast.Statement.DeclareFunction.id; annot = (loc, t); predicate } =
  with_semicolon
    (fuse
       [
         Atom "declare";
         space;
         s_type;
         Atom "function";
         space;
         identifier id;
         source_location_with_comments
           ( loc,
             match t with
             | (loc, Ast.Type.Function func) ->
               source_location_with_comments (loc, type_function ~sep:(Atom ":") func)
             | _ -> failwith "Invalid DeclareFunction" );
         begin
           match predicate with
           | Some pred -> fuse [pretty_space; type_predicate pred]
           | None -> Empty
         end;
       ])

and declare_variable ?(s_type = Empty) { Ast.Statement.DeclareVariable.id; annot } =
  with_semicolon
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

and declare_module_exports annot =
  with_semicolon (fuse [Atom "declare"; space; Atom "module.exports"; type_annotation annot])

and declare_module { Ast.Statement.DeclareModule.id; body; kind = _ } =
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
    ]

and declare_export_declaration
    { Ast.Statement.DeclareExportDeclaration.default; declaration; specifiers; source } =
  let s_export =
    fuse
      [
        Atom "export";
        space;
        ( if Option.is_some default then
          fuse [Atom "default"; space]
        else
          Empty );
      ]
  in
  match (declaration, specifiers) with
  | (Some decl, None) ->
    Ast.Statement.DeclareExportDeclaration.(
      (match decl with
      (* declare export var *)
      | Variable (loc, var) ->
        source_location_with_comments (loc, declare_variable ~s_type:s_export var)
      (* declare export function *)
      | Function (loc, func) ->
        source_location_with_comments (loc, declare_function ~s_type:s_export func)
      (* declare export class *)
      | Class (loc, c) -> source_location_with_comments (loc, declare_class ~s_type:s_export c)
      (* declare export default [type]
       * this corresponds to things like
       * export default 1+1; *)
      | DefaultType t -> with_semicolon (fuse [Atom "declare"; space; s_export; type_ t])
      (* declare export type *)
      | NamedType (loc, typeAlias) ->
        source_location_with_comments
          (loc, fuse [Atom "declare"; space; s_export; type_alias ~declare:false typeAlias])
      (* declare export opaque type *)
      | NamedOpaqueType (loc, opaqueType) ->
        source_location_with_comments
          (loc, fuse [Atom "declare"; space; s_export; opaque_type ~declare:false opaqueType])
      (* declare export interface *)
      | Interface (loc, interface) ->
        source_location_with_comments
          (loc, fuse [Atom "declare"; space; s_export; interface_declaration interface])))
  | (None, Some specifier) ->
    fuse [Atom "declare"; space; Atom "export"; pretty_space; export_specifier source specifier]
  | (_, _) -> failwith "Invalid declare export declaration"
