(**
* Copyright (c) 2016, Facebook, Inc.
* All rights reserved.
*
* This source code is licensed under the BSD-style license found in the
* LICENSE file in the "hack" directory of this source tree. An additional grant
* of patent rights can be found in the PATENTS file in the same directory.
*
*)

(* The general approach of the pretty printer is to give as much flexibility in
 * terms of where a newline can occur as possible, while maintaining a sensible
 * layout that is considerred human readable and "normal"
 * An optional break is introduced whenever it is possible for a line break to
 * occur in real life. Small components are grouped together so that each
 * component decide their layouts individually.
 * Single line comments is the exception to everything: a newline is forced at
 * the end of a single line comments even if the line otherwise fits. This rule
 * overrides the default behaviour of some operators, such as immediate_cons ^^^
 * which otherwise does not even introduce a break.
 * Design decisions include:
 *  1. Each statement is separated from others using a line break
 *  2. Brackets/parenthesis/braces are either both on different lines with child
 *     or both on the same line with the entire child on one line as well
 *  3. binary operators can be on different lines with arguments
 *  4. unary operators must be on the same line as the argument
 *  5. semicolons have to be on the same line as the last line of the statement
 *     that it ends
 *  6. Switch statements are specially treated since case label is only a label
 *     and not a block in the grammar. Cases are broken up into blocks in the
 *     printer and each case is grouped together to have individual layout
 *     options
 *)

(* The main data type that is used in the pretty printer is a 5 tuple:
 *
 *   (l_trivia, l_single, doc, r_trivia, r_single)
 *
 *   l_trivia: the doc generated from the leading trivia of the syntax node
 *   l_single: whether the leading trivia contains a single line comment
 *   doc     : the doc generated from the main body of the syntax node
 *   r_trivia: the doc generated from trailing trivia of the syntax node
 *   r_single: whether the trailing trivia contains a single line comment
 *)
open Pretty_printing_library_sig
open Limited_width_pretty_printing_library
(* utility functions to make combinators look simpler
 * See Lindig's paper for reference *)
module Utility (P : Library) = struct
  include P

  let space = text " "

  let absorb_nil_cons doc1 doc2 delimiter =
    match doc1, doc2 with
    | Nil, _ -> doc2
    | _, Nil -> doc1
    | _, _ -> doc1 ^^ delimiter ^^ doc2

  let break_cons doc1 doc2 = absorb_nil_cons doc1 doc2 break

  let space_cons doc1 doc2 = absorb_nil_cons doc1 doc2 space

  let choose_cons must is_empty x y =
    let delimiter =
      if must then must_break
      else if is_empty then breakwith ""
      else break
    in
    absorb_nil_cons x y delimiter


  let group_doc (x_lead, x_lead_single, x, x_trail, x_trail_single) =
    let optional_group = if x = Nil then x else group x in
    (x_lead, x_lead_single, optional_group, x_trail, x_trail_single)

  let combine (l_lead, l_lead_single, l, l_trail, l_trail_single) =
    let front_part = choose_cons l_lead_single false l_lead (group l) in
    space_cons front_part l_trail

  (* higher order function to combine two docs with leading and trailing
   * comments. This function creates a front part and a back part in a fixed
   * way and takes a function that combines the two parts in different ways *)
  let doc_combinor x y combinor =
    let (l_lead, l_lead_single, l, l_trail, l_trail_single) = x in
    let (r_lead, r_lead_single, r, r_trail, r_trail_single) = y in
    let front_p = space_cons l l_trail in
    let end_p = choose_cons r_lead_single false r_lead r in
    match l, r with
    | Nil, _ -> (r_lead, r_lead_single, r, r_trail, r_trail_single)
    | _, Nil -> (l_lead, l_lead_single, l, l_trail, l_trail_single)
    | _, _ ->
      (l_lead, l_lead_single, combinor front_p end_p, r_trail, r_trail_single)

  let doc_cons_opt empty x y =
    let (_, _, _, _, l_trail_single) = x in
    let combinor front_part end_part =
      choose_cons l_trail_single empty front_part end_part in
    doc_combinor x y combinor

  let doc_cons = doc_cons_opt false
  let (^|) = doc_cons

  let doc_cons_empty = doc_cons_opt true
  let (^^|) = doc_cons_empty

  let immediate_cons x y =
    let (_, _, _, _, l_trail_single) = x in
    let (_, r_lead_single, _, _, _) = y in
    let combinor front_part end_part =
      if l_trail_single || r_lead_single then
        choose_cons true false front_part end_part
      else front_part ^^ end_part
    in
    doc_combinor x y combinor
  let (^^^) = immediate_cons

  (* indent [r] by [indent] after [l] using the suitable line break *)
  let choose_indent_doc empty x y indent =
    let (_, _, _, _, l_trail_single) = x in
    let combinor front_part end_part =
      let break_choice =
       if l_trail_single then must_break
       else if empty then breakwith ""
       else break
      in
      front_part ^^ nest indent (break_choice ^^ end_part)
    in
    doc_combinor x y combinor

  (* put a break before [r] and nest [r] by [indent] in vertical layout *)
  let indent_doc = choose_indent_doc false

  let indent_doc_no_space = choose_indent_doc true

  (* typically we will want to indent block [blk] that is enclosed by
  * [l] and [r] by amount [indt] *)
  let indent_block l blk r indt =
    group_doc (indent_doc l blk indt ^| r)

  let indent_block_no_space l blk r indt =
    group_doc ((indent_doc_no_space l blk indt) ^^| r)
end

module LineConf = struct
 let line_width = 80
end
module Comparator = WidthConstrainedDocComparator(LineConf)
module Core = Pretty_printing_library.Make(Comparator)
module Printer = Utility(Core)
module Syntax = Full_fidelity_editable_syntax
module EditableToken = Full_fidelity_editable_token
open Syntax
open Printer

let get_doc_from_trivia trivia_lst allow_break =
  (* generate a document from a list of trivias. Return the doc, as well
   * as whether the trivia list contains a single line comment *)
  let module Trivia = Full_fidelity_editable_trivia in
  let module Kind = Full_fidelity_trivia_kind in
  let handle_trivia trivia = match Trivia.kind trivia with
    | Kind.WhiteSpace -> (nil, false)
    | Kind.EndOfLine -> (nil, false)
    | Kind.SingleLineComment ->
      (* no code after comments *)
      (text (Trivia.text trivia), true)
    | Kind.DelimitedComment ->
      (text (Trivia.text trivia), false)
  in
  let concat = if allow_break then break_cons else space_cons in
  let fold_fun x y =
    let (a, c1) = x in
    let (b, c2) = y in
    let result = concat a b in
    (result, c1 || c2)
  in
  (* no group here, since all breaks are compulsory. Will group on top level *)
  List.fold_left fold_fun (nil, false) (List.map handle_trivia trivia_lst)

let from_token x =
  let front_trivias = EditableToken.leading x in
  let end_trivias = EditableToken.trailing x in
  let (front_doc, front_single) = get_doc_from_trivia front_trivias true in
  let (end_doc, end_single) = get_doc_from_trivia end_trivias false in
  let doc = text (EditableToken.text x) in
  (front_doc, front_single, doc, end_doc, end_single)

(* create a 5-tuple (see top of file) from x with no trivias *)
let make_simple x = (nil, false, x, nil, false)
let indt = 2
let missing = make_simple nil
let error_header = make_simple (text "Error:")
let space = make_simple (text " ")
let colon = make_simple (text ":")
let comma = make_simple (text ",")
let l_square = make_simple (text "[")
let r_square = make_simple (text "]")

let rec get_doc node =
  match syntax node with
  | Missing -> missing
  | Token x -> from_token x
  | LiteralExpression x
  | VariableExpression x
  | QualifiedNameExpression x -> get_doc x
  | Error x -> get_from_children x
  | SyntaxList x -> get_from_children x
  | ScriptHeader x -> get_doc (header_less_than x) ^^^
                      get_doc (header_question x) ^^^
                      get_doc (header_language x)
  | Script x -> group_doc ( get_doc (script_header x)
                     ^| get_doc (script_declarations x) )
  | FunctionDeclaration x ->
    let preface = group_doc ( get_doc (function_attr x)
                       ^| get_doc (function_async x)
                       ^| get_doc (function_token x) ) in
    let name_and_generics =
      let type_params = get_doc (function_type_params x) in
      let name = get_doc (function_name x) in
      group_doc (indent_doc name type_params indt)
    in
    let parameters =
      let left = get_doc (function_left_paren x) in
      let right = get_doc (function_right_paren x) in
      let params = get_doc (function_params x) in
      indent_block_no_space left params right indt
    in
    let type_declaration =
      let fun_colon = get_doc (function_colon x) in
      let fun_type = get_doc (function_type x) in
      group_doc (fun_colon ^| fun_type)
    in
    let body = get_doc (function_body x) in
      group_doc (
        group_doc (
          group_doc ( group_doc (preface ^| name_and_generics) ^^| parameters )
          ^| type_declaration
        ) ^| body
      )
  | ParameterDeclaration x ->
    let attr = get_doc (param_attr x) in
    let parameter_type = get_doc (param_type x) in
    let parameter_name = get_doc (param_name x) in
    let parameter_default = get_doc (param_default x) in
    group_doc (attr ^| parameter_type ^| parameter_name ^| parameter_default)
  | DefaultArgumentSpecifier x ->
    let def_equal = get_doc (default_equal x) in
    let def_value = get_doc (default_value x) in
    group_doc (def_equal ^| def_value)
  | CompoundStatement x ->
    let left = get_doc (compound_left_brace x) in
    let right = get_doc (compound_right_brace x) in
    let body = get_doc (compound_statements x) in
    indent_block_no_space left body right indt
  | ExpressionStatement x ->
    let body = get_doc (expr_statement_expr x) in
    let semicolon = get_doc (expr_statement_semicolon x) in
    group_doc (body ^^^ semicolon) (* semicolon always follows the last line *)
  | WhileStatement x ->
    let keyword = get_doc (while_keyword x) in
    let left = get_doc (while_left_paren x) in
    let right = get_doc (while_right_paren x) in
    let condition = get_doc (while_condition_expr x) in
    let body = get_doc (while_body x) in
    let left_part = group_doc (keyword ^^| left) in
    let start_block = indent_block_no_space left_part condition right indt in
    let indt = peek_and_decide_indent (while_body x) indt in
    group_doc (indent_doc start_block body indt)
  | IfStatement x ->
    let keyword = get_doc (if_keyword x) in
    let left = get_doc (if_left_paren x) in
    let condition = get_doc (if_condition_expr x) in
    let right = get_doc (if_right_paren x) in
    let statement = get_doc (if_statement x) in
    let elseif_clause = get_doc (if_elseif_clauses x) in
    let else_clause = get_doc (if_else_clause x) in
    let left_part = group_doc (keyword ^^| left) in
    let start_block = indent_block_no_space left_part condition right indt in
    let if_statement =
      let indt = peek_and_decide_indent (if_statement x) indt in
      group_doc (indent_doc start_block statement indt) in
    group_doc (if_statement ^| elseif_clause ^| else_clause)
  | ElseifClause x ->
    let keyword = get_doc (elseif_keyword x) in
    let left = get_doc (elseif_left_paren x) in
    let condition = get_doc (elseif_condition_expr x) in
    let right = get_doc (elseif_right_paren x) in
    let statement = get_doc (elseif_statement x) in
    let left_part = group_doc (keyword ^^| left) in
    let start_block = indent_block_no_space left_part condition right indt in
    let indt = peek_and_decide_indent (elseif_statement x) indt in
    group_doc (indent_doc start_block statement indt)
  | ElseClause x ->
    let keyword = get_doc (else_keyword x) in
    let statement = get_doc (else_statement x) in
    let indt = peek_and_decide_indent (else_statement x) indt in
    group_doc (indent_doc keyword statement indt)
  | DoStatement x ->
    let keyword = get_doc (do_keyword x) in
    let statement = get_doc (do_statement x) in
    let while_keyword = get_doc (do_while_keyword x) in
    let left = get_doc (do_left_paren x) in
    let right = get_doc (do_right_paren x) in
    let condition = get_doc (do_condition_expr x) in
    let semicolon = get_doc (do_semicolon x) in
    let statement_part =
      let indt = peek_and_decide_indent (do_statement x) indt in
      group_doc (indent_doc keyword statement indt) in
    let left_part = group_doc (while_keyword ^^| left) in
    let condition_part = indent_block_no_space left_part condition right indt in
    group_doc (statement_part ^| condition_part) ^^^ semicolon
  | SwitchStatement x ->
    let keyword = get_doc (switch_keyword x) in
    let left = get_doc (switch_left_paren x) in
    let right = get_doc (switch_right_paren x) in
    let expr = get_doc (switch_expr x) in
    let statement = handle_switch x in
    let left_part = group_doc (keyword ^^| left) in
    let start_block = indent_block_no_space left_part expr right indt in
    group_doc (start_block ^| statement)
  | PrefixUnaryOperator x ->
    get_doc (unary_operator x) ^^^ get_doc (unary_operand x)
  | PostfixUnaryOperator x ->
    get_doc (unary_operand x) ^^^ get_doc (unary_operator x)
  | BinaryOperator x ->
    let left = get_doc (binary_left_operand x) in
    let op = get_doc (binary_operator x) in
    let right = get_doc (binary_right_operand x) in
    group_doc (left ^| op ^| right)
  | ParenthesizedExpression x ->
    let left = get_doc (paren_expr_left_paren x) in
    let right = get_doc (paren_expr_right_paren x) in
    let expr = get_doc (paren_expr x) in
    indent_block_no_space left expr right indt
  | BracedExpression x ->
    let left = get_doc (braced_expr_left_brace x) in
    let right = get_doc (braced_expr_right_brace x) in
    let expr = get_doc (braced_expr x) in
    indent_block_no_space left expr right indt
  | XHPExpression x ->
    let left = get_doc (xhp_open x) in
    let expr = get_doc (xhp_body x) in
    let right = get_doc (xhp_close x) in
    indent_block_no_space left expr right indt
  | XHPOpen x ->
    let name = get_doc (xhp_open_name x) in
    let attrs = get_doc (xhp_open_attrs x) in
    let close = get_doc (xhp_open_right_angle x) in
    group_doc (group_doc (indent_doc name attrs indt) ^| close)
  | XHPAttribute x ->
    let name = get_doc (xhp_attr_name x) in
    let equals = get_doc (xhp_attr_equal x) in
    let expr = get_doc (xhp_attr_expr x) in
    group_doc (group_doc (name ^^| equals) ^^| expr)
  | TypeConstant x ->
    let left = get_doc (type_constant_left_type x) in
    let right = get_doc (type_constant_right_type x) in
    let separator = get_doc (type_constant_separator x) in
    left ^^^ separator ^^^ right
  | SimpleTypeSpecifier x -> get_doc x
  | GenericTypeSpecifier x ->
    let name = get_doc (generic_class_type x) in
    let argument = get_doc (generic_arguments x) in
    group_doc (indent_doc_no_space name argument indt)
  | TypeArguments x ->
    let left = get_doc (type_arguments_left_angle x) in
    let right = get_doc (type_arguments_right_angle x) in
    let args = get_doc (type_arguments x) in
    indent_block_no_space left args right indt
  (* this ideally should never be called *)
  | CaseStatement x ->
    let keyword = get_doc (case_keyword x) in
    let expr = get_doc (case_expr x) in
    let colon = get_doc (case_colon x) in
    let statement = get_doc (case_stmt x) in
    let front_part = keyword ^^^ space ^^^ expr ^^^ colon in
    let indt = peek_and_decide_indent (case_stmt x) indt in
    group_doc (indent_doc front_part statement indt)
  | DefaultStatement x ->
    let keyword = get_doc (default_keyword x) in
    let colon = get_doc (default_colon x) in
    let statement = get_doc (default_stmt x) in
    let front_part = keyword ^^^ colon in
    let indt = peek_and_decide_indent (default_stmt x) indt in
    group_doc (indent_doc front_part statement indt)
  | ReturnStatement x ->
    let keyword = get_doc (return_keyword x) in
    let expr = get_doc (return_expr x) in
    let semicolon = get_doc (return_semicolon x) in
    let back_part = expr ^^^ semicolon in
    group_doc (indent_doc keyword back_part indt)
  | ThrowStatement x ->
    let keyword = get_doc (throw_keyword x) in
    let expr = get_doc (throw_expr x) in
    let semicolon = get_doc (throw_semicolon x) in
    let back_part = expr ^^^ semicolon in
    group_doc (indent_doc keyword back_part indt)
  | BreakStatement x ->
    let keyword = get_doc (break_keyword x) in
    let semicolon = get_doc (break_semicolon x) in
    keyword ^^^ semicolon
  | ContinueStatement x ->
    let keyword = get_doc (continue_keyword x) in
    let semicolon = get_doc (continue_semicolon x) in
    keyword ^^^ semicolon
(* sep is the compulsory separator separating the children in the list *)
and get_from_children_with_sep sep children =
  let fold_fun acc el = (acc ^^^ sep) ^| get_doc el in
  group_doc (List.fold_left fold_fun (make_simple nil) children)
and get_from_children node = get_from_children_with_sep (make_simple nil) node
(* if it is a compound statement, the curly braces do not need indent *)
and peek_and_decide_indent x default =
  match syntax x with
  | CompoundStatement _ -> 0
  | _ -> default
(* Generate documents for a switch statement *)
and handle_switch switch =
  match syntax (switch_compound_statement switch) with
  | CompoundStatement x ->
    let left = get_doc (compound_left_brace x) in
    let right = get_doc (compound_right_brace x) in
    let body =
      let compound_body = compound_statements x in
      match syntax compound_body with
      | SyntaxList lst -> handle_switch_lists lst
      | _ -> get_doc compound_body
    in
    indent_block_no_space left body right indt
  | _ -> get_doc (switch_compound_statement switch)
(* specifically identify case chunks and generate docs from the list of
 * statements inside the compound statement of the switch statements *)
and handle_switch_lists lst =
  (* fold on a reversed statement list, if the element is not a case or default
   * label, then add the element to the [current] chunk. Otherwise, group the
   * current chunks together with the label, resulting in an indented block *)
  let fold_fun (current, docs) node =
    match syntax node with
    | CaseStatement x ->
      let keyword = get_doc (case_keyword x) in
      let expr = get_doc (case_expr x) in
      let colon = get_doc (case_colon x) in
      let front_part = keyword ^^^ space ^^^ expr ^^^ colon in
      let case_chunk = SyntaxList ((case_stmt x) :: current) in
      let new_list = make case_chunk (value node) in
      let end_part = get_doc new_list in
      let new_chunk = group_doc (indent_doc front_part end_part indt) in
      ([], new_chunk :: docs)
    | DefaultStatement x ->
      let keyword = get_doc (default_keyword x) in
      let colon = get_doc (default_colon x) in
      let front_part = keyword ^^^ colon in
      let default_chunk = SyntaxList ((default_stmt x) :: current) in
      let new_list = make default_chunk (value node) in
      let end_part = get_doc new_list in
      let new_chunk = group_doc (indent_doc front_part end_part indt) in
      ([], new_chunk :: docs)
    (* if this is not a case statement, add it to current *)
    | _ -> (node :: current, docs)
  in
  let (rest, docs) = List.fold_left fold_fun ([], []) (List.rev lst) in
  let rest_list = SyntaxList rest in
  (* TODO better interface to do this? *)
  let rest_node = make rest_list Syntax.EditableSyntaxValue.NoValue in
  let rest_doc = get_doc rest_node in
  let all_docs = rest_doc :: docs in
  List.fold_left (^|) (make_simple nil) all_docs

let pretty_print node =
  let to_print = combine (get_doc node) in
  pretty 0 to_print
