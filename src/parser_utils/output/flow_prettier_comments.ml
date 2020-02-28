(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(*
 * The file reproduces the behavior from
 * https://github.com/prettier/prettier/blob/bcdb6936ba42658b8dcdf1f70b4e5e0c8ae8ec06/src/main/comments.js
 *
 * It assumes that the comments are in the same file as the statements
 *)

module LocMap = Loc_collections.LocMap

module CommentAttachCandidate = struct
  type 'M t = {
    preceding: ('M, 'M) Ast.Statement.t option;
    enclosing: ('M, 'M) Ast.Statement.t option;
    following: ('M, 'M) Ast.Statement.t option;
  }
end

let node_list_of_option ~f = Base.Option.value_map ~default:[] ~f

(* comments.js#findExpressionIndexForComment *)
let find_expression_index_for_node quasis ({ Loc.start = orig_start; _ }, _) =
  try
    let (found, _) =
      List.tl quasis
      |> List.mapi (fun i q -> (i, q))
      |> List.find (fun (_, ({ Loc.start; _ }, _)) -> Loc.pos_cmp orig_start start < 0)
    in
    found - 1
  with Not_found -> 0

(* / comments.js#findExpressionIndexForComment *)
(* comments.js#attach *)
let rec attach_comments ((_, ss, cs) : (Loc.t, Loc.t) Ast.program) : Js_layout_generator.comment_map
    =
  let (comment_list, comment_ties) = List.fold_left (attach_comment ss) ([], []) cs in
  (* The original algorithm in prettier may return some unresolved ties *)
  let comment_list = break_tie comment_list comment_ties in
  List.fold_left
    (fun map attach ->
      let (_, statement, _) = attach in
      let (loc, _) = statement in
      let comments_at_loc =
        match LocMap.find_opt loc map with
        | Some comments -> attach :: comments
        | None -> [attach]
      in
      LocMap.add loc comments_at_loc map)
    LocMap.empty
    comment_list

and attach_comment (statements : (Loc.t, Loc.t) Ast.Statement.t list) (comments, ties) comment =
  let attach_candidate = find_comment_attach statements comment in
  let attach_candidate_fixed = fix_template_literals comment attach_candidate in
  position_comment comments ties comment attach_candidate_fixed

(* comments.js#decorateComment *)
and find_comment_attach statements (comment_pos, _) =
  let rec find_comment statements l r candidate =
    if l >= r then
      candidate
    else
      (* In prettier it uses >> 1, which has different behavior for -1
       * and shouldn't be a problem here *)
      let m = l + ((r - l) / 2) in
      let ((pivot_pos, _) as pivot) = statements.(m) in
      match Loc.span_compare pivot_pos comment_pos with
      | n when n < 0 ->
        find_comment
          statements
          (m + 1)
          r
          { candidate with CommentAttachCandidate.preceding = Some pivot }
      | n when n > 0 ->
        find_comment statements l m { candidate with CommentAttachCandidate.following = Some pivot }
      | _ ->
        let children_nodes = Array.of_list (get_children_nodes pivot) in
        find_comment
          children_nodes
          0
          (Array.length children_nodes)
          { CommentAttachCandidate.preceding = None; enclosing = Some pivot; following = None }
  in
  let statements = Array.of_list statements in
  find_comment
    statements
    0
    (Array.length statements)
    { CommentAttachCandidate.preceding = None; enclosing = None; following = None }

(* comments.js#getSortedChildNodes *)
and get_children_nodes (statement : (Loc.t, Loc.t) Ast.Statement.t) =
  let (_loc, stmt) = statement in
  let open Ast.Statement in
  match stmt with
  | Block { Block.body } -> body
  | If { If.test; consequent; alternate; comments = _ } ->
    [statement_of_expression test; consequent] @ statement_list_of_option alternate
  | Labeled { Labeled.body; _ } -> [body]
  | With { With._object; body } -> [statement_of_expression _object; body]
  | TypeAlias _ -> []
  | OpaqueType _ -> []
  | Switch { Switch.discriminant; cases } ->
    get_children_nodes_expr discriminant
    @ List.fold_left
        (fun nodes (_, { Switch.Case.test; consequent }) ->
          let test_nodes = node_list_of_option ~f:get_children_nodes_expr test in
          let consequent_nodes =
            consequent |> Base.List.map ~f:get_children_nodes |> List.flatten
          in
          nodes @ test_nodes @ consequent_nodes)
        []
        cases
  | Return { Return.argument; comments = _ } ->
    node_list_of_option ~f:statement_list_of_expression argument
  | Throw { Throw.argument; comments = _ } -> statement_list_of_expression argument
  | Try { Try.block = (_, { Block.body }); handler; finalizer; _ } ->
    let handler_nodes =
      Base.Option.value_map
        ~default:[]
        ~f:(fun (_, { Try.CatchClause.param; body = (_, { Block.body }); _ }) ->
          node_list_of_option ~f:get_children_nodes_pattern param @ body)
        handler
    in
    let finalizer_nodes =
      Base.Option.value_map ~default:[] ~f:(fun (_, { Block.body }) -> body) finalizer
    in
    body @ handler_nodes @ finalizer_nodes
  | VariableDeclaration { VariableDeclaration.declarations; _ } ->
    List.fold_left
      (fun nodes (_, { VariableDeclaration.Declarator.init; _ }) ->
        nodes @ node_list_of_option ~f:get_children_nodes_expr init)
      []
      declarations
  | While { While.test; body } -> [statement_of_expression test; body]
  | DoWhile { DoWhile.test; body; comments = _ } -> [statement_of_expression test; body]
  | For { For.init; test; update; body } ->
    let init_nodes =
      node_list_of_option
        ~f:(fun init ->
          For.(
            match init with
            | InitDeclaration (loc, decl) -> get_children_nodes (loc, VariableDeclaration decl)
            | InitExpression expr -> get_children_nodes_expr expr))
        init
    in
    let test_nodes = node_list_of_option ~f:get_children_nodes_expr test in
    let update_nodes = node_list_of_option ~f:get_children_nodes_expr update in
    let body_nodes = get_children_nodes body in
    init_nodes @ test_nodes @ update_nodes @ body_nodes
  | ForIn { ForIn.left; right; body; _ } ->
    let left_nodes =
      ForIn.(
        match left with
        | LeftDeclaration (loc, decl) -> get_children_nodes (loc, VariableDeclaration decl)
        | LeftPattern pattern -> get_children_nodes_pattern pattern)
    in
    let right_nodes = get_children_nodes_expr right in
    let body_nodes = get_children_nodes body in
    left_nodes @ right_nodes @ body_nodes
  | ForOf { ForOf.left; right; body; _ } ->
    let left_nodes =
      ForOf.(
        match left with
        | LeftDeclaration (loc, decl) -> get_children_nodes (loc, VariableDeclaration decl)
        | LeftPattern pattern -> get_children_nodes_pattern pattern)
    in
    let right_nodes = get_children_nodes_expr right in
    let body_nodes = get_children_nodes body in
    left_nodes @ right_nodes @ body_nodes
  | DeclareClass _ -> []
  | DeclareVariable _ -> []
  | DeclareFunction _ -> []
  | DeclareModule { DeclareModule.body = (_, { Block.body }); _ } -> body
  | ExportNamedDeclaration { ExportNamedDeclaration.declaration; _ } ->
    statement_list_of_option declaration
  | ExportDefaultDeclaration { ExportDefaultDeclaration.declaration; _ } ->
    (match declaration with
    | ExportDefaultDeclaration.Declaration d -> [d]
    | ExportDefaultDeclaration.Expression e -> statement_list_of_expression e)
  | DeclareExportDeclaration _ -> []
  | ImportDeclaration _ -> []
  | Expression { Expression.expression; _ } -> get_children_nodes_expr expression
  | Debugger -> []
  | Empty -> []
  | EnumDeclaration _ -> []
  | Break _ -> []
  | ClassDeclaration clazz -> get_children_nodes_class clazz
  | Continue _ -> []
  | DeclareInterface _ -> []
  | DeclareModuleExports _ -> []
  | DeclareTypeAlias _ -> []
  | DeclareOpaqueType _ -> []
  | FunctionDeclaration funct -> get_children_nodes_function funct
  | InterfaceDeclaration _ -> []

and get_children_nodes_expr expression =
  let (loc, expr) = expression in
  let open Ast.Expression in
  match expr with
  | Array { Array.elements; comments = _ } ->
    List.fold_left
      (fun nodes element ->
        nodes @ Base.Option.value_map ~default:[] ~f:get_children_nodes_expression_or_spread element)
      []
      elements
  | ArrowFunction func -> get_children_nodes_function func
  | Assignment { Assignment.left; right; _ } ->
    get_children_nodes_pattern left @ get_children_nodes_expr right
  | Binary { Binary.left; right; _ } -> get_children_nodes_expr left @ get_children_nodes_expr right
  | Call { Call.callee; arguments; _ } ->
    get_children_nodes_expr callee @ get_children_nodes_arg_list arguments
  | Class clazz -> get_children_nodes_class clazz
  | Comprehension { Comprehension.blocks; filter } ->
    let block_nodes = get_children_nodes_comprehension_block_list blocks in
    let filter_nodes = node_list_of_option ~f:get_children_nodes_expr filter in
    block_nodes @ filter_nodes
  | Conditional { Conditional.consequent; alternate; _ } ->
    get_children_nodes_expr consequent @ get_children_nodes_expr alternate
  | Function func -> get_children_nodes_function func
  | Generator { Generator.blocks; filter } ->
    get_children_nodes_expr (loc, Comprehension { Comprehension.blocks; filter })
  | Identifier _ -> []
  | Import i -> get_children_nodes_expr i
  | JSXElement { Ast.JSX.openingElement; children; _ } ->
    get_children_nodes_jsx_opening openingElement @ get_children_nodes_jsx_child_list children
  | JSXFragment { Ast.JSX.frag_children; _ } -> get_children_nodes_jsx_child_list frag_children
  | Literal _ -> []
  | Logical { Logical.left; right; _ } ->
    get_children_nodes_expr left @ get_children_nodes_expr right
  | Member member -> get_children_nodes_member member
  | MetaProperty _ -> []
  | New { New.callee; arguments; _ } ->
    get_children_nodes_expr callee @ node_list_of_option ~f:get_children_nodes_arg_list arguments
  | Object { Object.properties; comments = _ } ->
    List.fold_left
      (fun nodes property ->
        nodes
        @
        match property with
        | Object.SpreadProperty (_, { Object.SpreadProperty.argument }) ->
          get_children_nodes_expr argument
        | Object.Property (_, property) ->
          (match property with
          | Object.Property.Init { value; _ } -> get_children_nodes_expr value
          | Object.Property.Method { value = (_, func); _ } -> get_children_nodes_function func
          | Object.Property.Get { value = (_, func); _ } -> get_children_nodes_function func
          | Object.Property.Set { value = (_, func); _ } -> get_children_nodes_function func))
      []
      properties
  | OptionalCall { OptionalCall.call = { Call.callee; arguments; _ }; _ } ->
    get_children_nodes_expr callee @ get_children_nodes_arg_list arguments
  | OptionalMember { OptionalMember.member; _ } -> get_children_nodes_member member
  | Sequence { Sequence.expressions } ->
    List.fold_left (fun nodes eos -> nodes @ get_children_nodes_expr eos) [] expressions
  | Super -> []
  | TaggedTemplate { TaggedTemplate.tag; quasi = (loc, quasi) } ->
    get_children_nodes_expr tag @ get_children_nodes_expr (loc, TemplateLiteral quasi)
  | TemplateLiteral { TemplateLiteral.expressions; _ } ->
    expressions |> Base.List.map ~f:get_children_nodes_expr |> List.flatten
  | This -> []
  | TypeCast { TypeCast.expression; _ } -> get_children_nodes_expr expression
  | Unary { Unary.argument; _ } -> get_children_nodes_expr argument
  | Update { Update.argument; _ } -> get_children_nodes_expr argument
  | Yield { Yield.argument; _ } -> node_list_of_option ~f:get_children_nodes_expr argument

and get_children_nodes_function { Ast.Function.body; _ } =
  match body with
  | Ast.Function.BodyBlock (_, { Ast.Statement.Block.body }) -> body
  | Ast.Function.BodyExpression expr -> get_children_nodes_expr expr

and get_children_nodes_arg_list (_loc, arguments) =
  List.fold_left (fun nodes eos -> nodes @ get_children_nodes_expression_or_spread eos) [] arguments

and get_children_nodes_expression_or_spread eos =
  match eos with
  | Ast.Expression.Expression expr -> get_children_nodes_expr expr
  | Ast.Expression.Spread (_, { Ast.Expression.SpreadElement.argument }) ->
    get_children_nodes_expr argument

and get_children_nodes_pattern (_loc, pattern) =
  let open Ast.Pattern in
  match pattern with
  | Object { Object.properties; _ } ->
    properties
    |> Base.List.map ~f:(fun property ->
           match property with
           | Object.Property (_, { Object.Property.key; pattern; default; shorthand = _ }) ->
             let key_nodes =
               match key with
               | Object.Property.Literal _ -> []
               | Object.Property.Identifier _ -> []
               | Object.Property.Computed expr -> get_children_nodes_expr expr
             in
             let pattern_nodes = get_children_nodes_pattern pattern in
             let default_nodes = node_list_of_option ~f:get_children_nodes_expr default in
             key_nodes @ pattern_nodes @ default_nodes
           | Object.RestProperty (_, { Object.RestProperty.argument }) ->
             get_children_nodes_pattern argument)
    |> List.flatten
  | Array { Array.elements; _ } ->
    elements
    |> Base.List.map ~f:(fun element_opt ->
           match element_opt with
           | Some (Array.Element (_, { Array.Element.argument; default })) ->
             let pattern_nodes = get_children_nodes_pattern argument in
             let default_nodes = node_list_of_option ~f:get_children_nodes_expr default in
             pattern_nodes @ default_nodes
           | Some (Array.RestElement (_, { Array.RestElement.argument })) ->
             get_children_nodes_pattern argument
           | None -> [])
    |> List.flatten
  | Identifier _ -> []
  | Expression expr -> get_children_nodes_expr expr

and get_children_nodes_class { Ast.Class.body = (_, { Ast.Class.Body.body }); _ } =
  List.fold_left
    (fun nodes member ->
      nodes
      @
      match member with
      | Ast.Class.Body.Method (_, { Ast.Class.Method.value = (_, funct); _ }) ->
        get_children_nodes_function funct
      | Ast.Class.Body.Property (_, { Ast.Class.Property.value; _ }) ->
        get_children_nodes_class_property_value value
      | Ast.Class.Body.PrivateField (_, { Ast.Class.PrivateField.value; _ }) ->
        get_children_nodes_class_property_value value)
    []
    body

and get_children_nodes_class_property_value = function
  | Ast.Class.Property.Declared -> []
  | Ast.Class.Property.Uninitialized -> []
  | Ast.Class.Property.Initialized expr -> statement_list_of_expression expr

and get_children_nodes_member { Ast.Expression.Member._object; property; _ } =
  (let open Ast.Expression.Member in
  match property with
  | PropertyIdentifier _ -> []
  | PropertyPrivateName _ -> []
  | PropertyExpression e -> get_children_nodes_expr e)
  @ get_children_nodes_expr _object

and get_children_nodes_comprehension_block_list
    (blocks : (Loc.t, Loc.t) Ast.Expression.Comprehension.Block.t list) =
  let open Ast.Expression.Comprehension in
  blocks
  |> Base.List.map ~f:(fun (_, { Block.left; right; _ }) ->
         get_children_nodes_pattern left @ get_children_nodes_expr right)
  |> List.flatten

and get_children_nodes_jsx_opening (_loc, { Ast.JSX.Opening.attributes; _ }) =
  let open Ast.JSX in
  attributes
  |> Base.List.map ~f:(fun attr ->
         match attr with
         | Opening.Attribute (_, { Attribute.value; _ }) ->
           node_list_of_option
             ~f:(fun value ->
               match value with
               | Attribute.ExpressionContainer
                   ( _,
                     { ExpressionContainer.expression = ExpressionContainer.Expression expression }
                   ) ->
                 get_children_nodes_expr expression
               | _ -> [])
             value
         | Opening.SpreadAttribute _ -> [])
  |> List.flatten

and get_children_nodes_jsx_child_list (_children_loc, children) =
  let open Ast.JSX in
  children
  |> Base.List.map ~f:(fun (loc, child) ->
         match child with
         | Element e -> get_children_nodes_expr (loc, Ast.Expression.JSXElement e)
         | Fragment f -> get_children_nodes_expr (loc, Ast.Expression.JSXFragment f)
         | ExpressionContainer { ExpressionContainer.expression } ->
           (match expression with
           | ExpressionContainer.Expression expression -> get_children_nodes_expr expression
           | _ -> [])
         | SpreadChild expr -> get_children_nodes_expr expr
         | Text _ -> [])
  |> List.flatten

and statement_of_expression (expression : (Loc.t, Loc.t) Ast.Expression.t) :
    (Loc.t, Loc.t) Ast.Statement.t =
  let (pos, _) = expression in
  let open Ast.Statement in
  (pos, Expression { Expression.expression; directive = None })

and statement_list_of_expression (expression : (Loc.t, Loc.t) Ast.Expression.t) :
    (Loc.t, Loc.t) Ast.Statement.t list =
  [statement_of_expression expression]

and statement_list_of_option = function
  | Some x -> [x]
  | None -> []

(* / comments.js#getSortedChildNodes *)
and fix_template_literals comment attach =
  match attach with
  | {
   CommentAttachCandidate.enclosing =
     Some
       ( _,
         Ast.Statement.Expression
           { Ast.Statement.Expression.expression = (_, Ast.Expression.TemplateLiteral lit); _ } );
   _;
  } ->
    retain_comments_inside_template_literal lit comment attach
  | _ -> attach

and retain_comments_inside_template_literal
    { Ast.Expression.TemplateLiteral.quasis; _ }
    comment
    { CommentAttachCandidate.preceding; following; enclosing } =
  let comment_index = find_expression_index_for_node quasis comment in
  let check_node node =
    match node with
    | Some n when find_expression_index_for_node quasis n <> comment_index -> None
    | any -> any
  in
  let preceding = check_node preceding in
  let following = check_node following in
  { CommentAttachCandidate.preceding; following; enclosing }

(* / comments.js#decorateComment *)
and position_comment comments ties comment attach_candidate =
  let { CommentAttachCandidate.preceding; following; enclosing } = attach_candidate in
  match (preceding, following, enclosing) with
  (* Patapam. FIXME the original attaches to the root of the AST *)
  | (None, None, None) -> raise Not_found
  (* Everything has precedence over enclosing *)
  | (None, Some following, Some _enclosing) ->
    (comments @ [(Js_layout_generator.Following, following, comment)], ties)
  | (Some preceding, None, Some _enclosing) ->
    (comments @ [(Js_layout_generator.Preceding, preceding, comment)], ties)
  (* No modifications required *)
  | (Some preceding, None, None) ->
    (comments @ [(Js_layout_generator.Preceding, preceding, comment)], ties)
  | (None, Some following, None) ->
    (comments @ [(Js_layout_generator.Following, following, comment)], ties)
  | (None, None, Some enclosing) ->
    (comments @ [(Js_layout_generator.Enclosing, enclosing, comment)], ties)
  (* Where the magic happens *)
  | (Some preceding, Some following, _) -> resolve_tie comments ties comment preceding following

and resolve_tie comments ties comment preceding following =
  let (following_loc, _) = following in
  let count = List.length ties in
  match count with
  | 0 -> (comments, ties @ [(comment, preceding, following)])
  | count ->
    let (_, _, (following_loc_last, _)) = List.nth ties (count - 1) in
    (match Loc.compare following_loc following_loc_last with
    | 0 -> (comments, ties @ [(comment, preceding, following)])
    | _ -> (break_tie comments ties, [(comment, preceding, following)]))

(* comments.js#breakTies *)
and break_tie comments = function
  | [] -> comments
  | ties ->
    (* tl;dr see if comments with the same preceding and following nodes are separated by newline,
     * Those comments after the newline are leading, the rest are trailing
     *
     * The original algorithm uses text lookup to break the ties.
     * The text isn't available here so I'll repro it using Loc.t
     *
     * I will reproduce the original docs for the next soul to gaze upon:
     *
     *  // Iterate backwards through tiesToBreak, examining the gaps
     *  // between the tied comments. In order to qualify as leading, a
     *  // comment must be separated from followingNode by an unbroken series of
     *  // gaps (or other comments). Gaps should only contain whitespace or open
     *  // parentheses.
     *)
    let reverse_ties = List.rev ties in
    let (leading, trailing) =
      List.fold_left
        (fun (leading, trailing) comment ->
          (* If we have not found a newline gap yet *)
          if trailing = [] then
            let (leading_comment, _, _) = List.hd leading in
            let ({ Loc._end = { Loc.line; _ }; _ }, _) = leading_comment in
            let line_previous = line in
            let (comment_original, _, _) = comment in
            let ({ Loc.start = { Loc.line; _ }; _ }, _) = comment_original in
            if abs (line_previous - line) >= 2 then
              (leading, [comment] @ trailing)
            else
              ([comment] @ leading, trailing)
          else
            (leading, [comment] @ trailing))
        ([List.hd reverse_ties], [])
        (List.tl reverse_ties)
    in
    let comments =
      List.fold_left
        (fun comments (comment, preceding, _) ->
          comments @ [(Js_layout_generator.Preceding, preceding, comment)])
        comments
        trailing
    in
    let comments =
      List.fold_left
        (fun comments (comment, _, following) ->
          comments @ [(Js_layout_generator.Following, following, comment)])
        comments
        leading
    in
    comments

(* / comments.js#breakTies *)
(* / comments.js#attach *)
