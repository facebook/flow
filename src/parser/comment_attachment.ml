(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast
open Parser_env

let id = Flow_ast_mapper.id

let map_loc = Flow_ast_mapper.map_loc

let map_opt = Flow_ast_mapper.map_opt

let id_list_last (map : 'a -> 'a) (lst : 'a list) : 'a list =
  match List.rev lst with
  | [] -> lst
  | hd :: tl ->
    let hd' = map hd in
    if hd == hd' then
      lst
    else
      List.rev (hd' :: tl)

(* Mapper that removes all trailing comments that appear after a given position in an AST node *)
class ['loc] trailing_comments_remover ~after_pos =
  object (this)
    inherit ['loc] Flow_ast_mapper.mapper

    method! syntax comments =
      let open Syntax in
      let { trailing; _ } = comments in
      let trailing' =
        List.filter (fun (loc, _) -> Loc.(pos_cmp loc.start after_pos < 0)) trailing
      in
      if List.length trailing = List.length trailing' then
        comments
      else
        { comments with trailing = trailing' }

    method! array _loc expr =
      let open Ast.Expression.Array in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! array_type t =
      let open Ast.Type.Array in
      let { comments; _ } = t in
      id this#syntax_opt comments t (fun comments' -> { t with comments = comments' })

    method! assignment _loc expr =
      let open Ast.Expression.Assignment in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! binary _loc expr =
      let open Ast.Expression.Binary in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! block _loc stmt =
      let open Ast.Statement.Block in
      let { comments; _ } = stmt in
      id this#syntax_opt comments stmt (fun comments' -> { stmt with comments = comments' })

    method! call _annot expr =
      let open Ast.Expression.Call in
      let { arguments; comments; _ } = expr in
      let arguments' = this#call_arguments arguments in
      let comments' = this#syntax_opt comments in
      if arguments == arguments' && comments == comments' then
        expr
      else
        { expr with arguments = arguments'; comments = comments' }

    method! call_arguments arg_list =
      let open Ast.Expression.ArgList in
      let (loc, { arguments; comments }) = arg_list in
      id this#syntax_opt comments arg_list (fun comments' ->
          (loc, { arguments; comments = comments' }))

    method! call_type_args targs =
      let open Ast.Expression.CallTypeArgs in
      let (loc, { arguments; comments }) = targs in
      id this#syntax_opt comments targs (fun comments' ->
          (loc, { arguments; comments = comments' }))

    method! class_ _loc cls =
      let open Ast.Class in
      let { body; comments; _ } = cls in
      let body' = this#class_body body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        cls
      else
        { cls with body = body'; comments = comments' }

    method! class_body body =
      let open Ast.Class.Body in
      let (loc, { body = _body; comments }) = body in
      id this#syntax_opt comments body (fun comments' ->
          (loc, { body = _body; comments = comments' }))

    method! class_extends _loc extends =
      let open Ast.Class.Extends in
      let { expr; targs; _ } = extends in
      if targs = None then
        id this#expression expr extends (fun expr' -> { extends with expr = expr' })
      else
        id (map_opt this#type_args) targs extends (fun targs' -> { extends with targs = targs' })

    method! class_implements implements =
      let open Ast.Class.Implements in
      let (loc, { interfaces; comments }) = implements in
      id (id_list_last this#class_implements_interface) interfaces implements (fun interfaces' ->
          (loc, { interfaces = interfaces'; comments }))

    method! class_implements_interface interface =
      let open Ast.Class.Implements.Interface in
      let (loc, { id = id_; targs }) = interface in
      if targs = None then
        id this#identifier id_ interface (fun id' -> (loc, { id = id'; targs }))
      else
        id (map_opt this#type_args) targs interface (fun targs' ->
            (loc, { id = id_; targs = targs' }))

    method! computed_key key =
      let open Ast.ComputedKey in
      let (loc, { expression; comments }) = key in
      id this#syntax_opt comments key (fun comments' -> (loc, { expression; comments = comments' }))

    method! conditional _loc expr =
      let open Ast.Expression.Conditional in
      let { alternate; comments; _ } = expr in
      let alternate' = this#expression alternate in
      let comments' = this#syntax_opt comments in
      if alternate == alternate' && comments = comments' then
        expr
      else
        { expr with alternate = alternate'; comments = comments' }

    method! function_ _loc func =
      let open Ast.Function in
      let { body; comments; _ } = func in
      let body' = this#function_body_any body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        func
      else
        { func with body = body'; comments = comments' }

    method! function_params (loc, params) =
      let open Ast.Function.Params in
      let { comments; _ } = params in
      id this#syntax_opt comments (loc, params) (fun comments' ->
          (loc, { params with comments = comments' }))

    method! function_type _loc func =
      let open Ast.Type.Function in
      let { return; comments; _ } = func in
      let return' = this#type_ return in
      let comments' = this#syntax_opt comments in
      if return == return' && comments == comments' then
        func
      else
        { func with return = return'; comments = comments' }

    method! generic_identifier_type git =
      let open Ast.Type.Generic.Identifier in
      match git with
      | Unqualified i -> id this#identifier i git (fun i -> Unqualified i)
      | Qualified (loc, ({ id; _ } as qualified)) ->
        let id' = this#identifier id in
        if id == id' then
          git
        else
          Qualified (loc, { qualified with id = id' })

    method! import _loc expr =
      let open Ast.Expression.Import in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! interface_type _loc t =
      let open Ast.Type.Interface in
      let { body; comments; _ } = t in
      let body' = map_loc this#object_type body in
      let comments' = this#syntax_opt comments in
      if body == body' && comments == comments' then
        t
      else
        { t with body = body'; comments = comments' }

    method! intersection_type _loc t =
      let { Ast.Type.Intersection.types = (t0, t1, ts); comments } = t in
      let (t1', ts') =
        match ts with
        | [] -> (this#type_ t1, [])
        | _ -> (t1, id_list_last this#type_ ts)
      in
      let comments' = this#syntax_opt comments in
      if t1 == t1' && ts == ts' && comments == comments' then
        t
      else
        { Ast.Type.Intersection.types = (t0, t1', ts'); comments = comments' }

    method! jsx_element _loc elem =
      let open Ast.JSX in
      let { comments; _ } = elem in
      id this#syntax_opt comments elem (fun comments' -> { elem with comments = comments' })

    method! jsx_fragment _loc frag =
      let open Ast.JSX in
      let { frag_comments = comments; _ } = frag in
      id this#syntax_opt comments frag (fun comments' -> { frag with frag_comments = comments' })

    method! logical _loc expr =
      let open Ast.Expression.Logical in
      let { right; comments; _ } = expr in
      let right' = this#expression right in
      let comments' = this#syntax_opt comments in
      if right == right' && comments == comments' then
        expr
      else
        { expr with right = right'; comments = comments' }

    method! new_ _loc expr =
      let open Ast.Expression.New in
      let { callee; targs; arguments; comments } = expr in
      let comments' = this#syntax_opt comments in
      match (targs, arguments) with
      (* new Callee<T>() *)
      | (_, Some _) ->
        let arguments' = map_opt this#call_arguments arguments in
        if arguments == arguments' && comments == comments' then
          expr
        else
          { expr with arguments = arguments'; comments = comments' }
      (* new Callee<T> *)
      | (Some _, _) ->
        let targs' = map_opt this#call_type_args targs in
        if targs == targs' && comments == comments' then
          expr
        else
          { expr with targs = targs'; comments = comments' }
      (* new Callee *)
      | (None, None) ->
        let callee' = this#expression callee in
        if callee == callee' && comments == comments' then
          expr
        else
          { expr with callee = callee'; comments = comments' }

    method! member _loc expr =
      let open Ast.Expression.Member in
      let { property; comments; _ } = expr in
      let property' = this#member_property property in
      let comments' = this#syntax_opt comments in
      if property == property' && comments == comments' then
        expr
      else
        { expr with property = property'; comments = comments' }

    method! object_ _loc expr =
      let open Ast.Expression.Object in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! object_type _loc obj =
      let open Ast.Type.Object in
      let { comments; _ } = obj in
      id this#syntax_opt comments obj (fun comments' -> { obj with comments = comments' })

    method! predicate pred =
      let open Ast.Type.Predicate in
      let (loc, { kind; comments }) = pred in
      id this#syntax_opt comments pred (fun comments' -> (loc, { kind; comments = comments' }))

    method! sequence _loc expr =
      let open Ast.Expression.Sequence in
      let { expressions; comments } = expr in
      let expressions' = id_list_last this#expression expressions in
      let comments' = this#syntax_opt comments in
      if expressions == expressions' && comments == comments' then
        expr
      else
        { expressions = expressions'; comments = comments' }

    method! template_literal _loc expr =
      let open Ast.Expression.TemplateLiteral in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! tuple_type t =
      let open Ast.Type.Tuple in
      let { comments; _ } = t in
      id this#syntax_opt comments t (fun comments' -> { t with comments = comments' })

    method! type_cast _loc expr =
      let open Ast.Expression.TypeCast in
      let { comments; _ } = expr in
      id this#syntax_opt comments expr (fun comments' -> { expr with comments = comments' })

    method! type_params tparams =
      let open Ast.Type.TypeParams in
      let (loc, { params; comments }) = tparams in
      id this#syntax_opt comments tparams (fun comments' -> (loc, { params; comments = comments' }))

    method! union_type _loc t =
      let { Ast.Type.Union.types = (t0, t1, ts); comments } = t in
      let (t1', ts') =
        match ts with
        | [] -> (this#type_ t1, [])
        | _ -> (t1, id_list_last this#type_ ts)
      in
      let comments' = this#syntax_opt comments in
      if t1 == t1' && ts == ts' && comments == comments' then
        t
      else
        { Ast.Type.Union.types = (t0, t1', ts'); comments = comments' }

    method! variable_declarator ~kind decl =
      let open Ast.Statement.VariableDeclaration.Declarator in
      let (loc, { id = ident; init }) = decl in
      match init with
      | None ->
        id (this#variable_declarator_pattern ~kind) ident decl (fun ident' ->
            (loc, { id = ident'; init }))
      | Some init ->
        id this#expression init decl (fun init' -> (loc, { id = ident; init = Some init' }))
  end

type trailing_and_remover_result = {
  trailing: Loc.t Comment.t list;
  remove_trailing: 'a. 'a -> (Loc.t trailing_comments_remover -> 'a -> 'a) -> 'a;
}

(* Returns a remover function which removes comments beginning after the previous token.
   No trailing comments are returned, since all comments since the last loc should be removed. *)
let trailing_and_remover_after_last_loc : Parser_env.env -> trailing_and_remover_result =
 fun env ->
  let open Loc in
  let remover =
    match Parser_env.last_loc env with
    | None -> None
    | Some _ when not (Peek.has_eaten_comments env) -> None
    | Some last_loc ->
      Parser_env.consume_comments_until env last_loc._end;
      let remover = new trailing_comments_remover ~after_pos:last_loc._end in
      Some remover
  in
  {
    trailing = [];
    remove_trailing =
      (fun node f ->
        match remover with
        | None -> node
        | Some remover -> f remover node);
  }

(* Consumes and returns comments on the same line as the previous token. Also returns a remover
   function which can be used to remove comments beginning after the previous token's line. *)
let trailing_and_remover_after_last_line : Parser_env.env -> trailing_and_remover_result =
 fun env ->
  let open Loc in
  let (trailing, remover) =
    match Parser_env.last_loc env with
    | None -> ([], None)
    | Some _ when not (Peek.has_eaten_comments env) -> (Eat.comments_until_next_line env, None)
    | Some last_loc ->
      Parser_env.consume_comments_until env last_loc._end;
      let trailing = Eat.comments_until_next_line env in
      let next_line_start = { line = last_loc._end.line + 1; column = 0 } in
      let remover = new trailing_comments_remover ~after_pos:next_line_start in
      (trailing, Some remover)
  in
  {
    trailing;
    remove_trailing =
      (fun node f ->
        match remover with
        | None -> node
        | Some remover -> f remover node);
  }

let trailing_and_remover : Parser_env.env -> trailing_and_remover_result =
 fun env ->
  if Peek.is_line_terminator env then
    trailing_and_remover_after_last_line env
  else
    trailing_and_remover_after_last_loc env

let id_remove_trailing env id =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing id (fun remover id -> remover#identifier id)

let expression_remove_trailing env expr =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing expr (fun remover expr -> remover#expression expr)

let block_remove_trailing env block =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing block (fun remover (loc, str) -> (loc, remover#block loc str))

let type_params_remove_trailing env tparams =
  match tparams with
  | None -> None
  | Some tparams ->
    let { remove_trailing; _ } = trailing_and_remover env in
    Some (remove_trailing tparams (fun remover tparams -> remover#type_params tparams))

let type_remove_trailing env ty =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing ty (fun remover ty -> remover#type_ ty)

let type_annotation_hint_remove_trailing env annot =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing annot (fun remover annot -> remover#type_annotation_hint annot)

let function_params_remove_trailing env params =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing params (fun remover params -> remover#function_params params)

let predicate_remove_trailing env pred =
  match pred with
  | None -> None
  | Some pred ->
    let { remove_trailing; _ } = trailing_and_remover env in
    Some (remove_trailing pred (fun remover pred -> remover#predicate pred))

let object_key_remove_trailing env key =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing key (fun remover key -> remover#object_key key)

let generic_type_remove_trailing env ty =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing ty (fun remover ty -> map_loc remover#generic_type ty)

let generic_type_list_remove_trailing env extends =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing extends (fun remover extends ->
      id_list_last (map_loc remover#generic_type) extends)

let class_implements_remove_trailing env implements =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing implements (fun remover impl -> remover#class_implements impl)

let string_literal_remove_trailing env str =
  let { remove_trailing; _ } = trailing_and_remover env in
  remove_trailing str (fun remover (loc, str) -> (loc, remover#string_literal_type loc str))

let statement_add_comments
    ((loc, stmt) : (Loc.t, Loc.t) Statement.t) (comments : (Loc.t, unit) Syntax.t option) :
    (Loc.t, Loc.t) Statement.t =
  let open Statement in
  let merge_comments inner = Flow_ast_utils.merge_comments ~inner ~outer:comments in
  let merge_comments_with_internal inner =
    Flow_ast_utils.merge_comments_with_internal ~inner ~outer:comments
  in
  ( loc,
    match stmt with
    | Block ({ Block.comments; _ } as s) ->
      Block { s with Block.comments = merge_comments_with_internal comments }
    | Break ({ Break.comments; _ } as s) ->
      Break { s with Break.comments = merge_comments comments }
    | ClassDeclaration ({ Class.comments; _ } as s) ->
      ClassDeclaration { s with Class.comments = merge_comments comments }
    | Continue ({ Continue.comments; _ } as s) ->
      Continue { s with Continue.comments = merge_comments comments }
    | Debugger { Debugger.comments } -> Debugger { Debugger.comments = merge_comments comments }
    | DeclareClass ({ DeclareClass.comments; _ } as s) ->
      DeclareClass { s with DeclareClass.comments = merge_comments comments }
    | DeclareExportDeclaration ({ DeclareExportDeclaration.comments; _ } as s) ->
      DeclareExportDeclaration
        { s with DeclareExportDeclaration.comments = merge_comments comments }
    | DeclareFunction ({ DeclareFunction.comments; _ } as s) ->
      DeclareFunction { s with DeclareFunction.comments = merge_comments comments }
    | DeclareInterface ({ Interface.comments; _ } as s) ->
      DeclareInterface { s with Interface.comments = merge_comments comments }
    | DeclareModule ({ DeclareModule.comments; _ } as s) ->
      DeclareModule { s with DeclareModule.comments = merge_comments comments }
    | DeclareModuleExports ({ DeclareModuleExports.comments; _ } as s) ->
      DeclareModuleExports { s with DeclareModuleExports.comments = merge_comments comments }
    | DeclareTypeAlias ({ TypeAlias.comments; _ } as s) ->
      DeclareTypeAlias { s with TypeAlias.comments = merge_comments comments }
    | DeclareOpaqueType ({ OpaqueType.comments; _ } as s) ->
      DeclareOpaqueType { s with OpaqueType.comments = merge_comments comments }
    | DeclareVariable ({ DeclareVariable.comments; _ } as s) ->
      DeclareVariable { s with DeclareVariable.comments = merge_comments comments }
    | DoWhile ({ DoWhile.comments; _ } as s) ->
      DoWhile { s with DoWhile.comments = merge_comments comments }
    | Empty { Empty.comments } -> Empty { Empty.comments = merge_comments comments }
    | EnumDeclaration ({ EnumDeclaration.comments; _ } as s) ->
      EnumDeclaration { s with EnumDeclaration.comments = merge_comments comments }
    | ExportDefaultDeclaration ({ ExportDefaultDeclaration.comments; _ } as s) ->
      ExportDefaultDeclaration
        { s with ExportDefaultDeclaration.comments = merge_comments comments }
    | ExportNamedDeclaration ({ ExportNamedDeclaration.comments; _ } as s) ->
      ExportNamedDeclaration { s with ExportNamedDeclaration.comments = merge_comments comments }
    | Expression ({ Expression.comments; _ } as s) ->
      Expression { s with Expression.comments = merge_comments comments }
    | For ({ For.comments; _ } as s) -> For { s with For.comments = merge_comments comments }
    | ForIn ({ ForIn.comments; _ } as s) ->
      ForIn { s with ForIn.comments = merge_comments comments }
    | ForOf ({ ForOf.comments; _ } as s) ->
      ForOf { s with ForOf.comments = merge_comments comments }
    | FunctionDeclaration ({ Function.comments; _ } as s) ->
      FunctionDeclaration { s with Function.comments = merge_comments comments }
    | If ({ If.comments; _ } as s) -> If { s with If.comments = merge_comments comments }
    | ImportDeclaration ({ ImportDeclaration.comments; _ } as s) ->
      ImportDeclaration { s with ImportDeclaration.comments = merge_comments comments }
    | InterfaceDeclaration ({ Interface.comments; _ } as s) ->
      InterfaceDeclaration { s with Interface.comments = merge_comments comments }
    | Labeled ({ Labeled.comments; _ } as s) ->
      Labeled { s with Labeled.comments = merge_comments comments }
    | Return ({ Return.comments; _ } as s) ->
      Return { s with Return.comments = merge_comments comments }
    | Switch ({ Switch.comments; _ } as s) ->
      Switch { s with Switch.comments = merge_comments comments }
    | Throw ({ Throw.comments; _ } as s) ->
      Throw { s with Throw.comments = merge_comments comments }
    | Try ({ Try.comments; _ } as s) -> Try { s with Try.comments = merge_comments comments }
    | TypeAlias ({ TypeAlias.comments; _ } as s) ->
      TypeAlias { s with TypeAlias.comments = merge_comments comments }
    | OpaqueType ({ OpaqueType.comments; _ } as s) ->
      OpaqueType { s with OpaqueType.comments = merge_comments comments }
    | VariableDeclaration ({ VariableDeclaration.comments; _ } as s) ->
      VariableDeclaration { s with VariableDeclaration.comments = merge_comments comments }
    | While ({ While.comments; _ } as s) ->
      While { s with While.comments = merge_comments comments }
    | With ({ With.comments; _ } as s) -> With { s with With.comments = merge_comments comments } )

(* Collects the first leading and last trailing comment on an AST node or its children.
   The first leading comment is the first attached comment that begins before the given node's loc,
   and the last trailing comment is the last attached comment that begins after the given node's loc. *)
class ['loc] comment_bounds_collector ~loc =
  object (this)
    inherit ['loc] Flow_ast_mapper.mapper

    val mutable first_leading = None

    val mutable last_trailing = None

    method comment_bounds = (first_leading, last_trailing)

    method collect_comments : 'internal. ('loc, 'internal) Syntax.t -> unit =
      function
      | { Syntax.leading; trailing; _ } ->
        List.iter this#visit_leading_comment leading;
        List.iter this#visit_trailing_comment trailing

    method collect_comments_opt =
      function
      | None -> ()
      | Some comments -> this#collect_comments comments

    method visit_leading_comment ((comment_loc, _) as comment) =
      let open Loc in
      match first_leading with
      | None -> if pos_cmp comment_loc.start loc.start < 0 then first_leading <- Some comment
      | Some (current_first_loc, _) ->
        if pos_cmp comment_loc.start current_first_loc.start < 0 then first_leading <- Some comment

    method visit_trailing_comment ((comment_loc, _) as comment) =
      let open Loc in
      match last_trailing with
      | None -> if pos_cmp comment_loc.start loc._end >= 0 then last_trailing <- Some comment
      | Some (current_last_loc, _) ->
        if pos_cmp current_last_loc.start comment_loc.start < 0 then last_trailing <- Some comment

    method! syntax comments =
      this#collect_comments comments;
      comments

    method! block _loc block =
      let { Statement.Block.comments; _ } = block in
      this#collect_comments_opt comments;
      block
  end

(* Given an AST node and a function to collect all its comments, return the first leading
   and last trailing comment on the node. *)
let comment_bounds loc node f =
  let collector = new comment_bounds_collector ~loc in
  ignore (f collector node);
  collector#comment_bounds

(* Return the first leading and last trailing comment of a statement *)
let statement_comment_bounds ((loc, _) as stmt : (Loc.t, Loc.t) Statement.t) :
    Loc.t Comment.t option * Loc.t Comment.t option =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#statement stmt);
  collector#comment_bounds

let expression_comment_bounds ((loc, _) as expr) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#expression expr);
  collector#comment_bounds

let block_comment_bounds (loc, block) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#block loc block);
  collector#comment_bounds

let object_property_comment_bounds property =
  let open Ast.Expression.Object in
  let collector =
    match property with
    | Property ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_property p);
      collector
    | SpreadProperty ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#spread_property p);
      collector
  in
  collector#comment_bounds

let object_type_property_comment_bounds property =
  let open Ast.Type.Object in
  let collector =
    match property with
    | Property ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_property_type p);
      collector
    | SpreadProperty ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_spread_property_type p);
      collector
    | Indexer ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_indexer_property_type p);
      collector
    | InternalSlot ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_internal_slot_property_type p);
      collector
    | CallProperty ((loc, _) as p) ->
      let collector = new comment_bounds_collector ~loc in
      ignore (collector#object_call_property_type p);
      collector
  in
  collector#comment_bounds

let switch_case_comment_bounds (loc, case) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#switch_case loc case);
  collector#comment_bounds

let function_param_comment_bounds (loc, param) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#function_param (loc, param));
  collector#comment_bounds

let function_rest_param_comment_bounds (loc, param) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#function_rest_param (loc, param));
  collector#comment_bounds

let function_type_param_comment_bounds (loc, param) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#function_param_type (loc, param));
  collector#comment_bounds

let function_type_rest_param_comment_bounds (loc, param) =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#function_rest_param_type (loc, param));
  collector#comment_bounds

let array_element_comment_bounds loc element =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#array_element element);
  collector#comment_bounds

let array_pattern_element_comment_bounds loc element =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#pattern_array_e element);
  collector#comment_bounds

let expression_or_spread_comment_bounds loc expr_or_spread =
  let collector = new comment_bounds_collector ~loc in
  ignore (collector#expression_or_spread expr_or_spread);
  collector#comment_bounds
