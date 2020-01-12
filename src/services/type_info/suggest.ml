(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type warning =
  | MissingFromTypeTables
  | NormalizerError of string
  | NonFunctionType of string
  | Serializer of string
  | SkipEmpty

let warning_desc_to_string = function
  | MissingFromTypeTables -> Utils_js.spf "Location was not found in type tables."
  | NormalizerError err -> Utils_js.spf "Normalizer error:\n%s" err
  | NonFunctionType ty_str -> Utils_js.spf "Expected function type but got: %s" ty_str
  | Serializer err_msg -> Utils_js.spf "Type serializer failed with:\n%s" err_msg
  | SkipEmpty -> Utils_js.spf "Inferred type is empty."

class visitor ~ty_query =
  object (this)
    inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

    val mutable _warnings = Errors.ConcreteLocPrintableErrorSet.empty

    method private warn loc (w : warning) =
      Errors.(
        let desc = warning_desc_to_string w in
        let err = mk_error loc (Friendly.message_of_string desc) in
        _warnings <- ConcreteLocPrintableErrorSet.add err _warnings;
        None)

    method warnings () = _warnings

    method private inferred_type ?blame_loc ?annotate_bottom:(ann_bot = false) loc =
      let blame_loc =
        match blame_loc with
        | Some bloc -> bloc
        | None -> loc
      in
      match ty_query loc with
      | Query_types.Success (_, ty) ->
        begin
          match ty with
          | Ty.Bot _ when not ann_bot -> this#warn blame_loc SkipEmpty
          | _ ->
            begin
              match Ty_serializer.type_ ty with
              | Ok type_ast -> Some (Loc.none, type_ast)
              | Error desc -> this#warn blame_loc (Serializer desc)
            end
        end
      | Query_types.FailureUnparseable (_, _, msg) -> this#warn blame_loc (NormalizerError msg)
      | Query_types.FailureNoMatch -> this#warn blame_loc MissingFromTypeTables

    method! expression (expr : (Loc.t, Loc.t) Ast.Expression.t) =
      let open Ast.Expression in
      let expr' = super#expression expr in
      match expr' with
      | (loc, Function x) ->
        Flow_ast_mapper.id (this#callable_return loc) x expr' (fun x -> (loc, Function x))
      | (loc, ArrowFunction x) ->
        Flow_ast_mapper.id (this#callable_return loc) x expr' (fun x -> (loc, ArrowFunction x))
      | _ -> expr'

    method! statement (stmt : (Loc.t, Loc.t) Ast.Statement.t) =
      let open Ast.Statement in
      let stmt' = super#statement stmt in
      match stmt' with
      | (loc, FunctionDeclaration x) ->
        Flow_ast_mapper.id (this#callable_return loc) x stmt' (fun x ->
            (loc, FunctionDeclaration x))
      | _ -> stmt'

    method! object_property (prop : (Loc.t, Loc.t) Ast.Expression.Object.Property.t) =
      let open Ast.Expression.Object.Property in
      let prop' = super#object_property prop in
      match prop' with
      | (loc, Method { value = (fn_loc, fn); key }) ->
        (* NOTE here we are indexing the type tables through the location of
         the entire method. The coverage tables should account for that.
         Alternatively, we could have used the location of the identifier,
         that gets logged in the type_info tables. (This would require some
         deeper unfolding.) For the moment we need both tables, but revisit
         this if this changes.
      *)
        let key' = this#object_key key in
        let fn' = this#callable_return fn_loc fn in
        if key == key' && fn == fn' then
          prop'
        else
          (loc, Method { key = key'; value = (fn_loc, fn') })
      | _ -> prop'

    method! class_method loc (meth : (Loc.t, Loc.t) Ast.Class.Method.t') =
      let open Ast.Class.Method in
      let open Ast.Expression.Object.Property in
      let meth' = super#class_method loc meth in
      let { key; value = (loc, func); _ } = meth' in
      match key with
      | Identifier (id_loc, _) ->
        let func' = this#callable_return id_loc func in
        { meth' with value = (loc, func') }
      | _ -> meth'

    method! function_param_pattern (patt : (Loc.t, Loc.t) Ast.Pattern.t) =
      let open Ast.Pattern in
      Identifier.(
        let patt' = super#function_param_pattern patt in
        match patt' with
        | (loc, Identifier ({ annot = Ast.Type.Missing _; _ } as id)) ->
          begin
            match this#inferred_type loc with
            | Some annot -> (loc, Identifier { id with annot = Ast.Type.Available annot })
            | None -> patt'
          end
        | _ -> patt')

    method callable_return loc func =
      let open Ast.Function in
      let { return; _ } = func in
      match return with
      | Ast.Type.Available _ -> func
      | Ast.Type.Missing missing_loc ->
        begin
          match this#inferred_type ~blame_loc:loc ~annotate_bottom:true missing_loc with
          | Some annot -> { func with return = Ast.Type.Available annot }
          | None -> func
        end
  end
