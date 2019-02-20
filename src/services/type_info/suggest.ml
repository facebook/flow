(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type warning =
  | MissingFromTypeTables
  | NormalizerError of Ty_normalizer.error
  | NonFunctionType of string
  | Serializer of string
  | SkipEmpty

let warning_desc_to_string = function
  | MissingFromTypeTables ->
    Utils_js.spf "Location was not found in type tables."
  | NormalizerError err ->
    Utils_js.spf "Normalizer error:\n%s" (Ty_normalizer.error_to_string err)
  | NonFunctionType ty_str ->
    Utils_js.spf "Expected function type but got: %s" ty_str
  | Serializer err_msg ->
    Utils_js.spf "Type serializer failed with:\n%s" err_msg
  | SkipEmpty ->
    Utils_js.spf "Inferred type is empty."

class visitor ~cxs = object(this)
  inherit [unit, Loc.t] Flow_ast_visitor.visitor ~init:() as super

  val mutable _warnings = Errors.ConcreteLocPrintableErrorSet.empty

  method private warn loc (w: warning) =
    let open Errors in
    let desc = warning_desc_to_string w in
    _warnings <- ConcreteLocPrintableErrorSet.add (mk_error loc (Friendly.message_of_string desc)) _warnings;
    None

  method warnings () = _warnings

  method private inferred_type ~search ~index loc =
    let search_loc = search loc |> ALoc.of_loc in
    match Utils_js.ALocMap.get search_loc cxs with
    | Some (Ok ty) -> (
        match index ty with
        | Ok Ty.Bot ->
          this#warn loc SkipEmpty
        | Ok ty -> (
            match Ty_serializer.type_ ty with
            | Ok type_ast ->
              Some (Loc.none, type_ast)
            | Error desc -> this#warn loc (Serializer desc)
          )
        | Error err -> this#warn loc err
      )
    | Some (Error err) -> this#warn loc (NormalizerError err)
    | None -> this#warn loc MissingFromTypeTables

  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    let open Ast.Expression in
    match super#expression expr with
    | loc, Function x ->
      Flow_ast_mapper.id (this#function_return loc) x expr (fun x -> loc, Function x)
    | loc, ArrowFunction x ->
      Flow_ast_mapper.id (this#arrow_return loc) x expr  (fun x -> loc, ArrowFunction x)
    | expr -> expr

  method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
    let open Ast.Statement in
    match super#statement stmt with
    | (loc, FunctionDeclaration x) ->
      Flow_ast_mapper.id (this#function_return loc) x stmt (fun x -> loc, FunctionDeclaration x)
    | stmt -> stmt

  method! object_property (prop: (Loc.t, Loc.t) Ast.Expression.Object.Property.t) =
    let open Ast.Expression.Object.Property in
    let prop = super#object_property prop in
    match prop with
    | loc, Method { value = (fn_loc, fn); key } ->
      (* NOTE here we are indexing the type tables through the location of
         the entire method. The coverage tables should account for that.
         Alternatively, we could have used the location of the identifier,
         that gets logged in the type_info tables. (This would require some
         deeper unfolding.) For the moment we need both tables, but revisit
         this if this changes.
      *)
      let key' = this#object_key key in
      let fn' = this#method_return fn_loc fn in
      if key == key' && fn == fn' then prop
      else (loc, Method { key = key'; value = (fn_loc, fn') })
    | _ -> prop

  method! class_method loc (meth: (Loc.t, Loc.t) Ast.Class.Method.t') =
    let open Ast.Class.Method in
    let open Ast.Expression.Object.Property in
    let meth = super#class_method loc meth in
    let { key; value = (loc, func); _ } = meth in
    match key with
    | Identifier (id_loc, _) ->
      let func' = this#method_return id_loc func in
      { meth with value = (loc, func') }
    | _ -> meth

  method! function_param_pattern (expr: (Loc.t, Loc.t) Ast.Pattern.t) =
    let open Ast.Pattern in
    let (loc, patt) = expr in
    let patt' = match patt with
      | Identifier { Identifier.name; annot; optional } -> (
          match annot with
          | Ast.Type.Missing mis_loc ->
            let annot = this#inferred_type ~search:(fun x -> x)
              ~index:(fun x -> Ok x) loc in
            let annot = match annot with
            | Some annot -> Ast.Type.Available annot
            | None -> Ast.Type.Missing mis_loc in
            Identifier { Identifier.name; annot; optional }
          | Ast.Type.Available _ -> patt
        )
      | _ ->
        let _, patt' = super#function_param_pattern expr in
        patt'
    in
    if patt == patt' then expr else (loc, patt')

  method arrow_return loc func =
    this#callable_return ~search:(fun x -> x) loc func

  method method_return loc func =
    this#callable_return ~search:(fun x -> x) loc func

  (* Constructs that have keyword 'function', but may be missing a name. *)
  method function_return loc func =
    let open Ast.Function in
    let { id; _ } = func in
    let id = Option.map id ~f:(fun (loc, name) -> ALoc.of_loc loc, name) in
    let search loc =
      Type_table.function_decl_loc id (ALoc.of_loc loc) |> ALoc.to_loc
    in
    this#callable_return ~search loc func

  method callable_return ~search loc func =
    let open Ast.Function in
    let { return; _ } = func in
    let return' =
      match return with
      | Ast.Type.Available _ -> return
      | Ast.Type.Missing _ as miss ->
        let index = Ty.(function
          | Fun { fun_return; _ } -> Ok fun_return
          | ty -> Error (NonFunctionType (Ty_printer.string_of_t ty))
        ) in
        match this#inferred_type ~search ~index loc with
        | Some annot -> Ast.Type.Available annot
        | None -> miss
    in
    if return' == return
      then func
      else { func with return = return' }

end
