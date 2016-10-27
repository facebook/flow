(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js
open Span

module Ast = Spider_monkey_ast

(**************************)

let unwrap_comment = Ast.Comment.(function
  loc, Block str | loc, Line str -> (loc, str)
)

let add_comment start _end cloc cstr cmap =
  let span = make_span start _end in
  SpanMap.add span (cloc, cstr) cmap

let make_comment_map progspan = function
  | [] -> SpanMap.empty
  | comment :: comments -> Loc.(
      let (lastloc, laststr), map = List.fold_left (
        fun ((prevloc, prevstr), map) c ->
          let nextloc, nextstr = unwrap_comment c in
          let map = add_comment prevloc.start nextloc.start
            prevloc prevstr map in
          (nextloc, nextstr), map
        ) (unwrap_comment comment, SpanMap.empty) comments
      in
      (* last comment *)
      add_comment lastloc.start progspan._end lastloc laststr map
    )

(*****************)
(* dynamic types *)
(*****************)

type dynamic =
  | ParamD of string * string
  | RetD of string
  | TypeD of Loc.t * string

let rec parse_docblock loc = function
  | [t] ->
      (try
         let t = parse_dynamic_type t in
         [TypeD (loc, t)]
       with _ -> [])

  | "@param" :: t :: x :: xs ->
      (try
         let t = parse_dynamic_type t in
         [ParamD (x, t)]
       with _ -> []) @
      (parse_docblock loc xs)

  | "@return" :: t :: xs ->
      (try
         let t = parse_dynamic_type t in
         [RetD t]
       with _ -> []) @
      (parse_docblock loc xs)

  | _ :: xs -> parse_docblock loc xs

  | [] -> []

and parse_dynamic_type_ i s = match s with

  | _ when Str.string_match (Str.regexp "array<") s i ->
      let t,i = parse_dynamic_type_ (Str.match_end()) s in
      assert (Str.string_match (Str.regexp ">") s i);
      spf "Array<%s>" t, Str.match_end()

  | _ when Str.string_match (Str.regexp "function():") s i ->
      let t,i = parse_dynamic_type_ i s in
      spf "() => %s" t, i

  | _ when Str.string_match (Str.regexp "function(") s i ->
      let ts,i = parse_dynamic_types [] (Str.match_end()) "):" "," s in
      let ts = ts |> List.rev_map ((^) "_: ") |> String.concat ", " in
      let t,i = parse_dynamic_type_ i s in
      spf "(%s) => %s" ts t, i

  | _ when Str.string_match (Str.regexp "?") s i ->
      let t,i = parse_dynamic_type_ (Str.match_end()) s in
      spf "?%s" t, i

  | _ when Str.string_match (Str.regexp "{") s i ->
      let t,i = parse_dynamic_type_ (Str.match_end()) s in
      assert (Str.string_match (Str.regexp "}") s i);
      t, Str.match_end()

  | _ when Str.string_match (Str.regexp "array") s i ->
      "Array<any>", Str.match_end()
  | _ when Str.string_match (Str.regexp "boolean") s i ->
      "boolean", Str.match_end()
  | _ when Str.string_match (Str.regexp "date") s i ->
      "Date", Str.match_end()
  | _ when Str.string_match (Str.regexp "function") s i ->
      "(_: ...Array<any>) => any", Str.match_end()
  | _ when Str.string_match (Str.regexp "number") s i ->
      "number", Str.match_end()
  | _ when Str.string_match (Str.regexp "object") s i ->
      "{}", Str.match_end()
  | _ when Str.string_match (Str.regexp "RegExp") s i ->
      "RegExp", Str.match_end()
  | _ when Str.string_match (Str.regexp "string") s i ->
      "string", Str.match_end()
  | _ when Str.string_match (Str.regexp "*") s i ->
      "any", Str.match_end()
  | _ when Str.string_match (Str.regexp "[A-Za-z$_][A-Za-z$_0-9]*") s i ->
      Str.matched_string s, Str.match_end()

  | _ -> assert false

and parse_dynamic_types ts i close sep s =
  let t,i = parse_dynamic_type_ i s in
  match s with
  | _ when Str.string_match (Str.regexp close) s i ->
      t::ts, Str.match_end()

  | _ when Str.string_match (Str.regexp sep) s i ->
      parse_dynamic_types (t::ts) (Str.match_end()) close sep s

  | _ -> assert false

and parse_dynamic_type s =
  let ts,i = parse_dynamic_types [] 0 "$" "|" s in
  assert (i = String.length s);
  match ts with
  | [t] -> t
  | _ -> spf "$Either<%s>" (ts |> List.rev |> String.concat ", ")

(* if there is a comment whose scope spans the given location,
   return the map with that comment removed, and parsed type annos
 *)
let mk_comment cmap loc =
  match SpanMap.get loc cmap with
  | Some (loc, cstr) ->
      let words = Str.split (Str.regexp "[ \t\n\\*/]+") cstr in
      SpanMap.remove loc cmap, parse_docblock loc words
  | _ ->
      cmap, []

let meta_fun cmap loc =
  let cmap, annos = mk_comment cmap loc in
  let tmap = List.fold_left (fun map -> function
    | ParamD (x,t) -> map |> SMap.add x t
    | RetD t -> map |> SMap.add (Reason.internal_name "return") t
    | _ -> map
  ) SMap.empty annos in
  cmap, tmap

let insert_before_with_suffix loc t suffix = Loc.(
  loc.start.line, loc.start.column, spf ": %s%s" t suffix
)

let insert_after loc t = Loc.(
  loc._end.line, loc._end.column, spf ": %s" t
)

let skip loc = Loc.(
  (* NOTE: leave multi-line comments alone for now *)
  let n = if loc.start.line <> loc._end.line then 0
    else loc._end.column - loc.start.column in
  loc.start.line, loc.start.column, string_of_int n
)

(* concat_map with an accumulator *)
let concat_fold f acc items =
  let acc, lists = List.fold_left (fun (acc, lists) item ->
    let acc, list = f acc item in
    acc, list :: lists
  ) (acc, []) items in
  acc, List.concat lists

let meta_params params map cmap =
  concat_fold Ast.Pattern.(fun cmap -> function
    | nloc, Identifier { Ast.Pattern.Identifier.name = (_, name); _ } -> (
        match SMap.get name map with
          | Some t -> cmap, [insert_after nloc t]
          | None ->
              let cmap, annos = mk_comment cmap nloc in
              cmap, match annos with
                | [TypeD (cloc, ctype)] ->
                    [skip cloc; insert_after nloc ctype]
                | _ ->
                    []
        )
    | _ -> cmap, []
  ) cmap params

let meta_return body map cmap =
  let bloc = Ast.Function.(match body with
    | BodyBlock (loc, _) -> loc
    | BodyExpression (loc, _) -> loc (* probably wrong, it's after the => *)
  ) in
  match SMap.get (Reason.internal_name "return") map with
    | Some t ->
        cmap, [insert_before_with_suffix bloc t " "]
    | None ->
        let cmap, annos = mk_comment cmap bloc in
        cmap, match annos with
          | [TypeD (cloc, ctype)] ->
              [skip cloc; insert_after cloc ctype]
          | _ ->
              []

let rec meta_array_element cmap = Ast.Expression.(function
  | Some (Expression e) -> meta_expression cmap e
  | Some (Spread (_, { SpreadElement.argument = e })) ->
      meta_expression cmap e
  | None ->
      cmap, []
)

and meta_expression_or_spread cmap = Ast.Expression.(function
  | Expression e ->
      meta_expression cmap e
  | Spread (_, { SpreadElement.argument }) ->
      meta_expression cmap argument
)

and meta_fbody cmap loc params body =
  let cmap, tmap = meta_fun cmap loc in
  concat_fold (fun cmap f -> f cmap) cmap [
    meta_params params tmap;
    meta_return body tmap;
    meta_body body
  ]

and meta_expression cmap = Ast.Expression.(function
  | _, Object { Object.properties } ->
      concat_fold (fun cmap -> function
        | Object.Property (loc, {
            Object.Property.value = (_, Function {
              Ast.Function.params = (params, _); body; _
            });
            key = Ast.Expression.Object.Property.Identifier _;
            _
          }) ->
            meta_fbody cmap loc params body

        | Object.Property (_, { Object.Property.value = v ; _ }) ->
            meta_expression cmap v

        | _ -> cmap, [] (* TODO? *)
      ) cmap properties

  | _, Array { Array.elements } ->
      concat_fold meta_array_element cmap elements

  | _, Call { Call.arguments; _ } ->
      concat_fold meta_expression_or_spread cmap arguments

  | _, Assignment { Assignment.right; _ } ->
      meta_expression cmap right

  | loc, Function { Ast.Function.params = (params, _); body; _ }
  | loc, ArrowFunction { Ast.Function.params = (params, _); body; _ } ->
      meta_fbody cmap loc params body

  | _ -> cmap, []
)

and meta_variable cmap (_, vdecl) = Ast.Statement.VariableDeclaration.(
  let { Declarator.init; _ } = vdecl in
  match init with
  | Some expr -> meta_expression cmap expr
  | None -> cmap, []
)

and meta_statement cmap = Ast.Statement.(function
  | _, VariableDeclaration { VariableDeclaration.declarations; _ } ->
      concat_fold meta_variable cmap declarations

  | _, Expression { Expression.expression = e } ->
      meta_expression cmap e

  | _, ClassDeclaration { Ast.Class.body; _ } ->
      let _, { Ast.Class.Body.body = elements; _ } = body in
      concat_fold Ast.Class.(fun cmap -> function
        | Body.Method (loc, {
            Method.key = Ast.Expression.Object.Property.Identifier _;
            value = _, { Ast.Function.params = (params, _); body; _ };
            kind = Method.Method | Method.Constructor;
            static = false;
            decorators = _;
          }) ->
            meta_fbody cmap loc params body
        | _ -> cmap, []
      ) cmap elements

  | loc, FunctionDeclaration { Ast.Function.params = (params, _); body; _ } ->
      meta_fbody cmap loc params body

  | _ -> cmap, [] (* TODO *)
)

and meta_body body cmap = Ast.Statement.(
  match body with
    | Ast.Function.BodyBlock (_, { Block.body }) ->
        meta_statements cmap body
    | Ast.Function.BodyExpression expr ->
        meta_expression cmap expr
)

and meta_statements cmap = concat_fold meta_statement cmap

let meta_program (loc, statements, comments) =
  let cmap = make_comment_map loc comments in
  let _, edits = meta_statements cmap statements in
  edits
