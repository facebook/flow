(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type context_node =
  | Statement
  | ExportDefault
  | Expression
  | ExpressionStatement
  | BindingIdentifier
  | Class
  | Member
  | ObjectKey
  | Type
  | SwitchCase
  | MatchPattern

(* TODO: include `of`, `in`, and `instanceof`. We don't currently autocomplete at positions where those are valid. *)
(* true, false, and null are not included here, because we already suggest those when we have type info *)
let expression_keywords ~pattern_matching_enabled =
  ["async"; "await"; "class"; "delete"; "function"; "import"; "new"; "typeof"; "void"; "yield"]
  @
  if pattern_matching_enabled then
    ["match"]
  else
    []

(** keywords to suggest in a statement (or expression statement) context, in
    almost-alphabetical order.

    the order here only matters amongst items with the same starting letter.
    for example, const is more common than class, which is more common than
    catch or continue; function is more common than finally.

    this way, when you type `c` and Quick Suggest triggers, we'll suggest
    [const, class, ...], instead of [case, catch, class, const, continue].
    you can also trigger Quick Suggest without typing anything (cmd+space),
    but that's sorted purely alphabetically. *)
let statement_keywords ~component_syntax_enabled =
  [
    "await";
    "async";
    "break";
    "const";
    "class";
    "case";
    "catch";
    "continue";
    "default";
    "debugger";
    "declare";
    "delete";
    "do";
    "else";
    "enum";
    "export";
    "extends";
    "function";
    "for";
    "finally";
    "if";
    "import";
    "implements";
    "interface";
    "let";
    "new";
    "opaque";
    "return";
    "static";
    "switch";
    "throw";
    "try";
    "type";
    "typeof";
    "var";
    "void";
    "while";
    "yield";
  ]
  @
  if component_syntax_enabled then
    ["component"; "hook"]
  else
    []

(** keywords that can appear after export default *)
let export_default_keywords ~component_syntax_enabled =
  [
    (* export default async function ... *)
    "async";
    (* The following keywords should be keep in sync with logic of Statement_parser.export_declaration *)
    "class";
    "function";
    "enum";
    (* export default new MyClass() ... *)
    "new";
  ]
  @
  if component_syntax_enabled then
    ["component"; "hook"]
  else
    []

let match_pattern_keywords = ["const"]

exception Found of context_node list

class mapper target =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target as super

    val mutable context = []

    method private with_context : 'a. context_node -> (unit -> 'a) -> 'a =
      fun node f ->
        let old_context = context in
        context <- node :: context;
        let result = f () in
        context <- old_context;
        result

    method! expression expr = this#with_context Expression (fun () -> super#expression expr)

    method! statement stmt = this#with_context Statement (fun () -> super#statement stmt)

    method! export_default_declaration loc stmt =
      this#with_context ExportDefault (fun () -> super#export_default_declaration loc stmt)

    method! expression_statement loc stmt =
      this#with_context ExpressionStatement (fun () -> super#expression_statement loc stmt)

    method! binding_type_identifier id =
      this#with_context BindingIdentifier (fun () -> super#binding_type_identifier id)

    method! pattern_identifier ?kind id =
      this#with_context BindingIdentifier (fun () -> super#pattern_identifier ?kind id)

    method! class_ loc cls = this#with_context Class (fun () -> super#class_ loc cls)

    method! member_property expr = this#with_context Member (fun () -> super#member_property expr)

    method! object_key key = this#with_context ObjectKey (fun () -> super#object_key key)

    method! type_ t = this#with_context Type (fun () -> super#type_ t)

    method! switch_case case = this#with_context SwitchCase (fun () -> super#switch_case case)

    method! match_pattern pattern =
      this#with_context MatchPattern (fun () -> super#match_pattern pattern)

    method! identifier (loc, id) =
      if this#target_contained_by loc then raise (Found context);
      super#identifier (loc, id)
  end

let keywords_of_context ~component_syntax_enabled ~pattern_matching_enabled context =
  match context with
  | Expression :: ExpressionStatement :: _
  | Statement :: _ ->
    statement_keywords ~component_syntax_enabled
  | ExportDefault :: _
  | Expression :: ExportDefault :: _ ->
    export_default_keywords ~component_syntax_enabled
  | Expression :: Member :: _
  | Expression :: SwitchCase :: _ ->
    []
  | Expression :: _ -> expression_keywords ~pattern_matching_enabled
  | MatchPattern :: _ when pattern_matching_enabled -> match_pattern_keywords
  | _ -> []

let keywords_at_loc ~component_syntax_enabled ~pattern_matching_enabled ast loc =
  (* We're looking for an identifier, considering the first character is equivalent. *)
  let target = Loc.first_char loc in
  let mapper = new mapper target in
  try
    ignore (mapper#program ast);
    []
  with
  | Found context -> keywords_of_context ~component_syntax_enabled ~pattern_matching_enabled context
