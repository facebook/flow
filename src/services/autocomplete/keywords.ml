(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type context_node =
  | Statement
  | Expression
  | ExpressionStatement
  | BindingIdentifier
  | Class
  | Member
  | ObjectKey
  | Type

(* TODO: include `of`, `in`, and `instanceof`. We don't currently autocomplete at positions where those are valid. *)
(* true, false, and null are not included here, because we already suggest those when we have type info *)
let expression_keywords =
  ["async"; "await"; "class"; "delete"; "function"; "import"; "new"; "typeof"; "void"; "yield"]

let statement_keywords =
  [
    "break";
    "case";
    "catch";
    "const";
    "continue";
    "debugger";
    "declare";
    "default";
    "do";
    "else";
    "enum";
    "export";
    "extends";
    "finally";
    "for";
    "if";
    "implements";
    "interface";
    "let";
    "opaque";
    "return";
    "static";
    "switch";
    "throw";
    "try";
    "type";
    "var";
    "void";
    "while";
  ]

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

    method! identifier (loc, id) =
      if this#is_target loc then raise (Found context);
      super#identifier (loc, id)
  end

let completion_item_of_keyword loc keyword =
  {
    ServerProt.Response.Completion.kind = Some Lsp.Completion.Keyword;
    name = keyword;
    labelDetail = None;
    description = None;
    itemDetail = None;
    text_edit = Some { ServerProt.Response.newText = keyword; insert = loc; replace = loc };
    additional_text_edits = [];
    sort_text = Some (Printf.sprintf "%020u" 0);
    preselect = false;
    documentation = None;
    tags = None;
    log_info = "keyword";
    insert_text_format = Lsp.Completion.PlainText;
  }

let keywords_of_context loc context =
  let keywords =
    match context with
    | Expression :: ExpressionStatement :: _ ->
      Base.List.append expression_keywords statement_keywords
    | Expression :: Member :: _ -> []
    | Expression :: _ -> expression_keywords
    | Statement :: _ -> statement_keywords
    | _ -> []
  in
  Base.List.map ~f:(completion_item_of_keyword loc) keywords

let keywords_at_loc ast loc =
  let mapper = new mapper loc in
  try
    ignore (mapper#program ast);
    []
  with
  | Found context -> keywords_of_context loc context
