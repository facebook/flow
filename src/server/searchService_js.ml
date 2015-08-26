(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Spider_monkey_ast
module Json = Hh_json

type search_result_type =
  | Class
  | Function

let result_type_to_string = function
  | Class -> "class"
  | Function -> "function"

let result_to_string (term: (Pos.t, search_result_type) SearchUtils.term) =
  SearchUtils.(
  Printf.sprintf "%s: %s, %s"
    term.name
    (Pos.string (Pos.to_absolute term.pos))
    (result_type_to_string term.result_type)
)

let pos_range p = Pos.(Lexing.(
  line p, start_cnum p + 1,
    (pos_end p).pos_lnum, (pos_end p).pos_cnum
))

(* TODO: deprecate this in favor of Reason_js.json_of_loc *)
let pos_to_json pos =
  let file = Pos.filename pos in
  let l0, c0, l1, c1 = pos_range pos in
  [ "path", Json.JString (Relative_path.to_absolute file);
    "line", Json.JInt l0;
    "endline", Json.JInt l1;
    "start", Json.JInt c0;
    "end", Json.JInt c1 ]

let result_to_json term =
  SearchUtils.(
  Json.JAssoc (
    ("name", Json.JString term.name) ::
    ("type", Json.JString (result_type_to_string term.result_type)) ::
    (pos_to_json term.pos)
  ))

module SS = SearchService.Make(struct
  type t = search_result_type
  let fuzzy_types = [Class; Function]
  let type_num = function
    | Class -> 1
    | Function -> 2
  let compare_result_type a b =
    (type_num a) - (type_num b)
end)

let lex_pos_of_loc filename p =
  { Lexing.pos_fname = filename;
    pos_lnum = p.Loc.line;
    pos_bol = p.Loc.column;
    pos_cnum = p.Loc.offset;
  }

let pos_of_loc loc =
  let fn =
    match loc.Loc.source with
    | None -> assert false
    | Some x -> x
  in
  Pos.make_from_lexing_pos
    ~pos_file:(Relative_path.(create Dummy fn))
    ~pos_start:(lex_pos_of_loc fn loc.Loc.start)
    ~pos_end:(lex_pos_of_loc fn loc.Loc._end)

let add_fuzzy_term id type_ acc =
  let loc, id = id in
  let key = id.Ast.Identifier.name in
  let pos = pos_of_loc loc in
  SS.WorkerApi.process_fuzzy_term key key pos type_ acc

(* Called by a worker after the file is parsed *)
let update fn ast =
  let fn = Relative_path.create Relative_path.Dummy fn in
  let _location, stmt_l, _commands = ast in
  let fuzzy_defs, trie_defs =
    List.fold_left begin fun (fuzzy_defs, trie_defs) (_, def) ->
      match def with
      | Ast.Statement.FunctionDeclaration
         { Ast.Statement.FunctionDeclaration.id = Some f_name; _ } ->
          let fuzzy_defs = add_fuzzy_term f_name Function fuzzy_defs in
          fuzzy_defs, trie_defs
      | Ast.Statement.DeclareFunction df ->
          let f_name = df.Ast.Statement.DeclareFunction.id in
          let fuzzy_defs = add_fuzzy_term f_name Function fuzzy_defs in
          fuzzy_defs, trie_defs
      | Ast.Statement.ClassDeclaration
         { Ast.Class.id = Some f_name; _ }
      | Ast.Statement.DeclareClass
         { Ast.Statement.Interface.id = f_name; _ } ->
          let fuzzy_defs = add_fuzzy_term f_name Class fuzzy_defs in
          fuzzy_defs, trie_defs
      | _ -> fuzzy_defs, trie_defs
    end (SS.Fuzzy.TMap.empty, []) stmt_l in
  SS.WorkerApi.update fn trie_defs fuzzy_defs

let query input =
  SS.MasterApi.query input None

let update_from_master files =
  (* hack's search service operates on Relative_paths, so we have to convert *)
  let files = ServerEnv.PathSet.fold (fun p acc ->
    let p = Path.to_string p in
    Relative_path.Set.add (Relative_path.create Relative_path.Dummy p) acc
  ) files Relative_path.Set.empty in
  SS.MasterApi.update_search_index files

let clear paths =
  (* hack's search service operates on Relative_paths, so we have to convert *)
  let paths = ServerEnv.PathSet.fold (fun p acc ->
    let p = Path.to_string p in
    Relative_path.Set.add (Relative_path.create Relative_path.Dummy p) acc
  ) paths Relative_path.Set.empty in
  SS.MasterApi.clear_shared_memory paths
