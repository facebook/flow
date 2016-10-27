(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module File_utils : sig
  type file_kind =
  | Dir of string
  | File of string

  val fold_files:
    ?max_depth:int -> ?filter:(string -> bool) -> ?file_only:bool ->
    string list ->
    (file_kind -> 'a -> 'a) ->
    'a ->
    'a
end = struct
  type file_kind =
  | Dir of string
  | File of string

  let lstat_kind file =
    let open Unix in
    try Some (lstat file).st_kind
    with Unix_error (ENOENT, _, _) ->
      prerr_endline ("File not found: "^file);
      None

  module FileSet = Set.Make(struct
    type t = file_kind
    let compare a b =
      match a, b with
      | Dir a', Dir b'
      | File a', File b' -> String.compare a' b'
      | Dir _, File _ -> 1
      | File _, Dir _ -> -1
  end)

  let fold_files (type t)
      ?max_depth ?(filter=(fun _ -> true)) ?(file_only = false)
      (paths: string list) (action: file_kind -> t -> t) (init: t) =
    let rec fold depth acc dir =
      let acc = if not file_only && filter dir
        then action (Dir dir) acc
        else acc
      in
      if max_depth = Some depth then
        acc
      else
        let files =
          Sys.readdir dir
          |> Array.fold_left (fun acc file ->
            let open Unix in
            let abs = Filename.concat dir file in
            match lstat_kind abs with
            | Some S_REG -> FileSet.add (File abs) acc
            | Some S_DIR -> FileSet.add (Dir abs) acc
            | _ -> acc
          ) FileSet.empty
        in
        FileSet.fold
          (fun entry acc ->
             match entry with
             | File file when filter file -> action (File file) acc
             | Dir file -> fold (depth+1) acc file
             | _ -> acc)
          files acc in
    List.fold_left (fold 0) init paths
end

module String_utils = struct
  let spf = Printf.sprintf

  let string_starts_with long short =
    try
      let long = String.sub long 0 (String.length short) in
      long = short
    with Invalid_argument _ ->
      false

  let strip_prefix prefix str =
    let prefix_length = String.length prefix in
    if string_starts_with str prefix
    then String.sub str prefix_length (String.length str - prefix_length)
    else str

  let split_extension str =
    let chopped = Filename.chop_extension str in
    let len = String.length str in
    let clen = String.length chopped in
    let ext = String.sub str (clen + 1) (len - clen - 1) in
    chopped, ext
end

module Hh_jsonTranslator : (
  Estree_translator.Translator with type t = Hh_json.json
) = struct
  open Hh_json

  type t = Hh_json.json

  let string x = JSON_String x
  let bool x = JSON_Bool x
  let obj props = JSON_Object (Array.to_list props)
  let array arr = JSON_Array (Array.to_list arr)
  let number x = JSON_Number (Utils_js.string_of_float_trunc x)
  let null = JSON_Null
  let regexp _loc _pattern _flags = JSON_Null
end

module Translate = Estree_translator.Translate (Hh_jsonTranslator)

module RunEsprimaTests : sig
  val main : string array -> unit
end = struct
  open Hh_json
  open String_utils

  module Ast = Spider_monkey_ast
  module SMap = Map.Make(String)
  module C = Tty

  let should_color =
    Sys.os_type <> "Win32" && Unix.isatty Unix.stdout && Sys.getenv "TERM" <> "dumb"

  let print to_print =
    if should_color
    then
      C.cprint to_print
    else
      let strings = List.map snd to_print in
      List.iter (Printf.printf "%s") strings

  type case_expectation =
    | Module of string
    | Tree of string
    | Tokens of string
    | Failure of string

  type case = {
    source: string option;
    expected: case_expectation option;
    diff: string option;
    skipped: string list;
  }

  type test = {
    test_name: string;
    cases: case SMap.t;
  }

  type case_result =
    | Case_ok
    | Case_skipped
    | Case_error of string list

  let empty_case = {
    source = None;
    expected = None;
    diff = None;
    skipped = [];
  }

  type path_part =
    | Prop of string
    | Index of int

  let rec string_of_path = function
    | [] -> "root"
    | Prop p :: rest -> spf "%s.%s" (string_of_path rest) p
    | Index i :: rest -> spf "%s[%s]" (string_of_path rest) (string_of_int i)

  let find_case name map = try SMap.find name map with Not_found -> empty_case

  let tests_of_path path =
    let relativize = strip_prefix path in
    File_utils.fold_files ~filter:(fun x -> x <> path) [path] (fun kind acc ->
      match kind with
      | File_utils.Dir dir ->
        let test = { test_name = relativize dir; cases = SMap.empty } in
        test::acc
      | File_utils.File file ->
        begin match acc with
        | test::rest ->
          let case_name = strip_prefix (test.test_name ^ Filename.dir_sep) (relativize file) in
          let case_name, ext = split_extension case_name in
          let cases = match ext with
          | "js" when Filename.check_suffix case_name ".source" ->
            let case_name = Filename.chop_suffix case_name ".source" in
            let case = find_case case_name test.cases in
            let case = { case with skipped = file::case.skipped } in
            SMap.add case_name case test.cases
          | "js" ->
            let case = find_case case_name test.cases in
            let source = Sys_utils.cat file in
            let case = { case with source = Some source; } in
            SMap.add case_name case test.cases
          | "json" ->
            let case_name, kind = split_extension case_name in
            let case = find_case case_name test.cases in
            let content = Sys_utils.cat file in
            let case = match kind with
            | "module" -> { case with expected = Some (Module content); }
            | "tree" -> { case with expected = Some (Tree content); }
            | "tokens" -> { case with expected = Some (Tokens content); }
            | "failure" -> { case with expected = Some (Failure content); }
            | _ -> { case with skipped = file::case.skipped }
            in
            SMap.add case_name case test.cases
          | "diff" ->
            let case = find_case case_name test.cases in
            let source = Sys_utils.cat file in
            let case = { case with diff = Some source; } in
            SMap.add case_name case test.cases
          | _ -> test.cases
          in
          ({ test with cases = cases })::rest
        | _ ->
          acc
        end
    ) []
    |> List.filter (fun test -> SMap.cardinal test.cases > 0)
    |> List.rev

  let expected_error_regex = Str.regexp "^Error: Line [0-9]+: \\(.*\\)$"

  let expected_different_property path expected actual =
    match path, expected, actual with
    | (Index _::Prop "errors"::[]), None, Some _ ->
      (* Don't ignore differences in errors *)
      false

    (* Flow always includes comments and locations *)
    | _, None, Some "comments"
    | _, None, Some "loc"
    | _, None, Some "range"
    | _, None, Some "source"

    (* Esprima doesn't support type annotations *)
    | _, None, Some "typeAnnotation"
    | _, None, Some "typeParameters"
    | _, None, Some "superTypeParameters"
    | _, None, Some "optional"
    | _, None, Some "returnType"
    | _, None, Some "predicate"
    | _, None, Some "implements"
    | _, None, Some "importKind"
    | _, None, Some "exportKind"

    (* Esprima doesn't support decorators *)
    (* https://github.com/estree/estree/blob/master/experimental/decorators.md *)
    | _, None, Some "decorators"

    (* Esprima doesn't support async functions *)
    | _, None, Some "async"

    (* TODO: Flow should include this *)
    | [], Some "sourceType", None

    (* TODO: enable this in tests *)
    | [], Some "tokens", None

    (* TODO: this hasn't been standardized yet. Esprima adds a "directive" field
       to ExpressionStatement to distinguish `"use strict"` from
       `("use strict")` or one with escaped characters.
        See https://github.com/jquery/esprima/issues/1006 and
        https://github.com/estree/estree/issues/6 *)
    | _, Some "directive", None

    (* Flow doesn't support this *)
    | _, Some "leadingComments", None
    | _, Some "trailingComments", None
    | _, Some "innerComments", None
      -> true

    | _ -> false

  let string_value_matches expected actual =
    if expected = actual then true
    else if Str.string_match expected_error_regex expected 0 then
      Str.matched_group 1 expected = actual
    else
      false

  let map_of_pairs pairs =
    List.fold_left (fun acc (key, value) ->
      SMap.add key value acc
    ) SMap.empty pairs

  let map_of_properties props =
    let open Ast.Expression in
    List.fold_left (fun acc prop ->
      match prop with
      | Object.Property (_loc, { Object.Property.
          key = Object.Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name; raw = _
          });
          value;
          kind = Object.Property.Init;
          _method = false; shorthand = false;
        }) ->
          SMap.add name value acc
      | _ -> failwith "Invalid JSON"
    ) SMap.empty props

  let rec test_tree
    (path: path_part list)
    (actual: Hh_json.json)
    (expected: Ast.Expression.t)
    (errors: string list)
  : string list =
    let open Ast.Expression in
    match actual, expected with
    | JSON_Object aprops,
      (_, Object { Object.properties = eprops }) ->
      let amap = map_of_pairs aprops in
      let emap = map_of_properties eprops in
      errors
      |> SMap.fold (test_actual_prop path emap) amap
      |> SMap.fold (test_expected_prop path amap) emap
    | JSON_Array aitems,
      (_, Array { Array.elements = eitems }) ->
      let a_len = List.length aitems in
      let e_len = List.length eitems in
      if e_len <> a_len then
        let path = string_of_path path in
        let err = spf "%s: Expected %d elements, got %d." path e_len a_len in
        err::errors
      else
        let _, diffs = List.fold_left2 (fun (i, acc) actual expected ->
          let path = (Index i)::path in
          let acc = match expected with
          | Some (Expression expr) ->
              test_tree path actual expr acc
          | _ -> (spf "%s: invalid JSON" (string_of_path path))::acc
          in
          i + 1, acc
        ) (0, errors) aitems eitems in
        diffs
    | JSON_Bool actual,
      (_, Literal { Ast.Literal.
        value = Ast.Literal.Boolean expected;
        raw = _;
      }) ->
        if actual <> expected then
          let path = string_of_path path in
          (spf "%s: Expected %b, got %b." path expected actual)::errors
        else
          errors
    | JSON_Number actual,
      (_, Literal { Ast.Literal.
        value = Ast.Literal.Number _;
        raw = expected;
      }) ->
        if actual <> expected then
          let path = string_of_path path in
          (spf "%s: Expected %s, got %s." path expected actual)::errors
        else
          errors
    | JSON_String actual,
      (_, Literal { Ast.Literal.
        value = Ast.Literal.String expected;
        raw = _;
      }) ->
        if not (string_value_matches expected actual) then
          let path = string_of_path path in
          (spf "%s: Expected %S, got %S." path expected actual)::errors
        else
          errors
    | JSON_Null,
      (_, Literal { Ast.Literal.
        value = Ast.Literal.Null;
        raw = _;
      }) ->
        errors
    | _, _ ->
        let path = string_of_path path in
        (spf "%s: Types do not match" path)::errors

  and test_actual_prop path expected_map name value acc =
    if SMap.mem name expected_map then
      let expected_value = SMap.find name expected_map in
      test_tree ((Prop name)::path) value expected_value acc
    else if expected_different_property path None (Some name) then
      acc
    else
      let path = string_of_path path in
      (spf "%s: Unexpected key %S" path name)::acc

  and test_expected_prop path actual_map name _ acc =
    if SMap.mem name actual_map ||
       expected_different_property path (Some name) None
    then acc
    else
      let path = string_of_path path in
      (spf "%s: Missing key %S" path name)::acc

  let prop_name_and_value = Ast.Expression.(function
    | { Object.Property.
        key = Object.Property.Literal (_, {
          Ast.Literal.value = Ast.Literal.String name; raw = _
        });
        value;
        kind = Object.Property.Init;
        _method = false; shorthand = false;
      } -> name, value
    | _ -> failwith "Invalid JSON"
  )

  let has_prop
      (needle: string)
      (haystack: Ast.Expression.Object.property list) =
    List.exists Ast.Expression.(function
      | Object.Property (_, prop) -> fst (prop_name_and_value prop) = needle
      | _ -> false
    ) haystack

  let rec apply_diff diff expected =
    let open Ast.Expression in
    match diff with
    | (_, Object { Object.properties = diff_props; _ }) ->
      begin match expected with
      | (loc, Object { Object.properties = expected_props; }) ->
        let properties = List.fold_left (fun props diff_prop ->
          match diff_prop with
          | Object.Property (diff_loc, diff_prop) ->
            let diff_name, diff_value = prop_name_and_value diff_prop in
            if not (has_prop diff_name props) then
              Object.Property (diff_loc, diff_prop)::props
            else List.fold_left (fun acc exp -> match exp with
              | Object.Property (exp_loc, exp_prop) ->
                let exp_name, exp_value = prop_name_and_value exp_prop in
                if exp_name = diff_name then
                  (* recursively apply diff *)
                  match apply_diff diff_value exp_value with
                  | Some value ->
                    let prop = Object.Property (exp_loc, { exp_prop with
                      Object.Property.value = value;
                    }) in
                    prop::acc
                  | None ->
                    acc
                else
                  let prop = Object.Property (exp_loc, exp_prop) in
                  prop::acc
              | prop -> prop::acc
            ) [] props
          | _ -> failwith "Invalid JSON"
        ) expected_props diff_props in
        Some (loc, Object { Object.properties })
      | (loc, Array { Array.elements = expected_elems; }) ->
        let expected_length = List.length expected_elems in
        let elements = List.fold_left (fun elems diff_prop ->
          match diff_prop with
          | Object.Property (_, diff_prop) ->
            let diff_name, diff_value = prop_name_and_value diff_prop in
            let diff_index = int_of_string diff_name in
            if diff_index >= expected_length then
              (* append the diff *)
              (* TODO: this should insert gaps, but I don't expect people to
                 write diffs that have gaps. *)
              List.rev ((Some (Expression diff_value))::(List.rev elems))
            else
              (* apply the diff *)
              List.mapi (fun index elem ->
                if index <> diff_index then elem
                else match elem with
                | None ->
                  Some (Expression diff_value)
                | Some (Expression exp_value) ->
                  begin match apply_diff diff_value exp_value with
                  | Some value -> Some (Expression value)
                  | None -> None
                  end
                | Some (Spread _) ->
                  failwith "Invalid JSON"
              ) elems
          | _ -> failwith "Invalid JSON"
        ) expected_elems diff_props in
        Some (loc, Array { Array.elements })
      | _ -> Some expected
      end
    | (_, Literal _) -> Some diff
    | (_, Identifier (_, "undefined")) ->
        None
    | _ -> failwith "Invalid diff format"

  let run_case case : case_result =
    match case.source with
    | None ->
      if List.length case.skipped = 0 then Case_error ["No source"]
      else Case_skipped
    | Some content ->
      let (ast, errors) = Parser_flow.program_file
        ~fail:false content (Some (Loc.SourceFile "42.js")) in
      let actual = match Translate.program ast with
      | JSON_Object params ->
          let params =
            if List.length errors > 0 then
              ("errors", Translate.errors errors)::params
            else params in
          JSON_Object params
      | _ -> assert false
      in
      begin match case.expected with
      | Some (Module _) -> (* TODO *) Case_skipped
      | Some (Tree tree) ->
          let expected = fst (Parser_flow.json_file ~fail:true tree None) in
          let expected = match case.diff with
          | Some str ->
            let diffs = fst (Parser_flow.json_file ~fail:true str None) in
            begin match apply_diff diffs expected with
            | Some x -> x
            | None -> failwith "unexpected diff: removed everything";
            end
          | None ->
            expected
          in
          let errors = test_tree [] actual expected [] in
          if List.length errors = 0 then Case_ok
          else Case_error errors
      | Some (Tokens _) -> (* TODO *) Case_skipped
      | Some (Failure _) -> (* TODO *) Case_skipped
      | None -> Case_error ["Nothing to do"]
      end

  type test_results = {
    ok: int;
    skipped: int;
    failed: int;
  }

  type suite_results = {
    ok_tests: int;
    ok_cases: int;
    skipped_tests: int;
    skipped_cases: int;
    failed_tests: int;
    failed_cases: int;
  }

  let empty_suite_results = {
    ok_tests = 0;
    ok_cases = 0;
    skipped_tests = 0;
    skipped_cases = 0;
    failed_tests = 0;
    failed_cases = 0;
  }

  let add_results suite test =
    { suite with
      ok_cases = suite.ok_cases + test.ok;
      skipped_cases = suite.skipped_cases + test.skipped;
      failed_cases = suite.failed_cases + test.failed;
    }

  let main argv =
    let path = match Array.to_list argv with
    | _cmd::path::[] -> path
    | _ -> prerr_endline "Invalid usage"; exit 1
    in
    let tests = tests_of_path path in
    let results = List.fold_left (fun results { test_name; cases; } ->
      print [C.Bold C.White, spf "=== %s ===\n" test_name];
      let test_results = SMap.fold (fun key case results ->
        (* print [C.Normal C.Default, spf "[ ] %s\r" key]; *)
        match run_case case with
        | Case_ok ->
          (* print [
            C.Normal C.Green, "[✓] PASS";
            C.Normal C.Default, spf ": %s\n" key
          ]; *)
          { results with ok = results.ok + 1 }
        | Case_skipped ->
          (* print [
            C.Normal C.Yellow, "[-] SKIP";
            C.Normal C.Default, spf ": %s\n" key
          ]; *)
          { results with skipped = results.skipped + 1 }
        | Case_error errs ->
          print [
            C.Normal C.Red, "[✗] FAIL";
            C.Normal C.Default, spf ": %s\n" key
          ];
          List.iter (fun err ->
            print [C.Normal C.Default, spf "    %s\n" err];
          ) errs;
          { results with failed = results.failed + 1 }
      ) cases { ok = 0; skipped = 0; failed = 0; } in
      print_endline "";
      let results = add_results results test_results in
      let results = if test_results.skipped > 0 then
        { results with skipped_tests = results.skipped_tests + 1; }
      else
        results
      in
      if test_results.failed > 0 then
        { results with failed_tests = results.failed_tests + 1; }
      else
        { results with ok_tests = results.ok_tests + 1; }
    ) empty_suite_results tests in

    if results.failed_tests = 0 then
      print [
        C.Bold C.Default, spf "Passed: %d (%d cases), Failed: %d (%d cases), Skipped: %d (%d cases)\n"
          results.ok_tests results.ok_cases
          results.failed_tests results.failed_cases
          results.skipped_tests results.skipped_cases
      ]
    else
      print [
        C.Bold C.Default, spf "Passed: %d (%d cases), "
          results.ok_tests results.ok_cases;
        C.BoldWithBG (C.White, C.Red), spf "Failed: %d (%d cases)"
          results.failed_tests results.failed_cases;
        C.Bold C.Default, spf ", Skipped: %d cases\n"
          results.skipped_cases;
      ]

end

let _ = RunEsprimaTests.main (Sys.argv)
