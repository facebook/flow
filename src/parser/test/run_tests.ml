(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module String_utils = struct
  let spf = Printf.sprintf

  let string_starts_with long short =
    try
      let long = String.sub long 0 (String.length short) in
      long = short
    with Invalid_argument _ -> false

  let strip_prefix prefix str =
    let prefix_length = String.length prefix in
    if string_starts_with str prefix then
      String.sub str prefix_length (String.length str - prefix_length)
    else
      str

  let split_extension str =
    let chopped = Filename.chop_extension str in
    let len = String.length str in
    let clen = String.length chopped in
    let ext = String.sub str (clen + 1) (len - clen - 1) in
    (chopped, ext)
end

module RunEsprimaTests : sig
  val main : unit -> unit
end = struct
  open Hh_json
  open String_utils
  module SMap = Map.Make (String)
  module C = Tty

  let should_color =
    Sys.os_type <> "Win32" && Unix.isatty Unix.stdout && Sys.getenv "TERM" <> "dumb"

  let print to_print =
    ( if should_color then
      C.cprint to_print
    else
      let strings = Base.List.map ~f:snd to_print in
      List.iter (Printf.printf "%s") strings );
    flush stdout

  type case_expectation =
    | Module of string
    | Tree of string
    | Tokens of string
    | Failure of string

  type case_diff =
    | Diff of string (* json of diffs to apply *)
    | Todo of string (* reason *)
    | Same

  type test_options = { intern_comments: bool }

  type case = {
    source: string option;
    options: test_options * Parser_env.parse_options option;
    expected: case_expectation option;
    diff: case_diff;
    skipped: string list;
  }

  type test = {
    test_name: string;
    cases: case SMap.t;
  }

  type case_result =
    | Case_ok
    | Case_skipped of string option (* reason *)
    | Case_error of string list

  let empty_case =
    {
      source = None;
      options = ({ intern_comments = false }, None);
      expected = None;
      diff = Same;
      skipped = [];
    }

  type path_part =
    | Prop of string
    | Index of int

  let rec string_of_path = function
    | [] -> "root"
    | Prop p :: rest -> spf "%s.%s" (string_of_path rest) p
    | Index i :: rest -> spf "%s[%s]" (string_of_path rest) (string_of_int i)

  let find_case name map = (try SMap.find name map with Not_found -> empty_case)

  let parse_options content =
    Base.Result.(
      let get_bool k v =
        try return (Hh_json.get_bool_exn v)
        with Assert_failure _ -> failf "invalid value for %S, expected bool" k
      in
      return (Hh_json.json_of_string content) >>= fun json ->
      begin
        match json with
        | Hh_json.JSON_Object props -> return props
        | _ -> fail "expected options to be a JSON object"
      end
      >>= fun props ->
      List.fold_left
        (fun opts (k, v) ->
          opts >>= fun (test_opts, opts) ->
          match k with
          | "enums" ->
            get_bool k v >>= fun v -> return (test_opts, { opts with Parser_env.enums = v })
          | "esproposal_class_instance_fields" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_class_instance_fields = v })
          | "esproposal_class_static_fields" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_class_static_fields = v })
          | "esproposal_decorators" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_decorators = v })
          | "esproposal_export_star_as" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_export_star_as = v })
          | "esproposal_optional_chaining" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_optional_chaining = v })
          | "esproposal_nullish_coalescing" ->
            get_bool k v >>= fun v ->
            return (test_opts, { opts with Parser_env.esproposal_nullish_coalescing = v })
          | "types" ->
            get_bool k v >>= fun v -> return (test_opts, { opts with Parser_env.types = v })
          | "use_strict" ->
            get_bool k v >>= fun v -> return (test_opts, { opts with Parser_env.use_strict = v })
          | "intern_comments" -> get_bool k v >>= fun v -> return ({ intern_comments = v }, opts)
          | _ -> failf "unknown option %S" k)
        (return ({ intern_comments = false }, Parser_env.default_parse_options))
        props)

  let tests_of_path path =
    let relativize = strip_prefix path in
    File_utils.fold_files
      ~filter:(fun x -> x <> path)
      [path]
      (fun kind acc ->
        match kind with
        | File_utils.Dir dir ->
          let test = { test_name = relativize dir; cases = SMap.empty } in
          test :: acc
        | File_utils.File file ->
          begin
            match acc with
            | test :: rest ->
              let case_name = strip_prefix (test.test_name ^ Filename.dir_sep) (relativize file) in
              let (case_name, ext) = split_extension case_name in
              let cases =
                match ext with
                | "js" when Filename.check_suffix case_name ".source" ->
                  let case_name = Filename.chop_suffix case_name ".source" in
                  let case = find_case case_name test.cases in
                  let case = { case with skipped = file :: case.skipped } in
                  SMap.add case_name case test.cases
                | "js" ->
                  let case = find_case case_name test.cases in
                  let source = Sys_utils.cat file in
                  let case = { case with source = Some source } in
                  SMap.add case_name case test.cases
                | "json" ->
                  let (case_name, kind) = split_extension case_name in
                  let case = find_case case_name test.cases in
                  let content = Sys_utils.cat file in
                  let case =
                    match kind with
                    | "module" -> { case with expected = Some (Module content) }
                    | "tree" -> { case with expected = Some (Tree content) }
                    | "tokens" -> { case with expected = Some (Tokens content) }
                    | "failure" -> { case with expected = Some (Failure content) }
                    | "options" ->
                      (* TODO: propagate errors better *)
                      let options = Base.Result.ok_or_failwith (parse_options content) in
                      { case with options = (fst options, Some (snd options)) }
                    | _ -> { case with skipped = file :: case.skipped }
                  in
                  SMap.add case_name case test.cases
                | "diff" ->
                  let case = find_case case_name test.cases in
                  let source = Sys_utils.cat file in
                  let case = { case with diff = Diff source } in
                  SMap.add case_name case test.cases
                | "skip" ->
                  let case = find_case case_name test.cases in
                  let skip = Sys_utils.cat file |> String.trim in
                  let case = { case with diff = Todo skip } in
                  SMap.add case_name case test.cases
                | _ -> test.cases
              in
              { test with cases } :: rest
            | _ -> acc
          end)
      []
    |> List.filter (fun test -> SMap.cardinal test.cases > 0)
    |> List.rev

  let expected_error_regex = Str.regexp "^Error: Line [0-9]+: \\(.*\\)$"

  let expected_different_property path expected actual =
    match (path, expected, actual) with
    | ([Index _; Prop "errors"], None, Some _) ->
      (* Don't ignore differences in errors *)
      false
    (* Flow always includes comments and locations *)
    | (_, None, Some "comments")
    | (_, None, Some "loc")
    | (_, None, Some "range")
    | (_, None, Some "source")
    (* Esprima doesn't support type annotations *)
    | (_, None, Some "typeAnnotation")
    | (_, None, Some "typeParameters")
    | (_, None, Some "superTypeParameters")
    | (_, None, Some "optional")
    | (_, None, Some "returnType")
    | (_, None, Some "predicate")
    | (_, None, Some "implements")
    | (_, None, Some "importKind")
    | (_, None, Some "exportKind")
    (* Esprima doesn't support decorators *)
    (* https://github.com/estree/estree/blob/master/experimental/decorators.md *)
    | (_, None, Some "decorators")
    (* Esprima doesn't support async functions *)
    | (_, None, Some "async")
    (* TODO: Flow should include this *)
    | ([], Some "sourceType", None)
    (* TODO: enable this in tests *)
    | ([], Some "tokens", None)
    (* Flow doesn't support this *)
    | (_, Some "leadingComments", None)
    | (_, Some "trailingComments", None)
    | (_, Some "innerComments", None) ->
      true
    | _ -> false

  let string_value_matches expected actual =
    if expected = actual then
      true
    else if Str.string_match expected_error_regex expected 0 then
      Str.matched_group 1 expected = actual
    else
      false

  let map_of_pairs pairs =
    List.fold_left (fun acc (key, value) -> SMap.add key value acc) SMap.empty pairs

  let map_of_properties props =
    let open Ast.Expression in
    List.fold_left
      (fun acc prop ->
        match prop with
        | Object.Property
            ( _loc,
              Object.Property.Init
                {
                  key =
                    Object.Property.Literal
                      (_, { Ast.Literal.value = Ast.Literal.String name; raw = _; comments = _ });
                  value;
                  shorthand = false;
                } ) ->
          SMap.add name value acc
        | _ -> failwith "Invalid JSON")
      SMap.empty
      props

  let string_of_json_type = function
    | JSON_Object _ -> "object"
    | JSON_Array _ -> "array"
    | JSON_String _ -> "string"
    | JSON_Number _ -> "number"
    | JSON_Bool _ -> "bool"
    | JSON_Null -> "null"

  let rec test_tree
      (path : path_part list)
      (actual : Hh_json.json)
      (expected : (Loc.t, Loc.t) Ast.Expression.t)
      (errors : string list) : string list =
    let open Ast.Expression in
    match (actual, expected) with
    | (JSON_Object aprops, (_, Object { Object.properties = eprops; comments = _ })) ->
      let amap = map_of_pairs aprops in
      let emap = map_of_properties eprops in
      errors
      |> SMap.fold (test_actual_prop path emap) amap
      |> SMap.fold (test_expected_prop path amap) emap
    | (JSON_Array aitems, (_, Array { Array.elements = eitems; comments = _ })) ->
      let a_len = List.length aitems in
      let e_len = List.length eitems in
      if e_len <> a_len then
        let path = string_of_path path in
        let err = spf "%s: Expected %d elements, got %d." path e_len a_len in
        err :: errors
      else
        let (_, diffs) =
          List.fold_left2
            (fun (i, acc) actual expected ->
              let path = Index i :: path in
              let acc =
                match expected with
                | Some (Expression expr) -> test_tree path actual expr acc
                | _ -> spf "%s: invalid JSON" (string_of_path path) :: acc
              in
              (i + 1, acc))
            (0, errors)
            aitems
            eitems
        in
        diffs
    | ( JSON_Bool actual,
        (_, Literal { Ast.Literal.value = Ast.Literal.Boolean expected; raw = _; comments = _ }) )
      ->
      if actual <> expected then
        let path = string_of_path path in
        spf "%s: Expected %b, got %b." path expected actual :: errors
      else
        errors
    | ( JSON_Number actual,
        (_, Literal { Ast.Literal.value = Ast.Literal.Number _; raw = expected; comments = _ }) ) ->
      if actual <> expected then
        let path = string_of_path path in
        spf "%s: Expected %s, got %s." path expected actual :: errors
      else
        errors
    | ( JSON_Number actual,
        ( _,
          Unary
            {
              Unary.operator = Unary.Minus;
              argument =
                ( _,
                  Literal { Ast.Literal.value = Ast.Literal.Number _; raw = expected; comments = _ }
                );
              comments = _;
            } ) ) ->
      let expected = "-" ^ expected in
      if actual <> expected then
        let path = string_of_path path in
        spf "%s: Expected %s, got %s." path expected actual :: errors
      else
        errors
    | ( JSON_String actual,
        (_, Literal { Ast.Literal.value = Ast.Literal.String expected; raw = _; comments = _ }) ) ->
      if not (string_value_matches expected actual) then
        let path = string_of_path path in
        spf "%s: Expected %S, got %S." path expected actual :: errors
      else
        errors
    | (JSON_Null, (_, Literal { Ast.Literal.value = Ast.Literal.Null; raw = _; comments = _ })) ->
      errors
    | (_, _) ->
      let path = string_of_path path in
      let act_type = string_of_json_type actual in
      spf "%s: Types do not match, got %s" path act_type :: errors

  and test_actual_prop path expected_map name value acc =
    if SMap.mem name expected_map then
      let expected_value = SMap.find name expected_map in
      test_tree (Prop name :: path) value expected_value acc
    else if value = JSON_Null then
      (* allow Flow to have extra properties when their values are null. this is
         a narrower exception than `expected_different_property`; that function
         allows the field to have any value, but sometimes we want to allow Flow
         to return consistent shapes, without allowing arbitrary differences. *)
      acc
    else if expected_different_property path None (Some name) then
      acc
    else
      let path = string_of_path path in
      spf "%s: Unexpected key %S" path name :: acc

  and test_expected_prop path actual_map name _ acc =
    if SMap.mem name actual_map || expected_different_property path (Some name) None then
      acc
    else
      let path = string_of_path path in
      spf "%s: Missing key %S" path name :: acc

  let prop_name_and_value =
    let open Ast.Expression in
    function
    | Object.Property.Init
        {
          key =
            Object.Property.Literal
              (_, { Ast.Literal.value = Ast.Literal.String name; raw = _; comments = _ });
          value;
          shorthand = false;
        } ->
      (name, value)
    | _ -> failwith "Invalid JSON"

  let has_prop (needle : string) (haystack : (Loc.t, Loc.t) Ast.Expression.Object.property list) =
    List.exists
      Ast.Expression.(
        function
        | Object.Property (_, prop) -> fst (prop_name_and_value prop) = needle
        | _ -> false)
      haystack

  let rec apply_diff diff expected =
    let open Ast.Expression in
    match diff with
    | (_, Object { Object.properties = diff_props; _ }) ->
      begin
        match expected with
        | (loc, Object { Object.properties = expected_props; comments }) ->
          let properties =
            List.fold_left
              (fun props diff_prop ->
                match diff_prop with
                | Object.Property (diff_loc, diff_prop) ->
                  let (diff_name, diff_value) = prop_name_and_value diff_prop in
                  if not (has_prop diff_name props) then
                    Object.Property (diff_loc, diff_prop) :: props
                  else
                    List.fold_left
                      (fun acc exp ->
                        match exp with
                        | Object.Property (exp_loc, exp_prop) ->
                          let exp_key =
                            match exp_prop with
                            | Object.Property.Init { key; _ } -> key
                            | _ -> failwith "Invalid JSON"
                          in
                          let (exp_name, exp_value) = prop_name_and_value exp_prop in
                          if exp_name = diff_name then
                            (* recursively apply diff *)
                            match apply_diff diff_value exp_value with
                            | Some value ->
                              let prop =
                                Object.Property
                                  ( exp_loc,
                                    Object.Property.Init { key = exp_key; value; shorthand = false }
                                  )
                              in
                              prop :: acc
                            | None -> acc
                          else
                            let prop = Object.Property (exp_loc, exp_prop) in
                            prop :: acc
                        | prop -> prop :: acc)
                      []
                      props
                | _ -> failwith "Invalid JSON")
              expected_props
              diff_props
          in
          Some (loc, Object { Object.properties; comments })
        | (loc, Array { Array.elements = expected_elems; comments }) ->
          let expected_length = List.length expected_elems in
          let elements =
            List.fold_left
              (fun elems diff_prop ->
                match diff_prop with
                | Object.Property (_, diff_prop) ->
                  let (diff_name, diff_value) = prop_name_and_value diff_prop in
                  let diff_index = int_of_string diff_name in
                  if diff_index >= expected_length then
                    (* append the diff *)
                    (* TODO: this should insert gaps, but I don't expect people to
                 write diffs that have gaps. *)
                    List.rev (Some (Expression diff_value) :: List.rev elems)
                  else
                    (* apply the diff *)
                    List.mapi
                      (fun index elem ->
                        if index <> diff_index then
                          elem
                        else
                          match elem with
                          | None -> Some (Expression diff_value)
                          | Some (Expression exp_value) ->
                            begin
                              match apply_diff diff_value exp_value with
                              | Some value -> Some (Expression value)
                              | None -> None
                            end
                          | Some (Spread _) -> failwith "Invalid JSON")
                      elems
                | _ -> failwith "Invalid JSON")
              expected_elems
              diff_props
          in
          Some (loc, Array { Array.elements; comments })
        | _ -> Some expected
      end
    | (_, Literal _) -> Some diff
    | (_, Identifier (_, { Ast.Identifier.name = "undefined"; comments = _ })) -> None
    | _ -> failwith "Invalid diff format"

  let parse_file (test_options, parse_options) content =
    let (ast, errors) = Parser_flow.program_file ~fail:false ~parse_options content None in
    let offset_table = Some (Offset_utils.make ~kind:Offset_utils.JavaScript content) in
    let module Translate =
      Estree_translator.Translate
        (Json_of_estree)
        (struct
          let include_interned_comments = test_options.intern_comments

          let include_comments = true

          let include_locs = true
        end)
    in
    match Translate.program offset_table ast with
    | JSON_Object params ->
      let params =
        if errors = [] then
          params
        else
          ("errors", Translate.errors errors) :: params
      in
      JSON_Object params
    | _ -> assert false

  let run_case case : case_result =
    match case.source with
    | None ->
      if List.length case.skipped = 0 && case.diff = Same then
        Case_error ["No source"]
      else
        Case_skipped None
    | Some content ->
      let actual = parse_file case.options content in
      let (diff, todo) =
        match case.diff with
        | Diff str -> (Some str, None)
        | Todo str -> (None, Some str)
        | Same -> (None, None)
      in
      begin
        match case.expected with
        | Some (Module _) -> (* TODO *) Case_skipped None
        | Some (Tree tree) ->
          let parse_result =
            try
              let (expected, json_errors) = Parser_flow.json_file ~fail:false tree None in
              (Some expected, json_errors)
            with Parse_error.Error errs -> (None, errs)
          in
          (match parse_result with
          | (_, (loc, err) :: _) ->
            let str =
              Printf.sprintf
                "Unable to parse .tree.json: %s: %s\n\nContents: %s"
                (Loc.debug_to_string loc)
                (Parse_error.PP.error err)
                tree
            in
            Case_error [str]
          | (None, []) -> Case_error ["Unable to parse .tree.json: unknown error"]
          | (Some expected, []) ->
            let expected =
              match diff with
              | Some str ->
                let diffs = fst (Parser_flow.json_file ~fail:true str None) in
                begin
                  match apply_diff diffs expected with
                  | Some x -> x
                  | None -> failwith "unexpected diff: removed everything"
                end
              | None -> expected
            in
            let errors = test_tree [] actual expected [] in
            begin
              match (errors, todo) with
              | ([], None) -> Case_ok
              | ([], Some _) -> Case_error ["Skipped test passes"]
              | (_, Some reason) -> Case_skipped (Some reason)
              | (_, None) -> Case_error errors
            end)
        | Some (Tokens _) -> (* TODO *) Case_skipped None
        | Some (Failure _) -> (* TODO *) Case_skipped None
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
    skipped_cases: int;
    failed_tests: int;
    failed_cases: int;
  }

  let empty_suite_results =
    { ok_tests = 0; ok_cases = 0; skipped_cases = 0; failed_tests = 0; failed_cases = 0 }

  let add_results suite test =
    {
      suite with
      ok_cases = suite.ok_cases + test.ok;
      skipped_cases = suite.skipped_cases + test.skipped;
      failed_cases = suite.failed_cases + test.failed;
    }

  let record_tree path test_name case_name case =
    match (case.source, case.expected) with
    | (Some content, None)
    | (Some content, Some (Tree _)) ->
      let ( / ) a b = a ^ Filename.dir_sep ^ b in
      let filename = (path / test_name / case_name) ^ ".tree.json" in
      let json = parse_file case.options content in
      let oc = open_out filename in
      output_string oc (Hh_json.json_to_multiline json);
      output_char oc '\n';
      close_out oc
    | _ -> ()

  type verbose_mode =
    | Quiet
    | Verbose
    | Normal

  let main () =
    let verbose_ref = ref Normal in
    let record_ref = ref false in
    let path_ref = ref None in
    let speclist =
      [
        ("-q", Arg.Unit (fun () -> verbose_ref := Quiet), "Enables quiet mode");
        ("-v", Arg.Unit (fun () -> verbose_ref := Verbose), "Enables verbose mode");
        ("-r", Arg.Set record_ref, "Re-record failing expected trees");
      ]
    in
    let usage_msg = "Runs flow parser on esprima tests. Options available:" in
    Arg.parse speclist (fun anon -> path_ref := Some anon) usage_msg;
    let path =
      match !path_ref with
      | Some path -> path
      | None ->
        prerr_endline "Invalid usage";
        exit 1
    in
    let quiet = !verbose_ref = Quiet in
    let verbose = !verbose_ref = Verbose in
    let record = !record_ref in
    let tests = tests_of_path path in
    let results =
      List.fold_left
        (fun results { test_name; cases } ->
          if not quiet then print [(C.Bold C.White, spf "=== %s ===\n" test_name)];
          let (test_results, _) =
            SMap.fold
              (fun key case (results, shown_header) ->
                (* print [C.Normal C.Default, spf "[ ] %s\r" key]; *)
                match run_case case with
                | Case_ok ->
                  if verbose then
                    print
                      [
                        (C.Normal C.Green, "[\xE2\x9C\x93] PASS");
                        (C.Normal C.Default, spf ": %s\n" key);
                      ];
                  ({ results with ok = results.ok + 1 }, shown_header)
                | Case_skipped reason ->
                  begin
                    match reason with
                    | Some ""
                    | None ->
                      ()
                    | Some reason ->
                      if not quiet then
                        print
                          [
                            (C.Normal C.Yellow, "[-] SKIP");
                            (C.Normal C.Default, spf ": %s - %s\n" key reason);
                          ]
                  end;
                  ({ results with skipped = results.skipped + 1 }, shown_header)
                | Case_error errs ->
                  if quiet && not shown_header then
                    print [(C.Bold C.White, spf "=== %s ===\n" test_name)];
                  print
                    [
                      (C.Normal C.Red, "[\xE2\x9C\x97] FAIL"); (C.Normal C.Default, spf ": %s\n" key);
                    ];
                  List.iter (fun err -> print [(C.Normal C.Default, spf "    %s\n" err)]) errs;
                  flush stdout;
                  if record then record_tree path test_name key case;
                  ({ results with failed = results.failed + 1 }, true)
                | exception exn ->
                  let exn = Exception.wrap exn in
                  if quiet && not shown_header then
                    print [(C.Bold C.White, spf "=== %s ===\n" test_name)];
                  print
                    [
                      (C.Normal C.Red, "[\xE2\x9C\x97] FAIL");
                      (C.Normal C.Default, spf ": %s\n" key);
                      (C.Normal C.Default, spf "    %s\n" (Exception.to_string exn));
                    ];
                  flush stdout;
                  ({ results with failed = results.failed + 1 }, true))
              cases
              ({ ok = 0; skipped = 0; failed = 0 }, false)
          in
          if not quiet then print_endline "";
          let results = add_results results test_results in
          if test_results.failed > 0 then
            { results with failed_tests = results.failed_tests + 1 }
          else
            { results with ok_tests = results.ok_tests + 1 })
        empty_suite_results
        tests
    in
    if results.failed_tests = 0 then
      let _ =
        print
          [
            ( C.Bold C.Default,
              spf
                "Passed: %d (%d cases), Failed: %d (%d cases), Skipped: %d cases\n"
                results.ok_tests
                results.ok_cases
                results.failed_tests
                results.failed_cases
                results.skipped_cases );
          ]
      in
      exit 0
    else
      let _ =
        print
          [
            (C.Bold C.Default, spf "Passed: %d (%d cases), " results.ok_tests results.ok_cases);
            ( C.BoldWithBG (C.White, C.Red),
              spf "Failed: %d (%d cases)" results.failed_tests results.failed_cases );
            (C.Bold C.Default, spf ", Skipped: %d cases\n" results.skipped_cases);
          ]
      in
      exit 1
end

let _ = RunEsprimaTests.main ()
