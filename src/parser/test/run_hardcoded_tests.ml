(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Hh_json
open Utils

module Ast = Spider_monkey_ast
module C = Tty

module RunHardcodedTests : sig
  val main : unit -> unit
end = struct

  let should_color = Unix.isatty Unix.stdout && Sys.getenv "TERM" <> "dumb"

  let print to_print =
    if should_color
    then
      C.print to_print
    else
      let strings = List.map snd to_print in
      List.iter (Printf.printf "%s") strings

  let parse_args () =
    let dump_ast = ref false in
    let filter = ref None in
    let json_errors = ref false in
    let test_file = ref None in
    let parse_test_file x =
      if !test_file <> None then raise (Arg.Bad "Too many arguments");
      test_file := Some x
    in
    let spec = Arg.align [
      "--dumpAst", Arg.Set dump_ast,
        " Dumps the esprima & flow ASTs before each test";
      "--filter", Arg.String (fun x -> filter := Some x),
        " Only run tests that match the regex";
      "--jsonErrors", Arg.Set json_errors,
        " Output errors in json format";
    ] in
    let usage = spf "usage: %s [OPTIONS] TEST_FILE\n\nSupported options\n"
      (Filename.basename Sys.executable_name) in
    Arg.parse spec parse_test_file usage;
    let opt_test_file = match !test_file with
    | Some x -> x
    | _ ->
      prerr_endline "ERROR: Path to test file is required\n";
      Arg.usage spec usage;
      exit 1
    in
    (!dump_ast, !filter, !json_errors, opt_test_file)

  let rec parse_spec_value = Ast.Expression.(function
    | (_, Object { Object.properties }) ->
      JAssoc (List.map parse_spec properties)
    | (_, Array { Array.elements }) ->
      JAssoc (List.mapi (fun i elem ->
        let child = match elem with
        | Some (Expression e) -> parse_spec_value e
        | Some (Spread _)
        | None -> failwith "spread is not supported"
        in
        string_of_int i, child
      ) elements)
    | (_, Literal { Ast.Literal.value = Ast.Literal.String name; _ }) ->
        JString name
    | (_, Literal { Ast.Literal.value = Ast.Literal.Number num; _ }) ->
        JFloat num
    | (_, Literal { Ast.Literal.value = Ast.Literal.Boolean value; _ }) ->
        JBool value
    | (_, Literal { Ast.Literal.value = Ast.Literal.Null; _ }) ->
        JNull
    | (_, Binary { Binary.operator = Binary.Plus; left; right; }) ->
        let left_json = parse_spec_value left in
        let right_json = parse_spec_value right in
        begin match (left_json, right_json) with
        | JString l, JString r -> JString (l ^ r)
        | _ -> prerr_endline
          "ERROR: unable to parse binary expressions that aren't just strings";
          exit 1
        end
    | x ->
      prerr_endlinef
        "ERROR: unexpected spec (rec) format: %s"
        (json_to_string (Hardcoded_test_runner.Translate.expression x));
        exit 1
  )

  and parse_spec = Ast.Expression.Object.(function
    | Property (_, {
        Property.
        kind = Property.Init;
        key =
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value;
        _
      }) ->
        let child = parse_spec_value value in
        let parts = Str.split (Str.regexp_string ".") name in
        begin match parts with
        | [] -> (name, child)
        | name::rest ->
          let tree = List.fold_right (fun part acc ->
            JAssoc [part, acc]
          ) rest child in
          (name, tree)
        end
    | Property (loc, _) ->
      prerr_endline ("ERROR: unexpected spec format at " ^ (Loc.string loc));
      exit 1
    | _ -> prerr_endline "ERROR: unexpected spec format"; exit 1
  )

  let parse_test = Ast.Expression.Object.(function
    | Property (_, {
        Property.
        kind = Property.Init;
        key =
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = (_, Ast.Expression.Object { Ast.Expression.Object.properties });
        _
      }) -> (name, JAssoc (List.map parse_spec properties))
    | _ -> prerr_endline "ERROR: unexpected test format"; exit 1
  )

  let parse_section = Ast.Expression.Object.(function
    | Property (_, {
        Property.
        kind = Property.Init;
        key =
          Property.Literal (_, {
            Ast.Literal.value = Ast.Literal.String name;
            _;
          });
        value = (_, Ast.Expression.Object { Ast.Expression.Object.properties });
        _
      }) -> (name, List.map parse_test properties)
    | _ -> prerr_endline "ERROR: unexpected section format"; exit 1
  )

  let parse_sections = Ast.Expression.(function
    | (_, Object { Object.properties }) -> List.map parse_section properties
    | _ -> prerr_endline "ERROR: expected module.exports = {...}"; exit 1
  )

  let parse_ast content =
    let (ast, _) = Parser_flow.program ~fail:true content in
    let expr = Ast.Statement.(match ast with
      | (_, [(_, Expression({Expression.expression;}))], _) -> expression
      | _ -> prerr_endline "ERROR: Unable to parse test file\n"; exit 1
    ) in
    let obj = Ast.Expression.(match expr with
      | (_, Assignment { Assignment.right; _ }) -> right
      | _ -> prerr_endline "ERROR: expected module.exports = {...}"; exit 1
    ) in
    parse_sections obj

  let test_section dump json (num_successes, num_failures, failures) (name, tests) =
    print [C.Bold C.White, spf "===%s===\n" name];
    let num_successes, num_failures, section_failures = List.fold_left (fun (num_successes, num_failures, failures) (content, specs) ->
      if should_color then begin
        print [
          C.Normal C.Yellow, "RUNNING";
          C.Normal C.Default, spf " %S\r" content;
        ]
      end;
      let succeeded, output = Hardcoded_test_runner.run dump json content specs in
      if succeeded
      then (
        print [
          C.Normal C.Green, "PASSED";
          C.Normal C.Default, spf ": %S\n" content;
        ];
        (num_successes + 1, num_failures, failures)
      ) else (
        print [
          C.NormalWithBG (C.White, C.Red), "FAILED";
          C.Normal C.Default, spf ": %S\n" content;
        ];
        let failure = (content, output) in
        (num_successes, num_failures + 1, failure :: failures)
      )
    ) (num_successes, num_failures, []) tests in
    let failures = if List.length section_failures > 0
      then (name, section_failures)::failures
      else failures
    in
    (num_successes, num_failures, failures)

  let main () =
    let dump_ast, filter, json_errors, test_file = parse_args () in

    let test_file_content = Sys_utils.cat test_file in
    let sections = parse_ast test_file_content in

    let sections = match filter with
    | Some x ->
      let regex = Str.regexp x in
      List.filter (fun (_, tests) ->
        List.exists (fun (code, _) ->
          Str.string_match regex code 0
        ) tests
      ) sections
    | None -> sections
    in

    if dump_ast then begin
      let num_tests = List.length sections in
      if num_tests > 20 then (
        prerr_endlinef
          "Oh summer child, you really don't want to dump the AST for %d tests. Try using --filter to run fewer tests"
          num_tests;
        exit 1
      )
    end;

    let num_successes, num_failures, failures =
      List.fold_left (test_section dump_ast json_errors) (0, 0, []) sections in

    print_endlinef "%d/%d tests passed"
      num_successes
      (num_successes + num_failures);

    if num_failures > 0 then begin
      print [
        C.NormalWithBG (C.White, C.Red),
        spf "*** %d TESTS FAILED! ***" num_failures;
        C.Normal C.Default, "\n";
      ];

      List.iter (fun (section, tests) ->
        print [C.Bold C.Default, spf "===%s Failures===\n" section];
        List.iter (fun (test, result) ->
          print [
            C.NormalWithBG (C.White, C.Red), spf "Test failure: %S" test;
            C.Normal C.Default, spf "\n%s\n" result;
          ]
        ) tests
      ) (List.rev failures);
      exit 1
    end

end

let _ = RunHardcodedTests.main ()
