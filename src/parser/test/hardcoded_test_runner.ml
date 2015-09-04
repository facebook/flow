(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Hh_json

let rec check_ast_walker ast path =
  match ast with
  | JAssoc properties ->
    List.fold_left (fun errors (name, value) ->
      let child_errors = check_ast_walker value (name::path) in
      (*
      if (name === "type" && env.should_run_ast_types()) {
        if (ast_types.namedTypes.hasOwnProperty(value)) {
          if (!ast_types.namedTypes[value].check(ast, true)) {
            env.ast_types_error();
          }
        } else {
          env.ast_types_error();
        }
      }
      *)
      child_errors @ errors
    ) [] properties
  | _ -> []

let check_ast ast =
  let errors = check_ast_walker ast ["root"] in
  if List.length errors > 0 then
    let output = errors
      |> List.mapi (fun i err ->
        spf "(#%d) %s" i err
      )
      |> String.concat "\n"
    in
    false, (spf "****AST Types Errors****\n%s" output)
  else
    true, ""

type diff = {
  path: string list;
  type_: string;
  expected: string option;
  actual: string option;
}

let string_of_diff diff =
  let expected_str = match (diff.expected, diff.actual) with
  | Some expected, Some actual  ->
    spf ". Expected %s, got %s" expected actual
  | _ -> ""
  in
  spf "%s: %s%s"
    (diff.path |> List.rev |> String.concat ".")
    diff.type_
    expected_str

let jstr_of_diff diff =
  let json = JAssoc [
    "path", JString (diff.path |> List.rev |> String.concat ".");
    "type", JString diff.type_;
    "expected", (match diff.expected with Some x -> JString x | _ -> JNull);
    "actual", (match diff.actual with Some x -> JString x | _ -> JNull);
  ] in
  json_to_multiline json

let mk_diff path msg expected actual =
  let to_json = function Some x -> Some (json_to_string x) | None -> None in
  { path; type_ = msg; expected = to_json expected; actual = to_json actual; }

let rec check_spec_walker ast spec path =
  match spec, ast with
  | JAssoc _, JList ast_elems ->
    let props = List.mapi (fun i elem ->
      string_of_int i, elem
    ) ast_elems in
    let ast = JAssoc (("length", JInt (List.length ast_elems))::props) in
    check_spec_walker ast spec path

  | JAssoc spec_props, JAssoc ast_props ->
    let ast_map = List.fold_left (fun acc (k, v) ->
      SMap.add k v acc
    ) SMap.empty ast_props in

    List.fold_left (fun diffs (prop, spec_val) ->
      if SMap.mem prop ast_map then
        let ast_val = SMap.find_unsafe prop ast_map in
        let child_path = prop :: path in
        let child_diffs = check_spec_walker ast_val spec_val child_path in
        List.rev_append child_diffs diffs
      else
        (mk_diff path (spf "Missing property %S" prop) None None)::diffs
    ) [] spec_props

  | JNull, JNull -> []
  | JBool v1, JBool v2 when v1 = v2 -> []
  | JString v1, JString v2 when v1 = v2 -> []
  | JInt v1, JInt v2 when v1 = v2 -> []
  | JFloat v1, JFloat v2 when v1 = v2 -> []
  | JFloat v1, JInt v2 when v1 = (float_of_int v2) -> []
  | JList _, _ -> [mk_diff path "Not an array" (Some spec) (Some ast)]
  | _, _ -> [mk_diff path "Wrong value" (Some spec) (Some ast)]

let check_spec (json_errors:bool) (ast:json) (spec:json) =
  let _printer = if json_errors then jstr_of_diff else string_of_diff in
  let diffs = check_spec_walker ast spec ["root"] in
  if List.length diffs > 0 then
    let diffs_str = diffs
      |> List.mapi (fun i diff ->
        spf "(#%d) %s" i (_printer diff)
      )
      |> String.concat "\n"
    in
    prerr_endline (json_to_multiline ast);
    prerr_endline ((json_to_multiline spec)^"\n\n");
    false, spf "****Unexpected Differences****\n%s" diffs_str
  else
    true, ""

let has_errors_prop x = match x with
  | JAssoc props -> List.exists (fun (name, _) -> name = "errors") props
  | _ -> false

let check_errors (errors: (Loc.t * Parse_error.t) list) (spec:json) =
  if List.length errors > 0 && not (has_errors_prop spec) then
  begin
    let errors_str = errors
      |> List.mapi (fun i (loc, err) ->
        spf "(#%d) (line %d, col %d) - (line %d, col %d): %s"
          i
          loc.Loc.start.Loc.line
          loc.Loc.start.Loc.column
          loc.Loc._end.Loc.line
          loc.Loc._end.Loc.column
          (Parse_error.PP.error err)
      )
      |> String.concat "\n"
    in
    false, spf "****Unexpected Errors****\n%s" errors_str
  end
  else
    true, ""

module JsonTranslator : (
  Estree_translator.Translator with type t = Hh_json.json
) = struct
  type t = Hh_json.json

  let string x = JString x
  let bool x = JBool x
  let obj props = JAssoc (Array.to_list props)
  let array arr = JList (Array.to_list arr)
  let number x = JFloat x
  let null = JNull
  let regexp _loc _pattern _flags = JNull
end

module Translate = Estree_translator.Translate (JsonTranslator)

let run (dump_ast:bool) (json_errors:bool) (parse_options:Parser_env.parse_options option) (content:string) (spec:json) =
  try
    let success = true in
    let output = [] in

    let (ast, errors) = Parser_flow.program ~fail:false ~parse_options content in
    let json = match Translate.program ast with
    | JAssoc params ->
        JAssoc (("errors", Translate.errors errors)::params)
    | _ -> assert false
    in

    let output = if dump_ast
      then (
        let str = spf "AST: %s\n" (json_to_multiline json) in
        print_string str;
        str :: output
      ) else output
    in

    let spec_success, spec_output = check_spec json_errors json spec in
    let ast_success, ast_output = check_ast json in
    let errors_success, errors_output = check_errors errors spec in

    let success = success && spec_success && ast_success && errors_success in
    let output = errors_output :: ast_output :: spec_output :: output in
    success, (output |> List.rev |> String.concat "")

  with Parse_error.Error errs ->
    false, spf "Flow exploded: %s" (json_to_multiline (Translate.errors errs))
