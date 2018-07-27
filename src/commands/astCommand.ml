(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow ast command *)
(***********************************************************************)

type ast_file_type =
  | File_json
  | File_js

let spec = {
  CommandSpec.
  name = "ast";
  doc = "Print the AST";
  usage = Printf.sprintf
    "Usage: %s ast [OPTION]... [FILE]\n\n\
      e.g. %s ast foo.js\n\
      or   %s ast < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> flag "--tokens" no_arg
        ~doc:"Include a list of syntax tokens in the output"
    |> flag "--pretty" no_arg
        ~doc:"Pretty-print JSON output"
    |> flag "--check" no_arg
        ~doc:"Checks whether the file parses, returning any errors but not the AST"
    |> flag "--debug" no_arg
        ~doc:"" (* undocumented *)
    |> flag "--type" (enum ["js", File_js; "json", File_json])
        ~doc:"Type of input file (js or json)"
    |> flag "--strict" no_arg
        ~doc:"Parse in strict mode"
    |> CommandUtils.from_flag
    |> CommandUtils.path_flag
    |> anon "file" (optional string)
  )
}

type ast_result_type =
  | Ast_json of (Loc.t, Loc.t) Ast.Expression.t
  | Ast_js of (Loc.t, Loc.t) Ast.program

let get_file path = function
  | Some filename -> File_input.FileName (CommandUtils.expand_path filename)
  | None ->
    File_input.FileContent (path, Sys_utils.read_stdin_to_string ())

module Translate = Estree_translator.Translate (Json_of_estree) (struct
  (* TODO: make these configurable via CLI flags *)
  let include_comments = true
  let include_locs = true
end)

module Token_translator = Token_translator.Translate (Json_of_estree)

let main include_tokens pretty check debug file_type_opt use_strict from path filename () =
  FlowEventLogger.set_from from;
  let use_relative_path = Option.value_map filename ~default:false ~f:Filename.is_relative in
  let file = get_file path filename in
  let content = File_input.content_of_file_input_unsafe file in

  let file_type =
    match file_type_opt with
    | Some t -> t
    | None ->
      begin match filename with
      | Some fn -> if Files.is_json_file fn then File_json else File_js
      | None -> File_js
      end
  in

  (**
   * Record token stream into a list when the --tokens flag is passed.
   * Note that tokens stream in in order, so the list is constructed in reverse
   * order.
   *)
  let tokens = ref [] in
  let token_sink =
    if not include_tokens then None else (Some(fun token_data ->
    tokens := (Token_translator.token token_data)::!tokens
  )) in

  let open Hh_json in
  let results =
    try
      (* Make the parser as permissive as possible.
         TODO: make these CLI flags *)
      let parse_options = Some Parser_env.({
        esproposal_class_instance_fields = true;
        esproposal_class_static_fields = true;
        esproposal_decorators = true;
        esproposal_export_star_as = true;
        esproposal_optional_chaining = true;
        esproposal_nullish_coalescing = true;
        types = true;
        use_strict;
      }) in

      let filename = File_input.path_of_file_input file in
      let filename = if use_relative_path
        then Option.map filename ~f:(Files.relative_path (Sys.getcwd ()))
        else filename
      in
      let (ast, errors) =
        match file_type with
        | File_js ->
          let filekey = Option.map filename ~f:(fun s -> File_key.SourceFile s) in
          let (ocaml_ast, errors) =
            Parser_flow.program_file ~fail:false ~parse_options ~token_sink content filekey
          in
          if debug then begin
            Ast.pp_program Loc.pp Loc.pp Format.err_formatter ocaml_ast;
            Printf.eprintf "\n%!"
          end;
          Ast_js ocaml_ast, errors
        | File_json ->
          let filekey = Option.map filename ~f:(fun s -> File_key.JsonFile s) in
          let (ocaml_ast, errors) =
            Parser_flow.json_file ~fail:false ~parse_options ~token_sink content filekey
          in
          if debug then begin
            Ast.Expression.pp Loc.pp Loc.pp Format.err_formatter ocaml_ast;
            Printf.eprintf "\n%!"
          end;
          Ast_json ocaml_ast, errors
      in
      if check then
        JSON_Object [
          "errors", Translate.errors errors;
          "tokens", JSON_Array (List.rev !tokens);
        ]
      else
        let translated_ast = match ast with
        | Ast_js ast -> Translate.program ast
        | Ast_json ast -> Translate.expression ast
        in
        match translated_ast with
        | JSON_Object params ->
            let errors_prop = ("errors", Translate.errors errors) in
            let tokens_prop = ("tokens", JSON_Array (List.rev !tokens)) in
            JSON_Object (errors_prop::tokens_prop::params)
        | _ -> assert false
    with Parse_error.Error l ->
      JSON_Object ["errors", Translate.errors l]
  in
  print_json_endline ~pretty results

let command = CommandSpec.command spec main
