(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

(***********************************************************************)
(* flow ast command *)
(***********************************************************************)

type ast_file_type =
  | File_json
  | File_js

type ast_include_comments =
  | Comments_true
  | Comments_false
  | Comments_docblock

let spec =
  {
    CommandSpec.name = "ast";
    doc = "Print the AST";
    usage =
      Printf.sprintf
        "Usage: %s ast [OPTION]... [FILE]\n\ne.g. %s ast foo.js\nor   %s ast < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> flag "--tokens" no_arg ~doc:"Include a list of syntax tokens in the output"
        |> flag "--pretty" no_arg ~doc:"Pretty-print JSON output"
        |> flag
             "--check"
             no_arg
             ~doc:"Checks whether the file parses, returning any errors but not the AST"
        |> flag "--debug" no_arg ~doc:"" (* undocumented *)
        |> flag
             "--pattern"
             no_arg
             ~doc:"Prints the AST structurally without locations to be used in pattern matching"
        |> flag
             "--type"
             (enum [("js", File_js); ("json", File_json)])
             ~doc:"Type of input file (js or json)"
        |> flag "--strict" no_arg ~doc:"Parse in strict mode"
        |> flag
             "--include-comments"
             (required
                ~default:Comments_true
                (enum
                   [
                     ("true", Comments_true);
                     ("false", Comments_false);
                     ("docblock-only", Comments_docblock);
                   ]
                )
             )
             ~doc:
               "Include or drop comments in the output (true, false or docblock-only) (default: true)"
        |> flag
             "--include-locs"
             (required ~default:true bool)
             ~doc:"Include or drop location information in the output (default: true)"
        |> CommandUtils.offset_style_flag
        |> CommandUtils.from_flag
        |> CommandUtils.path_flag
        |> anon "file" (optional string)
      );
  }

type ast_result_type =
  | Ast_json of (Loc.t, Loc.t) Ast.Expression.t
  | Ast_js of (Loc.t, Loc.t) Ast.Program.t

let get_file path = function
  | Some filename -> File_input.FileName (CommandUtils.expand_path filename)
  | None -> File_input.FileContent (path, Sys_utils.read_stdin_to_string ())

module Token_translator = Token_translator.Translate (Json_of_estree)

let pp_underscore_loc fmt _ = Format.pp_print_string fmt "_"

let main
    include_tokens
    pretty
    check
    debug
    pattern
    file_type_opt
    use_strict
    include_comments
    include_locs
    offset_style
    path
    filename
    () =
  let use_relative_path = Base.Option.value_map filename ~default:false ~f:Filename.is_relative in
  let file = get_file path filename in
  let content = File_input.content_of_file_input_unsafe file in
  let file_type =
    match file_type_opt with
    | Some t -> t
    | None ->
      begin
        match filename with
        | Some fn ->
          if Files.is_json_file fn then
            File_json
          else
            File_js
        | None -> File_js
      end
  in

  let module Translate =
    Estree_translator.Translate
      (Json_of_estree)
      (struct
        let include_locs = include_locs
      end)
  in
  (*
   * Record token stream into a list when the --tokens flag is passed.
   * Note that tokens stream in in order, so the list is constructed in reverse
   * order.
   *)
  let tokens = ref [] in
  let offset_kind = CommandUtils.offset_kind_of_offset_style offset_style in
  let offset_table = lazy (Offset_utils.make ~kind:offset_kind content) in
  let token_sink =
    if not include_tokens then
      None
    else
      Some
        (fun token_data ->
          tokens := Token_translator.token (Lazy.force offset_table) token_data :: !tokens)
  in
  Hh_json.(
    let results =
      try
        (* Make the parser as permissive as possible.
           TODO: make these CLI flags *)
        let parse_options =
          Some
            Parser_env.
              {
                enums = true;
                esproposal_class_instance_fields = true;
                esproposal_class_static_fields = true;
                esproposal_decorators = true;
                esproposal_export_star_as = true;
                esproposal_optional_chaining = true;
                esproposal_nullish_coalescing = true;
                types = true;
                use_strict;
              }
            
        in

        let filename = File_input.path_of_file_input file in
        let filename =
          if use_relative_path then
            Base.Option.map filename ~f:(Files.relative_path (Sys.getcwd ()))
          else
            filename
        in
        let (ast, errors) =
          match file_type with
          | File_js ->
            let filekey = Base.Option.map filename ~f:(fun s -> File_key.SourceFile s) in
            let (ocaml_ast, errors) =
              Parser_flow.program_file ~fail:false ~parse_options ~token_sink content filekey
            in
            if debug then (
              Ast.Program.pp Loc.pp Loc.pp Format.err_formatter ocaml_ast;
              Printf.eprintf "\n%!"
            );
            if pattern then (
              Ast.Program.pp pp_underscore_loc pp_underscore_loc Format.err_formatter ocaml_ast;
              Printf.eprintf "\n%!"
            );
            (Ast_js ocaml_ast, errors)
          | File_json ->
            let filekey = Base.Option.map filename ~f:(fun s -> File_key.JsonFile s) in
            let (ocaml_ast, errors) =
              Parser_flow.json_file ~fail:false ~parse_options ~token_sink content filekey
            in
            if debug then (
              Ast.Expression.pp Loc.pp Loc.pp Format.err_formatter ocaml_ast;
              Printf.eprintf "\n%!"
            );
            if pattern then (
              Ast.Expression.pp pp_underscore_loc pp_underscore_loc Format.err_formatter ocaml_ast;
              Printf.eprintf "\n%!"
            );
            (Ast_json ocaml_ast, errors)
        in
        if check then
          JSON_Object
            [("errors", Translate.errors errors); ("tokens", JSON_Array (List.rev !tokens))]
        else
          let offset_table = Some (Lazy.force offset_table) in
          let translated_ast =
            match ast with
            | Ast_js ast ->
              let ast =
                match include_comments with
                | Comments_true -> ast
                | Comments_false -> Comment_utils.strip_all_comments ast
                | Comments_docblock -> Comment_utils.strip_all_comments ~preserve_docblock:true ast
              in
              Translate.program offset_table ast
            | Ast_json ast ->
              let ast =
                match include_comments with
                | Comments_true -> ast
                | Comments_false
                | Comments_docblock ->
                  Comment_utils.strip_inlined_comments_expression ast
              in
              Translate.expression offset_table ast
          in
          match translated_ast with
          | JSON_Object params ->
            let errors_prop = ("errors", Translate.errors errors) in
            let tokens_prop = ("tokens", JSON_Array (List.rev !tokens)) in
            JSON_Object (errors_prop :: tokens_prop :: params)
          | _ -> assert false
      with
      | Parse_error.Error l -> JSON_Object [("errors", Translate.errors l)]
    in
    print_json_endline ~pretty results
  )

let command = CommandSpec.command spec main
