(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow ast command *)
(***********************************************************************)

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
    |> flag "--pretty" no_arg
        ~doc:"Pretty-print JSON output"
    |> anon "file" (optional string) ~doc:"[FILE]"
  )
}

module JsonTranslator : (
  Estree_translator.Translator with type t = Hh_json.json
) = struct
  type t = Hh_json.json

  open Hh_json

  let string x = JString x
  let bool x = JBool x
  let obj props = JAssoc (Array.to_list props)
  let array arr = JList (Array.to_list arr)
  let number x = JFloat x
  let null = JNull
  let regexp _loc _pattern _flags = JNull
end

let get_file = function
  | Some filename -> ServerProt.FileName (CommandUtils.expand_path filename)
  | None -> ServerProt.FileContent (None, Sys_utils.read_stdin_to_string ())

let main pretty filename () =
  let file = get_file filename in
  let content = ServerProt.file_input_get_content file in
  let module Translate = Estree_translator.Translate (JsonTranslator) in
  let results = try
    let (ocaml_ast, errors) = Parser_flow.program ~fail:false content in
    match Translate.program ocaml_ast with
    | Hh_json.JAssoc params ->
        Hh_json.JAssoc (("errors", Translate.errors errors)::params)
    | _ -> assert false
  with Parse_error.Error l ->
    Hh_json.JAssoc ["errors", Translate.errors l]
  in
  let json = if pretty
    then Hh_json.json_to_multiline results
    else Hh_json.json_to_string results
  in
  output_string stdout (json^"\n");
  flush stdout

let command = CommandSpec.command spec main
