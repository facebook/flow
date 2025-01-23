(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let spec =
  {
    CommandSpec.name = "env-builder-debug";
    doc = "Print the env-builder result as a dependency graph for debugging purposes";
    usage =
      Printf.sprintf
        "Usage: %s env-builder-debug [OPTION]... [FILE]\n\ne.g. %s env-builder-debug foo.js\nor   %s env-builder-debug < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty |> CommandUtils.from_flag |> CommandUtils.path_flag |> anon "file" (optional string)
      );
  }

let get_file path = function
  | Some filename -> File_input.FileName (CommandUtils.expand_path filename)
  | None -> File_input.FileContent (path, Sys_utils.read_all stdin)

module Token_translator = Token_translator.Translate (Json_of_estree)

let pp_underscore_loc fmt _ = Format.pp_print_string fmt "_"

module Context = struct
  type t = unit

  let enable_enums _cx = true

  let enable_pattern_matching _cx = true

  let file _cx = File_key.SourceFile "test.js"

  let jsx _cx = Options.Jsx_react

  let react_runtime _cx = Options.ReactRuntimeClassic

  let enable_const_params _cx = false

  let add_literal_subtypes _ _ = ()

  let add_exhaustive_check _ _ _ = ()

  let exhaustive_check _ _ = ([], false)
end

let print_graph graph =
  let () = print_endline "digraph {\n" in
  let () = List.iter (fun (from, to') -> Printf.printf "  \"%s\" -> \"%s\"\n" from to') graph in
  print_endline "}"

module Name_resolver = Name_resolver.Make_Test_With_Cx (Context)
module Name_def_ordering = Name_def_ordering.Make_Test_With_Cx (Context)

let autocomplete_hooks =
  {
    Env_api.With_ALoc.id_hook = (fun _ _ -> false);
    literal_hook = (fun _ -> false);
    obj_prop_decl_hook = (fun _ _ -> false);
  }

let main path filename () =
  let use_relative_path = Base.Option.value_map filename ~default:false ~f:Filename.is_relative in
  let file = get_file path filename in
  let content = File_input.content_of_file_input_unsafe file in
  let parse_options = Some Parser_env.permissive_parse_options in

  let filename = File_input.path_of_file_input file in
  let filename =
    if use_relative_path then
      Base.Option.map filename ~f:(Files.relative_path (Sys.getcwd ()))
    else
      filename
  in
  let (ast, errors) =
    let filekey = Base.Option.map filename ~f:(fun s -> File_key.SourceFile s) in
    Parser_flow.program_file ~fail:false ~parse_options ~token_sink:None content filekey
  in
  let ast = Ast_loc_utils.loc_to_aloc_mapper#program ast in
  if List.is_empty errors then
    (* Compute read -> write edges *)
    let (_, env) = Name_resolver.program_with_scope () ast in
    (* Compute write -> read edges *)
    let (inits, _) =
      Name_def.find_defs ~autocomplete_hooks ~react_jsx:true env Name_def.Module ast
    in
    (* Connect read -> write edges and write -> read edges to form a graph *)
    let graph = Name_def_ordering.build_graph ~autocomplete_hooks () env inits in
    let order_graph =
      let open Env_api in
      let print_kind_and_loc (k, l) =
        if k = Env_api.OrdinaryNameLoc then
          ALoc.debug_to_string l
        else
          Printf.sprintf "%s (%s)" (ALoc.debug_to_string l) (Env_api.show_def_loc_type k)
      in
      let print_write_kind_and_loc entry = "write: " ^ print_kind_and_loc entry in
      let print_read_loc loc = "read: " ^ ALoc.debug_to_string loc in
      graph
      |> EnvMap.elements
      |> Base.List.bind ~f:(fun (w_from, deps) ->
             deps
             |> EnvMap.elements
             |> Base.List.bind ~f:(fun (w_to, whys) ->
                    Base.List.bind (Nel.to_list whys) ~f:(fun why ->
                        [
                          (print_write_kind_and_loc w_from, print_read_loc why);
                          (print_read_loc why, print_write_kind_and_loc w_to);
                        ]
                    )
                )
         )
    in
    print_graph order_graph
  else
    prerr_endline "Program does not parse."

let command = CommandSpec.command spec main
