(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module FilenameMap = Utils_js.FilenameMap

let environment_initialized = ref None

let initialize_environment () =
  if !environment_initialized = None then (
    (* Kickstart daemon processes *)
    Daemon.check_entry_point ();

    (* Disable monitor updates as this is a single-use tool *)
    MonitorRPC.disable ();

    (* Improve backtraces *)
    Exception.record_backtrace true;

    (* Mark the environment as initialized *)
    environment_initialized := Some ()
  ) else
    ()

let main (module Runnable : Codemod_runner.RUNNABLE) codemod_flags () =
  let (CommandUtils.Codemod_params
        {
          options_flags = option_values;
          saved_state_options_flags;
          shm_flags;
          ignore_version;
          write;
          repeat;
          log_level;
          root;
          input_file;
          base_flag = base_flags;
          anon = filenames;
        }
        ) =
    codemod_flags
  in
  initialize_environment ();
  (* Normalizes filepaths (symlinks and shortcuts) *)
  let filenames = CommandUtils.get_filenames_from_input input_file filenames in
  if filenames = [] then (
    Printf.eprintf "Error: filenames or --input-file are required\n%!";
    exit 64 (* EX_USAGE *)
  );
  let flowconfig_name = base_flags.CommandUtils.Base_flags.flowconfig_name in
  let root =
    match root with
    | None -> CommandUtils.guess_root flowconfig_name (Some (List.hd filenames))
    | Some provided_root ->
      let dir = File_path.make provided_root in
      if File_path.file_exists (File_path.concat dir flowconfig_name) then
        dir
      else
        let msg = Utils_js.spf "Invalid root directory %s" provided_root in
        Exit.(exit ~msg Could_not_find_flowconfig)
  in
  let (flowconfig, flowconfig_hash) =
    CommandUtils.read_config_and_hash_or_exit
      ~enforce_warnings:(not ignore_version)
      (Server_files_js.config_file flowconfig_name root)
  in
  let shared_mem_config = CommandUtils.shm_config shm_flags flowconfig in
  let options =
    CommandUtils.make_options
      ~flowconfig_name
      ~flowconfig_hash
      ~flowconfig
      ~lazy_mode:(Some FlowConfig.Lazy)
      ~root
      ~options_flags:option_values
      ~saved_state_options_flags
  in
  let file_options = Options.file_options options in
  let roots = CommandUtils.expand_file_list ~options:file_options filenames in
  let roots =
    SSet.fold
      (fun f acc ->
        Utils_js.FilenameSet.add
          (Files.filename_from_string
             ~options:file_options
             ~consider_libdefs:true
             ~libs:SSet.empty
             f
          )
          acc)
      roots
      Utils_js.FilenameSet.empty
  in
  let module M = Codemod_utils.MakeMain (Runnable) in
  M.main ~options ~write ~repeat ~log_level ~shared_mem_config roots

let string_reporter (type a) (module Acc : Codemod_report.S with type t = a) : a Codemod_report.t =
  {
    Codemod_report.report = Codemod_report.StringReporter Acc.report;
    combine = Acc.combine;
    empty = Acc.empty;
  }

let common_annotate_flags prev =
  let open CommandSpec.ArgSpec in
  prev
  |> flag
       "--max-type-size"
       (required ~default:100 int)
       ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
  |> flag
       "--default-any"
       truthy
       ~doc:"Adds 'any' to all locations where normalization or validation fails"

(***********************)
(* Codemod subcommands *)
(***********************)

module Annotate_exports_command = struct
  let doc =
    "Annotates parts of input that are visible from the exports as required by Flow types-first mode."

  let spec =
    let module Literals = Insert_type_utils.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-exports";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-exports [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.codemod_flags
          |> flag
               "--preserve-literals"
               (required ~default:Literals.Never (enum preserve_string_literals_level))
               ~doc:""
          |> common_annotate_flags
        );
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_exports.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o = o

      let expand_roots ~env:_ files = files

      let visit =
        let mapper = Annotate_exports.mapper ~preserve_literals ~max_type_size ~default_any in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module KeyMirror_command = struct
  let doc = "Replaces instances of the type $ObjMapi<T, <K>(K) => K> with $KeyMirror<T>"

  let spec =
    {
      CommandSpec.name = "key-mirror";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod key-mirror [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args = CommandSpec.ArgSpec.(empty |> CommandUtils.codemod_flags);
    }

  let main codemod_flags () =
    let module Runner = Codemod_runner.MakeUntypedRunner (struct
      module Acc = Objmapi_to_keymirror.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let visit = Codemod_utils.make_visitor (Codemod_utils.Mapper Objmapi_to_keymirror.mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module RemoveReactImportCommand = struct
  let doc = "Remove unnecessary imports of React under react.runtime=automatic."

  let spec =
    {
      CommandSpec.name = "remove-unnecessary-react-import";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod remove-unnecessary-react-import [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args = CommandSpec.ArgSpec.(empty |> CommandUtils.codemod_flags);
    }

  let main codemod_flags () =
    let module Runner = Codemod_runner.MakeUntypedRunner (struct
      module Acc = Remove_react_import.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let visit = Codemod_utils.make_visitor (Codemod_utils.Mapper Remove_react_import.mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_optional_properties_command = struct
  let doc = "Inserts optional properties on object definitions where properties are missing."

  let spec =
    let module Literals = Insert_type_utils.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    let name = "annotate-optional-properties" in
    {
      CommandSpec.name;
      doc;
      usage =
        Printf.sprintf "Usage: %s codemod %s [OPTION]... [FILE]\n\n%s\n" name Utils_js.exe_name doc;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.codemod_flags
          |> flag
               "--preserve-literals"
               (required ~default:Literals.Never (enum preserve_string_literals_level))
               ~doc:""
          |> common_annotate_flags
        );
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let open Codemod_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_optional_properties.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let expand_roots ~env:_ files = files

      let check_options o = o

      let visit =
        let mapper =
          Annotate_optional_properties.mapper ~preserve_literals ~max_type_size ~default_any
        in
        Codemod_utils.make_visitor (Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    CommandUtils.subcommand_spec
      ~name:"codemod"
      ~doc:"Runs large-scale codebase refactors"
      [
        (Annotate_exports_command.spec.CommandSpec.name, Annotate_exports_command.command);
        ( Annotate_optional_properties_command.spec.CommandSpec.name,
          Annotate_optional_properties_command.command
        );
        (KeyMirror_command.spec.CommandSpec.name, KeyMirror_command.command);
        (RemoveReactImportCommand.spec.CommandSpec.name, RemoveReactImportCommand.command);
      ]
  in
  CommandSpec.command spec main
