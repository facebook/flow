(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        }) =
    codemod_flags
  in
  initialize_environment ();
  let filenames = CommandUtils.get_filenames_from_input input_file filenames in
  let flowconfig_name = base_flags.CommandUtils.Base_flags.flowconfig_name in
  let root =
    match root with
    | None -> CommandUtils.guess_root flowconfig_name (Some (List.hd filenames))
    | Some provided_root ->
      let dir = Path.make provided_root in
      if Path.file_exists (Path.concat dir flowconfig_name) then
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
  (* Normalizes filepaths (symlinks and shortcuts) *)
  if filenames = [] then (
    Printf.eprintf "Error: filenames or --input-file are required\n%!";
    exit 64 (* EX_USAGE *)
  );
  let file_options = Options.file_options options in

  let roots = CommandUtils.expand_file_list ~options:file_options filenames in
  let roots =
    SSet.fold
      (fun f acc ->
        Utils_js.FilenameSet.add (Files.filename_from_string ~options:file_options f) acc)
      roots
      Utils_js.FilenameSet.empty
  in
  let module M = Codemod_utils.MakeMain (Runnable) in
  M.main ~options ~write ~repeat ~log_level ~shared_mem_config roots

(***********************)
(* Codemod subcommands *)
(***********************)

module Annotate_exports_command = struct
  let doc =
    "Annotates parts of input that are visible from the exports as required by Flow types-first mode."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
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
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails");
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let open Codemod_utils in
    let open Insert_type_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Acc (Annotate_exports.SignatureVerificationErrorStats)

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      let check_options o = o

      let visit =
        let mapper = Annotate_exports.mapper ~preserve_literals ~max_type_size ~default_any in
        Codemod_utils.make_visitor (Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_escaped_generics = struct
  let doc = "Annotates parts of input that receive out-of-scope generics as inferred types."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-escaped-generics";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-escaped-generics [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.codemod_flags
          |> flag
               "--preserve-literals"
               (required ~default:Literals.Auto (enum preserve_string_literals_level))
               ~doc:""
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails");
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      include Annotate_escaped_generics

      let check_options o = o

      let visit = visit ~default_any ~preserve_literals ~max_type_size
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_lti_command = struct
  let doc = "Annotates function definitions required for Flow's local type interence."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-lti-experimental";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-lti-experimental [OPTION]... [FILE]\n\n%s\n"
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
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails"
          |> flag
               "--add-this-params"
               no_arg
               ~doc:"Adds a 'this' parameter and type annotations to functions where necessary");
    }

  let main codemod_flags preserve_literals max_type_size default_any add_this_params () =
    let open Codemod_utils in
    let open Insert_type_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Acc (Annotate_lti.ErrorStats)

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      (* Match all files the codemod is run over *)
      let check_options o = Options.{ o with opt_enforce_local_inference_annotations = true }

      let visit =
        let mapper =
          Annotate_lti.mapper ~preserve_literals ~max_type_size ~default_any ~add_this_params
        in
        Codemod_utils.make_visitor (Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_declarations_command = struct
  let doc =
    "Annotates variable declarations with multiple conflicting assignments, as required for Flow's local type interence."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-declarations-experimental";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-declarations-experimental [OPTION]... [FILE]\n\n%s\n"
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
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails");
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let open Codemod_utils in
    let open Insert_type_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Acc (Annotate_declarations.ErrorStats)

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      (* Match all files the codemod is run over *)
      let check_options o =
        let open Options in
        match o.opt_env_mode with
        | ClassicEnv opts when List.mem ConstrainWrites opts -> o
        | ClassicEnv opts -> { o with opt_env_mode = ClassicEnv (ConstrainWrites :: opts) }
        | SSAEnv -> { o with opt_env_mode = ClassicEnv [ConstrainWrites] }

      let visit =
        let mapper = Annotate_declarations.mapper ~preserve_literals ~max_type_size ~default_any in
        Codemod_utils.make_visitor (Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Rename_redefinitions_command = struct
  let doc = "Convert redefined variables into separate consts"

  let spec =
    {
      CommandSpec.name = "rename-redefinitions-experimental";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod rename-redefinitions-experimental [OPTION]... [FILE]\n\n%s\n"
          Utils_js.exe_name
          doc;
      args = CommandSpec.ArgSpec.(empty |> CommandUtils.codemod_flags);
    }

  let main codemod_flags () =
    let open Codemod_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Rename_redefinitions.Acc

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      (* Match all files the codemod is run over *)
      let check_options o =
        let open Options in
        match o.opt_env_mode with
        | ClassicEnv opts when List.mem ConstrainWrites opts -> o
        | ClassicEnv opts -> { o with opt_env_mode = ClassicEnv (ConstrainWrites :: opts) }
        | SSAEnv -> { o with opt_env_mode = ClassicEnv [ConstrainWrites] }

      let visit =
        let mapper = Rename_redefinitions.mapper in
        Codemod_utils.make_visitor (Mapper mapper)
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
    let open Codemod_utils in
    let open Insert_type_utils in
    let module Runner = Codemod_runner.MakeUntypedRunner (struct
      module Acc = UntypedAcc (Objmapi_to_keymirror.KeyMirrorStats)

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      let visit = Codemod_utils.make_visitor (Mapper Objmapi_to_keymirror.mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_empty_object_command = struct
  let doc = "Annotates empty objects."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-empty-object-experimental";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-empty-object-experimental [OPTION]... [FILE]\n\n%s\n"
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
          |> flag
               "--max-type-size"
               (required ~default:100 int)
               ~doc:"The maximum number of nodes allowed in a single type annotation (default: 100)"
          |> flag
               "--default-any"
               no_arg
               ~doc:"Adds 'any' to all locations where normalization or validation fails");
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let open Codemod_utils in
    let open Insert_type_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Acc (Annotate_empty_object.ErrorStats)

      type accumulator = Acc.t

      let reporter =
        {
          Codemod_report.report = Codemod_report.StringReporter Acc.report;
          combine = Acc.combine;
          empty = Acc.empty;
        }

      let check_options o = Options.{ o with opt_experimental_infer_indexers = true }

      let visit =
        let mapper = Annotate_empty_object.mapper ~preserve_literals ~max_type_size ~default_any in
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
        (Annotate_escaped_generics.spec.CommandSpec.name, Annotate_escaped_generics.command);
        (Annotate_lti_command.spec.CommandSpec.name, Annotate_lti_command.command);
        (Annotate_declarations_command.spec.CommandSpec.name, Annotate_declarations_command.command);
        (Rename_redefinitions_command.spec.CommandSpec.name, Rename_redefinitions_command.command);
        (KeyMirror_command.spec.CommandSpec.name, KeyMirror_command.command);
        (Annotate_empty_object_command.spec.CommandSpec.name, Annotate_empty_object_command.command);
      ]
  in
  CommandSpec.command spec main
