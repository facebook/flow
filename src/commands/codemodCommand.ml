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
          |> common_annotate_flags
        );
    }

  let main codemod_flags preserve_literals max_type_size default_any () =
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_exports.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o = o

      let visit =
        let mapper = Annotate_exports.mapper ~preserve_literals ~max_type_size ~default_any in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_lti_command = struct
  let doc = "Annotates function and class definitions required for Flow's local type interence."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-functions-and-classes";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-functions-and-classes [OPTION]... [FILE]\n\n%s\n"
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
          |> flag
               "--skip-normal-params"
               truthy
               ~doc:
                 "Skips adding function parameter type annotations (except for 'this' annotations) even if necessary"
          |> flag
               "--skip-this-params"
               truthy
               ~doc:"Skips adding 'this' parameter type annotations to functions even if necessary"
          |> flag
               "--skip-class-properties"
               truthy
               ~doc:
                 "Skips adding type annotations to class properties without initializers even if necessary"
          |> flag
               "--include-lti"
               truthy
               ~doc:"Adding missing annotations that are only detected under LTI mode."
          |> flag
               "--ignore-suppressed"
               truthy
               ~doc:"Do not annotate locations with suppressed missing-local-annot errors"
        );
    }

  module LtiPerFileMissingLocalAnnotErrorsHeap =
    SharedMem.NoCache
      (File_key)
      (struct
        type t = Loc.t list

        let description = "TransformableErrorsHeap"
      end)

  let main
      codemod_flags
      preserve_literals
      max_type_size
      default_any
      skip_normal_params
      skip_this_params
      skip_class_properties
      include_lti
      ignore_suppressed
      () =
    let reader = State_reader.create () in
    let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc ~reader in
    let module SimpleRunner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_lti.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      (* Match all files the codemod is run over *)
      let check_options o =
        let open Options in
        { o with opt_any_propagation = false; opt_inference_mode = ConstrainWrites }

      let visit ~options =
        let mapper =
          Annotate_lti.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~loc_of_aloc
            ~preserve_literals
            ~max_type_size
            ~default_any
            ~skip_normal_params
            ~skip_this_params
            ~skip_class_properties
            ~provided_error_locs:[]
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper) ~options
    end) in
    let module LTIRunner = Codemod_runner.MakeTypedRunnerWithPrepass (struct
      module Acc = Annotate_lti.Acc

      type accumulator = Acc.t

      type prepass_state = unit

      type prepass_result = Loc.t list

      let reporter = string_reporter (module Acc)

      let prepass_init () = ()

      let mod_prepass_options o = { o with Options.opt_inference_mode = Options.LTI }

      let check_options o = o

      let include_dependents_in_prepass = false

      let prepass_run cx () _file_key file_options _reader _file_sig _typed_ast =
        let errors = Context.errors cx in
        let errors =
          if ignore_suppressed then
            Error_suppressions.filter_suppressed_error_set
              ~root:(Context.root cx)
              ~file_options:(Some file_options)
              ~loc_of_aloc
              (Context.error_suppressions cx)
              errors
          else
            errors
        in
        errors
        |> Flow_error.ErrorSet.elements
        |> List.filter_map (fun error ->
               match Flow_error.code_of_error error with
               | Some Error_codes.MissingLocalAnnot ->
                 error
                 |> Flow_error.msg_of_error
                 |> Error_message.loc_of_msg
                 |> Base.Option.map ~f:(fun error_aloc ->
                        Parsing_heaps.Reader.loc_of_aloc ~reader error_aloc
                    )
               | _ -> None
           )

      let store_precheck_result result =
        FilenameMap.iter
          (fun file -> Base.Result.iter ~f:(LtiPerFileMissingLocalAnnotErrorsHeap.add file))
          result

      let visit ~options program =
        let provided_error_locs =
          LtiPerFileMissingLocalAnnotErrorsHeap.get
            (Base.Option.value_exn (fst program |> Loc.source))
          |> Base.Option.value ~default:[]
        in
        let mapper =
          Annotate_lti.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~loc_of_aloc
            ~preserve_literals
            ~max_type_size
            ~default_any
            ~skip_normal_params
            ~skip_this_params
            ~skip_class_properties
            ~provided_error_locs
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper) ~options program
    end) in
    main
      ( if include_lti then
        (module LTIRunner)
      else
        (module SimpleRunner)
      )
      codemod_flags
      ()

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

module Annotate_empty_array_command = struct
  module LtiPerFileEmptyArrayErrorsHeap =
    SharedMem.NoCache
      (File_key)
      (struct
        type t = Flow_error.ErrorSet.t

        let description = "TransformableErrorsHeap"
      end)

  let doc = "Annotates empty array literals, whose type cannot be inferred."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-empty-array";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-empty-array [OPTION]... [FILE]\n\n%s\n"
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
               "--generalize-maybe"
               truthy
               ~doc:"Generalize annotations containing null or void to maybe types"
          |> flag
               "--generalize-react-mixed-element"
               truthy
               ~doc:"Generalize annotations containing react elements to React.MixedElement"
          |> flag
               "--include-lti"
               truthy
               ~doc:
                 "Add casts to underconstrained empty arrays that are only detected under LTI mode."
          |> flag
               "--ignore-suppressed"
               truthy
               ~doc:"Do not annotate locations with suppressed errors"
          |> common_annotate_flags
        );
    }

  let main
      codemod_flags
      preserve_literals
      generalize_maybe
      generalize_react_mixed_element
      include_lti
      ignore_suppressed
      max_type_size
      default_any
      () =
    let module SimpleRunner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_declarations.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o =
        let open Options in
        { o with opt_inference_mode = ConstrainWrites; opt_array_literal_providers = true }

      let visit ~options program =
        let mapper =
          Annotate_empty_array.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~preserve_literals
            ~generalize_maybe
            ~generalize_react_mixed_element
            ~max_type_size
            ~default_any
            ~provided_error_set:Flow_error.ErrorSet.empty
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper) ~options program
    end) in
    let module LTIRunner = Codemod_runner.MakeTypedRunnerWithPrepass (struct
      module Acc = Annotate_declarations.Acc

      type accumulator = Acc.t

      type prepass_state = unit

      type prepass_result = Flow_error.ErrorSet.t

      let reporter = string_reporter (module Acc)

      let prepass_init () = ()

      let mod_prepass_options o = { o with Options.opt_inference_mode = Options.LTI }

      let check_options o = o

      let include_dependents_in_prepass = false

      let prepass_run cx () _file_key _file_options _reader _file_sig _typed_ast = Context.errors cx

      let store_precheck_result result =
        FilenameMap.iter
          (fun file -> Base.Result.iter ~f:(LtiPerFileEmptyArrayErrorsHeap.add file))
          result

      let visit ~options program =
        let provided_error_set =
          LtiPerFileEmptyArrayErrorsHeap.get (Base.Option.value_exn (fst program |> Loc.source))
          |> Base.Option.value ~default:Flow_error.ErrorSet.empty
        in
        let mapper =
          Annotate_empty_array.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~preserve_literals
            ~generalize_maybe
            ~generalize_react_mixed_element
            ~max_type_size
            ~default_any
            ~provided_error_set
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper) ~options program
    end) in
    main
      ( if include_lti then
        (module LTIRunner)
      else
        (module SimpleRunner)
      )
      codemod_flags
      ()

  let command = CommandSpec.command spec main
end

module Annotate_cycles_command = struct
  let doc =
    "Annotates variables or functions that depend on each other or that depend on themselves"

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    {
      CommandSpec.name = "annotate-cycles";
      doc;
      usage =
        Printf.sprintf
          "Usage: %s codemod annotate-cycles [OPTION]... [FILE]\n\n%s\n"
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
               "--generalize-maybe"
               truthy
               ~doc:"Generalize annotations containing null or void to maybe types"
          |> flag
               "--generalize-react-mixed-element"
               truthy
               ~doc:"Generalize annotations containing react elements to React.MixedElement"
          |> flag
               "--merge-array-elements"
               truthy
               ~doc:"combine arrays appearing in toplevel unions to a single array"
          |> common_annotate_flags
        );
    }

  let main
      codemod_flags
      preserve_literals
      generalize_maybe
      generalize_react_mixed_element
      merge_arrays
      max_type_size
      default_any
      () =
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_cycles.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o =
        Options.{ o with opt_cycle_errors = true; opt_inference_mode = Options.ConstrainWrites }

      let visit =
        let mapper =
          new Annotate_cycles.annotate_cycles_mapper
            ~default_any
            ~generalize_maybe
            ~generalize_react_mixed_element
            ~max_type_size
            ~preserve_literals
            ~merge_arrays
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_react_hooks_command = struct
  let doc = "Adds explicit type arguments to builtin React hooks calls"

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    let name = "annotate-react-hooks" in
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
          |> flag
               "--generalize-maybe"
               truthy
               ~doc:"Generalize annotations containing null or void to maybe types"
          |> flag
               "--generalize-react-mixed-element"
               truthy
               ~doc:"Generalize annotations containing react elements to React.MixedElement"
          |> common_annotate_flags
        );
    }

  let main
      codemod_flags
      preserve_literals
      generalize_maybe
      generalize_react_mixed_element
      max_type_size
      default_any
      () =
    let open Codemod_utils in
    let module Runner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_react_hooks.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o = { o with Options.opt_save_implicit_instantiation_results = true }

      let visit =
        let mapper =
          Annotate_react_hooks.mapper
            ~preserve_literals
            ~generalize_maybe
            ~generalize_react_mixed_element
            ~max_type_size
            ~default_any
        in
        Codemod_utils.make_visitor (Mapper mapper)
    end) in
    main (module Runner) codemod_flags ()

  let command = CommandSpec.command spec main
end

module Annotate_implicit_instantiation = struct
  let doc = "Adds explicit type arguments to underconstrained implicit instantiations"

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
    let preserve_string_literals_level =
      Literals.[("always", Always); ("never", Never); ("auto", Auto)]
    in
    let name = "annotate-implicit-instantiations" in
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
          |> flag
               "--generalize-maybe"
               truthy
               ~doc:"Generalize annotations containing null or void to maybe types"
          |> flag "--include-widened" truthy ~doc:"Include type arguments that are later widened"
          |> flag
               "--annotate-special-function-return"
               truthy
               ~doc:"Annotate special-cased function return to constrain type arguments"
          |> flag
               "--include-lti"
               truthy
               ~doc:
                 "Add type arguments to underconstrained calls that are only detected under LTI mode."
          |> flag
               "--ignore-suppressed"
               truthy
               ~doc:
                 "Do not annotate locations with suppressed underconstrained-implicit-instantiation errors"
          |> common_annotate_flags
        );
    }

  module LtiPerFileUnderconstrainedImplicitInstantiationErrorsHeap =
    SharedMem.NoCache
      (File_key)
      (struct
        type t = Flow_error.ErrorSet.t

        let description = "TransformableErrorsHeap"
      end)

  let main
      codemod_flags
      preserve_literals
      generalize_maybe
      include_widened
      annotate_special_fun_return
      include_lti
      ignore_suppressed
      max_type_size
      default_any
      () =
    let open Codemod_utils in
    let module SimpleRunner = Codemod_runner.MakeSimpleTypedRunner (struct
      module Acc = Annotate_implicit_instantiation.Acc

      type accumulator = Acc.t

      let reporter = string_reporter (module Acc)

      let check_options o =
        {
          o with
          Options.opt_run_post_inference_implicit_instantiation = true;
          opt_enable_post_inference_targ_widened_check = include_widened;
          opt_save_implicit_instantiation_results = true;
        }

      let visit ~options =
        let mapper =
          Annotate_implicit_instantiation.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~preserve_literals
            ~generalize_maybe
            ~max_type_size
            ~annotate_special_fun_return
            ~default_any
            ~provided_error_set:Flow_error.ErrorSet.empty
        in
        Codemod_utils.make_visitor (Mapper mapper) ~options
    end) in
    let module LTIRunner = Codemod_runner.MakeTypedRunnerWithPrepass (struct
      module Acc = Annotate_implicit_instantiation.Acc

      type accumulator = Acc.t

      type prepass_state = unit

      type prepass_result = Flow_error.ErrorSet.t

      let reporter = string_reporter (module Acc)

      let prepass_init () = ()

      let mod_prepass_options o = { o with Options.opt_inference_mode = Options.LTI }

      let check_options o =
        {
          o with
          Options.opt_run_post_inference_implicit_instantiation = true;
          opt_enable_post_inference_targ_widened_check = include_widened;
          opt_save_implicit_instantiation_results = true;
        }

      let include_dependents_in_prepass = false

      let prepass_run cx () _file_key _file_options _reader _file_sig _typed_ast = Context.errors cx

      let store_precheck_result result =
        FilenameMap.iter
          (fun file ->
            Base.Result.iter ~f:(LtiPerFileUnderconstrainedImplicitInstantiationErrorsHeap.add file))
          result

      let visit ~options program =
        let provided_error_set =
          LtiPerFileUnderconstrainedImplicitInstantiationErrorsHeap.get
            (Base.Option.value_exn (fst program |> Loc.source))
          |> Base.Option.value ~default:Flow_error.ErrorSet.empty
        in
        let mapper =
          Annotate_implicit_instantiation.mapper
            ~ignore_suppressed
            ~file_options:(Options.file_options options)
            ~preserve_literals
            ~generalize_maybe
            ~max_type_size
            ~annotate_special_fun_return
            ~default_any
            ~provided_error_set
        in
        Codemod_utils.make_visitor (Codemod_utils.Mapper mapper) ~options program
    end) in
    main
      ( if include_lti then
        (module LTIRunner)
      else
        (module SimpleRunner)
      )
      codemod_flags
      ()

  let command = CommandSpec.command spec main
end

module Annotate_optional_properties_command = struct
  let doc = "Inserts optional properties on object definitions where properties are missing."

  let spec =
    let module Literals = Codemod_hardcoded_ty_fixes.PreserveLiterals in
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
        (Annotate_cycles_command.spec.CommandSpec.name, Annotate_cycles_command.command);
        (Annotate_empty_array_command.spec.CommandSpec.name, Annotate_empty_array_command.command);
        (Annotate_exports_command.spec.CommandSpec.name, Annotate_exports_command.command);
        (Annotate_lti_command.spec.CommandSpec.name, Annotate_lti_command.command);
        ( Annotate_optional_properties_command.spec.CommandSpec.name,
          Annotate_optional_properties_command.command
        );
        (Annotate_react_hooks_command.spec.CommandSpec.name, Annotate_react_hooks_command.command);
        ( Annotate_implicit_instantiation.spec.CommandSpec.name,
          Annotate_implicit_instantiation.command
        );
        (KeyMirror_command.spec.CommandSpec.name, KeyMirror_command.command);
      ]
  in
  CommandSpec.command spec main
