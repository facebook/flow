module Init = struct
  let spec = {
    CommandSpec.
    name = "init";
    doc = "Initializes a directory to be used as a flow root directory";
    usage = Printf.sprintf
      "Usage: %s init [ROOT]\n\
        Initializes a directory to be used as a flow root directory\n\n\
        e.g. %s init /path/to/root\n\
        or %s init\n\
        or %s init --options \"optionA=123;optionB=456\"\n\n\
        If the root is not specified it is assumed to be the current working directory\n\n\
        This command will create and initialize /path/to/root/.flowconfig\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args = CommandSpec.ArgSpec.(
      empty
      |> CommandUtils.from_flag
      |> CommandUtils.flowconfig_flags
      |> flag "--options" (optional string)
          ~doc:"Semicolon-delimited list of key=value pairs"
      |> anon "root" (optional string)
          ~doc:"Root directory (default: current working directory)"
    )
  }

  let main from flowconfig_flags options root () =
    FlowEventLogger.set_from from;
    let root = match root with
    | None -> Sys.getcwd () |> Path.make
    | Some root -> Path.make root
    in
    FlowEventLogger.set_root (Some (Path.to_string root));
    let options = match options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str
    in
    let ignores = flowconfig_flags.CommandUtils.ignores in
    let includes = flowconfig_flags.CommandUtils.includes in
    let libs = flowconfig_flags.CommandUtils.libs in

    let file = Server_files_js.config_file root in
    if Sys.file_exists file
    then begin
      let msg = Utils_js.spf "Error: \"%s\" already exists!\n%!" file in
      FlowExitStatus.(exit ~msg Invalid_flowconfig)
    end;

    let config = FlowConfig.init ~ignores ~includes ~libs ~options in

    let out = Sys_utils.open_out_no_fail file in
    FlowConfig.write config out;
    Sys_utils.close_out_no_fail file out

  let command = CommandSpec.command spec main
end
