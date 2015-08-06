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
      |> flag "--options" (optional string)
          ~doc:"Semicolon-delimited list of key=value pairs"
      |> anon "root" (optional string)
          ~doc:"Root directory (default: current working directory)"
    )
  }

  let main from options root () =
    FlowEventLogger.set_from from;
    let root = match root with
    | None -> Sys.getcwd () |> Path.make
    | Some root -> Path.make root
    in
    let options = match options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str
    in
    FlowConfig.init root options

  let command = CommandSpec.command spec main
end
