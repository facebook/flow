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
      |> flag "--options" (optional string)
          ~doc:"Semicolon-delimited list of key=value pairs"
      |> anon "root" (optional string)
          ~doc:"Root directory (default: current working directory)"
    )
  }

  let main options root () =
    let root = match root with
    | None -> Sys.getcwd () |> Path.mk_path
    | Some root -> Path.mk_path root
    in
    let options = match options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str
    in
    FlowConfig.init root options

  let command = CommandSpec.command spec main
end
