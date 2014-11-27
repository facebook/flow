module Init = struct
  type env = {
    root : Path.path;
    configOptions: string list;
  }

  let parse_args () =
    let config_options = ref None in
    let options = CommandUtils.sort_opts [
      "--options", CommandUtils.arg_set_string config_options,
        " Initialize with these options. Multiple options are semicolon delimited";
    ] in
    let usage = Printf.sprintf
      "Usage: %s init [ROOT]\n\
       Initializes a directory to be used as a flow root directory\n\n\
       e.g. %s init /path/to/root\n\
       or %s init\n\
       or %s init --options \"optionA=123;optionB=456\"\n\n\
       If the root is not specified it is assumed to be the current working directory\n\n\
       This command will create and initialize /path/to/root/.flowconfig"
       Sys.argv.(0)
       Sys.argv.(0)
       Sys.argv.(0)
       Sys.argv.(0) in
    let args = ClientArgs.parse_without_command options usage "init" in
    let root = match args with
    | [] -> Sys.getcwd () |> Path.mk_path
    | [root] -> Path.mk_path root
    | _      -> Arg.usage options usage; exit 2 in
    let configOptions = match !config_options with
    | None -> []
    | Some str -> Str.split (Str.regexp ";") str in
    { root; configOptions; }

  let main env =
    FlowConfig.init env.root env.configOptions

  let run () = main (parse_args ())
end
