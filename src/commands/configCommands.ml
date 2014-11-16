module Init = struct
  type env = {
    root : Path.path
  }

  let parse_args () =
    let options = [] in
    let usage = Printf.sprintf
      "Usage: %s init [ROOT]\n\
       Initializes a directory to be used as a flow root directory\n\n\
       e.g. %s init /path/to/root\n\
       or %s init\n\n\
       If the root is not specified it is assumed to be the current working directory\n\n\
       This command will create and initialize /path/to/root/.flowconfig"
       Sys.argv.(0)
       Sys.argv.(0)
       Sys.argv.(0) in
    let args = ClientArgs.parse_without_command options usage "init" in
    match args with
    | [] -> { root = Sys.getcwd () |> Path.mk_path ; }
    | [root] -> { root = Path.mk_path root; }
    | _      -> Arg.usage options usage; exit 2

  let main env =
    FlowConfig.init env.root

  let run () = main (parse_args ())
end
