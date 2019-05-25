(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    { CommandSpec.
      name = "autofix";
      doc = "Modify code using Flow's analysis";
      usage = Printf.sprintf
        "Usage: %s autofix SUBCOMMAND [OPTIONS]...\n\
         Generate code using information available to Flow\n\n\
         SUBCOMMANDS:\n\
            suggest: Print a file filling in all missing type annotations\n"
        CommandUtils.exe_name;
      args = CommandSpec.ArgSpec.(
         empty
         |> anon "subcommand" (required (command [
              "suggest", SuggestCommand.command;]))
       )
     } in
  CommandSpec.command spec main
