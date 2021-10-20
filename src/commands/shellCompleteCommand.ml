(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandInfo

(***********************************************************************)
(* flow shell-complete --current num -- command *)
(***********************************************************************)

module Command (CommandList : COMMAND_LIST) = struct
  let spec =
    {
      CommandSpec.name = "shell-complete";
      doc = "";
      usage = Printf.sprintf "Usage: %s shell-complete --current N -- ARGV\n" CommandUtils.exe_name;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> CommandUtils.from_flag
          |> flag
               "--current"
               (optional int)
               ~doc:"Current term in the argument list being completed."
          |> rest
        );
    }

  let is_partial_flag substr = Str.string_match (Str.regexp "^-") substr 0

  let find_flag key flags =
    if not (is_partial_flag key) then
      None
    else
      try
        let metadata = SMap.find key flags in
        Some metadata.CommandSpec.ArgSpec.arg_count
      with
      | Not_found -> None

  let get_completion command current rest =
    let flags = CommandSpec.flags command in
    let prev = List.nth rest (current - 1) in
    match find_flag prev flags with
    | Some CommandSpec.ArgSpec.No_Arg
    | None ->
      if current < List.length rest && is_partial_flag (List.nth rest current) then
        let flags = SMap.keys flags in
        String.concat " " flags
      else
        "FILE"
    | _ -> "ARGUMENT"

  let main current rest () =
    let current =
      match current with
      | Some x -> x
      | None -> 0
    in
    let rest =
      match rest with
      | Some x -> x
      | None -> []
    in
    if current <= 1 then
      let commands =
        CommandList.commands |> Base.List.map ~f:(fun command -> CommandSpec.name command)
      in
      print_endline (String.concat " " commands)
    else
      try
        let cmdstr = String.lowercase_ascii (List.nth rest 1) in
        let command =
          CommandList.commands |> List.find (fun command -> CommandSpec.name command = cmdstr)
        in
        let completion = get_completion command current rest in
        print_endline completion
      with
      | Not_found -> ()

  let command = CommandSpec.command spec main
end
