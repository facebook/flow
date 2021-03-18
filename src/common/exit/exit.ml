(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include FlowExitStatus

type json_mode = { pretty: bool }

let json_mode = ref None

let set_json_mode ~pretty = json_mode := Some { pretty }

let unset_json_mode () = json_mode := None

let exit =
  let print_json ?msg t =
    match t with
    (* Commands that exit with these exit codes handle json output themselves *)
    | FlowExitStatus.No_error
    | FlowExitStatus.Type_error ->
      ()
    | _ ->
      begin
        match !json_mode with
        | None -> ()
        | Some { pretty } ->
          let json = Hh_json.JSON_Object (FlowExitStatus.json_props_of_t ?msg t) in
          Hh_json.print_json_endline ~pretty json
      end
  in
  fun ?msg t ->
    (match msg with
    | Some msg -> prerr_endline msg
    | None -> ());
    print_json ?msg t;
    if FlowEventLogger.should_log () then FlowEventLogger.exit msg (FlowExitStatus.to_string t);
    Stdlib.exit (FlowExitStatus.error_code t)
