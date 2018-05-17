(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type client_id = int

type request =
  | Subscribe
  | Autocomplete of (File_input.t * (* request id *) int)
  | DidOpen of (* filename *) string Nel.t
  | DidClose of (* filename *) string Nel.t
  | LspToServer of Lsp.lsp_message (* requests, notifications, responses from client *)

let string_of_request = function
| Subscribe -> "subscribe"
| Autocomplete _ -> "autocomplete"
| DidOpen _ -> "didOpen"
| DidClose _ -> "didClose"
| LspToServer _ -> "lspToServer"

(* Whereas string_of_request returns a normalized description of the request, this returns a more
 * detailed description *)
let denorm_string_of_request = function
| Subscribe -> "subscribe"
| Autocomplete (f, _) -> Printf.sprintf "autocomplete %s" (File_input.filename_of_file_input f)
| DidOpen files -> Printf.sprintf "didOpen %s" (String.concat " " (Nel.to_list files))
| DidClose files -> Printf.sprintf "didClose %s" (String.concat " " (Nel.to_list files))
| LspToServer incoming -> Printf.sprintf "lspToServer %s" (Lsp_fmt.message_to_string incoming)

type response =
  | Errors of {errors: Errors.ErrorSet.t; warnings: Errors.ErrorSet.t}
  | StartRecheck
  | EndRecheck
  | AutocompleteResult of (ServerProt.Response.autocomplete_response * (* request id *) int)
  | DidOpenAck
  | DidCloseAck
  | ServerExit of FlowExitStatus.t (* only used for the subset of exists which client handles *)
  | LspFromServer of Lsp.lsp_message (* requests, notifications, responses to client *)

let string_of_response = function
| Errors _ -> "errors"
| StartRecheck -> "startRecheck"
| EndRecheck -> "endRecheck"
| AutocompleteResult _ -> "autocompleteResult"
| DidOpenAck -> "didOpenAck"
| DidCloseAck -> "didCloseAck"
| ServerExit code -> "serverExit_" ^ (FlowExitStatus.to_string code)
| LspFromServer _ -> "lspFromServer"
