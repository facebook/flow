(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open ServerEnv

(* The following datatypes can be interpreted as follows:
 * MESSAGE_TAG : Argument type (sent from client to server) -> return type t *)
type _ t =
  | STATUS : Pos.absolute Errors.error_ list t
  | INFER_TYPE : ServerUtils.file_input * int * int -> ServerInferType.result t
  | COVERAGE_LEVELS : ServerUtils.file_input -> ServerColorFile.result t
  | AUTOCOMPLETE : string -> AutocompleteService.result t
  | IDENTIFY_FUNCTION : string * int * int -> string t
  | OUTLINE : string -> (Pos.absolute * string * string) list t
  | METHOD_JUMP : (string * bool) -> MethodJumps.result list t
  | FIND_REFS : ServerFindRefs.action -> ServerFindRefs.result t
  | REFACTOR : ServerRefactor.action -> ServerRefactor.patch list t
  | DUMP_SYMBOL_INFO : string list -> SymbolInfoService.result t
  | ARGUMENT_INFO : string * int * int -> ServerArgumentInfo.result t
  | PROLOG : string t
  | SEARCH : string * string -> ServerSearch.result t
  | COVERAGE_COUNTS : string -> ServerCoverageMetric.result t
  | LINT : string list -> ServerLint.result t
  | LINT_ALL : int -> ServerLint.result t
  | CREATE_CHECKPOINT : string -> unit t
  | RETRIEVE_CHECKPOINT : string -> string list option t
  | DELETE_CHECKPOINT : string -> bool t
  | KILL : unit t

let handle : type a. genv -> env -> a t -> a =
  fun genv env -> function
    | STATUS ->
        (* Logging can be pretty slow, so do it asynchronously and respond to
         * the client first *)
        ServerEnv.async begin fun () ->
          HackEventLogger.check_response env.errorl;
        end;
        let el = ServerError.sort_errorl env.errorl in
        List.map ~f:Errors.to_absolute el
    | COVERAGE_LEVELS fn -> ServerColorFile.go env fn
    | INFER_TYPE (fn, line, char) ->
        ServerInferType.go env (fn, line, char)
    | AUTOCOMPLETE content ->
        ServerAutoComplete.auto_complete env.nenv content
    | IDENTIFY_FUNCTION (content, line, char) ->
        ServerIdentifyFunction.go content line char
    | OUTLINE content ->
        FileOutline.outline content
    | METHOD_JUMP (class_, find_children) ->
        MethodJumps.get_inheritance class_ find_children env genv
    | FIND_REFS find_refs_action ->
        ServerFindRefs.go find_refs_action genv env
    | REFACTOR refactor_action -> ServerRefactor.go refactor_action genv env
    | DUMP_SYMBOL_INFO file_list ->
        SymbolInfoService.go genv.workers file_list env
    | ARGUMENT_INFO (contents, line, col) ->
        ServerArgumentInfo.go genv env contents line col
    | PROLOG -> PrologMain.go genv env
    | SEARCH (query, type_) -> ServerSearch.go query type_
    | COVERAGE_COUNTS path -> ServerCoverageMetric.go path genv env
    | LINT fnl -> ServerLint.go genv fnl
    | LINT_ALL code -> ServerLint.lint_all genv code
    | CREATE_CHECKPOINT x -> ServerCheckpoint.create_checkpoint x
    | RETRIEVE_CHECKPOINT x -> ServerCheckpoint.retrieve_checkpoint x
    | DELETE_CHECKPOINT x -> ServerCheckpoint.delete_checkpoint x
    | KILL -> ServerEnv.async (fun () -> ServerUtils.die_nicely genv)
