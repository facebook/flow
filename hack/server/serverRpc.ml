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
  | IDENTIFY_FUNCTION : string * int * int ->
      (string IdentifySymbolService.find_symbol_result) option t
  | OUTLINE : string -> (Pos.absolute * string * string) list t
  | METHOD_JUMP : (string * bool) -> MethodJumps.result list t
  | FIND_DEPENDENT_FILES: string list -> string list t
  | FIND_REFS : FindRefsService.action -> FindRefsService.result t
  | REFACTOR : ServerRefactor.action -> ServerRefactor.patch list t
  | DUMP_SYMBOL_INFO : string list -> SymbolInfoService.result t
  | DUMP_AI_INFO : string list -> Ai.InfoService.result t
  | ARGUMENT_INFO : string * int * int -> ServerArgumentInfo.result t
  | SEARCH : string * string -> ServerSearch.result t
  | COVERAGE_COUNTS : string -> ServerCoverageMetric.result t
  | LINT : string list -> ServerLint.result t
  | LINT_ALL : int -> ServerLint.result t
  | CREATE_CHECKPOINT : string -> unit t
  | RETRIEVE_CHECKPOINT : string -> string list option t
  | DELETE_CHECKPOINT : string -> bool t
  | STATS : Stats.t t
  | KILL : unit t
  | FIND_LVAR_REFS : string * int * int -> ServerFindLocals.result t
  | FORMAT : string * int * int -> string Format_hack.return t

let handle : type a. genv -> env -> a t -> a =
  fun genv env -> function
    | STATUS ->
      HackEventLogger.check_response (Errors.get_error_list env.errorl);
        let el = Errors.get_sorted_error_list env.errorl in
        List.map ~f:Errors.to_absolute el
    | COVERAGE_LEVELS fn -> ServerColorFile.go env fn
    | INFER_TYPE (fn, line, char) ->
        ServerInferType.go env (fn, line, char)
    | AUTOCOMPLETE content ->
        ServerAutoComplete.auto_complete env.tcopt env.files_info content
    | IDENTIFY_FUNCTION (content, line, char) ->
        ServerIdentifyFunction.go_absolute content line char env.tcopt
    | OUTLINE content ->
        FileOutline.outline content
    | METHOD_JUMP (class_, find_children) ->
      MethodJumps.get_inheritance env.tcopt class_ ~find_children
        env.files_info genv.workers
    | FIND_DEPENDENT_FILES file_list ->
        Ai.ServerFindDepFiles.go genv.workers file_list
          (ServerArgs.ai_mode genv.options)
    | FIND_REFS find_refs_action ->
        if ServerArgs.ai_mode genv.options = None then
          ServerFindRefs.go find_refs_action genv env
        else
          Ai.ServerFindRefs.go find_refs_action genv env
    | REFACTOR refactor_action -> ServerRefactor.go refactor_action genv env
    | DUMP_SYMBOL_INFO file_list ->
        SymbolInfoService.go genv.workers file_list env
    | DUMP_AI_INFO file_list ->
        Ai.InfoService.go Typing_check_utils.check_defs genv.workers
          file_list (ServerArgs.ai_mode genv.options) env.tcopt
    | ARGUMENT_INFO (contents, line, col) ->
        ServerArgumentInfo.go contents line col
    | SEARCH (query, type_) -> ServerSearch.go genv.workers query type_
    | COVERAGE_COUNTS path -> ServerCoverageMetric.go path genv env
    | LINT fnl -> ServerLint.go genv env fnl
    | LINT_ALL code -> ServerLint.lint_all genv env code
    | CREATE_CHECKPOINT x -> ServerCheckpoint.create_checkpoint x
    | RETRIEVE_CHECKPOINT x -> ServerCheckpoint.retrieve_checkpoint x
    | DELETE_CHECKPOINT x -> ServerCheckpoint.delete_checkpoint x
    | STATS -> Stats.get_stats ()
    | KILL -> ()
    | FIND_LVAR_REFS (content, line, char) ->
        ServerFindLocals.go content line char
    | FORMAT (content, from, to_) ->
        ServerFormat.go content from to_

let to_string : type a. a t -> _ = function
  | STATUS -> "STATUS"
  | INFER_TYPE _ -> "INFER_TYPE"
  | COVERAGE_LEVELS _ -> "COVERAGE_LEVELS"
  | AUTOCOMPLETE _ -> "AUTOCOMPLETE"
  | IDENTIFY_FUNCTION _ -> "IDENTIFY_FUNCTION"
  | OUTLINE _ -> "OUTLINE"
  | METHOD_JUMP _ -> "METHOD_JUMP"
  | FIND_DEPENDENT_FILES _ -> "FIND_DEPENDENT_FILES"
  | FIND_REFS _ -> "FIND_REFS"
  | REFACTOR _ -> "REFACTOR"
  | DUMP_SYMBOL_INFO _ -> "DUMP_SYMBOL_INFO"
  | DUMP_AI_INFO _ -> "DUMP_AI_INFO"
  | ARGUMENT_INFO _ -> "ARGUMENT_INFO"
  | SEARCH _ -> "SEARCH"
  | COVERAGE_COUNTS _ -> "COVERAGE_COUNTS"
  | LINT _ -> "LINT"
  | LINT_ALL _ -> "LINT_ALL"
  | CREATE_CHECKPOINT _ -> "CREATE_CHECKPOINT"
  | RETRIEVE_CHECKPOINT _ -> "RETRIEVE_CHECKPOINT"
  | DELETE_CHECKPOINT _ -> "DELETE_CHECKPOINT"
  | STATS -> "STATS"
  | KILL -> "KILL"
  | FIND_LVAR_REFS _ -> "FIND_LVAR_REFS"
  | FORMAT _ -> "FORMAT"
