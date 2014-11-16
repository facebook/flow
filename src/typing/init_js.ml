(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(* This module sets up the definitions for JavaScript globals. Eventually, this
   module should become redundant: we should be able to automatically interpret
   TypeScript type definition files for these and many other primitives. That
   said, in some cases handcoding may turn out to be necessary because the type
   system is not powerful enough to encode the invariants of a library
   function. In any case, this part of the design must be revisited in the
   future. *)

open Utils

module Ast = Spider_monkey_ast
module Errors = Errors_js
module Files = Files_js
module Flow = Flow_js
module Reason = Reason_js
module TI = Type_inference_js

let parse_lib () =
  !Files_js.lib_files
  |> List.map (fun lib_file ->
    try (
      let lib_content = cat lib_file in
      match fst (Parsing_service_js.do_parse lib_content lib_file) with
      | Some ast -> lib_file, ast
      | _ -> assert false
    )
    with _ ->
      failwith (spf "Can't read library definitions file %s, exiting." lib_file)
  )

let init_lib () =
  parse_lib () |> List.iter (fun (file, ast) ->
    let _, statements, _ = ast in
    Type_inference_js.init file statements
  )

let init () =
  init_lib ();
  Flow.Cache.clear();
  let cx = Flow.master_cx in
  let reason = Reason.builtin_reason "module" in
  let builtin_module = TI.mk_object cx reason in
  Flow.unit_flow cx (builtin_module, Flow.builtins)
