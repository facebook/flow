(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ppxlib

let flowlib_dir_ref = ref ""

let prelude_dir_ref = ref ""

let flowlib_cache = ref None

let prelude_cache = ref None

(* Read in all the flowlib files *)
let get_libs dir =
  Sys.readdir dir
  |> Array.fold_left
       (fun acc file ->
         if Filename.check_suffix file ".js" then
           let contents = Script_utils.string_of_file (Filename.concat dir file) in
           (file, contents) :: acc
         else
           acc)
       []

(* Turn the (name, contents) list into a PPX ast (string * string) array
 * expression *)
let compute lib_dir =
  let open Ast_helper in
  let libs = get_libs lib_dir in
  let hash =
    let state = Xx.init 0L in
    List.iter
      (fun (file, contents) ->
        Xx.update state file;
        Xx.update state contents)
      libs;
    state |> Xx.digest |> Xx.to_string |> Const.string |> Exp.constant
  in
  let contents =
    libs
    |> List.map (fun (name, contents) ->
           Exp.tuple [Exp.constant (Const.string name); Exp.constant (Const.string contents)]
       )
    |> Exp.array
  in
  (hash, contents)

let memo path cache_ref =
  match !cache_ref with
  | Some result -> result
  | None ->
    let result = compute path in
    cache_ref := Some result;
    result

let flowlib_hash ~loc:_ ~path:_ =
  let (hash, _contents) = memo !flowlib_dir_ref flowlib_cache in
  hash

(* Whenever we see [%flowlib_contents], replace it wil the flowlib contents *)
let flowlib_contents ~loc:_ ~path:_ =
  let (_hash, contents) = memo !flowlib_dir_ref flowlib_cache in
  contents

let prelude_hash ~loc:_ ~path:_ =
  let (hash, _contents) = memo !prelude_dir_ref prelude_cache in
  hash

let prelude_contents ~loc:_ ~path:_ =
  let (_hash, contents) = memo !prelude_dir_ref prelude_cache in
  contents

let make_rule name f =
  Context_free.Rule.extension
    (Extension.declare name Extension.Context.expression Ast_pattern.(pstr nil) f)

let rules =
  [
    make_rule "flowlib_hash" flowlib_hash;
    make_rule "flowlib_contents" flowlib_contents;
    make_rule "prelude_hash" prelude_hash;
    make_rule "prelude_contents" prelude_contents;
  ]

let () =
  Driver.add_arg "-flowlib" (Arg.Set_string flowlib_dir_ref) ~doc:"Path to flowlib directory";
  Driver.add_arg "-prelude" (Arg.Set_string prelude_dir_ref) ~doc:"Path to prelude directory";
  Driver.register_transformation ~rules "ppx_gen_flowlibs"
