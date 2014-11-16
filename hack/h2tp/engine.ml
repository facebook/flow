(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
module Sys = Sys_ext
module CE = Common_exns
module List = List_ext
module Parser_hack = Parser_hack_ext
module Opts = Converter_options

(*
  The order of maps is important since some of the maps depend upon
  previous transformations.

  1) Misc is at the end since it introduces id calls in various places where
  we might have desugared literal constructs into calls to new.

  2) Empty_collection_as_bool depends on finding literal constructs, so it
   requires to run before we desugar these (Ref_collections). Its also
   preferable to run it before Initializer_expressions since we introduce
   additional code in there based on our lack of knowledge of what each
   expression is doing. By first making that transformation, we can freely
   insert additional code that will not be transformed (since we are aware of
   it's purpose.)

  3) Initializer_expressions makes use of literal syntax and MUST run before
  Ref_collections.
*)

let true_fn = Fun.const true
let collection_mode_only {Opts.convert_collections; _} = convert_collections

let maps = [
  (Unsupported_constructs.map, true_fn);
  (Detect_collections.map, Utils.compose not collection_mode_only);
  (Erase_types.map, true_fn);
  (Constructor_arg_promotion.map, true_fn);
  (Lambda_expressions.map, true_fn);
  (Yield_break.map, true_fn);
  (Value_collections.map, true_fn);
  (Empty_collection_as_bool.map, collection_mode_only);
  (Initializer_expressions.map, collection_mode_only);
  (Ref_collections.map, collection_mode_only);
  (Misc.map, true_fn);
  (Hack_variadic.map, true_fn);
  (Deleted_constructs.map, true_fn);
  (Enum.map, true_fn);
]

let convert ast src dest =
  try
    let env = Opts.get () in
    let fold_fn ast (map, shouldMap) = if shouldMap env then map ast else ast in
    let ast = List.fold_left fold_fn ast maps in
    let src_path = Relative_path.create Relative_path.Dummy src in
    let ast = Prepend_require.prepend ast src_path in
    let str = Unparser.unparse Ast.PhpFile src_path ast in
    let dest = Sys.set_extension dest ".php" in
    Sys.write_file str dest
  with e ->
  match e with
    | CE.Todo m ->
      let m = "error unparsing " ^ src ^ ". " ^ m in
      raise (CE.InternalError m)
    | CE.FormatterError m ->
      let m = m ^ " in file " ^ src ^ "." in
      raise (CE.InternalError m)
    | CE.FileExists _ ->
      let m = "duplicate file named \"" ^ src ^ "\". Do you have two" ^
        "files with the same name and different extensions? (.hh and .php)" in
        raise (CE.InputError m)
    | CE.ConversionError _ | CE.ParseErrors _ | CE.CompoundError _ -> raise e
    | CE.Impossible | _ ->
        let m = "error processing " ^ src in
        raise (CE.InternalError m)

let process_php_file src dest =
  let {Parser_hack.is_hh_file; Parser_hack.ast; _} =
    (* Hack sets Root to the directory containing .hhconfig... but since the
     * dehackificator doesn't have that init sequence, this Root prefix just
     * maps to an empty string, and is used used here just to satisfy the OCaml
     * typechecker. *)
    Parser_hack.parse_or_die (Relative_path.create Relative_path.Dummy src) in
  (* Todo #t5306338 detect and throw error on decl mode files *)
  if is_hh_file
  then convert ast src dest
  else let m = "This tool does not currently support PHP files such as "^
    "\"" ^ src ^"\"" in
    raise (CE.InputError m)

let process_file src dest =
  if Sys.has_extension src ".php" || Sys.has_extension src ".hh"
  then process_php_file src dest
  else Sys.copy_file src dest

let process_filesystem_entry src dest =
  match src with
  | src when Sys.is_symlink src ->
      let m = "The filesystem entry at \"" ^ src ^"\" cannot be handled" in
      raise (CE.InputError m)
  | src when Sys.is_directory src ->
      Sys.copy_dir src dest
  | src when Sys.is_file src ->
      process_file src dest
  | _  ->
      let m = "The filesystem entry at \"" ^ src ^"\" cannot be handled" in
      raise (CE.InputError m)

let go src dest =
  if (Sys.file_exists dest) then raise (CE.InputError (
    "The destination " ^ dest ^ " exists and will not be overwritten."));
  if not (Sys.is_directory src) then raise (CE.InputError (
    "The source \"" ^ src ^ "\" is not a directory. This tool works over " ^
    "directories"));
  Sys.mkdir_p (Filename.dirname dest);
  let exns = ref [] in
  List.iter begin
    fun (s, d) ->
      try
        process_filesystem_entry s d
      with e ->
        exns := e::(!exns)
  end (Sys.recursive_file_pairs src dest);
  if List.not_empty !exns
  then raise (CE.CompoundError !exns)
