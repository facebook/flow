(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

 module List = Core_list

(*****************************************************************************)
(* Parsing the command line *)
(*****************************************************************************)

let parse_args() =
  let files = ref [] in
  let pattern = ref None in
  let target = ref None in
  let showpatch = ref false in
  let use_hh_f = ref false in
  let verbose = ref false in
  let expr_mode = ref false in
  let stmt_mode = ref false in
  Arg.parse
    [
      "-p", Arg.String (fun x -> pattern := Some x),
      "pattern for the matching";

      "--pattern", Arg.String (fun x -> pattern := Some x),
      "pattern for the matching";

      "--verbose", Arg.Set verbose,
      "print all the matches in the file rather than just the file that
       matched";

      "-t", Arg.String (fun x -> target := Some x),
      "target for the transformation (implied patching)";

      "--target", Arg.String (fun x -> target := Some x),
      "target for the transformation (implied patching)";

      "--showpatch", Arg.Set showpatch,
      "instead of doing the patch will just show the transformations that
       will be done";

      "--use_hh_format", Arg.Set use_hh_f,
      "run patches through hh_format after insertion";

      "-e", Arg.Set expr_mode,
      "Search for a single expression (pattern and target must both be
       nonstrict and consist of a single statement that is an expression)";

      "-s", Arg.Set stmt_mode,
      "Search for a sequence of statments (pattern and target must consist of a
       single block containing a sequence of statements)"
    ]
    (fun file -> files := file :: !files)
    (Printf.sprintf "Usage: %s (filename|directory)" Sys.argv.(0));
  !files, !pattern, !target, !use_hh_f, !verbose, !showpatch, !expr_mode,
  !stmt_mode


let path_to_relative filepath =
  Relative_path.create Relative_path.Dummy (Path.to_string filepath)

(* what happened on a specific file *)
type match_ret =
  | Success
  | NoMatch
  | ActionFailure
  | ParsingException of string
  | Php

(* extract necessary information from the file, match_ret returns Php
   if the file is php *)
let parse_file file : match_ret * string * Parser_hack.parser_return =
  let abs_fn = Path.to_string file in
  let content = Sys_utils.cat abs_fn in
  let parsing_errors, parser_output =
    Errors.do_
      (fun () ->
       let rp = path_to_relative file in
       Parser_hack.program rp content) in
  let pr =
    (* if the file is php, we can't search it *)
    if parser_output.Parser_hack.file_mode = None
    then Php
    else
      if parsing_errors <> []
      then
        (* Get a nice string to display about the parsing exception *)
        ParsingException
          (List.fold_left
             ~f:(fun so_far str_pair ->
                 so_far ^ "\n" ^ fst str_pair ^ " " ^ snd str_pair)
             ~init:""
             (Common_exns.flatten_error
                (Common_exns.ParseErrors parsing_errors)))
      else Success in
  (pr, content, parser_output)

(* Wraps the patch or match call to handle errors *)
let matcher_exception_wrapper
      (fn : match_ret -> string -> Parser_hack.parser_return -> match_ret)
      (txt_file : Path.t) : match_ret =
  let txt_ret, txt_src, txt_parse_ret = parse_file txt_file in
  if txt_ret <> Success
  then txt_ret
  else
    try
      fn txt_ret txt_src txt_parse_ret
    with (* some failure, print it *)
    | Failure msg ->
       print_endline
         ("\nFailed patching " ^ msg ^ " " ^ Path.to_string txt_file);
       ActionFailure
    | Invalid_argument msg ->
       print_endline
         ("\nInvalid arg exception " ^ msg ^ " " ^ Path.to_string txt_file);
       ActionFailure
    | Not_found ->
       print_endline
         ("\nNot found Exception " ^ Path.to_string txt_file);
       ActionFailure
    | _ ->
       print_endline
         ("\nGot some kind of error in patching " ^ Path.to_string txt_file);
       ActionFailure

(* wraps the patching or matching job, handles calling the appropriate
   function on each file in the bucket it is given *)
let hh_match_job
      (type a)
      (pat_info : a)
      (acc : int)
      (fnl : Path.t list)
      (fn : a -> Path.t -> match_ret) : int =
  try
  List.fold_left
    ~f:(fun acc filename ->
        try
          (* try to do the patch *)
          let return = fn pat_info filename in
          (match return with
           | Success -> acc + 1
           | ParsingException e -> begin
              print_endline
                ("\nParsing failure on file " ^
                  (Path.to_string filename) ^ "\n" ^ e);
              acc end
           | _ -> acc)
        with _ -> begin
          print_endline ("\nexception on file " ^ (Path.to_string filename));
          acc end)
    ~init:0
    fnl
  with _ ->
       let _ = print_endline "failed on job" in
       acc

(* Handle a directory by bucketing the contained php files and sending
   them off to worker threads *)
let directory dir fn =
  let path = Path.make dir in
  let next = Utils.compose
    (List.map ~f:Path.make)
    (Find.make_next_files ~filter:FindUtils.is_php path) in
  let workers = Worker.make GlobalConfig.nbr_procs GlobalConfig.gc_control in
  (* Information for the patcher to figure out what transformations to do *)
  let fileschanged =
    MultiWorker.call
      (Some workers)
      ~job:fn
      ~neutral:0
      ~merge:(+)
      ~next
  in
  let _ = Printf.printf "Succeeded in matching %d files" fileschanged in
  print_endline ""

(*****************************************************************************)
(* Actually does the patching *)
(*****************************************************************************)

(* Patches a single file *)
let patch_file pat_info txt_file : match_ret =
  let (transformations, pattern_ast, format_patches), is_expr, is_stmt =
    pat_info in
  let patch_fun =
    if is_expr
    then Matcher.patch_expr
    else if is_stmt
    then Matcher.patch_stmt
    else Matcher.match_and_patch in
  matcher_exception_wrapper
    (fun _ txt_src txt_parse_ret ->
      let new_source =
        patch_fun
          txt_parse_ret.Parser_hack.ast
          (path_to_relative txt_file)
          txt_src
          pattern_ast
          transformations
          ~use_hh_format:format_patches in
      match new_source with
      | None -> NoMatch (* nothing to patch in this file *)
      | Some new_source -> begin
          print_endline ("Success: " ^ (Path.to_string txt_file));
          (* write the new file to disk *)
          let oc = open_out (Path.to_string txt_file) in
          output_string oc new_source;
          close_out oc;
          Success end)
    txt_file

(*****************************************************************************)
(* Patch helpers *)
(*****************************************************************************)

(* Gets the patches to apply and the pattern to match *)
let preproc_patch pat_file tgt_file format_patches = begin
  (* Gets the code of each file and parses it *)
  let pat_ret, pat_src, pat_parse_ret = parse_file pat_file in
  let tgt_ret, tgt_src, tgt_parse_ret = parse_file tgt_file in
  (if pat_ret <> Success || tgt_ret <> Success
  then failwith "Pattern or target is misformatted and doesn't parse");
    (* preprocess pattern and target *)
  Patcher.preprocess_patch_file
    (path_to_relative pat_file) pat_src pat_parse_ret
    (path_to_relative tgt_file) tgt_src tgt_parse_ret,
  pat_parse_ret, format_patches end

(* job for the worker pool to patch a bucket of files *)
let patch_job pat_info acc fnl =
  hh_match_job pat_info acc fnl patch_file

(*****************************************************************************)
(* Actually does the matching *)
(*****************************************************************************)

(* Does matching over a single file *)
let match_file pat_info txt_file : match_ret =
  let (pat_parse_ret, _pat_file), print_verbose, is_expr, is_stmt = pat_info in
  let match_fun =
    if is_expr
    then Matcher.find_matches_expr
    else if is_stmt
    then Matcher.find_matches_stmt
    else Matcher.find_matches in
  matcher_exception_wrapper
    (fun _ txt_src txt_parse_ret ->
     let match_res =
       match_fun
         txt_parse_ret.Parser_hack.ast
         (path_to_relative txt_file)
         txt_src
         pat_parse_ret in
     match match_res with
     | [] -> NoMatch
     | _ -> begin
         (* Want it as all one string to not interleave prints between
            various worker threads *)
         print_endline ("Matched: " ^ (Path.to_string txt_file) ^
         if print_verbose
         then
           "\n" ^
           Matcher.format_matches match_res txt_src
         else "");
         Success end)
    txt_file

(*****************************************************************************)
(* Match helpers *)
(*****************************************************************************)

(* Gets the patches to apply and the pattern to match *)
let preproc_match pat_file = begin
  (* Gets the code of each file and parses it *)
  let pat_ret, _, pat_parse_ret = parse_file pat_file in
  (if pat_ret <> Success
  then failwith "Pattern is misformatted and doesn't parse");
    (* preprocess pattern and target *)
  pat_parse_ret, pat_file end

(* job for the worker pool to patch a bucket of files *)
let match_job pat_info acc fnl =
  hh_match_job pat_info acc fnl match_file

(*****************************************************************************)
(* The main entry point. *)
(*****************************************************************************)

let () =
  SharedMem.(init default_config);
  PidLog.log_oc := Some (open_out "/dev/null");
  let files, pattern, target, format_patches, verbose, showpatch, expr_mode,
    stmt_mode = parse_args() in
  (* Make sure there's a pattern and a target *)
  let pattern = match pattern with
    | None -> Printf.eprintf "No pattern specified\n"; exit 2
    | Some fname -> Path.make fname in
  match target with
  | None -> begin (* Matching not patching *)
     match files with
     | [dir] when Sys.is_directory dir ->
        let pat_info = (preproc_match pattern), verbose, expr_mode, stmt_mode in
        directory dir (match_job pat_info)
     | [filename] ->
        let filepath = Path.make filename in
        let pat_info = (preproc_match pattern), verbose, expr_mode, stmt_mode in
        ignore(match_file pat_info filepath)
     | _ ->
        Printf.eprintf "More than one file given\n";
        exit 2 end
  | Some fname -> (* Patching not matching *)
     let target = Path.make fname in
     let pat_info =
       (preproc_patch pattern target format_patches), expr_mode, stmt_mode in
     if showpatch
     then begin
       let (transfs,_,_),_,_ = pat_info in
       let relpat = path_to_relative pattern in
       let strns =
         Patcher.to_string_patch_maps
           transfs relpat (Sys_utils.cat (Relative_path.to_absolute relpat)) in
       print_endline "Stmt transformations:";
       List.fold_left
         ~f:(fun _ before -> print_endline before)
         ~init:()
         (fst strns);
       print_endline "Expr transformations:";
       List.fold_left
         ~f:(fun _ before -> print_endline before)
         ~init:()
         (snd strns) end
     else
       match files with
       | [dir] when Sys.is_directory dir ->
          directory dir (patch_job pat_info)
       | [filename] ->
          let filepath = Path.make filename in
          ignore(patch_file pat_info filepath)
       | _ ->
          Printf.eprintf "More than one file given\n";
          exit 2
