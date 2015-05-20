(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
open Utils
open Sys_utils

(*****************************************************************************)
(* Pretty prints a patch *)
(*****************************************************************************)

let print_patch filename (line, kind, type_) =
  let line = string_of_int line in
  let kind = Typing_suggest.string_of_kind kind in
  let tenv = Typing_env.empty TypecheckerOptions.permissive filename in
  let type_ = Typing_print.full tenv type_ in
  Printf.printf "File: %s, line: %s, kind: %s, type: %s\n" 
    (Relative_path.to_absolute filename) line kind type_

(*****************************************************************************)
(* Applies a patch to a file. *)
(*****************************************************************************)

let add_file env fn =
  let failed_parsing = Relative_path.Set.add fn env.ServerEnv.failed_parsing in
  { env with ServerEnv.failed_parsing = failed_parsing }

(* Maps filenames to the contents of those files, split into lines. Each line
 * is numbered with the line number of the *original* line in the file, before
 * any patches were applied this pass. We need the original line number since
 * patches are targeted to a specific textual position in the file, and if a
 * previous patch added newlines, we need to still know where to look in the
 * file to apply subsequent patches. The line numbers are thus nondecreasing,
 * but may repeat if a line was split into more than one line.
 *
 * As an example, see P2722213 as an input file, which ends up as P2722210 after
 * patching by the code in this file. The latter includes the line numbers we
 * maintain internally, updated after adding newlines from various mutations
 * herein. *)
let file_data = ref (Relative_path.Map.empty :
  (int * string) list Relative_path.Map.t)

let split_and_number content =
  let lines = split_lines content in
  let (_, numbered_lines) = List.fold_left begin fun (n, acc) line ->
    n+1, (n, line)::acc
  end (1, []) lines in
  List.rev numbered_lines

let read_file_raw fn =
  let content = cat_no_fail (Relative_path.to_absolute fn) in
  split_and_number content

let read_file fn =
  match Relative_path.Map.get fn !file_data with
    | Some d -> d
    | None -> read_file_raw fn

let write_file fn numbered_lines =
  let buf = Buffer.create 256 in
  List.iter begin fun (lnum, line) ->
    Buffer.add_string buf line;
    Buffer.add_char buf '\n'
  end numbered_lines;
  let abs_fn = Relative_path.to_absolute fn in
  let oc = open_out_no_fail abs_fn in
  output_string oc (Buffer.contents buf);
  close_out_no_fail abs_fn oc;
  file_data := Relative_path.Map.add fn numbered_lines !file_data

let apply_patch (genv:ServerEnv.genv) (env:ServerEnv.env) fn f =
  Printf.printf "Patching %s: %!" (Relative_path.to_absolute fn);
  let content = read_file fn in
  let patched = f fn content in
  if patched == content
  then begin
    Printf.printf "No patch\n"; flush stdout;
    [], env
  end
  else begin
    write_file fn patched;
    let env = add_file env fn in
    let env = ServerTypeCheck.type_check genv env in
    let errors = env.ServerEnv.errorl in
    if env.ServerEnv.errorl <> []
    then begin
      (* Reverting the changes *)
      write_file fn content;
      let env = add_file env fn in
      Printf.printf "Failed\n"; flush stdout;
      let env = ServerTypeCheck.type_check genv env in
      assert (env.ServerEnv.errorl = []);
      errors, env
    end
    else begin
      Printf.printf "OK\n"; flush stdout;
      [], env
    end
  end

(*****************************************************************************)
(* Section adding return types. *)
(*****************************************************************************)

let insert to_insert str loc =
  String.sub str 0 loc
  ^to_insert^
  String.sub str loc (String.length str - loc)

let rec search_and_insert indent type_ head = function
  | [] -> failwith "Could not find end of function definition"
  | (line_num, line_content) :: rl when String.contains line_content '{' ->
      let type_ = ": @"^type_ in
      let to_fix = String.index_from line_content 0 '{' in
      let to_fix, type_ =
        if to_fix > 1 && line_content.[to_fix - 1] = ' '
        then to_fix - 1, type_
        else to_fix, type_^" "
      in
      let line_len = to_fix + (String.length type_) in
      let len = String.length line_content in
      let insert_newline =
        to_fix > 1 && line_content.[to_fix - 1] = ')' && line_len > 80 in
      let patched_lines = if insert_newline then begin
        let line1 = String.sub line_content 0 (to_fix - 1) in
        let rest_of_line = String.sub line_content to_fix (len - to_fix) in
        let line2 = indent ^ ")" ^ type_ ^ rest_of_line in
        [(line_num, line1); (line_num, line2)]
      end else
        [(line_num, insert type_ line_content to_fix)]
      in
      Printf.printf
        "\n-%s\n+%s\n"
        line_content
        (String.concat "\n" (List.map snd patched_lines));
      List.rev_append head (patched_lines @ rl)
  | x :: rl -> search_and_insert indent type_ (x :: head) rl

let rec add_return_lines patch_line_num type_ acc = function
  | [] -> failwith ("Could not find line " ^ string_of_int patch_line_num)
  | (current_line_num, x) :: _ as l when current_line_num = patch_line_num ->
    let indent =
      if Str.string_match (Str.regexp "^\\( *\\).*") x 0
      then Str.matched_group 1 x
      else failwith begin
        "Expected line "^
        string_of_int current_line_num^
        "/"^
        string_of_int patch_line_num^
        " to always match the regex.\n"^
        x
      end
    in
    search_and_insert indent type_ acc l
  | x :: rl ->
    add_return_lines patch_line_num type_ (x :: acc) rl

let add_return patch_line type_ _ content =
    add_return_lines patch_line type_ [] content

(*****************************************************************************)
(* Section adding member types. *)
(*****************************************************************************)

(* Runs the specified file name through the hackificator to split up its member
 * variable declarations. Returns a tuple of the modified file and a list of
 * line numbers in the modified file corresponding to locations that were
 * split. NB: the modified file has sequential line numbers attached, and needs
 * to be run through reconcile_split in order to figure out what the correct
 * fixed up line numbers are. *)
let split_file fn =
  let abs_fn = Relative_path.to_absolute fn in
  let ic = Unix.open_process_in ("hackificator -split_vars " ^ abs_fn) in
  (match input_line ic with
    | "Split lines:" -> ()
    | wtf -> failwith ("Unexpected output from hackificator:\n" ^ wtf));
  (* Reads in the affected lines data from hackificator. It reports the line
   * numbers in the original input file that it split, which we need to
   * adjust to correspond to the output file that we now have; this is easy
   * since each split line added exactly one newline, so we just add the number
   * of adjustments previous to this line. *)
  let rec loop offset acc = begin
    try
      let n = int_of_string (input_line ic) in
      loop (offset + 1) ((n + offset) :: acc)
    with End_of_file ->
      acc
  end in
  let locs = List.rev (loop 0 []) in
  (match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> ()
    | _ -> failwith "hackificator -split_vars didn't exit cleanly");
  (read_file_raw fn, locs)

(* Reconciles the modified line numbers that we track internally with the
 * modifications that hackificator makes when splitting member vars (see
 * split_file). Uses the information that hackificator spits out about where
 * it split things to match lines in the old file with the new and reconcile the
 * line numbers. *)
let rec reconcile_split orig_content split_content split_locs acc =
  match (orig_content, split_content) with
    | ((o_n, o_l) :: o_c, (s_n, s_l) :: s_c) -> begin
      match split_locs with
        | (n :: split_locs) when n = s_n ->
          (* This line got split by split_vars -- i.e., there's an extra line
           * here that wasn't in the original file. Consume the item from
           * split_locs and from the split file, but do NOT consume the line
           * from orig_content -- since the split line is new by nature of being
           * split, we want to continue parining up the next split line with the
           * same original line. *)
          reconcile_split orig_content s_c split_locs ((o_n, s_l) :: acc)
        | _ ->
          (* If this line wasn't modified, consume from both sides, assigning
           * the old line number to the new data. *)
          reconcile_split o_c s_c split_locs ((o_n, s_l) :: acc)
    end
    | ([], []) -> acc
    | _ -> failwith "Mismatching file lengths"

let rec add_member_lines name patch_line type_ acc = function
  | [] -> failwith ("Could not find line " ^ string_of_int patch_line)
  | (current_line, _) :: _ when current_line > patch_line ->
      failwith
      ("Could not find member " ^ name ^ " on line " ^ string_of_int patch_line)
  | (current_line, line_content) :: rl when current_line = patch_line ->
      let name_re = Str.regexp_string name in
      (try
        let to_fix = Str.search_forward name_re line_content 0 in
        let to_fix =
          try
            (* Dealing with references &$myvar *)
            if line_content.[to_fix - 1] = '&'
            then to_fix - 1
            else to_fix
          with _ -> to_fix
        in
        let patched_line = insert (type_^" ") line_content to_fix in
        Printf.printf "\n-%s\n+%s\n" line_content patched_line;
        flush stdout;
        List.rev_append acc ((current_line, patched_line) :: rl)
      with Not_found ->
        let acc = (current_line, line_content) :: acc in
        add_member_lines name patch_line type_ acc rl)
  | x :: rl ->
      add_member_lines name patch_line type_ (x :: acc) rl

let add_member name patch_line type_ fn content =
  let (split_content, split_locs) = split_file fn in
  let content = reconcile_split content split_content split_locs [] in
  let content = List.rev content in
  let name = if name.[0] <> '$' then "$"^name else name in
  add_member_lines name patch_line type_ [] content

(*****************************************************************************)
(* Section adding parameter types. *)
(*****************************************************************************)
 
let add_soft_param name patch_line type_ _ content =
  (* As it turns out, the core logic for adding member vars works for parameters
   * too. *)
  add_member_lines name patch_line ("@" ^ type_) [] content

(*****************************************************************************)
(* Checking that the root directory is clean. *)
(*****************************************************************************)

let fail_not_clean genv =
  let msg = "The directory is not clean. Fix the errors before converting." in
  Printf.fprintf stderr "%s\n" msg;
  exit 3

let check_no_error genv env =
  match env.ServerEnv.errorl with
  | [] -> ()
  | _ -> fail_not_clean genv

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
open ServerEnv

(* Selects the files we want to annotate. *)
let select_files env dirname =
  Relative_path.Map.fold begin fun fn defs acc ->
    if is_prefix_dir dirname (Relative_path.to_absolute fn)
    then Relative_path.Map.add fn defs acc
    else acc
  end env.files_info Relative_path.Map.empty

(* Infers the types where annotations are missing. *)
let infer_types genv env dirname =
  let fast = select_files env dirname in
  let fast = FileInfo.simplify_fast fast in
  Typing_suggest_service.go genv.workers fast

(* Tries to apply the patches one by one, rolls back if it failed. *)
let apply_patches tried_patches (genv:ServerEnv.genv) env continue patches =
  let tcopt = ServerEnv.typechecker_options !env in
  let tenv = Typing_env.empty tcopt Relative_path.default in
  file_data := Relative_path.Map.empty;
  Relative_path.Map.iter begin fun fn patchl ->
    List.iter begin fun (line, k, type_ as patch) ->
      if Hashtbl.mem tried_patches (fn, patch) then () else
      let go patch =
        let errors, new_env = apply_patch genv !env fn patch in
        env := new_env;
        if errors = []
        then continue := true
      in
      Hashtbl.add tried_patches (fn, patch) true;
      let type_string = Typing_print.full tenv type_ in
      match k with
      | Typing_suggest.Kparam name ->
          go (add_soft_param name line type_string)
      | Typing_suggest.Kreturn ->
          go (add_return line type_string)
      | Typing_suggest.Kmember name ->
          go (add_member name line type_string)
    end patchl
  end patches;
  ()

(* Main entry point *)
let go (genv:ServerEnv.genv) env dirname_path =
  let dirname = Path.to_string dirname_path in
  let env = ref env in
  let continue = ref false in
  check_no_error genv !env;
  let tried_patches = Hashtbl.create 23 in
  let patches = infer_types genv !env dirname in
  apply_patches tried_patches genv env continue patches;
  while !continue do
    continue := false;
    let patches = infer_types genv !env dirname in
    apply_patches tried_patches genv env continue patches;
  done;
  ()
