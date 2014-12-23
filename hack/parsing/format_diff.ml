(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Helper function, splits the content of a file in a list of lines.  *)
(*****************************************************************************)

let split_lines =
  let re = Str.regexp "[\n]" in
  fun content -> Str.split re content

(*****************************************************************************)
(* Section buiding a list of intervals per file for a given diff.
 *
 * Typically, the output of git/hg looks like:
 * --- a/path/filename
 * +++ b/path/filename
 * @@ -line,length +line, length @@
 *
 * The information that we are interested in is the filename after +++
 * and the line number after '+' in the header section (the section after @@).
 * That's because we don't really care about what has been removed.
 * What we want is to format the new content, not the old one.
 * ParseDiff builds a list of intervals of modified lines for each file in
 * a diff.
 *
 * For example: ["myfile1", [4, 6; 7, 7]] means that the file named "myfile1"
 * has modified lines, from line 4 to 6 (inclusive) and the line 7.
 *)
(*****************************************************************************)

type filename = Relative_path.t
type interval = int * int
type file_diff = filename * interval list

module ParseDiff: sig

  val go: string -> file_diff list

end = struct

  type filename = Relative_path.t
  type interval = int * int
  type file_diff = filename * interval list

  type env = {
      (* The file we are currently parsing (None for '/dev/null') *)
      mutable file: string option;

      (* The list of lines that have been modified *)
      mutable modified: int list;

      (* The current line *)
      mutable line: int;

      (* The accumulator (for the result) *)
      mutable result: file_diff list;
    }

  (* The entry point *)
  let rec go content =
    let env = { file = None; modified = []; line = 0; result = [] } in
    let lines = split_lines content in
    start env lines;
    List.rev env.result

  (* Skip the text before the first +++ (to make things work with git show) *)
  and start env = function
    | [] -> ()
    | line :: lines
      when String.length line > 4 && String.sub line 0 3 = "+++" ->
        header env line;
        modified env 0 lines
    | _ :: lines -> start env lines

  (* Parses the content of a line starting with +++ (extracts the filename) *)
  and header env line =
    add_file env;
    let filename = String.sub line 4 (String.length line - 4) in
    (* Getting rid of the prefix b/ *)
    let filename =
      if filename = "/dev/null"
      then None
      else if String.length filename >= 2 && String.sub filename 0 2 = "b/"
      then Some (String.sub filename 2 (String.length filename - 2))
      else Some filename
    in
    env.file <- filename;
    env.modified <- []

  (* Parses the lines *)
  and modified env nbr = function
    | [] -> add_file env
    | line :: lines
      when String.length line > 4 && String.sub line 0 3 = "+++" ->
        header env line;
        modified env 0 lines
    | line :: lines
      when String.length line > 2 && String.sub line 0 2 = "@@" ->
        (* Find the position right after '+' in '@@ -line,len +line, len@@' *)
        let _ = Str.search_forward (Str.regexp "[+][0-9]+") line 0 in
        let matched = Str.matched_string line in
        let matched = String.sub matched 1 (String.length matched - 1) in
        let nbr = int_of_string matched in
        modified env nbr lines
    | line :: lines
      when String.length line >= 1 && String.sub line 0 1 = "+" ->
        (* Adds the line to the list of modified lines *)
        env.line <- env.line + 1;
        env.modified <- nbr :: env.modified;
        modified env (nbr+1) lines
    | line :: lines
      when String.length line >= 1 && String.sub line 0 1 = "-" ->
        (* Skips the line (we don't care about removed code) *)
        modified env nbr lines
    | _ :: lines ->
        modified env (nbr+1) lines

  and add_file env =
    (* Given a list of modifies lines => returns a list of intervals *)
    let lines_modified = List.rev_map (fun x -> (x, x)) env.modified in
    let lines_modified = normalize_intervals [] lines_modified in
    (* Adds the file to the list of results *)
    match env.file with
    | None -> ()
    | Some filename ->
        let path = Relative_path.concat Relative_path.Root filename in
        env.result <- (path, lines_modified) :: env.result

  (* Merges intervals when necessary.
   * For example: '[(1, 2), (2, 2), (2, 5); ...]' becomes '[(1, 5); ...]'.
   *)
  and normalize_intervals acc = function
    | [] -> List.rev acc
    | (start1, end1) :: (start2, end2) :: rl when end1 + 1 >= start2 ->
        normalize_intervals acc ((min start1 start2, max end1 end2) :: rl)
    | x :: rl -> normalize_intervals (x :: acc) rl

end

(*****************************************************************************)
(* TextBlocks.make produces a list of indivisible blocks for a given file.
 *
 * Let's consider the following input:
 * 1   <?hh
 * 2   $x = 0;
 * 3   $y =
 * 4     1;
 *
 * The result will look like this:
 * [(1, 1, "<?hh\n");
 *  (2, 2, "$x = 0;\n");
 *  (3, 4, "$y = 1;\n");
 * ]
 *
 * The interesting case is the third one (3, 4, "$y = 1;\n"). It means that
 * we should consider the lines [3;4] as an indivisible block. In other words,
 * if we want to replace any line between 3 and 4, we whould replace the
 * entire block.
 *)
(*****************************************************************************)

module TextBlocks: sig

  type line_start = int
  type line_end = int
  type content = string

  type t = (line_start * line_end * content)

  (* Takes the output of the formatter with the "info" option on and
   * produces a list of indivisible text blocks.
   *)
  val make: (string * Format_hack.source_pos list) -> t list

end = struct

  type line_start = int
  type line_end = int
  type content = string

  type t = (line_start * line_end * content)

  type token =
    | Eof
    | Block
    | Line of int
    | Text of string

  (* Merges blocks when they overlap. *)
  let rec merge acc = function
    | [] -> List.rev acc
    | (start1, end1, content1) :: (start2, end2, content2) :: rl
      when start2 <= end1 ->
        merge acc ((start1, end2, content1^content2) :: rl)
    | x :: rl -> merge (x :: acc) rl

  let rec make_token_list text source_info acc i =
    match source_info with
    | [] -> List.rev (Text (String.sub text i (String.length text-i)) :: acc)
    | (char_pos, tag) :: source_info ->
        let acc = Text (String.sub text i (char_pos-i)) :: acc in
        let acc =
          (match tag with
          | Format_hack.Block -> Block
          | Format_hack.Line n -> Line n)
            :: acc
        in
        make_token_list text source_info acc char_pos

  let make_next text source_info =
    let list = ref (make_token_list text source_info [] 0) in
    fun () ->
      match !list with
      | [] -> Eof
      | tok :: rl -> list := rl; tok

  let rec make (text, source_info) =
    let next = make_next text source_info in
    let buffer = Buffer.create 256 in
    let list = loop 1 1 buffer [] next in
    merge [] list

  and loop line_start line_end buffer acc next =
    match next() with
    | Eof -> List.rev acc
    | Block ->
        let line_start =
          if line_start > line_end then line_end else line_start
        in
        let content = Buffer.contents buffer in
        if content = ""
        then loop line_start line_end buffer acc next
        else
          let block = line_start, line_end, content in
          Buffer.clear buffer;
          let line_end = line_end + 1 in
          loop line_end line_end buffer (block :: acc) next
    | Text str ->
        Buffer.add_string buffer str;
        loop line_start line_end buffer acc next
    | Line nbr ->
        let line_end = nbr in
        loop line_start line_end buffer acc next


end

(*****************************************************************************)
(* Given a list of intervals and a list of blocks, determine which
 * indivisible blocks are matching.
 *
 * 'intervals' correspond to the ranges that have been modified by a diff.
 * intervals = [(1, 3); (4; 4)] should be read as: the lines 1 to 3 and the
 * line 4 have been modified (cf ParseDiff module above).
 *
 * 'blocks' is the list of indivible blocks produced by TextBlocks.
 *)
(*****************************************************************************)

let rec matching_blocks acc intervals blocks =
  match intervals, blocks with
  | [], _ | _, [] -> List.rev acc
  | (modif_start, modif_end) :: rest_intervals,
    (block_start, block_end, _ as block) :: rest_blocks ->
      if modif_end < block_start
      then matching_blocks acc rest_intervals blocks
      else if block_end < modif_start
      then matching_blocks acc intervals rest_blocks
      else matching_blocks (block :: acc) intervals rest_blocks

(*****************************************************************************)
(* Skips lines in a list of lines.
 * Returns a string containing the lines skipped and the remaining lines.
 *)
(*****************************************************************************)

let rec cut_lines buffer line_number end_block = function
  | [] -> Buffer.contents buffer, []
  | line :: lines when line_number <= end_block ->
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n';
      cut_lines buffer (line_number+1) end_block lines
  | lines -> Buffer.contents buffer, lines

(*****************************************************************************)
(* Applies the changes to a list of lines:
 * -) 'outc' the output channel that we are printing to
 * -) 'line_number' the current input line_number (not output!)
 * -) 'blocks' the list of indivisible blocks that should be replaced
 *     (cf TextBlocks)
 * -) 'lines' the remaining lines to be treated
 *)
(*****************************************************************************)

let rec apply_blocks outc line_number blocks lines =
  match blocks, lines with
  (* We reached the end of the input file. *)
  | _, [] -> ()

  (* We have no more modifications for that file. *)
  | [], line :: lines ->
      output_string outc line;
      output_char outc '\n';
      apply_blocks outc (line_number+1) blocks lines

  (* We have not reached the beginning of the block yet. *)
  | (start_block, _, _) :: _, line :: lines when line_number < start_block ->
      output_string outc line;
      output_char outc '\n';
      apply_blocks outc (line_number+1) blocks lines

  (* We found a block that must be replaced! *)
  | (start_block, end_block, new_content) :: blocks, lines ->
      (* First, cut the old content. *)
      let buf = Buffer.create 256 in
      let old_content, lines = cut_lines buf line_number end_block lines in
      (* Show what changed *)
      if old_content <> new_content
      then show_change start_block end_block old_content new_content;
      (* Output the change. *)
      output_string outc new_content;
      (* Carry on with the rest of the file. *)
      apply_blocks outc (end_block+1) blocks lines

and show_change start_block end_block old_content new_content =
  Printf.printf "Replacing: %d-%d\n" start_block end_block;
  let old_lines = split_lines old_content in
  let new_lines = split_lines new_content in
  List.iter (Printf.printf "-%s\n") old_lines;
  List.iter (Printf.printf "+%s\n") new_lines

(*****************************************************************************)
(* Formats a diff (in place) *)
(*****************************************************************************)

let parse_diff diff_text =
  ParseDiff.go diff_text

let rec apply ~diff:file_and_lines_modified =
  List.iter begin fun (filepath, modified_lines) ->
    let filename = Relative_path.to_absolute filepath in
    Printf.printf "File: %s\n" filename;
    let file_content = Utils.cat filename in
    apply_file filepath file_content modified_lines
  end file_and_lines_modified

and apply_file filepath file_content modified_lines =
  let filename = Relative_path.to_absolute filepath in
  match Format_hack.program_with_source_metadata filepath file_content with
  | Format_hack.Success formatted_content ->
      apply_formatted filepath formatted_content file_content modified_lines
  | Format_hack.Php_or_decl ->
      Printf.printf "PHP FILE: skipping\n"
  | Format_hack.Parsing_error _ ->
      Printf.fprintf stderr "Parsing error: %s\n" filename
  | Format_hack.Internal_error ->
      Printf.fprintf stderr "*** PANIC *** Internal error!: %s\n" filename

and apply_formatted filepath formatted_content file_content modified_lines =
  let filename = Relative_path.to_absolute filepath in
  let blocks = TextBlocks.make formatted_content in
  let blocks = matching_blocks [] modified_lines blocks in
  let lines = split_lines file_content in
  try
    let outc = open_out filename in
    apply_blocks outc 1 blocks lines;
    close_out outc
  with _ ->
    Printf.fprintf stderr "Error: could not modify file %s\n" filename
