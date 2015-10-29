(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow coverage command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "coverage";
  doc = "Shows coverage information for a given file";
  usage = Printf.sprintf
    "Usage: %s coverage [OPTION]... [FILE]\n\n\
      e.g. %s coverage foo.js\n\
      or   %s coverage < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> flag "--color" no_arg
        ~doc:"Print the file with colors showing which parts have known types"
    |> flag "--debug" no_arg
        ~doc:"Print debugging info about each range in the file to stderr"
    |> flag "--strip-root" no_arg
        ~doc:"Print paths without the root"
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> anon "file" (optional string) ~doc:"[FILE]"
  )
}

let handle_error ~json (loc, err) strip =
  let loc = strip loc in
  if json
  then (
    let loc = Errors_js.json_of_loc loc in
    let json = Hh_json.JSON_Object (("error", Hh_json.JSON_String err) :: loc) in
    output_string stderr ((Hh_json.json_to_string json)^"\n");
  ) else (
    let loc = Reason_js.string_of_loc loc in
    output_string stderr (Utils.spf "%s:\n%s\n" loc err);
  );
  flush stderr

let is_covered ctor = ctor <> "AnyT" && ctor <> "UndefT"

let accum_coverage (covered, total) (loc, ctor, str, reasons) =
  if is_covered ctor
  then (covered + 1, total + 1)
  else (covered, total + 1)

let colorize content from_offset to_offset color accum =
  if to_offset > from_offset then
    let substr = String.sub content from_offset (to_offset - from_offset) in
    (Tty.Normal color, substr)::accum, to_offset
  else accum, from_offset

let debug_range (loc, ctor, str, reasons) = Loc.(
  Utils.prerr_endlinef "%d:%d,%d:%d: %s (%s)"
    loc.start.line loc.start.column
    loc._end.line loc._end.column
    str
    ctor
)

let rec colorize_file content last_offset accum = Loc.(function
  | [] -> colorize content last_offset (String.length content) Tty.Default accum
  | (loc, ctor, str, reasons)::rest ->
    let offset, end_offset = loc.start.offset, loc._end.offset in

    (* catch up to the start of this range *)
    let accum, offset = colorize content last_offset offset Tty.Default accum in

    let color = if not (is_covered ctor) then Tty.Red else Tty.Default in
    let accum, offset = colorize content offset end_offset color accum in
    colorize_file content offset accum rest
)

let sort_ranges (a_loc, _, _, _) (b_loc, _, _, _) = Loc.(Pervasives.compare
  (a_loc.start.offset, a_loc._end.offset)
  (b_loc.start.offset, b_loc._end.offset)
)

let rec split_overlapping_ranges accum = Loc.(function
  | [] -> accum
  | range::[] -> range::accum
  | (loc1, ctor1, str1, reasons1)::(loc2, ctor2, str2, reasons2)::rest ->
      let accum, todo =
        if loc1._end.offset < loc2.start.offset then
          (* range 1 is completely before range 2, so consume range 1 *)
          (loc1, ctor1, str1, reasons1)::accum,
          (loc2, ctor2, str2, reasons2)::rest

        else if loc1.start.offset = loc2.start.offset then
          (* range 1 and 2 start at the same place, so consume range 1 and
             create a new range for the remainder of range 2, if any *)
          let rest =
            if loc1._end.offset <> loc2._end.offset then
              let tail_loc = { loc2 with
                start = { loc2.start with offset = loc1._end.offset + 1 }
              } in
              List.sort sort_ranges (
                (loc1, ctor1, str1, reasons1)::
                (tail_loc, ctor2, str2, reasons2)::
                rest
              )
            else
              (loc1, ctor1, str1, reasons1)::rest
          in
          accum, rest

        else if loc1._end.offset = loc2._end.offset then
          (* range 1 and 2 end at the same place, so split range 1 and consume
             the first part, which doesn't overlap *)
          let head_loc = { loc1 with
            _end = { loc1._end with offset = loc2.start.offset - 1 }
          } in
          (head_loc, ctor1, str1, reasons1)::accum,
          (loc2, ctor2, str2, reasons2)::rest

        else
          (* range 2 is in the middle of range 1, so split range 1 and consume
             the first part, which doesn't overlap, and then recurse on
             range2::range1tail::rest *)
          let head_loc = { loc1 with
            _end = { loc1._end with offset = loc2.start.offset - 1 }
          } in
          let tail_loc = { loc1 with
            start = { loc1.start with offset = loc2._end.offset + 1 }
          } in
          let todo =
            (loc2, ctor2, str2, reasons2)::
            (tail_loc, ctor1, str1, reasons1)::
            rest
          in
          (head_loc, ctor1, str1, reasons1)::accum,
          List.sort sort_ranges todo
      in
      split_overlapping_ranges accum todo
)

let handle_response ~json ~color ~debug types content strip =
  if debug then List.iter debug_range types;

  begin if color then
    let types = split_overlapping_ranges [] types |> List.rev in
    let colors, _ = colorize_file content 0 [] types in
    Tty.cprint (List.rev colors);
    print_endline ""
  end;

  let covered, total = List.fold_left accum_coverage (0, 0) types in
  let percent = (float_of_int covered /. float_of_int total) *. 100. in

  if json then
    let open Hh_json in
    JSON_Object [
      "expressions", JSON_Object [
        "covered_count", int_ covered;
        "uncovered_count", int_ (total - covered);
      ];
    ]
    |> json_to_string
    |> print_endline
  else
    Utils.print_endlinef
      "Covered: %0.2f%% (%d of %d expressions)\n" percent covered total

let main option_values root json color debug strip_root path filename () =
  let file = get_file_from_filename_or_stdin path filename in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.DUMP_TYPES (file, false));

  match (Timeout.input_value ic) with
  | (Some err, None) -> handle_error ~json err (relativize strip_root root)
  | (None, Some resp) ->
      let content = ServerProt.file_input_get_content file in
      handle_response
        ~json ~color ~debug
        resp content (relativize strip_root root)
  | (_, _) -> assert false

let command = CommandSpec.command spec main
