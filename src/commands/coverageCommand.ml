(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow coverage command *)
(***********************************************************************)

open CommandUtils
open Utils_js

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
    |> base_flags
    |> connect_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> from_flag
    |> flag "--color" no_arg
        ~doc:("Print the file with colors showing which parts have unknown types. " ^
              "Cannot be used with --json or --pretty")
    |> flag "--debug" no_arg
        ~doc:("Print debugging info about each range in the file to stderr. " ^
              "Cannot be used with --json or --pretty")
    |> path_flag
    |> flag "--respect-pragma" no_arg ~doc:"" (* deprecated *)
    |> flag "--all" no_arg
        ~doc:"Ignore absence of @flow pragma"
    |> anon "file" (optional string)
  )
}

let handle_error ~json ~pretty err =
  if json
  then (
    let open Hh_json in
    let json = JSON_Object ["error", JSON_String err] in
    prerr_json_endline ~pretty json;
  ) else (
    prerr_endline err;
  )

let accum_coverage (covered, total) (_loc, is_covered) =
  if is_covered
  then (covered + 1, total + 1)
  else (covered, total + 1)

let colorize content from_offset to_offset color accum =
  if to_offset > from_offset then
    let substr = String.sub content from_offset (to_offset - from_offset) in
    (Tty.Normal color, substr)::accum, to_offset
  else accum, from_offset

let debug_range (loc, is_covered) = Loc.(
  prerr_endlinef "%d:%d,%d:%d: (%b)"
    loc.start.line loc.start.column
    loc._end.line loc._end.column
    is_covered
)

let rec colorize_file content last_offset accum = Loc.(function
  | [] -> colorize content last_offset (String.length content) Tty.Default accum
  | (loc, is_covered)::rest ->
    let offset, end_offset = loc.start.offset, loc._end.offset in

    (* catch up to the start of this range *)
    let accum, offset = colorize content last_offset offset Tty.Default accum in

    let color = if not (is_covered) then Tty.Red else Tty.Default in
    let accum, offset = colorize content offset end_offset color accum in
    colorize_file content offset accum rest
)

let sort_ranges (a_loc, _) (b_loc, _) = Loc.(Pervasives.compare
  (a_loc.start.offset, a_loc._end.offset)
  (b_loc.start.offset, b_loc._end.offset)
)

let rec split_overlapping_ranges accum = Loc.(function
  | [] -> accum
  | range::[] -> range::accum
  | (loc1, is_covered1)::(loc2, is_covered2)::rest ->
      let accum, todo =
        if loc1._end.offset < loc2.start.offset then
          (* range 1 is completely before range 2, so consume range 1 *)
          (loc1, is_covered1)::accum,
          (loc2, is_covered2)::rest

        else if loc1.start.offset = loc2.start.offset then
          (* range 1 and 2 start at the same place, so consume range 1 and
             create a new range for the remainder of range 2, if any *)
          let rest =
            if loc1._end.offset <> loc2._end.offset then
              let tail_loc = { loc2 with
                start = { loc2.start with offset = loc1._end.offset + 1 }
              } in
              List.sort sort_ranges (
                (loc1, is_covered1)::
                (tail_loc, is_covered2)::
                rest
              )
            else
              (loc1, is_covered1)::rest
          in
          accum, rest

        else if loc1._end.offset = loc2._end.offset then
          (* range 1 and 2 end at the same place, so split range 1 and consume
             the first part, which doesn't overlap *)
          let head_loc = { loc1 with
            _end = { loc1._end with offset = loc2.start.offset - 1 }
          } in
          (head_loc, is_covered1)::accum,
          (loc2, is_covered2)::rest

        else if loc1._end.offset < loc2._end.offset then
          (* TODO: Given that at this point we also have loc1.start.offset <
             loc2.start.offset, it means that range 1 and 2 overlap but don't
             nest. Ideally, this case should never arise: we should be able to
             guarantee the invariant that ranges (same as "spans" in
             common/span.ml) are either disjoint or nest. However, some
             combination of bugs and incompleteness in statement.ml and
             parser_flow.ml cause this invariant to be violated. So here we are.

             Split range1, range2, and the overlapping part. Consume the first
             part of range1, which doesn't overlap. Also consume the overlapping
             part, which we assume to be small enough (usually, 1 token) to not
             contain any interesting nested stuff (recall that the overlap is a
             bug, not a feature), and optimistically consider it covered if
             range1 or range2 is covered (because the alternative is 1-token
             islands of uncovered stuff).
          *)
          let head_loc = { loc1 with
            _end = { loc1._end with offset = loc2.start.offset - 1 }
          } in
          let overlap_loc = { loc1 with
            start = loc2.start
          } in
          let tail_loc = { loc2 with
            start = { loc2.start with offset = loc1._end.offset + 1 }
          } in
          (head_loc, is_covered1)::(overlap_loc, is_covered1 || is_covered2)::accum,
          (tail_loc, is_covered2)::rest

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
            (loc2, is_covered2)::
            (tail_loc, is_covered1)::
            rest
          in
          (head_loc, is_covered1)::accum,
          List.sort sort_ranges todo
      in
      split_overlapping_ranges accum todo
)

let handle_response ~json ~pretty ~strip_root ~color ~debug (types : (Loc.t * bool) list) content =
  if debug then List.iter debug_range types;

  begin if color then
    let types = split_overlapping_ranges [] types |> List.rev in
    let colors, _ = colorize_file content 0 [] types in
    Tty.cprint ~color_mode:Tty.Color_Always (List.rev colors);
    print_endline ""
  end;

  let covered, total = List.fold_left accum_coverage (0, 0) types in
  let percent = if total = 0 then 100. else (float_of_int covered /. float_of_int total) *. 100. in

  if json then
    let covered_locs, uncovered_locs =
      let covered, uncovered = List.partition (fun (_, is_covered) -> is_covered) types in
      let locs_of = Core_list.map ~f:(fun (loc, _) -> loc) in
      locs_of covered, locs_of uncovered
    in
    let open Hh_json in
    JSON_Object [
      "expressions", JSON_Object [
        "covered_count", int_ covered;
        "covered_locs", JSON_Array (covered_locs |> Core_list.map ~f:(Reason.json_of_loc ~strip_root));
        "uncovered_count", int_ (total - covered);
        "uncovered_locs", JSON_Array (uncovered_locs |> Core_list.map ~f:(Reason.json_of_loc ~strip_root));
      ];
    ]
    |> print_json_endline ~pretty
  else
    Utils_js.print_endlinef
      "Covered: %0.2f%% (%d of %d expressions)\n" percent covered total

let main
    base_flags option_values json pretty root strip_root from color debug path respect_pragma
    all filename () =
  FlowEventLogger.set_from from;
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name)
    path filename in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  if not option_values.quiet && all && respect_pragma then prerr_endline
    "Warning: --all and --respect-pragma cannot be used together. --all wins.";

  (* TODO: --respect-pragma is deprecated. We will soon flip the default. As a
     transition, --all defaults to enabled. To maintain the current behavior
     going forward, callers should add --all, which currently is a no-op.
     Once we flip the default, --respect-pragma will have no effect and will
     be removed. *)
  let all = all || not respect_pragma in

  (* pretty implies json *)
  let json = json || pretty in

  if color && json
  then raise (CommandSpec.Failed_to_parse ("--color", "Can't be used with json flags"));
  if debug && json
  then raise (CommandSpec.Failed_to_parse ("--debug", "Can't be used with json flags"));

  let request = ServerProt.Request.COVERAGE (file, all) in

  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.COVERAGE (Error err) ->
    handle_error ~json ~pretty err
  | ServerProt.Response.COVERAGE (Ok resp) ->
    let content = File_input.content_of_file_input_unsafe file in
    handle_response ~json ~pretty ~strip_root ~color ~debug resp content
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
