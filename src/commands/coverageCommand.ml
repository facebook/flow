(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow coverage command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "coverage";
    doc = "Shows coverage information for a given file";
    usage =
      Printf.sprintf
        "Usage: %s coverage [OPTION]... [FILE]\n\ne.g. %s coverage foo.js\nor   %s coverage < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> strip_root_flag
        |> from_flag
        |> wait_for_recheck_flag
        |> flag
             "--color"
             truthy
             ~doc:
               ("Print the file with colors showing which parts have unknown types "
               ^ "(blue for 'empty' and red for 'any'). "
               ^ "Cannot be used with --json or --pretty"
               )
        |> flag
             "--debug"
             truthy
             ~doc:
               ("Print debugging info about each range in the file to stderr. "
               ^ "Cannot be used with --json or --pretty"
               )
        |> path_flag
        |> flag "--all" truthy ~doc:"Ignore absence of @flow pragma"
        |> anon "file" (optional string)
      );
  }

let handle_error ~json ~pretty err =
  if json then
    Hh_json.(
      let json = JSON_Object [("error", JSON_String err)] in
      prerr_json_endline ~pretty json
    )
  else
    prerr_endline err

let accum_coverage (checked, empty, total) (_loc, cov) =
  match cov with
  | Coverage.Kind.Any -> (checked, empty, total + 1)
  | Coverage.Kind.Empty -> (checked, empty + 1, total + 1)
  | Coverage.Kind.Checked -> (checked + 1, empty, total + 1)

let accum_coverage_locs (checked, empty, uncovered) (loc, cov) =
  match cov with
  | Coverage.Kind.Any -> (checked, empty, loc :: uncovered)
  | Coverage.Kind.Empty -> (checked, loc :: empty, loc :: uncovered)
  | Coverage.Kind.Checked -> (loc :: checked, empty, uncovered)

let colorize content from_offset to_offset color accum =
  if to_offset > from_offset then
    let substr = String.sub content from_offset (to_offset - from_offset) in
    ((Tty.Normal color, substr) :: accum, to_offset)
  else
    (accum, from_offset)

let debug_range (loc, cov) =
  Loc.(
    prerr_endlinef
      "%d:%d,%d:%d: %b"
      loc.start.line
      loc.start.column
      loc._end.line
      loc._end.column
      (Coverage.Kind.to_bool cov)
  )

let rec colorize_file content last_offset accum = function
  | [] -> colorize content last_offset (String.length content) Tty.Default accum
  | ((offset, end_offset), kind) :: rest ->
    (* catch up to the start of this range *)
    let (accum, offset) = colorize content last_offset offset Tty.Default accum in
    let color =
      match kind with
      | Coverage.Kind.Any -> Tty.Red
      | Coverage.Kind.Empty -> Tty.Blue
      | Coverage.Kind.Checked -> Tty.Default
    in
    let (accum, offset) = colorize content offset end_offset color accum in
    colorize_file content offset accum rest

let sort_ranges ((a_line, a_col), _) ((b_line, b_col), _) =
  let line = a_line - b_line in
  if line = 0 then
    a_col - b_col
  else
    line

let rec split_overlapping_ranges accum = function
  | [] -> accum
  | [range] -> range :: accum
  | (loc1, is_covered1) :: (loc2, is_covered2) :: rest ->
    let ((loc1_start, loc1_end), (loc2_start, loc2_end)) = (loc1, loc2) in
    let (accum, todo) =
      if loc1_end < loc2_start then
        (* range 1 is completely before range 2, so consume range 1 *)
        ((loc1, is_covered1) :: accum, (loc2, is_covered2) :: rest)
      else if loc1_start = loc2_start then
        (* range 1 and 2 start at the same place, so consume range 1 and
           create a new range for the remainder of range 2, if any *)
        let rest =
          if loc1_end <> loc2_end then
            let tail_loc = (loc1_end + 1, loc2_end) in
            List.sort sort_ranges ((loc1, is_covered1) :: (tail_loc, is_covered2) :: rest)
          else
            (loc1, is_covered1) :: rest
        in
        (accum, rest)
      else if loc1_end = loc2_end then
        (* range 1 and 2 end at the same place, so split range 1 and consume
           the first part, which doesn't overlap *)
        let head_loc = (loc1_start, loc2_start - 1) in
        ((head_loc, is_covered1) :: accum, (loc2, is_covered2) :: rest)
      else if loc1_end < loc2_end then
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
        let head_loc = (loc1_start, loc2_start - 1) in
        let overlap_loc = (loc2_start, loc1_end) in
        let tail_loc = (loc1_end + 1, loc2_end) in
        ( (head_loc, is_covered1)
          :: (overlap_loc, Coverage.Kind.m_or (is_covered1, is_covered2))
          :: accum,
          (tail_loc, is_covered2) :: rest
        )
      else
        (* range 2 is in the middle of range 1, so split range 1 and consume
           the first part, which doesn't overlap, and then recurse on
           range2::range1tail::rest *)
        let head_loc = (loc1_start, loc2_start - 1) in
        let tail_loc = (loc2_end + 1, loc1_end) in
        let todo = (loc2, is_covered2) :: (tail_loc, is_covered1) :: rest in
        ((head_loc, is_covered1) :: accum, Base.List.sort ~compare:sort_ranges todo)
    in
    split_overlapping_ranges accum todo

let handle_response
    ~json ~pretty ~strip_root ~color ~debug (types : (Loc.t * Coverage.Kind.t) list) content =
  if debug then Base.List.iter ~f:debug_range types;

  let offset_table = lazy (Offset_utils.make ~kind:Offset_utils.Utf8 content) in
  if color then (
    let coverage_offsets =
      let offset_table = Lazy.force offset_table in
      let loc_to_offset_pair loc =
        Loc.(Offset_utils.offset offset_table loc.start, Offset_utils.offset offset_table loc._end)
      in
      Base.List.map ~f:(fun (loc, covered) -> (loc_to_offset_pair loc, covered)) types
    in
    let coverage_offsets = Base.List.rev (split_overlapping_ranges [] coverage_offsets) in
    let (colors, _) = colorize_file content 0 [] coverage_offsets in
    Tty.cprint ~color_mode:Tty.Color_Always (Base.List.rev colors);
    print_endline ""
  );

  let (covered, empty, total) = Base.List.fold_left ~f:accum_coverage ~init:(0, 0, 0) types in
  let percent =
    if total = 0 then
      100.
    else
      float_of_int covered /. float_of_int total *. 100.
  in
  if json then
    let offset_table = Some (Lazy.force offset_table) in
    let (covered_locs, empty_locs, uncovered_locs) =
      let (covered, empty, uncovered) =
        Base.List.fold_left ~f:accum_coverage_locs ~init:([], [], []) types
      in
      (Base.List.rev covered, Base.List.rev empty, Base.List.rev uncovered)
    in
    Hh_json.(
      let covered_data =
        let covered_locs = Base.List.sort ~compare covered_locs in
        [
          ("covered_count", int_ covered);
          ( "covered_locs",
            JSON_Array (Base.List.map ~f:(Reason.json_of_loc ~strip_root ~offset_table) covered_locs)
          );
        ]
      in
      JSON_Object
        [
          ( "expressions",
            JSON_Object
              (covered_data
              @ [
                  ("uncovered_count", int_ (total - covered));
                  ( "uncovered_locs",
                    JSON_Array
                      (Base.List.map
                         ~f:(Reason.json_of_loc ~strip_root ~offset_table)
                         uncovered_locs
                      )
                  );
                  ("empty_count", int_ empty);
                  ( "empty_locs",
                    JSON_Array
                      (Base.List.map ~f:(Reason.json_of_loc ~strip_root ~offset_table) empty_locs)
                  );
                ]
              )
          );
        ]
      |> print_json_endline ~pretty
    )
  else
    Utils_js.print_endlinef "Covered: %0.2f%% (%d of %d expressions)\n" percent covered total

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    wait_for_recheck
    color
    debug
    path
    all
    filename
    () =
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path filename in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> File_input.path_of_file_input file)
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  (* pretty implies json *)
  let json = json || pretty in
  if color && json then
    raise
      (CommandSpec.Failed_to_parse
         { arg = "--color"; msg = "Can't be used with json flags"; details = None }
      );
  if debug && json then
    raise
      (CommandSpec.Failed_to_parse
         { arg = "--debug"; msg = "Can't be used with json flags"; details = None }
      );

  let request = ServerProt.Request.COVERAGE { input = file; force = all; wait_for_recheck } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.COVERAGE (Error err) -> handle_error ~json ~pretty err
  | ServerProt.Response.COVERAGE (Ok resp) ->
    let content = File_input.content_of_file_input_unsafe file in
    handle_response ~json ~pretty ~strip_root ~color ~debug resp content
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
