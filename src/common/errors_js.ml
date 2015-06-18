(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module C = Tty
module Json = Hh_json

type level = ERROR | WARNING
type message = (Reason_js.reason * string)
type error = level * message list (* message *) * message list (* trace *)

let pos_range p = Pos.(Lexing.(
  p.pos_start.pos_lnum, p.pos_start.pos_cnum + 1,
    p.pos_end.pos_lnum, p.pos_end.pos_cnum
))

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ ->
      let rev_list = List.rev message_list in
      let head = List.hd rev_list in
      let head = fst head, snd head ^ "\nError path:" in
      let rev_list = head :: List.tl rev_list in
      let message_list = List.rev rev_list in
      message_list @ trace_reasons

let format_reason_color
  ?(first=false)
  ?(one_line=false)
  ((p, s): Pos.t * string)
= Pos.(
  let l0, c0, l1, c1 = pos_range p in
  let err_clr  = if first then C.Normal C.Red else C.Normal C.Green in
  let file_clr = if first then C.Bold C.Blue else C.Bold C.Magenta in
  let line_clr = C.Normal C.Yellow in
  let col_clr  = C.Normal C.Cyan in

  let s = if one_line then Str.global_replace (Str.regexp "\n") "\\n" s else s in

  [file_clr, Relative_path.to_absolute p.pos_file]
  @ (if l0 > 0 && c0 > 0 && l1 > 0 && c1 > 0 then [
      (C.Normal C.Default, ":");
      (line_clr,           string_of_int l0);
      (C.Normal C.Default, ":");
      (col_clr,            string_of_int c0);
      (C.Normal C.Default, ",")
    ]
    @ (if l0 < l1 then [
        (line_clr,           string_of_int l1);
        (C.Normal C.Default, ":")
      ] else [])
    @ [
      (col_clr,            string_of_int c1)
    ] else [])
  @ [
    (C.Normal C.Default, ": ");
    (err_clr,            s);
    (C.Normal C.Default, if one_line then "\\n" else "\n");
  ]
)

let print_reason_color ~(first:bool) ~(one_line:bool) ((reason, s): message) =
  let p = Reason_js.pos_of_reason reason in
  let to_print = format_reason_color ~first ~one_line (p, s) in
  (if first then Printf.printf "\n");
  let should_color = Modes_js.(match modes.color with
    | Always -> true
    | Never -> false
    | Auto -> Unix.isatty Unix.stdout && Sys.getenv "TERM" <> "dumb"
  ) in
  if should_color
  then
    C.print to_print
  else
    let strings = List.map snd to_print in
    List.iter (Printf.printf "%s") strings

let print_error_color ~(one_line:bool) (e:error) =
  let level, messages, trace_reasons = e in
  let messages = append_trace_reasons messages trace_reasons in
  print_reason_color ~first:true ~one_line (List.hd messages);
  List.iter (print_reason_color ~first:false ~one_line) (List.tl messages)

let pos_of_error err =
  let _, messages, _ = err in
  match messages with
  | (reason, _) :: _ -> Reason_js.pos_of_reason reason
  | _ -> Pos.none

let file_of_error err =
  let pos = pos_of_error err in
  Relative_path.to_absolute pos.Pos.pos_file

let pos_to_json pos =
  let file = Pos.(pos.pos_file) in
  let l0, c0, l1, c1 = pos_range pos in
  [ "path", Json.JString (Relative_path.to_absolute file);
    "line", Json.JInt l0;
    "endline", Json.JInt l1;
    "start", Json.JInt c0;
    "end", Json.JInt c1 ]

(* warnings before errors, then first reason's position,
  then second reason's position. If all positions match then first message,
  then second message, etc
  TODO may not work for everything... *)
let compare =
  let rec compare_message_lists ml1 ml2 k_compare_messages =
    match ml1, ml2 with
    | [], [] -> k_compare_messages ()
    | (r1, m1)::rest1, (r2, m2)::rest2 ->
        (match Reason_js.compare r1 r2 with
        | 0 ->
            let k_compare_messages' () =
              match String.compare m1 m2 with
              | 0 -> k_compare_messages ()
              | i -> i
            in compare_message_lists rest1 rest2 k_compare_messages'
        | i -> i)
    | [], _ -> -1
    | _ -> 1

  in fun (lx, ml1, _) (ly, ml2, _) ->
    compare_message_lists ml1 ml2 (fun () -> 0)

module Error = struct
  type t = error
  let compare = compare
end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet = Set.Make(Error)

(* This is a data structure used to track what locations are being suppressed
 * and which suppressions have yet to be used.
 *)
module ErrorSuppressions = struct
  open Span
  module Ast = Spider_monkey_ast

  type error_suppressions = Ast.Loc.t SpanMap.t
  type t = {
    suppressions: error_suppressions;
    unused: error_suppressions;
  }

  let empty = {
    suppressions = SpanMap.empty;
    unused = SpanMap.empty;
  }

  let add loc { suppressions; unused; } = Ast.Loc.(
    let start = { loc.start with line = loc._end.line + 1; column = 0 } in
    let _end = { loc._end with line = loc._end.line + 2; column = 0 } in
    let suppression_loc = { loc with start; _end; } in
    {
      suppressions = SpanMap.add suppression_loc loc suppressions;
      unused = SpanMap.add suppression_loc loc unused;
    }
  )

  let union a b = {
    suppressions = SpanMap.union a.suppressions b.suppressions;
    unused = SpanMap.union a.unused b.unused;
  }

  let check_reason ((result, { suppressions; unused; }) as acc) reason =
    let loc = Reason_js.loc_of_reason reason in

    (* We only want to check the starting position of the reason *)
    let loc = Ast.Loc.({ loc with _end = loc.start; }) in
    if SpanMap.mem loc suppressions
    then true, { suppressions; unused = SpanMap.remove loc unused}
    else acc

  (* We need to check every reason in the error message in order to figure out
   * which suppressions are really unused...that's why we don't shortcircuit as
   * soon as we find a matching error suppression
   *)
  let rec check_error_messages acc = function
    | [] -> acc
    | (reason, error_message)::errors ->
        let acc = check_reason acc reason in
        check_error_messages acc errors

  (* Checks if an error should be suppressed. *)
  let check err suppressions =
    let _, message_list, _ = err in
    check_error_messages (false, suppressions) message_list

  (* Get's the locations of the suppression comments that are yet unused *)
  let unused { unused; _; } = SpanMap.values unused
end

let parse_error_to_flow_error (loc, err) =
  let reason = Reason_js.mk_reason "" loc in
  let msg = Parse_error.PP.error err in
  ERROR, [reason, msg], []

let to_list errors = ErrorSet.elements errors

(******* Error output functionality working on Hack's error *******)

(* adapted from Errors.to_json to output multi-line errors properly *)
let to_json (error : error) = Json.(
  let level, messages, trace_reasons = error in
  let level_str = match level with
  | ERROR -> "error"
  | WARNING -> "warning"
  in
  let messages = append_trace_reasons messages trace_reasons in
  let elts = List.map (fun (reason, w) ->
      JAssoc (("descr", Json.JString w) ::
              ("level", Json.JString level_str) ::
              (pos_to_json (Reason_js.pos_of_reason reason)))
    ) messages
  in
  JAssoc [ "message", JList elts ]
)

let print_errorl_json oc el =
  let res =
    if el = [] then
      Json.JAssoc [ "passed", Json.JBool true;
                    "errors", Json.JList [];
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
    else
      let errors_json = List.map to_json el in
      Json.JAssoc [ "passed", Json.JBool false;
                    "errors", Json.JList errors_json;
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
  in
  output_string oc (Json.json_to_string res);
  flush oc

(* adapted from Errors.to_string to omit error codes *)
let to_string (error : error) : string =
  let level, msgl, trace_reasons = error in
  let msgl = append_trace_reasons msgl trace_reasons in
  let buf = Buffer.create 50 in
  (match msgl with
  | [] -> assert false
  | (reason1, msg1) :: rest_of_error ->
      let pos1 = Reason_js.pos_of_reason reason1 in
      Buffer.add_string buf begin
        Printf.sprintf "%s\n%s\n" (Pos.string (Pos.to_absolute pos1)) msg1
      end;
      List.iter begin fun (reason, w) ->
        let p = Reason_js.pos_of_reason reason in
        let msg = Printf.sprintf "%s\n%s\n" (Pos.string (Pos.to_absolute p)) w
        in Buffer.add_string buf msg
      end rest_of_error
  );
  Buffer.contents buf

let print_errorl use_json el oc =
  if use_json then
    print_errorl_json oc el
  else begin
    if el = []
    then output_string oc "No errors!\n"
    else
      let sl = List.map to_string el in
      let sl = Utils.uniq (List.sort String.compare sl) in
      List.iter begin fun s ->
        if !Utils.debug then begin
          output_string stdout s;
          flush stdout;
        end;
        output_string oc s;
        output_string oc "\n";
      end sl
  end;
  flush oc

(* Human readable output *)
let print_error_summary ?(one_line=false) truncate (errors : error list) =
  let error_or_errors n = if n != 1 then "errors" else "error" in
  let print_error_if_not_truncated curr e =
    (if not(truncate) || curr < 50 then print_error_color ~one_line e);
    curr + 1
  in
  let total =
    List.fold_left print_error_if_not_truncated 0 errors
  in
  print_newline ();
  if truncate && total > 50 then (
    Printf.printf
      "... %d more %s (only 50 out of %d errors displayed)\n"
      (total - 50) (error_or_errors (total - 50)) total;
    print_endline "To see all errors, re-run Flow with --show-all-errors"
  ) else
    Printf.printf "Found %d %s\n" total (error_or_errors total)
