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
type message =
  | BlameM of Loc.t * string
  | CommentM of string
type error = level * message list (* message *) * message list (* trace *)

type pp_message = Loc.t * string
let to_pp = function
  | BlameM (loc, s) -> loc, s
  | CommentM s -> Loc.none, s

type flags = {
  color: Tty.color_mode;
  one_line: bool;
  show_all_errors: bool;
}

let default_flags = {
  color = Tty.Color_Auto;
  one_line = false;
  show_all_errors = false;
}

let message_of_reason reason =
  Reason_js.(BlameM (loc_of_reason reason, desc_of_reason reason))

let message_of_string s =
  CommentM s

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ ->
    message_list @ ((message_of_string "Error path:")::trace_reasons)

let format_reason_color
  ?(first=false)
  ?(one_line=false)
  (message: message)
= Loc.(
  let loc, s = to_pp message in
  let l0, c0 = loc.start.line, loc.start.column + 1 in
  let l1, c1 = loc._end.line, loc._end.column in
  let err_clr  = if first then C.Normal C.Red else C.Normal C.Green in
  let file_clr = if first then C.Bold C.Blue else C.Bold C.Magenta in
  let line_clr = C.Normal C.Yellow in
  let col_clr  = C.Normal C.Cyan in

  let s = if one_line then Str.global_replace (Str.regexp "\n") "\\n" s else s in

  let loc_format =
    (match loc.source with Some filename -> [file_clr, filename] | None -> []) @
      (if l0 > 0 && c0 > 0 && l1 > 0 && c1 > 0 then [
        (C.Normal C.Default, ":");
        (line_clr,           string_of_int l0);
        (C.Normal C.Default, ":");
        (col_clr,            string_of_int c0);
        (C.Normal C.Default, ",")
       ] @ (if l0 < l1 then [
        (line_clr,           string_of_int l1);
        (C.Normal C.Default, ":")
       ] else []) @ [
        (col_clr,            string_of_int c1)
      ] else []) in
  let s_format = [
    (err_clr,            s);
    (C.Normal C.Default, if one_line then "\\n" else "\n");
  ] in
  if loc_format = [] then s_format
  else loc_format @ ((C.Normal C.Default, ": ")::s_format)
)

let print_reason_color ~first ~one_line ~color (message: message) =
  let to_print = format_reason_color ~first ~one_line message in
  (if first then Printf.printf "\n");
  C.print ~color_mode:color to_print

let print_error_color ~one_line ~color e =
  let level, messages, trace_reasons = e in
  let messages = append_trace_reasons messages trace_reasons in
  print_reason_color ~first:true ~one_line ~color (List.hd messages);
  List.iter (print_reason_color ~first:false ~one_line ~color) (List.tl messages)

let loc_of_error err =
  let _, messages, _ = err in
  match messages with
  | message :: _ ->
      let loc, _ = to_pp message in
      loc
  | _ -> Loc.none

let file_of_error err =
  let loc = loc_of_error err in
  match loc.Loc.source with Some filename -> filename | None -> ""

(* TODO: deprecate this in favor of Reason_js.json_of_loc *)
let json_of_loc loc = Loc.(
  let file = match loc.source with Some filename -> filename | None -> "" in
  [ "path", Json.JString file;
    "line", Json.JInt loc.start.line;
    "endline", Json.JInt loc._end.line;
    "start", Json.JInt (loc.start.column + 1);
    "end", Json.JInt loc._end.column ]
)

(* first reason's position, then second reason's position, etc.; if all
   positions match then first message, then second message, etc. *)
let compare =
  let seq k1 k2 = fun () ->
    match k1 () with
    | 0 -> k2 ()
    | i -> i
  in
  let loc_cmp x1 x2 () =
    Loc.compare x1 x2
  in
  let string_cmp x1 x2 () =
    String.compare x1 x2
  in
  let rec compare_message_lists k = function
    | [], [] -> k ()
    | [], _ -> -1
    | _, [] -> 1
    | CommentM(_)::_, BlameM(_)::_ -> -1
    | BlameM(_)::_, CommentM(_)::_ -> 1

    | CommentM(m1)::rest1, CommentM(m2)::rest2 ->
        compare_message_lists (seq k (string_cmp m1 m2)) (rest1, rest2)

    | BlameM(loc1, m1)::rest1, BlameM(loc2, m2)::rest2 ->
        seq (loc_cmp loc1 loc2) (fun () ->
          compare_message_lists (seq k (string_cmp m1 m2)) (rest1, rest2)
        ) ()
  in
  fun (lx, ml1, _) (ly, ml2, _) ->
    compare_message_lists (fun () -> 0) (ml1, ml2)

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

  type error_suppressions = Loc.t SpanMap.t
  type t = {
    suppressions: error_suppressions;
    unused: error_suppressions;
  }

  let empty = {
    suppressions = SpanMap.empty;
    unused = SpanMap.empty;
  }

  let add loc { suppressions; unused; } = Loc.(
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

  let check_loc ((result, { suppressions; unused; }) as acc) loc =
    (* We only want to check the starting position of the reason *)
    let loc = Loc.({ loc with _end = loc.start; }) in
    if SpanMap.mem loc suppressions
    then true, { suppressions; unused = SpanMap.remove loc unused}
    else acc

  (* We need to check every reason in the error message in order to figure out
   * which suppressions are really unused...that's why we don't shortcircuit as
   * soon as we find a matching error suppression
   *)
  let rec check_error_messages acc = function
    | [] -> acc
    | message::errors ->
        let loc, _ = to_pp message in
        let acc = check_loc acc loc in
        check_error_messages acc errors

  (* Checks if an error should be suppressed. *)
  let check err suppressions =
    let _, message_list, _ = err in
    check_error_messages (false, suppressions) message_list

  (* Get's the locations of the suppression comments that are yet unused *)
  let unused { unused; _; } = SpanMap.values unused

  let cardinal { suppressions; unused } =
    SpanMap.cardinal suppressions + SpanMap.cardinal unused
end

let parse_error_to_flow_error (loc, err) =
  let msg = Parse_error.PP.error err in
  ERROR, [BlameM (loc, msg)], []

let to_list errors = ErrorSet.elements errors

(******* Error output functionality working on Hack's error *******)

(* adapted from Errors.to_json to output multi-line errors properly *)
let json_of_error (error : error) = Json.(
  let level, messages, trace_reasons = error in
  let level_str = match level with
  | ERROR -> "error"
  | WARNING -> "warning"
  in
  let messages = append_trace_reasons messages trace_reasons in
  let elts = List.map (fun message ->
      let loc, w = to_pp message in
      JAssoc (("descr", Json.JString w) ::
              ("level", Json.JString level_str) ::
              (json_of_loc loc))
    ) messages
  in
  JAssoc [ "message", JList elts ]
)

let json_of_errors errors = Json.JList (List.map json_of_error errors)

let print_error_json oc el =
  let res =
    if el = [] then
      Json.JAssoc [ "passed", Json.JBool true;
                    "errors", Json.JList [];
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
    else
      Json.JAssoc [ "passed", Json.JBool false;
                    "errors", json_of_errors el;
                    "version", Json.JString Build_id.build_id_ohai;
                  ]
  in
  output_string oc (Json.json_to_string res);
  flush oc

(* for vim and emacs plugins *)
let print_error_deprecated =
  let endline s = if s = "" then "" else s ^ "\n" in
  let to_pp_string message = Loc.(
    let loc, msg = to_pp message in
    let loc_str = match loc.source with
    | None -> ""
    | Some file ->
      let line = loc.start.line in
      let start = loc.start.column + 1 in
      let end_ = loc._end.column in
      if line <= 0 then
        Utils.spf "File \"%s\"" file
      else if line = loc._end.line && start - end_ = 1 then
        Utils.spf "File \"%s\", line %d, character %d" file line start
      else
        Utils.spf "File \"%s\", line %d, characters %d-%d" file line start end_
    in
    Printf.sprintf "%s%s" (endline loc_str) (endline msg)
  ) in
  let to_string (error : error) : string =
    let level, msgl, trace_reasons = error in
    let msgl = append_trace_reasons msgl trace_reasons in
    let buf = Buffer.create 50 in
    (match msgl with
    | [] -> assert false
    | message1 :: rest_of_error ->
        Buffer.add_string buf (to_pp_string message1);
        List.iter begin fun message ->
          Buffer.add_string buf (to_pp_string message)
        end rest_of_error
    );
    Buffer.contents buf
  in
  fun oc el ->
    let sl = List.map to_string el in
    let sl = Utils_js.uniq (List.sort String.compare sl) in
    List.iter begin fun s ->
      if !Utils.debug then begin
        output_string stdout s;
        flush stdout;
      end;
      output_string oc s;
      output_string oc "\n";
    end sl;
    flush oc

(* Human readable output *)
let print_error_summary ~flags errors =
  let error_or_errors n = if n != 1 then "errors" else "error" in
  let truncate = not (flags.show_all_errors) in
  let one_line = flags.one_line in
  let color = flags.color in
  let print_error_if_not_truncated curr e =
    (if not(truncate) || curr < 50 then print_error_color ~one_line ~color e);
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
