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

let pos_range p = Pos.(Lexing.(
  p.pos_start.pos_lnum, p.pos_start.pos_cnum + 1,
    p.pos_end.pos_lnum, p.pos_end.pos_cnum
))

let print_reason_color ~(first:bool) ((p, s): Pos.t * string) = Pos.(
  let l0, c0, l1, c1 = pos_range p in
  let err_clr  = if first then C.Normal C.Red else C.Normal C.Green in
  let file_clr = if first then C.Bold C.Blue else C.Bold C.Magenta in
  let line_clr = C.Normal C.Yellow in
  let col_clr  = C.Normal C.Cyan in

  let to_print = [
    (file_clr, Relative_path.to_absolute p.pos_file)
  ]
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
    (C.Normal C.Default, "\n");
  ] in

  if not first then Printf.printf "  " else Printf.printf "\n";
  if Unix.isatty Unix.stdout
  then
    C.print to_print
  else
    let strings = List.map (fun (_,x) -> x) to_print in
    List.iter (Printf.printf "%s") strings
)

let print_error_color (e:Errors.error) =
  let e = Errors.to_list e in
  print_reason_color ~first:true (List.hd e);
  List.iter (print_reason_color ~first:false) (List.tl e)

type level = ERROR | WARNING

type error = level * (Reason_js.reason * string) list

let file_of_error err =
  let _, messages = err in
  let pos = match messages with
  | (reason, _) :: _ -> Reason_js.pos_of_reason reason
  | _ -> Pos.none in
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

  in fun (lx, ml1) (ly, ml2) ->
    compare_message_lists ml1 ml2 (fun () -> 0)

module Error = struct
  type t = error
  let compare = compare
end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet = Set.Make(Error)

(******* TODO move to hack structure throughout ********)

let flow_error_to_hack_error flow_err =
  let level, messages = flow_err in
  let message_list = List.map (fun (reason, message) ->
    Reason_js.pos_of_reason reason, message
  ) messages in
  Errors.make_error message_list

let parse_error_to_flow_error (loc, err) =
  let reason = Reason_js.mk_reason "" loc in
  let msg = Parse_error.PP.error err in
  ERROR, [reason, msg]

let parse_error_to_hack_error parse_err =
  flow_error_to_hack_error (parse_error_to_flow_error parse_err)

(******* Error output functionality working on Hack's error *******)

(* adapted from Errors.to_json to output multi-line errors properly *)
let to_json (error : Errors.error) = Json.(
  let error_code = Errors.get_code error in
  let elts = List.map (fun (p, w) ->
      JAssoc (("descr", Json.JString w) ::
              ("code",  Json.JInt error_code) ::
              (pos_to_json p))
    ) (Errors.to_list error)
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
let to_string (e : Errors.error) : string =
  let msgl = Errors.to_list e in
  let buf = Buffer.create 50 in
  (match msgl with
  | [] -> assert false
  | (pos1, msg1) :: rest_of_error ->
      Buffer.add_string buf begin
        Printf.sprintf "%s\n%s\n" (Pos.string (Pos.to_absolute pos1)) msg1
      end;
      List.iter begin fun (p, w) ->
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
let print_error_summary truncate errors =
  let error_or_errors n = if n != 1 then "errors" else "error" in
  let print_error_if_not_truncated curr e =
    (if not(truncate) || curr < 50 then print_error_color e);
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
