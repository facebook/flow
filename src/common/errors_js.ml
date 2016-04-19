(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type error_kind =
  | ParseError
  | InferError
  | InferWarning
  | InternalError

let string_of_kind = function
| ParseError -> "ParseError"
| InferError -> "InferError"
| InferWarning -> "InferWarning"
| InternalError -> "InternalError"

(* internal rep for core info *)
type message =
  | BlameM of Loc.t * string
  | CommentM of string

(* simple structure for callers to specify error message content,
   converted to message internally. *)
type info = Loc.t * string list

(** for extra info, enough structure to do simple tree-shaped output *)
type info_tree =
  | InfoLeaf of info list
  | InfoNode of info list * info_tree list

let info_to_message (loc, msgs) =
  BlameM (loc, String.concat "" msgs)

type error = {
  kind: error_kind;
  messages: message list;
  op: message option;
  trace: message list;
  extra: info_tree list
}

let info_to_messages = function
| loc, [] -> [BlameM (loc, "")]
| loc, msg :: msgs ->
  BlameM (loc, msg) ::
  (msgs |> List.map (fun msg -> CommentM msg))

let infos_to_messages infos =
  List.concat (List.map info_to_messages infos)

let mk_error ?(kind=InferError) ?op_info ?trace_infos ?(extra=[]) infos =  {
  kind;
  messages = infos_to_messages infos;
  op = Utils_js.opt_map info_to_message op_info;
  trace = Utils_js.opt_map_default infos_to_messages [] trace_infos;
  extra
}

let simple_error ?(kind=InferError) loc msg =
  mk_error ~kind [loc, [msg]]

let internal_error filename msg =
  let position = Loc.({
    line = 1;
    column = 0;
    offset = 0;
  }) in
  let loc = Loc.({
    source = Some filename;
    start = position;
    _end = position;
  }) in
  simple_error ~kind:InternalError loc msg


(*******************************)

let to_pp = function
  | BlameM (loc, s) -> loc, s
  | CommentM s -> Loc.none, s

type stdin_file = (Path.t * string) option

let message_of_string s =
  CommentM s

let internal_error_prefix = "Internal error (see logs): "

let prepend_kind_message messages kind =
  match kind, messages with
  | _, BlameM ({Loc.source = Some Loc.LibFile _; _} as loc, _) :: _ ->
      let header = match kind with
      | ParseError -> "Library parse error:"
      | InferError -> "Library type error:"
      (* TODO: we don't publicly distinguish warnings vs errors right now *)
      | InferWarning -> "Library type error:"
      | InternalError -> internal_error_prefix
      in
      BlameM (loc, header) :: messages
  | InternalError, BlameM (loc, msg) :: messages ->
      BlameM (loc, internal_error_prefix^msg) :: messages
  | InternalError, CommentM msg :: messages ->
      CommentM (internal_error_prefix^msg) :: messages
  | _ -> messages

let prepend_op_reason messages = function
  | Some message -> message :: (message_of_string "Error:") :: messages
  | None -> messages

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ ->
    message_list @ (BlameM (Loc.none, "Trace:") :: trace_reasons)

(* note: this is used only in old output format, which is deprecated
   except for error diffs. indenting would be hard (locs are printed
   hard left in the old format), so we leave the tree flat. If there
   are any use cases for humans reading the old format, will need to
 revisit. *)
let messages_of_extra extra =
  let rec messages_of_info_tree tree =
    let infos, kids = match tree with
      | InfoLeaf infos -> infos, []
      | InfoNode (infos, kids) -> infos, kids
    in
    infos_to_messages infos @
    List.concat (List.map messages_of_info_tree kids)
  in
  List.concat (List.map messages_of_info_tree extra)

let append_extra_info messages extra =
  messages @ messages_of_extra extra

let rec strip_root_from_info_tree root tree =
  let strip_root_from_infos root =
    List.map (function (loc, strs) ->
      Reason_js.strip_root_from_loc root loc, strs
    )
  in
  match tree with
  | InfoLeaf infos ->
    InfoLeaf (strip_root_from_infos root infos)
  | InfoNode (infos, kids) ->
    InfoNode (
      strip_root_from_infos root infos,
      List.map (strip_root_from_info_tree root) kids
    )

let strip_root_from_extra root extra =
  List.map (strip_root_from_info_tree root) extra

let strip_root_from_message root = function
  | BlameM (loc, s) -> BlameM (Reason_js.strip_root_from_loc root loc, s)
  | CommentM s -> CommentM s

let strip_root_from_reason_list root messages =
  List.map (strip_root_from_message root) messages

let strip_root_from_error root error =
  let { messages; trace; op; extra; _ } = error in
  let messages = strip_root_from_reason_list root messages in
  let trace = strip_root_from_reason_list root trace in
  let op = match op with
  | Some op -> Some (strip_root_from_message root op)
  | None -> None
  in
  let extra = strip_root_from_extra root extra in
  { error with messages; trace; op; extra }

let strip_root_from_errors root errors =
  (* TODO verify this is still worth doing, otherwise just List.map it *)
  let ae = Array.of_list errors in
  Array.iteri (fun i error ->
    ae.(i) <- strip_root_from_error root error
  ) ae;
  Array.to_list ae

let format_reason_color
  ?(first=false)
  ?(one_line=false)
  (message: message)
= Loc.(
  let loc, s = to_pp message in
  let l0, c0 = loc.start.line, loc.start.column + 1 in
  let l1, c1 = loc._end.line, loc._end.column in
  let err_clr  = if first then Tty.Normal Tty.Red else Tty.Normal Tty.Green in
  let file_clr = if first then Tty.Bold Tty.Blue else Tty.Bold Tty.Magenta in
  let line_clr = Tty.Normal Tty.Yellow in
  let col_clr  = Tty.Normal Tty.Cyan in

  let s = if one_line then Str.global_replace (Str.regexp "\n") "\\n" s else s in

  let source = match loc.source with
  | Some LibFile filename
  | Some SourceFile filename
  | Some JsonFile filename -> [file_clr, filename]
  | None | Some Builtins -> []
  in
  let loc_format =
    source @
      (if l0 > 0 && c0 > 0 && l1 > 0 && c1 > 0 then [
        (Tty.Normal Tty.Default, ":");
        (line_clr,           string_of_int l0);
        (Tty.Normal Tty.Default, ":");
        (col_clr,            string_of_int c0);
        (Tty.Normal Tty.Default, ",")
       ] @ (if l0 < l1 then [
        (line_clr,           string_of_int l1);
        (Tty.Normal Tty.Default, ":")
       ] else []) @ [
        (col_clr,            string_of_int c1)
      ] else []) in
  let s_format = [
    (err_clr,            s);
    (Tty.Normal Tty.Default, if one_line then "\\n" else "\n");
  ] in
  if loc_format = [] then s_format
  else loc_format @ ((Tty.Normal Tty.Default, ": ")::s_format)
)

let format_info info =
  let msg = info_to_message info in
  let formatted = format_reason_color msg in
  String.concat "" (List.map snd formatted)

let print_reason_color ~first ~one_line ~color (message: message) =
  let to_print = format_reason_color ~first ~one_line message in
  (if first then Printf.printf "\n");
  Tty.cprint ~color_mode:color to_print

let print_error_color_old ~one_line ~color (e : error) =
  let { kind; messages; op; trace; extra } = e in
  let messages = prepend_kind_message messages kind in
  let messages = prepend_op_reason messages op in
  let messages = append_extra_info messages extra in
  let messages = append_trace_reasons messages trace in
  print_reason_color ~first:true ~one_line ~color (List.hd messages);
  List.iter (print_reason_color ~first:false ~one_line ~color) (List.tl messages)

let file_location_style text = (Tty.Underline Tty.Default, text)
let default_style text = (Tty.Normal Tty.Default, text)
let source_fragment_style text = (Tty.Normal Tty.Default, text)
let error_fragment_style text = (Tty.Normal Tty.Red, text)
let line_number_style text = (Tty.Bold Tty.Default, text)
let comment_style text = (Tty.Bold Tty.Default, text)
let comment_file_style text = (Tty.BoldUnderline Tty.Default, text)


let lib_prefix = "[LIB] "
let is_short_lib filename =
  let len = String.length lib_prefix in
  String.length filename > len && String.sub filename 0 len = lib_prefix

let relative_path ~strip_root ~root filename =
  if is_short_lib filename || Filename.is_relative filename
  then filename
  else if strip_root
  then Files_js.relative_path (Path.to_string root) filename
  else begin
    let relname = Files_js.relative_path (Sys.getcwd ()) filename in
    if String.length relname < String.length filename
      then relname
      else filename
  end

let highlight_error_in_line line c0 c1 =
  let prefix = String.sub line 0 c0 in
  let fragment = String.sub line c0 (c1 - c0) in
  let suffix = String.sub line c1 ((String.length line) - c1) in
  [
    source_fragment_style prefix;
    error_fragment_style fragment;
    source_fragment_style suffix;
  ]

(* 0-indexed *)
let nth_line ~n content =
  let rec loop ~n ~pos content =
    if pos > String.length content then
      raise (Invalid_argument "not enough lines");
    let next_newline =
      try String.index_from content pos '\n'
      with Not_found -> String.length content
    in
    if n < 0 then
      raise (Invalid_argument "can't choose negative line")
    else if n = 0 then
      String.sub content pos (next_newline - pos)
    else
      loop ~n:(n - 1) ~pos:(next_newline + 1) content
  in
  loop ~n ~pos:0 content

let read_line_in_file line filename stdin_file =
  match filename with
  | None ->
      None
  | Some filename ->
      try begin
        let content = match stdin_file with
        | Some (stdin_filename, content)
          when Path.to_string stdin_filename = filename ->
            content
        | _ ->
            Sys_utils.cat filename
        in
        try Some (nth_line ~n:line content)
        with Invalid_argument _ -> None
      end with Sys_error _ -> None

let file_of_source source =
  match source with
    | Some Loc.LibFile filename ->
        let filename =
          if is_short_lib filename
          then begin
            let prefix_len = String.length lib_prefix in
            String.sub filename prefix_len (String.length filename - prefix_len)
          end else filename in
        Some filename
    | Some Loc.SourceFile filename
    | Some Loc.JsonFile filename ->
        Some filename
    | Some Loc.Builtins -> None
    | None -> None

let print_file_at_location ~strip_root ~root stdin_file main_file loc s = Loc.(
  let l0 = loc.start.line in
  let l1 = loc._end.line in
  let c0 = loc.start.column in
  let c1 = loc._end.column in
  let filename = file_of_source loc.source in

  let see_another_file ~is_lib filename =
    if filename = main_file
    then [(default_style "")]
    else [
      comment_style (Printf.sprintf ". See%s: " (if is_lib then " lib" else ""));
      comment_file_style (Printf.sprintf
        "%s:%d"
        (relative_path ~strip_root ~root filename)
        l0)
    ] in

  let code_line = read_line_in_file (l0 - 1) filename stdin_file in

  match code_line, filename with
  | _, None ->
    [
      comment_style s;
      default_style "\n";
    ]
  | None, _ ->
      let original_filename, is_lib = match filename, loc.source with
      | Some filename, Some Loc.LibFile _
      | None, Some Loc.LibFile filename -> filename,true
      | Some filename, _
      | None, Some Loc.SourceFile filename
      | None, Some Loc.JsonFile filename -> filename, false
      | None, Some Loc.Builtins
      | None, None -> failwith "Should only have lib and source files at this point" in
      [comment_style s] @
      (see_another_file ~is_lib original_filename) @
      [default_style "\n"];
  | Some code_line, Some filename ->
      let is_lib = match loc.source with
      | Some Loc.LibFile _ -> true
      | _ -> false in
      let line_number_text = Printf.sprintf "%3d: " l0 in
      let highlighted_line = if (l1 == l0) && (String.length code_line) >= c1
        then highlight_error_in_line code_line c0 c1
        else [source_fragment_style code_line]
      in
      let padding =
        let line_num = String.make (String.length line_number_text) ' ' in
        let spaces =
          if String.length code_line <= c0 then ""
          else
            let prefix = String.sub code_line 0 c0 in
            Str.global_replace (Str.regexp "[^\t ]") " " prefix
        in
        line_num ^ spaces
      in
      let underline_size = if l1 == l0
        then max 1 (c1 - c0)
        else 1
      in
      let underline = String.make underline_size '^' in
      line_number_style line_number_text ::
      highlighted_line @
      [comment_style (Printf.sprintf "\n%s%s %s" padding underline s)] @
      (see_another_file ~is_lib filename) @
      [default_style "\n"]
)

let print_message_nice ~strip_root ~root stdin_file main_file message =
  let loc, s = to_pp message in
  print_file_at_location ~strip_root ~root stdin_file main_file loc s

let print_extra_info =
  let nonterm_newline = Str.regexp "\n\\(.\\)" in
  let rec f ~strip_root ~root stdin_file main_file indent tree =
    let infos, kids = match tree with
      | InfoLeaf infos -> infos, []
      | InfoNode (infos, kids) -> infos, kids
    in
    let lines = List.map (fun (loc, strs) ->
      let msg = String.concat ". " strs in
      let lines = print_file_at_location ~strip_root ~root stdin_file main_file loc msg in
      match lines with
      | [] -> []
      | (style, str) :: lines ->
        let nonterm_newline_plus_indent = Utils.spf "\n%s\\1" indent in
        (style, indent ^ str) :: (List.map (
          fun (style, s) ->
            style,
            Str.global_replace nonterm_newline nonterm_newline_plus_indent s
          ) lines
        )
    ) infos in
    lines @ List.concat (
      List.map (f ~strip_root ~root stdin_file main_file ("  " ^ indent)) kids
    )
  in fun ~strip_root ~root stdin_file main_file tree ->
    List.concat (f ~strip_root ~root stdin_file main_file "  " tree)

let loc_of_error (err: error) =
  let { messages; op; _ } = err in
  let messages = prepend_op_reason messages op in
  match messages with
  | message :: _ ->
      let loc, _ = to_pp message in
      loc
  | _ -> Loc.none

let infos_of_error { messages; _ } =
  let rev_infos = List.fold_left (
    fun acc -> function
    | BlameM (loc, str) -> (loc, [str]) :: acc
    | CommentM str ->
      match acc with
      | (loc, strs) :: tail -> (loc, str :: strs) :: tail
      | [] -> [Loc.none, [str]] (* shouldn't start with a comment *)
    ) [] messages in
  List.rev (List.map (fun (loc, strs) -> loc, List.rev strs) rev_infos)

let extra_of_error { extra; _ } = extra

let file_of_error err =
  let loc = loc_of_error err in
  file_of_source loc.Loc.source

let print_error_header ~strip_root ~root message =
  let loc, _ = to_pp message in
  let filename = file_of_source loc.Loc.source in
  let relfilename = match filename with
  | Some fn -> relative_path ~strip_root ~root fn
  | None -> "[No file]" in
  [
    file_location_style (Printf.sprintf "%s:%d" relfilename Loc.(loc.start.line));
    default_style "\n"
  ]

let append_comment blame comment =
  match blame with
  | BlameM(loc, s) ->
    (match comment with
    | "Error:" -> BlameM(loc, s) (* Almost everywhere we have "Error:" that hurts readability *)
    | comment ->
      let combined_comment = if String.length s > 0
        then s ^ ". " ^ comment
        else comment
      in
      BlameM(loc, combined_comment))
  | CommentM(_) -> failwith "should not be comment"

let maybe_combine_message_text messages message =
  match message with
    | BlameM (_, _) -> message :: messages
    | CommentM s ->
      match messages with
      | x :: xs -> append_comment x s :: xs
      | _ -> failwith "can't append comment to nonexistent blame"

let merge_comments_into_blames messages =
  List.fold_left maybe_combine_message_text [] messages |> List.rev

let remove_newlines (color, text) =
  (color, Str.global_replace (Str.regexp "\n") "\\n" text)

let get_pretty_printed_error_new ~stdin_file:stdin_file ~strip_root ~one_line ~root
  (error : error) =
  let { kind; messages; op; trace; extra } = error in
  let messages = prepend_kind_message messages kind in
  let messages = prepend_op_reason messages op in
  let messages = append_trace_reasons messages trace in
  let messages = merge_comments_into_blames messages in
  let header = print_error_header ~strip_root ~root (List.hd messages) in
  let main_file = match file_of_error error with
    | Some filename -> filename
    | None -> "[No file]" in
  let formatted_messages = List.concat (List.map (
    print_message_nice ~strip_root ~root stdin_file main_file
  ) messages) in
  let formatted_extra = List.concat (List.map (
    print_extra_info ~strip_root ~root stdin_file main_file
  ) extra) in
  let to_print = header @ formatted_messages @ formatted_extra in
  let to_print = if one_line then List.map remove_newlines to_print
    else to_print in
  (to_print @ [default_style "\n"])

let print_error_color_new ~stdin_file:stdin_file ~strip_root ~one_line ~color ~root (error : error) =
  let to_print =
    get_pretty_printed_error_new ~stdin_file ~strip_root ~one_line ~root error in
  Tty.cprint ~color_mode:color to_print

(* TODO: deprecate this in favor of Reason_js.json_of_loc *)
let deprecated_json_props_of_loc loc = Loc.(
  let file = match loc.source with
  | Some x -> Hh_json.JSON_String (string_of_filename x)
  | None -> Hh_json.JSON_String "" (* TODO: return Hh_json.JSON_Null *)
  in
  [ "path", file;
    "line", Hh_json.int_ loc.start.line;
    "endline", Hh_json.int_ loc._end.line;
    "start", Hh_json.int_ (loc.start.column + 1);
    "end", Hh_json.int_ loc._end.column ]
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
  fun {messages = ml1; op = op1; _} {messages = ml2; op = op2; _} ->
    let ml1 = match op1 with Some op -> op :: ml1 | None -> ml1 in
    let ml2 = match op2 with Some op -> op :: ml2 | None -> ml2 in
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

  let check_loc ((_result, { suppressions; unused; }) as acc) loc =
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
  let check (err: error) suppressions =
    let {messages; op; _} = err in
    (* We also check the op message *)
    let messages = match op with
    | None -> messages
    | Some op -> op::messages in
    check_error_messages (false, suppressions) messages

  (* Get's the locations of the suppression comments that are yet unused *)
  let unused { unused; _; } = SpanMap.values unused

  let cardinal { suppressions; unused } =
    SpanMap.cardinal suppressions + SpanMap.cardinal unused
end

let parse_error_to_flow_error (loc, err) = {
  kind = ParseError;
  messages = [BlameM (loc, Parse_error.PP.error err)];
  op = None;
  trace = [];
  extra = []
}

let to_list errors = ErrorSet.elements errors

(******* Error output functionality working on Hack's error *******)

let unwrap_message = function
  | BlameM (loc, str) when loc <> Loc.none -> str, Some loc
  | BlameM (_, str) | CommentM str -> str, None

let json_of_message_props message =
  let open Hh_json in
  let desc, loc = unwrap_message message in
  let type_ = match message with
  | BlameM _ -> "Blame"
  | CommentM _ -> "Comment" in
  ("descr", JSON_String desc) ::
  ("type", JSON_String type_) ::
  match loc with
  | None -> deprecated_json_props_of_loc Loc.none
  | Some loc ->
    ("loc", Reason_js.json_of_loc loc) ::
    deprecated_json_props_of_loc loc

let json_of_message message =
  Hh_json.JSON_Object (json_of_message_props message)

let json_of_message_with_context ~stdin_file message =
  let open Hh_json in
  let _, loc = unwrap_message message in
  let code_line = match loc with
  | None -> None
  | Some loc ->
      let open Loc in
      let filename = file_of_source loc.source in
      let line = loc.start.line - 1 in
      read_line_in_file line filename stdin_file in
  let context = ("context", match code_line with
  | None -> JSON_Null
  | Some context -> JSON_String context) in
  Hh_json.JSON_Object (context :: (json_of_message_props message))

let json_of_infos ~json_of_message infos =
  let open Hh_json in
  JSON_Array (List.map json_of_message (infos_to_messages infos))

let rec json_of_info_tree ~json_of_message tree =
  let open Hh_json in
  let infos, kids = match tree with
    | InfoLeaf infos -> infos, None
    | InfoNode (infos, kids) -> infos, Some kids
  in
  JSON_Object (
    ("message", json_of_infos ~json_of_message infos) ::
    match kids with
    | None -> []
    | Some kids ->
      ["children", JSON_Array (List.map (json_of_info_tree ~json_of_message) kids)]
  )

(* adapted from Errors.to_json to output multi-line errors properly *)
let json_of_error_props ~json_of_message { kind; messages; op; trace; extra } =
  let open Hh_json in
  let kind_str, severity_str = match kind with
    | ParseError -> "parse", "error"
    | InferError -> "infer", "error"
    | InferWarning -> "infer", "warning"
    | InternalError -> "internal", "error"
  in
  let props = [
    "kind", JSON_String kind_str;
    "level", JSON_String severity_str;
    "message", JSON_Array (List.map json_of_message messages);
  ] in
  (* add trace if present *)
  let props = match trace with [] -> props | _ ->
    ("trace", JSON_Array (List.map json_of_message trace)) :: props
  in
  (* add op if present *)
  let props = match op with None -> props | Some op ->
    let op_loc, op_desc = to_pp op in
    ("operation", json_of_message (BlameM (op_loc, op_desc))):: props
  in
  (* add extra if present *)
  if extra = []
  then props
  else ("extra", JSON_Array (List.map (json_of_info_tree ~json_of_message) extra)) :: props

let json_of_error error =
  Hh_json.JSON_Object (json_of_error_props ~json_of_message error)

let json_of_error_with_context ~stdin_file error =
  let json_of_message = json_of_message_with_context ~stdin_file in
  Hh_json.JSON_Object (json_of_error_props ~json_of_message error)

let json_of_errors errors =
  Hh_json.JSON_Array (List.map json_of_error errors)

let json_of_errors_with_context ~stdin_file errors =
  Hh_json.JSON_Array (List.map (json_of_error_with_context ~stdin_file) errors)

let print_error_json ?(stdin_file=None) oc el =
  let open Hh_json in
  let res = JSON_Object [
    "flowVersion", JSON_String FlowConfig.version;
    "errors", json_of_errors_with_context ~stdin_file el;
    "passed", JSON_Bool (el = []);
  ] in
  output_string oc (json_to_string res);
  flush oc

(* for vim and emacs plugins *)
let string_of_loc_deprecated loc = Loc.(
  match loc.source with
    | None
    | Some Builtins -> ""
    | Some LibFile file
    | Some SourceFile file
    | Some JsonFile file ->
      let line = loc.start.line in
      let start = loc.start.column + 1 in
      let end_ = loc._end.column in
      if line <= 0 then
        Utils_js.spf "File \"%s\", line 0" file
      else if line = loc._end.line && start - end_ = 1 then
        Utils_js.spf "File \"%s\", line %d, character %d" file line start
      else
        Utils_js.spf "File \"%s\", line %d, characters %d-%d" file line start end_
)

let print_error_deprecated =
  let endline s = if s = "" then "" else s ^ "\n" in
  let to_pp_string message =
    let loc, msg = to_pp message in
    let loc_str = string_of_loc_deprecated loc in
    Printf.sprintf "%s%s" (endline loc_str) (endline msg)
  in
  let to_string (error : error) : string =
    let {messages; trace; _} = error in
    let messages = append_trace_reasons messages trace in
    let buf = Buffer.create 50 in
    (match messages with
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
    let sl = ListUtils.uniq (List.sort String.compare sl) in
    List.iter begin fun s ->
      output_string oc s;
      output_string oc "\n";
    end sl;
    flush oc

(* Human readable output *)
let print_error_summary ~flags ?(stdin_file=None) ~strip_root ~root errors =
  let error_or_errors n = if n != 1 then "errors" else "error" in
  let truncate = not (flags.Options.show_all_errors) in
  let one_line = flags.Options.one_line in
  let color = flags.Options.color in
  let print_error_color = if flags.Options.old_output_format
    then print_error_color_old
    else print_error_color_new ~stdin_file ~strip_root ~root
  in
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
