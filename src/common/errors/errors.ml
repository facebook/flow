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
  | DuplicateProviderError

let string_of_kind = function
| ParseError -> "ParseError"
| InferError -> "InferError"
| InferWarning -> "InferWarning"
| InternalError -> "InternalError"
| DuplicateProviderError -> "DuplicateProviderError"

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

let is_duplicate_provider_error error =
  error.kind = DuplicateProviderError

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

let prepend_op_reason messages = function
  | Some message -> message :: (message_of_string "Error:") :: messages
  | None -> messages

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ ->
    message_list @ (BlameM (Loc.none, "Trace:") :: trace_reasons)


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

let relative_path ~strip_root filename =
  if is_short_lib filename || Filename.is_relative filename
  then filename
  else match strip_root with
  | Some root -> Files.relative_path (Path.to_string root) filename
  | None ->
    let relname = Files.relative_path (Sys.getcwd ()) filename in
    if String.length relname < String.length filename
      then relname
      else filename

let relative_lib_path ~strip_root filename =
  let sep = Filename.dir_sep in
  match strip_root with
  | Some root ->
    let root_str = Printf.sprintf "%s%s" (Path.to_string root) sep in
    if String_utils.string_starts_with filename root_str then
      relative_path ~strip_root filename
    else
      Printf.sprintf "<BUILTINS>%s%s" sep (Filename.basename filename)
  | None -> relative_path ~strip_root filename

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
let get_lines ~start ~len content =
  let rec loop ~start ~len ~acc ~pos content =
    if len = 0
    then List.rev acc
    else begin
      if pos > String.length content then
        raise (Invalid_argument "not enough lines");
      let next_newline =
        try String.index_from content pos '\n'
        with Not_found -> String.length content
      in
      let continue =
        if start < 0 then
          raise (Invalid_argument "can't choose negative line")
        else if start = 0 then
          let acc = (String.sub content pos (next_newline - pos))::acc in
          loop ~start ~len:(len - 1) ~acc
        else
          loop ~start:(start - 1) ~len ~acc
        in

        continue ~pos:(next_newline + 1) content
    end
  in
  loop ~start ~len ~acc:[] ~pos:0 content

let read_lines_in_file loc filename stdin_file =
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
          if Filename.is_relative filename then failwith
            (Utils_js.spf "Expected absolute location, got %s" filename);
          Sys_utils.cat filename
        in
        try
          let open Loc in
          let lines = get_lines
            ~start:(loc.start.line - 1)
            ~len:(loc._end.line - loc.start.line + 1)
            content in
          match lines with
          | [] -> None
          | first::rest -> Some (first, rest)
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
    | Some Loc.JsonFile filename
    | Some Loc.ResourceFile filename ->
        Some filename
    | Some Loc.Builtins -> None
    | None -> None

let loc_of_error (err: error) =
  let { messages; op; _ } = err in
  let messages = prepend_op_reason messages op in
  match messages with
  | message :: _ ->
      let loc, _ = to_pp message in
      loc
  | _ -> Loc.none

let locs_of_error =
  let locs_of_info_list = List.fold_left (fun acc (loc, _) -> loc::acc)

  in let rec locs_of_info_tree acc = function
  | InfoLeaf infos -> locs_of_info_list acc infos
  | InfoNode (infos, branches) -> locs_of_extra (locs_of_info_list acc infos) branches

  and locs_of_extra acc tree = List.fold_left locs_of_info_tree acc tree

  in fun (err: error) ->
    let { messages; op; extra; _ } = err in
    let messages = prepend_op_reason messages op in
    let extra_locs = locs_of_extra [] extra in
    List.fold_left (fun acc message ->
      let loc, _ = to_pp message in
      loc::acc
    ) extra_locs messages

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


(* TODO: deprecate this in favor of Reason.json_of_loc *)
let deprecated_json_props_of_loc ~strip_root loc = Loc.(
  let file = match loc.source with
  | Some x -> Hh_json.JSON_String (Reason.string_of_source ~strip_root x)
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
  let kind_cmp =
    (* show internal errors first, then duplicate provider errors, then parse
       errors. then both infer warnings and errors at the same priority. *)
    let order_of_kind = function
    | InternalError -> 1
    | DuplicateProviderError -> 2
    | ParseError -> 3
    | InferError -> 4
    | InferWarning -> 4
    in
    fun k1 k2 -> (order_of_kind k1) - (order_of_kind k2)
  in
  let compare_message msg1 msg2 =
    match msg1, msg2 with
    | CommentM _, BlameM _ -> -1
    | BlameM _, CommentM _ -> 1
    | CommentM m1, CommentM m2 -> String.compare m1 m2
    | BlameM (loc1, m1), BlameM (loc2, m2) ->
        let k = Loc.compare loc1 loc2 in
        if k = 0 then String.compare m1 m2 else k
  in
  let rec compare_lists f list1 list2 =
    match list1, list2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | hd1::tl1, hd2::tl2 ->
        let k = f hd1 hd2 in
        if k = 0 then compare_lists f tl1 tl2 else k
  in
  let compare_info (loc1, msgs1) (loc2, msgs2) =
    let k = Loc.compare loc1 loc2 in
    if k = 0 then compare_lists String.compare msgs1 msgs2 else k
  in
  let rec compare_info_tree tree1 tree2 =
    match tree1, tree2 with
    | InfoLeaf k1, InfoLeaf k2 -> compare_lists compare_info k1 k2
    | InfoLeaf k1, InfoNode (k2, _) ->
        let k = compare_lists compare_info k1 k2 in
        if k = 0 then -1 else k
    | InfoNode (k1, _), InfoLeaf k2 ->
        let k = compare_lists compare_info k1 k2 in
        if k = 0 then 1 else k
    | InfoNode (k1, rest1), InfoNode (k2, rest2) ->
        let k = compare_lists compare_info k1 k2 in
        if k = 0 then compare_lists compare_info_tree rest1 rest2 else k
  in
  fun err1 err2 ->
    let {
      kind = k1;
      messages = ml1;
      op = op1;
      extra = extra1;
      trace = _;
    } = err1 in
    let {
      kind = k2;
      messages = ml2;
      op = op2;
      extra = extra2;
      trace = _;
    } = err2 in
    let loc1, loc2 = loc_of_error err1, loc_of_error err2 in
    let k = Loc.compare loc1 loc2 in
    if k = 0 then
      let k = kind_cmp k1 k2 in
      if k = 0 then
        let ml1 = match op1 with Some op -> op :: ml1 | None -> ml1 in
        let ml2 = match op2 with Some op -> op :: ml2 | None -> ml2 in
        let k = compare_lists compare_message ml1 ml2 in
        if k = 0 then compare_lists compare_info_tree extra1 extra2
        else k
      else k
    else k

module Error = struct
  type t = error
  let compare = compare
end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet = Set.Make(Error)

(* Human readable output *)
module Cli_output = struct
  let print_file_at_location ~strip_root stdin_file main_file loc s = Loc.(
    let l0 = loc.start.line in
    let l1 = loc._end.line in
    let c0 = loc.start.column in
    let c1 = loc._end.column in
    let filename = file_of_source loc.source in

    let see_another_file ~is_lib filename =
      if filename = main_file
      then [(default_style "")]
      else
        let prefix = Printf.sprintf ". See%s: "
          (if is_lib then " lib" else "") in
        let filename = if is_lib
          then relative_lib_path ~strip_root filename
          else relative_path ~strip_root filename
        in
        [
          comment_style prefix;
          comment_file_style (Printf.sprintf "%s:%d" filename l0)
        ]
    in

    let code_line = read_lines_in_file loc filename stdin_file in

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
        | None, Some Loc.JsonFile filename
        | None, Some Loc.ResourceFile filename -> filename, false
        | None, Some Loc.Builtins
        | None, None ->
          failwith "Should only have lib and source files at this point" in
        [comment_style s] @
        (see_another_file ~is_lib original_filename) @
        [default_style "\n"];
    | Some code_lines, Some filename ->
        let is_lib = match loc.source with
        | Some Loc.LibFile _ -> true
        | _ -> false in
        begin match code_lines with
        | code_line, [] ->
          (* Here we have a single line of context *)
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
        | code_lines ->
          (* Here we have multiple lines of context *)

          (* The most lines of context that we'll show before abridging *)
          let max_lines = 5 in

          (* Don't abridge if we could just show all the lines *)
          let abridged = l1 - l0 + 1 > max_lines in

          (* Highlight the context *)
          let highlighted_lines = code_lines
          |> Nel.to_list
          |> List.fold_left (fun (line_num, acc) line ->
              if not abridged || line_num - l0 < max_lines - 2 || line_num = l1
              then
                let line_number_text =
                  line_number_style (Utils_js.spf "\n%3d: " line_num) in
                let highlighted_line =
                  (* First line *)
                  if line_num = l0
                  then highlight_error_in_line line c0 (String.length line)
                  (* Last line *)
                  else if line_num = l1
                  then highlight_error_in_line line 0 c1
                  (* middle lines *)
                  else [error_fragment_style line] in
                line_num + 1, (line_number_text :: highlighted_line)::acc
              else if line_num - l0 = max_lines - 1
              then line_num + 1, [line_number_style "\n...:"]::acc
              else line_num + 1, acc
            ) (l0, [])
          |> snd
          |> List.rev
          |> List.flatten in

          let first_line = code_lines |> Nel.hd in
          let last_line = code_lines |> Nel.rev |> Nel.hd in

          (* Don't underline the whitespace at the beginning of the last line *)
          let underline_prefix =
            if Str.string_match (Str.regexp "^\\([\t ]*\\).*") last_line 0
            then Str.matched_group 1 last_line
            else "" in

          let overline_size = max 1 (String.length first_line - c0) in
          let underline_size = max 1 (c1 - String.length underline_prefix) in

          let line len = if len > 0 then String.make len '-' else "" in
          let overline = "v" ^ (line (overline_size - 1))  in
          let underline = (line (underline_size - 1)) ^ "^" in

          let overline_padding =
            let line_num =
              String.make (String.length (Printf.sprintf "%3d: " l0)) ' ' in
            let spaces =
              if String.length first_line <= c0 then ""
              else
                let prefix = String.sub first_line 0 c0 in
                Str.global_replace (Str.regexp "[^\t ]") " " prefix
            in
            line_num ^ spaces
          in

          let underlineline_padding =
            String.make (String.length (Printf.sprintf "%3d: " l1)) ' ' in

          let comment = Printf.sprintf
            "\n%s%s%s %s"
            underlineline_padding
            underline_prefix
            underline
            s in

          [comment_style (Printf.sprintf "%s%s" overline_padding overline)] @
          highlighted_lines @
          [comment_style comment] @
          (see_another_file ~is_lib filename) @
          [default_style "\n"]
        end
  )

  let print_message_nice ~strip_root stdin_file main_file message =
    let loc, s = to_pp message in
    print_file_at_location ~strip_root stdin_file main_file loc s

  let print_extra_info =
    let nonterm_newline = Str.regexp "\n\\(.\\)" in
    let rec f ~strip_root stdin_file main_file indent tree =
      let infos, kids = match tree with
        | InfoLeaf infos -> infos, []
        | InfoNode (infos, kids) -> infos, kids
      in
      let lines = List.map (fun (loc, strs) ->
        let msg = String.concat ". " strs in
        let lines =
          print_file_at_location ~strip_root stdin_file main_file loc msg in
        match lines with
        | [] -> []
        | (style, str) :: lines ->
          let nonterm_newline_plus_indent = Utils_js.spf "\n%s\\1" indent in
          (style, indent ^ str) :: (List.map (
            fun (style, s) ->
              style,
              Str.global_replace nonterm_newline nonterm_newline_plus_indent s
            ) lines
          )
      ) infos in
      lines @ List.concat (
        List.map (f ~strip_root stdin_file main_file ("  " ^ indent)) kids
      )
    in fun ~strip_root stdin_file main_file tree ->
      List.concat (f ~strip_root stdin_file main_file "  " tree)

  let print_error_header ~strip_root ~kind message =
    let loc, _ = to_pp message in
    let prefix, relfilename = match loc.Loc.source with
      | Some Loc.LibFile filename ->
        let header = match kind with
        | ParseError -> "Library parse error:"
        | InferError -> "Library type error:"
        (* TODO: we don't publicly distinguish warnings vs errors right now *)
        | InferWarning -> "Library type error:"
        | InternalError -> internal_error_prefix
        (* TODO: is this possible? What happens when there are two `declare
           module`s with the same name? *)
        | DuplicateProviderError -> "Library duplicate provider error:"
        in
        [comment_file_style (header^"\n")],
        relative_lib_path ~strip_root filename
      | Some Loc.SourceFile filename
      | Some Loc.JsonFile filename
      | Some Loc.ResourceFile filename ->
          [], relative_path ~strip_root filename
      | Some Loc.Builtins -> [], "[No file]"
      | None -> [], "[No file]"
    in
    let file_loc = Printf.sprintf "%s:%d" relfilename Loc.(loc.start.line) in
    prefix @ [
      file_location_style file_loc;
      default_style "\n"
    ]

  let append_comment blame comment =
    match blame with
    | BlameM(loc, s) ->
      (match comment with
      (* Almost everywhere we have "Error:" that hurts readability *)
      | "Error:" -> BlameM(loc, s)
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

  let get_pretty_printed_error ~stdin_file:stdin_file ~strip_root ~one_line
    (error : error) =
    let { kind; messages; op; trace; extra } = error in
    let messages = prepend_op_reason messages op in
    let messages = append_trace_reasons messages trace in
    let messages = merge_comments_into_blames messages in
    let header = print_error_header ~strip_root ~kind (List.hd messages) in
    let main_file = match file_of_error error with
      | Some filename -> filename
      | None -> "[No file]" in
    let formatted_messages = List.concat (List.map (
      print_message_nice ~strip_root stdin_file main_file
    ) messages) in
    let formatted_extra = List.concat (List.map (
      print_extra_info ~strip_root stdin_file main_file
    ) extra) in
    let to_print = header @ formatted_messages @ formatted_extra in
    let to_print = if one_line then List.map remove_newlines to_print
      else to_print in
    (to_print @ [default_style "\n"])

  let print_error_color
      ?(out_channel=stdout)
      ~stdin_file:stdin_file
      ~strip_root
      ~one_line
      ~color
      (error : error) =
    let to_print =
      get_pretty_printed_error ~stdin_file ~strip_root ~one_line error in
    Tty.cprint ~out_channel ~color_mode:color to_print

  let print_errors ~out_channel ~flags ?(stdin_file=None) ~strip_root errors =
    let error_or_errors n = if n != 1 then "errors" else "error" in
    let truncate = not (flags.Options.show_all_errors) in
    let one_line = flags.Options.one_line in
    let color = flags.Options.color in
    let print_error_if_not_truncated e curr =
      if not(truncate) || curr < 50 then print_error_color
        ~stdin_file ~strip_root ~one_line ~color ~out_channel e;

      curr + 1
    in
    let total = ErrorSet.fold print_error_if_not_truncated errors 0 in
    if total > 0 then print_newline ();
    if truncate && total > 50 then (
      Printf.fprintf
        out_channel
        "... %d more %s (only 50 out of %d errors displayed)\n"
        (total - 50) (error_or_errors (total - 50)) total;
      Printf.fprintf
        out_channel
        "To see all errors, re-run Flow with --show-all-errors\n";
      flush out_channel
    ) else
      Printf.fprintf out_channel "Found %d %s\n" total (error_or_errors total)
end

(* JSON output *)
module Json_output = struct
  let unwrap_message = function
  | BlameM (loc, str) when loc <> Loc.none -> str, Some loc
  | BlameM (_, str) | CommentM str -> str, None

  let json_of_message_props ~strip_root message =
    let open Hh_json in
    let desc, loc = unwrap_message message in
    let type_ = match message with
    | BlameM _ -> "Blame"
    | CommentM _ -> "Comment" in
    ("descr", JSON_String desc) ::
    ("type", JSON_String type_) ::
    match loc with
    | None -> deprecated_json_props_of_loc ~strip_root Loc.none
    | Some loc ->
      ("loc", Reason.json_of_loc ~strip_root loc) ::
      deprecated_json_props_of_loc ~strip_root loc

  let json_of_message ~strip_root message =
    Hh_json.JSON_Object (json_of_message_props ~strip_root message)

  let json_of_message_with_context ~strip_root ~stdin_file message =
    let open Hh_json in
    let _, loc = unwrap_message message in
    let code_line = match loc with
    | None -> None
    | Some loc ->
        let open Loc in
        let filename = file_of_source loc.source in
        (match read_lines_in_file loc filename stdin_file with
        | Some l -> Some (Nel.hd l)
        | None -> None)
    in
    let context = ("context", match code_line with
    | None -> JSON_Null
    | Some context -> JSON_String context) in
    Hh_json.JSON_Object (context :: (json_of_message_props ~strip_root message))

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
        let kids = List.map (json_of_info_tree ~json_of_message) kids in
        ["children", JSON_Array kids]
    )

  let json_of_error_props
    ~strip_root ~json_of_message
    ?(suppression_locs=Loc.LocSet.empty) { kind; messages; op; trace; extra } =
    let open Hh_json in
    let kind_str, severity_str = match kind with
      | ParseError -> "parse", "error"
      | InferError -> "infer", "error"
      | InferWarning -> "infer", "warning"
      | InternalError -> "internal", "error"
      | DuplicateProviderError -> "duplicate provider", "error"
    in
    let suppressions = suppression_locs
    |> Loc.LocSet.elements
    |> List.map (fun loc ->
        JSON_Object [ "loc", Reason.json_of_loc ~strip_root loc]
      ) in
    let props = [
      "kind", JSON_String kind_str;
      "level", JSON_String severity_str;
      "suppressions", JSON_Array suppressions;
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
    if extra = [] then props
    else
      let extra = List.map (json_of_info_tree ~json_of_message) extra in
      ("extra", JSON_Array extra) :: props

  let json_of_error ~strip_root error =
    let json_of_message = json_of_message ~strip_root in
    Hh_json.JSON_Object (json_of_error_props ~strip_root ~json_of_message  error)

  let json_of_error_with_context
    ~strip_root ~stdin_file (error, suppression_locs) =
    let json_of_message =
      json_of_message_with_context ~strip_root ~stdin_file in
    Hh_json.JSON_Object (json_of_error_props ~strip_root ~json_of_message ~suppression_locs error)

  let json_of_errors ~strip_root errors =
    let f = json_of_error ~strip_root in
    Hh_json.JSON_Array (List.map f (ErrorSet.elements errors))

  let json_of_errors_with_context
    ~strip_root ~stdin_file ~suppressed_errors errors =
    let errors = errors
      |> ErrorSet.elements
      |> List.map (fun errs -> errs, Loc.LocSet.empty) in
    let f = json_of_error_with_context ~strip_root ~stdin_file in
    Hh_json.JSON_Array (
      List.map f errors @
      List.map f suppressed_errors
    )

  let full_status_json_of_errors
    ~strip_root ~suppressed_errors ?(profiling=None) ?(stdin_file=None) errors =
    let open Hh_json in

    let props = [
      "flowVersion", JSON_String FlowConfig.version;
      "errors", json_of_errors_with_context
        ~strip_root ~stdin_file ~suppressed_errors errors;
      "passed", JSON_Bool (ErrorSet.is_empty errors);
    ] in
    let props = match profiling with
    | None -> props
    | Some profiling -> props @ Profiling_js.to_json_properties profiling in
    JSON_Object props

  let print_errors
      ~out_channel
      ~strip_root
      ~suppressed_errors
      ?(pretty=false) ?(profiling=None) ?(stdin_file=None)
      el =
    let open Hh_json in
    let res = full_status_json_of_errors
      ~strip_root ~profiling ~stdin_file ~suppressed_errors el in
    output_string out_channel (json_to_string ~pretty res);
    flush out_channel
end

(* for vim and emacs plugins *)
module Vim_emacs_output = struct
  let string_of_loc ~strip_root loc = Loc.(
    match loc.source with
    | None
    | Some Builtins -> ""
    | Some file ->
      let file = Reason.string_of_source ~strip_root file in
      let line = loc.start.line in
      let start = loc.start.column + 1 in
      let end_ = loc._end.column in
      let spf = Utils_js.spf in
      if line <= 0 then
        spf "File \"%s\", line 0" file
      else if line = loc._end.line && start - end_ = 1 then
        spf "File \"%s\", line %d, character %d" file line start
      else
        spf "File \"%s\", line %d, characters %d-%d" file line start end_
  )

  let print_errors =
    let endline s = if s = "" then "" else s ^ "\n" in
    let to_pp_string ~strip_root message =
      let loc, msg = to_pp message in
      let loc_str = string_of_loc ~strip_root loc in
      Printf.sprintf "%s%s" (endline loc_str) (endline msg)
    in
    let to_string ~strip_root (error : error) : string =
      let {messages; trace; _} = error in
      let messages = append_trace_reasons messages trace in
      let buf = Buffer.create 50 in
      (match messages with
      | [] -> assert false
      | message1 :: rest_of_error ->
          Buffer.add_string buf (to_pp_string ~strip_root message1);
          List.iter begin fun message ->
            Buffer.add_string buf (to_pp_string ~strip_root message)
          end rest_of_error
      );
      Buffer.contents buf
    in
    fun ~strip_root oc el ->
      let sl = ErrorSet.fold (fun err acc ->
        (to_string ~strip_root err)::acc
      ) el [] in
      let sl = ListUtils.uniq (List.sort String.compare sl) in
      List.iter begin fun s ->
        output_string oc s;
        output_string oc "\n";
      end sl;
      flush oc
end
