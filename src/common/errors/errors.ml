(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Severity

type error_kind =
  | ParseError
  | InferError
  | InferWarning
  | InternalError
  | DuplicateProviderError
  | RecursionLimitError
  | LintError of Lints.lint_kind

let string_of_kind = function
| ParseError -> "ParseError"
| InferError -> "InferError"
| InferWarning -> "InferWarning"
| InternalError -> "InternalError"
| DuplicateProviderError -> "DuplicateProviderError"
| RecursionLimitError -> "RecursionLimitError"
| LintError lint_kind -> "LintError" ^ "-" ^ Lints.string_of_kind lint_kind

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

type classic_error = {
  messages: message list;
  extra: info_tree list
}

module LocMap = Utils_js.LocMap

(* Types and utilities for friendly errors. *)
module Friendly = struct
  open Reason

  (* The error message format is designed to render well in all the environments
   * which a Flow error message appears. This includes:
   *
   * - The CLI.
   * - An IDE.
   * - A CI job.
   * - https://flow.org/try
   *
   * Some environments can render full blocks of code, like the CLI. Most
   * environments work best with a sentence of text. Like an IDE or a CI job.
   * So we optimize our error messages for a single, readable, sentence and
   * enhance in environments with greater capabilities.
   *
   * Messages can have some inline styles. Such as inline code blocks. We also
   * include references to location in code in our messages. This information
   * is enhanced in more capable environments instead of degrading in less
   * capable environments.
   *
   * The single message is split into two parts. A "root" and the rest of the
   * message. If we have a root then some environments (such as the CLI) will
   * merge errors with the same root cause into a single block.
   *)
  type 'a t' = {
    root: 'a error_root option;
    loc: Loc.t;
    message: 'a message;
  }

  and 'a error_root = {
    root_loc: Loc.t;
    root_message: 'a message;
  }

  and 'a message = 'a message_feature list

  and 'a message_feature =
    | Inline of message_inline list
    | Reference of message_inline list * 'a

  and message_inline =
    | Text of string
    | Code of string

  type t = Loc.t t'

  (* This function was introduced into the OCaml standard library in 4.04.0. Not
   * all of our tooling supports 4.04.0 yet, so we have a small
   * equivalent implementation here. *)
  let split_on_char c s =
    let rec loop i c s =
      try
        let k = String.index_from s i c in
        (String.sub s i (k - i))::(loop (k + 1) c s)
      with
        Not_found -> [String.sub s i (String.length s - i)]
    in
    loop 0 c s

  (* Converts a string into a message_inline list. e.g.:
   * "hello `world`" becomes: [Text "hello "; Code "world"].
   *
   * The inverse of string_of_message_inlines. *)
  let message_inlines_of_string s =
    List.mapi (fun i s ->
      if i mod 2 = 0
        then Text s
        else Code s
    ) (split_on_char '`' s)

  (* Converts a string into a message. e.g.:
   * "hello `world`" becomes a message where "world" is styled as inline code. *)
  let message_of_string s =
    [Inline (message_inlines_of_string s)]

  (* Converts a message_inline list into a string. e.g.:
   * [Text "hello "; Code "world"] becomes: "hello `world`"
   *
   * The inverse of message_inlines_of_string. *)
  let string_of_message_inlines =
    List.fold_left (fun message -> function
    | Text text -> message ^ text
    | Code text -> message ^ "`" ^ text ^ "`"
    ) ""

  (* Convenience functions for constructing friendly error messages. e.g.
   *
   *     [ref lower; text " is incompatible with "; ref upper]
   *
   * Is an example of an incompatibility error message. *)

  let text s = Inline [Text s]
  let code s = Inline [Code s]

  let ref ?(loc=true) r =
    let desc = desc_of_reason ~unwrap:(is_scalar_reason r) r in
    let desc = match desc with
    | RCode code -> [Code code]
    | _ -> message_inlines_of_string (string_of_desc desc)
    in
    if loc then (
      let loc = match annot_loc_of_reason r with
      | Some loc -> loc
      | None -> def_loc_of_reason r
      in
      if loc = Loc.none then
        Inline desc
      else
        Reference (desc, loc)
    ) else (
      Inline desc
    )

  (* Concatenates a list of messages with a conjunction according to the "rules"
   * of the English language. *)
  let conjunction_concat ?(conjunction="and") = function
  | [] -> []
  | x::[] -> x
  | x1::x2::[] -> x1 @ [Inline [Text (" " ^ conjunction ^ " ")]] @ x2
  | xs ->
    let rec loop = function
    | []
    | _::[] -> failwith "unreachable"
    | x1::x2::[] -> x1 @ [Inline [Text (", " ^ conjunction ^ " ")]] @ x2
    | x::xs -> x @ [Inline [Text ", "]] @ loop xs
    in
    loop xs

  (* Capitalizes the first letter in the message. Does not capitalize code or
   * text in references. *)
  let capitalize = function
  | [] -> []
  | (Inline ((Text s)::xs))::message -> (Inline ((Text (String.capitalize_ascii s))::xs))::message
  | message -> message

  (* The intermediate fold extract_references uses. Returns both a loc_to_id map
   * and an id_to_loc map. These maps are the inverses of one another. Also
   * returns a transformed message. *)
  let extract_references_intermediate next_id loc_to_id id_to_loc message =
    let (next_id, loc_to_id, id_to_loc, message) =
      List.fold_left (fun (next_id, loc_to_id, id_to_loc, message) message_feature ->
        match message_feature with
        | Inline inlines ->
          (next_id, loc_to_id, id_to_loc, Inline inlines :: message)
        | Reference (inlines, loc) ->
          (match LocMap.get loc loc_to_id with
          | Some id ->
            (next_id, loc_to_id, id_to_loc, Reference (inlines, id) :: message)
          | None ->
            let id = next_id in
            let loc_to_id = LocMap.add loc id loc_to_id in
            let id_to_loc = IMap.add id loc id_to_loc in
            (next_id + 1, loc_to_id, id_to_loc, Reference (inlines, id) :: message)
          )
      ) (next_id, loc_to_id, id_to_loc, []) message
    in
    (next_id, loc_to_id, id_to_loc, List.rev message)

  (* Extracts common location references from a message. In order, each location
   * will be replaced with an integer reference starting at 1. If some reference
   * has the same location as another then they will share an id. *)
  let extract_references : Loc.t message -> (Loc.t IMap.t * int message) =
    fun message ->
      let (_, _, id_to_loc, message) =
        extract_references_intermediate 1 LocMap.empty IMap.empty message in
      (id_to_loc, message)

  (* Converts our friendly error to a classic error message. *)
  let to_classic { root; loc; message } =
    (* Add our root to the message... *)
    let message = match root with
    | Some { root_message; _ } -> root_message @ [Inline [Text " "]] @ message
    | None -> message
    in
    (* Extract the references from the message. *)
    let (references, message) = extract_references message in
    (* Turn the message into a string. *)
    let message = List.fold_left (fun message -> function
    | Inline inlines ->
      message ^ string_of_message_inlines inlines
    | Reference (inlines, id) ->
      message ^ string_of_message_inlines inlines ^ " [" ^ string_of_int id ^ "]"
    ) "" message in
    {
      messages = [BlameM (loc, message)];
      extra = (
        if not (IMap.is_empty references) then
          (InfoLeaf [(Loc.none, ["References:"])])::
          (references
          |> IMap.bindings
          |> List.map (fun (id, loc) ->
            InfoLeaf [(loc, ["[" ^ string_of_int id ^ "]"])]))
        else
          []
      );
    }
end

type error' =
  | Classic of classic_error
  | Friendly of Friendly.t

type error = error_kind * message list * error'

let is_duplicate_provider_error (kind, _, _) = kind = DuplicateProviderError

let info_to_messages = function
| loc, [] -> [BlameM (loc, "")]
| loc, msg :: msgs ->
  BlameM (loc, msg) ::
  (msgs |> List.map (fun msg -> CommentM msg))

let infos_to_messages infos =
  List.concat (List.map info_to_messages infos)

let mk_error ?(kind=InferError) ?trace_infos ?(extra=[]) infos =
  let trace = Option.value_map trace_infos ~default:[] ~f:infos_to_messages in
  let infos = match kind, infos with
    | LintError lint_kind, (head_loc, head_str::head_tail)::tail ->
      let prefix = (Lints.string_of_kind lint_kind) ^ ": " in
      (head_loc, (prefix ^ head_str)::head_tail)::tail
    | _ -> infos
  in
  (kind, trace, Classic {
    messages = infos_to_messages infos;
    extra
  })

let mk_friendly_error
  ?(kind=InferError)
  ?trace_infos
  loc
  message
=
  let open Friendly in
  let trace = Option.value_map trace_infos ~default:[] ~f:infos_to_messages in
  let message = match kind with
  | LintError kind -> message @ [text " ("; code (Lints.string_of_kind kind); text ")"]
  | _ -> message
  in
  (kind, trace, Friendly {
    root = None;
    loc;
    message;
  })

let mk_friendly_error_with_root
  ?(kind=InferError)
  ?trace_infos
  (root_loc, root_message)
  (loc, message)
=
  let open Friendly in
  let trace = Option.value_map trace_infos ~default:[] ~f:infos_to_messages in
  let message = match kind with
  | LintError kind -> message @ [text " ("; code (Lints.string_of_kind kind); text ")"]
  | _ -> message
  in
  (kind, trace, Friendly {
    root = Some { root_loc; root_message };
    loc;
    message;
  })

(*******************************)

let to_pp = function
  | BlameM (loc, s) -> loc, s
  | CommentM s -> Loc.none, s

type stdin_file = (Path.t * string) option

let internal_error_prefix = "Internal error (see logs): "

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ ->
    message_list @ (BlameM (Loc.none, "Trace:") :: trace_reasons)


let file_location_style text = (Tty.Underline Tty.Default, text)
let default_style text = (Tty.Normal Tty.Default, text)
let source_fragment_style text = (Tty.Normal Tty.Default, text)
let error_heading_style text = (Tty.Bold Tty.Red, text)
let warning_heading_style text = (Tty.Bold Tty.Yellow, text)
let error_fragment_style text = (Tty.Normal Tty.Red, text)
let warning_fragment_style text = (Tty.Normal Tty.Yellow, text)
let line_number_style text = (Tty.Bold Tty.Default, text)
let comment_style text = (Tty.Bold Tty.Default, text)
let comment_file_style text = (Tty.BoldUnderline Tty.Default, text)
let dim_style text = (Tty.Dim Tty.Default, text)


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

let highlight_error_in_line ~severity_style line c0 c1 =
  let prefix = String.sub line 0 c0 in
  let fragment = String.sub line c0 (c1 - c0) in
  let suffix = String.sub line c1 ((String.length line) - c1) in
  [
    source_fragment_style prefix;
    severity_style fragment;
    source_fragment_style suffix;
  ]

(* 0-indexed
 *
 * If start + len adds to more lines then exist in the file, then the full len
 * will not be returned. *)
let get_lines ~start ~len content =
  let rec loop ~start ~len ~acc ~pos content =
    if len = 0 || pos > String.length content
    then List.rev acc
    else begin
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
    | Some File_key.LibFile filename ->
        let filename =
          if is_short_lib filename
          then begin
            let prefix_len = String.length lib_prefix in
            String.sub filename prefix_len (String.length filename - prefix_len)
          end else filename in
        Some filename
    | Some File_key.SourceFile filename
    | Some File_key.JsonFile filename
    | Some File_key.ResourceFile filename ->
        Some filename
    | Some File_key.Builtins -> None
    | None -> None

let loc_of_classic_error { messages; _ } =
  match messages with
  | message :: _ ->
      let loc, _ = to_pp message in
      loc
  | _ -> Loc.none

let loc_of_error ((_, _, err): error) =
  let open Friendly in
  match err with
  | Classic err -> loc_of_classic_error err
  | Friendly { loc; _ } -> loc

let loc_of_error_for_compare ((_, _, err): error) =
  let open Friendly in
  match err with
  | Classic err -> loc_of_classic_error err
  | Friendly { root=Some {root_loc; _}; _ } -> root_loc
  | Friendly { loc; _ } -> loc

let locs_of_error =
  let locs_of_info_list = List.fold_left (fun acc (loc, _) -> loc::acc)

  in let rec locs_of_info_tree acc = function
  | InfoLeaf infos -> locs_of_info_list acc infos
  | InfoNode (infos, branches) -> locs_of_extra (locs_of_info_list acc infos) branches

  and locs_of_extra acc tree = List.fold_left locs_of_info_tree acc tree

  in fun ((_, _, err): error) ->
    match err with
    | Classic { messages; extra; _ } ->
      let extra_locs = locs_of_extra [] extra in
      List.fold_left (fun acc message ->
        let loc, _ = to_pp message in
        loc::acc
      ) extra_locs messages
    | Friendly { Friendly.loc; root; message; } -> Friendly.(
      let message =
        (match root with Some { root_message; _ } -> root_message | None -> []) @
        message
      in
      let acc = loc ::
        (match root with Some { root_loc; _ } -> [root_loc] | None -> []) in
      (List.fold_left (fun acc feature ->
        match feature with
        | Inline _ -> acc
        | Reference (_, loc) -> loc :: acc
      ) acc message))

let infos_of_error (_, _, error) =
  match error with
  | Classic { messages; _ } ->
    let rev_infos = List.fold_left (
      fun acc -> function
      | BlameM (loc, str) -> (loc, [str]) :: acc
      | CommentM str ->
        match acc with
        | (loc, strs) :: tail -> (loc, str :: strs) :: tail
        | [] -> [Loc.none, [str]] (* shouldn't start with a comment *)
      ) [] messages in
    List.rev (List.map (fun (loc, strs) -> loc, List.rev strs) rev_infos)
  | Friendly _ -> []

let extra_of_error (_, _, error) =
  match error with
  | Classic { extra; _ } -> extra
  | Friendly _ -> []

let file_of_classic_error err =
  let loc = loc_of_classic_error err in
  file_of_source loc.Loc.source

let kind_of_error (kind, _, _) = kind


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
   positions match then first message, then second message, etc.

   for friendly errors check the location, docs slug, and then message.

   friendly errors go before classic errors when the kind is the same and the
   friendly error's location is the same as the classic error's first
   location. *)
let compare =
  let kind_cmp =
    (* show internal errors first, then duplicate provider errors, then parse
       errors, then recursion limit errors. then both infer warnings and errors
       at the same priority. then lint errors *)
    let order_of_kind = function
    | InternalError -> 1
    | DuplicateProviderError -> 2
    | ParseError -> 3
    | RecursionLimitError -> 4
    | InferError -> 5
    | InferWarning -> 5
    | LintError _ -> 6
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
  let compare_message_inline m1 m2 =
    let open Friendly in
    match m1, m2 with
    | Text s1, Text s2 -> String.compare s1 s2
    | Text _, Code _ -> 1
    | Code _, Text _ -> -1
    | Code s1, Code s2 -> String.compare s1 s2
  in
  let compare_message_feature m1 m2 =
    let open Friendly in
    match m1, m2 with
    | Inline m1, Inline m2 -> compare_lists compare_message_inline m1 m2
    | Inline _, Reference _ -> 1
    | Reference _, Inline _ -> -1
    | Reference (m1, loc1), Reference (m2, loc2) ->
      let k = compare_lists compare_message_inline m1 m2 in
      if k = 0 then Loc.compare loc1 loc2 else k
  in
  fun err1 err2 ->
    let open Friendly in
    let loc1, loc2 = loc_of_error_for_compare err1, loc_of_error_for_compare err2 in
    let (k1, _, err1), (k2, _, err2) = err1, err2 in
    let k = Loc.compare loc1 loc2 in
    if k = 0 then
      let k = kind_cmp k1 k2 in
      if k = 0 then match err1, err2 with
      | Friendly _, Classic _ -> 1
      | Classic _, Friendly _ -> -1
      | Classic { messages = ml1; extra = extra1 },
        Classic { messages = ml2; extra = extra2 } ->
        let k = compare_lists compare_message ml1 ml2 in
        if k = 0 then compare_lists compare_info_tree extra1 extra2
        else k
      | Friendly { root=Some _; _ }, Friendly { root=None; _ } -> -1
      | Friendly { root=None; _ }, Friendly { root=Some _; _ } -> 1
      | Friendly { root=Some { root_message=rm1; _ }; loc=loc1; message=m1 },
        Friendly { root=Some { root_message=rm2; _ }; loc=loc2; message=m2 } ->
        let k = compare_lists compare_message_feature rm1 rm2 in
        if k = 0 then
          let k = Loc.compare loc1 loc2 in
          if k = 0 then compare_lists compare_message_feature m1 m2
          else k
        else k
      | Friendly { root=None; message=m1; _ },
        Friendly { root=None; message=m2; _ } ->
        compare_lists compare_message_feature m1 m2
      else k
    else k

module Error = struct
  type t = error
  let compare = compare
end

(* we store errors in sets, currently, because distinct
   traces may share endpoints, and produce the same error *)
module ErrorSet = Set.Make(Error)

type error_group =
  (* Classic errors are never grouped. *)
  | ClassicSingleton of error_kind * message list * classic_error
  (* Friendly errors without a root are never grouped. When traces are enabled
   * all friendly errors will never group. *)
  | FriendlySingleton of error_kind * message list * Friendly.t
  (* Friendly errors that share a root are grouped together. *)
  | FriendlyGroup of {
      kind: error_kind;
      root: Loc.t Friendly.error_root;
      messages: (Loc.t * Loc.t Friendly.message) Nel.t;
      omitted: int;
    }

exception Interrupt_ErrorSet_fold of error_group list

(* Folds an ErrorSet into a grouped list. However, the group and all sub-groups
 * are in reverse order. *)
let collect_errors_into_groups max set =
  let open Friendly in
  try
    let (_, acc) = ErrorSet.fold (fun (kind, trace, error) (n, acc) ->
      let omit = Option.value_map max ~default:false ~f:(fun max -> max <= n) in
      let acc = match error with
      (* If the error is not a candidate for grouping then add a singleton to
       * our accumulator. *)
      | Classic error ->
        if omit then raise (Interrupt_ErrorSet_fold acc);
        ClassicSingleton (kind, trace, error) :: acc
      | Friendly error when trace <> [] ->
        if omit then raise (Interrupt_ErrorSet_fold acc);
        FriendlySingleton (kind, trace, error) :: acc
      | Friendly ({ root = None; _ } as error) ->
        if omit then raise (Interrupt_ErrorSet_fold acc);
        FriendlySingleton (kind, trace, error) :: acc
      (* Friendly errors with a root might need to be grouped. *)
      | Friendly { loc; message; root = Some root } ->
        (match acc with
        (* When the root location and message match the previous group, add our
         * friendly error to the group. We can do this by only looking at the last
         * group because ErrorSet is sorted so that friendly errors with the same
         * root loc/message are stored next to each other.
         *
         * If we are now omitting errors then increment the omitted count
         * instead of adding a message. *)
        | FriendlyGroup {kind = kind'; root = root'; messages = messages; omitted} :: acc' when (
            kind = kind' &&
            Loc.compare root.root_loc root'.root_loc = 0 &&
            root.root_message = root'.root_message
          ) ->
          FriendlyGroup {
            kind = kind';
            root = root';
            messages = if omit then messages else Nel.cons (loc, message) messages;
            omitted = if omit then omitted + 1 else omitted;
          } :: acc'
        (* If the roots did not match then we have a friendly singleton. *)
        | _ ->
          if omit then raise (Interrupt_ErrorSet_fold acc);
          FriendlyGroup {
            kind = kind;
            root = root;
            messages = Nel.one (loc, message);
            omitted = 0;
          } :: acc
        )
      in
      (n + 1, acc)
    ) set (0, []) in
    acc
  with
    Interrupt_ErrorSet_fold acc ->
      acc

(* Human readable output *)
module Cli_output = struct
  type error_flags = {
    color: Tty.color_mode;
    include_warnings: bool;
    (* This has to do with the exit code, which is not controlled by this module, but it's
    convenient to keep the flags about errors co-located *)
    max_warnings: int option;
    one_line: bool;
    show_all_errors: bool;
    unicode: bool;
    message_width: int;
  }

  let severity_fragment_style = function
  | Err -> error_fragment_style
  | Warn -> warning_fragment_style
  | Off ->
    Utils_js.assert_false "CLI output is only called with warnings and errors."

  let severity_heading_style = function
  | Err -> error_heading_style
  | Warn -> warning_heading_style
  | Off ->
    Utils_js.assert_false "CLI output is only called with warnings and errors."

  let print_file_at_location ~strip_root ~severity stdin_file main_file loc s = Loc.(
    let l0 = loc.start.line in
    let l1 = loc._end.line in
    let c0 = loc.start.column in
    let c1 = loc._end.column in
    let filename = file_of_source loc.source in
    let severity_style = severity_fragment_style severity in

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
        | Some filename, Some File_key.LibFile _
        | None, Some File_key.LibFile filename -> filename,true
        | Some filename, _
        | None, Some File_key.SourceFile filename
        | None, Some File_key.JsonFile filename
        | None, Some File_key.ResourceFile filename -> filename, false
        | None, Some File_key.Builtins
        | None, None ->
          failwith "Should only have lib and source files at this point" in
        [comment_style s] @
        (see_another_file ~is_lib original_filename) @
        [default_style "\n"];
    | Some code_lines, Some filename ->
        let is_lib = match loc.source with
        | Some File_key.LibFile _ -> true
        | _ -> false in
        begin match code_lines with
        | code_line, [] ->
          (* Here we have a single line of context *)
          let line_number_text = Printf.sprintf "%3d: " l0 in
          let highlighted_line = if (l1 == l0) && (String.length code_line) >= c1
            then highlight_error_in_line ~severity_style code_line c0 c1
            else [source_fragment_style code_line]
          in
          let padding =
            let line_num = String.make (String.length line_number_text) ' ' in
            let spaces =
              let prefix = if String.length code_line <= c0
                then code_line
                else String.sub code_line 0 c0
              in
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
                  then highlight_error_in_line ~severity_style line c0 (String.length line)
                  (* Last line *)
                  else if line_num = l1
                  then highlight_error_in_line ~severity_style line 0 c1
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

  let print_message_nice ~strip_root ~severity stdin_file main_file message =
    let loc, s = to_pp message in
    print_file_at_location ~strip_root ~severity stdin_file main_file loc s

  let print_extra_info =
    let nonterm_newline = Str.regexp "\n\\(.\\)" in
    let rec f ~strip_root ~severity stdin_file main_file indent tree =
      let infos, kids = match tree with
        | InfoLeaf infos -> infos, []
        | InfoNode (infos, kids) -> infos, kids
      in
      let lines = List.map (fun (loc, strs) ->
        let msg = String.concat ". " strs in
        let lines =
          print_file_at_location ~strip_root ~severity stdin_file main_file loc msg in
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
        List.map (f ~strip_root ~severity stdin_file main_file ("  " ^ indent)) kids
      )
    in fun ~strip_root ~severity stdin_file main_file tree ->
      List.concat (f ~strip_root ~severity stdin_file main_file "  " tree)

  let print_error_header ~strip_root ~kind ~severity message =
    let loc, _ = to_pp message in
    let prefix, relfilename = match loc.Loc.source with
      | Some File_key.LibFile filename ->
        let header = match kind with
        | ParseError -> "Library parse error:"
        | InferError -> "Library type error:"
        (* "InferWarning"s should still really be treated as errors. *)
        | InferWarning -> "Library type error:"
        | InternalError -> internal_error_prefix
        (* TODO: is this possible? What happens when there are two `declare
           module`s with the same name? *)
        | DuplicateProviderError -> "Library duplicate provider error:"
        | RecursionLimitError -> "Library recursion limit error:"
        | LintError lint_kind ->
          let lint_string = Lints.string_of_kind lint_kind in
          Printf.sprintf "Library lint %s (%s):"
            (output_string_of_severity severity) lint_string
        in
        [comment_file_style (header^"\n")],
        relative_lib_path ~strip_root filename
      | Some File_key.SourceFile filename
      | Some File_key.JsonFile filename
      | Some File_key.ResourceFile filename ->
        let heading_style = severity_heading_style severity in
        let severity_str = severity
          |> output_string_of_severity
          |> String.capitalize_ascii
        in
        [heading_style (severity_str ^ ":"); default_style " "], relative_path ~strip_root filename
      | Some File_key.Builtins -> [], "[No file]"
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

  let get_pretty_printed_classic_error
    ~stdin_file
    ~strip_root
    ~severity
    kind trace error
  =
    let { messages; extra } = error in
    let messages = append_trace_reasons messages trace in
    let messages = merge_comments_into_blames messages in
    let header = print_error_header ~strip_root ~kind ~severity (List.hd messages) in
    let main_file = match file_of_classic_error error with
      | Some filename -> filename
      | None -> "[No file]" in
    let formatted_messages = List.concat (List.map (
      print_message_nice ~strip_root ~severity stdin_file main_file
    ) messages) in
    let formatted_extra = List.concat (List.map (
      print_extra_info ~strip_root ~severity stdin_file main_file
    ) extra) in
    header @ formatted_messages @ formatted_extra @ [default_style "\n"]

  (* ==========================
   * Full Terminal Width Header
   * ==========================
   *
   * The header will always be the length of flags.message_width which in turn
   * is the lesser of the terminal length and 120 characters. *)
  let print_header_friendly ~strip_root ~flags ~severity loc =
    let severity_style = severity_fragment_style severity in
    let severity_name = match severity with
    | Err -> "Error"
    | Warn -> "Warning"
    | Off -> failwith "unreachable"
    in
    let horizontal_line_length =
      flags.message_width - (String.length severity_name + 1)
    in
    let filename =
      let open Loc in
      let open File_key in
      let { source; start = { line; column; _ }; _ } = loc in
      let pos = ":" ^ string_of_int line ^ ":" ^ string_of_int (column + 1) in
      match source with
      | Some (LibFile filename)
        -> relative_lib_path ~strip_root filename ^ pos
      | Some (SourceFile filename)
      | Some (JsonFile filename)
      | Some (ResourceFile filename)
        -> relative_path ~strip_root filename ^ pos
      | Some Builtins
      | None
        -> ""
    in
    (* If the filename is longer then the remaining horizontal line length we
     * put the filename on a new line. Otherwise the filename eats some of the
     * horizontal line space.
     *
     * We put two spaces of padding between the horizontal line and the
     * filename. This looks better in some fonts. *)
    let filename_with_padding_length = String.length filename + 1 in
    let filename_on_newline = filename_with_padding_length + 1 > horizontal_line_length in
    let horizontal_line_length =
      if filename_on_newline then
        horizontal_line_length
      else
        horizontal_line_length - filename_with_padding_length
    in
    (* The horizontal line which fills the space in our error message header
     * will be made out of [U+2508][1] characters when unicode is enabled. In
     * UTF-8 this character is encoded with three code points.
     *
     * [1]: https://duckduckgo.com/?q=U%2B2508
     *)
    let horizontal_line =
      if flags.unicode then
        String.init (horizontal_line_length * 3) (fun i ->
          match i mod 3 with
          | 0 -> '\xE2'
          | 1 -> '\x94'
          | 2 -> '\x88'
          | _ -> failwith "unreachable"
        )
      else
        String.make horizontal_line_length '-'
    in
    (* Construct the header by appending the constituent pieces. *)
    [
      severity_style (
        severity_name
          ^ " "
          ^ horizontal_line
          ^ (if filename_on_newline then "\n" else " ")
          ^ filename
      );
      default_style "\n";
    ]

  module FileKeyMap = MyMap.Make(File_key)

  type tag_kind =
    | Open
    | Close

  type tags = (Loc.position * int * tag_kind) list FileKeyMap.t

  (* See the definition of update_colors for why we need this cyclic type. *)
  type opened = Opened of opened IMap.t

  type 'a color =
    (* Rank based color. *)
    | Color of int
    (* Custom color passed into the layout_references algorithm. *)
    | CustomColor of 'a

  (* ==========================
   * Reference Layout Algorithm
   * ==========================
   *
   * In our error messages, we print a code frame and the code frame may have
   * any number of overlapping references. These overlapping references are
   * colored in such a way where the smallest location always has the
   * same color. For instance, consider the following source text diagram:
   *
   *     Source Text  >  abcdefghijklmnopqrstuvwxyz0123456789
   *     Coloration   >    XXOOOOXXX  XXXXOOOOOOO     OOOOO
   *     Reference 1  >    |-------|
   *     Reference 2  >      |--|
   *     Reference 3  >               |------|
   *     Reference 4  >                   |-----|
   *     Reference 5  >                               |---|
   *
   * Reference 1 completely contains reference 2. References 3 and 4 overlap.
   * Reference 5 is completely on its own.
   *
   * The "Coloration" row depicts how we would like to "paint" the source text
   * in our design for error messages. In the "Coloration" row: spaces represent
   * unpainted space, O's represent some first color, and X's represent some
   * second color.
   *
   * Notice how reference 2 and reference 5 share the same color. "O". This is
   * because, in our design, the references with the smallest contained area
   * always share the same color. The references which contain them have the
   * second color, "X".
   *
   * Notice how in references 3 and 4, one does not contain another. Instead the
   * two references overlap. This is not common in Flow's error messages.
   * However, we still need reasonable support for this case in our algorithm.
   * For implementation convenience, we choose to give the reference which comes
   * second the "O" color and the reference which comes first the "X" color.
   *
   * This example only demonstrates two colors. "O" and "X". However, in
   * principle there could be any number of overlapping references.
   *
   * NOTE: Our current algorithm is not that efficient. There is a lot of room
   * for optimization. For instance, a binary tree may be used instead of a list
   * for the variable "tags". Since the number of references in a single file is
   * almost always under 100, the implementation currently prefers legibility
   * and correctness to efficiency.
   *
   * ## Inputs and Outputs
   *
   * We take in a map of reference ids to reference locations.
   *
   * We return both:
   * - A map of reference ids to reference colors.
   * - A map of filenames to lists of open/close tags to be used in rendering
   *   source text. *)
  let layout_references ~(custom_colors:'a IMap.t) : Loc.t IMap.t -> ('a color IMap.t * tags) =
    let open Loc in
    let rec add_tags colors opened id start end' tags tags_acc =
      match tags with
      (* For an empty array, add both the open and close tags. Also add a color
       * of 0 for this id. *)
      | [] ->
        let color = Option.value_map (IMap.get id custom_colors)
          ~f:(fun custom -> CustomColor custom) ~default:(Color 0) in
        let colors = IMap.add id color colors in
        (colors, List.rev ((end', id, Close) :: (start, id, Open) :: tags_acc))
      (* Search for the correct place where start should appear in our list.
       *
       * Note that we may introduce an Open tag _before_ a Close tag at the same
       * position! This is intentional as it introduces overlapping. If we have
       * two tags that open/close at the same position we want them to be
       * different colors. Intersecting accomplishes this. *)
      | ((pos, tag_id, tag_kind) as tag) :: tags when pos_cmp pos start < 0 ->
        (* Keep track of the ids which are opening and closing so that we can
         * increment their colors if start is inside them.
         *
         * Also keep track of the ids which were opened at the time this tag was
         * opened. This allows us to move backwards through the containment tree
         * and update tag colors appropriately.
         *
         * See the definition of update_colors for more information. *)
        let opened = Opened (
          let Opened opened' = opened in
          match tag_kind with
          | Open -> IMap.add tag_id opened opened'
          | Close -> IMap.remove tag_id opened'
        ) in
        add_tags colors opened id start end' tags (tag :: tags_acc)
      (* We've found the correct place for start! If there are any tags which
       * are currently open then we need to increment their colors. *)
      | tags ->
        (* Add our closing tag. We will get from this operation the color which
         * we should add for our current reference. *)
        let (color, tags) = add_close_tag colors id end' tags 0 [] in
        (* Add a color for this id to the colors map. If our id exists in
         * custom_colors then we add a custom color. Otherwise we add a color
         * based on the current rank and update similar rank-based colors. *)
        let colors = match IMap.get id custom_colors with
        | Some custom ->
          IMap.add id (CustomColor custom) colors
        | None ->
          (* Increment the colors of all open references by the color of this tag.
           * It is a logic error if some open_id does not exist in colors. *)
          let colors = update_colors opened colors color in
          (* Add the color for this reference. *)
          IMap.add id (Color color) colors
        in
        (* Finish by adding an open tag. A corresponding closing tag will have
         * been added by add_close_tag. *)
        (colors, List.rev_append ((start, id, Open) :: tags_acc) tags)

    and add_close_tag colors id end' tags color_acc tags_acc =
      match tags with
      | [] ->
        (color_acc, List.rev ((end', id, Close) :: tags_acc))
      (* Search for the last place to add our end position.
       *
       * Note that we may introduce a Close tag _after_ an Open tag at the same
       * position! This is intentional as it introduces overlapping. If we have
       * two tags that open/close at the same position we want them to be
       * different colors. Intersecting accomplishes this. *)
      | ((pos, tag_id, tag_kind) as tag) :: tags when pos_cmp pos end' <= 0 ->
        (* If we run into an open tag then our color must be at least 1 greater
         * then the color of the opened tag. *)
        let color_acc = match tag_kind with
        | Close -> color_acc
        | Open ->
          (match IMap.get tag_id colors with
          | None -> max color_acc (0 + 1)
          | Some (Color tag_color) -> max color_acc (tag_color + 1)
          | Some (CustomColor _) -> color_acc
          )
        in
        add_close_tag colors id end' tags color_acc (tag :: tags_acc)
      (* When we find the location for our close tag, add it. *)
      | tags ->
        (color_acc, List.rev_append ((end', id, Close) :: tags_acc) tags)

    (* According to our design, the color of a location is one plus the largest
     * color opened inside of the location. See add_close_tag for the
     * implementation of this logic when we add a location that opens *before*
     * a location that already exists in tags.
     *
     * The logic for updating colors when a location opens *after* a location
     * that already exists in tags is more difficult. Not only must we try to
     * update the locations which are currently opened, but we must also
     * propagate updates to previously opened tags. Consider the uncommon case
     * our logic takes pain to support:
     *
     *     Reference 1  >  |-------|
     *     Reference 2  >       |--------|
     *     Reference 3  >             |--------|
     *
     * Let's say references 1 and 2 already exist. We are adding reference 3. At
     * this point, reference 1 will have a color of 1 and reference 2 will have
     * a color of 0.
     *
     * We add reference 3 which has a color of 0. Reference 3 opens inside of
     * reference 2 so we update reference 2's color to 0 + 1. Now reference 2
     * and reference 1 have a color of 1. This is incorrect. Reference 1 should
     * have a color of 2 at this point.
     *
     * Our `opened` type keeps track of the open references when another
     * reference opens. So we have access to reference 1 through reference 2.
     * Update reference 1's color to 1 + 1.
     *
     * We can short-circuit the recursive traversal of reference trees when we
     * can't update the color of most recently opened tags. *)
    and update_colors (Opened opened) colors color =
      IMap.fold (fun open_id opened colors ->
        let open_color = Option.value (IMap.get open_id colors) ~default:(Color 0) in
        match open_color with
        | CustomColor _ -> colors
        | Color open_color ->
          if open_color >= color + 1 then
            colors
          else
            let colors = IMap.add open_id (Color (color + 1)) colors in
            update_colors opened colors (color + 1)
      ) opened colors
    in
    fun references ->
      List.fold_left (fun (colors, file_tags) (id, loc) ->
        match loc.source with
        | None -> (colors, file_tags)
        | Some source ->
          let tags = Option.value (FileKeyMap.get source file_tags) ~default:[] in
          let (colors, tags) = add_tags colors (Opened IMap.empty) id loc.start loc._end tags [] in
          let file_tags = FileKeyMap.add source tags file_tags in
          (colors, file_tags)
      ) (IMap.empty, FileKeyMap.empty) (IMap.bindings references)

  (* To be used with the result of layout_references.
   *
   * There are some special values influenced by logic
   * in layout_friendly_error_group.
   *
   * - We manually set the color of primary references to CustomColor `Primary
   *   which should always be red.
   * - We manually set the color of root locations to CustomColor `Root which
   *   should always be the default terminal color.
   *)
  let get_tty_color_internal = function
  | CustomColor `Primary -> Tty.Red
  | CustomColor `Root -> Tty.Default
  | Color rank ->
    (match rank mod 3 with
    | 0 -> Tty.Cyan
    | 1 -> Tty.Yellow
    | 2 -> Tty.Green
    | _ -> failwith "unreachable"
    )

  let get_tty_color id colors =
    get_tty_color_internal (Option.value (IMap.get id colors) ~default:(Color 0))

  (* Gets the Tty color from a stack of ids. This function will ignore
   * CustomColor `Root if there are other colors on the stack. *)
  let rec get_tty_color_from_stack ids colors =
    match ids with
    | [] -> None
    | id :: ids ->
      (match Option.value (IMap.get id colors) ~default:(Color 0) with
      | CustomColor `Root ->
        Option.value_map (get_tty_color_from_stack ids colors)
          ~f:(fun x -> Some x) ~default:(Some Tty.Default)
      | color ->
        Some (get_tty_color_internal color)
      )

  let bullet_char ~flags =
    (* Use [U+2022][1] for the bullet character if unicode is enabled.
     *
     * [1]: http://graphemica.com/%E2%80%A2 *)
    if flags.unicode then "\xE2\x80\xA2" else "-"

  (* ==================
   * Error Message Text
   * ==================
   *
   * Text of an error message decorated with styles. We also accumulate all
   * the references in the message which will be rendered in another step.
   *
   * Most of the following code is responsible for splitting our message onto
   * multiple lines. This would make for a great interview question. *)
  let print_message_friendly =
    let open Friendly in
    (* Takes an input list of styled inline code parts of the following shape:
     *
     *     (bool * Tty.style * string) list
     *
     * The first boolean tells us whether or not we are allowed to break the
     * string inside into multiple lines.
     *
     * We return a data structure of:
     *
     *     (int * (Tty.style * string) list) list
     *
     * Each item in the list is a "word". Each "word" is a tuple. The first
     * element of the tuple is the length of the word. The second element of
     * the tuple is a list of styled strings which make up the word. A word
     * can have multiple different styles inside of it.
     *
     * We can put line breaks in between each "word". However, we cannot put
     * a line break inside the strings after we have split them into words.
     *
     * Note that a word may have spaces in it. The input strs includes a
     * boolean that determines whether or not a string is breakable even if it
     * has spaces. Not breaking even when we have spaces is useful for
     * rendering code. *)
    let split_into_words =
      let merge style str word_in_progress_acc =
        match word_in_progress_acc with
        (* If we do not have a word in progress then create a new word
         * from str. *)
        | None ->
          (String.length str, [(style, str)])
        (* If there is another word then we want to merge str with
         * that word. We can assume that there is no breakpoint between
         * the end of str and the beginning of the next word. *)
        | Some (len, word) ->
          (String.length str + len, (style, str) :: word)
      in
      let rec loop
        finished_words_acc
        word_in_progress_acc
        strs
      =
        match strs with
        | [] ->
          let finished_words_acc = (match word_in_progress_acc with
          | None -> finished_words_acc
          | Some word -> word :: finished_words_acc
          ) in
          List.rev_map (fun (n, words) -> (n, List.rev words)) finished_words_acc

        | (breakable, style, str) :: strs ->
          (* If our string is breakable then try to find the first space. Use
           * that space as the breakpoint. *)
          let bp = if breakable
            then try Some (String.index str ' ') with Not_found -> None
            else None
          in
          (match bp with
          (* If we have no breakpoint then we want to either create a new word
           * or combine our str with the first word in the result of
           * recursively calling split_into_words. *)
          | None ->
            loop
              finished_words_acc
              (Some (merge style str word_in_progress_acc))
              strs

          (* If we have a breakpoint then we need to split up our str and
           * create a new word with the left half and recurse with the
           * right half. *)
          | Some bp ->
            let left = String.sub str 0 bp in
            let right = String.sub str (bp + 1) ((String.length str) - (bp + 1)) in
            (* We need to recurse with the right half of our split str because
             * there may be more words we need to split out in it. bp is only
             * the first breakpoint in str. We can assume that our right half
             * is breakable since we would have no breakpoint if str was
             * not breakable. *)
            loop
              (merge style left word_in_progress_acc :: finished_words_acc)
              None
              ((true, style, right) :: strs)
          )
      in
      loop [] None
    in
    let is_style_underlined = function
    | Tty.Normal _ -> false
    | Tty.Bold _ -> false
    | Tty.Dim _ -> false
    | Tty.Underline _ -> true
    | Tty.BoldUnderline _ -> true
    | Tty.DimUnderline _ -> true
    | Tty.NormalWithBG _ -> false
    | Tty.BoldWithBG _ -> false
    in
    let is_first_underlined = function
    | [] -> false
    | (style, _) :: _ -> is_style_underlined style
    in
    let is_last_underlined words = words |> List.rev |> is_first_underlined in
    let with_len = function
    | None -> None
    | Some styles ->
      Some (List.fold_left (fun acc (_, s) -> acc + String.length s) 0 styles, styles)
    in
    (* Hard breaks a single Tty style list returned by split_into_words. We use
     * this when a word is, by itself, larger then our line length. Returns a:
     *
     *     (int * (Tty.style * string) list)
     *
     * Where int represents the length of the last line. *)
    let hard_break_styles =
      let rec loop acc pos ~line_length = function
      | [] -> (pos, List.rev acc)
      | (style, str) :: styles ->
        let bp = line_length - pos in
        let len = String.length str in
        if len > bp then
          let left = String.sub str 0 bp in
          let right = String.sub str bp (len - bp) in
          loop
            (default_style "\n" :: (style, left) :: acc)
            0
            ~line_length
            ((style, right) :: styles)
        else
          loop
            ((style, str) :: acc)
            (pos + len)
            ~line_length
            styles
      in
      loop [] 0
    in
    (* Concatenates a words data structure created by split_into_words into a:
     *
     *     (Tty.style * string) list
     *
     * Which can be rendered by the Tty module. This is where we will line
     * break depending on the length of our word and the position of the word
     * on the current line.
     *
     * TODO: Handle orphans gracefully. *)
    let concat_words_into_lines
      ~line_length
      ?indentation_first
      ?indentation
      words
    =
      let indentation = with_len indentation in
      let indentation_first = match indentation_first with
      | (Some _ as indentation_first) -> with_len indentation_first
      | None -> indentation
      in
      match words with
        (* No words means no string. *)
        | [] -> Option.value_map indentation_first ~default:[] ~f:snd
        (* If we have a single word we will use that as our initializer for
         * our fold on the rest of our words. *)
        | init :: words ->
          let init =
            let (init_len, init_word) = init in
            let init = match indentation_first with
            | None -> (init_len, [init_word])
            | Some (indentation_first_len, indentation_first) ->
              (indentation_first_len + init_len, [init_word; indentation_first])
            in
            ((fun () -> is_last_underlined init_word), init)
          in
          let (_, (_, acc)) = List.fold_left (fun (last_underlined, (pos, acc)) (len, word) ->
            (* If our position on the line plus one (for the space we would
             * insert) plus the length of our word will fit in our line length
             * then add the word to the current line separated from acc with a
             * space. Otherwise start a new line where word is the only text. *)
            if pos + 1 + len > line_length then
              let last_underlined () = is_last_underlined word in
              let (newline_len, newline) = match indentation with
              | None ->
                (0, [default_style "\n"])
              | Some (indentation_len, indentation) ->
                (indentation_len, default_style "\n" :: indentation)
              in
              if len <= line_length then
                (last_underlined, (len + newline_len, word :: newline :: acc))
              else
                let (len, word) = hard_break_styles ~line_length word in
                (last_underlined, (len + newline_len, word :: newline :: acc))
            else
              (* If both the end of the last word was underlined *and* the
               * beginning of the next word is underlined then we also want to
               * underline the space we insert. *)
              let should_underline = is_first_underlined word && last_underlined () in
              let space =
                let style =
                  if should_underline then
                    (Tty.Underline Tty.Default)
                  else
                    (Tty.Normal Tty.Default)
                in
                (style, " ")
              in
              let last_underlined () = is_last_underlined word in
              (last_underlined, (pos + 1 + len, (space :: word) :: acc))
          ) init words in
          List.concat (List.rev acc)
    in
    (* Create the tuple structure we pass into split_into_words. Code is not
     * breakable but Text is breakable. *)
    let print_message_inline ~flags ~reference = function
    | Code s when not (Tty.should_color flags.color) ->
      (false, Tty.Normal Tty.Default, "`" ^ s ^ "`")
    | Text s when reference -> (true, Tty.Underline Tty.Default, s)
    | Code s when reference -> (false, Tty.BoldUnderline Tty.Default, s)
    | Text s -> (true, Tty.Normal Tty.Default, s)
    | Code s -> (false, Tty.Bold Tty.Default, s)
    in
    (* Put it all together! *)
    fun ~flags ~colors ?(bullet=false) message ->
      let message = List.rev (List.fold_left (fun acc feature ->
        match feature with
        | Inline inlines ->
          List.rev_append
            (List.map (print_message_inline ~flags ~reference:false) inlines)
            acc
        | Reference (inlines, id) ->
          let message =
            List.map (print_message_inline ~flags ~reference:true) inlines @
            [
              (false, Tty.Normal Tty.Default, " ");
              (false, Tty.Dim Tty.Default, "[");
              (false, Tty.Normal (get_tty_color id colors), string_of_int id);
              (false, Tty.Dim Tty.Default, "]");
            ]
          in
          List.rev_append message acc
      ) [] message) in
      (* Use [U+2022][1] for the bullet character if unicode is enabled.
       *
       * [1]: http://graphemica.com/%E2%80%A2 *)
      let indentation_first =
        if bullet then
          Some [default_style (" " ^ bullet_char ~flags ^ " ")]
        else
          None
      in
      let message =
        concat_words_into_lines
          ~line_length:flags.message_width
          ?indentation_first
          ?indentation:(if bullet then Some [default_style "   "] else None)
          (split_into_words message)
      in
      message

  (* Shows 3 lines of context in both directions from the root location. *)
  let root_context_lines = 3

  (* Omit code that takes up more then 15 lines in either direction. For 30
   * lines in total.
   *
   * 30 lines is a lot of code. The reasoning behind this is we want to show as
   * much context as possible. *)
  let omit_after_lines = 15

  (* Merge locs within only 3 lines of one another. *)
  let merge_nearby_lines = 3

  (* =========================
   * Error Message Code Frames
   * =========================
   *
   * We render the root location for our friendly error message. Decorated with
   * the reference locations from the message. *)
  let print_code_frames_friendly
    ~stdin_file
    ~strip_root
    ~flags
    ~references
    ~colors
    ~tags
    root_loc
  =
    let open Loc in
    (* Get a list of all the locations we will want to display. We want to
     * display references and the root location with some extra lines
     * for context. *)
    let locs =
      (* Expand the root location with 3 lines of context in either direction.
       * However, don't expand before the first line or after the last line. If
       * we expand past the last line then read_lines_in_file will skip
       * those lines. *)
      let expanded_root_loc =
        let start_line = max 1 (root_loc.start.line - root_context_lines) in
        let end_line = root_loc._end.line + root_context_lines in
        { root_loc with
          start = { root_loc.start with line = start_line };
          _end = { root_loc._end with line = end_line };
        }
      in
      expanded_root_loc :: List.map snd (IMap.bindings references)
    in
    (* Group our locs by their file key.
     *
     * Also split large locs into two smaller locs.
     *
     * Also compute the largest line number. We need this to compute the
     * gutter width. *)
    let (max_line, locs) = List.fold_left (fun (max_line, acc) loc ->
      match loc.source with
      | None -> failwith "expected loc to have a source"
      | Some source ->
        (* If our loc is larger then some threshold determined by
         * omit_after_lines then split it into two locs. *)
        let new_locs =
          if ((loc._end.line - loc.start.line) + 1) <= (omit_after_lines * 2) then
            [loc]
          else
            let loc1 = { loc with
              _end = { loc.start with
                line = loc.start.line + (omit_after_lines - 1);
              };
            } in
            let loc2 = { loc with
              start = { loc._end with
                line = loc._end.line - (omit_after_lines - 1);
              };
            } in
            [loc1; loc2]
        in
        (* Add the new locs to our FileKeyMap. *)
        let locs = Option.value (FileKeyMap.get source acc) ~default:[] in
        (max max_line loc._end.line, FileKeyMap.add source (new_locs @ locs) acc)
    ) (0, FileKeyMap.empty) locs in
    (* Perform some organization operations on our locs. *)
    let locs = FileKeyMap.map (fun locs ->
      (* Sort all of the locations we want to display. Locations in the root
       * file should appear first. Sort in the reverse direction. Our next merge
       * step will flip the list back around. *)
      let locs = List.sort Loc.compare locs in
      (* Merge the locations we want to display. We start with the location with
       * the lowest line number. Our fold depends on this to merge correctly. *)
      let locs = List.fold_left (fun acc loc ->
        match acc with
        (* Init. *)
        | [] -> [loc]
        (* If the previous loc + 3 lines below intersects with the next loc then
         * we want to merge those locs into one code frame. *)
        | last_loc :: acc when (
            Loc.lines_intersect loc
              { last_loc with
                _end = { last_loc._end with
                  line = last_loc._end.line + (merge_nearby_lines + 1);
                };
              }
          ) ->
          let loc = {
            source = last_loc.source;
            start = if pos_cmp loc.start last_loc.start < 0 then loc.start else last_loc.start;
            _end = if pos_cmp loc._end last_loc._end > 0 then loc._end else last_loc._end;
          } in
          loc :: acc
        (* Otherwise, add the loc by itself. *)
        | acc -> loc :: acc
      ) [] locs in
      (* Return the reversed locs. *)
      List.rev locs
    ) locs in
    (* Organize all our references onto the line which we will find them on. We
     * do a second pass to sort these references and determine the
     * gutter width. *)
    let file_line_references = IMap.fold (fun id loc file_line_references ->
      if id < 0 then
        file_line_references
      else
        match loc.source with
        | None -> failwith "expected loc to have a source"
        | Some source ->
          let line_references =
            Option.value (FileKeyMap.get source file_line_references) ~default:IMap.empty in
          let references =
            Option.value (IMap.get loc.start.line line_references) ~default:[] in
          let references = (id, loc.start) :: references in
          let line_references = IMap.add loc.start.line references line_references in
          FileKeyMap.add source line_references file_line_references
    ) references FileKeyMap.empty in
    (* Create the styled text which we will put in the gutter for all of our
     * file line references.
     *
     * We use a ref for gutter_width since we want to map file_line_references
     * in place instead of using a fold which would re-create the map. *)
    let gutter_width = ref 5 in
    let file_line_references = FileKeyMap.map (IMap.map (fun references ->
      (* Reverse sort the references by their start position. We reverse sort
       * since we fold_left next. *)
      let references = List.sort (fun (_, a) (_, b) -> pos_cmp b a) references in
      (* Fold the list. Creating the width and the string we will
       * ultimately render. *)
      let (width, references) = List.fold_left (fun (width, acc) (id, _) ->
        let string_id = string_of_int id in
        let width = width + 2 + String.length string_id in
        let acc =
          (dim_style "[")
            :: (Tty.Normal (get_tty_color id colors), string_id)
            :: (dim_style "]")
            :: acc
        in
        (width, acc)
      ) (1, [default_style " "]) references in
      let (width, references) = (width + 1, default_style " " :: references) in
      (* Set gutter_width to the larger of the current gutter_width or the width
       * for this line. *)
      gutter_width := max !gutter_width width;
      (* Return the final list of references for the line along with the width. *)
      (width, references)
    )) file_line_references in
    let gutter_width = !gutter_width in
    (* Get the line number gutter length by looking at the string length for the
     * maximum line number.
     *
     * Sometimes, the maximum line number will not be read. So this might not be
     * the true maximum line number. However, for the purposes of
     * max_line_number_length this imprecision is not important.
     *
     * The penalty for this imprecision is our code frame gutter might be a
     * little wider then it needs to be in unlikely edge cases. *)
    let max_line_number_length = String.length (string_of_int max_line) in
    (* When Unicode characters are enabled, the gutter divider is a box
     * drawing [U+2502][1] character. Otherwise we use an ascii pipe symbol.
     *
     * [1]: http://graphemica.com/%E2%94%82 *)
    let vertical_line = if flags.unicode then "\xE2\x94\x82" else "|" in
    (* Print the code frame for each loc. Highlighting appropriate references. *)
    let code_frames = FileKeyMap.mapi (fun file_key locs ->
      (* Used by read_lines_in_file. *)
      let filename = file_of_source (Some file_key) in
      (* Get some data structures associated with this file. *)
      let tags = Option.value (FileKeyMap.get file_key tags) ~default:[] in
      let line_references =
        Option.value (FileKeyMap.get file_key file_line_references) ~default:IMap.empty in
      (* Fold all the locs for this file into code frames. *)
      let (_, _, code_frames) = List.fold_left (fun (tags, opened, code_frames) loc ->
        (* Read the lines from this location. *)
        let lines = read_lines_in_file loc filename stdin_file in
        match lines with
        | None -> (tags, opened, code_frames)
        | Some lines ->
          (* Create the code frame styles. *)
          let (_, tags, opened, code_frame) = List.fold_left (fun (n, tags, opened, acc) line ->
            (* Loop which will paint the different parts of a line of code in
             * our code frame. Eats tags on the current line. *)
            let rec loop acc col tags opened line =
              (* Get the current style for the line. *)
              let style = Option.value_map (get_tty_color_from_stack opened colors)
                ~f:(fun color -> Tty.Normal color) ~default:(Tty.Dim Tty.Default) in
              match tags with
              (* If we have no more tags then use our current style with
               * the line. *)
              | [] ->
                (tags, opened, (style, line) :: acc)
              (* If we have a tag on this line then eat it and add the new
               * opened tag to `opened`. Note that our condition depends on tag
               * being well formed by layout_references! *)
              | (pos, tag_id, tag_kind) :: tags when pos.line = n ->
                let opened = match tag_kind with
                | Open -> tag_id :: opened
                | Close -> List.filter (fun id -> tag_id <> id) opened
                in
                let split = pos.column - col in
                let left = String.sub line 0 split in
                let right = String.sub line split (String.length line - split) in
                let acc = (style, left) :: acc in
                loop acc pos.column tags opened right
              (* If we do not have a tag on this line then use our current style
               * with this line of code. *)
              | tags ->
                (tags, opened, (style, line) :: acc)
            in
            (* Start that loop! *)
            let (tags, opened, code_line) = loop [] 0 tags opened line in
            let code_line = List.rev code_line in
            (* Create the gutter text. *)
            let gutter =
              match IMap.get n line_references with
              | None ->
                [default_style (String.make gutter_width ' ')]
              | Some (width, references) when width < gutter_width ->
                default_style (String.make (gutter_width - width) ' ') :: references
              | Some (_, references) ->
                references
            in
            (* Create the next line. *)
            let next_line =
              (* Get the line number string with appropriate padding. *)
              let line_number =
                let n = string_of_int n in
                let padding = String.make (max_line_number_length - (String.length n)) ' ' in
                let n = (Tty.Dim Tty.Default, n) in
                [default_style padding; n; dim_style vertical_line]
              in
              gutter @
              line_number @
              (* If the line is empty then strip the whitespace which would be
               * trailing whitespace anyways. *)
              (if line = "" then [] else [default_style " "]) @
              code_line @
              [default_style "\n"]
            in
            (* Increment our line count and add the next line to
             * our accumulator. *)
            (n + 1, tags, opened, next_line :: acc)
          ) (loc.start.line, tags, opened, []) (Nel.to_list lines) in
          (tags, opened, List.concat (List.rev code_frame) :: code_frames)
      ) (tags, [], []) locs in
      match code_frames with
      | [] -> []
      | code_frame :: code_frames ->
        (* Add all of our code frames together with a colon for omitted chunks
         * of code in the file. *)
        List.concat (List.fold_left (fun acc code_frame ->
          code_frame :: [
            default_style (String.make (gutter_width + max_line_number_length) ' ');
            dim_style ":";
            default_style "\n";
          ] :: acc
        ) [code_frame] code_frames)
    ) locs in
    (* Get the root code frame from our map of code frames. We will start with
     * this code frame. *)
    let root_file_key = match root_loc.source with
    | None -> failwith "expected loc to have a source"
    | Some file_key -> file_key
    in
    let root_code_frame = FileKeyMap.get root_file_key code_frames in
    let code_frames = FileKeyMap.remove root_file_key code_frames in
    (* If we only have a root code frame then only render that. *)
    if FileKeyMap.is_empty code_frames then
      Option.value root_code_frame ~default:[]
    else
      let code_frames = FileKeyMap.bindings code_frames in
      let code_frames = match root_code_frame with
      | None -> code_frames
      | Some root_code_frame -> (root_file_key, root_code_frame) :: code_frames
      in
      (* Add a title to non-root code frames and concatenate them all together! *)
      List.concat (List.rev (List.fold_left (fun acc (file_key, code_frame) ->
        let open File_key in
        let file_name = match file_key with
        | LibFile filename
          -> relative_lib_path ~strip_root filename
        | SourceFile filename
        | JsonFile filename
        | ResourceFile filename
          -> relative_path ~strip_root filename
        | Builtins
          -> "(builtins)"
        in
        let header = [
          default_style (String.make gutter_width ' ');
          default_style (file_name ^ "\n");
        ] in
        let header = if acc = [] then header else default_style "\n" :: header in
        (header @ code_frame) :: acc
      ) [] code_frames))

  (* Goes through the process of laying out a friendly error message group by
   * combining our lower level functions like extract_references_intermediate
   * and layout_references.
   *
   * After we extract all of the reference locations from our message and give
   * them an id we then add a couple of "shadow references" with negative ids to
   * the references map. These shadow references include:
   *
   * - The root location of an error message.
   * - The primary locations of all error messages in the group.
   *
   * These shadow references are not shown in the gutter since they are not
   * referred to in the message text. Which is why they have a negative id.
   *
   * We add these "shadow references" into our reference system so that they may
   * be laid out along with all the other references by layout_references.
   *
   * We generally also provide a custom color to layout_references for these
   * "shadow references" so we don't use their default layout color. *)
  let layout_friendly_error_group
    root
    messages
  =
    let open Friendly in
    let module LocSet = Utils_js.LocSet in
    (* Setup our initial loc_to_id and id_to_loc maps. *)
    let (next_id, loc_to_id, id_to_loc) = (1, LocMap.empty, IMap.empty) in
    (* If we have a root then extract the references from that root message. *)
    let (next_id, loc_to_id, id_to_loc, root_message) = match root with
    | None -> (next_id, loc_to_id, id_to_loc, None)
    | Some { root_message; _ } ->
      let (next_id, loc_to_id, id_to_loc, root_message) =
        extract_references_intermediate next_id loc_to_id id_to_loc root_message in
      (next_id, loc_to_id, id_to_loc, Some root_message)
    in
    (* Extract the references and primary locations from all of our messages.
     * Messages is a non-empty list so we want to preserve that
     * for messages_reversed. *)
    let (next_id, loc_to_id, id_to_loc, messages_reversed, primary_locs) =
      let ((first_loc, first_message), messages) = messages in
      let (next_id, loc_to_id, id_to_loc, first_message) =
        extract_references_intermediate next_id loc_to_id id_to_loc first_message in
      let primary_locs = LocSet.add first_loc LocSet.empty in
      List.fold_left (fun (next_id, loc_to_id, id_to_loc, messages, primary_locs) (loc, message) ->
        let (next_id, loc_to_id, id_to_loc, message) =
          extract_references_intermediate next_id loc_to_id id_to_loc message in
        let primary_locs = LocSet.add loc primary_locs in
        (next_id, loc_to_id, id_to_loc, Nel.cons message messages, primary_locs)
      ) (next_id, loc_to_id, id_to_loc, Nel.one first_message, primary_locs) messages
    in
    (* Find all the references for primary locations. If there is not yet a
     * reference for a primary location then we create one. *)
    let (next_id, loc_to_id, id_to_loc, primary_loc_ids) =
      LocSet.fold (fun loc (next_id, loc_to_id, id_to_loc, primary_loc_ids) ->
        match LocMap.get loc loc_to_id with
        (* If there is a reference for this primary location then don't alter
         * our loc_to_id or id_to_loc maps. *)
        | Some id -> (next_id, loc_to_id, id_to_loc, ISet.add id primary_loc_ids)
        (* If there is no reference for this primary location then create a
         * negative id. Negative ids will not be rendered in the code
         * frame gutter. *)
        | None ->
          let id = -1 * next_id in
          let next_id = next_id + 1 in
          let loc_to_id = LocMap.add loc id loc_to_id in
          let id_to_loc = IMap.add id loc id_to_loc in
          let primary_loc_ids = ISet.add id primary_loc_ids in
          (next_id, loc_to_id, id_to_loc, primary_loc_ids)
      ) primary_locs (next_id, loc_to_id, id_to_loc, ISet.empty)
    in
    (* Go through a very similar process as primary locations to add a reference
     * for the root location and record its id. If a reference already exists
     * then we will not higlight our root location any differently! *)
    let (next_id, loc_to_id, id_to_loc, root_id) = match root with
    | None -> (next_id, loc_to_id, id_to_loc, None)
    | Some { root_loc; _ } ->
      (match LocMap.get root_loc loc_to_id with
      | Some _ -> (next_id, loc_to_id, id_to_loc, None)
      | None ->
        let id = -1 * next_id in
        let next_id = next_id + 1 in
        let loc_to_id = LocMap.add root_loc id loc_to_id in
        let id_to_loc = IMap.add id root_loc id_to_loc in
        (next_id, loc_to_id, id_to_loc, Some id)
      )
    in
    (* Create a custom color map for our primary location and root locations. *)
    let custom_colors = IMap.empty in
    (* Set the custom color for all primary loc ids to `Primary. *)
    let custom_colors = ISet.fold (fun id custom_colors ->
      IMap.add id `Primary custom_colors
    ) primary_loc_ids custom_colors in
    (* Manually set the custom color for the root loc to `Root. *)
    let custom_colors = match root_id with
    | None -> custom_colors
    | Some id -> IMap.add id `Root custom_colors
    in
    (* Layout all of our references. Including the negative ones. *)
    let (colors, tags) = layout_references ~custom_colors id_to_loc in
    (* Return everything we need for printing error messages. *)
    let _ = (next_id, loc_to_id) in
    (id_to_loc, colors, tags, root_message, messages_reversed)

  let get_pretty_printed_friendly_error_group
    ~stdin_file
    ~strip_root
    ~flags
    ~severity
    trace
    root
    messages
    omitted
  =
    let open Friendly in
    (* The header location is either the root location when we have more then
     * one message, or the primary location of the one message we have. *)
    let header =
      let loc = match root, messages with
      | None, ((loc, _), _) | _, ((loc, _), []) -> loc
      | Some { root_loc; _ }, _ -> root_loc
      in
      print_header_friendly ~strip_root ~flags ~severity loc
    in
    (* Layout our entire friendly error group. This returns a bunch of data we
     * will need to print our friendly error group. *)
    let (references, colors, tags, root_message, messages_reversed) =
      layout_friendly_error_group root messages in
    (* Print the text of our error message depending on the root message and
     * reversed messages. *)
    let message = match root_message, messages_reversed with
    (* When we do not have a root, we assume that there is only one error
     * message. Based on our error message grouping logic this is a
     * safe assumption. *)
    | None, (message, _) ->
      print_message_friendly ~flags ~colors message @ [default_style "\n"]
    (* If we have a root message and only one message then add them together
     * separated by a space. *)
    | Some root_message, (message, []) when omitted = 0 ->
      print_message_friendly ~flags ~colors (root_message @ (text " " :: message)) @
      [default_style "\n"]
    (* If we have a root message and more then one extra error messages then
     * we want to use our bullet list style. *)
    | Some root_message, (message, messages) ->
      let root_message =
        print_message_friendly ~flags ~colors root_message @ [default_style ":\n"] in
      let messages = List.rev (List.map (fun message ->
        print_message_friendly ~flags ~colors ~bullet:true message @
        [default_style "\n"]
      ) (message :: messages)) in
      let message = List.concat (root_message :: messages) in
      let message =
        if omitted = 0 then
          message
        else
          message @
          [default_style (
            " " ^
            bullet_char ~flags ^
            " ... " ^
            string_of_int omitted ^
            " more error" ^
            (if omitted = 1 then "" else "s") ^
            ".\n"
          )]
      in
      message
    in
    (* Print the code frame for our error message. *)
    let code_frame =
      let root_loc =
        Option.value_map root
          ~default:(let ((loc, _), _) = messages in loc)
          ~f:(fun { root_loc; _ } -> root_loc)
      in
      if Tty.should_color flags.color then
        print_code_frames_friendly
          ~stdin_file
          ~strip_root
          ~flags
          ~references
          ~colors
          ~tags
          root_loc
      else
        []
    in
    (* Put it all together! *)
    List.concat [
      (* Header: *)
      header;
      [default_style "\n"];

      (* Error Message: *)
      message;

      (* Code frame: *)
      (match code_frame with [] -> [] | code_frame -> default_style "\n" :: code_frame);

      (* Trace: *)
      (match trace with [] -> [] | _ -> [default_style "\n"]);
      List.concat (List.map (
        print_message_nice ~strip_root ~severity stdin_file (
          match file_of_source (let ((loc, _), _) = messages in loc.Loc.source) with
          | Some filename -> filename
          | None -> "[No file]"
        )
      ) (append_trace_reasons [] trace));

      (* Next error: *)
      [default_style "\n\n"];
    ]

  let print_styles ~out_channel ~flags styles =
    let styles =
      if flags.one_line then
        List.map remove_newlines styles
      else
        styles
    in
    Tty.cprint ~out_channel ~color_mode:flags.color styles

  let print_errors =
    let render_counts =
      let error_or_errors n = if n != 1 then "errors" else "error" in
      let warning_or_warnings n = if n != 1 then "warnings" else "warning" in

      fun ~err_count ~warn_count sep ->
        (* If there are 0 errors and 0 warnings, just render "0 errors" *)
        if warn_count = 0 then
          Printf.sprintf "%d%s%s"
            err_count sep (error_or_errors err_count)
        else if err_count = 0 then
          Printf.sprintf "%d%s%s"
            warn_count sep (warning_or_warnings warn_count)
        else (* err_count > 0 and warn_count > 0 *)
          Printf.sprintf "%d%s%s and %d%s%s"
            err_count sep (error_or_errors err_count)
            warn_count sep (warning_or_warnings warn_count)
    in

    fun ~out_channel ~flags ?(stdin_file=None)
      ~strip_root ~errors ~warnings () ->
      let truncate = not (flags.show_all_errors) in
      let max_count = if truncate then Some 50 else None in
      let err_count = ErrorSet.cardinal errors in
      let warn_count = ErrorSet.cardinal warnings in
      let errors = collect_errors_into_groups max_count errors in
      let warnings =
        collect_errors_into_groups
          (Option.map max_count ~f:(fun max_count -> max_count - err_count))
          warnings
      in
      let total_count = err_count + warn_count in
      let () =
        let iter_group ~severity group =
          let styles = match group with
          | ClassicSingleton (kind, trace, error) ->
            get_pretty_printed_classic_error
              ~stdin_file
              ~strip_root
              ~severity
              kind trace error
          | FriendlySingleton (_, trace, error) ->
            let open Friendly in
            get_pretty_printed_friendly_error_group
              ~stdin_file
              ~strip_root
              ~flags
              ~severity
              trace
              error.root
              (Nel.one (error.loc, error.message))
              0
          | FriendlyGroup { kind=_; root; messages; omitted } ->
            get_pretty_printed_friendly_error_group
              ~stdin_file
              ~strip_root
              ~flags
              ~severity
              []
              (Some root)
              (Nel.rev messages)
              omitted
          in
          print_styles ~out_channel ~flags styles
        in
        List.iter (iter_group ~severity:Err) (List.rev errors);
        List.iter (iter_group ~severity:Warn) (List.rev warnings);
      in
      if total_count > 0 then print_newline ();
      if truncate && total_count > 50 then (
        let remaining_errs, remaining_warns = if err_count - 50 < 0
          then 0, warn_count - (50 - err_count)
          else err_count - 50, warn_count
        in
        Printf.fprintf
          out_channel
          "... %s (only 50 out of %s displayed)\n"
          (render_counts ~err_count:remaining_errs ~warn_count:remaining_warns " more ")
          (render_counts ~err_count ~warn_count " ");
        Printf.fprintf
          out_channel
          "To see all errors, re-run Flow with --show-all-errors\n";
        flush out_channel
      ) else
        Printf.fprintf out_channel "Found %s\n"
          (render_counts ~err_count ~warn_count " ")
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

  let json_of_classic_error_props ~json_of_message error =
    let open Hh_json in
    let { messages; extra } = error in
    let props = [
      "message", JSON_Array (List.map json_of_message messages);
    ] in
    (* add extra if present *)
    if extra = [] then props
    else
      let extra = List.map (json_of_info_tree ~json_of_message) extra in
      ("extra", JSON_Array extra) :: props

  let json_of_error_props
    ~strip_root ~json_of_message ~severity
    ?(suppression_locs=Utils_js.LocSet.empty) (kind, trace, error) =
    let open Hh_json in
    let kind_str = match kind with
      | ParseError -> "parse"
      | InferError -> "infer"
      (* "InferWarning"s should still really be treated as errors. (The name is outdated.) *)
      | InferWarning -> "infer"
      | InternalError -> "internal"
      | DuplicateProviderError -> "duplicate provider"
      | RecursionLimitError -> "recursion limit exceeded"
      | LintError _ -> "lint"
    in
    let severity_str = output_string_of_severity severity in
    let suppressions = suppression_locs
    |> Utils_js.LocSet.elements
    |> List.map (fun loc ->
        JSON_Object [ "loc", Reason.json_of_loc ~strip_root loc]
      ) in
    let props = [
      "kind", JSON_String kind_str;
      "level", JSON_String severity_str;
      "suppressions", JSON_Array suppressions;
    ] in
    props @
    (* add the error type specific props *)
    (match error with
    | Classic error ->
      json_of_classic_error_props ~json_of_message error
    | Friendly error ->
      json_of_classic_error_props ~json_of_message (Friendly.to_classic error)) @
    (* add trace if present *)
    match trace with
    | [] -> []
    | _ -> ["trace", JSON_Array (List.map json_of_message trace)]

  let json_of_error_with_context
    ~strip_root ~stdin_file ~severity (error, suppression_locs) =
    let json_of_message =
      json_of_message_with_context ~strip_root ~stdin_file in
    Hh_json.JSON_Object (json_of_error_props ~strip_root ~json_of_message
      ~severity ~suppression_locs error)

  let json_of_errors_with_context
    ~strip_root ~stdin_file ~suppressed_errors ~errors ~warnings () =
    let errors = errors
      |> ErrorSet.elements
      |> List.map (fun err -> err, Utils_js.LocSet.empty) in
    let warnings = warnings
      |> ErrorSet.elements
      |> List.map (fun warn -> warn, Utils_js.LocSet.empty)
    in
    let f = json_of_error_with_context ~strip_root ~stdin_file in
    Hh_json.JSON_Array (
      List.map (f ~severity:Err) errors @
      List.map (f ~severity:Warn) warnings @
      (* We want these to show up as "suppressed error"s, not "suppressed off"s *)
      List.map (f ~severity:Err) suppressed_errors
    )

  let full_status_json_of_errors ~strip_root ~suppressed_errors
    ?(profiling=None) ?(stdin_file=None) ~errors ~warnings () =
    let open Hh_json in

    let props = [
      "flowVersion", JSON_String Flow_version.version;
      "errors", json_of_errors_with_context
        ~strip_root ~stdin_file ~suppressed_errors ~errors ~warnings ();
      "passed", JSON_Bool (ErrorSet.is_empty errors);
    ] in
    let props = match profiling with
    | None -> props
    | Some profiling -> props @ Profiling_js.to_json_properties profiling
    in
    JSON_Object props

  let print_errors
      ~out_channel
      ~strip_root
      ~suppressed_errors
      ?(pretty=false) ?(profiling=None) ?(stdin_file=None)
      ~errors
      ~warnings
      () =
    let open Hh_json in
    let res = full_status_json_of_errors ~strip_root ~profiling ~stdin_file
      ~suppressed_errors ~errors ~warnings () in
    if pretty then output_string out_channel (json_to_multiline res)
    else json_to_output out_channel res;
    flush out_channel
end

(* for vim and emacs plugins *)
module Vim_emacs_output = struct
  let string_of_loc ~strip_root loc = Loc.(
    match loc.source with
    | None
    | Some File_key.Builtins -> ""
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
    let to_pp_string ~strip_root prefix message =
      let loc, msg = to_pp message in
      let loc_str = string_of_loc ~strip_root loc in
      Printf.sprintf "%s%s%s" (endline loc_str) prefix (endline msg)
    in
    let classic_to_string ~strip_root prefix trace error =
      let {messages; _} = error in
      let messages = append_trace_reasons messages trace in
      let buf = Buffer.create 50 in
      (match messages with
      | [] -> assert false
      | message1 :: rest_of_error ->
          Buffer.add_string buf (to_pp_string ~strip_root prefix message1);
          List.iter begin fun message ->
            Buffer.add_string buf (to_pp_string ~strip_root "" message)
          end rest_of_error
      );
      Buffer.contents buf
    in
    let to_string ~strip_root prefix ((_, trace, error) : error) : string =
      match error with
      | Classic error ->
        classic_to_string ~strip_root prefix trace error
      | Friendly error ->
        classic_to_string ~strip_root prefix trace (Friendly.to_classic error)
    in
    fun ~strip_root oc ~errors ~warnings () ->
      let sl = []
        |> ErrorSet.fold (fun err acc ->
            (to_string ~strip_root "Error: " err)::acc
          ) (errors)
        |> ErrorSet.fold (fun warn acc ->
            (to_string ~strip_root "Warning: " warn)::acc
          ) (warnings)
        |> List.sort String.compare
        |> ListUtils.uniq
      in
      List.iter begin fun s ->
        output_string oc s;
        output_string oc "\n";
      end sl;
      flush oc
end
