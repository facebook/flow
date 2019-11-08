(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Severity

type infer_warning_kind =
  | ExportKind
  | OtherKind

type error_kind =
  | ParseError (* An error produced by the parser *)
  | PseudoParseError (* An error produced elsewhere but reported as a parse error *)
  | InferError
  | InferWarning of infer_warning_kind
  | InternalError
  | DuplicateProviderError
  | RecursionLimitError
  | LintError of Lints.lint_kind

let string_of_kind = function
  | ParseError -> "ParseError"
  | PseudoParseError -> "PseudoParseError"
  | InferError -> "InferError"
  | InferWarning _ -> "InferWarning"
  | InternalError -> "InternalError"
  | DuplicateProviderError -> "DuplicateProviderError"
  | RecursionLimitError -> "RecursionLimitError"
  | LintError lint_kind -> "LintError" ^ "-" ^ Lints.string_of_kind lint_kind

(* internal rep for core info *)
type 'a message =
  | BlameM of 'a * string
  | CommentM of string

(* simple structure for callers to specify error message content,
   converted to message internally. *)
type 'a info = 'a * string list

(** for extra info, enough structure to do simple tree-shaped output *)
type 'a info_tree =
  | InfoLeaf of 'a info list
  | InfoNode of 'a info list * 'a info_tree list

type 'a classic_error = {
  messages: 'a message list;
  extra: 'a info_tree list;
}

module LocSet = Loc_collections.LocSet
module LocMap = Loc_collections.LocMap

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
    loc: 'a;
    root: 'a error_root option;
    message: 'a error_message;
  }

  and 'a error_root = {
    root_loc: 'a;
    root_message: 'a message;
  }

  and 'a error_message =
    | Normal of {
        message: 'a message;
        frames: 'a message list option;
      }
    | Speculation of {
        frames: 'a message list;
        branches: (int * 'a t') list;
      }

  and 'a message = 'a message_feature list

  and 'a message_feature =
    | Inline of message_inline list
    | Reference of message_inline list * 'a

  and message_inline =
    | Text of string
    | Code of string

  (* The composition of some root error message and a list of associated
   * error messages. This structure is used in two contexts:
   *
   * 1. Grouping errors with the same error_root.
   * 2. Grouping errors from speculation_branches.
   *)
  type 'a message_group = {
    group_message: 'a message;
    group_message_list: 'a message_group list;
  }

  type t = ALoc.t t'

  type docs = {
    call: string;
    tuplemap: string;
    objmap: string;
    objmapi: string;
  }

  let docs =
    {
      call = "https://flow.org/en/docs/types/utilities/#toc-call";
      tuplemap = "https://flow.org/en/docs/types/utilities/#toc-tuplemap";
      objmap = "https://flow.org/en/docs/types/utilities/#toc-objmap";
      objmapi = "https://flow.org/en/docs/types/utilities/#toc-objmapi";
    }

  (* This function was introduced into the OCaml standard library in 4.04.0. Not
   * all of our tooling supports 4.04.0 yet, so we have a small
   * equivalent implementation here. *)
  let split_on_char c s =
    let rec loop i c s =
      try
        let k = String.index_from s i c in
        String.sub s i (k - i) :: loop (k + 1) c s
      with Not_found -> [String.sub s i (String.length s - i)]
    in
    loop 0 c s

  (* Converts a string into a message_inline list. e.g.:
   * "hello `world`" becomes: [Text "hello "; Code "world"].
   *
   * The inverse of string_of_message_inlines. *)
  let message_inlines_of_string s =
    Base.List.mapi
      ~f:(fun i s ->
        if i mod 2 = 0 then
          Text s
        else
          Code s)
      (split_on_char '`' s)

  (* Converts a string into a message. e.g.:
   * "hello `world`" becomes a message where "world" is styled as inline code. *)
  let message_of_string s = [Inline (message_inlines_of_string s)]

  (* Converts a message_inline list into a string. e.g.:
   * [Text "hello "; Code "world"] becomes: "hello `world`"
   *
   * The inverse of message_inlines_of_string. *)
  let string_of_message_inlines =
    List.fold_left
      (fun message -> function
        | Text text -> message ^ text
        | Code text -> message ^ "`" ^ text ^ "`")
      ""

  (* Convenience functions for constructing friendly error messages. e.g.
   *
   *     [ref lower; text " is incompatible with "; ref upper]
   *
   * Is an example of an incompatibility error message. *)

  let text s = Inline [Text s]

  let code s = Inline [Code s]

  let ref ?(loc = true) r =
    let desc = desc_of_reason ~unwrap:(is_scalar_reason r) r in
    let desc =
      match desc with
      | RCode code -> [Code code]
      | _ -> message_inlines_of_string (string_of_desc desc)
    in
    if loc then
      let loc =
        match annot_loc_of_reason r with
        | Some loc -> loc
        | None -> def_loc_of_reason r
      in
      if loc = Loc.none then
        Inline desc
      else
        Reference (desc, loc)
    else
      Inline desc

  (* Concatenates a list of messages with a conjunction according to the "rules"
   * of the English language. *)
  let conjunction_concat ?(conjunction = "and") = function
    | [] -> []
    | [x] -> x
    | [x1; x2] -> x1 @ [Inline [Text (" " ^ conjunction ^ " ")]] @ x2
    | xs ->
      let rec loop = function
        | []
        | [_] ->
          failwith "unreachable"
        | [x1; x2] -> x1 @ [Inline [Text (", " ^ conjunction ^ " ")]] @ x2
        | x :: xs -> x @ [Inline [Text ", "]] @ loop xs
      in
      loop xs

  (* Flattens out the Inline and Text constructors in an error message. Helpful
   * for hiding implementation details in our JSON output. *)
  let flatten_message =
    let rec loop_inlines inlines =
      match inlines with
      | [] -> []
      | (Code _ as inline) :: inlines -> inline :: loop_inlines inlines
      | (Text text as inline) :: inlines ->
        let inlines = loop_inlines inlines in
        (match inlines with
        | [] -> [inline]
        | Text text' :: inlines -> Text (text ^ text') :: inlines
        | inlines -> inline :: inlines)
    in
    let rec loop features =
      match features with
      | [] -> []
      | Reference (inlines, loc) :: features ->
        let inlines = loop_inlines inlines in
        let features = loop features in
        let feature = Reference (inlines, loc) in
        (match features with
        | [] -> [feature]
        | Inline inlines' :: features -> feature :: Inline (loop_inlines inlines') :: features
        | features -> feature :: features)
      | (Inline inlines as feature) :: features ->
        let features = loop features in
        (match features with
        | [] -> [feature]
        | Inline inlines' :: features -> Inline (inlines @ inlines') :: features
        | features -> feature :: features)
    in
    fun features ->
      let features = loop features in
      match features with
      | [] -> []
      | Inline inlines :: features -> Inline (loop_inlines inlines) :: features
      | features -> features

  (* Capitalizes the first letter in the message. Does not capitalize code or
   * text in references. *)
  let capitalize = function
    | [] -> []
    | Inline (Text s :: xs) :: message ->
      Inline (Text (String.capitalize_ascii s) :: xs) :: message
    | message -> message

  (* Uncapitalizes the first letter in the message. Does not uncapitalize code
   * or text in references. *)
  let uncapitalize = function
    | [] -> []
    | Inline (Text s :: xs) :: message ->
      Inline (Text (String.uncapitalize_ascii s) :: xs) :: message
    | message -> message

  (* Adds some message to the beginning of a group message. *)
  let append_group_message message { group_message; group_message_list } =
    { group_message = message @ (text " " :: uncapitalize group_message); group_message_list }

  (* Creates a message group from the error_message type. If show_all_branches
   * is false then we will hide speculation branches with a lower score. If any
   * speculation branches are hidden then the boolean we return will be true. *)
  let message_group_of_error =
    let message_of_frames frames acc_frames =
      let frames = Base.List.concat (List.rev (frames :: acc_frames)) in
      Base.List.concat (Base.List.intersperse (List.rev frames) [text " of "])
    in
    let rec flatten_speculation_branches
        ~show_all_branches ~hidden_branches ~high_score acc_frames acc = function
      | [] -> (hidden_branches, high_score, acc)
      | (score, error) :: branches ->
        (match error.message with
        (* If we have a speculation error with no frames and no root then we want
         * to flatten the branches of that error.
         *
         * We ignore the score for these errors. Instead propagating the
         * high_score we already have. *)
        | Speculation { branches = nested_branches; frames } when Option.is_none error.root ->
          (* We don't perform tail-call recursion here, but it's unlikely that
           * speculations will be so deeply nested that we blow the stack. *)
          let (hidden_branches, high_score, acc) =
            flatten_speculation_branches
              ~show_all_branches
              ~hidden_branches
              ~high_score
              (frames :: acc_frames)
              acc
              nested_branches
          in
          (* Resume recursion in our branches list. *)
          flatten_speculation_branches
            ~show_all_branches
            ~hidden_branches
            ~high_score
            acc_frames
            acc
            branches
        (* We add every other error if it has an appropriate score. *)
        | _ ->
          let (high_score, hidden_branches, acc) =
            if show_all_branches then
              (* If we are configured to show all branches then always add our
               * error to acc. *)
              (high_score, hidden_branches, (acc_frames, error) :: acc)
            else if
              (* If this message has a better score then throw away all old
               * messages. We are now hiding some messages. *)
              score > high_score
            then
              (score, hidden_branches || acc <> [], [(acc_frames, error)])
            (* If this message has the same score as our high score then add
             * it to acc and keep our high score. *)
            else if score = high_score then
              (high_score, hidden_branches, (acc_frames, error) :: acc)
            (* If this message has a lower score then our high score we skip
             * the error. We are now hiding at least one message. *)
            else
              (high_score, true, acc)
          in
          (* Recurse... *)
          flatten_speculation_branches
            ~show_all_branches
            ~hidden_branches
            ~high_score
            acc_frames
            acc
            branches)
    in
    let rec loop ~show_root ~show_all_branches ~hidden_branches acc_frames error =
      match error.message with
      (* Create normal error messages. *)
      | Normal { message; frames = Some frames } ->
        (* Add the frames to our error message. *)
        let frames = message_of_frames frames acc_frames in
        let message =
          if frames = [] then
            message
          else
            message @ (text " in " :: frames)
        in
        (* Add the root to our error message when we are configured to show
         * the root. *)
        let message =
          match error.root with
          | Some { root_message; _ } when show_root -> root_message @ (text " " :: message)
          | _ -> message
        in
        (* Finish our error message with a period. But only if frames
         * is empty! *)
        let message = message @ [text "."] in
        (* Get the primary location. It is the root location if error.loc is
         * outside of the root location. *)
        let primary_loc =
          match error.root with
          | None -> error.loc
          | Some { root_loc; _ } ->
            if Loc.contains root_loc error.loc then
              error.loc
            else
              root_loc
        in
        (hidden_branches, primary_loc, { group_message = message; group_message_list = [] })
      (* If there are no frames then we only want to add the root message (if we
       * have one) and return. We can safely ignore acc_frames. If a message has
       * frames set to None then the message is not equipped to handle
       * extra frames. *)
      | Normal { message; frames = None } ->
        (* Add the root to our error message when we are configured to show
         * the root. *)
        let message =
          match error.root with
          | Some { root_message; _ } when show_root -> root_message @ (text " " :: message)
          | _ -> message
        in
        (hidden_branches, error.loc, { group_message = message; group_message_list = [] })
      (* When we have a speculation error, do some work to create a message
       * group. Flatten out nested speculation errors with no frames. Hide
       * frames with low scores. Use a single message_group if we hide all but
       * one branches. *)
      | Speculation { frames; branches } ->
        (* Loop through our speculation branches. We will flatten out relevant
         * union branches and hide branches with a low score in this loop. *)
        let (hidden_branches, _, speculation_errors_rev) =
          flatten_speculation_branches
            ~show_all_branches
            ~high_score:min_int
            ~hidden_branches:false
            []
            []
            branches
        in
        (match speculation_errors_rev with
        (* When there is only one branch in acc (we had one branch with a
         * "high score") and this error does not have a root then loop while
         * adding the frames from this speculation error message. *)
        | [(acc_frames', speculation_error)] when Option.is_none speculation_error.root ->
          loop
            ~show_root
            ~show_all_branches
            ~hidden_branches
            (frames :: (acc_frames' @ acc_frames))
            { speculation_error with root = error.root }
        (* If there were more then one branches with high scores add them all
         * together in an error message group. *)
        | _ ->
          (* Get the message from the frames. *)
          let frames = message_of_frames frames acc_frames in
          let message =
            (* If we don't have an error root _and_ we don't have any frames
             * then use a mock error message. (We should always have a root
             * for speculation errors in theory, but as long as there are
             * UnknownUses it's possible we won't get a root.) *)
            if (Option.is_none error.root || not show_root) && frames = [] then
              [text "all branches are incompatible:"]
            else
              (* Otherwise create a message with our frames and optionally the
               * error message root. *)
              let message =
                if frames = [] then
                  []
                else
                  text "in " :: frames
              in
              (* Add the root to our error message when we are configured to
               * show the root. *)
              let message =
                match error.root with
                | Some { root_message; _ } when show_root ->
                  if message = [] then
                    root_message
                  else
                    root_message @ (text " " :: message)
                | _ -> message
              in
              (* Finish our error message with a colon. *)
              let message = message @ [text ":"] in
              message
          in
          (* Get the message group for all of our speculation errors. *)
          let (hidden_branches, group_message_list) =
            List.fold_left
              (fun (hidden_branches, group_message_list) (acc_frames', error) ->
                let (hidden_branches, _, message_group) =
                  loop ~show_root:true ~show_all_branches ~hidden_branches acc_frames' error
                in
                (hidden_branches, message_group :: group_message_list))
              (hidden_branches, [])
              (List.rev speculation_errors_rev)
          in
          ( hidden_branches,
            error.loc,
            {
              group_message = message;
              group_message_list =
                Base.List.mapi
                  ~f:(fun i message_group ->
                    append_group_message
                      ( if i = 0 then
                        [text "Either"]
                      else
                        [text "Or"] )
                      message_group)
                  group_message_list;
            } ))
    in
    (* Partially apply loop with the state it needs. Have fun! *)
    loop ~hidden_branches:false []

  let extract_references_message_intermediate ~next_id ~loc_to_id ~id_to_loc ~message =
    let (next_id, loc_to_id, id_to_loc, message) =
      List.fold_left
        (fun (next_id, loc_to_id, id_to_loc, message) message_feature ->
          match message_feature with
          | Inline inlines -> (next_id, loc_to_id, id_to_loc, Inline inlines :: message)
          | Reference (inlines, loc) ->
            (match LocMap.find_opt loc loc_to_id with
            | Some id -> (next_id, loc_to_id, id_to_loc, Reference (inlines, id) :: message)
            | None ->
              let id = next_id in
              let loc_to_id = LocMap.add loc id loc_to_id in
              let id_to_loc = IMap.add id loc id_to_loc in
              (next_id + 1, loc_to_id, id_to_loc, Reference (inlines, id) :: message)))
        (next_id, loc_to_id, id_to_loc, [])
        message
    in
    (next_id, loc_to_id, id_to_loc, List.rev message)

  (* The intermediate fold extract_references uses. Returns both a loc_to_id map
   * and an id_to_loc map. These maps are the inverses of one another. Also
   * returns a transformed message. *)
  let rec extract_references_intermediate ~next_id ~loc_to_id ~id_to_loc ~message_group =
    let (next_id, loc_to_id, id_to_loc, group_message) =
      extract_references_message_intermediate
        ~next_id
        ~loc_to_id
        ~id_to_loc
        ~message:message_group.group_message
    in
    let (next_id, loc_to_id, id_to_loc, group_message_list_rev) =
      List.fold_left
        (fun (next_id, loc_to_id, id_to_loc, group_message_list_rev) message_group ->
          let (next_id, loc_to_id, id_to_loc, message_group) =
            extract_references_intermediate ~next_id ~loc_to_id ~id_to_loc ~message_group
          in
          (next_id, loc_to_id, id_to_loc, message_group :: group_message_list_rev))
        (next_id, loc_to_id, id_to_loc, [])
        message_group.group_message_list
    in
    ( next_id,
      loc_to_id,
      id_to_loc,
      { group_message; group_message_list = List.rev group_message_list_rev } )

  (* Extracts common location references from a message. In order, each location
   * will be replaced with an integer reference starting at 1. If some reference
   * has the same location as another then they will share an id. *)
  let extract_references : Loc.t message_group -> Loc.t IMap.t * int message_group =
   fun message_group ->
    let (_, _, id_to_loc, message) =
      extract_references_intermediate
        ~next_id:1
        ~loc_to_id:LocMap.empty
        ~id_to_loc:IMap.empty
        ~message_group
    in
    (id_to_loc, message)

  (* Turns a group_message back into a message. We do this by adding all the
   * messages together. We don't insert newlines. This is a suboptimal
   * representation of a grouped message, but it works for our purposes. *)
  let message_of_group_message =
    let rec loop acc { group_message; group_message_list } =
      List.fold_left loop (group_message :: acc) group_message_list
    in
    fun message_group ->
      let acc = loop [] message_group in
      Base.List.concat (Base.List.intersperse (List.rev acc) [text " "])

  (* Converts our friendly error to a classic error message. *)
  let to_classic error =
    let (_, loc, message) =
      message_group_of_error ~show_all_branches:false ~show_root:true error
    in
    (* Extract the references from the message. *)
    let (references, message) = extract_references message in
    (* We use a basic strategy that concatenates all group messages together.
     * This isn't the most attractive approach, but it works for consumers of
     * the classic format. *)
    let message = message_of_group_message message in
    (* Turn the message into a string. *)
    let message =
      List.fold_left
        (fun message -> function
          | Inline inlines -> message ^ string_of_message_inlines inlines
          | Reference (inlines, id) ->
            message ^ string_of_message_inlines inlines ^ " [" ^ string_of_int id ^ "]")
        ""
        message
    in
    {
      messages = [BlameM (loc, message)];
      extra =
        ( if not (IMap.is_empty references) then
          InfoLeaf [(Loc.none, ["References:"])]
          :: ( references
             |> IMap.bindings
             |> Base.List.map ~f:(fun (id, loc) ->
                    InfoLeaf [(loc, ["[" ^ string_of_int id ^ "]"])]) )
        else
          [] );
    }
end

type 'loc printable_error = error_kind * 'loc message list * 'loc Friendly.t'

let info_to_messages = function
  | (loc, []) -> [BlameM (loc, "")]
  | (loc, msg :: msgs) -> BlameM (loc, msg) :: (msgs |> Base.List.map ~f:(fun msg -> CommentM msg))

let infos_to_messages infos = Base.List.(infos >>= info_to_messages)

let mk_error
    ?(kind = InferError)
    ?(trace_infos : 'loc info list option)
    ?(root : ('loc * 'loc Friendly.message) option)
    ?(frames : 'loc Friendly.message list option)
    (loc : 'loc)
    (message : 'loc Friendly.message) : 'loc printable_error =
  Friendly.(
    let trace = Option.value_map trace_infos ~default:[] ~f:infos_to_messages in
    let message =
      match kind with
      | LintError kind -> message @ [text " ("; code (Lints.string_of_kind kind); text ")"]
      | _ -> message
    in
    ( kind,
      trace,
      {
        loc;
        root = Option.map root (fun (root_loc, root_message) -> { root_loc; root_message });
        message = Normal { message; frames };
      } ))

let mk_speculation_error ?(kind = InferError) ?trace_infos ~loc ~root ~frames ~speculation_errors =
  Friendly.(
    let trace = Option.value_map trace_infos ~default:[] ~f:infos_to_messages in
    let branches =
      Base.List.map ~f:(fun (score, (_, _, error)) -> (score, error)) speculation_errors
      |> ListUtils.dedup
    in
    ( kind,
      trace,
      {
        loc;
        root = Option.map root (fun (root_loc, root_message) -> { root_loc; root_message });
        message = Speculation { frames; branches };
      } ))

(*******************************)

let to_pp = function
  | BlameM (loc, s) -> (loc, s)
  | CommentM s -> (Loc.none, s)

type stdin_file = (Path.t * string) option

let append_trace_reasons message_list trace_reasons =
  match trace_reasons with
  | [] -> message_list
  | _ -> message_list @ (BlameM (Loc.none, "Trace:") :: trace_reasons)

let default_style text = (Tty.Normal Tty.Default, text)

let source_fragment_style text = (Tty.Normal Tty.Default, text)

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
  if is_short_lib filename || Filename.is_relative filename then
    filename
  else
    match strip_root with
    | Some root -> Files.relative_path (Path.to_string root) filename
    | None ->
      let relname = Files.relative_path (Sys.getcwd ()) filename in
      if String.length relname < String.length filename then
        relname
      else
        filename

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
  let suffix = String.sub line c1 (String.length line - c1) in
  [source_fragment_style prefix; severity_style fragment; source_fragment_style suffix]

(* 0-indexed
 *
 * If start + len adds to more lines than exist in the file, then the full len
 * will not be returned. *)
let get_lines ~start ~len content =
  let rec loop ~start ~len ~acc ~pos content =
    if len = 0 || pos > String.length content then
      List.rev acc
    else
      let next_newline =
        (try String.index_from content pos '\n' with Not_found -> String.length content)
      in
      let continue =
        if start < 0 then
          raise (Invalid_argument "can't choose negative line")
        else if start = 0 then
          let acc = String.sub content pos (next_newline - pos) :: acc in
          loop ~start ~len:(len - 1) ~acc
        else
          loop ~start:(start - 1) ~len ~acc
      in
      continue ~pos:(next_newline + 1) content
  in
  loop ~start ~len ~acc:[] ~pos:0 content

let read_file ~stdin_file filename =
  match stdin_file with
  | Some (stdin_path, contents) when Path.to_string stdin_path = filename -> Some contents
  | _ ->
    if Filename.is_relative filename then
      failwith (Utils_js.spf "Expected absolute location, got %s" filename);
    Sys_utils.cat_or_failed filename

let get_offset_table_expensive ~stdin_file ~offset_kind loc =
  let open Option in
  let open Utils_js in
  Loc.source loc
  >>= File_key.to_path %> Base.Result.ok
  >>= read_file ~stdin_file
  >>| Offset_utils.make ~kind:offset_kind

let read_lines_in_file loc filename stdin_file =
  match filename with
  | None -> None
  | Some filename ->
    (match read_file ~stdin_file filename with
    | None -> None
    | Some content ->
      (try
         Loc.(
           let lines =
             get_lines
               ~start:(loc.start.line - 1)
               ~len:(loc._end.line - loc.start.line + 1)
               content
           in
           match lines with
           | [] -> None
           | first :: rest -> Some (first, rest))
       with Invalid_argument _ -> None))

let file_of_source source =
  match source with
  | Some (File_key.LibFile filename) ->
    let filename =
      if is_short_lib filename then
        let prefix_len = String.length lib_prefix in
        String.sub filename prefix_len (String.length filename - prefix_len)
      else
        filename
    in
    Some filename
  | Some (File_key.SourceFile filename)
  | Some (File_key.JsonFile filename)
  | Some (File_key.ResourceFile filename) ->
    Some filename
  | Some File_key.Builtins -> None
  | None -> None

let loc_of_printable_error ((_, _, { Friendly.loc; _ }) : 'loc printable_error) = loc

let loc_of_printable_error_for_compare ((_, _, err) : 'a printable_error) =
  Friendly.(
    match err with
    | { root = Some { root_loc; _ }; _ } -> root_loc
    | { loc; _ } -> loc)

let locs_of_printable_error =
  let locs_of_message locs message =
    Friendly.(
      List.fold_left
        (fun locs feature ->
          match feature with
          | Inline _ -> locs
          | Reference (_, loc) -> loc :: locs)
        locs
        message)
  in
  let rec locs_of_friendly_error locs error =
    Friendly.(
      let { loc; root; message } = error in
      let locs =
        Option.value_map root ~default:locs ~f:(fun { root_message; root_loc } ->
            root_loc :: locs_of_message locs root_message)
      in
      let locs =
        match message with
        | Normal { frames; message } ->
          let locs =
            Option.value_map frames ~default:locs ~f:(List.fold_left locs_of_message locs)
          in
          let locs = locs_of_message locs message in
          locs
        | Speculation { frames; branches } ->
          let locs = List.fold_left locs_of_message locs frames in
          let locs =
            List.fold_left (fun locs (_, error) -> locs_of_friendly_error locs error) locs branches
          in
          locs
      in
      let locs = loc :: locs in
      locs)
  in
  (fun ((_, _, error) : 'loc printable_error) -> locs_of_friendly_error [] error)

let kind_of_printable_error (kind, _, _) = kind

(* TODO: deprecate this in favor of Reason.json_of_loc *)
let deprecated_json_props_of_loc ~strip_root loc =
  Loc.(
    let file =
      match loc.source with
      | Some x -> Hh_json.JSON_String (Reason.string_of_source ~strip_root x)
      | None -> Hh_json.JSON_String ""
      (* TODO: return Hh_json.JSON_Null *)
    in
    [
      ("path", file);
      ("line", Hh_json.int_ loc.start.line);
      ("endline", Hh_json.int_ loc._end.line);
      ("start", Hh_json.int_ (loc.start.column + 1));
      ("end", Hh_json.int_ loc._end.column);
    ])

(* first reason's position, then second reason's position, etc.; if all
   positions match then first message, then second message, etc.

   for friendly errors check the location, docs slug, and then message. *)
let rec compare compare_loc =
  let kind_cmp =
    (* show internal errors first, then duplicate provider errors, then parse
       errors, then recursion limit errors. then both infer warnings and errors
       at the same priority. then lint errors *)
    let order_of_kind = function
      | InternalError -> 1
      | DuplicateProviderError -> 2
      | ParseError -> 3
      | PseudoParseError -> 3
      | RecursionLimitError -> 4
      | InferError -> 5
      | InferWarning _ -> 5
      | LintError _ -> 6
    in
    (fun k1 k2 -> order_of_kind k1 - order_of_kind k2)
  in
  let rec compare_lists f list1 list2 =
    match (list1, list2) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (hd1 :: tl1, hd2 :: tl2) ->
      let k = f hd1 hd2 in
      if k = 0 then
        compare_lists f tl1 tl2
      else
        k
  in
  let compare_option f o1 o2 =
    match (o1, o2) with
    | (Some x1, Some x2) -> f x1 x2
    | (Some _, None) -> 1
    | (None, Some _) -> -1
    | (None, None) -> 0
  in
  let compare_message_inline m1 m2 =
    Friendly.(
      match (m1, m2) with
      | (Text s1, Text s2) -> String.compare s1 s2
      | (Text _, Code _) -> 1
      | (Code _, Text _) -> -1
      | (Code s1, Code s2) -> String.compare s1 s2)
  in
  let compare_message_feature m1 m2 =
    Friendly.(
      match (m1, m2) with
      | (Inline m1, Inline m2) -> compare_lists compare_message_inline m1 m2
      | (Inline _, Reference _) -> 1
      | (Reference _, Inline _) -> -1
      | (Reference (m1, loc1), Reference (m2, loc2)) ->
        let k = compare_loc loc1 loc2 in
        if k = 0 then
          compare_lists compare_message_inline m1 m2
        else
          k)
  in
  let compare_friendly_message m1 m2 =
    Friendly.(
      match (m1, m2) with
      | (Normal { frames = fs1; message = m1 }, Normal { frames = fs2; message = m2 }) ->
        let k = compare_option (compare_lists (compare_lists compare_message_feature)) fs1 fs2 in
        if k = 0 then
          compare_lists compare_message_feature m1 m2
        else
          k
      | (Normal _, Speculation _) -> -1
      | (Speculation _, Normal _) -> 1
      | (Speculation { frames = fs1; branches = b1 }, Speculation { frames = fs2; branches = b2 })
        ->
        let k = compare_lists (compare_lists compare_message_feature) fs1 fs2 in
        if k = 0 then
          let k = List.length b1 - List.length b2 in
          if k = 0 then
            compare_lists
              (fun (_, err1) (_, err2) ->
                compare compare_loc (InferError, [], err1) (InferError, [], err2))
              b1
              b2
          else
            k
        else
          k)
  in
  fun err1 err2 ->
    Friendly.(
      let (loc1, loc2) =
        (loc_of_printable_error_for_compare err1, loc_of_printable_error_for_compare err2)
      in
      let ((k1, _, err1), (k2, _, err2)) = (err1, err2) in
      let k = compare_loc loc1 loc2 in
      if k = 0 then
        let k = kind_cmp k1 k2 in
        if k = 0 then
          match (err1, err2) with
          | ({ root = Some _; _ }, { root = None; _ }) -> -1
          | ({ root = None; _ }, { root = Some _; _ }) -> 1
          | ( { root = Some { root_message = rm1; _ }; loc = loc1; message = m1 },
              { root = Some { root_message = rm2; _ }; loc = loc2; message = m2 } ) ->
            let k = compare_lists compare_message_feature rm1 rm2 in
            if k = 0 then
              let k = compare_loc loc1 loc2 in
              if k = 0 then
                compare_friendly_message m1 m2
              else
                k
            else
              k
          | ({ root = None; message = m1; _ }, { root = None; message = m2; _ }) ->
            compare_friendly_message m1 m2
        else
          k
      else
        k)

module ConcreteLocPrintableErrorSet = Set.Make (struct
  type t = Loc.t printable_error

  let compare = compare Loc.compare
end)

type 'a error_group =
  (* Friendly errors without a root are never grouped. When traces are enabled
   * all friendly errors will never group. *)
  | Singleton of error_kind * 'a message list * 'a Friendly.t'
  (* Friendly errors that share a root are grouped together. The errors list
   * is reversed. *)
  | Group of {
      kind: error_kind;
      root: 'a Friendly.error_root;
      errors_rev: 'a Friendly.t' Nel.t;
      omitted: int;
    }

exception Interrupt_PrintableErrorSet_fold of Loc.t error_group list

(* Folds an PrintableErrorSet into a grouped list. However, the group and all sub-groups
 * are in reverse order. *)
let collect_errors_into_groups max set =
  Friendly.(
    try
      let (_, acc) =
        ConcreteLocPrintableErrorSet.fold
          (fun (kind, trace, error) (n, acc) ->
            let omit = Option.value_map max ~default:false ~f:(fun max -> max <= n) in
            let acc =
              match error with
              | error when trace <> [] ->
                if omit then raise (Interrupt_PrintableErrorSet_fold acc);
                Singleton (kind, trace, error) :: acc
              | { root = None; _ } as error ->
                if omit then raise (Interrupt_PrintableErrorSet_fold acc);
                Singleton (kind, trace, error) :: acc
              (* Friendly errors with a root might need to be grouped. *)
              | { root = Some root; _ } as error ->
                (match acc with
                (* When the root location and message match the previous group, add our
                 * friendly error to the group. We can do this by only looking at the last
                 * group because PrintableErrorSet is sorted so that friendly errors with the same
                 * root loc/message are stored next to each other.
                 *
                 * If we are now omitting errors then increment the omitted count
                 * instead of adding a message. *)
                | Group { kind = kind'; root = root'; errors_rev; omitted } :: acc'
                  when kind = kind'
                       && Loc.compare root.root_loc root'.root_loc = 0
                       && root.root_message = root'.root_message ->
                  Group
                    {
                      kind = kind';
                      root = root';
                      errors_rev =
                        ( if omit then
                          errors_rev
                        else
                          Nel.cons error errors_rev );
                      omitted =
                        ( if omit then
                          omitted + 1
                        else
                          omitted );
                    }
                  :: acc'
                (* If the roots did not match then we have a friendly singleton. *)
                | _ ->
                  if omit then raise (Interrupt_PrintableErrorSet_fold acc);
                  Group { kind; root; errors_rev = Nel.one error; omitted = 0 } :: acc)
            in
            (n + 1, acc))
          set
          (0, [])
      in
      acc
    with Interrupt_PrintableErrorSet_fold acc -> acc)

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
    show_all_branches: bool;
    unicode: bool;
    message_width: int;
  }

  let severity_fragment_style = function
    | Err -> error_fragment_style
    | Warn -> warning_fragment_style
    | Off -> Utils_js.assert_false "CLI output is only called with warnings and errors."

  let print_file_at_location ~strip_root ~severity stdin_file main_file loc s =
    Loc.(
      let l0 = loc.start.line in
      let l1 = loc._end.line in
      let c0 = loc.start.column in
      let c1 = loc._end.column in
      let filename = file_of_source loc.source in
      let severity_style = severity_fragment_style severity in
      let see_another_file ~is_lib filename =
        if filename = main_file then
          [default_style ""]
        else
          let prefix =
            Printf.sprintf
              ". See%s: "
              ( if is_lib then
                " lib"
              else
                "" )
          in
          let filename =
            if is_lib then
              relative_lib_path ~strip_root filename
            else
              relative_path ~strip_root filename
          in
          [comment_style prefix; comment_file_style (Printf.sprintf "%s:%d" filename l0)]
      in
      let code_line = read_lines_in_file loc filename stdin_file in
      match (code_line, filename) with
      | (_, None) -> [comment_style s; default_style "\n"]
      | (None, _) ->
        let (original_filename, is_lib) =
          match (filename, loc.source) with
          | (Some filename, Some (File_key.LibFile _))
          | (None, Some (File_key.LibFile filename)) ->
            (filename, true)
          | (Some filename, _)
          | (None, Some (File_key.SourceFile filename))
          | (None, Some (File_key.JsonFile filename))
          | (None, Some (File_key.ResourceFile filename)) ->
            (filename, false)
          | (None, Some File_key.Builtins)
          | (None, None) ->
            failwith "Should only have lib and source files at this point"
        in
        [comment_style s] @ see_another_file ~is_lib original_filename @ [default_style "\n"]
      | (Some code_lines, Some filename) ->
        let is_lib =
          match loc.source with
          | Some (File_key.LibFile _) -> true
          | _ -> false
        in
        begin
          match code_lines with
          | (code_line, []) ->
            (* Here we have a single line of context *)
            let line_number_text = Printf.sprintf "%3d: " l0 in
            let highlighted_line =
              if l1 == l0 && String.length code_line >= c1 then
                highlight_error_in_line ~severity_style code_line c0 c1
              else
                [source_fragment_style code_line]
            in
            let padding =
              let line_num = String.make (String.length line_number_text) ' ' in
              let spaces =
                let prefix =
                  if String.length code_line <= c0 then
                    code_line
                  else
                    String.sub code_line 0 c0
                in
                Str.global_replace (Str.regexp "[^\t ]") " " prefix
              in
              line_num ^ spaces
            in
            let underline_size =
              if l1 == l0 then
                max 1 (c1 - c0)
              else
                1
            in
            let underline = String.make underline_size '^' in
            (line_number_style line_number_text :: highlighted_line)
            @ [comment_style (Printf.sprintf "\n%s%s %s" padding underline s)]
            @ see_another_file ~is_lib filename
            @ [default_style "\n"]
          | code_lines ->
            (* Here we have multiple lines of context *)

            (* The most lines of context that we'll show before abridging *)
            let max_lines = 5 in
            (* Don't abridge if we could just show all the lines *)
            let abridged = l1 - l0 + 1 > max_lines in
            (* Highlight the context *)
            let highlighted_lines =
              code_lines
              |> Nel.to_list
              |> List.fold_left
                   (fun (line_num, acc) line ->
                     if (not abridged) || line_num - l0 < max_lines - 2 || line_num = l1 then
                       let line_number_text =
                         line_number_style (Utils_js.spf "\n%3d: " line_num)
                       in
                       let highlighted_line =
                         (* First line *)
                         if line_num = l0 then
                           highlight_error_in_line ~severity_style line c0 (String.length line)
                         (* Last line *)
                         else if line_num = l1 then
                           highlight_error_in_line ~severity_style line 0 c1
                         (* middle lines *)
                         else
                           [error_fragment_style line]
                       in
                       (line_num + 1, (line_number_text :: highlighted_line) :: acc)
                     else if line_num - l0 = max_lines - 1 then
                       (line_num + 1, [line_number_style "\n...:"] :: acc)
                     else
                       (line_num + 1, acc))
                   (l0, [])
              |> snd
              |> List.rev
              |> List.flatten
            in
            let first_line = code_lines |> Nel.hd in
            let last_line = code_lines |> Nel.rev |> Nel.hd in
            (* Don't underline the whitespace at the beginning of the last line *)
            let underline_prefix =
              if Str.string_match (Str.regexp "^\\([\t ]*\\).*") last_line 0 then
                Str.matched_group 1 last_line
              else
                ""
            in
            let overline_size = max 1 (String.length first_line - c0) in
            let underline_size = max 1 (c1 - String.length underline_prefix) in
            let line len =
              if len > 0 then
                String.make len '-'
              else
                ""
            in
            let overline = "v" ^ line (overline_size - 1) in
            let underline = line (underline_size - 1) ^ "^" in
            let overline_padding =
              let line_num = String.make (String.length (Printf.sprintf "%3d: " l0)) ' ' in
              let spaces =
                if String.length first_line <= c0 then
                  ""
                else
                  let prefix = String.sub first_line 0 c0 in
                  Str.global_replace (Str.regexp "[^\t ]") " " prefix
              in
              line_num ^ spaces
            in
            let underlineline_padding =
              String.make (String.length (Printf.sprintf "%3d: " l1)) ' '
            in
            let comment =
              Printf.sprintf "\n%s%s%s %s" underlineline_padding underline_prefix underline s
            in
            [comment_style (Printf.sprintf "%s%s" overline_padding overline)]
            @ highlighted_lines
            @ [comment_style comment]
            @ see_another_file ~is_lib filename
            @ [default_style "\n"]
        end)

  let print_message_nice ~strip_root ~severity stdin_file main_file message =
    let (loc, s) = to_pp message in
    print_file_at_location ~strip_root ~severity stdin_file main_file loc s

  let remove_newlines (color, text) = (color, Str.global_replace (Str.regexp "\n") "\\n" text)

  (* ==========================
   * Full Terminal Width Header
   * ==========================
   *
   * The header will always be the length of flags.message_width which in turn
   * is the lesser of the terminal length and 120 characters. *)
  let print_header_friendly ~strip_root ~flags ~severity loc =
    let severity_style = severity_fragment_style severity in
    let severity_name =
      match severity with
      | Err -> "Error"
      | Warn -> "Warning"
      | Off -> failwith "unreachable"
    in
    let horizontal_line_length = flags.message_width - (String.length severity_name + 1) in
    let filename =
      Loc.(
        File_key.(
          let { source; start = { line; column; _ }; _ } = loc in
          let pos = ":" ^ string_of_int line ^ ":" ^ string_of_int (column + 1) in
          match source with
          | Some (LibFile filename) -> relative_lib_path ~strip_root filename ^ pos
          | Some (SourceFile filename)
          | Some (JsonFile filename)
          | Some (ResourceFile filename) ->
            relative_path ~strip_root filename ^ pos
          | Some Builtins
          | None ->
            ""))
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
            | _ -> failwith "unreachable")
      else
        String.make horizontal_line_length '-'
    in
    (* Construct the header by appending the constituent pieces. *)
    [
      severity_style
        ( severity_name
        ^ " "
        ^ horizontal_line
        ^ ( if filename_on_newline then
            "\n"
          else
            " " )
        ^ filename );
      default_style "\n";
    ]

  module FileKeyMap = WrappedMap.Make (File_key)

  type tag_kind =
    | Open of Loc.position
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
  let layout_references ~(custom_colors : 'a IMap.t) : Loc.t IMap.t -> 'a color IMap.t * tags =
    Loc.(
      let rec add_tags colors opened id start end' tags tags_acc =
        match tags with
        (* For an empty array, add both the open and close tags. Also add a color
         * of 0 for this id. *)
        | [] ->
          let color =
            Option.value_map
              (IMap.find_opt id custom_colors)
              ~f:(fun custom -> CustomColor custom)
              ~default:(Color 0)
          in
          let colors = IMap.add id color colors in
          (colors, List.rev ((end', id, Close) :: (start, id, Open end') :: tags_acc))
        (* Search for the correct place where start should appear in our list.
         *
         * If pos and start are equal and the current tag ends _after_ the end
         * position we are inserting then we want to open inside the current tag.
         *
         * Note that we may introduce an Open tag _before_ a Close tag at the same
         * position! This is intentional as it introduces overlapping. If we have
         * two tags that open/close at the same position we want them to be
         * different colors. Intersecting accomplishes this. *)
        | ((pos, tag_id, tag_kind) as tag) :: tags
          when let k = pos_cmp pos start in
               k < 0
               || k = 0
                  &&
                  match tag_kind with
                  | Close -> false
                  | Open tag_end -> pos_cmp end' tag_end < 0 ->
          (* Keep track of the ids which are opening and closing so that we can
           * increment their colors if start is inside them.
           *
           * Also keep track of the ids which were opened at the time this tag was
           * opened. This allows us to move backwards through the containment tree
           * and update tag colors appropriately.
           *
           * See the definition of update_colors for more information. *)
          let opened =
            Opened
              (let (Opened opened') = opened in
               match tag_kind with
               | Open _ -> IMap.add tag_id opened opened'
               | Close -> IMap.remove tag_id opened')
          in
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
          let colors =
            match IMap.find_opt id custom_colors with
            | Some custom -> IMap.add id (CustomColor custom) colors
            | None ->
              (* Increment the colors of all open references by the color of this tag.
               * It is a logic error if some open_id does not exist in colors. *)
              let colors = update_colors opened colors color in
              (* Add the color for this reference. *)
              IMap.add id (Color color) colors
          in
          (* Finish by adding an open tag. A corresponding closing tag will have
           * been added by add_close_tag. *)
          (colors, List.rev_append ((start, id, Open end') :: tags_acc) tags)
      and add_close_tag colors id end' tags color_acc tags_acc =
        match tags with
        | [] -> (color_acc, List.rev ((end', id, Close) :: tags_acc))
        (* Search for the last place to add our end position.
         *
         * Note that we may introduce a Close tag _after_ an Open tag at the same
         * position! This is intentional as it introduces overlapping. If we have
         * two tags that open/close at the same position we want them to be
         * different colors. Intersecting accomplishes this. *)
        | ((pos, tag_id, tag_kind) as tag) :: tags when pos_cmp pos end' <= 0 ->
          (* If we run into an open tag then our color must be at least 1 greater
           * then the color of the opened tag. *)
          let color_acc =
            match tag_kind with
            | Close -> color_acc
            | Open _ ->
              (match IMap.find_opt tag_id colors with
              | None -> max color_acc (0 + 1)
              | Some (Color tag_color) -> max color_acc (tag_color + 1)
              | Some (CustomColor _) -> color_acc)
          in
          add_close_tag colors id end' tags color_acc (tag :: tags_acc)
        (* When we find the location for our close tag, add it. *)
        | tags -> (color_acc, List.rev_append ((end', id, Close) :: tags_acc) tags)
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
        IMap.fold
          (fun open_id opened colors ->
            let open_color = Option.value (IMap.find_opt open_id colors) ~default:(Color 0) in
            match open_color with
            | CustomColor _ -> colors
            | Color open_color ->
              if open_color >= color + 1 then
                colors
              else
                let colors = IMap.add open_id (Color (color + 1)) colors in
                update_colors opened colors (color + 1))
          opened
          colors
      in
      fun references ->
        List.fold_left
          (fun (colors, file_tags) (id, loc) ->
            match loc.source with
            | None -> (colors, file_tags)
            | Some source ->
              let tags = Option.value (FileKeyMap.find_opt source file_tags) ~default:[] in
              let (colors, tags) =
                add_tags colors (Opened IMap.empty) id loc.start loc._end tags []
              in
              let file_tags = FileKeyMap.add source tags file_tags in
              (colors, file_tags))
          (IMap.empty, FileKeyMap.empty)
          (IMap.bindings references))

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
      (match rank mod 4 with
      | 0 -> Tty.Cyan
      | 1 -> Tty.Yellow
      | 2 -> Tty.Green
      | 3 -> Tty.Magenta
      | _ -> failwith "unreachable")

  let get_tty_color id colors =
    get_tty_color_internal (Option.value (IMap.find_opt id colors) ~default:(Color 0))

  (* Gets the Tty color from a stack of ids. This function will ignore
   * CustomColor `Root if there are other colors on the stack. *)
  let rec get_tty_color_from_stack ids colors =
    match ids with
    | [] -> None
    | id :: ids ->
      (match Option.value (IMap.find_opt id colors) ~default:(Color 0) with
      | CustomColor `Root ->
        Option.value_map
          (get_tty_color_from_stack ids colors)
          ~f:(fun x -> Some x)
          ~default:(Some Tty.Default)
      | color -> Some (get_tty_color_internal color))

  let bullet_char ~flags =
    (* Use [U+2022][1] for the bullet character if unicode is enabled.
     *
     * [1]: http://graphemica.com/%E2%80%A2 *)
    if flags.unicode then
      "\xE2\x80\xA2"
    else
      "-"

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
    Friendly.(
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
          | None -> (String.length str, [(style, str)])
          (* If there is another word then we want to merge str with
           * that word. We can assume that there is no breakpoint between
           * the end of str and the beginning of the next word. *)
          | Some (len, word) -> (String.length str + len, (style, str) :: word)
        in
        let rec loop finished_words_acc word_in_progress_acc strs =
          match strs with
          | [] ->
            let finished_words_acc =
              match word_in_progress_acc with
              | None -> finished_words_acc
              | Some word -> word :: finished_words_acc
            in
            List.rev_map (fun (n, words) -> (n, List.rev words)) finished_words_acc
          | (breakable, style, str) :: strs ->
            (* If our string is breakable then try to find the first space. Use
             * that space as the breakpoint. *)
            let bp =
              if breakable then
                try Some (String.index str ' ') with Not_found -> None
              else
                None
            in
            (match bp with
            (* If we have no breakpoint then we want to either create a new word
             * or combine our str with the first word in the result of
             * recursively calling split_into_words. *)
            | None -> loop finished_words_acc (Some (merge style str word_in_progress_acc)) strs
            (* If we have a breakpoint then we need to split up our str and
             * create a new word with the left half and recurse with the
             * right half. *)
            | Some bp ->
              let left = String.sub str 0 bp in
              let right = String.sub str (bp + 1) (String.length str - (bp + 1)) in
              (* We need to recurse with the right half of our split str because
               * there may be more words we need to split out in it. bp is only
               * the first breakpoint in str. We can assume that our right half
               * is breakable since we would have no breakpoint if str was
               * not breakable. *)
              loop
                (merge style left word_in_progress_acc :: finished_words_acc)
                None
                ((true, style, right) :: strs))
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
              loop ((style, str) :: acc) (pos + len) ~line_length styles
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
      let concat_words_into_lines ~line_length ?indentation_first ?indentation words =
        let indentation = with_len indentation in
        let indentation_first =
          match indentation_first with
          | Some _ as indentation_first -> with_len indentation_first
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
            let init =
              match indentation_first with
              | None -> (init_len, [init_word])
              | Some (indentation_first_len, indentation_first) ->
                (indentation_first_len + init_len, [init_word; indentation_first])
            in
            ((fun () -> is_last_underlined init_word), init)
          in
          let (_, (_, acc)) =
            List.fold_left
              (fun (last_underlined, (pos, acc)) (len, word) ->
                (* If our position on the line plus one (for the space we would
                 * insert) plus the length of our word will fit in our line length
                 * then add the word to the current line separated from acc with a
                 * space. Otherwise start a new line where word is the only text. *)
                if pos + 1 + len > line_length then
                  let last_underlined () = is_last_underlined word in
                  let (newline_len, newline) =
                    match indentation with
                    | None -> (0, [default_style "\n"])
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
                        Tty.Underline Tty.Default
                      else
                        Tty.Normal Tty.Default
                    in
                    (style, " ")
                  in
                  let last_underlined () = is_last_underlined word in
                  (last_underlined, (pos + 1 + len, (space :: word) :: acc)))
              init
              words
          in
          Base.List.concat (List.rev acc)
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
      fun ~flags ~colors ~indentation message ->
        let message =
          List.rev
            (List.fold_left
               (fun acc feature ->
                 match feature with
                 | Inline inlines ->
                   List.rev_append
                     (Base.List.map ~f:(print_message_inline ~flags ~reference:false) inlines)
                     acc
                 | Reference (inlines, id) ->
                   let message =
                     Base.List.map ~f:(print_message_inline ~flags ~reference:true) inlines
                     @ [
                         (false, Tty.Normal Tty.Default, " ");
                         (false, Tty.Dim Tty.Default, "[");
                         (false, Tty.Normal (get_tty_color id colors), string_of_int id);
                         (false, Tty.Dim Tty.Default, "]");
                       ]
                   in
                   List.rev_append message acc)
               []
               message)
        in
        (* Create the indentation for our message. The first line of indentation
         * will contain a bullet character. *)
        let indentation_space =
          if indentation <= 0 then
            None
          else
            Some (String.make ((indentation - 1) * 3) ' ')
        in
        let indentation_first =
          Option.map indentation_space ~f:(fun space ->
              [default_style (space ^ " " ^ bullet_char ~flags ^ " ")])
        in
        let indentation =
          Option.map indentation_space ~f:(fun space -> [default_style (space ^ "   ")])
        in
        let message =
          concat_words_into_lines
            ~line_length:flags.message_width
            ?indentation_first
            ?indentation
            (split_into_words message)
        in
        message @ [default_style "\n"])

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

  (* When Unicode characters are enabled, the gutter divider is a box
   * drawing [U+2502][1] character. Otherwise we use an ascii pipe symbol.
   *
   * [1]: http://graphemica.com/%E2%94%82 *)
  let vertical_line ~flags =
    if flags.unicode then
      "\xE2\x94\x82"
    else
      "|"

  (* Prints a File_key.t to a string. *)
  let print_file_key ~strip_root file_key =
    File_key.(
      match file_key with
      | Some (LibFile filename) -> relative_lib_path ~strip_root filename
      | Some (SourceFile filename)
      | Some (JsonFile filename)
      | Some (ResourceFile filename) ->
        relative_path ~strip_root filename
      | Some Builtins
      | None ->
        "(builtins)")

  exception Oh_no_file_contents_have_changed

  (* =========================
   * Error Message Code Frames
   * =========================
   *
   * We render the root location for our friendly error message. Decorated with
   * the reference locations from the message. *)
  let print_code_frames_friendly ~stdin_file ~strip_root ~flags ~references ~colors ~tags root_loc
      =
    Loc.(
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
          {
            root_loc with
            start = { root_loc.start with line = start_line };
            _end = { root_loc._end with line = end_line };
          }
        in
        expanded_root_loc :: Base.List.map ~f:snd (IMap.bindings references)
      in
      (* Group our locs by their file key.
       *
       * Also split large locs into two smaller locs.
       *
       * Also compute the largest line number. We need this to compute the
       * gutter width. *)
      let (max_line, locs) =
        List.fold_left
          (fun (max_line, acc) loc ->
            match loc.source with
            | None -> failwith "expected loc to have a source"
            | Some source ->
              (* If our loc is larger then some threshold determined by
               * omit_after_lines then split it into two locs. *)
              let new_locs =
                if loc._end.line - loc.start.line + 1 <= omit_after_lines * 2 then
                  [loc]
                else
                  let loc1 =
                    {
                      loc with
                      _end = { loc.start with line = loc.start.line + (omit_after_lines - 1) };
                    }
                  in
                  let loc2 =
                    {
                      loc with
                      start = { loc._end with line = loc._end.line - (omit_after_lines - 1) };
                    }
                  in
                  [loc1; loc2]
              in
              (* Add the new locs to our FileKeyMap. *)
              let locs = Option.value (FileKeyMap.find_opt source acc) ~default:[] in
              (max max_line loc._end.line, FileKeyMap.add source (new_locs @ locs) acc))
          (0, FileKeyMap.empty)
          locs
      in
      (* Perform some organization operations on our locs. *)
      let locs =
        FileKeyMap.map
          (fun locs ->
            (* Sort all of the locations we want to display. Locations in the root
             * file should appear first. Sort in the reverse direction. Our next merge
             * step will flip the list back around. *)
            let locs = List.sort Loc.compare locs in
            (* Merge the locations we want to display. We start with the location with
             * the lowest line number. Our fold depends on this to merge correctly. *)
            let locs =
              List.fold_left
                (fun acc loc ->
                  match acc with
                  (* Init. *)
                  | [] -> [loc]
                  (* If the previous loc + 3 lines below intersects with the next loc then
                   * we want to merge those locs into one code frame. *)
                  | last_loc :: acc
                    when Loc.lines_intersect
                           loc
                           {
                             last_loc with
                             _end =
                               {
                                 last_loc._end with
                                 line = last_loc._end.line + (merge_nearby_lines + 1);
                               };
                           } ->
                    let loc =
                      {
                        source = last_loc.source;
                        start =
                          ( if pos_cmp loc.start last_loc.start < 0 then
                            loc.start
                          else
                            last_loc.start );
                        _end =
                          ( if pos_cmp loc._end last_loc._end > 0 then
                            loc._end
                          else
                            last_loc._end );
                      }
                    in
                    loc :: acc
                  (* Otherwise, add the loc by itself. *)
                  | acc -> loc :: acc)
                []
                locs
            in
            (* Return the reversed locs. *)
            List.rev locs)
          locs
      in
      (* Organize all our references onto the line which we will find them on. We
       * do a second pass to sort these references and determine the
       * gutter width. *)
      let file_line_references =
        IMap.fold
          (fun id loc file_line_references ->
            if id < 0 then
              file_line_references
            else
              match loc.source with
              | None -> failwith "expected loc to have a source"
              | Some source ->
                let line_references =
                  Option.value
                    (FileKeyMap.find_opt source file_line_references)
                    ~default:IMap.empty
                in
                let references =
                  Option.value (IMap.find_opt loc.start.line line_references) ~default:[]
                in
                let references = (id, loc.start) :: references in
                let line_references = IMap.add loc.start.line references line_references in
                FileKeyMap.add source line_references file_line_references)
          references
          FileKeyMap.empty
      in
      (* Create the styled text which we will put in the gutter for all of our
       * file line references.
       *
       * We use a ref for gutter_width since we want to map file_line_references
       * in place instead of using a fold which would re-create the map. *)
      let gutter_width = ref 5 in
      let file_line_references =
        FileKeyMap.map
          (IMap.map (fun references ->
               (* Reverse sort the references by their start position. We reverse sort
                * since we fold_left next. *)
               let references = List.sort (fun (_, a) (_, b) -> pos_cmp b a) references in
               (* Fold the list. Creating the width and the string we will
                * ultimately render. *)
               let (width, references) =
                 List.fold_left
                   (fun (width, acc) (id, _) ->
                     let string_id = string_of_int id in
                     let width = width + 2 + String.length string_id in
                     let acc =
                       dim_style "["
                       :: (Tty.Normal (get_tty_color id colors), string_id)
                       :: dim_style "]"
                       :: acc
                     in
                     (width, acc))
                   (1, [default_style " "])
                   references
               in
               let (width, references) = (width + 1, default_style " " :: references) in
               (* Set gutter_width to the larger of the current gutter_width or the width
                * for this line. *)
               gutter_width := max !gutter_width width;

               (* Return the final list of references for the line along with the width. *)
               (width, references)))
          file_line_references
      in
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
      let vertical_line = vertical_line ~flags in
      (* Print the code frame for each loc. Highlighting appropriate references. *)
      let code_frames =
        FileKeyMap.mapi
          (fun file_key locs ->
            (* Used by read_lines_in_file. *)
            let filename = file_of_source (Some file_key) in
            (* Get some data structures associated with this file. *)
            let tags = Option.value (FileKeyMap.find_opt file_key tags) ~default:[] in
            let line_references =
              Option.value (FileKeyMap.find_opt file_key file_line_references) ~default:IMap.empty
            in
            (* Fold all the locs for this file into code frames. *)
            let (_, _, code_frames) =
              List.fold_left
                (fun (tags, opened, code_frames) loc ->
                  (* Read the lines from this location. *)
                  let lines = read_lines_in_file loc filename stdin_file in
                  match lines with
                  | None ->
                    (* Failed to read the file, so skip this code frame *)
                    (tags, opened, code_frames)
                  | Some lines ->
                    (try
                       (* Create the code frame styles. *)
                       let (_, tags, opened, code_frame) =
                         List.fold_left
                           (fun (n, tags, opened, acc) line ->
                             (* Loop which will paint the different parts of a line of code in
                              * our code frame. Eats tags on the current line. *)
                             let rec loop acc col tags opened line =
                               (* Get the current style for the line. *)
                               let style =
                                 Option.value_map
                                   (get_tty_color_from_stack opened colors)
                                   ~f:(fun color -> Tty.Normal color)
                                   ~default:(Tty.Dim Tty.Default)
                               in
                               match tags with
                               (* If we have no more tags then use our current style with
                                * the line. *)
                               | [] -> (tags, opened, (style, line) :: acc)
                               (* If we have a tag on this line then eat it and add the new
                                * opened tag to `opened`. Note that our condition depends on tag
                                * being well formed by layout_references! *)
                               | (pos, tag_id, tag_kind) :: tags when pos.line = n ->
                                 let opened =
                                   match tag_kind with
                                   | Open _ -> tag_id :: opened
                                   | Close -> List.filter (fun id -> tag_id <> id) opened
                                 in
                                 let split = pos.column - col in
                                 let (left, right) =
                                   (* TODO: Get a SHA for each file when we parse it, and include the SHA in the
                                    * loc. Then, we can know for sure whether a file has changed or not when we
                                    * go to pretty print an error.
                                    *
                                    * Here we only know for sure that a file has changed when a particular line
                                    * is too short, which means we can sometimes print bad code frames.
                                    *)
                                   try
                                     ( String.sub line 0 split,
                                       String.sub line split (String.length line - split) )
                                   with Invalid_argument _ ->
                                     raise Oh_no_file_contents_have_changed
                                 in
                                 let acc = (style, left) :: acc in
                                 loop acc pos.column tags opened right
                               (* If we do not have a tag on this line then use our current style
                                * with this line of code. *)
                               | tags -> (tags, opened, (style, line) :: acc)
                             in
                             (* Start that loop! *)
                             let (tags, opened, code_line) = loop [] 0 tags opened line in
                             let code_line = List.rev code_line in
                             (* Create the gutter text. *)
                             let gutter =
                               match IMap.find_opt n line_references with
                               | None -> [default_style (String.make gutter_width ' ')]
                               | Some (width, references) when width < gutter_width ->
                                 default_style (String.make (gutter_width - width) ' ')
                                 :: references
                               | Some (_, references) -> references
                             in
                             (* Create the next line. *)
                             let next_line =
                               (* Get the line number string with appropriate padding. *)
                               let line_number =
                                 let n = string_of_int n in
                                 let padding =
                                   String.make (max_line_number_length - String.length n) ' '
                                 in
                                 let n = (Tty.Dim Tty.Default, n) in
                                 [default_style padding; n; dim_style vertical_line]
                               in
                               gutter
                               @ line_number
                               (* If the line is empty then strip the whitespace which would be
                                * trailing whitespace anyways. *)
                               @ ( if line = "" then
                                   []
                                 else
                                   [default_style " "] )
                               @ code_line
                               @ [default_style "\n"]
                             in
                             (* Increment our line count and add the next line to
                              * our accumulator. *)
                             (n + 1, tags, opened, next_line :: acc))
                           (loc.start.line, tags, opened, [])
                           (Nel.to_list lines)
                       in
                       (tags, opened, Base.List.concat (List.rev code_frame) :: code_frames)
                     with Oh_no_file_contents_have_changed ->
                       (* Realized the file has changed, so skip this code frame *)
                       (tags, opened, code_frames)))
                (tags, [], [])
                locs
            in
            match code_frames with
            | [] -> []
            | code_frame :: code_frames ->
              (* Add all of our code frames together with a colon for omitted chunks
               * of code in the file. *)
              Base.List.concat
                (List.fold_left
                   (fun acc code_frame ->
                     code_frame
                     :: [
                          default_style (String.make (gutter_width + max_line_number_length) ' ');
                          dim_style ":";
                          default_style "\n";
                        ]
                     :: acc)
                   [code_frame]
                   code_frames))
          locs
      in
      (* Get the root code frame from our map of code frames. We will start with
       * this code frame. *)
      let root_file_key =
        match root_loc.source with
        | None -> failwith "expected loc to have a source"
        | Some file_key -> file_key
      in
      let root_code_frame = FileKeyMap.find_opt root_file_key code_frames in
      let code_frames = FileKeyMap.remove root_file_key code_frames in
      (* If we only have a root code frame then only render that. *)
      if FileKeyMap.is_empty code_frames then
        Option.value root_code_frame ~default:[]
      else
        let code_frames = FileKeyMap.bindings code_frames in
        let code_frames =
          match root_code_frame with
          | None -> code_frames
          | Some root_code_frame -> (root_file_key, root_code_frame) :: code_frames
        in
        (* Add a title to non-root code frames and concatenate them all together! *)
        Base.List.concat
          (List.rev
             (List.fold_left
                (fun acc (file_key, code_frame) ->
                  let file_key = print_file_key ~strip_root (Some file_key) in
                  let header =
                    [default_style (String.make gutter_width ' '); default_style (file_key ^ "\n")]
                  in
                  let header =
                    if acc = [] then
                      header
                    else
                      default_style "\n" :: header
                  in
                  (header @ code_frame) :: acc)
                []
                code_frames)))

  (* ===================================
   * Error Message Colorless Code Frames
   * ===================================
   *
   * Renders the root location along with reference locations, but
   * without color! *)
  let print_colorless_code_frames_friendly
      ~stdin_file ~strip_root ~flags ~references ~root_reference_id root_loc =
    Loc.(
      let vertical_line = vertical_line ~flags in
      (* Get the maximum end line number. We will use this for computing our
       * gutter width. *)
      let max_end_line =
        IMap.fold
          (fun _ loc max_end_line -> max max_end_line loc._end.line)
          references
          root_loc._end.line
      in
      (* Get the max gutter extension length which is the length of the longest
       * line number plus 3. *)
      let gutter_width = 3 + String.length (string_of_int max_end_line) in
      (* Prints a single, colorless, location. *)
      let print_loc ~with_filename id loc =
        (* Get the lines for the location... *)
        let filename = file_of_source loc.source in
        let lines = read_lines_in_file loc filename stdin_file in
        let lines =
          Option.map lines (fun line_list ->
              (* Print every line by appending the line number and appropriate
               * gutter width. *)
              let (_, lines) =
                Nel.fold_left
                  (fun (n, lines) line ->
                    (* If we show more lines then some upper limit omit any extra code. *)
                    if
                      n >= loc.start.line + omit_after_lines
                      && n <= loc._end.line - omit_after_lines
                    then
                      if n = loc.start.line + omit_after_lines then
                        let gutter = String.make gutter_width ' ' in
                        (n + 1, lines ^ gutter ^ ":\n")
                      else
                        (n + 1, lines)
                      (* Otherwise, render the line. *)
                    else
                      let n_string = string_of_int n in
                      let gutter_space = String.make (gutter_width - String.length n_string) ' ' in
                      let gutter = gutter_space ^ n_string ^ vertical_line in
                      let lines =
                        if line = "" then
                          lines ^ gutter ^ "\n"
                        else
                          lines ^ gutter ^ " " ^ line ^ "\n"
                      in
                      (n + 1, lines))
                  (loc.start.line, "")
                  line_list
              in
              (* Get our gutter space for the underline and overline. *)
              let gutter_space = String.make (gutter_width + 2) ' ' in
              (* Add the overline for our loc. *)
              let lines =
                if loc.start.line = loc._end.line then
                  lines
                else
                  let first_line_len = String.length (Nel.hd line_list) in
                  (* In some cases, we create a location that starts at or after the
               end of a line. This probably shouldn't happen, but if it does, we
               can still create an overline with a carat pointing to that column
               position. *)
                  let first_line_len = max first_line_len (loc.start.column + 1) in
                  gutter_space
                  ^ String.make loc.start.column ' '
                  ^ "v"
                  ^ String.make (first_line_len - loc.start.column - 1) '-'
                  ^ "\n"
                  ^ lines
              in
              (* Add the underline for our loc. *)
              let lines =
                lines
                ^ gutter_space
                ^
                if loc.start.line = loc._end.line then
                  String.make loc.start.column ' '
                  ^ String.make (loc._end.column - loc.start.column) '^'
                else
                  let last_line = Nel.hd (Nel.rev line_list) in
                  (* Don't underline the whitespace at the beginning of the last line *)
                  let underline_prefix =
                    if Str.string_match (Str.regexp "^\\([\t ]*\\).*") last_line 0 then
                      Str.matched_group 1 last_line
                    else
                      ""
                  in
                  (* TODO - if dash_length is less than 0 that means the line in question probably
                   * changed. As mentioned in another comment in this file, we should have better
                   * detection and behavior when we notice that the file we're reading for context has
                   * changed. But at the very least we shouldn't crash, which is what will happen if
                   * we call String.make with a negative length *)
                  let dash_length = loc._end.column - String.length underline_prefix - 1 in
                  underline_prefix ^ String.make (max dash_length 0) '-' ^ "^"
              in
              (* If we have a reference id then add it just after the underline. *)
              let lines =
                match id with
                | Some id when id > 0 -> lines ^ " [" ^ string_of_int id ^ "]"
                | _ -> lines
              in
              (* Add a final newline to lines. *)
              let lines = lines ^ "\n" in
              (* Return our final lines string *)
              lines)
        in
        (* If we were configured to print the filename then add it to our lines
         * before returning. *)
        if not with_filename then
          lines
        else
          Option.map lines (fun lines ->
              let space = String.make 3 ' ' in
              let filename = print_file_key ~strip_root loc.source in
              let filename =
                filename
                ^ ":"
                ^ string_of_int loc.start.line
                ^ ":"
                ^ string_of_int (loc.start.column + 1)
              in
              space ^ filename ^ "\n" ^ lines)
      in
      (* Print the locations for all of our references. *)
      let references =
        IMap.fold
          (fun id loc acc ->
            let is_root =
              Option.value_map root_reference_id ~default:false ~f:(fun root_id -> root_id = id)
            in
            (* Skip this reference if either it is a "shadow reference" or it is the
             * reference for the root. *)
            if id <= 0 || is_root then
              acc
            else
              let code = print_loc ~with_filename:true (Some id) loc in
              match code with
              | None -> acc
              | Some code -> acc ^ code)
          references
          ""
      in
      (* Add the "References:" label if we have some references. *)
      let references =
        match references with
        | "" -> ""
        | _ -> "\nReferences:\n" ^ references
      in
      (* Print the root location. *)
      match print_loc ~with_filename:(references <> "") root_reference_id root_loc with
      | Some root_code -> [default_style (root_code ^ references)]
      | None -> [default_style references])

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
  let layout_friendly_error_group ~root_loc ~primary_locs ~message_group =
    Friendly.(
      (* Setup our initial loc_to_id and id_to_loc maps. *)
      let (next_id, loc_to_id, id_to_loc) = (1, LocMap.empty, IMap.empty) in
      (* Extract all our references from the message group. *)
      let (next_id, loc_to_id, id_to_loc, message_group) =
        extract_references_intermediate ~next_id ~loc_to_id ~id_to_loc ~message_group
      in
      (* Find all the references for primary locations. If there is not yet a
       * reference for a primary location then we create one. *)
      let (next_id, loc_to_id, id_to_loc, primary_loc_ids) =
        LocSet.fold
          (fun loc (next_id, loc_to_id, id_to_loc, primary_loc_ids) ->
            match LocMap.find_opt loc loc_to_id with
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
              (next_id, loc_to_id, id_to_loc, primary_loc_ids))
          primary_locs
          (next_id, loc_to_id, id_to_loc, ISet.empty)
      in
      (* Go through a very similar process as primary locations to add a reference
       * for the root location and record its id. If a reference already exists
       * then we will not higlight our root location any differently! *)
      let (next_id, loc_to_id, id_to_loc, root_id, custom_root_color) =
        match LocMap.find_opt root_loc loc_to_id with
        | Some id -> (next_id, loc_to_id, id_to_loc, Some id, false)
        | None ->
          let id = -1 * next_id in
          let next_id = next_id + 1 in
          let loc_to_id = LocMap.add root_loc id loc_to_id in
          let id_to_loc = IMap.add id root_loc id_to_loc in
          (next_id, loc_to_id, id_to_loc, Some id, true)
      in
      (* Create a custom color map for our primary location and root locations. *)
      let custom_colors = IMap.empty in
      (* Set the custom color for all primary loc ids to `Primary. *)
      let custom_colors =
        ISet.fold
          (fun id custom_colors -> IMap.add id `Primary custom_colors)
          primary_loc_ids
          custom_colors
      in
      (* Manually set the custom color for the root loc to `Root. *)
      let custom_colors =
        match root_id with
        | Some id when custom_root_color -> IMap.add id `Root custom_colors
        | _ -> custom_colors
      in
      (* Layout all of our references. Including the negative ones. *)
      let (colors, tags) = layout_references ~custom_colors id_to_loc in
      (* Return everything we need for printing error messages. *)
      let _ = (next_id, loc_to_id) in
      (id_to_loc, root_id, colors, tags, message_group))

  let get_pretty_printed_friendly_error_group
      ~stdin_file ~strip_root ~flags ~severity ~trace ~root_loc ~primary_locs ~message_group =
    Friendly.(
      (* Get the primary and root locations. *)
      let primary_loc =
        if LocSet.cardinal primary_locs = 1 then
          LocSet.min_elt primary_locs
        else
          root_loc
      in
      (* The header location is the primary location when we have color and the
       * root location when we don't have color. *)
      let header =
        print_header_friendly
          ~strip_root
          ~flags
          ~severity
          ( if Tty.should_color flags.color then
            primary_loc
          else
            root_loc )
      in
      (* Layout our entire friendly error group. This returns a bunch of data we
       * will need to print our friendly error group. *)
      let (references, root_reference_id, colors, tags, message_group) =
        layout_friendly_error_group ~root_loc ~primary_locs ~message_group
      in
      (* Print the text of our error message by traversing the message_group. We
       * print group_message at the current indentation and group_message_list at
       * the current indentation plus 1. *)
      let message =
        let rec loop ~indentation acc message_group =
          let acc =
            print_message_friendly ~flags ~colors ~indentation message_group.group_message :: acc
          in
          loop_list ~indentation:(indentation + 1) acc message_group.group_message_list
        and loop_list ~indentation acc message_group_list =
          match message_group_list with
          | [] -> acc
          | message_group :: message_group_list ->
            (* Not tail-recursive for message_group depth. Generally message_group
             * should not be more then 5 or so deep. *)
            let acc = loop ~indentation acc message_group in
            loop_list ~indentation acc message_group_list
        in
        Base.List.concat (List.rev (loop ~indentation:0 [] message_group))
      in
      (* Print the code frame for our error message. *)
      let code_frame =
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
          print_colorless_code_frames_friendly
            ~stdin_file
            ~strip_root
            ~flags
            ~references
            ~root_reference_id
            root_loc
      in
      (* Put it all together! *)
      Base.List.concat
        [
          (* Header: *)
            header;
          [default_style "\n"];
          (* Error Message: *)
            message;
          (* Code frame: *)
            (match code_frame with
            | [] -> []
            | code_frame -> default_style "\n" :: code_frame);
          (* Trace: *)
            (match trace with
            | [] -> []
            | _ -> [default_style "\n"]);
          Base.List.concat
            (Base.List.map
               ~f:
                 (print_message_nice
                    ~strip_root
                    ~severity
                    stdin_file
                    (match file_of_source root_loc.Loc.source with
                    | Some filename -> filename
                    | None -> "[No file]"))
               (append_trace_reasons [] trace));
          (* Next error: *)
            [default_style "\n"];
        ])

  let get_pretty_printed_error
      ~flags ~stdin_file ~strip_root ~severity ~show_all_branches ~on_hidden_branches group =
    let check (hidden_branches, a, b) =
      if hidden_branches then on_hidden_branches ();
      (a, b)
    in
    match group with
    (* Singleton errors concatenate the optional error root with the error
     * message and render a single message. Sometimes singletons will also
     * render a trace. *)
    | Singleton (_, trace, error) ->
      Friendly.(
        let (primary_loc, { group_message; group_message_list }) =
          check (message_group_of_error ~show_all_branches ~show_root:true error)
        in
        get_pretty_printed_friendly_error_group
          ~stdin_file
          ~strip_root
          ~flags
          ~severity
          ~trace
          ~root_loc:
            (match error.root with
            | Some { root_loc; _ } -> root_loc
            | None -> error.loc)
          ~primary_locs:(LocSet.singleton primary_loc)
          ~message_group:{ group_message = capitalize group_message; group_message_list })
    (* Groups either render a single error (if there is only a single group
     * member) or a group will render a list of errors where some are
     * optionally omitted. *)
    | Group { kind = _; root; errors_rev; omitted } ->
      Friendly.(
        (* Constructs the message group. *)
        let (primary_locs, message_group) =
          match errors_rev with
          (* When we only have a single message, append the root and move on. *)
          | (error, []) when omitted = 0 ->
            let (primary_loc, message_group) =
              check (message_group_of_error ~show_all_branches ~show_root:true error)
            in
            (LocSet.singleton primary_loc, message_group)
          (* When we have multiple members we need to put them in a group with the
           * root message as the group message. *)
          | _ ->
            let acc =
              if omitted = 0 then
                []
              else
                [
                  {
                    group_message_list = [];
                    group_message =
                      [
                        text
                          ( "... "
                          ^ string_of_int omitted
                          ^ " more error"
                          ^ ( if omitted = 1 then
                              ""
                            else
                              "s" )
                          ^ "." );
                      ];
                  };
                ]
            in
            let (primary_locs, group_message_list) =
              List.fold_left
                (fun (primary_locs, acc) error ->
                  let (primary_loc, message_group) =
                    check (message_group_of_error ~show_all_branches ~show_root:false error)
                  in
                  (LocSet.add primary_loc primary_locs, message_group :: acc))
                (LocSet.empty, acc)
                (Nel.to_list errors_rev)
            in
            (primary_locs, { group_message = root.root_message @ [text ":"]; group_message_list })
        in
        get_pretty_printed_friendly_error_group
          ~stdin_file
          ~strip_root
          ~flags
          ~severity
          ~trace:[]
          ~root_loc:root.root_loc
          ~primary_locs
          ~message_group)

  let print_styles ~out_channel ~flags styles =
    let styles =
      if flags.one_line then
        Base.List.map ~f:remove_newlines styles
      else
        styles
    in
    Tty.cprint ~out_channel ~color_mode:flags.color styles;
    Tty.cprint ~out_channel ~color_mode:flags.color [default_style "\n"]

  let format_errors =
    let render_counts =
      let error_or_errors n =
        if n != 1 then
          "errors"
        else
          "error"
      in
      let warning_or_warnings n =
        if n != 1 then
          "warnings"
        else
          "warning"
      in
      fun ~err_count ~warn_count sep ->
        (* If there are 0 errors and 0 warnings, just render "0 errors" *)
        if warn_count = 0 then
          Printf.sprintf "%d%s%s" err_count sep (error_or_errors err_count)
        else if err_count = 0 then
          Printf.sprintf "%d%s%s" warn_count sep (warning_or_warnings warn_count)
        else
          (* err_count > 0 and warn_count > 0 *)
          Printf.sprintf
            "%d%s%s and %d%s%s"
            err_count
            sep
            (error_or_errors err_count)
            warn_count
            sep
            (warning_or_warnings warn_count)
    in
    fun ~out_channel ~flags ?(stdin_file = None) ~strip_root ~errors ~warnings ~lazy_msg () ->
      let truncate = not flags.show_all_errors in
      let max_count =
        if truncate then
          Some 50
        else
          None
      in
      let err_count = ConcreteLocPrintableErrorSet.cardinal errors in
      let warn_count = ConcreteLocPrintableErrorSet.cardinal warnings in
      let errors = collect_errors_into_groups max_count errors in
      let warnings =
        collect_errors_into_groups
          (Option.map max_count ~f:(fun max_count -> max_count - err_count))
          warnings
      in
      let total_count = err_count + warn_count in
      let hidden_branches = ref false in
      let () =
        let iter_group ~severity group =
          let styles =
            get_pretty_printed_error
              ~flags
              ~stdin_file
              ~strip_root
              ~severity
              ~show_all_branches:flags.show_all_branches (* Feels like React... *)
              ~on_hidden_branches:(fun () -> hidden_branches := true)
              group
          in
          print_styles ~out_channel ~flags styles
        in
        List.iter (iter_group ~severity:Err) (List.rev errors);
        List.iter (iter_group ~severity:Warn) (List.rev warnings)
      in
      let hidden_branches = !hidden_branches in
      if total_count > 0 then print_newline ();
      if truncate && total_count > 50 then (
        let (remaining_errs, remaining_warns) =
          if err_count - 50 < 0 then
            (0, warn_count - (50 - err_count))
          else
            (err_count - 50, warn_count)
        in
        Printf.fprintf
          out_channel
          "... %s (only 50 out of %s displayed)\n"
          (render_counts ~err_count:remaining_errs ~warn_count:remaining_warns " more ")
          (render_counts ~err_count ~warn_count " ");
        Printf.fprintf out_channel "To see all errors, re-run Flow with --show-all-errors\n";
        flush out_channel
      ) else
        Printf.fprintf out_channel "Found %s\n" (render_counts ~err_count ~warn_count " ");
      if hidden_branches then
        Printf.fprintf
          out_channel
          "\nOnly showing the most relevant union/intersection branches.\nTo see all branches, re-run Flow with --show-all-branches\n";
      Option.iter lazy_msg ~f:(Printf.fprintf out_channel "\n%s\n");
      ()

  let print_errors
      ~out_channel ~flags ?(stdin_file = None) ~strip_root ~errors ~warnings ~lazy_msg () =
    format_errors ~out_channel ~flags ~stdin_file ~strip_root ~errors ~warnings ~lazy_msg ()
end

(* JSON output *)
module Json_output = struct
  type json_version =
    | JsonV1
    | JsonV2

  let unwrap_message = function
    | BlameM (loc, str) when loc <> Loc.none -> (str, Some loc)
    | BlameM (_, str)
    | CommentM str ->
      (str, None)

  let json_of_message_props ~stdin_file ~strip_root ~offset_kind message =
    Hh_json.(
      let (desc, loc) = unwrap_message message in
      let type_ =
        match message with
        | BlameM _ -> "Blame"
        | CommentM _ -> "Comment"
      in
      ("descr", JSON_String desc)
      :: ("type", JSON_String type_)
      ::
      (match loc with
      | None -> deprecated_json_props_of_loc ~strip_root Loc.none
      | Some loc ->
        let offset_table = get_offset_table_expensive ~stdin_file ~offset_kind loc in
        ("loc", Reason.json_of_loc ~strip_root ~offset_table ~catch_offset_errors:true loc)
        :: deprecated_json_props_of_loc ~strip_root loc))

  (* Returns the first line of the context *)
  let json_of_loc_context ~stdin_file loc =
    Hh_json.(
      let code_line =
        match loc with
        | None -> None
        | Some loc ->
          Loc.(
            let filename = file_of_source loc.source in
            (match read_lines_in_file loc filename stdin_file with
            | Some l -> Some (Nel.hd l)
            | None -> None))
      in
      match code_line with
      | None -> JSON_Null
      | Some context -> JSON_String context)

  let json_of_loc_context_abridged ~stdin_file ~max_len loc =
    Hh_json.(
      let code_lines =
        match loc with
        | None -> None
        | Some loc ->
          Loc.(
            let filename = file_of_source loc.source in
            (* Read the lines referenced in the loc *)
            (match read_lines_in_file loc filename stdin_file with
            | Some l ->
              let lines = Nel.to_list l in
              let num_lines = List.length lines in
              let numbered_lines =
                Base.List.mapi
                  ~f:(fun i line -> (string_of_int (i + loc.start.line), JSON_String line))
                  lines
              in
              if
                num_lines <= max_len
                (* There are few enough lines that we can use them all for context *)
              then
                Some numbered_lines
              else
                (* There are too many lines for context. Let's take some lines from the start of the loc
                 * and some from the end of the loc *)
                let start_len = (max_len + 1) / 2 in
                (* ceil *)
                let end_len = max_len / 2 in
                (* floor *)
                Some
                  ( Base.List.sub numbered_lines ~pos:0 ~len:start_len
                  @ Base.List.sub numbered_lines ~pos:(num_lines - end_len) ~len:end_len )
            | None -> None))
      in
      match code_lines with
      | None -> JSON_Null
      | Some code_lines -> JSON_Object code_lines)

  let json_of_loc_with_context ~strip_root ~stdin_file ~offset_kind loc =
    Hh_json.(
      let props =
        let offset_table = get_offset_table_expensive ~stdin_file ~offset_kind loc in
        Reason.json_of_loc_props ~strip_root ~offset_table ~catch_offset_errors:true loc
        @ [("context", json_of_loc_context_abridged ~stdin_file ~max_len:5 (Some loc))]
      in
      JSON_Object props)

  let json_of_message_with_context ~strip_root ~stdin_file ~offset_kind message =
    Hh_json.(
      let (_, loc) = unwrap_message message in
      let context = ("context", json_of_loc_context ~stdin_file loc) in
      JSON_Object (context :: json_of_message_props ~stdin_file ~strip_root ~offset_kind message))

  let json_of_infos ~json_of_message infos =
    Hh_json.(JSON_Array (Base.List.map ~f:json_of_message (infos_to_messages infos)))

  let rec json_of_info_tree ~json_of_message tree =
    Hh_json.(
      let (infos, kids) =
        match tree with
        | InfoLeaf infos -> (infos, None)
        | InfoNode (infos, kids) -> (infos, Some kids)
      in
      JSON_Object
        ( ("message", json_of_infos ~json_of_message infos)
        ::
        (match kids with
        | None -> []
        | Some kids ->
          let kids = Base.List.map ~f:(json_of_info_tree ~json_of_message) kids in
          [("children", JSON_Array kids)]) ))

  let json_of_classic_error_props ~json_of_message error =
    Hh_json.(
      let { messages; extra } = error in
      let props = [("message", JSON_Array (Base.List.map ~f:json_of_message messages))] in
      (* add extra if present *)
      if extra = [] then
        props
      else
        let extra = Base.List.map ~f:(json_of_info_tree ~json_of_message) extra in
        ("extra", JSON_Array extra) :: props)

  let json_of_message_inline_friendly message_inline =
    Hh_json.(
      Friendly.(
        match message_inline with
        | Text text -> JSON_Object [("kind", JSON_String "Text"); ("text", JSON_String text)]
        | Code code -> JSON_Object [("kind", JSON_String "Code"); ("text", JSON_String code)]))

  let json_of_message_friendly message =
    Hh_json.(
      Friendly.(
        let message = flatten_message message in
        JSON_Array
          (Base.List.concat
             (Base.List.map
                ~f:(function
                  | Inline inlines -> Base.List.map ~f:json_of_message_inline_friendly inlines
                  | Reference (inlines, id) ->
                    [
                      JSON_Object
                        [
                          ("kind", JSON_String "Reference");
                          ("referenceId", JSON_String (string_of_int id));
                          ( "message",
                            JSON_Array (Base.List.map ~f:json_of_message_inline_friendly inlines)
                          );
                        ];
                    ])
                message))))

  let rec json_of_message_group_friendly message_group =
    Hh_json.(
      Friendly.(
        let { group_message; group_message_list } = message_group in
        let group_message = json_of_message_friendly group_message in
        if group_message_list = [] then
          group_message
        else
          JSON_Object
            [
              ("kind", JSON_String "UnorderedList");
              ("message", group_message);
              ( "items",
                JSON_Array (Base.List.map ~f:json_of_message_group_friendly group_message_list) );
            ]))

  let json_of_references ~strip_root ~stdin_file ~offset_kind references =
    Hh_json.(
      JSON_Object
        (List.rev
           (IMap.fold
              (fun id loc acc ->
                ( string_of_int id,
                  json_of_loc_with_context ~strip_root ~stdin_file ~offset_kind loc )
                :: acc)
              references
              [])))

  let json_of_friendly_error_props ~strip_root ~stdin_file ~offset_kind error =
    Hh_json.(
      Friendly.(
        let (_, primary_loc, message_group) =
          message_group_of_error ~show_all_branches:false ~show_root:true error
        in
        let (references, message_group) = extract_references message_group in
        let root_loc =
          match error.root with
          | None -> JSON_Null
          | Some { root_loc; _ } ->
            json_of_loc_with_context ~strip_root ~stdin_file ~offset_kind root_loc
        in
        [
          (* Unfortunately, Nuclide currently depends on this flag. Remove it in
           * the future? *)
            ("classic", JSON_Bool false);
          (* NOTE: `primaryLoc` is the location we want to show in an IDE! `rootLoc`
           * is another loc which Flow associates with some errors. We include it
           * for tools which are interested in using the location to enhance
           * their rendering. `primaryLoc` will always be inside `rootLoc`. *)
            ( "primaryLoc",
              json_of_loc_with_context ~strip_root ~stdin_file ~offset_kind primary_loc );
          ("rootLoc", root_loc);
          (* NOTE: This `messageMarkup` can be concatenated into a string when
           * implementing the LSP error output. *)
            ("messageMarkup", json_of_message_group_friendly message_group);
          (* NOTE: These `referenceLocs` can become `relatedLocations` when
           * implementing the LSP error output. *)
            ("referenceLocs", json_of_references ~strip_root ~stdin_file ~offset_kind references);
        ]))

  let json_of_error_props
      ~strip_root
      ~stdin_file
      ~version
      ~json_of_message
      ~severity
      ~offset_kind
      ?(suppression_locs = Loc_collections.LocSet.empty)
      (kind, trace, error) =
    Hh_json.(
      let kind_str =
        match kind with
        | ParseError -> "parse"
        (* We report this as a parse error even though it wasn't produced by the parser *)
        | PseudoParseError -> "parse"
        | InferError -> "infer"
        (* "InferWarning"s should still really be treated as errors. (The name is outdated.) *)
        | InferWarning _ -> "infer"
        | InternalError -> "internal"
        | DuplicateProviderError -> "duplicate provider"
        | RecursionLimitError -> "recursion limit exceeded"
        | LintError _ -> "lint"
      in
      let severity_str = output_string_of_severity severity in
      let suppressions =
        suppression_locs
        |> Loc_collections.LocSet.elements
        |> Base.List.map ~f:(fun loc ->
               let offset_table = get_offset_table_expensive ~stdin_file ~offset_kind loc in
               JSON_Object
                 [
                   ( "loc",
                     Reason.json_of_loc ~strip_root ~offset_table ~catch_offset_errors:true loc );
                 ])
      in
      let props =
        [
          ("kind", JSON_String kind_str);
          ("level", JSON_String severity_str);
          ("suppressions", JSON_Array suppressions);
        ]
      in
      props
      (* add the error type specific props *)
      @ (match version with
        | JsonV1 -> json_of_classic_error_props ~json_of_message (Friendly.to_classic error)
        | JsonV2 -> json_of_friendly_error_props ~strip_root ~stdin_file ~offset_kind error)
      @
      (* add trace if present *)
      match trace with
      | [] -> []
      | _ -> [("trace", JSON_Array (Base.List.map ~f:json_of_message trace))])

  let json_of_error_with_context
      ~strip_root ~stdin_file ~version ~severity ~offset_kind (error, suppression_locs) =
    let json_of_message = json_of_message_with_context ~strip_root ~stdin_file ~offset_kind in
    Hh_json.JSON_Object
      (json_of_error_props
         ~strip_root
         ~stdin_file
         ~version
         ~offset_kind
         ~json_of_message
         ~severity
         ~suppression_locs
         error)

  let json_of_errors_with_context
      ~strip_root
      ~stdin_file
      ~suppressed_errors
      ?(version = JsonV1)
      ~offset_kind
      ~errors
      ~warnings
      () =
    let f = json_of_error_with_context ~strip_root ~stdin_file ~version ~offset_kind in
    let obj_props_rev =
      []
      |> ConcreteLocPrintableErrorSet.fold
           (fun error acc -> f ~severity:Err (error, Loc_collections.LocSet.empty) :: acc)
           errors
      |> ConcreteLocPrintableErrorSet.fold
           (fun warn acc -> f ~severity:Warn (warn, Loc_collections.LocSet.empty) :: acc)
           warnings
    in
    (* We want these to show up as "suppressed error"s, not "suppressed off"s *)
    let obj_props_rev =
      List.fold_left
        (fun acc suppressed_error ->
          let suppressed_error =
            let (err, suppressions) = suppressed_error in
            (err, suppressions)
          in
          f ~severity:Err suppressed_error :: acc)
        obj_props_rev
        suppressed_errors
    in
    Hh_json.JSON_Array (List.rev obj_props_rev)

  (* This function has an unusual signature because the first part can be
     expensive -- specifically `json_of_errors_with_context` can take a while,
     and we would like to include the time spent in our profiling data.

     However, that profiling data is also included in the output. This function
     is designed to be partially applied, with the partial application
     performing the expensive work within a running profiling segment. The
     returned closure can be passed the finished profiling data. *)
  let full_status_json_of_errors
      ~strip_root
      ~suppressed_errors
      ?(version = JsonV1)
      ?(stdin_file = None)
      ~offset_kind
      ~errors
      ~warnings
      () =
    Hh_json.(
      let props =
        [
          ("flowVersion", JSON_String Flow_version.version);
          ( "jsonVersion",
            JSON_String
              (match version with
              | JsonV1 -> "1"
              | JsonV2 -> "2") );
          ( "errors",
            json_of_errors_with_context
              ~strip_root
              ~stdin_file
              ~suppressed_errors
              ~version
              ~offset_kind
              ~errors
              ~warnings
              () );
          ("passed", JSON_Bool (ConcreteLocPrintableErrorSet.is_empty errors));
        ]
      in
      (fun ~profiling_props -> JSON_Object (props @ profiling_props)))

  let format_errors
      ~out_channel
      ~strip_root
      ~suppressed_errors
      ~pretty
      ?version
      ?(stdin_file = None)
      ~offset_kind
      ~errors
      ~warnings
      () =
    Hh_json.(
      let get_json =
        full_status_json_of_errors
          ~strip_root
          ?version
          ~stdin_file
          ~suppressed_errors
          ~offset_kind
          ~errors
          ~warnings
          ()
      in
      fun ~profiling_props ->
        let res = get_json ~profiling_props in
        if pretty then
          output_string out_channel (json_to_multiline res)
        else
          json_to_output out_channel res;
        flush out_channel)

  let print_errors
      ~out_channel
      ~strip_root
      ~suppressed_errors
      ~pretty
      ?version
      ~offset_kind
      ?(stdin_file = None)
      ~errors
      ~warnings
      () =
    format_errors
      ~out_channel
      ~strip_root
      ~suppressed_errors
      ~pretty
      ?version
      ~offset_kind
      ~stdin_file
      ~errors
      ~warnings
      ()
      ~profiling_props:[]
end

(* for vim and emacs plugins *)
module Vim_emacs_output = struct
  let string_of_loc ~strip_root loc =
    Loc.(
      match loc.source with
      | None
      | Some File_key.Builtins ->
        ""
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
          spf "File \"%s\", line %d, characters %d-%d" file line start end_)

  let print_errors =
    let endline s =
      if s = "" then
        ""
      else
        s ^ "\n"
    in
    let to_pp_string ~strip_root prefix message =
      let (loc, msg) = to_pp message in
      let loc_str = string_of_loc ~strip_root loc in
      Printf.sprintf "%s%s%s" (endline loc_str) prefix (endline msg)
    in
    let classic_to_string ~strip_root prefix trace error =
      let { messages; _ } = error in
      let messages = append_trace_reasons messages trace in
      let buf = Buffer.create 50 in
      (match messages with
      | [] -> assert false
      | message1 :: rest_of_error ->
        Buffer.add_string buf (to_pp_string ~strip_root prefix message1);
        List.iter
          begin
            fun message -> Buffer.add_string buf (to_pp_string ~strip_root "" message)
          end
          rest_of_error);
      Buffer.contents buf
    in
    let to_string ~strip_root prefix ((_, trace, error) : Loc.t printable_error) : string =
      classic_to_string ~strip_root prefix trace (Friendly.to_classic error)
    in
    fun ~strip_root oc ~errors ~warnings () ->
      let sl =
        []
        |> ConcreteLocPrintableErrorSet.fold
             (fun err acc -> to_string ~strip_root "Error: " err :: acc)
             errors
        |> ConcreteLocPrintableErrorSet.fold
             (fun warn acc -> to_string ~strip_root "Warning: " warn :: acc)
             warnings
        |> List.sort String.compare
        |> ListUtils.uniq
      in
      List.iter
        begin
          fun s ->
          output_string oc s;
          output_string oc "\n"
        end
        sl;
      flush oc
end

module Lsp_output = struct
  type t = {
    loc: Loc.t;
    (* the file+range at which the message applies *)
    message: string;
    (* the diagnostic's message *)
    code: string;
    (* an error code *)
    relatedLocations: (Loc.t * string) list;
  }

  let lsp_of_error (error : Loc.t printable_error) : t =
    (* e.g. "Error about `code` in type Ref(`foo`)"                    *)
    (* will produce LSP message "Error about `code` in type `foo` [1]" *)
    (* and the LSP related location will have message "[1]: `foo`"      *)
    let (kind, _, friendly) = error in
    let (_, loc, group) =
      Friendly.message_group_of_error ~show_all_branches:false ~show_root:true friendly
    in
    let (references, group) = Friendly.extract_references group in
    let features = Friendly.message_of_group_message group in
    (* This is the accumulator function to build up the message and related locations... *)
    let f (message, relatedLocations) feature =
      match feature with
      | Friendly.Inline inlines ->
        let message = message ^ Friendly.string_of_message_inlines inlines in
        (message, relatedLocations)
      | Friendly.Reference (inlines, id) ->
        let ref_id = string_of_int id in
        let ref_text = Friendly.string_of_message_inlines inlines in
        let ref_loc = IMap.find id references in
        let ref_message = Printf.sprintf "[%s] %s" ref_id ref_text in
        let message = Printf.sprintf "%s %s [%s]" message ref_text ref_id in
        (message, (ref_loc, ref_message) :: relatedLocations)
    in
    let (message, relatedLocations) = List.fold_left f ("", []) features in
    {
      loc;
      message = String.trim message;
      code = string_of_kind kind;
      relatedLocations = List.rev relatedLocations;
    }
end
