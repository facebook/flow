(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type layout_node =
  | SourceLocation of Loc.t * layout_node  (** A layout with location information *)
  | Concat of layout_node list  (** A list of nodes that don't break *)
  | Group of layout_node list  (** A list of nodes to try to fit on one line *)
  | Sequence of list_config * layout_node list
      (** Join elements, allow for breaking over over lines *)
  | Indent of layout_node  (** Increase the indentation *)
  | Newline  (** Force a line break *)
  | Atom of string  (** Print a string *)
  | Identifier of Loc.t * string  (** Print an identifier, useful for source map name mappings *)
  | IfPretty of layout_node * layout_node  (** Only print left for pretty output else right *)
  | IfBreak of layout_node * layout_node  (** Print left if break else right *)
  | Empty  (** Print nothing *)

and when_to_break =
  | Break_if_needed
  | Break_if_pretty

and list_config = {
  break: when_to_break;
  inline: bool * bool;
      (** Whether a break should be placed at the start and end of a sequence when
          splitting over lines *)
  indent: int;
}

let seq = { break = Break_if_needed; inline = (false, false); indent = 2 }

(* Whitespace utils *)
let space = Atom " "

let pretty_space = IfPretty (space, Empty)

let ugly_space = IfPretty (Empty, space)

let flat_ugly_space = IfBreak (Empty, ugly_space)

let hardline = Newline

(** Force a line break (`\n`) in pretty mode *)
let pretty_hardline = IfPretty (Newline, Empty)

(** Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise a space *)
let line = IfBreak (Newline, space)

(** Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise a pretty space *)
let pretty_line = IfBreak (Newline, pretty_space)

(** Inserts a line break (`\n`) if the code doesn't fit on one line, otherwise nothing *)
let softline = IfBreak (Newline, Empty)

let if_pretty if_ else_ =
  if if_ = Empty && else_ = Empty then
    Empty
  else
    IfPretty (if_, else_)

let if_break if_ else_ =
  if if_ = Empty && else_ = Empty then
    Empty
  else
    IfBreak (if_, else_)

let group items =
  let items =
    List.rev
      (List.fold_left
         (fun acc -> function
           | Empty -> acc
           | Concat more -> List.rev_append more acc
           | item -> item :: acc)
         []
         items
      )
  in
  match items with
  | [(Group _ as hd)] -> hd
  | _ -> Group items

(** Fuse a list of items together, no spaces or breaks will be inserted *)
let fuse items =
  let items =
    List.rev
      (List.fold_left
         (fun acc -> function
           | Empty -> acc
           | Concat more -> List.rev_append more acc
           | item -> item :: acc)
         []
         items
      )
  in
  match items with
  | [] -> Empty
  | [item] -> item
  | _ -> Concat items

let join sep nodes =
  let rec helper acc = function
    | [] -> List.rev acc
    | hd :: tl ->
      let acc =
        if acc = [] then
          [hd]
        else
          hd :: sep :: acc
      in
      helper acc tl
  in
  fuse (helper [] nodes)

let fuse_list =
  let rec helper ~sep acc = function
    | [] -> fuse (List.rev acc)
    | [item] -> helper ~sep (item :: acc) []
    | item :: items -> helper ~sep (pretty_space :: sep :: item :: acc) items
  in
  fun ?(sep = Empty) ?(wrap = (Empty, Empty)) items ->
    fuse [fst wrap; helper ~sep [] items; snd wrap]

let wrap_and_indent ?break (before, after) items =
  let break =
    match break with
    | Some break -> break
    | None -> softline
  in
  let layout =
    if items = [] then
      Empty
    else
      fuse [Indent (fuse (break :: items)); break]
  in
  fuse [before; layout; after]

let new_list
    ?(wrap = (Empty, Empty)) ?(sep = Empty) ?(wrap_spaces = false) ?(trailing_sep = true) items =
  let items_layout =
    if items = [] then
      items
    else
      [
        join (fuse [sep; pretty_line]) items;
        ( if trailing_sep then
          if_break (Atom ",") Empty
        else
          Empty
        );
      ]
  in
  let break =
    if wrap_spaces then
      Some pretty_line
    else
      None
  in
  wrap_and_indent ?break wrap items_layout

(** All purpose list *)
let list
    ?(break = Break_if_needed)
    ?(wrap = (Empty, Empty))
    ?(sep = Empty)
    ?(trailing = true)
    ?(inline = (false, false))
    ?(indent = 2)
    items =
  let add_seperator is_last item =
    fuse
      [
        item;
        if_break
          ( if is_last && trailing then
            if_pretty sep Empty
          else if not is_last then
            sep
          else
            Empty
          )
          ( if is_last then
            Empty
          else
            fuse [sep; pretty_space]
          );
      ]
  in
  let items_count = List.length items - 1 in
  let layout_items =
    fuse
      [
        fst wrap;
        Sequence
          ( { break; inline; indent },
            List.mapi (fun i item -> add_seperator (i = items_count) item) items
          );
        snd wrap;
      ]
  in
  (* Wrap items in additional sequence so `IfBreak`s within wrap are
     not triggered by adjacent lists. *)
  Sequence ({ break = Break_if_needed; inline = (true, true); indent = 0 }, [layout_items])

(** Takes a list of layout nodes and intersperses spaces: a `space` if a space is necessary
    to separate two tokens, or a `pretty_space` if it's only needed for aesthetics. Generally a
    space is required, except if the last char of one node or the first char of the next node is
    a punctuator, then spaces are only for aesthetics (e.g. `new Foo` vs `new(Foo)`) *)
let fuse_with_space =
  let is_punctuator = function
    | '{'
    | '('
    | ')'
    | '['
    | ']'
    | '.'
    | ';'
    | ','
    | '<'
    | '>'
    | '='
    | '!'
    | '+'
    | '-'
    | '*'
    | '%'
    | '^'
    | '&'
    | '~'
    | '|'
    | '?'
    | ':'
    | '/'
    | '"'
    | '\'' ->
      true
    | _ -> false
  in
  let rec ugly_char ~mode = function
    | Atom str
    | Identifier (_, str) ->
      if str = "" then
        None
      else
        Some
          ( if mode = `First then
            str.[0]
          else
            str.[String.length str - 1]
          )
    | Empty -> None
    | Indent node -> ugly_char ~mode node
    | Newline -> None
    | SourceLocation (_, node)
    | IfPretty (_, node)
    | IfBreak (_, node) ->
      ugly_char ~mode node
    | Concat nodes
    | Group nodes
    | Sequence (_, nodes) ->
      let nodes =
        if mode = `First then
          nodes
        else
          List.rev nodes
      in
      List.fold_left
        (fun acc node ->
          match acc with
          | Some _ -> acc
          | None -> ugly_char ~mode node)
        None
        nodes
  in
  let opt_punctuator = function
    | Some x when is_punctuator x -> true
    | _ -> false
  in
  let rec helper acc = function
    | Empty :: rest -> helper acc rest
    | a :: Empty :: rest -> helper acc (a :: rest)
    | a :: b :: rest ->
      let prev = ugly_char ~mode:`Last a |> opt_punctuator in
      let next = ugly_char ~mode:`First b |> opt_punctuator in
      let sp =
        if prev || next then
          pretty_space
        else
          space
      in
      helper (sp :: a :: acc) (b :: rest)
    | [a] -> helper (a :: acc) []
    | [] -> List.rev acc
  in
  (fun nodes -> fuse (helper [] nodes))

module Debug : sig
  val string_of_layout : layout_node -> string

  val layout_of_layout : layout_node -> layout_node
end = struct
  let spf = Printf.sprintf

  let debug_string_of_loc loc =
    Loc.(spf "%d:%d-%d:%d" loc.start.line loc.start.column loc._end.line loc._end.column)

  let debug_string_of_when_to_break = function
    | Break_if_needed -> "Break_if_needed"
    | Break_if_pretty -> "Break_if_pretty"

  let rec string_of_layout = function
    | SourceLocation (loc, child) ->
      spf "SourceLocation (%s, %s)" (debug_string_of_loc loc) (string_of_layout child)
    | Concat items ->
      let items = items |> Base.List.map ~f:string_of_layout |> String.concat "; " in
      spf "Concat [%s]" items
    | Group items ->
      let items = items |> Base.List.map ~f:string_of_layout |> String.concat "; " in
      spf "Group [%s]" items
    | Sequence ({ break; inline = (inline_before, inline_after); indent }, node_list) ->
      let config =
        spf
          "{break=%s; inline=(%b, %b); indent=%d}"
          (debug_string_of_when_to_break break)
          inline_before
          inline_after
          indent
      in
      let nodes = node_list |> Base.List.map ~f:string_of_layout |> String.concat "; " in
      spf "Sequence (%s, [%s])" config nodes
    | Atom str -> spf "Atom %S" str
    | Identifier (loc, str) -> spf "Identifier (%s, %S)" (debug_string_of_loc loc) str
    | IfPretty (left, right) ->
      spf "IfPretty (%s, %s)" (string_of_layout left) (string_of_layout right)
    | IfBreak (left, right) ->
      spf "IfBreak (%s, %s)" (string_of_layout left) (string_of_layout right)
    | Indent node -> spf "Indent (%s)" (string_of_layout node)
    | Newline -> "Newline"
    | Empty -> "Empty"

  let rec layout_of_layout = function
    | SourceLocation (loc, child) ->
      Concat
        [
          Atom "SourceLocation";
          pretty_space;
          list
            ~wrap:(Atom "(", Atom ")")
            ~sep:(Atom ",")
            [Atom (debug_string_of_loc loc); layout_of_layout child];
        ]
    | Concat items ->
      Concat
        [
          Atom "Concat";
          pretty_space;
          list ~wrap:(Atom "[", Atom "]") ~sep:(Atom ";") (Base.List.map ~f:layout_of_layout items);
        ]
    | Group items ->
      Concat
        [
          Atom "Group";
          pretty_space;
          list ~wrap:(Atom "[", Atom "]") ~sep:(Atom ";") (Base.List.map ~f:layout_of_layout items);
        ]
    | Sequence ({ break; inline = (inline_before, inline_after); indent }, node_list) ->
      let config =
        list
          ~wrap:(Atom "{", Atom "}")
          ~sep:(Atom ";")
          [
            Atom (spf "break=%s" (debug_string_of_when_to_break break));
            Atom (spf "inline=(%b, %b)" inline_before inline_after);
            Atom (spf "indent=%d" indent);
          ]
      in
      let nodes =
        list ~wrap:(Atom "[", Atom "]") ~sep:(Atom ";") (Base.List.map ~f:layout_of_layout node_list)
      in
      Concat
        [
          Atom "Sequence";
          pretty_space;
          list ~wrap:(Atom "(", Atom ")") ~sep:(Atom ",") [config; nodes];
        ]
    | Atom str -> Atom (spf "Atom %S" str)
    | Identifier (loc, str) -> Atom (spf "Identifier (%s, %S)" (debug_string_of_loc loc) str)
    | IfPretty (left, right) ->
      Concat
        [
          Atom "IfPretty";
          pretty_space;
          list
            ~wrap:(Atom "(", Atom ")")
            ~sep:(Atom ",")
            [layout_of_layout left; layout_of_layout right];
        ]
    | IfBreak (left, right) ->
      Concat
        [
          Atom "IfBreak";
          pretty_space;
          list
            ~wrap:(Atom "(", Atom ")")
            ~sep:(Atom ",")
            [layout_of_layout left; layout_of_layout right];
        ]
    | Indent child ->
      Concat
        [
          Atom "Indent";
          pretty_space;
          list ~wrap:(Atom "(", Atom ")") ~sep:(Atom ",") [layout_of_layout child];
        ]
    | Newline -> Atom "Newline"
    | Empty -> Atom "Empty"
end
