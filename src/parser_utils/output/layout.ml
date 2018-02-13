(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type layout_node =
  (* A layout with location information *)
  | SourceLocation of Loc.t * layout_node
  (* A list of nodes that don't break *)
  | Concat of layout_node list
  (* Join elements, allow for breaking over over lines *)
  | Sequence of list_config * (layout_node list)
  (* Print a string *)
  | Atom of string
  (* Print an identifier, useful for source map name mappings *)
  | Identifier of Loc.t * string
  (* Only print left for pretty output else right *)
  | IfPretty of layout_node * layout_node
  (* Print left if break else right *)
  | IfBreak of layout_node * layout_node
  (* Print nothing *)
  | Empty

and when_to_break =
  | Break_if_needed
  | Break_if_pretty
  | Break_always (* even in compact mode! *)

and list_config = {
  break: when_to_break;
  (* Whether a break should be placed at the start and end of a sequence when
     splitting over lines *)
  inline: bool * bool;
  indent: int;
}

let seq = {
  break = Break_if_needed;
  inline = (false, false);
  indent = 2;
}

(* Whitespace utils *)
let space = Atom " "
let pretty_space = IfPretty (space, Empty)
let ugly_space = IfPretty (Empty, space)
let flat_space = IfBreak (Empty, space)
let flat_pretty_space = IfBreak (Empty, pretty_space)
let flat_ugly_space = IfBreak (Empty, ugly_space)

let if_pretty if_ else_ =
  if if_ = Empty && else_ = Empty then Empty else IfPretty (if_, else_)

let if_break if_ else_ =
  if if_ = Empty && else_ = Empty then Empty else IfBreak (if_, else_)

(* Fuse a list of items together, no spaces or breaks will be inserted *)
let fuse items =
  let items = List.filter (function Empty -> false | _ -> true) items in
  match items with
  | [] -> Empty
  | item::[] -> item
  | _ -> Concat items

(* Fuse a list of items to align vertically *)
let fuse_vertically
  ?(indent=0)
  ?(inline=(false, false))
  items =
  Sequence ({ break=Break_if_pretty; indent; inline }, items)

let fuse_list =
  let rec helper ~sep acc = function
  | [] -> fuse (List.rev acc)
  | item::[] -> helper ~sep (item::acc) []
  | item::items -> helper ~sep (pretty_space::sep::item::acc) items
  in
  fun ?(sep=Empty) ?(wrap=(Empty, Empty)) items ->
    fuse [
      fst wrap;
      helper ~sep [] items;
      snd wrap;
    ]

(* All purpose list *)
let list
  ?(break=Break_if_needed)
  ?(wrap=(Empty, Empty))
  ?(sep=Empty)
  ?(trailing=true)
  ?(inline=(false, false))
  ?(indent=2)
  items =
  let add_seperator is_last item =
    fuse [
      item;
      if_break
        (
          if is_last && trailing then if_pretty sep Empty
          else if not is_last then sep
          else Empty
        )
        (if is_last then Empty else fuse [sep; pretty_space])
    ] in
  let items_count = List.length items - 1 in
  let layout_items = fuse [
    fst wrap;
    Sequence (
      { break; inline; indent },
      List.mapi (fun i item -> add_seperator (i = items_count) item) items
    );
    snd wrap;
  ] in
  (* Wrap items in additional sequence so `IfBreak`s within wrap are
     not triggered by adjacent lists. *)
  Sequence (
    { break=Break_if_needed; inline=(true, true); indent=0 },
    [layout_items]
  )

let fuse_with_space =
  let is_punctuator = function
  | '{' | '(' | ')' | '[' | ']' | '.'
  | ';' | ',' | '<' | '>' | '=' | '!'
  | '+' | '-' | '*' | '%' | '^' | '&'
  | '~' | '|' | '?' | ':' | '/' | '"'
  | '\'' -> true
  | _ -> false
  in
  let rec ugly_char ~mode = function
    | Atom str
    | Identifier (_, str) ->
        if str = "" then None else
        Some (if mode = `First then str.[0] else str.[String.length str - 1])
    | Empty -> None
    | SourceLocation (_, node)
    | IfPretty (_, node)
    | IfBreak (_, node) -> ugly_char ~mode node
    | Concat nodes
    | Sequence (_, nodes) ->
        let nodes = if mode = `First then nodes else List.rev nodes in
        List.fold_left (fun acc node ->
          match acc with Some _ -> acc | None -> ugly_char ~mode node
        ) None nodes
  in
  let opt_punctuator = function
  | Some x when is_punctuator x -> true
  | _ -> false
  in
  let rec helper acc = function
  | a::b::rest ->
    let prev = ugly_char ~mode:`Last a |> opt_punctuator in
    let next = ugly_char ~mode:`First b |> opt_punctuator in
    let sp = if prev || next then pretty_space else space in
    helper (sp::a::acc) (b::rest)
  | a::[] ->
    helper (a::acc) []
  | [] ->
    List.rev acc
  in
  fun nodes -> fuse (helper [] nodes)

let rec debug_string_of_layout (node: layout_node) =
  let spf = Printf.sprintf in
  let debug_string_of_loc loc =
    let open Loc in
    spf "%d:%d-%d:%d"
      loc.start.line loc.start.column
      loc._end.line loc._end.column
  in
  let debug_string_of_when_to_break = function
  | Break_if_needed -> "Break_if_needed"
  | Break_if_pretty -> "Break_if_pretty"
  | Break_always -> "Break_always"
  in
  match node with
  | SourceLocation (loc, child) ->
    spf "SourceLocation (%s, %s)"
      (debug_string_of_loc loc)
      (debug_string_of_layout child)

  | Concat items ->
    let items =
      items
      |> List.map debug_string_of_layout
      |> String.concat "; "
    in
    spf "Concat [%s]" items

  | Sequence ({ break; inline=(inline_before, inline_after); indent; }, node_list) ->
    let config = spf
      "{break=%s; inline=(%b, %b); indent=%d}"
      (debug_string_of_when_to_break break)
      inline_before inline_after
      indent
    in
    let nodes =
      node_list
      |> List.map debug_string_of_layout
      |> String.concat "; "
    in
    spf "Sequence (%s, [%s])" config nodes

  | Atom str ->
    spf "Atom %S" str

  | Identifier (loc, str) ->
    spf "Identifier (%s, %S)" (debug_string_of_loc loc) str

  | IfPretty (left, right) ->
    spf "IfPretty (%s, %s)"
      (debug_string_of_layout left)
      (debug_string_of_layout right)

  | IfBreak (left, right) ->
    spf "IfBreak (%s, %s)"
      (debug_string_of_layout left)
      (debug_string_of_layout right)

  | Empty ->
    spf "Empty"
