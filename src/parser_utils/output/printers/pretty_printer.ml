(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout

(* TODO: Make this configurable *)
let max_width = 80

type break_mode =
  | Break
  | Flat

type writer = {
  src: Source.t;
  pos: int;
}

type item = break_mode * layout_node

(** [fits ~width ~rest items] determines whether [items @ rest] can be printed in [width]
    characters. [items] and [rest] are separate as an optimization to avoid list concats. *)
let rec fits ~width ~rest (items : item list) =
  if width < 0 then
    false
  else
    match (items, rest) with
    | ([], []) -> true
    | ([], _) -> fits ~width ~rest:[] rest
    | ((mode, hd) :: tl, _) ->
      begin
        match hd with
        | Empty -> fits ~width ~rest tl
        | SourceLocation (_, hd) -> fits ~width ~rest ((mode, hd) :: tl)
        | IfPretty (hd, _) -> fits ~width ~rest ((mode, hd) :: tl)
        | IfBreak (if_, else_) ->
          let node =
            match mode with
            | Break -> if_
            | Flat -> else_
          in
          fits ~width ~rest ((mode, node) :: tl)
        | Group items
        | Concat items ->
          let rev_items = Base.List.rev_map ~f:(fun item -> (mode, item)) items in
          fits ~width ~rest (Base.List.rev_append rev_items tl)
        | Indent node -> fits ~width ~rest ((mode, node) :: tl)
        (* Respect forced breaks *)
        | Newline ->
          (match mode with
          | Break -> true
          | Flat -> false)
        | Sequence ({ break = Break_if_pretty; _ }, _) -> false
        | Sequence ({ break = _; inline = (before, _); indent = _ }, nodes) ->
          (* TODO: need to consider `after`. and indent? *)
          ((not before) && mode = Break) || fits ~width ~rest ((mode, Concat nodes) :: tl)
        | Identifier (_, x)
        | Atom x ->
          fits ~width:(width - String.length x) ~rest tl
      end

let print =
  let break_and_indent ind (w : writer) =
    let src = w.src |> Source.add_newline |> Source.add_space ind in
    { src; pos = ind }
    (* Reset indentation to our inset *)
  in
  (* folds a non-empty list *)
  let rec fold ~f w nodes =
    match nodes with
    | node :: rest ->
      let w = f w rest node in
      fold ~f w rest
    | [] -> w
  in
  let rec print_node (indent : int) (w : writer) (rest : item list) ((mode, node) : item) : writer =
    match node with
    | SourceLocation (loc, node) ->
      let src = Source.push_loc loc w.src in
      let w = print_node indent { w with src } rest (mode, node) in
      let src = Source.pop_loc w.src in
      { w with src }
    | Concat nodes ->
      let items = Base.List.map ~f:(fun node -> (mode, node)) nodes in
      fold ~f:(print_node indent) w items
    | Newline -> break_and_indent indent w
    | Indent node -> print_node (indent + 2) w rest (mode, node)
    | Sequence ({ break = Break_if_pretty; inline = (left, right); indent = extra_indent }, nodes)
      ->
      let inner_indent = indent + extra_indent in
      let w =
        if not left then
          break_and_indent inner_indent w
        else
          w
      in
      let (w, _) =
        fold
          ~f:(fun (w, i) rest node ->
            let w = print_node inner_indent w rest node in
            if i > 0 then
              (break_and_indent inner_indent w, i - 1)
            else
              (w, 0))
          (w, List.length nodes - 1)
          (Base.List.map ~f:(fun node -> (mode, node)) nodes)
      in
      if not right then
        break_and_indent indent w
      else
        w
    | Group nodes as layout ->
      let mode =
        if fits ~width:(max_width - w.pos) ~rest [(Flat, layout)] then
          Flat
        else
          Break
      in
      print_node indent w rest (mode, Concat nodes)
    | Sequence (({ break = Break_if_needed; _ } as config), nodes) as layout ->
      let item =
        if fits ~width:(max_width - w.pos) ~rest [(Flat, layout)] then
          (Flat, Concat nodes)
        else
          (Break, Sequence ({ config with break = Break_if_pretty }, nodes))
      in
      print_node indent w rest item
    | Atom s ->
      let src = Source.add_string s w.src in
      { src; pos = w.pos + String.length s }
    | Identifier (loc, s) ->
      let src = Source.add_identifier loc s w.src in
      { src; pos = w.pos + String.length s }
    | IfPretty (node, _) -> print_node indent w rest (mode, node)
    | IfBreak (on_break, otherwise) ->
      let node =
        match mode with
        | Break -> on_break
        | Flat -> otherwise
      in
      print_node indent w rest (mode, node)
    | Empty -> w
  in
  fun ~source_maps ?(skip_endline = false) node ->
    let w = { src = Source.create ~source_maps (); pos = 0 } in
    let { src; _ } = print_node 0 w [] (Break, node) in
    if skip_endline then
      src
    else
      Source.add_newline src
