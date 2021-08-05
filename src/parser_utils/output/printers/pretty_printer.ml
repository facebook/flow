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

type context = {
  mode: break_mode;
  ind: int;
}

let rec fits ~width ~mode nodes =
  if width < 0 then
    false
  else
    match nodes with
    | [] -> true
    | hd :: tl ->
      begin
        match hd with
        | Empty -> fits ~width ~mode tl
        | SourceLocation (_, hd) -> fits ~width ~mode (hd :: tl)
        | IfPretty (hd, _) -> fits ~width ~mode (hd :: tl)
        | IfBreak (if_, else_) ->
          let node =
            match mode with
            | Break -> if_
            | Flat -> else_
          in
          fits ~width ~mode (node :: tl)
        | Group items
        | Concat items ->
          fits ~width ~mode (items @ tl)
        | Indent node -> fits ~width ~mode (node :: tl)
        (* Respect forced breaks *)
        | Newline -> false
        | Sequence ({ break = Break_if_pretty; _ }, _) -> false
        | Sequence ({ break = _; inline = (before, _); indent = _ }, items) ->
          (* TODO: need to consider `after`. and indent? *)
          ((not before) && mode = Break) || fits ~width ~mode (items @ tl)
        | Identifier (_, x)
        | Atom x ->
          fits ~width:(width - String.length x) ~mode tl
      end

let print =
  let break_and_indent ind (w : writer) =
    let src = w.src |> Source.add_newline |> Source.add_space ind in
    { src; pos = ind }
    (* Reset indentation to our inset *)
  in
  let rec print_node (context : context) (w : writer) : layout_node -> writer = function
    | SourceLocation (loc, node) ->
      let src = Source.push_loc loc w.src in
      let w = print_node context { w with src } node in
      let src = Source.pop_loc w.src in
      { w with src }
    | Concat nodes -> List.fold_left (print_node context) w nodes
    | Newline -> break_and_indent context.ind w
    | Indent node -> print_node { context with ind = context.ind + 2 } w node
    | Sequence ({ break = Break_if_pretty; inline = (left, right); indent = extra_indent }, nodes)
      ->
      let indent = context.ind in
      let inner_indent = indent + extra_indent in
      let w =
        if not left then
          break_and_indent inner_indent w
        else
          w
      in
      let (w, _) =
        let inner_context = { ind = inner_indent; mode = Break } in
        List.fold_left
          (fun (w, i) node ->
            let w = print_node inner_context w node in
            if i > 0 then
              (break_and_indent inner_indent w, i - 1)
            else
              (w, 0))
          (w, List.length nodes - 1)
          nodes
      in
      if not right then
        break_and_indent indent w
      else
        w
    | Group nodes as layout ->
      let mode =
        if fits ~width:(max_width - w.pos) ~mode:Flat [layout] then
          Flat
        else
          Break
      in
      print_node { context with mode } w (Concat nodes)
    | Sequence (({ break = Break_if_needed; _ } as config), nodes) as layout ->
      let (mode, node) =
        if fits ~width:(max_width - w.pos) ~mode:Flat [layout] then
          (Flat, Concat nodes)
        else
          (Break, Sequence ({ config with break = Break_if_pretty }, nodes))
      in
      print_node { context with mode } w node
    | Atom s ->
      let src = Source.add_string s w.src in
      { src; pos = w.pos + String.length s }
    | Identifier (loc, s) ->
      let src = Source.add_identifier loc s w.src in
      { src; pos = w.pos + String.length s }
    | IfPretty (node, _) -> print_node context w node
    | IfBreak (on_break, otherwise) ->
      let node =
        match context.mode with
        | Break -> on_break
        | Flat -> otherwise
      in
      print_node context w node
    | Empty -> w
  in
  fun ~source_maps ?(skip_endline = false) node ->
    let w = { src = Source.create ~source_maps (); pos = 0 } in
    let { src; _ } = print_node { mode = Flat; ind = 0 } w node in
    if skip_endline then
      src
    else
      Source.add_newline src
