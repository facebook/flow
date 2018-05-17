(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Layout

(* TODO: Make this configurable *)
let max_width = 80
type break_mode = | Break | Flat
type writer = { src: Source.t; pos: int }
type context = { mode: break_mode; ind: int }

let rec fits ~width ~context nodes =
  if width < 0 then false else
  match nodes with
  | [] -> true
  | next::rest -> begin
    match next with
    | Empty -> fits ~width ~context rest
    | SourceLocation (_, next) -> fits ~width ~context (next::rest)
    | IfPretty (next, _) -> fits ~width ~context (next::rest)
    | IfBreak (if_, else_) ->
        let nodes = match context.mode with
        | Break -> if_::rest
        | Flat -> else_::rest
        in
        fits ~width ~context nodes
    | Concat items -> fits ~width ~context (items @ rest)
    (* Respect forced breaks *)
    | Sequence ({ break = Break_if_pretty | Break_always; _ }, _) -> false
    | Sequence ({ break = _; inline = (before, _); indent = _ }, items) ->
      (* TODO: need to consider `after`. and indent? *)
      (not before && context.mode = Break) || (fits ~width ~context (items @ rest))
    | Identifier (_, x)
    | Atom x -> fits ~width:(width - String.length x) ~context rest
  end

let print =
  let break_and_indent (c: context) (w: writer) =
    let src = w.src
    |> Source.add_newline
    |> Source.add_space c.ind
    in
    { src; pos=c.ind } (* Reset indentation to our inset *)
  in

  let rec print_node (context: context) (w: writer) : layout_node -> writer =
    function
    | SourceLocation (loc, node) ->
      let src = Source.push_loc loc w.src in
      let w = print_node context {w with src} node in
      let src = Source.pop_loc w.src in
      { w with src }
    | Concat nodes -> List.fold_left (print_node context) w nodes
    | Sequence ({ break=Break_if_pretty; inline=(left, right); indent }, nodes)
    | Sequence ({ break=Break_always; inline=(left, right); indent }, nodes) ->
      let inner_context = { ind = context.ind + indent; mode = Break } in
      let w = if not left then break_and_indent inner_context w else w in
      let (w, _) = List.fold_left
        (fun (w, i) node ->
          let w = print_node inner_context w node in
          if i > 0 then (break_and_indent inner_context w, i - 1)
          else (w, 0)
        )
        (w, List.length nodes - 1)
        nodes in
      if not right then break_and_indent context w else w
    | Sequence ({ break=Break_if_needed; inline; indent }, nodes) as layout -> begin
      let flat_context = {context with mode = Flat} in
      if fits ~width:(max_width - w.pos) ~context:flat_context [layout] then (
        print_node flat_context w (Concat nodes)
      ) else (
        let break_context = {context with mode=Break} in
        print_node break_context w
          (Sequence ({ break=Break_if_pretty; inline; indent }, nodes))
      )
    end
    | Atom s ->
      let src = Source.add_string s w.src in
      { src; pos=w.pos + String.length s }
    | Identifier (loc, s) ->
      let src = Source.add_identifier loc s w.src in
      { src; pos=w.pos + String.length s }
    | IfPretty (node, _) -> print_node context w node
    | IfBreak (on_break, otherwise) -> begin
      match context.mode with
      | Break -> print_node context w on_break
      | Flat -> print_node context w otherwise
      end
    | Empty -> w
  in

  fun ~source_maps node ->
    let { src; _ } = print_node
      { mode=Flat; ind = 0 }
      { src=Source.create ~source_maps (); pos=0 }
      node
    in
    Source.add_newline src
