(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast_builder

module Layout_builder = struct
  open Layout

  let expression ?(expr_ctxt=Js_layout_generator.normal_context) ast =
    Js_layout_generator.expression ~ctxt:expr_ctxt ast

  let empty = Empty

  let loc ?(loc=Loc.none) node =
    SourceLocation (loc, node)

  let program_loc loc =
    { loc with Loc.start = { Loc.line = 1; column = 0; offset = 0; }}

  let program ?loc:(prog_loc=Loc.none) node =
    loc ~loc:(program_loc prog_loc) node

  let sequence ~break ?(inline=(false, false)) ?(indent=2) items =
    Sequence ({break; inline; indent}, items)

  let fused items =
    Concat items

  let fused_vertically
    ?(indent=0)
    ?(inline=(false, false))
    items =
    Sequence ({ break=Break_if_pretty; indent; inline }, items)

  let id ?(loc=Loc.none) str =
    Identifier (loc, str)

  let atom str =
    Atom str

  let pretty_space =
    IfPretty (Atom " ", Empty)

  let flat_pretty_space =
    IfBreak (Empty, IfPretty (Atom " ", Empty))

  let wrap_in_parens x =
    fused [
      atom "("; sequence ~break:Layout.Break_if_needed [x]; atom ")";
    ]

  type printer_pos = Word of string | Phrase of string

  let printer =
    let spf = Printf.sprintf in
    let is_program_loc loc =
      let open Loc in
      match loc with
      | { source = None;
          start = { line = 1; column = 0; offset = 0 };
          _end = { line = 0; column = 0; offset = 0 }
        } -> true
      | _ -> false
    in
    let string_of_loc loc =
      let open Loc in
      spf "{Loc.none with start={Loc.line=%d; column=%d; offset=%d}; _end={Loc.line=%d; column=%d; offset=%d}}"
        loc.start.line loc.start.column loc.start.offset
        loc._end.line loc._end.column loc._end.offset
    in
    let string_of_when_to_break = function
    | Break_if_needed -> "Layout.Break_if_needed"
    | Break_if_pretty -> "Layout.Break_if_pretty"
    | Break_always -> "Layout.Break_always"
    in
    let word x = Word x in
    let phrase fmt = Printf.ksprintf (fun str -> Phrase str) fmt in
    let rec top ~i = function
    | SourceLocation (loc, child) ->
      if is_program_loc loc then
        phrase "program %s" (helper ~i child)
      else
        let loc = if loc = Loc.none then "" else spf " ~loc:%s" (string_of_loc loc) in
        phrase "loc%s %s" loc (helper ~i child)

    | Concat items ->
      phrase "fused %s" (list ~i items)

    | Sequence ({ break = Break_if_pretty; inline = (false, false); indent = 0 }, items) ->
      phrase "fused_vertically %s" (list ~i items)

    | Sequence (
        { break = Break_if_needed; inline = (true, true); indent = 0 },
        [Concat [
          Atom "(";
          Sequence ({ break = Break_if_needed; inline = (false, false); indent = 2}, [x]);
          Atom ")";
        ]]
      ) ->
      phrase "wrap_in_parens %s" (helper ~i x)

    | Sequence ({ break; inline; indent; }, items) ->
      let break = spf " ~break:%s" (string_of_when_to_break break) in
      let inline = match inline with
        | false, false -> ""
        | before, after -> spf " ~inline:(%b, %b)" before after
      in
      let indent = if indent = 2 then "" else spf " ~indent:%d" indent in
      phrase "sequence%s%s%s %s" break inline indent (list ~i items)

    | Atom str ->
      phrase "atom %S" str

    | Identifier (loc, str) ->
      let loc = if loc = Loc.none then "" else spf " ~loc:%s" (string_of_loc loc) in
      phrase "id%s %S" loc str

    | IfPretty (Atom " ", Empty) -> word "pretty_space"

    | IfPretty (left, right) ->
      phrase "Layout.IfPretty (%s, %s)" (helper ~i left) (helper ~i right)

    | IfBreak (Empty, IfPretty (Atom " ", Empty)) -> word "flat_pretty_space"

    | IfBreak (left, right) ->
      phrase "Layout.IfBreak (%s, %s)" (helper ~i left) (helper ~i right)

    | Empty ->
      word "empty"
    and list ~i nodes =
      let indent = String.make (i * 2) ' ' in
      let f node =
        match top ~i:(i + 1) node with
        | Word str
        | Phrase str -> spf "  %s%s;" indent str
      in
      let str = nodes
      |> List.map f
      |> String.concat "\n"
      in
      spf "[\n%s\n%s]" str indent
    and helper ~i node =
      match top ~i node with
      | Word str -> str
      | Phrase str -> spf "(%s)" str
    in
    fun node ->
      spf "L.%s" (helper ~i:0 node)
end

module Layout_matcher = struct
  open Layout

  let return x = Ok x
  let (>>=) x f = match x with
    | Error _ as x -> x
    | Ok x -> f x

  let empty = function
    | Empty -> Ok ()
    | x -> Error x

  let loc = function
    | SourceLocation (_, x) -> Ok x
    | x -> Error x

  let fused = function
    | Concat x -> Ok x
    | x -> Error x

  let nth_fused n = function
    | Concat xs as layout -> (try Ok (List.nth xs n) with Failure _ -> Error layout)
    | layout -> Error layout

  let atom = function
    | Atom x -> Ok x
    | x -> Error x

  (* TODO: support matching break, inline, indent *)
  let sequence = function
    | Sequence (_, x) -> Ok x
    | x -> Error x

  let nth_sequence n = function
    | Sequence (_, xs) as layout -> (try Ok (List.nth xs n) with Failure _ -> Error layout)
    | layout -> Error layout

  let pretty_space = function
    | IfPretty (Atom " ", Empty) -> Ok ()
    | x -> Error x


  (* higher level helpers *)

  let body_of_function_declaration ast =
    return (Js_layout_generator.statement ast)
    >>= loc
    >>= (nth_fused 3) (* skip function name, space, params *)
    >>= loc
    >>= (nth_sequence 0)
    >>= (nth_fused 1) (* skip { to get body *)

end

let assert_layout ~ctxt ?msg expected actual =
  assert_equal ~ctxt ?msg ~printer:Layout_builder.printer expected actual

let assert_layout_result ~ctxt ?msg expected actual =
  match actual with
  | Ok layout -> assert_layout ~ctxt ?msg expected layout
  | Error layout ->
      assert_equal ~ctxt
        ~msg:(Printf.sprintf "Unable to decode %s" (Layout_builder.printer layout))
        true false

let assert_layout_of_expression
    ~ctxt ?msg ?(expr_ctxt=Js_layout_generator.normal_context)
    expected ast =
  let actual = Js_layout_generator.expression ~ctxt:expr_ctxt ast in
  assert_layout ~ctxt ?msg expected actual

let assert_layout_of_statement ~ctxt ?msg expected ast =
  let actual = Js_layout_generator.statement ast in
  assert_layout ~ctxt ?msg expected actual

let assert_layout_of_statement_string ~ctxt ?msg expected str =
  let ast = statement_of_string str in
  assert_layout_of_statement ~ctxt ?msg expected ast
