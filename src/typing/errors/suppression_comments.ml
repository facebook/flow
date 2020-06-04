(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
    Suppression comments have the following syntax:

    <SUPPRESSOR> := $FlowIssue | $FlowFixMe | $FlowExpectedError

    //<SUPPRESSOR>[CODE]...
*)
open Utils_js

module CodeSet : Set.S with type elt = string = Set.Make (struct
  type t = string

  let compare = Pervasives.compare
end)

type applicable_codes =
  | All
  | Specific of CodeSet.t

let join_applicable_codes c1 c2 =
  match (c1, c2) with
  | (All, _)
  | (_, All) ->
    All
  | (Specific c1, Specific c2) -> Specific (CodeSet.union c1 c2)

let consume_token token str =
  let open Base in
  String.chop_prefix str ~prefix:token
  |> Option.map ~f:(mk_tuple_swapped true)
  |> Option.value ~default:(str, false)

(* Consumes any element of [tokens] from the front of [str].
   If there was no match at all, returns [x, chopped], where
   [x] is [str] with all elements of [tokens] removed from the
   front, and [chopped] is whether or not any tokens were removed *)
let consume_tokens =
  let ( >>= ) (x, b) f =
    let (x', b') = f x in
    (x', b || b')
  in
  let rec consume_tokens tokens (str, chopped) =
    let (str', chopped') =
      Base.List.fold_left ~f:(fun acc -> consume_token %> ( >>= ) acc) ~init:(str, false) tokens
    in
    if not chopped' then
      (str, chopped)
    else
      consume_tokens tokens (str', chopped')
  in
  (fun tokens str -> consume_tokens tokens (str, false))

let should_suppress comment =
  let (comment, is_suppressor) =
    consume_tokens [" "; "\n"; "\t"; "\r"; "*"] comment
    |> fst
    |> consume_tokens ["$FlowFixMe"; "$FlowIssue"; "$FlowExpectedError"]
  in
  if not is_suppressor then
    None
  else
    let (comment, has_preceding_spaces) = consume_tokens [" "; "\n"; "\t"; "\r"] comment in
    let (comment, has_code) = consume_token "[" comment in
    if not has_code then
      Some All
    else
      match Base.String.index comment ']' with
      | None -> Some All
      | Some 0 -> None (* $FlowFixMe[] is not a real code *)
      | Some index ->
        (* //$FlowFixMe [code] is invalid *)
        if has_preceding_spaces then
          None
        else
          Some (Specific (Base.String.prefix comment index |> CodeSet.singleton))
