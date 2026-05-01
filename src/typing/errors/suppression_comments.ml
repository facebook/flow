(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(*
   Suppression comments have the following syntax:

   <SUPPRESSOR> := $FlowIssue | $FlowFixMe | $FlowExpectedError | $FlowIgnore

   //<SUPPRESSOR>[CODE]...
*)
open Utils_js
open Loc_collections

module CodeWithLocOrd = struct
  type t = string * Loc.t

  (* Locs are just metadata here, should not affect behavior *)
  let compare (c1, _) (c2, _) = Base.String.compare c1 c2
end

module CodeSet : Flow_set.S with type elt = string * Loc.t = Flow_set.Make (CodeWithLocOrd)

module CodeMap : Flow_map.S with type key = string * Loc.t = Flow_map.Make (CodeWithLocOrd)

type applicable_codes =
  | Specific of CodeSet.t
  | All of {
      locs: LocSet.t;
      warn_unused: bool;
    }

let locs_of_applicable_codes = function
  | Specific codes -> CodeSet.elements codes |> List.map snd
  | All { locs; warn_unused = true } -> LocSet.elements locs
  | All { warn_unused = false; _ } -> []

let join_applicable_codes c1 c2 =
  match (c1, c2) with
  | (Specific c1, Specific c2) -> Specific (CodeSet.union c1 c2)
  | (All { locs = l1; warn_unused = w1 }, All { locs = l2; warn_unused = w2 }) ->
    All { locs = LocSet.union l1 l2; warn_unused = w1 && w2 }
  | (All _, Specific _) -> c1
  | (Specific _, All _) -> c2

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

let is_valid_code_char c =
  let ascii = Char.code c in
  ascii = 45 (* - *) || (ascii >= 97 && ascii <= 122)

(* lowercase letters*)

type bad_suppression_kind =
  | MissingCode
  | MalformedCode

(* After matching a TypeScript directive prefix, ensure the next character is
   not part of the directive identifier — otherwise the prefix isn't really a
   directive (e.g. `@ts-ignoree`). Anything that can't continue a `[a-zA-Z0-9_-]`
   identifier is a valid boundary, which permits the common
   `// @ts-ignore: reason` / `// @ts-expect-error: reason` description form. *)
let ts_directive_boundary remainder =
  if String.length remainder = 0 then
    true
  else
    match remainder.[0] with
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '_'
    | '-' ->
      false
    | _ -> true

let try_ts_directive ~directive comment =
  let (rest, matched) = consume_token directive comment in
  matched && ts_directive_boundary rest

let should_suppress ~is_ts_file comment loc =
  let (comment, _) = consume_tokens [" "; "\n"; "\t"; "\r"; "*"] comment in
  let ts_match =
    if is_ts_file then
      if try_ts_directive ~directive:"@ts-expect-error" comment then
        Some (All { locs = LocSet.singleton loc; warn_unused = true })
      else if try_ts_directive ~directive:"@ts-ignore" comment then
        Some (All { locs = LocSet.singleton loc; warn_unused = false })
      else
        None
    else
      None
  in
  match ts_match with
  | Some codes -> Ok (Some codes)
  | None ->
    let (comment, is_suppressor) = consume_tokens ["$FlowFixMe"; "$FlowExpectedError"] comment in
    if not is_suppressor then
      Ok None
    else
      let (comment, has_preceding_spaces) = consume_tokens [" "; "\n"; "\t"; "\r"] comment in
      let (comment, has_code) = consume_token "[" comment in
      if not has_code then
        Error MissingCode
      else (
        match Base.String.index comment ']' with
        | None -> Error MalformedCode (* Not a code if the bracket is not terminated *)
        | Some 0 -> Error MalformedCode (* $FlowFixMe[] is not a real code *)
        | Some index ->
          (* //$FlowFixMe [code] is invalid *)
          if has_preceding_spaces then
            Error MalformedCode
          else
            let code = Base.String.prefix comment index in
            if Base.String.for_all ~f:is_valid_code_char code then
              Ok (Some (Specific (CodeSet.singleton (code, loc))))
            else
              Error MalformedCode
      )
