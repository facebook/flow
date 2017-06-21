(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

(**************************)

let string_of_pos p = Loc.(
  spf "%d:%d" p.line p.column
)

let string_of_span span = Loc.(
  spf "%s - %s" (string_of_pos span.start) (string_of_pos span._end)
)

let make_span startpos endpos = Loc.({
  source = None; start = startpos; _end = endpos
})

module Span = struct
  type t = Loc.t

  (* Two positions are equal iff
   * 1) They have the same line number
   * 2) They have the same column number
   *)
  let poscmp p0 p1 = Loc.(
    match Pervasives.compare p0.line p1.line with
      | 0 -> Pervasives.compare p0.column p1.column
      | _ as x -> x
  )

  (* Span A and B are considered equal iff
  * 1) They come from the same file (or both have no file)
  * 2) A.start >= B.start
  * 3) A.end <= B.end
  *
  * So basically they need to be in the same file and A needs to be contained
  * within B.
  *)
  let compare l0 l1 = Loc.(
    match Pervasives.compare l0.source l1.source with
    | 0 -> (
      match poscmp l0.start l1.start with
        | 0
        | 1 -> (
            match poscmp l0._end l1._end with
              | 0
              | -1 -> 0
              | _ -> 1
            )
        | _ -> -1
      )
    | _ as x -> x
  )
end

module SpanMap = MyMap.Make (Span)
