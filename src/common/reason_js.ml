(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module defines a general notion of trace, which is used in modules
   Type_inference_js and Flow_js to record how the typechecker reasons about
   code, systematically collecting, simplifying, and solving constraints. This
   is extremely useful, not only for debugging the typechecker but also to
   really understand why an error is reported. *)

(* Eventually, trace information should be printed out only in verbose mode,
   since Flow reports all errors it finds and the trace for every error can get
   quite detailed dependening on how far apart the "source" and "sink" are and
   how convoluted the flow between them is. *)

open Utils
module Ast = Spider_monkey_ast

type reason = {
  derivable: bool;
  desc: string;
  pos: Pos.t;
}

let lexpos file line col = {
  Lexing.pos_fname = file;
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = col;
}

let pos_of_loc loc = Ast.Loc.(
  let file = match loc.source with Some s -> s | None -> "" in
  { Pos.
    pos_file = Relative_path.create Relative_path.Dummy file;
    pos_start = lexpos file loc.start.line loc.start.column;
    pos_end = lexpos file loc._end.line loc._end.column;
  }
)

let diff_range loc = Ast.Loc.(
  let line1, line2 = loc.start.line, loc._end.line in
  (* TODO: Get rid of +1 which is here to ensure same behavior as old code
     using Pos.info_pos *)
  let start, end_  = loc.start.column + 1, loc._end.column in
  (line2 - line1, end_ - start)
)

let in_range p loc = Ast.Loc.(
  let line, start, end_ = Pos.info_pos p in
  let loc_line1, loc_line2 = loc.start.line, loc._end.line in
  (* TODO: Get rid of +1 which is here to ensure same behavior as old code
     using Pos.info_pos *)
  let loc_start, loc_end  = loc.start.column + 1, loc._end.column in
  loc_line1 <= line && line <= loc_line2 &&
    loc_start <= start && end_ <= loc_end
)

let rec patch ll offset lines = function
  | [] -> ()
  | (l,c,str)::insertions ->
      let c = if l = ll then c + offset else c in
      let del = try Some (int_of_string str) with _ -> None in
      let line = lines.(l - 1) in
      let shift = match del with
      | Some n -> (* delete n chars at l, c *)
          lines.(l - 1) <- spf "%s%s"
            (Str.string_before line c) (Str.string_after line (c + n));
          -n
      | None -> (* insert str at l, c *)
          lines.(l - 1) <- spf "%s%s%s"
            (Str.string_before line c) str (Str.string_after line c);
          String.length str
      in
      let offset = (if l = ll then offset else 0) + shift in
      patch l offset lines insertions

let do_patch lines insertions =
  let lines = Array.of_list lines in
  patch 1 0 lines insertions;
  String.concat "\n" (Array.to_list lines)

let string_of_pos pos =
  let file = Pos.filename pos in
  if file = Relative_path.default then
    ""
  else
    let line, start, end_ = Pos.info_pos pos in
    if line <= 0 then
      spf "File \"%s\", line 1, character 0" (Relative_path.to_absolute file)
    else if Pos.length pos = 1 then
      spf "File \"%s\", line %d, character %d"
        (Relative_path.to_absolute file) line start
    else
      spf "File \"%s\", line %d, characters %d-%d"
        (Relative_path.to_absolute file) line start end_

let new_reason s pos = {
  derivable = false;
  desc = s;
  pos = pos;
}

let reason_of_string s =
  new_reason s Pos.none

let mk_reason s loc =
  new_reason s (pos_of_loc loc)

let string_of_reason r =
  let spos = string_of_pos r.pos in
  let desc = r.desc in
  if spos = ""
  then desc
  else (
    if desc = ""
    then spos
    else spf "%s:\n%s" spos desc
  )

let pos_of_reason r =
  r.pos

let loc_of_reason r =
  let p = pos_of_reason r in Pos.(Lexing.(Ast.Loc.({
    source =
      if p.pos_file = Relative_path.default then None
      else Some (Relative_path.to_absolute p.pos_file);
    start = {
      line = p.pos_start.pos_lnum;
      column = p.pos_start.pos_cnum - p.pos_start.pos_bol;
      offset = p.pos_start.pos_cnum;
    };
    _end = {
      line = p.pos_end.pos_lnum;
      column = p.pos_end.pos_cnum - p.pos_end.pos_bol;
      offset = p.pos_end.pos_cnum;
    }
  })))

let desc_of_reason r =
  r.desc

(* TODO: delete *)
let desc_of_reason2 r =
  desc_of_reason r

(* simple way to get reasons whose descriptions are simple prefix-extensions of
   the original *)
let prefix_reason prefix reason =
  new_reason (spf "%s%s" prefix (desc_of_reason reason)) (pos_of_reason reason)

(* simple way to get reasons whose descriptions are simple replacements of the
   original *)
let replace_reason replacement reason =
  new_reason replacement (pos_of_reason reason)

let repos_reason pos reason =
  new_reason (desc_of_reason reason) pos

let internal_name name =
  spf ".%s" name

let is_internal_name name =
  String.length name >= 1 && name.[0] = '.'

let is_derivable_reason r =
  r.derivable

let derivable_reason r =
  { r with derivable = true }

let builtin_reason x =
  new_reason x (Pos.make_from
    (Relative_path.create Relative_path.Dummy (Files_js.get_flowlib_root ())))
  |> derivable_reason

(* reasons compare on their positions *)
let compare r1 r2 =
  Pervasives.compare (pos_of_reason r1) (pos_of_reason r2)

let same_scope r1 r2 =
  r1.pos.Pos.pos_file = r2.pos.Pos.pos_file
