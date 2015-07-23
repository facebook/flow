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
module Json = Hh_json

let mk_id () = Ident.make ""

(* Reasons are included in types mainly for error reporting, but sometimes we
   also use reasons in types to recover information on the source code that
   caused those reasons to be created. Two examples of such secondary uses of
   reasons are:

   - strictness analysis: we use reasons to locate the origin of an object and
   the origin of an operation on the object, and use such origins to determine
   whether certain errors should be suppressed.

   - termination analysis: we use reasons to limit instantiation of type
   parameters in polymorphic types at particular locations, to prevent the type
   checker from generating an unbounded number of constraints. The `pos` field
   of reasons is sufficient to distinguish code locations, except that as an
   implementation strategy for checking polymorphic definitions, we walk over
   the same source code multiple times to check it with different instantiations
   of type parameters, and to index "copies" of the reasons created in those
   passes over the same source code, we use an additional `test_id` field.
*)
module TestID = struct
  let _current = ref None

  (* Get current test id. *)
  let current() = !_current

  (* Call f on a, installing new_test_id as the current test_id, and restoring
     the current test_id when done. (See also the function mk_reason below.) *)
  let run f a =
    let test_id = current () in
    _current := Some (mk_id ());
    f a;
    _current := test_id

end

type reason = {
  test_id: int option;
  derivable: bool;
  desc: string;
  loc: Loc.t;
}

let lexpos file line col = {
  Lexing.pos_fname = file;
  Lexing.pos_lnum = line;
  Lexing.pos_bol = 0;
  Lexing.pos_cnum = col;
}

let diff_range loc = Loc.(
  let line1, line2 = loc.start.line, loc._end.line in
  (* TODO: Get rid of +1 which is here to ensure same behavior as old code
     using Pos.info_pos *)
  let start, end_  = loc.start.column + 1, loc._end.column in
  (line2 - line1, end_ - start)
)

let in_range loc1 loc2 = Loc.(
  let loc1_line = loc1.start.line in
  let loc1_start, loc1_end = loc1.start.column, loc1._end.column in
  let loc2_line1, loc2_line2 = loc2.start.line, loc2._end.line in
  let loc2_start, loc2_end  = loc2.start.column, loc2._end.column in
  loc2_line1 <= loc1_line && loc1_line <= loc2_line2 &&
    loc2_start <= loc1_start && loc1_end <= loc2_end
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

let string_of_loc loc = Loc.(
  match loc.source with
  | None -> ""
  | Some file ->
    let line = loc.start.line in
    let start = loc.start.column + 1 in
    let end_ = loc._end.column in
    if line <= 0 then
      spf "File \"%s\"" file
    else if line = loc._end.line && start - end_ = 1 then
      spf "File \"%s\", line %d, character %d" file line start
    else
      spf "File \"%s\", line %d, characters %d-%d" file line start end_
)

let json_of_loc loc = Json.(Loc.(
  JAssoc [
    "file", JString (match loc.source with Some x -> x | None -> "");
    "start", JAssoc [
      "line", JInt loc.start.line;
      "col", JInt loc.start.column;
    ];
    "end", JAssoc [
      "line", JInt loc._end.line;
      "col", JInt loc._end.column;
    ];
  ]
))

(* reason constructors, accessors, etc. *)

let mk_reason_with_test_id test_id desc loc = {
  test_id;
  derivable = false;
  desc;
  loc;
}

(* The current test_id is included in every new reason. *)
let mk_reason desc loc =
  mk_reason_with_test_id (TestID.current()) desc loc

(* Lift a string to a reason. Usually used as a dummy reason. *)
let reason_of_string s =
  mk_reason_with_test_id None s Loc.none

let loc_of_reason r = r.loc

let string_of_reason r =
  let spos = string_of_loc (loc_of_reason r) in
  let desc = r.desc in
  if spos = ""
  then desc
  else (
    if desc = ""
    then spos
    else spf "%s:\n%s" spos desc
  )

let json_of_reason r = Json.(
  JAssoc [
    "pos", json_of_loc (loc_of_reason r);
    "desc", JString r.desc
  ]
)

let dump_reason r =
  spf "[%s] %S" (string_of_loc (loc_of_reason r)) r.desc

let desc_of_reason r =
  r.desc

let internal_name name =
  spf ".%s" name

let is_internal_name name =
  String.length name >= 1 && name.[0] = '.'

let internal_module_name name =
  spf "$module__%s" name

let is_internal_module_name name =
  Str.string_match (Str.regexp "\\$module__.*") name 0

let is_derivable_reason r =
  r.derivable

let derivable_reason r =
  { r with derivable = true }

let builtin_reason x =
  mk_reason x Loc.({ none with source = Some Files_js.global_file_name })
  |> derivable_reason

let is_builtin_reason r =
  r.loc.Loc.source = Some Files_js.global_file_name

(* reasons compare on their locations *)
let compare r1 r2 =
  Pervasives.compare (loc_of_reason r1) (loc_of_reason r2)

let same_scope r1 r2 =
  r1.loc.Loc.source = r2.loc.Loc.source

(* reason transformers: *)

(* returns reason whose description is prefix-extension of original *)
let prefix_reason prefix reason =
  mk_reason (spf "%s%s" prefix (desc_of_reason reason))
    (loc_of_reason reason)

(* returns reason whose description is suffix-extension of original *)
let suffix_reason suffix reason =
  mk_reason (spf "%s%s" (desc_of_reason reason) suffix)
    (loc_of_reason reason)

(* returns reason whose description is prefix+suffix-extension of original *)
let wrap_reason prefix suffix reason =
  mk_reason (spf "%s%s%s" prefix (desc_of_reason reason) suffix)
    (loc_of_reason reason)

(* returns reason with new description and position of original *)
let replace_reason replacement reason =
  mk_reason replacement (loc_of_reason reason)

(* returns reason with new location and description of original *)
let repos_reason loc reason =
  mk_reason (desc_of_reason reason) loc

(* helper: strip root from positions *)
let strip_root reason path = Loc.(
  let loc = loc_of_reason reason in
  let source = match loc.source with
  | None -> None
  | Some file -> Some (
    if file = Files_js.global_file_name
    then "[LIB]"
    else if Files_js.is_lib_file file
    then spf "[LIB] %s" (Filename.basename file)
    else Files_js.relative_path
      (spf "%s%s" (Path.to_string path) Filename.dir_sep) file
  ) in
  let loc = { loc with source } in
  repos_reason loc reason
)
