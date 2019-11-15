(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core

(*****************************************************************************)
(* The file extensions we are interested in *)
(*****************************************************************************)

let extensions =
  [
    ".php";
    (* normal php file *)
    ".phpt";
    (* our php template or test files *)
    ".hack";
    (* open source hack: bikeshed entry *)
    ".hck";
    (* open source hack: bikeshed entry *)
    ".hh";
    (* open source hack: biekshed entry *)
    ".hhi";
    (* interface files only visible to the type checker *)
    ".xhp";
    (* XHP extensions *)
  ]

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_hack path =
  (not (is_dot_file path))
  && List.exists extensions (Filename.check_suffix path)

(* Returns whether one of the ancestral directories of path has the given
 * name. *)
let rec has_ancestor path ancestor_name =
  let dirname = Filename.dirname path in
  if dirname = path then
    (* Terminal condition *)
    false
  else if Filename.basename dirname = ancestor_name then
    true
  else
    has_ancestor dirname ancestor_name

let file_filter f =
  (* Filter the relative path *)
  let f = Relative_path.strip_root_if_possible f |> Option.value ~default:f in
  is_hack f && not (FilesToIgnore.should_ignore f)

let path_filter f = Relative_path.suffix f |> file_filter
