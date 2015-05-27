(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* The file extensions we are interested in *)
(*****************************************************************************)

let extensions = [
  ".php"  ; (* normal php file *)
  ".hh"   ; (* Hack extension some open source code is starting to use *)
  ".phpt" ; (* our php template files *)
  ".hhi"  ; (* interface files only visible to the type checker *)
  ".xhp"  ; (* XHP extensions *)
]

let is_directory path = try Sys.is_directory path with Sys_error _ -> false

let is_dot_file path =
  let filename = Filename.basename path in
  String.length filename > 0 && filename.[0] = '.'

let is_php path =
  not (is_dot_file path) &&
  List.exists (Filename.check_suffix path) extensions &&
  not (is_directory path) &&
  not (FilesToIgnore.should_ignore path)

let is_js path =
  not (is_dot_file path) &&
  Filename.check_suffix path ".js" &&
  not (is_directory path)
