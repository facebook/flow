(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

exception Malformed_result

type hg_rev = string
type svn_rev = string

val current_working_copy_hg_rev : string ->
  (** bool indicates if there are working copy changes. *)
  (hg_rev * bool) Future.t

(** Get the SVN base revision of the current working copy in the given
 * repo dir. *)
val current_working_copy_base_rev : string ->
  svn_rev Future.t

val get_closest_svn_ancestor : hg_rev -> string -> svn_rev Future.t
val files_changed_since_svn_rev :
  hg_rev ->
  (** SVN Revision should be an ancestor of hg_rev. *)
  svn_rev ->
  (** repository path. *)
  string ->
  string Future.t

(** hg update to the base svn revision. *)
val update_to_base_rev : svn_rev -> string -> unit Future.t
