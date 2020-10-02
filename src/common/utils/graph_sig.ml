(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t

  type elt

  type set

  type map

  val of_map : map -> t

  val update_from_map : t -> map -> to_remove:set -> t

  (* Raises Not_found if the element does not exist. *)
  val find : elt -> t -> set

  (* Gets the set of backward edges. Raises Not_found if the element does not exist. *)
  val find_backward : elt -> t -> set

  val find_opt : elt -> t -> set option

  (* Gets the set of backward edges. *)
  val find_backward_opt : elt -> t -> set option

  val to_map : t -> map

  val to_backward_map : t -> map

  (* Fold over forward edges *)
  val fold : (elt -> set -> 'a -> 'a) -> t -> 'a -> 'a

  (* The map function must be injective, otherwise the results are undefined. *)
  val map : (elt -> elt) -> t -> t
end
