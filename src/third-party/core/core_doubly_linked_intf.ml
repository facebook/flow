(** Doubly-linked lists.

    Compared to other doubly-linked lists, in this one:

    1. Calls to modification functions ([insert*], [move*], ...) detect if the list is
    being iterated over ([iter], [fold], ...), and if so raise an exception.  For example,
    a use like the following would raise:

    {[
      iter t ~f:(fun _ -> ... remove t e ...)
    ]}

    2. There is a designated "front" and "back" of each list, rather than viewing each
    element as an equal in a ring.

    3. Elements know which list they're in. Each operation that takes an [Elt.t] also
    takes a [t], first checks that the [Elt] belongs to the [t], and if not, raises.

    4. Related to (3), lists cannot be split, though a sort of splicing is available as
    [transfer]. In other words, no operation will cause one list to become two. This
    makes this module unsuitable for maintaining the faces of a planar graph under edge
    insertion and deletion, for example.

    5. Another property permitted by (3) and (4) is that [length] is O(1). *)

module type S = sig
  module Elt : sig
    type 'a t

    val value : 'a t -> 'a

    (** pointer equality *)
    val equal : 'a t -> 'a t -> bool

    val set : 'a t -> 'a -> unit
  end

  type 'a t

  (** {2 Creating doubly-linked lists} *)

  val create : unit -> 'a t

  (** [of_list l] returns a doubly-linked list [t] with the same elements as [l] and in
      the same order (i.e., the first element of [l] is the first element of [t]). It is
      always the case that [l = to_list (of_list l)]. *)
  val of_list : 'a list -> 'a t

  val of_array : 'a array -> 'a t

  (** {2 Constant-time insertion of a new element} *)

  val insert_before : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t

  val insert_after : 'a t -> 'a Elt.t -> 'a -> 'a Elt.t

  val insert_first : 'a t -> 'a -> 'a Elt.t

  val insert_last : 'a t -> 'a -> 'a Elt.t

  (** {2 Constant-time move of an element from and to positions in the same list}

      An exception is raised if [elt] is equal to [anchor]. *)

  val move_to_front : 'a t -> 'a Elt.t -> unit

  val move_after : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit

  val move_before : 'a t -> 'a Elt.t -> anchor:'a Elt.t -> unit

  (** {2 Constant-time removal of an element} *)

  val remove : 'a t -> 'a Elt.t -> unit

  val remove_first : 'a t -> 'a option

  val remove_last : 'a t -> 'a option

  (** [clear t] removes all elements from the list in constant time. *)
  val clear : 'a t -> unit
end

module type Doubly_linked = sig
  module type S = S

  include S
end
