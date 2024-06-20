(** The key is used for the hashtable of queue elements. *)
module type Key = Core_hashtbl.Key_plain

module type S1 = sig
  type 'key create_arg

  type 'key create_key

  (** A hash-queue, where the values are of type ['data]. *)
  type ('key, 'data) t

  (** [create ()] returns an empty queue.  The arguments [growth_allowed] and [size] are
      referring to the underlying hashtable.

      @param growth_allowed defaults to true
      @param size initial size -- default to 16
  *)
  val create : ?growth_allowed:bool -> ?size:int -> 'key create_arg -> ('key create_key, 'data) t

  (** Clears the queue. *)
  val clear : ('key, 'data) t -> unit

  (** {2 Adding, removing, and replacing elements}

      Note that even the non-[*_exn] versions can raise, but only if there is an ongoing
      iteration. *)

  (** See {!enqueue_exn}. [enqueue_front_exn t k v] is the same as [enqueue_exn t `front k
      v] *)
  val enqueue_front_exn : ('key, 'data) t -> 'key -> 'data -> unit

  (** Like {!lookup_and_move_to_back}, but moves element to the front of the queue *)
  val lookup_and_move_to_front : ('key, 'data) t -> 'key -> 'data option

  (** [dequeue_back t] returns the back element of the queue. *)
  val dequeue_back : ('key, 'data) t -> 'data option
end

module type S0 = sig
  type ('key, 'data) hash_queue

  type key

  include
    S1
      with type 'key create_key := key
      with type 'key create_arg := unit
      with type ('key, 'data) t := ('key, 'data) hash_queue

  type 'data t = (key, 'data) hash_queue
end

module type S_backend = sig
  include
    S1 with type 'key create_arg := 'key Core_hashtbl.Hashable.t with type 'key create_key := 'key

  module type S = S0 with type ('key, 'data) hash_queue := ('key, 'data) t

  module Make (Key : Key) : S with type key = Key.t
end

(** A hash-queue is a combination of a queue and a hashtable that
    supports constant-time lookup and removal of queue elements in addition to
    the usual queue operations (enqueue, dequeue). The queue elements are
    key-value pairs. The hashtable has one entry for each element of the queue.

    Calls to functions that would modify a hash-queue (e.g. [enqueue], [dequeue],
    [remove], [replace]) detect if a client is in the middle of iterating over the
    queue (e.g., [iter], [fold], [for_all], [exists]) and if so, raise an exception.
*)
module type Hash_queue = sig
  module type Key = Key

  module type S_backend = S_backend

  (** equivalent to [Make_backend (Hashtbl)] *)
  include S_backend
end
