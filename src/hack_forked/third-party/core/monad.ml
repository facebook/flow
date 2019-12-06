module type Basic = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t

  val return : 'a -> 'a t

  (* The [map] argument to [Monad.Make] says how to implement the monad's [map] function.
     [`Define_using_bind] means to define [map t ~f = bind t (fun a -> return (f a))].
     [`Custom] overrides the default implementation, presumably with something more
     efficient.

     Some other functions returned by [Monad.Make] are defined in terms of [map], so
     passing in a more efficient [map] will improve their efficiency as well. *)
  val map : [ `Define_using_bind | `Custom of 'a t -> f:('a -> 'b) -> 'b t ]
end

module type Infix = sig
  type 'a t

  (** [t >>= f] returns a computation that sequences the computations represented by two
      monad elements.  The resulting computation first does [t] to yield a value [v], and
      then runs the computation returned by [f v]. *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [t >>| f] is [t >>= (fun a -> return (f a))]. *)
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  (** A monad is an abstraction of the concept of sequencing of computations.  A value of
      type 'a monad represents a computation that returns a value of type 'a. *)
  include
    Infix

  module Monad_infix : Infix with type 'a t := 'a t

  (** [bind t f] = [t >>= f] *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [return v] returns the (trivial) computation that returns v. *)
  val return : 'a -> 'a t

  (** [map t ~f] is t >>| f. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** [join t] is [t >>= (fun t' -> t')]. *)
  val join : 'a t t -> 'a t

  (** [ignore t] = map t ~f:(fun _ -> ()). *)
  val ignore : 'a t -> unit t

  val all : 'a t list -> 'a list t

  val all_ignore : unit t list -> unit t
end

module Make (M : Basic) : S with type 'a t := 'a M.t = struct
  let bind = M.bind

  let return = M.return

  let map_via_bind ma ~f = M.bind ma (fun a -> M.return (f a))

  let map =
    match M.map with
    | `Define_using_bind -> map_via_bind
    | `Custom x -> x

  module Monad_infix = struct
    let ( >>= ) = bind

    let ( >>| ) t f = map t ~f
  end

  include Monad_infix

  let join t = t >>= (fun t' -> t')

  let ignore t = map t ~f:(fun _ -> ())

  let all =
    let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= (fun v -> loop (v :: vs) ts)
    in
    (fun ts -> loop [] ts)

  let rec all_ignore = function
    | [] -> return ()
    | t :: ts -> t >>= (fun () -> all_ignore ts)
end

(**
   Multi parameter monad.
   The second parameter get unified across all the computation. This is used
   to encode monads working on a multi parameter data structure like
   ([('a,'b result)]).
*)
module type Basic2 = sig
  type ('a, 'd) t

  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t

  val map :
    [ `Define_using_bind | `Custom of ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t ]

  val return : 'a -> ('a, _) t
end

(** Same as Infix, except the monad type has two arguments. The second is always just
    passed through. *)
module type Infix2 = sig
  type ('a, 'd) t

  val ( >>= ) : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t

  val ( >>| ) : ('a, 'd) t -> ('a -> 'b) -> ('b, 'd) t
end

(** The same as S except the monad type has two arguments. The second is always just
    passed through. *)
module type S2 = sig
  include Infix2

  module Monad_infix : Infix2 with type ('a, 'd) t := ('a, 'd) t

  val bind : ('a, 'd) t -> ('a -> ('b, 'd) t) -> ('b, 'd) t

  val return : 'a -> ('a, _) t

  val map : ('a, 'd) t -> f:('a -> 'b) -> ('b, 'd) t

  val join : (('a, 'd) t, 'd) t -> ('a, 'd) t

  val ignore : (_, 'd) t -> (unit, 'd) t

  val all : ('a, 'd) t list -> ('a list, 'd) t

  val all_ignore : (unit, 'd) t list -> (unit, 'd) t
end

module Check_S2_refines_S (X : S) : S2 with type ('a, 'd) t = 'a X.t = struct
  type ('a, 'd) t = 'a X.t

  include struct
    open X

    let ( >>= ) = ( >>= )

    let ( >>| ) = ( >>| )

    let bind = bind

    let return = return

    let map = map

    let join = join

    let ignore = ignore

    let all = all

    let all_ignore = all_ignore
  end

  module Monad_infix = struct
    open X.Monad_infix

    let ( >>= ) = ( >>= )

    let ( >>| ) = ( >>| )
  end
end

module Make2 (M : Basic2) : S2 with type ('a, 'd) t := ('a, 'd) M.t = struct
  let bind = M.bind

  let return = M.return

  let map_via_bind ma ~f = M.bind ma (fun a -> M.return (f a))

  let map =
    match M.map with
    | `Define_using_bind -> map_via_bind
    | `Custom x -> x

  module Monad_infix = struct
    let ( >>= ) = bind

    let ( >>| ) t f = map t ~f
  end

  include Monad_infix

  let join t = t >>= (fun t' -> t')

  let ignore t = map t ~f:(fun _ -> ())

  let all =
    let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= (fun v -> loop (v :: vs) ts)
    in
    (fun ts -> loop [] ts)

  let rec all_ignore = function
    | [] -> return ()
    | t :: ts -> t >>= (fun () -> all_ignore ts)
end
