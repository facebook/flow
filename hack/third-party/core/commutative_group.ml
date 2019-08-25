(** A signature for a commutative group (in the group-theory sense).

    An implementation of this interface should have the following properties:

    1: associativity: (a+b)+c = a+(b+c) for all elt's a,b,c
    2: identity: zero+a = a+zero = a for all elt's a
    3: inverses: given any elt a there exists a (unique) elt b such that a+b=b+a=zero
    4: commutativity: a+b = b+a
*)

module type S = sig
  type t (* an element of the group *)

  val zero : t

  val ( + ) : t -> t -> t

  val ( - ) : t -> t -> t
end
