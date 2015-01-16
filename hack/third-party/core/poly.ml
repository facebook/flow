(** [Poly] is a convenient shorthand for [Polymorphic_compare] in the common case that one
    wants to use a polymorphic comparator directly in an expression, e.g. [Poly.equal a
    b]. *)

include Polymorphic_compare
