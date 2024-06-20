open Core_hashtbl_intf
module Hashable = Core_hashtbl_intf.Hashable
module Creators = Hashtbl.Creators

include (
  Hashtbl :
    sig
      type ('a, 'b) t = ('a, 'b) Hashtbl.t

      include Base.Hashtbl.S_without_submodules with type ('a, 'b) t := ('a, 'b) t
    end
)

module type S_plain = S_plain with type ('a, 'b) hashtbl = ('a, 'b) t

module type S = S with type ('a, 'b) hashtbl = ('a, 'b) t

module type Key_plain = Key_plain

module type Key = Key

module Make_plain_with_hashable (T : sig
  module Key : Key_plain

  val hashable : Key.t Hashable.t
end) =
struct
  let hashable = T.hashable

  type key = T.Key.t

  type ('a, 'b) hashtbl = ('a, 'b) t

  type 'a t = (T.Key.t, 'a) hashtbl

  type 'a key_ = T.Key.t

  include Creators (struct
    type 'a t = T.Key.t

    let hashable = hashable
  end)

  include (Hashtbl : sig end)
end

module Make_plain (Key : Key_plain) = Make_plain_with_hashable (struct
  module Key = Key

  let hashable = { Hashable.hash = Key.hash; compare = Key.compare; sexp_of_t = Key.sexp_of_t }
end)

module Make (Key : Key) = struct
  include Make_plain (Key)
end

let hashable = Hashtbl.Private.hashable
