(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type t [@@deriving show]

  val compare : t -> t -> int

  val equal : t -> t -> bool

  (* Exposes the underlying representation of the location. Use for debugging purposes only. Do not
   * expose these results in user output or make typecheker behavior depend on it. *)
  val debug_to_string : ?include_source:bool -> t -> string

  module LMap : sig
    include WrappedMap.S with type key = t

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  end

  module LSet : Set.S with type elt = t

  module LSetUtils : sig
    val ident_map : (LSet.elt -> LSet.elt) -> LSet.t -> LSet.t
  end
end

module LSetUtils (S : Set.S) = struct
  let ident_map f map =
    let changed = ref false in
    let map' =
      S.map
        (fun elem ->
          let elem' = f elem in
          if elem == elem' then
            elem
          else (
            changed := true;
            elem'
          ))
        map
    in
    if !changed then
      map'
    else
      map
end

module LocS : S with type t = Loc.t = struct
  type t = Loc.t [@@deriving show]

  let compare = Loc.compare

  let equal = Loc.equal

  let debug_to_string = Loc.debug_to_string

  module LMap = struct
    include WrappedMap.Make (Loc)

    let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
     (fun pp_data -> make_pp Loc.pp pp_data)

    let show pp_data x = Format.asprintf "%a" (pp pp_data) x
  end

  module LSet = Set.Make (Loc)
  module LSetUtils = LSetUtils (LSet)
end

module ALocS : S with type t = ALoc.t = struct
  type t = ALoc.t [@@deriving show]

  let compare = ALoc.compare

  let equal = ALoc.equal

  let debug_to_string = ALoc.debug_to_string

  module LMap = struct
    include WrappedMap.Make (ALoc)

    let pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit =
     (fun pp_data -> make_pp ALoc.pp pp_data)

    let show pp_data x = Format.asprintf "%a" (pp pp_data) x
  end

  module LSet = Set.Make (ALoc)
  module LSetUtils = LSetUtils (LSet)
end
