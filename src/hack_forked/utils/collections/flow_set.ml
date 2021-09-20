(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  type elt

  type t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val iter : (elt -> unit) -> t -> unit

  val map : (elt -> elt) -> t -> t

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> int

  val elements : t -> elt list

  val min_elt : t -> elt

  val min_elt_opt : t -> elt option

  val max_elt : t -> elt

  val max_elt_opt : t -> elt option

  val choose : t -> elt

  val choose_opt : t -> elt option

  val find : elt -> t -> elt

  val find_opt : elt -> t -> elt option

  val to_seq : t -> elt Seq.t

  val of_list : elt list -> t

  val make_pp : (Format.formatter -> elt -> unit) -> Format.formatter -> t -> unit
end

module Make (Ord : Set.OrderedType) : S with type elt = Ord.t = struct
  include Set.Make (Ord)

  let make_pp pp_key fmt iset =
    Format.fprintf fmt "@[<2>{";
    let elements = elements iset in
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    ignore
      (List.fold_left
         (fun sep s ->
           if sep then Format.fprintf fmt ";@ ";
           pp_key fmt s;
           true)
         false
         elements);
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    Format.fprintf fmt "@,}@]"
end
