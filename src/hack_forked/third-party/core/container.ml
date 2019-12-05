let fold_count fold t ~f =
  fold t ~init:0 ~f:(fun n a ->
      if f a then
        n + 1
      else
        n)

let fold_sum
    (type a) (module M : Commutative_group.S with type t = a) fold t ~f =
  fold t ~init:M.zero ~f:(fun n a -> M.( + ) n (f a))

let fold_min fold t ~cmp =
  fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some min ->
        if cmp min elt > 0 then
          Some elt
        else
          acc)

let fold_max fold t ~cmp =
  fold t ~init:None ~f:(fun acc elt ->
      match acc with
      | None -> Some elt
      | Some max ->
        if cmp max elt < 0 then
          Some elt
        else
          acc)
