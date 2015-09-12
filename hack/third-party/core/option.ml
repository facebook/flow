type 'a t = 'a option

let is_none = function None -> true | _ -> false

let is_some = function Some _ -> true | _ -> false

let value_map o ~default ~f =
  match o with
  | Some x -> f x
  | None   -> default

let iter o ~f =
  match o with
  | None -> ()
  | Some a -> f a

let map2 o1 o2 ~f =
  match o1, o2 with
  | Some a1, Some a2 -> Some (f a1 a2)
  | _ -> None

let call x ~f =
  match f with
  | None -> ()
  | Some f -> f x

let value t ~default =
  match t with
  | None -> default
  | Some x -> x
;;

let to_array t =
  match t with
  | None -> [||]
  | Some x -> [|x|]
;;

let to_list t =
  match t with
  | None -> []
  | Some x -> [x]
;;

let min_elt t ~cmp:_ = t
let max_elt t ~cmp:_ = t

let sum (type a) (module M : Commutative_group.S with type t = a) t ~f =
  match t with
  | None -> M.zero
  | Some x -> f x
;;

let for_all t ~f =
  match t with
  | None -> true
  | Some x -> f x
;;

let exists t ~f =
  match t with
  | None -> false
  | Some x -> f x
;;

let mem ?(equal = (=)) t a =
  match t with
  | None -> false
  | Some a' -> equal a a'
;;

let length t =
  match t with
  | None -> 0
  | Some _ -> 1
;;

let is_empty = is_none

let fold t ~init ~f =
  match t with
  | None -> init
  | Some x -> f init x
;;

let count t ~f =
  match t with
  | None -> 0
  | Some a -> if f a then 1 else 0
;;

let find t ~f =
  match t with
  | None -> None
  | Some x -> if f x then Some x else None
;;

let find_map t ~f =
  match t with
  | None -> None
  | Some a -> f a
;;

let equal f t t' =
  match t, t' with
  | None, None -> true
  | Some x, Some x' -> f x x'
  | _ -> false

let some x = Some x

let both x y =
  match x,y with
  | Some a, Some b -> Some (a,b)
  | _ -> None

let first_some x y =
  match x with
  | Some _ -> x
  | None -> y

let some_if cond x = if cond then Some x else None

let merge a b ~f =
  match a, b with
  | None, x | x, None -> x
  | Some a, Some b -> Some (f a b)

let filter t ~f =
  match t with
  | Some v as o when f v -> o
  | _ -> None

let try_with f =
  try Some (f ())
  with _ -> None

include Monad.Make (struct
  type 'a t = 'a option
  let return x = Some x
  let map t ~f =
    match t with
    | None -> None
    | Some a -> Some (f a)
  ;;
  let map = `Custom map
  let bind o f =
    match o with
    | None -> None
    | Some x -> f x
end)
