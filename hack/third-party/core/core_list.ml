module UnlabeledList = List
module List = StdLabels.List
module String = StdLabels.String

let invalid_argf = Core_printf.invalid_argf

module T = struct
  type 'a t = 'a list
end

include T

module Or_unequal_lengths = struct
  type 'a t =
    | Ok of 'a
    | Unequal_lengths
end

let of_list t = t

let range
    ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "Core_list.range: stride must be non-zero";

  (* Generate the range from the last element, so that we do not need to rev it *)
  let rec loop last counter accum =
    if counter <= 0 then
      accum
    else
      loop (last - stride) (counter - 1) (last :: accum)
  in
  let stride_sign =
    if stride > 0 then
      1
    else
      -1
  in
  let start =
    match start with
    | `inclusive -> start_i
    | `exclusive -> start_i + stride
  in
  let stop =
    match stop with
    | `inclusive -> stop_i + stride_sign
    | `exclusive -> stop_i
  in
  let num_elts = (stop - start + stride - stride_sign) / stride in
  loop (start + (stride * (num_elts - 1))) num_elts []

(* Standard functions *)
let length = List.length

let hd_exn = List.hd

let tl_exn = List.tl

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'

let nth t n =
  if n < 0 then
    None
  else
    let rec nth_aux t n =
      match t with
      | [] -> None
      | a :: t ->
        if n = 0 then
          Some a
        else
          nth_aux t (n - 1)
    in
    nth_aux t n

let nth_exn t n =
  match nth t n with
  | None ->
    invalid_argf "List.nth_exn %d called on list of length %d" n (length t) ()
  | Some a -> a

let rev_append = List.rev_append

let rev = function
  | ([] | [_]) as res -> res
  | x :: y :: rest -> rev_append rest [y; x]

let unordered_append l1 l2 =
  match (l1, l2) with
  | ([], l)
  | (l, []) ->
    l
  | _ -> List.rev_append l1 l2

let rev_map t ~f = List.rev_map t ~f

exception Length_mismatch of string * int * int

let check_length2_exn name l1 l2 =
  let n1 = length l1 in
  let n2 = length l2 in
  if n1 <> n2 then
    raise (invalid_argf "length mismatch in %s: %d <> %d " name n1 n2 ())

let check_length2 l1 l2 ~f =
  if length l1 <> length l2 then
    Or_unequal_lengths.Unequal_lengths
  else
    Or_unequal_lengths.Ok (f l1 l2)

let check_length3 name l1 l2 l3 =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3 then
    raise
      (invalid_argf
         "length mismatch in %s: %d <> %d || %d <> %d"
         name
         n1
         n2
         n2
         n3
         ())

let iter2 l1 l2 ~f = check_length2 l1 l2 ~f:(List.iter2 ~f)

let iter2_exn l1 l2 ~f =
  check_length2_exn "iter2_exn" l1 l2;
  List.iter2 l1 l2 ~f

let rev_map2 l1 l2 ~f = check_length2 l1 l2 ~f:(List.rev_map2 ~f)

let rev_map2_exn l1 l2 ~f =
  check_length2_exn "rev_map2_exn" l1 l2;
  List.rev_map2 l1 l2 ~f

let fold2 l1 l2 ~init ~f = check_length2 l1 l2 ~f:(List.fold_left2 ~init ~f)

let fold2_exn l1 l2 ~init ~f =
  check_length2_exn "fold2_exn" l1 l2;
  List.fold_left2 l1 l2 ~init ~f

let for_all2 l1 l2 ~f = check_length2 l1 l2 ~f:(List.for_all2 ~f)

let for_all2_exn l1 l2 ~f =
  check_length2_exn "for_all2_exn" l1 l2;
  List.for_all2 l1 l2 ~f

let exists2 l1 l2 ~f = check_length2 l1 l2 ~f:(List.exists2 ~f)

let exists2_exn l1 l2 ~f =
  check_length2_exn "exists2_exn" l1 l2;
  List.exists2 l1 l2 ~f

let mem ?(equal = ( = )) t a = List.exists t ~f:(equal a)

(* This is a copy of the standard library assq function. *)
let rec assq x = function
  | [] -> raise Not_found
  | (a, b) :: l ->
    if a == x then
      b
    else
      assq x l

(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for List.filter in profiling). *)
let rev_filter t ~f =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l ->
      if f x then
        find ~f (x :: accu) l
      else
        find ~f accu l
  in
  find ~f [] t

let filter t ~f = rev (rev_filter t ~f)

let sort = List.sort

let stable_sort = List.stable_sort

let fast_sort = List.fast_sort

(* 4.02 forgot to add sort_uniq to ListLabels, but it was added in 4.03:
   https://github.com/ocaml/ocaml/commit/512d128918544ae1da0c808e811f3a7f177524d2 *)
let sort_uniq ~(cmp : 'a -> 'a -> int) (lst : 'a list) =
  UnlabeledList.sort_uniq cmp lst

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
      (match f x with
      | None -> loop l
      | Some _ as r -> r)
  in
  loop t

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
      if f x then
        Some x
      else
        loop l
  in
  loop t

let find_exn t ~f = List.find t ~f

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l ->
      if f i x then
        Some (i, x)
      else
        loop (i + 1) l
  in
  loop 0 t

(** changing the order of arguments on some standard [List] functions. *)
let exists t ~f = List.exists t ~f

let for_all t ~f = List.for_all t ~f

let iter t ~f = List.iter t ~f

(** For the container interface. *)
let fold t ~init ~f = List.fold_left t ~f ~init

let fold_left = fold

let to_array = Hack_caml.Array.of_list

let to_list t = t

(** Tail recursive versions of standard [List] module *)

let slow_append l1 l2 = List.rev_append (List.rev l1) l2

(* There are a few optimized list operations here, including append and map.  There are
   basically two optimizations in play: loop unrolling, and dynamic switching between
   stack and heap allocation.

   The loop-unrolling is straightforward, we just unroll 5 levels of the loop.  This makes
   each iteration faster, and also reduces the number of stack frames consumed per list
   element.

   The dynamic switching is done by counting the number of stack frames, and then
   switching to the "slow" implementation when we exceed a given limit.  This means that
   short lists use the fast stack-allocation method, and long lists use a slower one that
   doesn't require stack space.
*)
let rec count_append l1 l2 count =
  match l2 with
  | [] -> l1
  | _ ->
    (match l1 with
    | [] -> l2
    | [x1] -> x1 :: l2
    | [x1; x2] -> x1 :: x2 :: l2
    | [x1; x2; x3] -> x1 :: x2 :: x3 :: l2
    | [x1; x2; x3; x4] -> x1 :: x2 :: x3 :: x4 :: l2
    | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
      x1
      :: x2
      :: x3
      :: x4
      :: x5
      ::
      ( if count > 1000 then
        slow_append tl l2
      else
        count_append tl l2 (count + 1) ))

let append l1 l2 = count_append l1 l2 0

let map_slow l ~f = List.rev (List.rev_map ~f l)

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [x1] ->
    let f1 = f x1 in
    [f1]
  | [x1; x2] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [f1; f2]
  | [x1; x2; x3] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [f1; f2; f3]
  | [x1; x2; x3; x4] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [f1; f2; f3; f4]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1
    :: f2
    :: f3
    :: f4
    :: f5
    ::
    ( if ctr > 1000 then
      map_slow ~f tl
    else
      count_map ~f tl (ctr + 1) )

let map l ~f = count_map ~f l 0

let ( >>| ) l f = map l ~f

let map2_exn l1 l2 ~f = List.rev (rev_map2_exn l1 l2 ~f)

let rev_map3_exn l1 l2 l3 ~f =
  check_length3 "rev_map3" l1 l2 l3;
  let rec loop l1 l2 l3 ac =
    match (l1, l2, l3) with
    | ([], [], []) -> ac
    | (x1 :: l1, x2 :: l2, x3 :: l3) -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 []

let map3_exn l1 l2 l3 ~f = List.rev (rev_map3_exn l1 l2 l3 ~f)

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)

let fold_right l ~f ~init = fold ~f:(fun a b -> f b a) ~init (List.rev l)

let unzip list =
  let rec loop list l1 l2 =
    match list with
    | [] -> (List.rev l1, List.rev l2)
    | (x, y) :: tl -> loop tl (x :: l1) (y :: l2)
  in
  loop list [] []

let zip_exn l1 l2 = map2_exn ~f:(fun a b -> (a, b)) l1 l2

let zip l1 l2 = (try Some (zip_exn l1 l2) with _ -> None)

(** Additional list operations *)

let rev_mapi l ~f =
  let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l

let mapi l ~f = List.rev (rev_mapi l ~f)

let iteri l ~f =
  ignore
    (fold l ~init:0 ~f:(fun i x ->
         let () = f i x in
         i + 1))

let foldi t ~f ~init =
  snd (fold t ~init:(0, init) ~f:(fun (i, acc) v -> (i + 1, f i acc v)))

let filteri l ~f =
  List.rev
    (foldi
       l
       ~f:(fun pos acc x ->
         if f pos x then
           x :: acc
         else
           acc)
       ~init:[])

let reduce l ~f =
  match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> raise (Invalid_argument "List.reduce_exn")
  | Some v -> v

let groupi l ~break =
  let groups =
    foldi l ~init:[] ~f:(fun i acc x ->
        match acc with
        | [] -> [[x]]
        | current_group :: tl ->
          if break i (hd_exn current_group) x then
            [x] :: current_group :: tl
          (* start new group *)
          else
            (x :: current_group) :: tl)
    (* extend current group *)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

let concat_map l ~f =
  let rec aux acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l

let concat_mapi l ~f =
  let rec aux cont acc = function
    | [] -> List.rev acc
    | hd :: tl -> aux (cont + 1) (rev_append (f cont hd) acc) tl
  in
  aux 0 [] l

let merge l1 l2 ~cmp =
  let rec loop acc l1 l2 =
    match (l1, l2) with
    | ([], l2) -> rev_append acc l2
    | (l1, []) -> rev_append acc l1
    | (h1 :: t1, h2 :: t2) ->
      if cmp h1 h2 <= 0 then
        loop (h1 :: acc) t1 l2
      else
        loop (h2 :: acc) l1 t2
  in
  loop [] l1 l2

include struct
  (* We are explicit about what we import from the general Monad functor so that
   * we don't accidentally rebind more efficient list-specific functions.
   *)
  module Monad = Monad.Make (struct
    type 'a t = 'a list

    let bind x f = concat_map x ~f

    let map = `Custom map

    let return x = [x]
  end)

  open Monad
  module Monad_infix = Monad_infix

  let ignore = ignore

  let join = join

  let bind = bind

  let ( >>= ) = bind

  let return = return

  let all = all

  let all_ignore = all_ignore
end

(** returns final element of list *)
let rec last_exn list =
  match list with
  | [x] -> x
  | _ :: tl -> last_exn tl
  | [] -> raise (Invalid_argument "Core_list.last")

(** optionally returns final element of list *)
let rec last list =
  match list with
  | [x] -> Some x
  | _ :: tl -> last tl
  | [] -> None

let find_consecutive_duplicate t ~equal =
  match t with
  | [] -> None
  | a1 :: t ->
    let rec loop a1 t =
      match t with
      | [] -> None
      | a2 :: t ->
        if equal a1 a2 then
          Some (a1, a2)
        else
          loop a2 t
    in
    loop a1 t

(* returns list without adjacent duplicates *)
let remove_consecutive_duplicates list ~equal =
  let rec loop list accum =
    match list with
    | [] -> accum
    | [hd] -> hd :: accum
    | hd1 :: hd2 :: tl ->
      if equal hd1 hd2 then
        loop (hd2 :: tl) accum
      else
        loop (hd2 :: tl) (hd1 :: accum)
  in
  rev (loop list [])

(** returns sorted version of list with duplicates removed *)
let dedup ?(compare = Pervasives.compare) list =
  match list with
  | [] -> [] (* performance hack *)
  | _ ->
    let equal x x' = compare x x' = 0 in
    let sorted = List.sort ~cmp:compare list in
    remove_consecutive_duplicates ~equal sorted

let contains_dup ?compare lst = length (dedup ?compare lst) <> length lst

let find_a_dup ?(compare = Pervasives.compare) l =
  let sorted = List.sort ~cmp:compare l in
  let rec loop l =
    match l with
    | []
    | [_] ->
      None
    | hd1 :: hd2 :: tl ->
      if compare hd1 hd2 = 0 then
        Some hd1
      else
        loop (hd2 :: tl)
  in
  loop sorted

let count t ~f = Container.fold_count fold t ~f

let sum m t ~f = Container.fold_sum m fold t ~f

let min_elt t ~cmp = Container.fold_min fold t ~cmp

let max_elt t ~cmp = Container.fold_max fold t ~cmp

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then
      accum
    else
      loop (i - 1) (f (i - 1) :: accum)
  in
  loop n []

let rev_filter_map l ~f =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f hd with
      | Some x -> loop tl (x :: accum)
      | None -> loop tl accum)
  in
  loop l []

let filter_map (l : 'a list) ~(f : 'a -> 'b option) : 'b list =
  List.rev (rev_filter_map l ~f)

let rev_filter_mapi l ~f =
  let rec loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f i hd with
      | Some x -> loop (i + 1) tl (x :: accum)
      | None -> loop (i + 1) tl accum)
  in
  loop 0 l []

let filter_mapi l ~f = List.rev (rev_filter_mapi l ~f)

let filter_opt l = filter_map l ~f:(fun x -> x)

let partition_map t ~f =
  let rec loop t fst snd =
    match t with
    | [] -> (rev fst, rev snd)
    | x :: t ->
      (match f x with
      | `Fst y -> loop t (y :: fst) snd
      | `Snd y -> loop t fst (y :: snd))
  in
  loop t [] []

let partition_tf t ~f =
  let f x =
    if f x then
      `Fst x
    else
      `Snd x
  in
  partition_map t ~f

module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list

  let find t ?(equal = Poly.equal) key =
    match find t ~f:(fun (key', _) -> equal key key') with
    | None -> None
    | Some x -> Some (snd x)

  let find_exn t ?(equal = Poly.equal) key =
    match find t key ~equal with
    | None -> raise Not_found
    | Some value -> value

  let mem t ?(equal = Poly.equal) key = find t ~equal key <> None

  let remove t ?(equal = Poly.equal) key =
    filter t ~f:(fun (key', _) -> not (equal key key'))

  let add t ?(equal = Poly.equal) key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key

  let inverse t = map t ~f:(fun (x, y) -> (y, x))

  let map t ~f = List.map t ~f:(fun (key, value) -> (key, f value))
end

let sub l ~pos ~len =
  (* We use [pos > length l - len] rather than [pos + len > length l] to avoid the
     possibility of overflow. *)
  if pos < 0 || len < 0 || pos > length l - len then invalid_arg "List.sub";
  List.rev
    (foldi l ~init:[] ~f:(fun i acc el ->
         if i >= pos && i < pos + len then
           el :: acc
         else
           acc))

(*let slice a start stop =*)
(*Ordered_collection_common.slice ~length_fun:length ~sub_fun:sub*)
(*a start stop*)

let split_n t_orig n =
  if n <= 0 then
    ([], t_orig)
  else
    let rec loop n t accum =
      if n = 0 then
        (List.rev accum, t)
      else
        match t with
        | [] -> (t_orig, []) (* in this case, t_orig = List.rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum)
    in
    loop n t_orig []

let take t n = fst (split_n t n)

let drop t n = snd (split_n t n)

let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> (rev acc, t)
  in
  loop [] xs

let take_while t ~f = fst (split_while t ~f)

let drop_while t ~f = snd (split_while t ~f)

let cartesian_product list1 list2 =
  if list2 = [] then
    []
  else
    let rec loop l1 l2 accum =
      match l1 with
      | [] -> accum
      | hd :: tl ->
        loop tl l2 (List.rev_append (map ~f:(fun x -> (hd, x)) l2) accum)
    in
    List.rev (loop list1 list2 [])

let concat l = fold_right l ~init:[] ~f:append

let concat_no_order l = fold l ~init:[] ~f:(fun acc l -> rev_append l acc)

let cons x l = x :: l

let is_empty l =
  match l with
  | [] -> true
  | _ -> false

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | []
    | [_] ->
      true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 <= 0 && loop rest
  in
  loop l

let is_sorted_strictly l ~compare =
  let rec loop l =
    match l with
    | []
    | [_] ->
      true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 < 0 && loop rest
  in
  loop l

module Infix = struct
  let ( @ ) = append
end

let compare a b ~cmp =
  let rec loop a b =
    match (a, b) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (x :: xs, y :: ys) ->
      let n = cmp x y in
      if n = 0 then
        loop xs ys
      else
        n
  in
  loop a b

let equal t1 t2 ~equal =
  let rec loop t1 t2 =
    match (t1, t2) with
    | ([], []) -> true
    | (x1 :: t1, x2 :: t2) -> equal x1 x2 && loop t1 t2
    | _ -> false
  in
  loop t1 t2

let transpose =
  let rec transpose_aux t rev_columns =
    match
      partition_map t ~f:(function
          | [] -> `Snd ()
          | x :: xs -> `Fst (x, xs))
    with
    | (_ :: _, _ :: _) -> None
    | ([], _) -> Some (rev_append rev_columns [])
    | (heads_and_tails, []) ->
      let (column, trimmed_rows) = unzip heads_and_tails in
      transpose_aux trimmed_rows (column :: rev_columns)
  in
  (fun t -> transpose_aux t [])

exception Transpose_got_lists_of_different_lengths of int list

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None ->
    raise
      (Transpose_got_lists_of_different_lengths (List.map l ~f:List.length))

let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)

let rec replicate ~num x =
  match num with
  | 0 -> []
  | n when n < 0 ->
    invalid_argf "List.replicate was called with %d argument" n ()
  | _ -> x :: replicate ~num:(num - 1) x
