(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let () = Random.self_init ()
let debug = ref false
let profile = ref false

let log = ref (fun (_ : string)  -> ())

let d s =
  if !debug
  then begin
    print_string s;
    flush stdout;
  end

let dn s =
  if !debug
  then begin
    print_string s;
    print_newline();
    flush stdout;
  end

module String = struct
  include String
  let to_string x = x
end

module type MapSig = sig
  type +'a t
  type key

  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val mem: key -> 'a t -> bool
  val add: key -> 'a -> 'a t -> 'a t
  val get: key -> 'a t -> 'a option
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val remove: key -> 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  val find_unsafe: key -> 'a t -> 'a
  val is_empty: 'a t -> bool
  val union: 'a t -> 'a t -> 'a t
  val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal: 'a t -> int
  val compare: 'a t -> 'a t -> int
  val equal: 'a t -> 'a t -> bool
  val filter: (key -> 'a -> bool) -> 'a t -> 'a t
  val merge : (key -> 'a option -> 'b option -> 'c option)
    -> 'a t -> 'b t -> 'c t
  val choose : 'a t -> key * 'a
  val split: key -> 'a t -> 'a t * 'a option * 'a t
  val keys: 'a t -> key list
  val values: 'a t -> 'a list

  val map_env: ('c -> 'a -> 'c * 'b) -> 'c -> 'a t -> 'c * 'b t
  (* use only in testing code *)
  val elements: 'a t -> (key * 'a) list
end

module MyMap: functor (Ord: Map.OrderedType)
-> MapSig with type key = Ord.t
= functor (Ord: Map.OrderedType) -> struct
    include Map.Make(Ord)
    let get x t =
      try Some (find x t) with Not_found -> None

    let find_unsafe = find

    let union x y =
      fold add x y

    let cardinal m = fold (fun _ _ acc -> 1 + acc) m 0
    let compare x y = compare Pervasives.compare x y
    let equal x y = compare x y = 0

    let filter f m =
      fold begin fun x y acc ->
        if f x y then add x y acc else acc
      end m empty

    let keys m = fold (fun k v acc -> k :: acc) m []
    let values m = fold (fun k v acc -> v :: acc) m []
    let elements m = fold (fun k v acc -> (k,v)::acc) m []

    let map_env f env m =
      fold (
        fun x y (env, acc) ->
          let env, y = f env y in
          env, add x y acc
      ) m (env, empty)

  end

module SMap = MyMap(String)
module IMap = MyMap(Ident)
module ISet = Set.Make(Ident)
module SSet = Set.Make(String)
module CSet = Set.Make(Char)
module Map = struct end

(* HashSet is just a HashTable where the keys are actually the values, and we
 * ignore the actual values inside the HashTable. *)
module type HashSetSig = sig
  type 'a t

  val create: int -> 'a t
  val clear: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> 'a -> unit
  val mem: 'a t -> 'a -> bool
  val remove: 'a t -> 'a -> unit
  val iter: ('a -> unit) -> 'a t -> unit
  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
end

module HashSet = (struct
  type 'a t = ('a, unit) Hashtbl.t

  let create size = Hashtbl.create size
  let clear set = Hashtbl.clear set
  let copy set = Hashtbl.copy set
  let add set x = Hashtbl.replace set x ()
  let mem set x = Hashtbl.mem set x
  let remove set x = Hashtbl.remove set x
  let iter f set = Hashtbl.iter (fun k _ -> f k) set
  let fold f set acc = Hashtbl.fold (fun k _ acc -> f k acc) set acc
  let length set = Hashtbl.length set
end : HashSetSig)

let spf = Printf.sprintf
let print_endlinef fmt = Printf.ksprintf print_endline fmt
let prerr_endlinef fmt = Printf.ksprintf prerr_endline fmt

let opt f env = function
  | None -> env, None
  | Some x -> let env, x = f env x in env, Some x

let opt_map f = function
  | None -> None
  | Some x -> Some (f x)

let rec lmap f env l =
  match l with
  | [] -> env, []
  | x :: rl ->
      let env, x = f env x in
      let env, rl = lmap f env rl in
      env, x :: rl

let smap_inter m1 m2 =
  SMap.fold (
  fun x y acc ->
    if SMap.mem x m2
    then SMap.add x y acc
    else acc
 ) m1 SMap.empty

let imap_inter m1 m2 =
  IMap.fold (
  fun x y acc ->
    if IMap.mem x m2
    then IMap.add x y acc
    else acc
 ) m1 IMap.empty

let smap_union m1 m2 = SMap.fold SMap.add m1 m2
let imap_union m1 m2 = IMap.fold IMap.add m1 m2

let smap_inter_list = function
  | [] -> SMap.empty
  | x :: rl ->
      List.fold_left smap_inter x rl

let imap_inter_list = function
  | [] -> IMap.empty
  | x :: rl ->
      List.fold_left imap_inter x rl

(* This is a significant misnomer... you may want fold_left_env instead. *)
let lfold = lmap

let rec lfold2 f env l1 l2 =
  match l1, l2 with
  | [], [] -> env, []
  | [], _ | _, [] -> raise (Invalid_argument "lfold2")
  | x1 :: rl1, x2 :: rl2 ->
      let env, x = f env x1 x2 in
      let env, rl = lfold2 f env rl1 rl2 in
      env, x :: rl

let wlfold2 f env l1 l2 =
  match l1, l2 with
  | [], [] -> env, []
  | [], l | l, [] -> env, l
  | x1 :: rl1, x2 :: rl2 ->
      let env, x = f env x1 x2 in
      let env, rl = lfold2 f env rl1 rl2 in
      env, x :: rl

let rec wfold_left2 f env l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> env
  | x1 :: rl1, x2 :: rl2 ->
      let env = f env x1 x2 in
      wfold_left2 f env rl1 rl2

let apply_for_env_fold f env acc x =
  let env, x = f env x in
  env, x :: acc

let rec fold_left_env f env acc l =
  match l with
  | [] -> env, acc
  | x :: rl ->
      let env, acc = f env acc x in
      fold_left_env f env acc rl

let rec make_list f n =
  if n = 0
  then []
  else f() :: make_list f (n-1)

let safe_ios p s =
  try Some (int_of_string s)
  with _ -> None

let sl l =
  List.fold_right (^) l ""

let soi = string_of_int

let maybe f env = function
  | None -> ()
  | Some x -> f env x

(* Since OCaml usually runs w/o backtraces enabled, the note makes errors
 * easier to debug. *)
let unsafe_opt_note note = function
  | None -> raise (Invalid_argument note)
  | Some x -> x

let unsafe_opt x = unsafe_opt_note "unsafe_opt got None" x

let liter f env l = List.iter (f env) l

let inter_list = function
  | [] -> SSet.empty
  | x :: rl ->
      List.fold_left SSet.inter x rl

let rec list_last f1 f2 =
  function
    | [] -> ()
    | [x] -> f2 x
    | x :: rl -> f1 x; list_last f1 f2 rl

let rec uniq = function
  | [] -> []
  | [x] -> [x]
  | x :: (y :: _ as l) when x = y -> uniq l
  | x :: rl -> x :: uniq rl


let is_prefix_dir dir fn =
  let prefix = dir ^ Filename.dir_sep in
  String.length fn > String.length prefix &&
  String.sub fn 0 (String.length prefix) = prefix

let try_with_channel oc f1 f2 =
  try
    let result = f1 oc in
    close_out oc;
    result
  with e ->
    close_out oc;
    f2 e

let rec cut_after n = function
  | [] -> []
  | l when n <= 0 -> []
  | x :: rl -> x :: cut_after (n-1) rl

let iter_n_acc n f acc =
  let acc = ref acc in
  for i = 1 to n do
    acc := f !acc
  done;
  !acc

let set_of_list list =
  List.fold_right SSet.add list SSet.empty

(* \A\B\C -> A\B\C *)
let strip_ns s =
  if String.length s == 0 || s.[0] <> '\\' then s
  else String.sub s 1 ((String.length s) - 1)

(* \A\B\C -> C *)
let strip_all_ns s =
  try
    let base_name_start = String.rindex s '\\' + 1 in
    String.sub s base_name_start ((String.length s) - base_name_start)
  with Not_found -> s

let str_starts_with long short =
  try
    let long = String.sub long 0 (String.length short) in
    long = short
  with Invalid_argument _ ->
    false

let str_ends_with long short =
  try
    let len = String.length short in
    let long = String.sub long (String.length long - len) len in
    long = short
  with Invalid_argument _ ->
    false

(* Return a copy of the string with prefixing string removed.
 * The function is a no-op if it s does not start with prefix.
 * Modeled after Python's string.lstrip.
 *)
let lstrip s prefix =
  let prefix_length = String.length prefix in
  if str_starts_with s prefix
  then String.sub s prefix_length (String.length s - prefix_length)
  else s

let string_of_char = String.make 1

(*****************************************************************************)
(* Same as List.iter2, except that we only iterate as far as the shortest
 * of both lists.
 *)
(*****************************************************************************)

let rec iter2_shortest f l1 l2 =
  match l1, l2 with
  | [], _ | _, [] -> ()
  | x1 :: rl1, x2 :: rl2 -> f x1 x2; iter2_shortest f rl1 rl2

(* We may want to replace this with a tail-recursive map at some point,
 * factoring here so we have a clean way to grep. *)
let rev_rev_map f l = List.rev (List.rev_map f l)

let fold_fun_list acc fl =
  List.fold_left (|>) acc fl

let compose f g x = f (g x)

let with_context ~enter ~exit ~do_ =
  enter ();
  let result = try do_ () with e ->
    exit ();
    raise e in
  exit ();
  result
