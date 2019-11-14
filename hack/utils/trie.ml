(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

open Hh_core

(* Utility functions *)

let make_pair (a : 'a) (b : 'b) : 'a * 'b = (a, b)

let common_prefix (s1 : string) (s2 : string) : int =
  let i = ref 0 in
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  while !i < l1 && !i < l2 && s1.[!i] = s2.[!i] do
    i := !i + 1
  done;
  !i

let drop s c =
  let l = String.length s in
  String.sub s c (l - c)

let take s c = String.sub s 0 c

let ( |> ) (o : 'a) (f : 'a -> 'b) : 'b = f o

let id (x : 'a) : 'a = x

type 'a return = { return: 'b. 'a -> 'b }

let with_return (type t) (f : _ -> t) =
  let module Capture = struct
    exception Return of t
  end in
  let return = { return = (fun x -> raise (Capture.Return x)) } in
  (try f return with Capture.Return x -> x)

(* Trie implementation *)

type 'a t =
  | Leaf of 'a
  | Node of 'a t SMap.t ref

let create () : 'a t = Node (ref SMap.empty)

exception Inconsistent_trie of string

let get_node (trie : 'a t) : 'a t SMap.t ref =
  match trie with
  | Node n -> n
  | _ -> raise (Inconsistent_trie "Cannot match to leaf")

let get_leaf (trie : 'a t) : 'a =
  match trie with
  | Leaf v -> v
  | _ -> raise (Inconsistent_trie "Cannot match to node")

(* Match a string s with a key; return a tuple:
    i : int    -- position where the match ends
    k : string -- the full key matched
    n : 'a t   -- the node associated with key k
*)
let trie_assoc_partial (trie : 'a t) (w : string) : (int * string * 'a t) option
    =
  with_return (fun e ->
      !(get_node trie)
      |> SMap.iter (fun key elt ->
             let c = common_prefix key w in
             if (not (c = 0)) || (key = "" && w = "") then
               e.return (Some (c, key, elt)));
      None)

let rec mem (trie : 'a t) (w : string) : bool =
  with_return (fun e ->
      let (i, key, child) =
        match trie_assoc_partial trie w with
        | Some x -> x
        | None -> e.return false
      in
      if key = "" then e.return true;

      if String.length key = i then e.return (mem child (drop w i));

      false)

let add_one (node : 'a t) (c : string) (inner : 'a t) : unit =
  let elts = get_node node in
  elts := SMap.add c inner !elts

(* split key in position c, put left part as new key to a new node n
 * and put right part as child of n, then return n *)
let split_key (parent : 'a t) (key : string) (child : 'a t) (c : int) : 'a t =
  let left_key = take key c in
  let right_key = drop key c in
  let parent_list = get_node parent in
  parent_list := SMap.remove key !parent_list;

  let n = create () in
  add_one parent left_key n;
  add_one n right_key child;
  n

let add_leaf (node : 'a t) (key : string) (v : 'a) : unit =
  let leaf =
    match key with
    | "" -> Leaf v
    | _ ->
      let res = create () in
      add_one res "" (Leaf v);
      res
  in
  add_one node key leaf

let rec add
    ?(if_exist : 'b -> 'a -> unit = (fun _ _ -> ()))
    ~(transform : 'a -> 'b)
    (trie : 'b t)
    (w : string)
    (v : 'a) : unit =
  with_return (fun e ->
      let (c, key, child) =
        match trie_assoc_partial trie w with
        | Some x -> x
        | None -> e.return (add_leaf trie w (transform v))
      in
      if String.length key = c && w = "" then
        (* leaf exists; use if_exists callback *)
        e.return (if_exist (get_leaf child) v);

      if c = String.length key then
        (* full key match; do final recursive call *)
        e.return (add child (drop w c) v ~if_exist ~transform);

      (* Partial match: need to split key with common parts *)
      let n = split_key trie key child c in
      add_leaf n (drop w c) (transform v))

let to_list
    (limit : int option)
    (trie : 'b t)
    (kmap : string -> 'a)
    (vmap : 'a -> 'b -> 'c) : 'c list =
  with_return (fun e ->
      let reslist = ref [] in
      let rescount = ref 0 in
      let more () =
        match limit with
        | Some i -> i > !rescount
        | None -> true
      in
      let rec to_list_aux t s =
        match t with
        | Leaf v ->
          if more () then (
            reslist := vmap (kmap s) v :: !reslist;
            incr rescount
          ) else
            e.return (List.rev !reslist)
        | Node cs ->
          SMap.fold (fun tail rhs _acc -> to_list_aux rhs (s ^ tail)) !cs ()
      in
      to_list_aux trie "";
      List.rev !reslist)

let find_impl
    ?(limit : int option = None)
    (exact : bool)
    (trie : 'a t)
    (pre : string)
    (vmap : string -> 'a -> 'c) : 'c list =
  with_return (fun e ->
      let append = ( ^ ) pre in
      let rec find_impl_aux trie p =
        let (c, key, child) =
          match trie_assoc_partial trie p with
          | Some x -> x
          | None -> e.return []
        in
        match (String.length key = c, (not exact) && String.length p = c) with
        | (true, _) when String.length p = 0 -> to_list limit child append vmap
        | (true, true) -> to_list limit child append vmap
        | (true, _) -> find_impl_aux child (drop p c)
        | (false, true) ->
          to_list limit child (fun k -> pre ^ drop key c ^ k) vmap
        | _ -> []
      in
      find_impl_aux trie pre)

let find (trie : 'a t) (s : string) : 'a =
  match find_impl true trie s make_pair with
  | (_s, v) :: _tl -> v
  | _ -> raise Not_found

let find_prefix (trie : 'a t) (s : string) (vmap : string -> 'a -> 'b) : 'b list
    =
  find_impl false trie s vmap

let find_prefix_limit
    (i : int) (trie : 'a t) (s : string) (vmap : string -> 'a -> 'b) : 'b list =
  find_impl false trie s vmap ~limit:(Some i)

let remove_one (trie : 'a t) (key : string) : unit =
  let elts = get_node trie in
  elts := SMap.remove key !elts

let rec remove_impl (exact : bool) (trie : 'a t) (s : string) : unit =
  with_return (fun e ->
      let (c, key, child) =
        match trie_assoc_partial trie s with
        | Some x -> x
        | None -> e.return ()
      in
      match (String.length key = c, exact, String.length s = c) with
      | (true, true, true) when c = 0 -> remove_one trie (take s c)
      | (true, false, true) -> remove_one trie (take s c)
      | (true, _, _) -> remove_impl exact child (drop s c)
      | _ -> ())

let remove (trie : 'a t) (s : string) : unit = remove_impl true trie s

let remove_prefix (trie : 'a t) (s : string) : unit = remove_impl false trie s

(* let rec merge ?(if_exist : 'a -> 'a -> unit = fun _ _ -> ()) *)
(*               (trieDes : 'a t) *)
(*               (trieSrc : 'a t) : unit = *)
(*   let rec merge_one des s n = *)
(*     try *)
(*       let (c, key, child) = trie_assoc_partial des s in *)
(*       if String.length key = c then begin *)
(*         (\* matched whole key. *)
(*          * Either continue to merge child, if whole s matched, *)
(*          * or continue to match in des's path, if some s left*\) *)
(*         if (String.length s = c) && (not (s = "")) then *)
(*           merge child n ~if_exist *)
(*         else if (not (s = "")) then *)
(*           merge_one child (string_drop s c) n *)
(*         else *)
(*           (\* empty key match, means same value exist *)
(*            * resolve use if_exist *\) *)
(*           let desv = get_leaf child in *)
(*           let srcv = get_leaf n in *)
(*           if_exist desv srcv *)
(*         end *)
(*       else *)
(*         (\* partially match key, need to split key, and merge into *\) *)
(*         let common = string_take s c in *)
(*         remove_one des key; *)
(*         if (c = String.length s) then begin *)
(*           (\* when s is fully match, just take the node keyed by s *)
(*            * no need to create new one *\) *)
(*           add_one des common n; *)
(*           merge_one n (string_drop key c) child *)
(*         end *)
(*         else begin *)
(*           let t = create () in *)
(*           add_one t (string_drop key c) child; *)
(*           add_one t (string_drop s c) n; *)
(*           add_one des common t *)
(*         end *)
(*     with Not_found -> *)
(*        add_one des s n *)
(*     in *)
(*   let l = get_node trieSrc in *)
(*   List.iter (fun (s, n) -> merge_one trieDes s n) !l *)

let rec to_string_impl (buf : Buffer.t) (trie : 'a t) : unit =
  match trie with
  | Node elts ->
    SMap.fold
      (fun k v _ ->
        Printf.bprintf buf "%S:{" k;
        to_string_impl buf v;
        Printf.bprintf buf "}")
      !elts
      ()
  | Leaf _v -> ()

let to_string (trie : 'a t) : string =
  let buf = Buffer.create 250 in
  to_string_impl buf trie;
  Buffer.contents buf
