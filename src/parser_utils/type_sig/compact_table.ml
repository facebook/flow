(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make () = struct
  type index = int [@@deriving show]

  (* Nodes form a doubly linked list. *)
  type 'a node = {
    mutable data: 'a;
    mutable next: 'a node;
    mutable prev: 'a node;
    mutable marked: bool;
    mutable index: index;
  }

  type 'a builder = 'a node option ref

  type 'a indexed = (int * 'a node) option

  type 'a t = 'a array [@@deriving show]

  let create () = ref None

  (* Note that the builder accumulates nodes in insertion order. *)
  let push b data =
    match !b with
    | None ->
      let rec head = { data; next = head; prev = head; marked = false; index = -1 } in
      b := Some head;
      head
    | Some head ->
      let last = head.prev in
      let node = { data; next = head; prev = last; marked = false; index = -1 } in
      head.prev <- node;
      last.next <- node;
      node

  let tail_exn b =
    match !b with
    | None -> failwith "tail_exn: empty builder"
    | Some head -> head.prev

  let splice into f =
    let b = create () in
    let x = f b in
    begin
      match !b with
      | None -> ()
      | Some head ->
        let last = head.prev in
        let next = into.next in
        head.prev <- into;
        into.next <- head;
        last.next <- next;
        next.prev <- last
    end;
    x

  let modify node f = node.data <- f node.data

  let mark node f =
    if not node.marked then begin
      node.marked <- true;
      f node.data
    end

  let merge_none _ _ = None

  let compact ?(merge = merge_none) =
    let rec loop head prev node =
      if node == head then begin
        head.prev <- prev;
        prev.next <- head;
        (prev.index + 1, head)
      end else if node.marked then begin
        match merge prev.data node.data with
        | None ->
          node.index <- prev.index + 1;
          node.prev <- prev;
          prev.next <- node;
          loop head node node.next
        | Some merged ->
          prev.data <- merged;
          node.index <- prev.index;
          loop head prev node.next
      end else
        loop head prev node.next
    in
    let rec loop0 head prev node =
      if node == head then
        None
      else if node.marked then begin
        node.index <- 0;
        node.prev <- prev;
        prev.next <- node;
        Some (loop node node node.next)
      end else
        loop0 head prev node.next
    in
    fun b ->
      match !b with
      | None -> None
      | Some head ->
        (* Reset builder. Can be re-used to build a new table. *)
        b := None;
        if head.marked then begin
          head.index <- 0;
          Some (loop head head head.next)
        end else
          loop0 head head head.next

  let index_exn node =
    (* If this is failing, the marking pass is probably missing something.
       See the comment at the top of `type_sig_pack *)
    assert (node.index >= 0);
    node.index

  external set : 'a t -> index -> 'a -> unit = "%array_unsafe_set"

  let copy =
    let rec loop f dst head node =
      if node == head then
        ()
      else
        let x = f node.data in
        set dst node.index x;
        loop f dst head node.next
    in
    fun f -> function
      | None -> [||]
      | Some (size, head) ->
        let x = f head.data in
        let dst = Array.make size x in
        loop f dst head head.next;
        dst

  let init = Array.init

  external get : 'a t -> index -> 'a = "%array_unsafe_get"

  let length = Array.length

  let iter = Array.iter

  let iteri = Array.iteri

  let map = Array.map

  let mapi = Array.mapi

  let to_array x = x

  let to_array_map = map

  module IndexSet = Set.Make (struct
    type t = index

    let compare a b = a - b
  end)
end
