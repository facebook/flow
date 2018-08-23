(**
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

module Make(Ord: Set.OrderedType) = struct
  type elt = Ord.t
  type t = {
    mutable __queue: elt option array;
    mutable size: int;
  }

  let rec make_empty n = {
    __queue = Array.make n None;
    size = 0;
  }

  and is_empty t = t.size = 0

  and pop t =
    if t.size = 0 then failwith "Popping from an empty priority queue";
    let v = Array.get t.__queue 0 in
    t.size <- t.size - 1;

    if t.size <> 0 then begin
      let last = Array.get t.__queue t.size in
      t.__queue.(t.size) <- None;
      __bubble_down t.__queue t.size last 0;
    end;

    match v with
      | None -> failwith "Attempting to return a null value"
      | Some v -> v

  and push t element =
    if Array.length t.__queue = t.size then begin
      let new_queue = Array.make ((Array.length t.__queue) * 2 + 1) None in
      Array.blit t.__queue 0 new_queue 0 (Array.length t.__queue);
      t.__queue <- new_queue;
    end;

    t.__queue.(t.size) <- Some element;
    __bubble_up t.__queue t.size;
    t.size <- t.size + 1;
    ()

  and __swap arr i j =
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp

  and __bubble_up arr index =
    if index = 0 then ();
    let pindex = (index - 1) / 2 in
    match Array.get arr index, Array.get arr pindex with
      | None, _ | _, None ->
        failwith "Unexpected null index found when calling __bubble_up"
      | Some e, Some p ->
        if Ord.compare e p < 0 then begin
          __swap arr index pindex;
          __bubble_up arr pindex
        end;

  and __bubble_down arr size value index =
    let right_child_index = index * 2 + 2 in
    let left_child_index = right_child_index - 1 in

    if right_child_index < size then begin
      match
        Array.get arr right_child_index,
        Array.get arr left_child_index,
        value with
          | None, _, _ | _, None, _ | _, _, None ->
            failwith "Unexpected null index found when calling __bubble_down"
          | Some r, Some l, Some v ->
            let smaller_child, smaller_child_index =
              if Ord.compare r l < 0
              then r, right_child_index
              else l, left_child_index
            in

            if Ord.compare v smaller_child <= 0 then
              arr.(index) <- value
            else begin
              arr.(index) <- Array.get arr smaller_child_index;
              __bubble_down arr size value smaller_child_index
            end;
    end else if left_child_index < size then begin
      match Array.get arr left_child_index, value with
        | None, _ | _, None ->
          failwith "Unexpected null index found when calling __bubble_down"
        | Some l, Some v ->
          if Ord.compare v l <= 0 then
            arr.(index) <- value
          else begin
            arr.(index) <- Array.get arr left_child_index;
            arr.(left_child_index) <- value
          end
    end else arr.(index) <- value;

end
