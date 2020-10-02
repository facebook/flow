(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module IntMap = WrappedMap.Make (struct
  type t = int

  let compare x y = x - y
end)

let map_of_elements = List.fold_left (fun map (k, v) -> IntMap.add k v map) IntMap.empty

let test_WrappedMap_union () =
  let map1 = map_of_elements [(1, 2); (3, 4)] in
  let map2 = map_of_elements [(1, 10); (5, 6)] in
  let () =
    let union = IntMap.union map1 map2 in
    let expected = map_of_elements [(1, 2); (3, 4); (5, 6)] in
    if not (IntMap.equal ( = ) union expected) then failwith "Maps not equal"
  in
  let () =
    let union = IntMap.union ~combine:(fun _ _ snd -> Some snd) map1 map2 in
    let expected = map_of_elements [(1, 10); (3, 4); (5, 6)] in
    if not (IntMap.equal ( = ) union expected) then failwith "Maps not equal"
  in
  let () =
    let union = IntMap.union ~combine:(fun _ _ _ -> None) map1 map2 in
    let expected = map_of_elements [(3, 4); (5, 6)] in
    if not (IntMap.equal ( = ) union expected) then failwith "Maps not equal"
  in
  true

let test_ImmQueue () =
  let queue = ImmQueue.empty in
  if not (ImmQueue.is_empty queue) then failwith "not empty";
  let (x, queue) = ImmQueue.peek queue in
  if not (x = None) then failwith "peeking an empty queue should return None";
  let queue = ImmQueue.push queue 4 in
  if ImmQueue.is_empty queue then failwith "empty";
  let queue = ImmQueue.push queue 5 in
  if ImmQueue.length queue <> 2 then failwith "wrong length";
  let queue = ImmQueue.push queue 6 in
  let (x, queue) = ImmQueue.peek queue in
  (match x with
  | Some 4 -> ()
  | _ -> failwith "wrong value");
  let (x, queue) = ImmQueue.pop queue in
  (match x with
  | Some 4 -> ()
  | _ -> failwith "wrong value");
  let (x, queue) = ImmQueue.pop_unsafe queue in
  if x <> 5 then failwith "wrong value";
  let (x, queue) = ImmQueue.pop_unsafe queue in
  if x <> 6 then failwith "wrong value";
  let did_throw =
    try
      ignore (ImmQueue.pop_unsafe queue);
      false
    with ImmQueue.Empty -> true
  in
  if not did_throw then failwith "expected an exception";
  let (x, _) = ImmQueue.pop queue in
  match x with
  | Some _ -> failwith "expected none"
  | None ->
    ();

    let queue = ImmQueue.push (ImmQueue.push (ImmQueue.push ImmQueue.empty 1) 2) 3 in
    let (_, queue) = ImmQueue.pop queue in
    let queue = ImmQueue.push (ImmQueue.push queue 4) 5 in
    let acc = ref [] in
    ImmQueue.iter queue ~f:(fun i -> acc := !acc @ [i]);
    if !acc <> [2; 3; 4; 5] then failwith "expected 2345 iter order";
    if ImmQueue.to_list queue <> [2; 3; 4; 5] then failwith "expected 2345 list";

    let queue2 = ImmQueue.from_list [6; 7; 8] in
    let (_, queue2) = ImmQueue.pop queue2 in
    let queue2 = ImmQueue.push (ImmQueue.push queue2 9) 0 in
    let queue3 = ImmQueue.concat [queue; queue2] in
    if ImmQueue.to_list queue3 <> [2; 3; 4; 5; 7; 8; 9; 0] then failwith "expected 23457890 cat";

    true

let tests = [("test_WrappedMap_union", test_WrappedMap_union); ("test_ImmQueue", test_ImmQueue)]

let () = Unit_test.run_all tests
