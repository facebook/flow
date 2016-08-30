(*****************************************************************************)
(* Exceptions *)
(*****************************************************************************)
exception Error_name_already_bound of string * Pos.t * Pos.t
exception Error_unbound_name of string * Pos.t

(*****************************************************************************)
(* Set/Map etc ... *)
(*****************************************************************************)

module SSet = struct
  include Set.Make(String)

  let of_list l =
    List.fold_left (fun acc name -> add name acc) empty l

end

module IMap = struct

  include Map.Make(struct
    type t = int
    let compare = (-)
  end)

  let get x t = try Some (find x t) with Not_found -> None
  let values t = List.rev (fold (fun _ x y -> x :: y) t [])

  let of_list l =
    List.fold_left (fun acc (k, v) -> add k v acc) empty l

  let merge_fst m1 m2 =
    merge (fun _ vo1 vo2 -> match vo1, vo2 with
    | Some _, _ -> vo1
    | _, Some _ -> vo2
    | None, None -> assert false)
    m1 m2
end

module ISet = struct

  include Set.Make(struct
    type t = int
    let compare = (-)
  end)
end

module SMap = struct
  include Map.Make(String)
  let get x map = try Some (find x map) with Not_found -> None
  let union map1 map2 = fold add map1 map2

  let get_list x map =
    try find x map with Not_found -> []

  let add_list x y map =
    let l = get_list x map in
    let l = y :: l in
    add x l map

  let get_smap x map =
    try find x map with Not_found -> empty

  let get_sset x map =
    try find x map with Not_found -> SSet.empty

  let map_fold f acc m =
    fold begin fun x v (acc, result) ->
      let acc, v = f acc v in
      let result = add x v result in
      acc, result
    end m (acc, empty)

  let key_set t = fold (fun k _ y -> SSet.add k y) t SSet.empty
  let values t = List.rev (fold (fun _ x y -> x :: y) t [])
  let key_values t = List.rev (fold (fun k x y -> (k, x) :: y) t [])

end

(*****************************************************************************)
(* A string map where all the elements are unique.
 * An error is thrown when that is not the case.
 *)
(*****************************************************************************)
module UMap: sig
  type 'a t
  val empty: 'a t
  val cardinal: 'a t -> int
  val is_empty: 'a t -> bool
  val (@@): ((Pos.t * string) * 'a) -> 'a t -> 'a t
  val add: (Pos.t * string) -> 'a -> 'a t -> 'a t
  val remove: string -> 'a t -> 'a t
  val remove_pos: (Pos.t * string) -> 'a t -> 'a t
  val replace: string -> 'a -> 'a t -> 'a t
  val add_replace: (Pos.t * string) -> 'a -> 'a t -> 'a t
  val mem: string -> 'a t -> bool
  val mem_pos: (Pos.t * string) -> 'a t -> bool
  val get: string -> 'a t -> 'a option
  val get_with_pos : string -> 'a t -> (Pos.t * 'a) option
  val get_pos: (Pos.t * string) -> 'a t -> 'a option
  val find: (Pos.t * string) -> 'a t -> 'a
  val find_unsafe: string -> 'a t -> 'a
  val get_list: string -> 'a list t -> 'a list
  val get_sset: string -> SSet.t t -> SSet.t
  val get_sset_pos: (Pos.t * string) -> SSet.t t -> SSet.t
  val union: 'a t -> 'a t -> 'a t
  val iter: (string -> 'a -> unit) -> 'a t -> unit
  val iter_pos: ((Pos.t * string) -> 'a -> unit) -> 'a t -> unit
  val fold: (string -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_pos: ((Pos.t * string) -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map: ('a -> 'b) -> 'a t -> 'b t
  val mapi: ((Pos.t * string) -> 'a -> 'b) -> 'a t -> 'b t
  val values: 'a t -> 'a list
  val key_values: 'a t -> (string * 'a) list
  val key_pos_values: 'a t -> ((Pos.t * string) * 'a) list
  val map_fold: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  val mapi_fold: ('acc -> (Pos.t * string) -> 'a -> 'acc * 'b)
      -> 'acc -> 'a t -> 'acc * 'b t
  val merge:
      ('acc -> 'a -> 'a -> 'acc * 'a) -> 'acc -> 'a t -> 'a t -> 'acc * 'a t
  val filter: (string -> 'a -> bool) -> 'a t -> 'a t
  val key_list: 'a t -> string list
  val key_set: 'a t -> SSet.t
  val for_all : (string -> 'a -> bool) -> 'a t -> bool
  val singleton : (Pos.t * string) -> 'a -> 'a t
 end = struct

  type 'a t = (Pos.t * 'a) SMap.t

  let empty = SMap.empty

  let cardinal = SMap.cardinal

  let add (pos, name) value map =
    if SMap.mem name map
    then
      let old_pos, _ = SMap.find name map in
      raise (Error_name_already_bound (name, pos, old_pos))
    else SMap.add name (pos, value) map

  let remove = SMap.remove
  let remove_pos (_, x) y = SMap.remove x y

  let replace name value map =
    try
      let pos, _ = SMap.find name map in
      SMap.add name (pos, value) map
    with Not_found ->
      failwith "Trying to replace a non-existent value"

  let add_replace (pos, name) value map =
    SMap.add name (pos, value) map

  let mem = SMap.mem
  let mem_pos (_, x) t = SMap.mem x t

  let (@@) (name, value) map = add name value map

  let union map1 map2 =
    SMap.fold begin fun name (pos, value) acc ->
      add (pos, name) value acc
    end map1 map2

  let get x t = try Some (snd (SMap.find x t)) with Not_found -> None
  let get_with_pos x t = try Some (SMap.find x t) with Not_found -> None
  let get_pos (_, x) t = try Some (snd (SMap.find x t)) with Not_found -> None
  let get_list x t = try snd (SMap.find x t) with Not_found -> []
  let get_sset x t = try snd (SMap.find x t) with Not_found -> SSet.empty
  let get_sset_pos (_, x) t = get_sset x t

  let find (pos, x) t =
    try snd (SMap.find x t) with Not_found ->
      raise (Error_unbound_name (x, pos))

  let find_unsafe x t = snd (SMap.find x t)

  let iter f = SMap.iter begin fun x (_, y) -> f x y end
  let iter_pos f = SMap.iter begin fun x (pos, y) -> f (pos, x) y end

  let fold f map acc =
    SMap.fold begin fun name (_, value) acc ->
      f name value acc
    end map acc

  let fold_pos f map acc =
    SMap.fold begin fun name (pos, value) acc ->
      f (pos, name) value acc
    end map acc

  let map f t =
    SMap.map begin fun (pos, value) ->
      let value = f value in
      pos, value
    end t

  let mapi f t =
    SMap.mapi begin fun key (pos, value) ->
      let value = f (pos, key) value in
      pos, value
    end t

  let values t = List.rev (fold (fun _ x y -> x :: y) t [])
  let key_values t = List.rev (fold (fun k x y -> (k, x) :: y) t [])
  let key_pos_values t = List.rev (fold_pos (fun k x y -> (k, x) :: y) t [])

  let is_empty = SMap.is_empty

  let map_fold f acc m =
    SMap.fold begin fun x (pos, v) (acc, result) ->
      let acc, v = f acc v in
      let result = SMap.add x (pos, v) result in
      acc, result
    end m (acc, empty)

  let mapi_fold f acc m =
    SMap.fold begin fun x (pos, v) (acc, result) ->
      let acc, v = f acc (pos, x) v in
      let result = SMap.add x (pos, v) result in
      acc, result
    end m (acc, empty)

  let merge f acc m1 m2 =
    let acc, m =
      fold_pos begin fun name value1 (acc, m) ->
        match get (snd name) m2 with
        | None -> acc, m
        | Some value2 ->
            let acc, value_ = f acc value1 value2 in
            let m = add_replace name value_ m in
            acc, m
      end m1 (acc, m1)
    in
    let m =
      fold_pos begin fun name value2 m ->
        if not (mem (snd name) m1)
        then add_replace name value2 m
        else m
      end m2 m
    in
    acc, m

  let filter p m = SMap.filter (fun s (_, v) -> p s v) m
  let key_list t = List.map fst (key_values t)
  let key_set t = SSet.of_list (key_list t)

  let for_all p m = SMap.for_all (fun s (_, v) -> p s v) m

  let singleton (pos, s) v = SMap.singleton s (pos, v)

end
