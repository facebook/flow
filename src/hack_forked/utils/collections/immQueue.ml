(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t = {
  incoming: 'a list;
  outgoing: 'a list;
  length: int;
}

exception Empty

let empty = { incoming = []; outgoing = []; length = 0 }

let length t = t.length

let is_empty t = length t = 0

let push t x = { t with incoming = x :: t.incoming; length = t.length + 1 }

let prepare_for_read t =
  match t.outgoing with
  | [] -> { t with incoming = []; outgoing = List.rev t.incoming }
  | _ -> t

let pop t =
  let t = prepare_for_read t in
  match t.outgoing with
  | [] -> (None, t)
  | hd :: tl -> (Some hd, { t with outgoing = tl; length = t.length - 1 })

let peek t =
  let t = prepare_for_read t in
  match t.outgoing with
  | [] -> (None, t)
  | hd :: _ -> (Some hd, t)

let pop_unsafe t =
  match pop t with
  | (Some x, t) -> (x, t)
  | (None, _) -> raise Empty

let exists t ~f = List.exists f t.outgoing || List.exists f t.incoming

let iter t ~f =
  List.iter f t.outgoing;
  List.iter f (List.rev t.incoming)

let from_list x = { incoming = []; outgoing = x; length = List.length x }

let to_list x = x.outgoing @ List.rev x.incoming

let concat t =
  {
    incoming = [];
    outgoing = Base.List.concat_map ~f:to_list t;
    length = List.map (fun u -> u.length) t |> List.fold_left ( + ) 0;
  }
