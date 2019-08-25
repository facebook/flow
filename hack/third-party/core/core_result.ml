module Stable = struct
  module V1 = struct
    type ('a, 'b) t = ('a, 'b) Pervasives.result =
      | Ok of 'a
      | Error of 'b
  end
end

include Stable.V1

type ('a, 'b) _t = ('a, 'b) t

include Monad.Make2 (struct
  type ('a, 'b) t = ('a, 'b) _t

  let bind x f =
    match x with
    | Error _ as x -> x
    | Ok x -> f x

  let map x ~f =
    match x with
    | Error _ as x -> x
    | Ok x -> Ok (f x)

  let map = `Custom map

  let return x = Ok x
end)

let fail x = Error x

let failf format = Printf.ksprintf fail format

(* This definition shadows the version created by the functor application above, but it
   is much more efficient. *)
let map t ~f =
  match t with
  | Ok x -> Ok (f x)
  | Error _ as x -> x

let map_error t ~f =
  match t with
  | Ok _ as x -> x
  | Error x -> Error (f x)

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok = function
  | Ok x -> Some x
  | Error _ -> None

let error = function
  | Ok _ -> None
  | Error x -> Some x

let of_option opt ~error =
  match opt with
  | Some x -> Ok x
  | None -> Error error

let iter v ~f =
  match v with
  | Ok x -> f x
  | Error _ -> ()

let iter_error v ~f =
  match v with
  | Ok _ -> ()
  | Error x -> f x

let ok_fst = function
  | Ok x -> `Fst x
  | Error x -> `Snd x

let ok_if_true bool ~error =
  if bool then
    Ok ()
  else
    Error error

let try_with f = (try Ok (f ()) with exn -> Error exn)

let ok_unit = Ok ()

let ok_exn = function
  | Ok x -> x
  | Error exn -> raise exn

let ok_or_failwith = function
  | Ok x -> x
  | Error str -> failwith str

module Export = struct
  type ('ok, 'err) _result = ('ok, 'err) t =
    | Ok of 'ok
    | Error of 'err

  let is_error = is_error

  let is_ok = is_ok
end

let combine t1 t2 ~ok ~err =
  match (t1, t2) with
  | (Ok _, Error e)
  | (Error e, Ok _) ->
    Error e
  | (Ok ok1, Ok ok2) -> Ok (ok ok1 ok2)
  | (Error err1, Error err2) -> Error (err err1 err2)
