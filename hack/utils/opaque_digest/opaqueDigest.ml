include Digest

let to_raw_contents x = x

(* take the raw contents of the Digest
   and convert it into a digest if it would be capable of being
   to_hex'd *)
let from_raw_contents x =
  try
    let (_ : string) = to_hex x in
    Some x
  with Invalid_argument _ -> None
