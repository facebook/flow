(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type entry =
  | NotYetWritten of Type.t
  | Entry of Type.t

type t = (Reason.name, entry) Hashtbl.t

let add_not_yet_seen_builtin builtins name t = Hashtbl.add builtins name (NotYetWritten t)

let get_builtin builtins name ~on_missing =
  match Hashtbl.find_opt builtins name with
  | None -> on_missing ()
  | Some (NotYetWritten t)
  | Some (Entry t) ->
    t

(* Having to pass flow_t is not ideal. In a more ideal world, we should be able to functorize
 * this module over FLOW_COMMON so that we could call flow_t directly. Unfortunately, even that
 * would not be enough. This module is also used in Context, so we cannot have its signature depend
 * on Context.t. Since there is no reasonable solution here, we require a flow_t argument that
 * already has the context eliminated from its signature.
 *
 * Why do we even need to run a Flow in here? Well, we need to be able to always pass back a Type.t
 * when using get_builtin. In cases where we have not yet seen the symbol we are requesting, our
 * solution is to pass back an OpenT that will eventually resolve to that symbol when it is found.
 *)
let set_builtin ~flow_t builtins name t =
  match Hashtbl.find_opt builtins name with
  | None -> Hashtbl.add builtins name (Entry t)
  | Some (NotYetWritten tvar) ->
    flow_t (t, tvar);
    Hashtbl.replace builtins name (Entry tvar)
  | Some (Entry _) -> failwith "Should have been excluded"

let empty () : t = Hashtbl.create 0

let map_entries builtins ~f =
  Hashtbl.iter
    (fun name entry ->
      let entry' =
        match entry with
        | NotYetWritten t -> NotYetWritten (f t)
        | Entry t -> Entry (f t)
      in
      Hashtbl.replace builtins name entry')
    builtins
