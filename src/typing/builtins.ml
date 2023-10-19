(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = (Reason.name, Type.t lazy_t) Hashtbl.t

let builtin_set builtins =
  Hashtbl.fold (fun k _ acc -> NameUtils.Set.add k acc) builtins NameUtils.Set.empty

let get_builtin_opt builtins name = Hashtbl.find_opt builtins name |> Option.map Lazy.force

let get_builtin builtins name ~on_missing =
  match Hashtbl.find_opt builtins name with
  | None -> on_missing ()
  | Some t -> Ok (Lazy.force t)

let set_builtin builtins name t =
  match Hashtbl.find_opt builtins name with
  | None -> Hashtbl.add builtins name t
  | Some _ -> failwith ("Should have been excluded: " ^ Reason.display_string_of_name name)

let empty () : t = Hashtbl.create 0

let optimize_entries builtins ~optimize =
  Hashtbl.iter
    (fun name t ->
      let entry' = optimize (Lazy.force t) in
      Hashtbl.replace builtins name (lazy entry'))
    builtins
