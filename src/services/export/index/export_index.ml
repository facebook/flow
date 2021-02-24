(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | Default
  | Named
  | NamedType
  | Namespace

and source =
  | Global
  | Builtin of string  (** [Builtin "foo"] refers to a `declare module "foo"` lib *)
  | File_key of File_key.t

and export = source * kind [@@deriving show, ord]

module ExportSet = struct
  include Set.Make (struct
    type t = export

    let compare = compare_export
  end)

  let pp fmt x =
    Format.fprintf fmt "@[<hv 2>{";
    let elements = elements x in
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    ignore
      (List.fold_left
         (fun sep elt ->
           if sep then Format.fprintf fmt ";@ ";
           pp_export fmt elt;
           true)
         false
         elements);
    (match elements with
    | [] -> ()
    | _ -> Format.fprintf fmt " ");
    Format.fprintf fmt "}@]"

  let show x = Format.asprintf "%a" pp x
end

type t = ExportSet.t SMap.t [@@deriving show]

let empty = SMap.empty

let add : string -> source -> kind -> t -> t =
  let add_file file_key kind = function
    | None -> Some (ExportSet.singleton (file_key, kind))
    | Some exports -> Some (ExportSet.add (file_key, kind) exports)
  in
  (fun name file_key kind t -> SMap.update name (add_file file_key kind) t)

let merge x y = SMap.union ~combine:(fun _key a b -> Some (ExportSet.union a b)) x y

let fold_names ~f ~init t = SMap.fold (fun name exports acc -> f acc name exports) t init

let fold ~f ~init t =
  fold_names
    ~f:(fun acc name exports -> ExportSet.fold (fun export acc -> f acc name export) exports acc)
    ~init
    t

let map ~f t = SMap.map (ExportSet.map f) t

let subtract old_t t =
  let (t, dead_names) =
    SMap.fold
      (fun name files_to_remove (t, dead_names) ->
        match SMap.find_opt name t with
        | Some files ->
          let updated = ExportSet.diff files files_to_remove in
          if ExportSet.is_empty updated then
            (SMap.remove name t, name :: dead_names)
          else
            (SMap.add name updated t, dead_names)
        | None -> (t, dead_names))
      old_t
      (t, [])
  in
  (t, dead_names)

(** [find name t] returns all of the [(file_key, kind)] tuples that export [name] *)
let find name (t : t) =
  match SMap.find_opt name t with
  | Some exports -> exports
  | None -> ExportSet.empty

let find_seq name t =
  match SMap.find_opt name t with
  | Some t -> ExportSet.to_seq t
  | None -> Seq.empty

(** [keys t] returns all of the exported names from every file in [t] *)
let keys t = SMap.keys t

let kind_is_value = function
  | Default
  | Named
  | Namespace ->
    true
  | NamedType -> false

let kind_is_type = function
  | Default
  | Named
  | Namespace ->
    false
  | NamedType -> true
