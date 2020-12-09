(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Maps filenames to info about a module, including the module's name.
    note: currently we may have many files for one module name.
    this is an issue. *)

(* shared heap for package.json tokens by filename *)
module PackageHeap =
  SharedMem.WithCache
    (StringKey)
    (struct
      type t = (Package_json.t, unit) result

      let description = "Package"
    end)

(* shared heap for package.json directories by package name *)
module ReversePackageHeap =
  SharedMem.WithCache
    (StringKey)
    (struct
      type t = string

      let description = "ReversePackage"
    end)

(* Flow doesn't support incrementally changing the package heaps, so we don't need to add this to
 * a transaction *)
module Package_heap_mutator : sig
  val add_package_json : string -> Package_json.t -> unit

  val add_error : string -> unit
end = struct
  let add_package_json filename package_json =
    PackageHeap.add filename (Ok package_json);
    match Package_json.name package_json with
    | Some name -> ReversePackageHeap.add name (Filename.dirname filename)
    | None -> ()

  let add_error filename = PackageHeap.add filename (Error ())
end

module type READER = sig
  type reader

  val get_package : reader:reader -> string -> (Package_json.t, unit) result option

  val get_package_directory : reader:reader -> string -> string option
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t = struct
  type reader = Mutator_state_reader.t

  let get_package ~reader:_ = PackageHeap.get

  let get_package_directory ~reader:_ = ReversePackageHeap.get
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  (* We don't support incrementally updating the package heaps, so we never actually oldify
   * anything. Therefore we always can read from the package heap directly *)
  let get_package ~reader:_ = PackageHeap.get

  let get_package_directory ~reader:_ = ReversePackageHeap.get
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_package ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package ~reader
    | State_reader reader -> Reader.get_package ~reader

  let get_package_directory ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package_directory ~reader
    | State_reader reader -> Reader.get_package_directory ~reader
end

(******************** APIs for saving/loading saved state *********************)

module For_saved_state = struct
  exception Package_not_found of string

  let get_package_json_unsafe file =
    match PackageHeap.get file with
    | Some package -> package
    | None -> raise (Package_not_found file)
end
