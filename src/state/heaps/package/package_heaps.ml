(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
end = struct
  let add_package_json filename package_json =
    match Package_json.name package_json with
    | Some name -> ReversePackageHeap.add name (Filename.dirname filename)
    | None -> ()
end

module type READER = sig
  type reader

  val get_package_directory : reader:reader -> string -> string option
end

module Mutator_reader : READER with type reader = Mutator_state_reader.t = struct
  type reader = Mutator_state_reader.t

  let get_package_directory ~reader:_ = ReversePackageHeap.get
end

module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let get_package_directory ~reader:_ = ReversePackageHeap.get
end

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_package_directory ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package_directory ~reader
    | State_reader reader -> Reader.get_package_directory ~reader
end
