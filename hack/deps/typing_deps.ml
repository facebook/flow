(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Utils

(**********************************)
(* Handling dependencies *)
(**********************************)

module Dep = struct
  type variant =
    (* GConst is used for "global" constants, in other words,
     * constants that were introduced using "const X = ..." or
     * "define('X', ...)".
     *)
    | GConst of string
    | GConstName of string

    (* Const is used to represent class constants. *)
    | Const of string * string

    | Class of string
    | Fun of string
    | FunName of string
    | Prop of string * string
    | SProp of string * string
    | Method of string * string
    | SMethod of string * string
    | Cstr of string
    | Extends of string

  type t = int

  (* 30 bits for the hash and 1 bit to determine if something is a class *)
  let mask = 1 lsl 30 - 1

  let make = function
    | Class class_name ->
        let h = Hashtbl.hash class_name land mask in
        let h = h lsl 1 in
        let h = h lor 1 in
        h
    | Extends class_name ->
        let h = Hashtbl.hash class_name land mask in
        let h = h lsl 1 in
        h
    | variant ->
        let h = Hashtbl.hash variant land mask in
        let h = h lsl 1 in
        h

  let is_class x = x land 1 = 1
  let extends_of_class x = x lxor 1

  let compare = (-)

  let to_string = function
    | GConst s -> "GConst "^s
    | GConstName s -> "GConstName "^s
    | Const (cls, s) -> spf "Const %s::%s" cls s
    | Class s -> "Class "^s
    | Fun s -> "Fun "^s
    | FunName s -> "FunName "^s
    | Prop (cls, s) -> spf "Prop %s::%s" cls s
    | SProp (cls, s) -> spf "SProp %s::%s" cls s
    | Method (cls, s) -> spf "Method %s::%s" cls s
    | SMethod (cls, s) -> spf "SMethod %s::%s" cls s
    | Cstr s -> "Cstr "^s
    | Extends s -> "Extends "^s

end

module DepSet = Set.Make (Dep)

(****************************************************************************)
(* Module for a compact graph. *)
(* Please consult hh_shared.c for the underlying representation. *)
(****************************************************************************)
module Graph = struct
  external hh_add_dep: int -> unit     = "hh_add_dep"
  external hh_get_dep: int -> int list = "hh_get_dep"

  let add x y = hh_add_dep ((x lsl 31) lor y)

  let get x =
    let l = hh_get_dep x in
    List.fold_left l ~f:begin fun acc node ->
      DepSet.add node acc
    end ~init:DepSet.empty
end

(*****************************************************************************)
(* Module keeping track of what object depends on what. *)
(*****************************************************************************)
let trace = ref true

(* Instead of actually recording the dependencies in shared memory, we record
 * string representations of them for printing out *)
let debug_trace = ref false
let dbg_dep_set = HashSet.create 0

let add_idep root obj =
  if !trace then Graph.add (Dep.make obj) (Dep.make root);
  if !debug_trace then
    (* Note: this is the inverse of what is actually stored in the shared
     * memory table. I find it easier to read "X depends on Y" instead of
     * "Y is a dependent of X" *)
    HashSet.add dbg_dep_set
      ((Dep.to_string root) ^ " -> " ^ (Dep.to_string obj))

let dump_deps oc =
  let xs = HashSet.fold (fun x xs -> x :: xs) dbg_dep_set [] in
  let xs = List.sort String.compare xs in
  List.iter xs print_endline

let get_ideps_from_hash x =
  Graph.get x

let get_ideps x =
  Graph.get (Dep.make x)

(* Gets ALL the dependencies ... hence the name *)
let get_bazooka x =
  match x with
  | Dep.Const (cid, _)
  | Dep.Prop (cid, _)
  | Dep.SProp (cid, _)
  | Dep.Method (cid, _)
  | Dep.Cstr cid
  | Dep.SMethod (cid, _)
  | Dep.Extends cid
  | Dep.Class cid -> get_ideps (Dep.Class cid)
  | Dep.Fun fid -> get_ideps (Dep.Fun fid)
  | Dep.FunName fid -> get_ideps (Dep.FunName fid)
  | Dep.GConst cid -> get_ideps (Dep.GConst cid)
  | Dep.GConstName cid -> get_ideps (Dep.GConstName cid)

(*****************************************************************************)
(* Module keeping track which files contain the toplevel definitions. *)
(*****************************************************************************)

let (ifiles: (Dep.t, Relative_path.Set.t) Hashtbl.t ref) = ref (Hashtbl.create 23)

let get_files deps =
  DepSet.fold begin fun dep acc ->
    try
      let files = Hashtbl.find !ifiles dep in
      Relative_path.Set.union files acc
    with Not_found -> acc
  end deps Relative_path.Set.empty

let update_files fast =
  Relative_path.Map.iter fast begin fun filename info ->
    let {FileInfo.funs; classes; typedefs;
         consts = _ (* TODO probably a bug #3844332 *);
         comments = _;
         file_mode = _;
         consider_names_just_for_autoload = _;
        } = info in
    let funs = List.fold_left funs ~f:begin fun acc (_, fun_id) ->
      DepSet.add (Dep.make (Dep.Fun fun_id)) acc
    end ~init:DepSet.empty in
    let classes = List.fold_left classes ~f:begin fun acc (_, class_id) ->
      DepSet.add (Dep.make (Dep.Class class_id)) acc
    end ~init:DepSet.empty in
    let classes = List.fold_left typedefs ~f:begin fun acc (_, type_id) ->
      DepSet.add (Dep.make (Dep.Class type_id)) acc
    end ~init:classes in
    let defs = DepSet.union funs classes in
    DepSet.iter begin fun def ->
      let previous =
        try Hashtbl.find !ifiles def with Not_found -> Relative_path.Set.empty
      in
      Hashtbl.replace !ifiles def (Relative_path.Set.add previous filename)
    end defs
  end
