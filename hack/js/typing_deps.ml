(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*
 * This is how the javascript version of Hack figures out what dependencies
 * it needs.
 * On the IDE client, the user will open a file, and we'll try to typecheck it.
 * When we finished typechecking, we needed to know what dependencies were
 * missing (so we could pull them in)
 *
 * This was accomplished by replacing the Typing_deps module.
 * By replacing the model, we can find out the names of every
 * dependency the client has seen.
 *)

open Utils

(* If we're currently adding a dependency. This should be false if we're adding
 * a file we want to typecheck or autocomplete in. It should be true if it's
 * a dependency of a file we're typechecking or autocompleting *)
let is_dep = ref false

(**********************************)
(* Handling dependencies *)
(**********************************)

module Dep = struct
  type t =
    | GConst of string
    | GConstName of string
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

  type variant = t

  let compare = Pervasives.compare

  let to_string = function
    | GConst s -> "const:"^s
    | GConstName s -> "constname:"^s
    | Class s -> "class:"^s
    | Fun s -> "fun:"^s
    | FunName s -> "funname:"^s
    | Const (x, y) -> x^"::(const)"^y
    | Prop (x, y) -> x^"->$"^y
    | SProp (x, y) -> x^"::$"^y
    | Method (x, y) -> x^"->"^y
    | SMethod (x, y) -> x^"::"^y
    | Cstr s -> s^"->__construct"
    | Extends s -> "extends:"^s
end

module DSet = Set.Make(Dep)
module DMap = MyMap(Dep)

(*
let print_deps deps =
  Printf.printf "Deps: ";
  DSet.iter (fun x -> Printf.printf "%s " (Dep.to_string x)) deps;
  Printf.printf "\n"
*)

let merge_deps deps1 deps2 =
  DMap.fold begin fun x y acc ->
    match DMap.get x acc with
    | None -> DMap.add x y acc
    | Some y' ->
        DMap.add x (DSet.union y y') acc
  end deps1 deps2

let split_deps deps =
  let funs, classes = SSet.empty, SSet.empty in
  DSet.fold begin fun x (funs, classes) ->
    match x with
    | Dep.GConst s
    | Dep.GConstName s
    | Dep.Const (s, _)
    | Dep.Prop (s, _)
    | Dep.SProp (s, _)
    | Dep.Method (s, _)
    | Dep.SMethod (s, _)
    | Dep.Cstr s
    | Dep.Extends s
    | Dep.Class s -> funs, SSet.add s classes
    | Dep.FunName s
    | Dep.Fun s -> SSet.add s funs, classes
  end deps (funs, classes)

(*
let get_dep_id = function
  | Dep.Const (cid, _)
  | Dep.Class cid
  | Dep.Prop (cid, _)
  | Dep.SProp (cid, _)
  | Dep.Method (cid, _)
  | Dep.Cstr cid
  | Dep.SMethod (cid, _)
  | Dep.WClass cid -> cid
  | Dep.Fun fid -> fid
*)

(*****************************************************************************)
(* Module keeping track of what object depends on what.
 *)
(*****************************************************************************)

let (iclasses: string list SMap.t ref) = ref SMap.empty
let (ifuns: string list SMap.t ref) = ref SMap.empty

let get_list table x =
  match SMap.get x table with
  | None -> []
  | Some x -> x

let add_list table k v =
  let l = get_list table k in
  SMap.add k (v :: l) table

let add_files_report_changes fast =
  let has_changed = ref false in
  SMap.iter begin fun fn (funs_in_file, classes_in_file) ->
    List.iter begin fun (_, fid) ->
      if not (List.mem fn (get_list !ifuns fid))
      then begin
        has_changed := true;
        ifuns := add_list !ifuns fid fn;
      end
    end funs_in_file;
    List.iter begin fun (_, cid) ->
      if not (List.mem fn (get_list !iclasses cid))
      then begin
        has_changed := true;
        iclasses := add_list !iclasses cid fn;
      end
    end classes_in_file;
  end fast;
  !has_changed

let add_files fast =
  ignore (add_files_report_changes fast)

let get_files_to_rename fast =
  (* Adding the files where we have seen the global definition the last time
   * If I have a file defining class A. I want to re-check all the files
   * that we have seen previously defining the class A, to see if there is
   * any conflict.
   *)
  let acc = ref SSet.empty in
  let add l = List.iter (fun x -> acc := SSet.add x !acc) l in
  SMap.iter begin fun filename (fun_idl, class_idl) ->
    List.iter (fun (_, fid) -> add (get_list !ifuns fid)) fun_idl;
    List.iter (fun (_, cid) -> add (get_list !iclasses cid)) class_idl;
  end fast;
  let prev = SMap.fold (fun x _ acc -> SSet.add x acc) fast SSet.empty in
  SSet.diff !acc prev

let get_additional_files deps =
  let (funs, classes) = split_deps deps in
  let acc = SSet.empty in
  let add env x acc =
    let files = get_list env x in
    List.fold_left (fun acc x -> SSet.add x acc) acc files
  in
  let acc = SSet.fold (add !ifuns) funs acc in
  let acc = SSet.fold (add !iclasses) classes acc in
  acc

(*****************************************************************************)
(* Module keeping track of what object depends on what. *)
(*****************************************************************************)

let update_igraph deps =
  ()

let add_iedge obj root =
  ()

let deps = ref DSet.empty
let extends_igraph = Hashtbl.create 23

let add_idep root obj =
  match obj with
  | (Dep.FunName _ as x)
  | (Dep.Fun _ as x) -> deps := DSet.add x !deps
  | Dep.GConst s
  | Dep.GConstName s
  | Dep.Const (s, _)
  | Dep.Class s
  | Dep.Prop (s, _)
  | Dep.SProp (s, _)
  | Dep.Method (s, _)
  | Dep.SMethod (s, _)
  | Dep.Cstr s ->
        (* Say we're typechecking FileA and FileA has a dependency on FileB.
         * FileB has a dependency on FileC.
         *
         * When we typecheck FileA, we find out that we need FileB, so we
         * hh_add_dep FileB.
         *
         * When FileB gets named and decled, it adds FileC to our dep set.
         *
         * However, we don't actually need FileC, unless FileB is inheriting
         * from FileC.
         *
         * This If makes it so that FileC would not be added in this case.
         *)
      if not !is_dep then deps := DSet.add (Dep.Class s) !deps
  | Dep.Extends s ->
      (match root with
      | Some (Dep.Class root) ->
          (* We want to remember what needs to be udpated
           * if a super class changes, all the sub_classes
           * have to be updated.
           *)
          let iext =
            try Hashtbl.find extends_igraph s
            with Not_found -> SSet.empty
          in
          let iext = SSet.add root iext in
          Hashtbl.replace extends_igraph s iext
      | _ -> ());
      deps := DSet.add (Dep.Class s) !deps

let get_ideps x =
  DSet.empty

let get_bazooka x =
  DSet.empty

let update_files_info workers fast = raise Exit
let update_dependencies workers deps = raise Exit
