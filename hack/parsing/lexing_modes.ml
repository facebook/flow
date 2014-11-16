(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type mode =
  | Toplevel
  | Class
  | ClassKw
  | Function
  | Expr
  | Attr
  | Xhp
  | Enum
  | UAttr
  | Initializer
  | Type

let string = function
  | Toplevel -> "Toplevel"
  | Class -> "Class"
  | ClassKw -> "ClassKw"
  | Function -> "Function"
  | Expr -> "Expr"
  | Attr -> "Attr"
  | Xhp -> "Xhp"
  | Enum -> "Enum"
  | UAttr -> "UAttr"
  | Initializer -> "Initializer"
  | Type -> "Type"

type t = mode list ref
let debug = false
let empty() = ref [Toplevel]

let o stack s =
  if debug then begin
    List.iter (fun _ -> Printf.printf " ") !stack;
    print_string s;
    print_newline();
  end

let p stack =
  if debug then begin
    Printf.printf "Stack: ";
    (List.iter (fun x -> Printf.printf "%s " (string x)) !stack);
    Printf.printf "\n"
  end

(* We check we are at toplevel because the keyword class could be used
   in a different context
 *)
let in_class stack =
  stack :=
    match !stack with
    | [Toplevel] as st ->
        o stack "Entering class";
        ClassKw :: st
    | s -> s

let in_type stack =
  stack :=
    match !stack with
    | [Toplevel] as st ->
        o stack "Entering Type";
        Type :: st
    | s -> s

let in_uattr stack =
  match !stack with
  | Toplevel :: _
  | Class :: _
  | Function :: _ ->
    o stack "Entering user attribute";
    stack := UAttr :: !stack
  | _ -> ()


let in_enum stack =
  stack :=
    match !stack with
    | Class :: _ ->
        o stack "Entering Enum def";
        Enum :: !stack
    | s -> s

let in_function stack =
  stack :=
    match !stack with
    | (Toplevel | Class) :: _ as st ->
        o stack "Entering function";
        Function :: st
    | Expr :: _ as st ->
        o stack "Entering function";
        Function :: st
    | s -> s

let in_block stack =
  stack :=
    match !stack with
    | Function :: _ ->
        o stack "Entering Function Body";
        Expr :: !stack
    | (Attr | Xhp | Expr ) :: _ ->
        o stack "Entering Block";
        Expr :: !stack
    | ClassKw :: st ->
        Class :: st
    | Class :: _
    | Initializer :: _ ->
        o stack "Entering initializer";
        Initializer :: !stack
    | _ -> !stack

let out_block stack =
  stack :=
    (match !stack with
    | Class :: st ->
        o stack "Leaving class";
        st
    | Enum :: st ->
        o stack "Leaving Enum";
        st
    | Expr :: Function :: st ->
        o stack "Leaving Function Body";
        st
    | Expr :: st ->
        o stack "Leaving Block";
        st
    | Initializer :: st ->
        o stack "Leaving initializer";
        st
    | st -> st)

let out_uattr stack =
  stack :=
    match !stack with
    | UAttr :: tail -> tail
    | _ as st -> st

let semi stack =
  stack :=
    match !stack with
    | Function :: st ->
        o stack "Function has no body";
        st
    | Type :: st ->
        o stack "Leaving Type";
        st
    | st -> st

let otag stack =
  stack :=
    match !stack with
    | (Toplevel | Expr | Xhp) :: _ ->
        o stack "Entering xhp attributes";
        Attr :: !stack
    | _ -> !stack

let gt stack =
  stack :=
    match !stack with
    | Attr :: st ->
        o stack "Leaving xhp attributes";
        o stack "Entering XHP";
        p stack;
        Xhp :: st
    | st -> st

let slash_gt stack =
  stack :=
    match !stack with
    | Attr :: st ->
        o stack "Leaving xhp attributes";
        st
    | st -> st

let ctag stack =
  stack :=
    match !stack with
    | Xhp :: st ->
        o stack "Leaving XHP";
        p stack;
        st
    | st -> st

let is_in_class stack =
  match !stack with
  | Class :: _ -> true
  | _ -> false

let is_in_uattr stack =
  match !stack with
  | UAttr :: _ -> true
  | _ -> false

let is_xhp stack =
  match !stack with
  | Xhp :: _ -> true
  | _ -> false

let could_be_xhp stack =
  match !stack with
  | (Toplevel | Expr | Xhp) :: _ -> true
  | _ -> false

let below_attr stack =
  match !stack with
  | Attr :: _ -> true
  | Enum :: _ -> true
  | _ -> false

let get_modes stack = !stack
let get_mode stack = List.hd !stack
