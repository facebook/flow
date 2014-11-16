(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
module List = List_ext

type unparsed =
  | StrEmpty
  | StrBlank (* Blank space *)
  | StrComma
  | StrSemi
  | StrComment of string
  | Str of string
  | StrList of unparsed list (* just a list of strings to be concatted *)
  | StrWords of unparsed list (* strings separated by spaces *)
  | StrStatement of unparsed list (* a statement, ends with a semi colon *)
  | StrCommaList of unparsed list (* comma separated list *)
  | StrSemiList of unparsed list (* semicolon separated list *)
  | StrParens of unparsed (* an item that should be in parens () *)
  | StrBraces of unparsed (* an item that should be in braces {} *)

let dump strs =
  let buf = Buffer.create 10_000 in
  let indent_level = ref 0 in
  let add str = Buffer.add_string buf str in
  let newline () =
    Buffer.add_string buf "\n";
    Buffer.add_string buf (String.make !indent_level ' ') in
  let rec process = function
    | StrEmpty -> add "StrEmpty"
    | StrBlank -> add "StrBlank"
    | StrComma -> add "StrComma"
    | StrSemi  -> add "StrSemi"
    | Str s -> add ("Str " ^ s)
    | StrComment s -> add ("StrComment " ^ s)
    | StrList ss -> items [Str "StrList"] ss
    | StrWords ss -> items [Str "StrWords"] ss
    | StrStatement ss -> items [Str "StrStatement "] ss
    | StrCommaList ss -> items [Str "StrCommaList"] ss
    | StrSemiList ss -> items [Str "StrSemiList"] ss
    | StrParens s -> add "("; add "StrParens "; process s; add ")"
    | StrBraces s -> add "("; add "StrBraces "; process s; add ")"
  and items cons ss =
    add "(";
    List.iter process cons;
    indent_level := !indent_level + 2;
    match ss with
    | [] -> add ")"
    | (s::ss) ->
        newline ();
        process s;
        List.iter (fun s -> add ","; newline (); process s) ss;
        add ")";
        indent_level := !indent_level - 2 in
  process strs;
  let res = Buffer.contents buf in
  Buffer.reset buf;
  res

let to_string strs =
  let buf = Buffer.create 10_000 in
  let add str = Buffer.add_string buf str in
  let rec process = function
    | StrEmpty -> ()
    | StrBlank -> add " "
    | StrComma -> add ","
    | StrSemi  -> add ";"
    | Str s -> add s
    | StrComment s -> add "// "; add s; add "\n"
    | StrList ss -> process_list ss
    | StrWords ss -> words ss
    | StrStatement ss ->
      words ss; process StrSemi
    | StrCommaList ss ->
        intersperse StrComma ss |> process_list;
    | StrSemiList ss ->
        intersperse StrSemi ss |> process_list;
    | StrParens s -> add "("; process s; add ")";
    | StrBraces s -> add "{"; process s; add "}";
  and nonempty = List.filter (fun s -> s <> StrEmpty)
  and process_list ss = nonempty ss |> List.iter process
  and intersperse sep ss = nonempty ss |> List.intersperse sep
  and words ss = intersperse StrBlank ss |> List.iter process in
  process strs;
  let res = Buffer.contents buf in
  Buffer.reset buf;
  res
