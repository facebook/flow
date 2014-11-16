(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* julien: this is a copy/paste of the original pp.ml.
 * It is slightly modified, and I don't know how much these modifications
 * affect xhpizer. I hope to be able to merge these two files back together.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* we use a backtracking model *)
exception Fail

type env = {
  (* the actual printing hook, sometimes temporarily set to do_nothing()
   * when trying something before actually printing it. *)
  print: (string -> unit);

  (* stack of margin, push'ed and pop'ed when processing {} *)
  mutable margin: int list;

  (* current column *)
  mutable cmargin: int;
  (* current line *)
  mutable line: int;

  (* depth in the tree of try_ *)
  mutable level: int;

  (* for the parenthesis automatic insertion *)
  mutable priority: int;

  (* pad: ?? *)
  mutable last_nl: bool;
  mutable emptyl: bool;
  mutable failed: bool;
  mutable pushed: bool;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let empty o = {
  print = o;
  margin = [0];
  priority = 0;
  cmargin = 0;
  level = 0;
  pushed = false;
  last_nl = false;
  emptyl = false;
  failed = false;
  line = 0;
}

let debug env f =
  let buf = Buffer.create 256 in
  let env' = { env with print = (fun s -> Buffer.add_string buf s)} in
  (try f env' with _ -> ());
  Printf.printf "Debug %s\n" (Buffer.contents buf)

let do_nothing _ = ()

(*****************************************************************************)
(* Newlines and spaces *)
(*****************************************************************************)

let print env x =
  (* todo: this is the case right now for comments.
   * we should normalize those comments?
   * if (String.contains x '\n')
   * then failwith (Printf.sprintf "%s contains a newline\n" x);
   *)

  env.last_nl <- false;
  env.emptyl <- false;

  env.cmargin <- env.cmargin + String.length x;
  if env.cmargin >= 80
  then
    (* there is nothing to backtrack on, so just print it *)
    if env.level = 0
    then begin env.print x; env.failed <- true end
    else raise Fail
  else env.print x

let spaces env =
  for i = 1 to List.hd env.margin do
    print env " ";
  done

let newline env =
  env.pushed <- false;
  if env.last_nl
  then env.emptyl <- true;
  env.last_nl <- true;
  env.cmargin <- 0;
  env.line <- env.line + 1;
  env.print "\n"

let newline_opt env =
  if env.last_nl
  then ()
  else newline env

let space_or_nl env =
  if env.cmargin < 75
  then print env " "
  else (newline env; spaces env)

(*****************************************************************************)
(* Margins *)
(*****************************************************************************)

let push env =
  env.pushed <- true;
  env.margin <- List.hd env.margin + 2 :: env.margin

let pop env =
  env.margin <- List.tl env.margin

let nest env f =
  push env;
  f env;
  pop env

let nest_opt env f =
  if env.pushed
  then f env
  else begin
    push env;
    f env;
    pop env
  end

let nestc env f =
  env.margin <- env.cmargin :: env.margin;
  f env;
  pop env

let nest_block env f =
  print env "{";
  newline env;
  nest env f;
  spaces env;
  print env "}"

let nest_block_nl env f =
  nest_block env f;
  newline env

(*****************************************************************************)
(* Lists *)
(*****************************************************************************)

let rec simpl_list env f sep = function
  | [] -> ()
  | [x] -> f env x
  | x :: rl -> f env x; print env sep; simpl_list env f sep rl

let rec list_sep env f sep = function
  | [] -> ()
  | [x] -> f env x
  | x :: rl -> f env x; sep env; list_sep env f sep rl

let flat_list env f opar l sep cpar =
  print env opar;
  list_sep env f (fun env -> print env sep; print env " ") l;
  print env cpar

(* pad: used to take a last_nl parameter, but it was not used *)
let nl_nested_list env f opar l sep cpar =
  print env opar;
  nest env (fun env ->
    newline env;
    spaces env;
    list_sep env f (fun env -> print env sep; newline env; spaces env) l;
    newline env;
  );
  if cpar <> ""
  then begin
    spaces env;
    print env cpar
  end

(*****************************************************************************)
(* Backtracking combinators *)
(*****************************************************************************)

let fail () = raise Fail

let try_ env f =
  f { env with print = do_nothing; level = env.level + 1};
  f env


let choice_left env f1 f2 =
  try try_ env f1
  with
  | Fail when env.level = 0 ->
      (try f2 env with Fail -> assert false)
  (* otherwise, just let the exception bubble up more *)


let choice_right env f1 f2 =
  try try_ env f1
  with Fail ->
    f2 env

let try_hard env f =
  try
    f { env with print = do_nothing; level = 1 };
    f env
  with Fail ->
    let env' = { env with failed = false; print = do_nothing; level = 0 } in
    f env';
    if env'.failed
    then raise Fail
    else f { env with level = 0 }

let cut_list env f l =
  List.iter (
    fun x ->
      choice_right env
        (fun env -> f env x)
        (fun env -> newline env; spaces env; f env x)
  ) l


let list env f opar l sep cpar =
  let simple = (fun env -> flat_list env f opar l sep cpar) in
  let nested = (fun env -> nl_nested_list env f opar l sep cpar) in
  choice_right env simple nested

let list_left env f opar l sep cpar =
  let simple = (fun env -> if l <> [] then print env " "; flat_list env f opar l sep cpar) in
  let nested = (fun env -> nl_nested_list env f opar l sep cpar) in
  choice_left env simple nested

let nested_arg env f opar l sep cpar =
  let rec elt = function
    | [] -> assert false
    | [x] ->
        f env x;
        newline env;
    | x :: rl ->
        f env x; print env sep; newline env; spaces env;
        elt rl
  in
  nestc env (
    fun env ->
      print env opar;
      nestc env (
        fun env ->
          elt l;
      );
      spaces env;
      print env cpar;
  )


let fun_args env f opar l sep cpar =
  let simple = (
    fun env ->
      let line = env.line in
      flat_list env f opar l sep cpar;
      if line <> env.line then fail();
  ) in
  let nl_nested = (fun env -> nl_nested_list env f opar l sep cpar) in
  choice_right env simple nl_nested


let nested_list env f opar l sep cpar last_nl =
  env.margin <- env.cmargin :: env.margin;
  print env opar;
  env.margin <- env.cmargin :: env.margin;
  list_sep env f (fun env -> print env sep; newline env; spaces env) l;
  if last_nl
  then begin
    print env sep;
    newline env;
    pop env;
    spaces env;
    print env cpar;
  end
  else begin
    print env cpar;
    pop env;
  end;
  pop env

let fun_params env f l =
  let opar = "(" in
  let sep  = "," in
  let cpar = ")" in
  let simple = (
    fun env ->
      let line = env.line in
      flat_list env f opar l sep cpar;
      if line <> env.line then fail();
  ) in
  let nl_nested = (fun env -> nested_list env f opar l sep cpar true) in
  choice_right env simple nl_nested

(*****************************************************************************)
(* Parenthesis handling *)
(*****************************************************************************)

let paren prio env f =
  let old_prio = env.priority in
  env.priority <- prio;
  if (prio >= old_prio) || (prio = -1)
  then f env
  else begin
    print env "(";
    f env;
    print env ")";
  end;
  env.priority <- old_prio

(*****************************************************************************)
(* String helpers *)
(*****************************************************************************)
(* module PpString = struct *)

let char_is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false

let is_space s i = char_is_space s.[i]

let rec is_only_space s i =
  if i >= String.length s
  then true
  else is_space s i && is_only_space s (i+1)

let strip s =
  let c1 = ref 0 in
  let c2 = ref (String.length s - 1) in
  while is_space s !c1 do
    incr c1;
  done;
  while is_space s !c2 do
    decr c2;
  done;
  let c2 = String.length s - 1 - !c2 in
  String.sub s !c1 (String.length s - !c1 - c2)

let space = function
  | ' ' | '\t' -> true
  | _ -> false

let rec find_cut x start i =
  if i < 20
  then start
  else if space x.[i]
  then i
  else find_cut x start (i-1)

let rec string quote sep env x =
  choice_left env (
    fun env ->
      print env x
  ) (
    fun env ->
      let size = 80 - env.cmargin - String.length sep - 1 in
      let size = find_cut x size size in
      let s = String.sub x 0 size in
      let rest = String.sub x size (String.length x - size) in
      print env s;
      print env quote;
      print env sep;
      newline env;
      spaces env;
      print env quote;
      string quote sep env rest
  )

let string quote sep env x =
  if env.cmargin >= 20
  then begin
    print env quote;
    print env x;
    print env quote
  end
  else
    nestc env (
      fun env ->
        print env quote;
        string quote sep env x;
        print env quote;
    )

let first_char_escape env s =
  if s = "" then 0 else
    match s.[0] with
    | 'A' .. 'Z' | 'a' .. 'z' | '&' | ' ' | '\n' | '<' | '>' -> 0
    | c ->
        print env "{'";
        let size = ref 1 in
        while !size < String.length s && not (char_is_space s.[!size]) do incr size done;
        let size = !size in
        print env (String.sub s 0 size);
        print env "'}";
        if size < String.length s then print env " ";
        size

let print_text env s =
  let size = ref (String.length s - 1) in
  while !size >= 0 && char_is_space s.[!size] do
    decr size;
  done;
  let size = !size in
  let buf = Buffer.create 80 in
  let last_is_space = ref true in
  nestc env (
    fun env ->
      let i = first_char_escape env s in
      for i = i to size do
        if String.contains "\n\t " s.[i]
        then
          if !last_is_space
          then ()
          else begin
            last_is_space := true;
            print env (Buffer.contents buf);
            Buffer.clear buf;
            space_or_nl env;
          end
        else (last_is_space := false; Buffer.add_char buf s.[i])
      done;
      print env (Buffer.contents buf);
      Buffer.clear buf;
  )
