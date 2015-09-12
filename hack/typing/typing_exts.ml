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

Ad-hoc rules for typing some common idioms

  For printf-style functions: If the last argument (before varargs) of a
  function has the type FormatString<X>, we assume that the format
  string is interpreted by the formatter X, which should look like this:

  interface PrintfFormatter {
    function format_0x25() : string;
    function format_x(int) : string;
    function format_f(float) : string;
    function format_upcase_l() : PrintfFormatter;
  }

  Each method can return a string or another formatter (for
  multi-character sequences like %Ld); the parameters are copied into
  the function signature when instantiating it at the call site.

*)

open Typing_defs
open Utils

module Env = Typing_env
module Reason = Typing_reason
module Print = Typing_print
module SN = Naming_special_names

let magic_method_name input =
  match input with
    | None -> "format_eof"
    | Some c ->
        let uc = Char.uppercase c
        and lc = Char.lowercase c in
          if lc == uc then Printf.sprintf "format_0x%02x" (Char.code lc)
          else if c == uc then "format_upcase_" ^ String.make 1 lc
          else "format_" ^ String.make 1 lc

let lookup_magic_type (env:Env.env) (class_:locl ty) (fname:string) :
    Env.env * (locl fun_params * locl ty option) option =
  match class_ with
    | (_, Tclass ((_, className), [])) ->
        (match Env.get_class env className with
           | Some c ->
               let env, ce_type =
                 Env.get_member true env c fname |>
                   opt
                     (fun env x ->
                      Typing_phase.localize_with_self env x.ce_type)
                     env
               in
               (match ce_type with
                  | Some (_, Tfun {
                                   ft_params = pars;
                            ft_ret = ty;
                            _;
                          }
                    ) ->
                     env, Some (pars,
                                (match ty with
                                 | (_, Tprim Nast.Tstring) -> None
                                 | _ -> Some ty))
                  | _ -> env, None)
           | None -> env, None)
    | _ -> env, None

let get_char s i =
  if i >= String.length s then None else Some (String.get s i)

let parse_printf_string env s pos (class_:locl ty) : Env.env * locl fun_params =
  let rec read_text env i : Env.env * locl fun_params =
    match get_char s i with
      | Some '%' -> read_modifier env (i+1) class_ i
      | Some _   -> read_text env (i+1)
      | None     -> env, []
  and read_modifier env i class_ i0 : Env.env * locl fun_params =
    let fname = magic_method_name (get_char s i) in
    let snippet = String.sub s i0 ((min (i+1) (String.length s)) - i0) in
    let add_reason = List.map
      (function name, (why, ty) ->
         name, (Reason.Rformat (pos,snippet,why), ty)) in
    match lookup_magic_type env class_ fname with
      | env, Some (good_args, None) ->
          let env, xs = read_text env (i+1) in
          env, add_reason good_args @ xs
      | env, Some (good_args, Some next) ->
          let env, xs = read_modifier env (i+1) next i0 in
          env, add_reason good_args @ xs
      | env, None ->
          Errors.format_string
            pos snippet s (Reason.to_pos (fst class_)) fname (Print.suggest class_);
          let env, xs = read_text env (i+1) in
          env, add_reason xs
  in
    read_text env 0

type ('a, 'b) either = Left of 'a | Right of 'b

let mapM (f:'s->'x->'s*'y) : 'st -> 'x list -> 's * 'y list =
  let rec f' st xs =
    match xs with
      | [] -> st, []
      | (x::xs) ->
          let st', x' = f st x in
          let st'', xs' = f' st' xs in
            st'', x' :: xs'
  in f'

(* If expr is a constant string, that string, otherwise a position
   where it is obviously not *)
let rec const_string_of (env:Env.env) (e:Nast.expr) : Env.env * (Pos.t, string) either =
  let glue x y = match x,y with
    | Right sx, Right sy -> Right (sx^sy)
    | Left p, _ -> Left p
    | _, Left p -> Left p in
    match e with
      | _, Nast.String (_, s) -> env, Right s
      | _, Nast.String2 (xs, s) ->
          let env, xs = mapM const_string_of env (List.rev xs) in
          env, List.fold_right glue xs (Right s)
    | _, Nast.Binop (Ast.Dot, a, b) ->
        let env, stra = const_string_of env a in
        let env, strb = const_string_of env b in
        env, glue stra strb
    | p, _ -> env, Left p

(* Specialize a function type using whatever we can tell about the args *)
let retype_magic_func (env:Env.env) (ft:locl fun_type) (el:Nast.expr list) : Env.env * locl fun_type =
  let rec f env param_types args : Env.env * locl fun_params option =
    (match param_types, args with
      | [(_,    (_,   Toption (_, Tclass ((_, fs), [_       ]))))], [(_, Nast.Null)]
        when SN.Classes.is_format_string fs -> env,None
      | [(name, (why, Toption (_, Tclass ((_, fs), [type_arg]))))], (arg :: _)
      | [(name, (why,             Tclass ((_, fs), [type_arg] )))], (arg :: _)
        when SN.Classes.is_format_string fs ->
          (match const_string_of env arg with
             |  env, Right str ->
                  let env, argl =
                    parse_printf_string env str (fst arg) type_arg in
                  env, Some ((name, (why, Tprim Nast.Tstring)) :: argl)
             |  env, Left pos ->
                  if Env.is_strict env
                  then Errors.expected_literal_string pos;
                  env, None)
      | (param::params), (_::args) ->
          (match f env params args with
             | env, None -> env, None
             | env, Some xs -> env, Some (param :: xs))
      | _ -> env, None)
  in match f env ft.ft_params el with
    | env, None -> env, ft
    | env, Some xs ->
      let num_params = List.length xs in
      env, { ft with
        ft_params  = xs;
        ft_arity   = Fstandard (num_params, num_params);
      }
