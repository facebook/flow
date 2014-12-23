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

let map_fst f (x,y) = (f x, y)
let map_snd f (x,y) = (x, f y)

let rec map_ty_ f t =
  match t with
    | Tarray (ot, ot') -> f (Tarray ((opt_map (map_ty f) ot),
                                     (opt_map (map_ty f) ot')))
    | Tgeneric (a,ot) -> f (Tgeneric (a, opt_map (map_ty f) ot))
    | Toption t -> f (Toption (map_ty f t))
    | Tapply (x, ts) -> f (Tapply (x, List.map (map_ty f) ts))
    | Ttuple ts -> f (Ttuple (List.map (map_ty f) ts))
    | x -> f x
and map_ty f = map_snd (map_ty_ f)

let fresh_tvars (env:Env.env) (ts:tparam list) (ps:fun_params) (uniq:int) : Env.env * fun_params * tparam list =
  let append_id s = Printf.sprintf "%s_%d" s uniq in
  let rename_str s =
    match List.find_all (fun (_, (pos,str),_) -> str = s) ts with
      | [_] -> append_id s
      | _ -> s in
  let rename_ty = function
    | Tapply ((pos,name), args) ->
        Tapply ((pos, rename_str name), args)
    | Tgeneric (name, ot) ->
        Tgeneric (rename_str name, ot)
    | s -> s in
  let ps' = List.map (map_snd (map_ty rename_ty)) ps in
  let ts' = List.map (fun (x, y, z) -> x, map_snd rename_str y, z) ts in
  env, ps', ts'

let lookup_magic_type (env:Env.env) (class_:ty) (fname:string) (uniq:int) :
    Env.env * (fun_params * tparam list * ty option) option =
  match class_ with
    | (why, Tapply ((_, className), [])) ->
        (match Env.get_class env className with
           | Some c ->
               (match Env.get_member true env c fname with
                  | Some {
                      ce_type = (_, Tfun {
                                   ft_tparams = tpars;
                                   ft_params = pars;
                                   ft_ret = ty;
                                   _ });
                      _
                    } ->
                      let env, pars, tpars = fresh_tvars env tpars pars uniq
                      in env, Some (pars,
                                    tpars,
                                    (match ty with
                                       | (_, Tprim Nast.Tstring) -> None
                                       | x -> Some ty))
                  | _ -> env, None)
           | None -> env, None)
    | _ -> env, None

let get_char s i =
  if i >= String.length s then None else Some (String.get s i)

let parse_printf_string (env:Env.env) (s:string) (pos:Pos.t) (class_:ty) : Env.env * fun_params * tparam list =
  let rec read_text env i : Env.env * fun_params * tparam list =
    match get_char s i with
      | Some '%' -> read_modifier env (i+1) class_ i
      | Some c   -> read_text env (i+1)
      | None     -> env, [], []
  and read_modifier env i class_ i0 : Env.env * fun_params * tparam list =
    let fname = magic_method_name (get_char s i) in
    let snippet = String.sub s i0 ((min (i+1) (String.length s)) - i0) in
    let add_reason = List.map
      (function name, (why, ty) ->
         name, (Reason.Rformat (pos,snippet,why), ty)) in
    match lookup_magic_type env class_ fname i0 with
      | env, Some (good_args, targs, None) ->
            (match read_text env (i+1) with
               | env, xs, ys -> env, add_reason good_args @ xs, targs @ ys)
      | env, Some (good_args, targs, Some next) ->
          (match read_modifier env (i+1) next i0 with
             | env, xs, ys -> env, add_reason good_args @ xs, targs @ ys)
      | env, None ->
          Errors.format_string
            pos snippet s (Reason.to_pos (fst class_)) fname (Print.suggest class_);
            (match read_text env (i+1) with
             | env, xs, ys -> env, add_reason xs, ys)
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
let retype_magic_func (env:Env.env) (ft:fun_type) (el:Nast.expr list) : Env.env * fun_type =
  let rec f env param_types args : Env.env * (fun_params * tparam list) option =
    (match param_types, args with
      | [(_,    (_,   Toption (_, Tapply ((_, fs), [_       ]))))], [(_, Nast.Null)]
        when fs = SN.Classes.cFormatString -> env,None
      | [(name, (why, Toption (_, Tapply ((_, fs), [type_arg]))))], (arg :: args)
      | [(name, (why,             Tapply ((_, fs), [type_arg] )))], (arg :: args)
        when fs = SN.Classes.cFormatString ->
          (match const_string_of env arg with
             |  env, Right str ->
                  let env, argl, targl = parse_printf_string env str (fst arg) type_arg in
                  env, Some ((name, (why, Tprim Nast.Tstring)) :: argl, targl)
             |  env, Left pos ->
                  if Env.is_strict env
                  then Errors.expected_literal_string pos;
                  env, None)
      | (param::params), (_::args) ->
          (match f env params args with
             | env, None -> env, None
             | env, Some (xs,ys) -> env, Some (param :: xs, ys))
      | _ -> env, None)
  in match f env ft.ft_params el with
    | env, None -> env, ft
    | env, Some (xs,ys) ->
      let num_params = List.length xs in
      env, { ft with
        ft_tparams = ft.ft_tparams @ ys;
        ft_params  = xs;
        ft_arity   = Fstandard (num_params, num_params);
      }
