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
open Typing_defs

(* The following classes are used to make sure we make no typing
 * mistake when interacting with the database. The database knows
 * how to associate a string to a string. We need to deserialize
 * the string and make sure the type is correct. By using these
 * modules, the places where there could be a typing mistake are
 * very well isolated.
*)

(* Module used to represent serialized classes *)
module Class = struct
  type t = Typing_defs.class_type
  let prefix = Prefix.make()
end

(* a function type *)
module Fun = struct
  type t = decl Typing_defs.fun_type
  let prefix = Prefix.make()
end

module Typedef = struct

  type visibility =
    | Public
    | Private

  type tdef =
    visibility * Typing_defs.tparam list * decl ty option * decl ty * Pos.t

  type tdef_or_error =
    | Error
    | Ok of tdef

  type t = tdef_or_error
  let prefix = Prefix.make()
end

module GConst = struct
  type t = decl ty
  let prefix = Prefix.make()
end

module Funs = SharedMem.WithCache (String) (Fun)
module Classes = SharedMem.WithCache (String) (Class)
module Typedefs = SharedMem.WithCache (String) (Typedef)
module GConsts = SharedMem.WithCache (String) (GConst)

module FuncTerminality = struct

  (* Not adding a Typing_dep here because it will be added when the
   * Nast is fully processed (by the caller of this code) *)
  let get_fun = Funs.get

  let get_static_meth (cls_name:string) (meth_name:string) =
    match Classes.get cls_name with
      | None -> None
      | Some { Typing_defs.tc_smethods ; _ } ->
        begin match Utils.SMap.get meth_name tc_smethods with
          | None -> None
          | Some { Typing_defs.ce_type = (_r, Typing_defs.Tfun fty) ; _} ->
            Some fty
          | Some _ -> None
        end

  let raise_exit_if_terminal = function
    | None -> ()
    | Some ({ Typing_defs.ft_ret = (_r, Typing_defs.Tprim Nast.Tnoreturn); _})
      -> raise Exit
    | Some _ -> ()

end
