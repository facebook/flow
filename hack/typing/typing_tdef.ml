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
open Typing_defs

module Reason = Typing_reason
module Env    = Typing_env
module DefsDB = Typing_heap
module Inst   = Typing_instantiate
module TSubst = Typing_subst
module TUtils = Typing_utils
module TAccess = Typing_taccess
module Phase  = Typing_phase

(*****************************************************************************)
(* Expanding type definition *)
(*****************************************************************************)

let expand_typedef_ ?force_expand:(force_expand=false) ety_env env r x argl =
  let pos = Reason.to_pos r in
  if Typing_defs.has_expanded ety_env x
  then begin
      Errors.cyclic_typedef pos;
      env, (ety_env, (r, Tany))
    end
  else begin
      let tdef = Typing_env.get_typedef env x in
      let tdef = match tdef with None -> assert false | Some x -> x in
      match tdef with
      | DefsDB.Typedef.Error -> env, (ety_env, (r, Tany))
      | DefsDB.Typedef.Ok tdef ->
         let visibility, tparaml, tcstr, expanded_ty, tdef_pos = tdef in
         let should_expand =
           force_expand ||
             match visibility with
             | DefsDB.Typedef.Private ->
                Pos.filename tdef_pos = Env.get_file env
             | DefsDB.Typedef.Public -> true
         in
         if List.length tparaml <> List.length argl
         then begin
             let n = List.length tparaml in
             let n = string_of_int n in
             Errors.type_param_arity pos x n
           end;
         let ety_env = {
           ety_env with
           type_expansions = (tdef_pos, x) :: ety_env.type_expansions;
           substs = TSubst.make tparaml argl;
         } in
         let env, expanded_ty =
           if should_expand
           then begin
               let env, expanded_ty =
                 Phase.localize ~ety_env env expanded_ty in
               env, expanded_ty
             end
           else begin
               let env, tcstr =
                 match tcstr with
                 | None -> env, None
                 | Some tcstr ->
                    let env, tcstr =
                      Phase.localize ~ety_env env tcstr in
                    env, Some tcstr
               in
               env, (r, Tabstract (AKnewtype (x, argl), tcstr))
             end
         in
         if Naming_special_names.Classes.is_format_string x
         then env, (ety_env, (r, Tclass ((pos, x), argl)))
         else env, (ety_env, (r, snd expanded_ty))
    end

let expand_typedef ety_env env r x argl = expand_typedef_ ety_env env r x argl

(* Expand a typedef, smashing abstraction and collecting a trail
 * of where the typedefs come from. *)
let rec force_expand_typedef ~ety_env env (t : locl ty) =
  match t with
  | r, Tabstract (AKnewtype (x, argl), _) ->
     let env, (ety_env, ty) =
       expand_typedef_ ~force_expand:true ety_env env r x argl in
     force_expand_typedef ~ety_env env ty
  | ty -> env, ty, List.rev_map ety_env.type_expansions fst

(*****************************************************************************)
(*****************************************************************************)

let () = TUtils.expand_typedef_ref := expand_typedef
