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

module ShapeMap = Nast.ShapeMap

(*****************************************************************************)
(* Functor traversing a type, but applies a user defined function for
 * positions.
 *)
(*****************************************************************************)
module TraversePos(ImplementPos: sig val pos: Pos.t -> Pos.t end) = struct
  open Typing_reason

  let pos = ImplementPos.pos

  let rec reason = function
    | Rnone                  -> Rnone
    | Rwitness p             -> Rwitness (pos p)
    | Ridx (p, r)            -> Ridx (pos p, reason r)
    | Ridx_vector p          -> Ridx_vector (pos p)
    | Rappend p              -> Rappend (pos p)
    | Rfield p               -> Rfield (pos p)
    | Rforeach p             -> Rforeach (pos p)
    | Rasyncforeach p        -> Rasyncforeach (pos p)
    | Raccess p              -> Raccess (pos p)
    | Rarith p               -> Rarith (pos p)
    | Rarith_ret p           -> Rarith_ret (pos p)
    | Rarray_plus_ret p      -> Rarray_plus_ret (pos p)
    | Rstring2 p             -> Rstring2 (pos p)
    | Rcomp p                -> Rcomp (pos p)
    | Rconcat p              -> Rconcat (pos p)
    | Rconcat_ret p          -> Rconcat_ret (pos p)
    | Rlogic p               -> Rlogic (pos p)
    | Rlogic_ret p           -> Rlogic_ret (pos p)
    | Rbitwise p             -> Rbitwise (pos p)
    | Rbitwise_ret p         -> Rbitwise_ret (pos p)
    | Rstmt p                -> Rstmt (pos p)
    | Rno_return p           -> Rno_return (pos p)
    | Rno_return_async p     -> Rno_return_async (pos p)
    | Rret_fun_kind (p, k)   -> Rret_fun_kind (pos p, k)
    | Rhint p                -> Rhint (pos p)
    | Rnull_check p          -> Rnull_check (pos p)
    | Rnot_in_cstr p         -> Rnot_in_cstr (pos p)
    | Rthrow p               -> Rthrow (pos p)
    | Rplaceholder p         -> Rplaceholder (pos p)
    | Rattr p                -> Rattr (pos p)
    | Rxhp p                 -> Rxhp (pos p)
    | Rret_div p             -> Rret_div (pos p)
    | Ryield_gen p           -> Ryield_gen (pos p)
    | Ryield_asyncgen p      -> Ryield_asyncgen (pos p)
    | Ryield_asyncnull p     -> Ryield_asyncnull (pos p)
    | Ryield_send p          -> Ryield_send (pos p)
    | Rlost_info (s, r1, p2) -> Rlost_info (s, reason r1, pos p2)
    | Rcoerced (p1, p2, x)   -> Rcoerced (pos p1, pos p2, x)
    | Rformat (p1, s, r)     -> Rformat (pos p1, s, reason r)
    | Rclass_class (p, s)    -> Rclass_class (pos p, s)
    | Runknown_class p       -> Runknown_class (pos p)
    | Rdynamic_yield (p1, p2, s1, s2) -> Rdynamic_yield(pos p1, pos p2, s1, s2)
    | Rmap_append p          -> Rmap_append (pos p)
    | Rvar_param p           -> Rvar_param (pos p)
    | Runpack_param p        -> Runpack_param (pos p)
    | Rinstantiate (r1,x,r2) -> Rinstantiate (reason r1, x, reason r2)
    | Rarray_filter (p, r)   -> Rarray_filter (pos p, reason r)
    | Rtype_access (r1, x, r2) -> Rtype_access (reason r1, x, reason r2)
    | Rexpr_dep_type (r, p, n) -> Rexpr_dep_type (reason r, pos p, n)
    | Rnullsafe_op p           -> Rnullsafe_op (pos p)
    | Rtconst_no_cstr (p, s)   -> Rtconst_no_cstr (pos p, s)
    | Rused_as_map p           -> Rused_as_map (pos p)
    | Rused_as_shape p         -> Rused_as_shape (pos p)
  let string_id (p, x) = pos p, x

  let rec ty (p, x) =
    reason p, ty_ x

  and ty_: decl ty_ -> decl ty_ = function
    | Tany
    | Tthis
    | Tmixed as x          -> x
    | Tarray (ty1, ty2)    -> Tarray (ty_opt ty1, ty_opt ty2)
    | Tprim _ as x         -> x
    | Tgeneric (s, cstr_opt) -> Tgeneric (s, constraint_ cstr_opt)
    | Ttuple tyl           -> Ttuple (List.map tyl ty)
    | Toption x            -> Toption (ty x)
    | Tfun ft              -> Tfun (fun_type ft)
    | Tapply (sid, xl)     -> Tapply (string_id sid, List.map xl ty)
    | Taccess (root_ty, ids) ->
        Taccess (ty root_ty, List.map ids string_id)
    | Tshape (fields_known, fdm) ->
        Tshape (fields_known, ShapeMap.map ty fdm)

  and ty_opt x = Option.map x ty

  and constraint_ = function
    | None -> None
    | Some (ck, x) -> Some (ck, ty x)

  and fun_type ft =
    { ft with
      ft_tparams = List.map ft.ft_tparams type_param   ;
      ft_params  = List.map ft.ft_params fun_param     ;
      ft_ret     = ty ft.ft_ret                        ;
      ft_pos     = pos ft.ft_pos                       ;
    }

  and fun_param (x, y) = x, ty y

  and class_elt ce =
    { ce_final       = ce.ce_final      ;
      ce_is_xhp_attr = ce.ce_is_xhp_attr;
      ce_override    = ce.ce_override   ;
      ce_synthesized = ce.ce_synthesized;
      ce_visibility  = ce.ce_visibility ;
      ce_type        = ty ce.ce_type    ;
      ce_origin      = ce.ce_origin     ;
    }

  and typeconst tc =
    { ttc_name = string_id tc.ttc_name;
      ttc_constraint = ty_opt tc.ttc_constraint;
      ttc_type = ty_opt tc.ttc_type;
      ttc_origin = tc.ttc_origin;
    }

  and type_param (variance, sid, x) =
    variance, string_id sid, constraint_ x

  and class_type tc =
    { tc_final                 = tc.tc_final                          ;
      tc_need_init             = tc.tc_need_init                      ;
      tc_deferred_init_members = tc.tc_deferred_init_members          ;
      tc_abstract              = tc.tc_abstract                       ;
      tc_members_fully_known   = tc.tc_members_fully_known            ;
      tc_kind                  = tc.tc_kind                           ;
      tc_name                  = tc.tc_name                           ;
      tc_pos                   = tc.tc_pos                            ;
      tc_extends               = tc.tc_extends                        ;
      tc_req_ancestors         = tc.tc_req_ancestors                  ;
      tc_req_ancestors_extends = tc.tc_req_ancestors_extends          ;
      tc_tparams               = List.map tc.tc_tparams type_param    ;
      tc_consts                = SMap.map class_elt tc.tc_consts      ;
      tc_typeconsts            = SMap.map typeconst tc.tc_typeconsts  ;
      tc_props                 = SMap.map class_elt tc.tc_props       ;
      tc_sprops                = SMap.map class_elt tc.tc_sprops      ;
      tc_methods               = SMap.map class_elt tc.tc_methods     ;
      tc_smethods              = SMap.map class_elt tc.tc_smethods    ;
      tc_construct             = Option.map (fst tc.tc_construct) class_elt,
                                   (snd tc.tc_construct);
      tc_ancestors             = SMap.map ty tc.tc_ancestors          ;
      tc_enum_type             = Option.map tc.tc_enum_type enum_type ;
    }

  and enum_type te =
    { te_base       = ty te.te_base           ;
      te_constraint = ty_opt te.te_constraint ;
    }

  and typedef ({td_tparams; td_constraint; _} as tdef) =
    let td_tparams = List.map td_tparams type_param in
    let td_constraint = ty_opt td_constraint in
    {tdef with td_tparams; td_constraint}
end

(*****************************************************************************)
(* Applies a position substitution to a class type *)
(*****************************************************************************)
module SubstPos = struct
  let class_type subst c =
    let module IPos = struct
      let pos x =
        try Hashtbl.find subst x with Not_found -> x
    end in
    let module Apply = TraversePos(IPos) in
    Apply.class_type c
end

(*****************************************************************************)
(* Returns a signature with all the positions replaced with Pos.none *)
(*****************************************************************************)
module NormalizeSig = struct
  include TraversePos(struct let pos _ = Pos.none end)
end
