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
(* Module used when we want to figure out what has changed.
 * 1) The user modifies a file
 * 2) We compute the type of the file (cf typing_redecl_service.ml)
 * 3) We compare the old type and the new type of the class (or function)
 *    to see if anything has changed. This is where the code of this module
 *    comes into play. This code compares the old and the new class type
 *    and computes the set of dependencies if something needs to be
 *    either redeclared or checked again.
 *)
(*****************************************************************************)
open Utils
open Typing_defs
open Typing_deps

module Env = Typing_env
module ShapeMap = Nast.ShapeMap

(*****************************************************************************)
(* Module comparing types "modulo" positions.
 * It returns a substitution (from positions to position) and a true when
 * both types are equivalent.
 * Let's imagine we have ty1 ty2:
 * 1) They are equivalent, and all the positions are the same
 *    CompareTypes.ty ty1 ty2 = [], true
 * 2) They are equivalent, but a position differs:
 *    CompareTypes.ty ty1 ty2 = subst, true
 *    where (apply_subst subst ty1) = ty2
 * 3) They are different:
 *    CompareTypes.ty ty1 ty2 = _, false
 *)
(*****************************************************************************)
module CompareTypes = struct

  type result = (Pos.t * Pos.t) list * bool

  let default : result = [], false

  let string_id (subst, same) (p1, s1) (p2, s2) =
    if s1 <> s2
    then default
    else (p1, p2) :: subst, same

  let pos (subst, same as acc) p1 p2 =
    if p1 = p2
    then acc
    else (p1, p2) :: subst, same

  let reason acc r1 r2 =
    let p1 = Typing_reason.to_pos r1 in
    let p2 = Typing_reason.to_pos r2 in
    pos acc p1 p2

  let smap f acc smap1 smap2 =
    let acc =
      SMap.fold begin fun x ce1 acc ->
        try
          let ce2 = SMap.find_unsafe x smap2 in
          f acc ce1 ce2
        with Not_found -> default
      end smap1 acc
    in
    (* Checking if all the members in smap2 are defined in smap1 *)
    try
      SMap.iter (fun x _ -> ignore (SMap.find_unsafe x smap1)) smap2;
      acc
    with Not_found -> default

  let cmp_opt f acc t1 t2 =
    match t1, t2 with
    | None, None -> acc
    | Some t1, Some t2 -> f acc t1 t2
    | _ -> default

  let rec ty acc (r1, x) (r2, y) =
    let acc = reason acc r1 r2 in
    let acc = ty_ acc x y in
    acc

  and ty_ (subst, same as acc) (ty1: decl ty_) (ty2: decl ty_) =
    match ty1, ty2 with
    | Tany, Tany
    | Tthis, Tthis
    | Tmixed, Tmixed -> acc
    | Tarray (ty1, ty2), Tarray (ty3, ty4) ->
        let acc = ty_opt (subst, same) ty1 ty3 in
        let acc = ty_opt acc ty2 ty4 in
        acc
    | Tgeneric (s1, cstr1), Tgeneric (s2, cstr2) ->
        let same = same && s1 = s2 in
        constraint_ (subst, same) cstr1 cstr2
    | Toption ty1, Toption ty2 ->
        ty acc ty1 ty2
    | Tprim x, Tprim y ->
        subst, same && x = y
    | Tfun f1, Tfun f2 ->
        fun_type acc f1 f2
    | Tapply (sid1, tyl1), Tapply (sid2, tyl2) ->
        let acc = string_id acc sid1 sid2 in
        let acc = tyl acc tyl1 tyl2 in
        acc
    | Taccess (root_ty1, ids1), Taccess (root_ty2, ids2)
      when List.length ids1 = List.length ids2 ->
        let acc = ty acc root_ty1 root_ty2 in
        List.fold_left2 string_id acc ids1 ids2
    | Ttuple tyl1, Ttuple tyl2 ->
        tyl acc tyl1 tyl2
    | Tshape (fields_known1, fdm1), Tshape (fields_known2, fdm2) ->
        let subst, same = ShapeMap.fold begin fun name v1 acc ->
          match ShapeMap.get name fdm2 with
          | None -> default
          | Some v2 ->
              ty acc v1 v2
        end fdm1 acc in
        begin match fields_known1, fields_known2 with
          | FieldsPartiallyKnown unset_fields1,
            FieldsPartiallyKnown unset_fields2 ->
              ShapeMap.fold begin fun name unset_pos1 acc ->
                match ShapeMap.get name unset_fields2 with
                  | None -> default
                  | Some unset_pos2 -> pos acc unset_pos1 unset_pos2
               end unset_fields1 (subst, same)
          | _ -> subst, same && (fields_known1 = fields_known2)
        end
    | (Tany | Tmixed | Tarray (_, _) | Tfun _ | Taccess (_, _) | Tgeneric (_, _)
       | Toption _ | Tprim _ | Tshape _| Tapply (_, _) | Ttuple _ | Tthis
      ), _ -> default

  and tyl acc tyl1 tyl2 =
    if List.length tyl1 <> List.length tyl2
    then default
    else List.fold_left2 ty acc tyl1 tyl2

  and ty_opt acc ty1 ty2 = cmp_opt ty acc ty1 ty2

  and constraint_ (subst, same) cstr_opt1 cstr_opt2 =
    match cstr_opt1, cstr_opt2 with
      | Some (ck1, ty1), Some (ck2, ty2) when ck1 = ck2 ->
          ty (subst, same) ty1 ty2
      | _ -> subst, false

  and fun_type acc ft1 ft2 =
    let acc = pos acc ft1.ft_pos ft2.ft_pos in
    let acc = tparam_list acc ft1.ft_tparams ft2.ft_tparams in
    let acc = fun_arity acc ft1.ft_arity ft2.ft_arity in
    let acc = fun_params acc ft1.ft_params ft2.ft_params in
    let subst, same = ty acc ft1.ft_ret ft2.ft_ret in
    subst, same && ft1.ft_abstract = ft2.ft_abstract

  and fun_arity acc arity1 arity2 =
    let subst, same = acc in
    match arity1, arity2 with
    | Fvariadic (min1, (_, ty1)), Fvariadic (min2, (_, ty2)) ->
      let subst, same = ty acc ty1 ty2 in
      subst, same && min1 = min2
    | Fellipsis min1, Fellipsis min2 -> subst, same && min1 = min2
    | Fstandard (min1, max1), Fstandard (min2, max2) ->
      subst, same && min1 = min2 && max1 = max2
    | _, _ -> subst, false

  and fun_params acc params1 params2 =
    if List.length params1 <> List.length params2
    then default
    else List.fold_left2 fun_param acc params1 params2

  and fun_param acc (name1, ty1) (name2, ty2) =
    if name1 <> name2
    then default
    else ty acc ty1 ty2

  and tparam_list acc tpl1 tpl2 =
    if List.length tpl1 <> List.length tpl2
    then default
    else List.fold_left2 tparam acc tpl1 tpl2

  and variance acc x1 x2 =
    match x1, x2 with
    | Ast.Covariant, Ast.Covariant -> acc
    | Ast.Contravariant, Ast.Contravariant -> acc
    | Ast.Invariant, Ast.Invariant -> acc
    | _ -> default

  and tparam acc (variance1, sid1, x1) (variance2, sid2, x2) =
    let acc = variance acc variance1 variance2 in
    let acc = string_id acc sid1 sid2 in
    let acc = constraint_ acc x1 x2 in
    acc

  and class_elt (subst, same) celt1 celt2 =
    let same = same && celt1.ce_visibility = celt2.ce_visibility in
    let same = same && celt1.ce_final = celt2.ce_final in
    let same = same && celt1.ce_is_xhp_attr = celt2.ce_is_xhp_attr in
    let same = same && celt1.ce_override = celt2.ce_override in
    let same = same && celt1.ce_synthesized = celt2.ce_synthesized in
    ty (subst, same) celt1.ce_type celt2.ce_type

  and members acc m1 m2 = smap class_elt acc m1 m2

  and typeconst (subst, same) tc1 {
    ttc_name = tc2_ttc_name;
    ttc_constraint = tc2_ttc_constraint;
    ttc_type = tc2_ttc_type;
    ttc_origin = tc2_ttc_origin;
  } =
    let acc = subst, same && tc1.ttc_origin = tc2_ttc_origin in
    let acc = string_id acc tc1.ttc_name tc2_ttc_name in
    let acc = ty_opt acc tc1.ttc_constraint tc2_ttc_constraint in
    ty_opt acc tc1.ttc_type tc2_ttc_type

  and typeconsts acc tc1 tc2 = smap typeconst acc tc1 tc2

  and constructor acc c1 c2 =
    let subst, same = match (fst c1), (fst c2) with
      | Some x1, Some x2 -> class_elt acc x1 x2
      | _ -> acc
    in subst, same && (snd c1 = snd c2)

  and ancestry acc imp1 imp2 = smap ty acc imp1 imp2

  and enum_type acc et1 et2 =
    let acc = ty acc et1.te_base et2.te_base in
    let acc = ty_opt acc et1.te_constraint et2.te_constraint in
    acc

  and class_ (subst, same) c1 c2 =
    let same =
      same &&
      c1.tc_final = c2.tc_final &&
      c1.tc_need_init = c2.tc_need_init &&
      c1.tc_members_fully_known = c2.tc_members_fully_known &&
      c1.tc_abstract = c2.tc_abstract &&
      c1.tc_kind = c2.tc_kind &&
      c1.tc_name = c2.tc_name &&
      SSet.compare c1.tc_deferred_init_members c2.tc_deferred_init_members = 0 &&
      SSet.compare c1.tc_extends c2.tc_extends = 0 &&
      SSet.compare c1.tc_req_ancestors_extends c2.tc_req_ancestors_extends = 0
    in
    let acc = subst, same in
    let acc = tparam_list acc c1.tc_tparams c2.tc_tparams in
    let acc = members acc c1.tc_consts c2.tc_consts in
    let acc = members acc c1.tc_props c2.tc_props in
    let acc = members acc c1.tc_sprops c2.tc_sprops in
    let acc = members acc c1.tc_methods c2.tc_methods in
    let acc = members acc c1.tc_smethods c2.tc_smethods in
    let acc = typeconsts acc c1.tc_typeconsts c2.tc_typeconsts in
    let acc = constructor acc c1.tc_construct c2.tc_construct in
    let acc = ancestry acc c1.tc_req_ancestors c2.tc_req_ancestors in
    let acc = ancestry acc c1.tc_ancestors c2.tc_ancestors in
    let acc = cmp_opt enum_type acc c1.tc_enum_type c2.tc_enum_type in
    acc

end

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
    | Ridx p                 -> Ridx (pos p)
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
    | Ttuple tyl           -> Ttuple (List.map (ty) tyl)
    | Toption x            -> Toption (ty x)
    | Tfun ft              -> Tfun (fun_type ft)
    | Tapply (sid, xl)     -> Tapply (string_id sid, List.map (ty) xl)
    | Taccess (root_ty, ids) ->
        Taccess (ty root_ty, List.map string_id ids)
    | Tshape (fields_known, fdm) ->
        Tshape (fields_known, ShapeMap.map ty fdm)

  and ty_opt x = opt_map ty x

  and constraint_ = function
    | None -> None
    | Some (ck, x) -> Some (ck, ty x)

  and fun_type ft =
    { ft with
      ft_tparams = List.map type_param ft.ft_tparams   ;
      ft_params  = List.map fun_param ft.ft_params     ;
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
      tc_tparams               = List.map type_param tc.tc_tparams    ;
      tc_consts                = SMap.map class_elt tc.tc_consts      ;
      tc_typeconsts            = SMap.map typeconst tc.tc_typeconsts  ;
      tc_props                 = SMap.map class_elt tc.tc_props       ;
      tc_sprops                = SMap.map class_elt tc.tc_sprops      ;
      tc_methods               = SMap.map class_elt tc.tc_methods     ;
      tc_smethods              = SMap.map class_elt tc.tc_smethods    ;
      tc_construct             = opt_map class_elt (fst tc.tc_construct), (snd tc.tc_construct);
      tc_ancestors             = SMap.map ty tc.tc_ancestors          ;
      tc_user_attributes       = tc.tc_user_attributes                ;
      tc_enum_type             = opt_map enum_type tc.tc_enum_type    ;
    }

  and enum_type te =
    { te_base       = ty te.te_base           ;
      te_constraint = ty_opt te.te_constraint ;
    }

  and typedef = function
    | Typing_heap.Typedef.Error as x -> x
    | Typing_heap.Typedef.Ok (is_abstract, tparams, tcstr, h, pos) ->
        let tparams = List.map type_param tparams in
        let tcstr = ty_opt tcstr in
        let tdef = (is_abstract, tparams, tcstr, ty h, pos) in
        Typing_heap.Typedef.Ok tdef
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

(*****************************************************************************)
(* Given two classes give back the set of functions or classes that need
 * to be rechecked
 *)
(*****************************************************************************)
module ClassDiff = struct

  let smap_left s1 s2 =
    SMap.fold begin fun x ty1 diff ->
      let ty2 = SMap.get x s2 in
      match ty2 with
      | Some ty2 ->
          if ty1 = ty2 then diff else
          SSet.add x diff
      | None ->
          SSet.add x diff
    end s1 SSet.empty

  let smap s1 s2 =
    SSet.union (smap_left s1 s2) (smap_left s2 s1)

  let add_inverted_dep build_obj x acc =
    DepSet.union (Typing_deps.get_ideps (build_obj x)) acc

  let add_inverted_deps acc build_obj xset =
    SSet.fold (add_inverted_dep build_obj) xset acc

  let compare cid class1 class2 =
    let acc = DepSet.empty in
    let is_unchanged = true in

    (* compare class constants *)
    let consts_diff = smap class1.tc_consts class2.tc_consts in
    let is_unchanged = is_unchanged && SSet.is_empty consts_diff in
    let acc = add_inverted_deps acc (fun x -> Dep.Const (cid, x)) consts_diff in

    (* compare class members *)
    let props_diff = smap class1.tc_props class2.tc_props in
    let is_unchanged = is_unchanged && SSet.is_empty props_diff in
    let acc = add_inverted_deps acc (fun x -> Dep.Prop (cid, x)) props_diff in

    (* compare class static members *)
    let sprops_diff = smap class1.tc_sprops class2.tc_sprops in
    let is_unchanged = is_unchanged && SSet.is_empty sprops_diff in
    let acc = add_inverted_deps acc (fun x -> Dep.SProp (cid, x)) sprops_diff in

    (* compare class methods *)
    let methods_diff = smap class1.tc_methods class2.tc_methods in
    let is_unchanged = is_unchanged && SSet.is_empty methods_diff in
    let acc = add_inverted_deps acc (fun x -> Dep.Method (cid, x)) methods_diff in

    (* compare class static methods *)
    let smethods_diff = smap class1.tc_smethods class2.tc_smethods in
    let is_unchanged = is_unchanged && SSet.is_empty smethods_diff in
    let acc = add_inverted_deps acc (fun x -> Dep.SMethod (cid, x)) smethods_diff in

    (* compare class constructors *)
    let cstr_diff = class1.tc_construct <> class2.tc_construct in
    let is_unchanged = is_unchanged && not cstr_diff in
    let cstr_ideps = Typing_deps.get_ideps (Dep.Cstr cid) in
    let acc = if cstr_diff then DepSet.union acc cstr_ideps else acc in

    (* compare class type constants *)
    let typeconsts_diff = smap class1.tc_typeconsts class2.tc_typeconsts in
    let is_unchanged = is_unchanged && SSet.is_empty typeconsts_diff in
    let acc =
      add_inverted_deps acc (fun x -> Dep.Class (cid^"::"^x)) typeconsts_diff in

    acc, is_unchanged

end

(*****************************************************************************)
(* Determines if there is a "big" difference between two classes
 * What it really means: most of the time, a change in a class doesn't affect
 * the users of the class, recomputing the sub-classes is enough.
 * However, there are some cases, where we really need to re-check all the
 * use cases of a class. For example: if a class doesn't implement an
 * interface anymore, all the subtyping is changed, so we have to recheck
 * all the places where the class was used.
 *)
(*****************************************************************************)
let class_big_diff class1 class2 =
  let class1 = NormalizeSig.class_type class1 in
  let class2 = NormalizeSig.class_type class2 in
  class1.tc_need_init <> class2.tc_need_init ||
  SSet.compare class1.tc_deferred_init_members class2.tc_deferred_init_members <> 0 ||
  class1.tc_members_fully_known <> class2.tc_members_fully_known ||
  class1.tc_kind <> class2.tc_kind ||
  class1.tc_tparams <> class2.tc_tparams ||
  SMap.compare class1.tc_ancestors class2.tc_ancestors <> 0 ||
  SMap.compare class1.tc_req_ancestors class2.tc_req_ancestors <> 0 ||
  SSet.compare class1.tc_req_ancestors_extends class2.tc_req_ancestors_extends <> 0 ||
  SSet.compare class1.tc_extends class2.tc_extends <> 0 ||
  class1.tc_enum_type <> class2.tc_enum_type ||
  (* due to, e.g. switch exhaustiveness checks, a change in an enum's
   * constant set is a "big" difference *)
    (class1.tc_enum_type <> None &&
       not (SSet.is_empty (ClassDiff.smap class1.tc_consts class2.tc_consts)))

(*****************************************************************************)
(* Given a class name adds all the subclasses, we need a "trace" to follow
 * what we have already added.
 *)
(*****************************************************************************)
let rec get_extend_deps_ trace cid_hash to_redecl =
  if DepSet.mem cid_hash !trace
  then to_redecl
  else begin
    trace := DepSet.add cid_hash !trace;
    let cid_hash = Typing_deps.Dep.extends_of_class cid_hash in
    let ideps = Typing_deps.get_ideps_from_hash cid_hash in
    DepSet.fold begin fun obj acc ->
      if Typing_deps.Dep.is_class obj
      then
        let to_redecl = DepSet.add obj acc in
        get_extend_deps_ trace obj to_redecl
    else to_redecl
  end ideps to_redecl
  end

(*****************************************************************************)
(* GET EVERYTHING, don't think, don't try to be subtle, don't try to be
 * smart what so ever, just get EVERYTHING that ever used the class "cid"
 * (cid = class identifier).
 * Hence the name "get_bazooka".
 *)
(*****************************************************************************)
and get_all_dependencies trace cid (to_redecl, to_recheck) =
  let bazooka = Typing_deps.get_bazooka (Dep.Class cid) in
  let to_redecl = DepSet.union bazooka to_redecl in
  let to_recheck = DepSet.union bazooka to_recheck in
  let cid_hash = Typing_deps.Dep.make (Dep.Class cid) in
  let to_redecl = get_extend_deps_ trace cid_hash to_redecl in
  to_redecl, to_recheck

let get_extend_deps cid_hash to_redecl =
  get_extend_deps_ (ref DepSet.empty) cid_hash to_redecl

(*****************************************************************************)
(* Determine which functions/classes have to be rechecked after comparing
 * the old and the new type signature of "fid" (function identifier).
*)
(*****************************************************************************)
let get_fun_deps old_funs fid (to_redecl, to_recheck) =
  match SMap.find_unsafe fid old_funs, Env.Funs.get fid with
  | None, None ->
      to_redecl, to_recheck
  | None, _ | _, None ->
      let where_fun_is_used = Typing_deps.get_bazooka (Dep.Fun fid) in
      let to_recheck = DepSet.union where_fun_is_used to_recheck in
      let fun_name = Typing_deps.get_bazooka (Dep.FunName fid) in
      DepSet.union fun_name to_redecl, DepSet.union fun_name to_recheck
  | Some fty1, Some fty2 ->
      let fty1 = NormalizeSig.fun_type fty1 in
      let fty2 = NormalizeSig.fun_type fty2 in
      let is_same_signature = fty1 = fty2 in
      if is_same_signature
      then to_redecl, to_recheck
      else
        (* No need to add Dep.FunName stuff here -- we found a function with the
         * right name already otherwise we'd be in the None case above. *)
        let where_fun_is_used = Typing_deps.get_bazooka (Dep.Fun fid) in
        to_redecl, DepSet.union where_fun_is_used to_recheck

let get_funs_deps old_funs funs =
  SSet.fold (get_fun_deps old_funs) funs (DepSet.empty, DepSet.empty)

(*****************************************************************************)
(* Determine which functions/classes have to be rechecked after comparing
 * the old and the new typedef
*)
(*****************************************************************************)
let get_type_deps old_types tid to_recheck =
  match SMap.find_unsafe tid old_types, Env.Typedefs.get tid with
  | None, None ->
      to_recheck
  | None, _ | _, None ->
      let bazooka = Typing_deps.get_bazooka (Dep.Class tid) in
      DepSet.union bazooka to_recheck
  | Some tdef1, Some tdef2 ->
      let tdef1 = NormalizeSig.typedef tdef1 in
      let tdef2 = NormalizeSig.typedef tdef2 in
      let is_same_signature = tdef1 = tdef2 in
      if is_same_signature
      then to_recheck
      else
        let where_type_is_used = Typing_deps.get_ideps (Dep.Class tid) in
        let to_recheck = DepSet.union where_type_is_used to_recheck in
        to_recheck

let get_types_deps old_types types =
  SSet.fold (get_type_deps old_types) types DepSet.empty

(*****************************************************************************)
(* Determine which top level definitions have to be rechecked if the constant
 * changed.
 *)
(*****************************************************************************)
let get_gconst_deps old_gconsts cst_id (to_redecl, to_recheck) =
  match SMap.find_unsafe cst_id old_gconsts, Env.GConsts.get cst_id with
  | None, None ->
      to_redecl, to_recheck
  | None, _ | _, None ->
      let where_const_is_used = Typing_deps.get_bazooka (Dep.GConst cst_id) in
      let to_recheck = DepSet.union where_const_is_used to_recheck in
      let const_name = Typing_deps.get_bazooka (Dep.GConstName cst_id) in
      DepSet.union const_name to_redecl, DepSet.union const_name to_recheck
  | Some cst1, Some cst2 ->
      let is_same_signature = cst1 = cst2 in
      if is_same_signature
      then to_redecl, to_recheck
      else
        let where_type_is_used = Typing_deps.get_ideps (Dep.GConst cst_id) in
        let to_recheck = DepSet.union where_type_is_used to_recheck in
        to_redecl, to_recheck

let get_gconsts_deps old_gconsts gconsts =
  SSet.fold (get_gconst_deps old_gconsts) gconsts (DepSet.empty, DepSet.empty)

(*****************************************************************************)
(* Determine which functions/classes have to be rechecked after comparing
 * the old and the new type signature of "cid" (class identifier).
*)
(*****************************************************************************)
let get_class_deps old_classes new_classes trace cid (to_redecl, to_recheck) =
  match SMap.find_unsafe cid old_classes, SMap.find_unsafe cid new_classes with
  | None, None -> to_redecl, to_recheck
  | None, _ | _, None ->
      get_all_dependencies trace cid (to_redecl, to_recheck)
  | Some class1, Some class2 when class_big_diff class1 class2 ->
      get_all_dependencies trace cid (to_redecl, to_recheck)
  | Some class1, Some class2 ->
      let nclass1 = NormalizeSig.class_type class1 in
      let nclass2 = NormalizeSig.class_type class2 in
      let deps, is_unchanged = ClassDiff.compare cid nclass1 nclass2 in
      let cid_hash = Typing_deps.Dep.make (Dep.Class cid) in
      if is_unchanged
      then
        let _, is_unchanged = ClassDiff.compare cid class1 class2 in
        if is_unchanged
        then to_redecl, to_recheck
        else
          (* If we reach this case it means that class1 and class2
           * have the same signatures, but that some of their
           * positions differ. We therefore must redeclare the sub-classes
           * but not recheck them.
           *)
          let to_redecl = get_extend_deps_ trace cid_hash to_redecl in
          to_redecl, to_recheck
      else
        let to_redecl = get_extend_deps_ trace cid_hash to_redecl in
        let to_recheck = DepSet.union to_redecl to_recheck in
        DepSet.union deps to_redecl, DepSet.union deps to_recheck

let get_classes_deps old_classes new_classes classes =
  SSet.fold
    (get_class_deps old_classes new_classes (ref DepSet.empty))
    classes
    (DepSet.empty, DepSet.empty)

(*****************************************************************************)
(* When the type of a class didn't change, returns a substitution from
 * the positions in the old type to the positions in the new ones.
 * The idea is applying this substitution is enough to compute the new type
 * of all the subclasses. No need to fully redeclare all of them.
 *)
(*****************************************************************************)
let get_classes_psubst old_classes new_classes classes =
  let subst = Hashtbl.create 23 in
  let is_empty = ref true in
  SSet.iter begin fun cid ->
    match SMap.find_unsafe cid old_classes, SMap.find_unsafe cid new_classes with
    | Some class1, Some class2 ->
        let subst_list, same = CompareTypes.class_ ([], true) class1 class2 in
        if same then begin
          List.iter begin fun (pos1, pos2) ->
            is_empty := false;
            Hashtbl.add subst pos1 pos2
          end subst_list
        end
    | _ -> ()
  end classes;
  subst, !is_empty
