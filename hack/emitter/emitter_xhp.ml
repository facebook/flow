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
open Utils
module N = Nast
module C = Emitter_core
module XC = Emitter_consts.XHPConsts

(* Implementation of XHP. We implement XHP by desugaring down to base
 * language features.
 * See https://github.com/facebook/xhp-lib/blob/master/INTERNALS.md for
 * documentation about some of the details.
 *
 * The HHVM implementation is built by directly desugaring xhp in the
 * parser and so lives in hphp/parser/hphp.y
 *)

let get_prop_name s = lstrip s ":"

(* convert a prop to an entry that goes into the array returned
 * by the __xhpAttributeDeclaration method. Returns None if the prop
 * isn't an xhp prop. *)
let convert_prop prop =
  if not prop.N.cv_is_xhp then None else
  let p, name = prop.N.cv_id in
  let default_val = match prop.N.cv_expr with
    | None -> p, N.Null
    | Some e -> e in

  (* Build the type information that XHP wants:
   * https://github.com/facebook/xhp-lib/blob/master/INTERNALS.md *)
  let null = p, N.Null in
  (* XXX: we can't emit TYPE_ENUM currently since we squashed it down into
   * int/string/mixed during naming *)
  let rec get_type hopt = match hopt with
    | None -> XC.cTYPE_VAR, null
    | Some (p, h) -> match h with
        | N.Hoption h -> get_type (Some h)
        | N.Hprim N.Tstring -> XC.cTYPE_STRING, null
        | N.Hprim N.Tbool -> XC.cTYPE_BOOL, null
        | N.Hprim N.Tint -> XC.cTYPE_INTEGER, null
        | N.Harray _ | N.Htuple _ | N.Hshape _ -> XC.cTYPE_ARRAY, null
        | N.Habstr (s, _) | N.Happly ((_, s), _) ->
          XC.cTYPE_OBJECT, (p, N.String (p, C.fmt_name s))
        | N.Hprim (N.Tvoid | N.Tresource | N.Tnum |
                   N.Tarraykey | N.Tnoreturn as pr) ->
          XC.cTYPE_OBJECT, (p, N.String (p, Emitter_types.fmt_prim pr))
        | N.Hany | N.Hmixed | N.Hthis | N.Hfun _ | N.Haccess _ ->
          XC.cTYPE_VAR, null
        | N.Hprim N.Tfloat -> XC.cTYPE_FLOAT, null
  in
  let attr_type, type_name = get_type prop.N.cv_type in
  (* The elements of the array are:
   * <type, extra_type, default val, required>
   * type is an value from the XHPAttributeType class,
   * extra_type is a class name or something.
   * XXX: We report that none of the attrs are required since the
   * nast doesn't have the information currently *)
  Some (
    (p, N.String (p, get_prop_name name)),
    (p, C.make_varray [
      p, N.Int (p, string_of_int attr_type);
      type_name;
      default_val;
      p, N.Int (p, "0");
    ]))

let named_body block = N.NamedBody {N.fnb_nast = block; N.fnb_unsafe = false }
let empty_method name =
  {
    N.m_final = false; m_abstract = false; m_visibility = N.Protected;
    m_tparams = []; m_variadic = N.FVnonVariadic;
    m_params = []; m_fun_kind = Ast.FSync;
    m_user_attributes = []; m_ret = None;
    m_body = named_body [];
    m_name = name;
  }

(* From a list of xhp attributes, generate a class's
 * __xhpAttributeDeclaration method, which returns an array describing
 * all of the xhp attributes of the class. *)
let make_attr_decl_method cls xhp_props =
  if xhp_props = [] && cls.N.c_xhp_attr_uses = [] then [] else

  let attr_decl_meth_name = "__xhpAttributeDeclaration" in

  let make_attr_decl_call pos cid =
    pos,
    N.Call (N.Cnormal,
            (pos, N.Class_const (cid, (pos, attr_decl_meth_name))),
            [], [])
  in

  let convert_attr_use = function
    | pos, N.Happly ((_, cls), _) ->
      make_attr_decl_call pos (N.CI (pos, cls))
    | _ -> C.bug "xhp attr use not apply??"
  in

  let p = fst cls.N.c_name in
  (* We need to produce an array describing all of our attributes.
   * Our attributes are the combination of the declared attrs of this
   * class, the parent's attrs, and any attrs pulled in from
   * the xhp_attr_uses weird trait things. We collect them all up with
   * array_merge. *)
  let attr_expr =
    p, N.Call (N.Cnormal, (p, N.Id (p, "\\array_merge")), ([
      make_attr_decl_call p N.CIparent;
      p, C.make_kvarray (xhp_props); ] @
     List.map ~f:convert_attr_use cls.N.c_xhp_attr_uses), []) in

  (* XXX: once we support static we should cache this stuff in a static
   * variable properly. *)
  (* XXX: HHVM returns this stuff with a ref, which I think is an
   * optimization and not semantic in this situation? *)
  let body = [ N.Return (p, Some attr_expr) ] in
  let meth = empty_method (p, attr_decl_meth_name) in
  [{ meth with N.m_body = named_body body }]


(* make the __xhpChildrenDeclaration method.
 * XXX: we don't actually have children information, so we always
 * generate a method saying we can handle any children *)
let make_children_decl_method cls =
  let children_decl_meth_name = "__xhpChildrenDeclaration" in

  (* If there is already a children decl method defined,
   * don't emit our own. *)
  if List.exists cls.N.c_static_methods
    (fun m -> snd m.N.m_name = children_decl_meth_name) then [] else

  let p = fst cls.N.c_name in
  let body = [ N.Return (p, Some (p, N.Int (p, "1"))) ] in
  let meth = empty_method (p, children_decl_meth_name) in
  [{ meth with N.m_body = named_body body }]


(* Generate the __xhpCategoryDeclaration method from the category. *)
let make_category_decl_method cls =
  if cls.N.c_xhp_category = [] then [] else

  let children_decl_meth_name = "__xhpCategoryDeclaration" in
  (* This just returns an array mapping all of the categories to 1 *)
  let entries = List.map cls.N.c_xhp_category begin fun (pos, name) ->
    ((pos, N.String (pos, name)), (pos, N.Int (pos, "1"))) end in

  let p = fst cls.N.c_name in
  let body = [ N.Return (p, Some (p, C.make_kvarray entries)) ] in
  let meth = empty_method (p, children_decl_meth_name) in
  [{ meth with N.m_body = named_body body }]


(* Given a class that uses xhp, transform the class to add any
 * methods, etc that we need to implement the xhp stuff.
 * throws all that info away. *)
let convert_class cls =
  if not cls.N.c_is_xhp then cls else

  let xhp_props = List.filter_map ~f:convert_prop cls.N.c_vars in
  let new_smethods = make_attr_decl_method cls xhp_props in
  (* These methods are bizarrely not static *)
  let new_methods = make_children_decl_method cls @
                    make_category_decl_method cls in

  { cls with N.c_static_methods = new_smethods @ cls.N.c_static_methods ;
             N.c_methods = new_methods @ cls.N.c_methods }


(* Convert an obj_get on a xhp attribute to a getAttribute call *)
let convert_obj_get p (obj, prop, null_flavor) =
  let propName = match prop with
    | _, N.Id (npos, s) -> npos, N.String (npos, get_prop_name s)
    | _ -> assert false in

  p,
  N.Call (N.Cnormal,
          (p, N.Obj_get (obj, (p, N.Id (p, "getAttribute")), null_flavor)),
          [propName], [])


(* Convert XML syntax into just a constructor call. *)
let convert_xml p (id, attrs, children) =
  (* Translate into a constructor call. The arguments are:
   *  1) map-like array of attributes
   *  2) vec-like array of children
   *  3) filename, for debugging
   *  4) line number, for debugging
   *)
  let convert_xml_attr ((pos, _) as name, v) = ((pos, N.String name), v) in
  let attrs_array = p, C.make_kvarray (List.map ~f:convert_xml_attr attrs) in
  let children_array = p, C.make_varray children in
  let filename = p, N.String (p, Pos.filename (Pos.to_absolute p)) in
  let line, _, _ = Pos.info_pos p in
  let line = p, N.Int (p, string_of_int line) in
  p,
  N.New (
    N.CI id,
    [attrs_array; children_array; filename; line], [])
