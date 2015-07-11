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
(* Pretty printing of types *)
(*****************************************************************************)

open Utils
open Typing_defs

module SN = Naming_special_names
module Reason = Typing_reason
(*****************************************************************************)
(* Computes the string representing a type in an error message.
 * We generally don't want to show the whole type. If an error was due
 * because something is a Vector instead of an int, we don't want to show
 * the type Vector<Vector<array<int>>> because it could be misleading.
 * The error is due to the fact that it is a Vector, regardless of the
 * type parameters.
 *)
(*****************************************************************************)

module ErrorString = struct

  let tprim = function
    | Nast.Tvoid       -> "void"
    | Nast.Tint        -> "an int"
    | Nast.Tbool       -> "a bool"
    | Nast.Tfloat      -> "a float"
    | Nast.Tstring     -> "a string"
    | Nast.Tclassname s -> "string ("^s^"::class)"
    | Nast.Tnum        -> "a num (int/float)"
    | Nast.Tresource   -> "a resource"
    | Nast.Tarraykey   -> "an array key (int/string)"
    | Nast.Tnoreturn   -> "noreturn (throws or exits)"

  let rec type_: type a. a ty_ -> _ = function
    | Tany               -> "an untyped value"
    | Tunresolved l      -> unresolved l
    | Tarray (x, y)      -> array (x, y)
    | Ttuple _           -> "a tuple"
    | Tmixed             -> "a mixed value"
    | Toption _          -> "a nullable type"
    | Tprim tp           -> tprim tp
    | Tvar _             -> "some value"
    | Tanon _    -> "a function"
    | Tfun _     -> "a function"
    | Tgeneric (x, _)    -> "a value of declared generic type "^x
    | Tabstract (ak, _)
        when AbstractKind.is_classname ak -> "a classname string"
    | Tabstract (ak, cstr) -> abstract ak cstr
    | Tclass ((_, x), _) -> "an object of type "^(strip_ns x)
    | Tapply ((_, x), _)
        when x = SN.Classes.cClassname -> "a classname string"
    | Tapply ((_, x), _) -> "an object of type "^(strip_ns x)
    | Tobject            -> "an object"
    | Tshape _           -> "a shape"
    | Taccess (root_ty, ids) -> tconst root_ty ids
    | Tthis -> "the type 'this'"

  and array: type a. a ty option * a ty option -> _ = function
    | None, None     -> "an array"
    | Some _, None   -> "an array (used like a vector)"
    | Some _, Some _ -> "an array (used like a hashtable)"
    | _              -> assert false

  and abstract ak cstr =
    let x = strip_ns @@ AbstractKind.to_string ak in
    match ak, cstr with
    | AKnewtype (_, _), _ -> "an object of type "^x
    | AKgeneric (_, _), _ -> "a value of generic type "^x
    | AKdependent (`cls c, []), Some (_, ty) ->
        type_ ty^" (known to be exactly the class '"^strip_ns c^"')"
    | AKdependent ((`static | `expr _), _), _ ->
        "the expression dependent type "^x
    | AKdependent (_, _::_), _ -> "the abstract type constant "^x
    | AKdependent _, _ ->
        "the type '"^x^"'"
        ^Option.value_map cstr ~default:""
          ~f:(fun (_, ty) -> "\n  that is compatible with "^type_ ty)

  and unresolved l =
    let l = List.map snd l in
    let l = List.map type_ l in
    let s = List.fold_right SSet.add l SSet.empty in
    let l = SSet.elements s in
    unresolved_ l

  and unresolved_ = function
    | []      -> "an undefined value"
    | [x]     -> x
    | x :: rl -> x^" or "^unresolved_ rl

  and tconst: type a. a ty -> _ -> _ = fun root_ty ids ->
    let f x =
      let x =
        if String.contains x '<'
        then "this"
        else x
      in
      List.fold_left (fun acc (_, sid) -> acc^"::"^sid)
        ("the type constant "^strip_ns x) ids in
    match snd root_ty with
    | Tgeneric (x, _) -> f x
    | Tapply ((_, x), _) -> f x
    | Tclass ((_, x), _) -> f x
    | Tabstract (ak, _) -> f @@ AbstractKind.to_string ak
    | Taccess _ as x ->
        List.fold_left (fun acc (_, sid) -> acc^"::"^sid)
          (type_ x) ids
     | _ ->
         "a type constant"

end

(*****************************************************************************)
(* Module used to "suggest" types.
 * When a type is missing, it is nice to suggest a type to the user.
 * However, there are some cases where parts of the type is still unresolved.
 * When that is the case, we print '...' and let the user replace the missing
 * parts with a real type. So if we inferred that something was a Vector,
 * but we didn't manage to infer the type of the elements, the output becomes:
 * Vector<...>.
 *)
(*****************************************************************************)

module Suggest = struct

  let rec type_: type a. a ty -> string = fun (_, ty) ->
    match ty with
    | Tarray _               -> "array"
    | Tthis                  -> SN.Typehints.this
    | Tunresolved _          -> "..."
    | Ttuple (l)             -> "("^list l^")"
    | Tany                   -> "..."
    | Tmixed                 -> "mixed"
    | Tgeneric (s, _)        -> s
    | Tabstract (AKgeneric (s, _), _) -> s
    | Toption ty             -> "?" ^ type_ ty
    | Tprim tp               -> prim tp
    | Tvar _                 -> "..."
    | Tanon _       -> "..."
    | Tfun _ -> "..."
    | Tapply ((_, cid), [])  -> Utils.strip_ns cid
    | Tapply ((_, cid), [x]) -> (Utils.strip_ns cid)^"<"^type_ x^">"
    | Tapply ((_, cid), l)   -> (Utils.strip_ns cid)^"<"^list l^">"
    | Tclass ((_, cid), []) -> Utils.strip_ns cid
    | Tabstract (AKnewtype (cid, []), _)  -> Utils.strip_ns cid
    | Tclass ((_, cid), [x]) -> (Utils.strip_ns cid)^"<"^type_ x^">"
    | Tabstract (AKnewtype (cid, [x]), _) ->
        (Utils.strip_ns cid)^"<"^type_ x^">"
    | Tclass ((_, cid), l) -> (Utils.strip_ns cid)^"<"^list l^">"
    | Tabstract (AKnewtype (cid, l), _)   ->
        (Utils.strip_ns cid)^"<"^list l^">"
    | Tabstract (AKdependent (_, _), _) -> "..."
    | Tobject                -> "..."
    | Tshape _               -> "..."
    | Taccess (root_ty, ids) ->
        let x =
          match snd root_ty with
          | Tapply ((_, x), _) -> Some x
          | Tthis -> Some SN.Typehints.this
          | _ -> None in
        (match x with
         | None -> "..."
         | Some x ->
            List.fold_left (fun acc (_, sid) -> acc^"::"^sid)
                           (strip_ns x) ids
        )

  and list: type a. a ty list -> string = function
    | []      -> ""
    | [x]     -> type_ x
    | x :: rl -> type_ x ^ ", "^ list rl

  and prim = function
    | Nast.Tvoid   -> "void"
    | Nast.Tint    -> "int"
    | Nast.Tbool   -> "bool"
    | Nast.Tfloat  -> "float"
    | Nast.Tstring -> "string"
    | Nast.Tclassname s -> "string ("^s^"::class)"
    | Nast.Tnum    -> "num (int/float)"
    | Nast.Tresource -> "resource"
    | Nast.Tarraykey -> "arraykey (int/string)"
    | Nast.Tnoreturn -> "noreturn"

end

(*****************************************************************************)
(* Pretty-printer of the "full" type. *)
(*****************************************************************************)

module Full = struct
  module Env = Typing_env

  let rec list_sep o s f l =
    match l with
    | [] -> ()
    | [x] -> f x
    | x :: rl -> f x; o s; list_sep o s f rl

  let rec ty: type a. _ -> _ -> _ -> a ty -> _ =
    fun st env o (_, x) -> ty_ st env o x

  and ty_: type a. _ -> _ -> _ -> a ty_ -> _ =
    fun st env o x ->
    let k: type b. b ty -> _ = fun x -> ty st env o x in
    let list: type c. (c ty -> unit) -> c ty list -> _ =
      fun x y -> list_sep o ", " x y in
    match x with
    | Tany -> o "_"
    | Tthis -> o SN.Typehints.this
    | Tmixed -> o "mixed"
    | Tarray (None, None) -> o "array"
    | Tarray (Some x, None) -> o "array<"; k x; o ">"
    | Tarray (Some x, Some y) -> o "array<"; k x; o ", "; k y; o ">"
    | Tarray (None, Some _) -> assert false
    | Tclass ((_, s), []) -> o s
    | Tapply ((_, s), []) -> o s
    | Tgeneric (s, _) -> o s
    | Taccess (root_ty, ids) ->
        k root_ty;
        o (List.fold_left (fun acc (_, sid) -> acc ^ "::" ^ sid) "" ids)
    | Toption x -> o "?"; k x
    | Tprim x -> prim o x
    | Tvar n when ISet.mem n st -> o "[rec]"
    | Tvar n ->
        let _, ety = Env.expand_type env (Reason.Rnone, x) in
        let st = ISet.add n st in
        ty st env o ety
    | Tfun ft ->
      if ft.ft_abstract then o "abs " else ();
      o "(function"; fun_type st env o ft; o ")";
      (match ft.ft_ret with
        | (Reason.Rdynamic_yield _, _) -> o " [DynamicYield]"
        | _ -> ())
    | Tclass ((_, s), tyl) -> o s; o "<"; list k tyl; o ">"
    | Tabstract (AKnewtype (s, []), _) -> o s
    | Tabstract (AKnewtype (s, tyl), _) -> o s; o "<"; list k tyl; o ">"
    | Tabstract (ak, _) -> o @@ AbstractKind.to_string ak;
    (* Don't strip_ns here! We want the FULL type, including the initial slash.
    *)
    | Tapply ((_, s), tyl) -> o s; o "<"; list k tyl; o ">"
    | Ttuple tyl -> o "("; list k tyl; o ")"
    | Tanon _ -> o "[fun]"
    | Tunresolved tyl -> list_sep o "& " k tyl
    | Tobject -> o "object"
    | Tshape _ -> o "[shape]"

  and prim o x =
    o (match x with
    | Nast.Tvoid   -> "void"
    | Nast.Tint    -> "int"
    | Nast.Tbool   -> "bool"
    | Nast.Tfloat  -> "float"
    | Nast.Tstring -> "string"
    | Nast.Tclassname s -> "string ("^s^"::class)"
    | Nast.Tnum    -> "num"
    | Nast.Tresource -> "resource"
    | Nast.Tarraykey -> "arraykey"
    | Nast.Tnoreturn -> "noreturn"
    )

  and fun_type: type a. _ -> _ -> _ -> a fun_type -> _ =
    fun st env o ft ->
    (match ft.ft_tparams, ft.ft_arity with
      | [], Fstandard _ -> ()
      | [], _ -> o "<...>";
      | l, Fstandard _ -> o "<"; list_sep o ", " (tparam o) l; o ">"
      | l, _ -> o "<"; list_sep o ", " (tparam o) l; o "..."; o ">"
    );
    o "("; list_sep o ", " (fun_param st env o) ft.ft_params; o "): ";
    ty st env o ft.ft_ret

  and fun_param: type a. _ -> _ -> _ -> a fun_param -> _ =
    fun st env o (param_name, param_type) ->
    match param_name, param_type with
    | None, _ -> ty st env o param_type
    | Some param_name, (_, Tany) -> o param_name
    | Some param_name, param_type ->
        ty st env o param_type; o " "; o param_name

  and tparam o (_, (_, x), _) = o x

  let to_string env x =
    let buf = Buffer.create 50 in
    ty ISet.empty env (Buffer.add_string buf) x;
    Buffer.contents buf

  let to_string_strip_ns env x =
    let buf = Buffer.create 50 in
    let add_string str =
      let str = Utils.strip_ns str in
      Buffer.add_string buf str
    in
    ty ISet.empty env add_string x;
    Buffer.contents buf

  let to_string_decl (x: decl ty) =
    let env = Typing_env.empty TypecheckerOptions.default Relative_path.default in
    to_string env x
end

(*****************************************************************************)
(* Prints the internal type of a class, this code is meant to be used for
 * debugging purposes only.
 *)
(*****************************************************************************)

module PrintClass = struct

  let indent = "    "
  let bool = string_of_bool
  let sset s =
    let contents = SSet.fold (fun x acc -> x^" "^acc) s "" in
    Printf.sprintf "Set( %s)" contents

  let pos p =
    let line, start, end_ = Pos.info_pos p in
    Printf.sprintf "(line %d: chars %d-%d)" line start end_

  let class_kind = function
    | Ast.Cabstract -> "Cabstract"
    | Ast.Cnormal -> "Cnormal"
    | Ast.Cinterface -> "Cinterface"
    | Ast.Ctrait -> "Ctrait"
    | Ast.Cenum -> "Cenum"

  let constraint_ty_opt = function
    | None -> ""
    | Some (Ast.Constraint_as, ty) -> "as " ^ (Full.to_string_decl ty)
    | Some (Ast.Constraint_super, ty) -> "super " ^ (Full.to_string_decl ty)

  let variance = function
    | Ast.Covariant -> "+"
    | Ast.Contravariant -> "-"
    | Ast.Invariant -> ""

  let tparam (var, (position, name), cstr_opt) =
    variance var^pos position^" "^name^" "^
    (constraint_ty_opt cstr_opt)

  let tparam_list l =
    List.fold_right (fun x acc -> tparam x^", "^acc) l ""

  let class_elt ce =
    let vis =
      match ce.ce_visibility with
      | Vpublic -> "public"
      | Vprivate _ -> "private"
      | Vprotected _ -> "protected"
    in
    let synth = (if ce.ce_synthesized then "synthetic " else "") in
    let type_ = Full.to_string_decl ce.ce_type in
    synth^vis^" "^type_

  let class_elt_smap m =
    SMap.fold begin fun field v acc ->
      "("^field^": "^class_elt v^") "^acc
    end m ""

  let class_elt_smap_with_breaks m =
    SMap.fold begin fun field v acc ->
      "\n"^indent^field^": "^(class_elt v)^acc
    end m ""

  let typeconst {
    ttc_name = tc_name;
    ttc_constraint = tc_constraint;
    ttc_type = tc_type;
    ttc_origin = origin;
  } =
    let name = snd tc_name in
    let ty x = Full.to_string_decl x in
    let constraint_ =
      match tc_constraint with
      | None -> ""
      | Some x -> " as "^ty x
    in
    let type_ =
      match tc_type with
      | None -> ""
      | Some x -> " = "^ty x
    in
    name^constraint_^type_^" (origin:"^origin^")"

  let typeconst_smap m =
    SMap.fold begin fun _ v acc ->
      "\n("^(typeconst v)^")"^acc
    end m ""

  let ancestors_smap m =
    (* Format is as follows:
     *    ParentKnownToHack
     *  ! ParentCompletelyUnknown
     *  ~ ParentPartiallyKnown  (interface|abstract|trait)
     *
     * ParentPartiallyKnown must inherit one of the ! Unknown parents, so that
     * sigil could be omitted *)
    SMap.fold begin fun field v acc ->
      let sigil, kind = match Typing_env.Classes.get field with
        | None -> "!", ""
        | Some {tc_members_fully_known; tc_kind; _} ->
          (if tc_members_fully_known then " " else "~"),
          " ("^class_kind tc_kind^")"
      in
      let ty_str = Full.to_string_decl v in
      "\n"^indent^sigil^" "^ty_str^kind^acc
    end m ""

  let user_attribute_list xs =
    List.fold_left begin fun acc { Nast.ua_name; _ } ->
      acc^"("^snd ua_name^": expr) "
    end "" xs

  let constructor (ce_opt, consist) =
    let consist_str = if consist then " (consistent in hierarchy)" else "" in
    let ce_str = match ce_opt with
      | None -> ""
      | Some ce -> class_elt ce
    in ce_str^consist_str

  let class_type c =
    let tc_need_init = bool c.tc_need_init in
    let tc_members_fully_known = bool c.tc_members_fully_known in
    let tc_abstract = bool c.tc_abstract in
    let tc_deferred_init_members = sset c.tc_deferred_init_members in
    let tc_kind = class_kind c.tc_kind in
    let tc_name = c.tc_name in
    let tc_tparams = tparam_list c.tc_tparams in
    let tc_consts = class_elt_smap c.tc_consts in
    let tc_typeconsts = typeconst_smap c.tc_typeconsts in
    let tc_props = class_elt_smap c.tc_props in
    let tc_sprops = class_elt_smap c.tc_sprops in
    let tc_methods = class_elt_smap_with_breaks c.tc_methods in
    let tc_smethods = class_elt_smap_with_breaks c.tc_smethods in
    let tc_construct = constructor c.tc_construct in
    let tc_ancestors = ancestors_smap c.tc_ancestors in
    let tc_req_ancestors = ancestors_smap c.tc_req_ancestors in
    let tc_req_ancestors_extends = sset c.tc_req_ancestors_extends in
    let tc_extends = sset c.tc_extends in
    let tc_user_attributes = user_attribute_list c.tc_user_attributes in
    "tc_need_init: "^tc_need_init^"\n"^
    "tc_members_fully_known: "^tc_members_fully_known^"\n"^
    "tc_abstract: "^tc_abstract^"\n"^
    "tc_deferred_init_members: "^tc_deferred_init_members^"\n"^
    "tc_kind: "^tc_kind^"\n"^
    "tc_name: "^tc_name^"\n"^
    "tc_tparams: "^tc_tparams^"\n"^
    "tc_consts: "^tc_consts^"\n"^
    "tc_typeconsts: "^tc_typeconsts^"\n"^
    "tc_props: "^tc_props^"\n"^
    "tc_sprops: "^tc_sprops^"\n"^
    "tc_methods: "^tc_methods^"\n"^
    "tc_smethods: "^tc_smethods^"\n"^
    "tc_construct: "^tc_construct^"\n"^
    "tc_ancestors: "^tc_ancestors^"\n"^
    "tc_extends: "^tc_extends^"\n"^
    "tc_req_ancestors: "^tc_req_ancestors^"\n"^
    "tc_req_ancestors_extends: "^tc_req_ancestors_extends^"\n"^
    "tc_user_attributes: "^tc_user_attributes^"\n"^
    ""
end

module PrintFun = struct

  let fparam (sopt, ty) =
    let s = match sopt with
      | None -> "[None]"
      | Some s -> s in
    s ^ " " ^ (Full.to_string_decl ty) ^ ", "

  let farity = function
    | Fstandard (min, max) -> Printf.sprintf "non-variadic: %d to %d" min max
    | Fvariadic (min, _) ->
      Printf.sprintf "variadic: ...$arg-style (PHP 5.6); min: %d" min
    | Fellipsis min -> Printf.sprintf "variadic: ...-style (Hack); min: %d" min

  let fparams l =
    List.fold_right (fun x acc -> (fparam x)^acc) l ""

  let fun_type f =
    let ft_pos = PrintClass.pos f.ft_pos in
    let ft_abstract = string_of_bool f.ft_abstract in
    let ft_arity = farity f.ft_arity in
    let ft_tparams = PrintClass.tparam_list f.ft_tparams in
    let ft_params = fparams f.ft_params in
    let ft_ret = Full.to_string_decl f.ft_ret in
    "ft_pos: "^ft_pos^"\n"^
    "ft_abstract: "^ft_abstract^"\n"^
    "ft_arity: "^ft_arity^"\n"^
    "ft_tparams: "^ft_tparams^"\n"^
    "ft_params: "^ft_params^"\n"^
    "ft_ret: "^ft_ret^"\n"^
    ""
end

module PrintTypedef = struct

  let typedef = function
    | Typing_heap.Typedef.Error -> "[Error]"
    | Typing_heap.Typedef.Ok (_vis, tparaml, constr_opt, ty, pos) ->
      let tparaml_s = PrintClass.tparam_list tparaml in
      let constr_s = match constr_opt with
        | None -> "[None]"
        | Some constr -> Full.to_string_decl constr in
      let ty_s = Full.to_string_decl ty in
      let pos_s = PrintClass.pos pos in
      "ty: "^ty_s^"\n"^
      "tparaml: "^tparaml_s^"\n"^
      "constraint: "^constr_s^"\n"^
      "pos: "^pos_s^"\n"^
      ""

end

(*****************************************************************************)
(* User API *)
(*****************************************************************************)

let error: type a. a ty_ -> _ = fun ty -> ErrorString.type_ ty
let suggest: type a. a ty -> _ =  fun ty -> Suggest.type_ ty
let full env ty = Full.to_string env ty
let full_strip_ns env ty = Full.to_string_strip_ns env ty
let debug env ty =
  let e_str = error (snd ty) in
  let f_str = full_strip_ns env ty in
  e_str^" "^f_str
let class_ c = PrintClass.class_type c
let gconst gc = Full.to_string_decl gc
let fun_ f = PrintFun.fun_type f
let typedef td = PrintTypedef.typedef td
let strip_ns env phase_ty =
  match phase_ty with
  | Typing_defs.DeclTy type_ ->
      full_strip_ns env type_
  | Typing_defs.LoclTy type_ ->
      full_strip_ns env type_
