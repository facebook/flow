(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(** Module "naming" a program.
 *
 * The naming phase consists in several things
 * 1- get all the global names
 * 2- transform all the local names into a unique identifier
 *)
open Utils
open Ast

module N = Nast
module ShapeMap = N.ShapeMap
module SN = Naming_special_names

(*****************************************************************************)
(* The types *)
(*****************************************************************************)

type fun_set = Utils.SSet.t
type class_set = Utils.SSet.t
type typedef_set = Utils.SSet.t
type const_set = Utils.SSet.t
type decl_set = fun_set * class_set * typedef_set * const_set

(* We want to keep the positions of names that have been
 * replaced by identifiers.
 *)
type positioned_ident = (Pos.t * Ident.t)
type map = positioned_ident SMap.t
type canon_names_map = string SMap.t
let canon_key = String.lowercase

(* <T as A>, A is a type constraint *)
type type_constraint = (Ast.constraint_kind * Ast.hint) option

type genv = {

  (* strict? decl? partial? *)
  in_mode: FileInfo.mode;

  (* various options that control the strictness of the typechecker *)
  tcopt: TypecheckerOptions.t;

  (* are we in the body of a try statement? *)
  in_try: bool;

  (* are we in the body of a non-static member function? *)
  in_instance_method: bool;

  (* In function foo<T1, ..., Tn> or class<T1, ..., Tn>, the field
   * type_params knows T1 .. Tn. It is able to find out about the
   * constraint on these parameters. *)
  type_params: type_constraint SMap.t;

  (* The parameters is their original order
   * Necessary to type "this".
   *)
  type_paraml: Ast.id list;

  (* Set of class names defined, and their positions *)
  classes: (map * canon_names_map) ref;

  (* Set of function names defined, and their positions *)
  funs: (map * canon_names_map) ref;

  (* Set of typedef names defined, and their position *)
  typedefs: map ref;

  (* Set of constant names defined, and their position *)
  gconsts: map ref;

  (* The current class, None if we are in a function *)
  current_cls: (Ast.id * Ast.class_kind) option;

  (* Normally we don't need to add dependencies at this stage, but there
   * are edge cases when we do.  *)
  droot: Typing_deps.Dep.variant option;

  (* Namespace environment, e.g., what namespace we're in and what use
   * declarations are in play. *)
  namespace: Namespace_env.env;
}

(* How to behave when we see an unbound name.  Either we raise an
 * error, or we call a function first and continue if it can resolve
 * the name.  This is used to nest environments when processing
 * closures. *)
type unbound_mode =
  | UBMErr
  | UBMFunc of ((Pos.t * string) -> positioned_ident)

(* The local environment *)
type lenv = {

  (* The set of locals *)
  locals: map ref;

  (* The set of constants *)
  consts: map ref;

  (* We keep all the locals, even if we are in a different scope
   * to provide better error messages.
   * if you write:
   * if(...) {
   *   $x = ...;
   * }
   * Technically, passed this point, $x is unbound.
   * But it is much better to keep it somewhere, so that you can
   * say it is bound, but in a different scope.
   *)
  all_locals: Pos.t SMap.t ref;

  (* Some statements can define new variables afterwards, e.g.,
   * if (...) {
   *    $x = ...;
   * } else {
   *    $x = ...;
   * }
   * We need to give $x the same name in both branches, but we don't want
   * $x to actually be a local until after the if block. So we stash it here,
   * to indicate a name has been pre-allocated, but that the variable isn't
   * actually defined yet.
   *)
  pending_locals: map ref;

  (* Tag controlling what we do when we encounter an unbound name.
   * This is used when processing a lambda expression body that has
   * an automatic use list.
   *
   * See expr_lambda for details.
   *)
  unbound_mode: unbound_mode;

  (* The presence of an "UNSAFE" in the function body changes the
   * verifiability of the function's return type, since the unsafe
   * block could return. For the sanity of the typechecker, we flatten
   * this out, but need to track if we've seen an "UNSAFE" in order to
   * do so. *)
  has_unsafe: bool ref;
}

(* The environment VISIBLE to the outside world. *)
type env = {
  itcopt: TypecheckerOptions.t;
  iclasses: map * canon_names_map;
  ifuns: map * canon_names_map;
  itypedefs: map;
  iconsts: map;
}

(**
 * Returns the list of classes which have been seen.
 * Useful for things like dumping json formatted information about the www
 * world.
 *)
let get_classes env =
  SMap.fold (fun key _ acc -> key :: acc) (fst env.iclasses) []

(*****************************************************************************)
(* Predefined names *)
(*****************************************************************************)

let predef_funs = ref SMap.empty
let predef_funnames = ref SMap.empty
let predef_fun x =
  let var = Pos.none, Ident.make x in
  let canon_x = canon_key x in
  predef_funs := SMap.add x var !predef_funs;
  predef_funnames := SMap.add canon_x x !predef_funnames;
  x

let is_int    = predef_fun SN.StdlibFunctions.is_int
let is_bool   = predef_fun SN.StdlibFunctions.is_bool
let is_array  = predef_fun SN.StdlibFunctions.is_array
let is_float  = predef_fun SN.StdlibFunctions.is_float
let is_string = predef_fun SN.StdlibFunctions.is_string
let is_null   = predef_fun SN.StdlibFunctions.is_null
let is_resource = predef_fun SN.StdlibFunctions.is_resource

let predef_tests_list =
  [is_int; is_bool; is_float; is_string; is_null; is_array; is_resource]
let predef_tests = List.fold_right SSet.add predef_tests_list SSet.empty

(*****************************************************************************)
(* Empty (initial) environments *)
(*****************************************************************************)

let empty tcopt = {
  itcopt    = tcopt;
  iclasses  = SMap.empty, SMap.empty;
  ifuns     = !predef_funs, !predef_funnames;
  itypedefs = SMap.empty;
  iconsts   = SMap.empty;
}

(* The primitives to manipulate the naming environment *)
module Env = struct

  let empty_local() = {
    locals     = ref SMap.empty;
    consts     = ref SMap.empty;
    all_locals = ref SMap.empty;
    pending_locals = ref SMap.empty;
    unbound_mode = UBMErr;
    has_unsafe = ref false;
  }

  let empty_global nenv = {
    in_mode       = FileInfo.Mstrict;
    tcopt         = nenv.itcopt;
    in_try        = false;
    in_instance_method = false;
    type_params   = SMap.empty;
    type_paraml   = [];
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
    current_cls   = None;
    droot         = None;
    namespace     = Namespace_env.empty;
  }

  let make_class_genv nenv params mode tparams (cid, ckind) namespace = {
    in_mode       =
      (if !Autocomplete.auto_complete then FileInfo.Mpartial else mode);
    tcopt         = nenv.itcopt;
    in_try        = false;
    in_instance_method = false;
    type_params   = params;
    type_paraml   = tparams;
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
    current_cls   = Some (cid, ckind);
    droot         = Some (Typing_deps.Dep.Class (snd cid));
    namespace;
  }

  let make_class_env nenv params c =
    let tparams = List.map (fun (_, x, _) -> x) c.c_tparams in
    let genv = make_class_genv nenv params c.c_mode
      tparams (c.c_name, c.c_kind) c.c_namespace in
    let lenv = empty_local () in
    let env  = genv, lenv in
    env

  let make_typedef_genv nenv cstrs tdef = {
    in_mode       = FileInfo.(if !Ide.is_ide_mode then Mpartial else Mstrict);
    tcopt         = nenv.itcopt;
    in_try        = false;
    in_instance_method = false;
    type_params   = cstrs;
    type_paraml   = List.map (fun (_, x, _) -> x) tdef.t_tparams;
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
    current_cls   = None;
    droot         = None;
    namespace     = tdef.t_namespace;
  }

  let make_typedef_env genv cstrs tdef =
    let genv = make_typedef_genv genv cstrs tdef in
    let lenv = empty_local () in
    let env  = genv, lenv in
    env

  let make_fun_genv nenv params f_mode f_name f_namespace = {
    in_mode       = f_mode;
    tcopt         = nenv.itcopt;
    in_try        = false;
    in_instance_method = false;
    type_params   = params;
    type_paraml   = [];
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
    current_cls   = None;
    droot         = Some (Typing_deps.Dep.Fun f_name);
    namespace     = f_namespace;
  }

  let make_fun_decl_genv nenv params f =
    make_fun_genv nenv params f.f_mode (snd f.f_name) f.f_namespace

  let make_const_genv nenv cst = {
    in_mode       = cst.cst_mode;
    tcopt         = nenv.itcopt;
    in_try        = false;
    in_instance_method = false;
    type_params   = SMap.empty;
    type_paraml   = [];
    classes       = ref nenv.iclasses;
    funs          = ref nenv.ifuns;
    typedefs      = ref nenv.itypedefs;
    gconsts       = ref nenv.iconsts;
    current_cls   = None;
    droot         = Some (Typing_deps.Dep.GConst (snd cst.cst_name));
    namespace     = cst.cst_namespace;
  }

  let make_const_env nenv cst =
    let genv = make_const_genv nenv cst in
    let lenv = empty_local () in
    let env  = genv, lenv in
    env

  let new_var env (p, x) =
    if SMap.mem x !env
    then begin
      let p', _ = SMap.find_unsafe x !env in
      Errors.error_name_already_bound x x p p'
    end;
    let y = p, Ident.make x in
    env := SMap.add x y !env;
    y

  let lookup genv env (p, x) =
    let v = SMap.get x env in
    match v with
    | None ->
      (match genv.in_mode with
        | FileInfo.Mstrict -> Errors.unbound_name p x `const
        | FileInfo.Mpartial | FileInfo.Mdecl when not
            (TypecheckerOptions.assume_php genv.tcopt) ->
          Errors.unbound_name p x `const
        | FileInfo.Mdecl | FileInfo.Mpartial -> ()
      );
      p, Ident.make x
    | Some v -> p, snd v

  (* Check and see if the user might have been trying to use one of the
   * generics in scope as a runtime value *)
  let check_no_runtime_generic genv (p, name) =
    let tparaml = SMap.keys genv.type_params in
    if List.mem name tparaml then Errors.generic_at_runtime p;
    ()

  let canonicalize genv env_and_names (p, name) kind =
    let env, canon_names = !env_and_names in
    if SMap.mem name env then (p, name)
    else (
      let name_key = canon_key name in
      match SMap.get name_key canon_names with
        | Some canonical ->
          let p_canon, _ = SMap.find_unsafe canonical env in
          Errors.did_you_mean_naming p name p_canon canonical;
          (* Recovering from the capitalization error means
           * returning the name in its canonical form *)
          p, canonical
        | None ->
          (match genv.in_mode with
            | FileInfo.Mpartial | FileInfo.Mdecl
                when TypecheckerOptions.assume_php genv.tcopt
                || name = SN.Classes.cUnknown -> ()
            | FileInfo.Mstrict -> Errors.unbound_name p name kind
            | FileInfo.Mpartial | FileInfo.Mdecl ->
                Errors.unbound_name p name kind
          );
          p, name
    )

  let check_variable_scoping env (p, x) =
    match SMap.get x !(env.all_locals) with
    | Some p' -> Errors.different_scope p x p'
    | None -> ()

  let is_superglobal =
    let l = [
      "$GLOBALS"; "$_SERVER"; "$_GET"; "$_POST"; "$_FILES";
      "$_COOKIE"; "$_SESSION"; "$_REQUEST"; "$_ENV"
    ] in
    let h = Hashtbl.create 23 in
    List.iter (fun x -> Hashtbl.add h x true) l;
    fun x -> Hashtbl.mem h x

  (* Adds a local variable, without any check *)
  let add_lvar (_, lenv) (_, name) (p, x) =
    lenv.locals := SMap.add name (p, x) !(lenv.locals)

  (* Defines a new local variable *)
  let new_lvar (_, lenv) (p, x) =
    let lcl = SMap.get x !(lenv.locals) in
    let p, ident =
      match lcl with
      | Some lcl -> p, snd lcl
      | None ->
          let ident = match SMap.get x !(lenv.pending_locals) with
            | Some (_, ident) -> ident
            | None -> Ident.make x in
          let y = p, ident in
          lenv.all_locals := SMap.add x p !(lenv.all_locals);
          lenv.locals := SMap.add x y !(lenv.locals);
          y
    in
    Naming_hooks.dispatch_lvar_hook ident (p, x) !(lenv.locals);
    p, ident

  let new_pending_lvar (_, lenv) (p, x) =
    match SMap.get x !(lenv.locals), SMap.get x !(lenv.pending_locals) with
    | None, None ->
        let y = p, Ident.make x in
        lenv.pending_locals := SMap.add x y !(lenv.pending_locals)
    | _ -> ()

  let promote_pending (_, lenv as env) =
    SMap.iter begin fun x (p, ident) ->
      add_lvar env (p, x) (p, ident)
    end !(lenv.pending_locals);
    lenv.pending_locals := SMap.empty

  let handle_undefined_variable (genv, env) (p, x) =
    match env.unbound_mode with
    | UBMErr -> Errors.undefined p x; p, Ident.make x
    | UBMFunc f -> f (p, x)

  (* Function used to name a local variable *)
  let lvar (genv, env) (p, x) =
    let p, ident =
      if is_superglobal x && genv.in_mode = FileInfo.Mpartial
      then p, Ident.tmp()
      else
        let lcl = SMap.get x !(env.locals) in
        match lcl with
        | Some lcl -> p, snd lcl
        | None when not !Autocomplete.auto_complete ->
            check_variable_scoping env (p, x);
            handle_undefined_variable (genv, env) (p, x)
        | None -> p, Ident.tmp()
    in
    Naming_hooks.dispatch_lvar_hook ident (p, x) !(env.locals);
    p, ident

  let get_name genv namespace x =
    ignore (lookup genv namespace x); x

  (* For dealing with namespace fallback on constants *)
  let elaborate_and_get_name_with_fallback mk_dep genv genv_sect x =
    let fq_x = Namespaces.elaborate_id genv.namespace x in
    let need_fallback =
      genv.namespace.Namespace_env.ns_name <> None &&
      not (String.contains (snd x) '\\') in
    let pos_map = !(genv_sect) in
    if need_fallback then begin
      let global_x = (fst x, "\\" ^ (snd x)) in
      (* Explicitly add dependencies on both of the consts we could be
       * referring to here. Normally naming doesn't have to deal with
       * deps at all -- they are added during typechecking just by the
       * nature of looking up a class or function name. However, we're
       * flattening namespaces here, and the fallback behavior of
       * consts means that we might suddenly be referring to a
       * different const without any change to the callsite at
       * all. Adding both dependencies explicitly captures this
       * action-at-a-distance. *)
      Typing_deps.add_idep genv.droot (mk_dep (snd fq_x));
      Typing_deps.add_idep genv.droot (mk_dep (snd global_x));
      let mem (_, s) = SMap.mem s pos_map in
      match mem fq_x, mem global_x with
      (* Found in the current namespace *)
      | true, _ -> get_name genv pos_map fq_x
      (* Found in the global namespace *)
      | _, true -> get_name genv pos_map global_x
      (* Not found. Pick the more specific one to error on. *)
      | false, false -> get_name genv pos_map fq_x
    end else
      get_name genv pos_map fq_x

  (* For dealing with namespace fallback on functions *)
  let elaborate_and_get_name_with_canonicalized_fallback mk_dep genv genv_sect x =
    let fq_x = Namespaces.elaborate_id genv.namespace x in
    let need_fallback =
      genv.namespace.Namespace_env.ns_name <> None &&
      not (String.contains (snd x) '\\') in
    let pos_map, canon_map = !(genv_sect) in
    if need_fallback then begin
      let global_x = (fst x, "\\" ^ (snd x)) in
      (* Explicitly add dependencies on both of the functions we could be
       * referring to here. Normally naming doesn't have to deal with deps at
       * all -- they are added during typechecking just by the nature of
       * looking up a class or function name. However, we're flattening
       * namespaces here, and the fallback behavior of functions means that we
       * might suddenly be referring to a different function without any
       * change to the callsite at all. Adding both dependencies explicitly
       * captures this action-at-a-distance. *)
      Typing_deps.add_idep genv.droot (mk_dep (snd fq_x));
      Typing_deps.add_idep genv.droot (mk_dep (snd global_x));
      (* canonicalize the names being searched *)
      let mem (_, nm) = SMap.mem (canon_key nm) (canon_map) in
      match mem fq_x, mem global_x with
      | true, _ -> (* Found in the current namespace *)
        let fq_x = canonicalize genv genv_sect fq_x `func in
        get_name genv pos_map fq_x
      | _, true -> (* Found in the global namespace *)
        let global_x = canonicalize genv genv_sect global_x `func in
        get_name genv pos_map global_x
      | false, false ->
        (* Not found. Pick the more specific one to error on. *)
        get_name genv pos_map fq_x
    end else
      let fq_x = canonicalize genv genv_sect fq_x `func in
      get_name genv pos_map fq_x

  let global_const (genv, env) x  =
    elaborate_and_get_name_with_fallback
      (* Same idea as Dep.FunName, see below. *)
      (fun x -> Typing_deps.Dep.GConstName x)
      genv
      genv.gconsts
      x

  let class_name (genv, _) x =
    (* Generic names are not allowed to shadow class names *)
    check_no_runtime_generic genv x;
    let x = Namespaces.elaborate_id genv.namespace x in
    let pos, name = canonicalize genv genv.classes x `cls in
    (* Don't let people use strictly internal classes
     * (except when they are being declared in .hhi files) *)
    if name = SN.Classes.cHH_BuiltinEnum &&
      not (str_ends_with (Relative_path.to_absolute (Pos.filename pos)) ".hhi")
    then Errors.using_internal_class pos (strip_ns name);
    pos, name

  let fun_id (genv, _) x =
    elaborate_and_get_name_with_canonicalized_fallback
      (* Not just Dep.Fun, but Dep.FunName. This forces an incremental full
       * redeclaration of this class if the name changes, not just a
       * retypecheck -- the name that is referred to here actually changes as
       * a result of what else is defined, which is stronger than just the need
       * to retypecheck. *)
      (fun x -> Typing_deps.Dep.FunName x)
      genv
      genv.funs
      x

  let new_const (genv, env) x =
    try ignore (new_var env.consts x); x with exn ->
      match genv.in_mode with
      | FileInfo.Mstrict -> raise exn
      | FileInfo.Mpartial | FileInfo.Mdecl -> x

  let resilient_new_canon_var env_and_names (p, name) =
    let env, canon_names = !env_and_names in
    let name_key = canon_key name in
    match SMap.get name_key canon_names with
      | Some canonical ->
        let p', id = SMap.find_unsafe canonical env in
        if Pos.compare p p' = 0 then (p, id)
        else begin
          Errors.error_name_already_bound name canonical p p';
          p', id
        end
      | None ->
        let pos_and_id = p, Ident.make name in
        env_and_names :=
          SMap.add name pos_and_id env, SMap.add name_key name canon_names;
        pos_and_id

  let resilient_new_var env (p, x) =
    if SMap.mem x !env
    then begin
      let p', y = SMap.find_unsafe x !env in
      if Pos.compare p p' = 0 then (p, y)
      else begin
        Errors.error_name_already_bound x x p p';
        p', y
      end
    end
    else
      let y = p, Ident.make x in
      env := SMap.add x y !env;
      y

  let new_fun_id genv x =
    if SMap.mem (snd x) !predef_funs then () else
    ignore (resilient_new_canon_var genv.funs x)

  let new_class_id genv x =
    ignore (resilient_new_canon_var genv.classes x)

  let new_typedef_id genv x =
    let v = resilient_new_canon_var genv.classes x in
    genv.typedefs := SMap.add (snd x) v !(genv.typedefs);
    ()

  let new_global_const_id genv x =
    let v = resilient_new_var genv.gconsts x in
    genv.gconsts := SMap.add (snd x) v !(genv.gconsts);
    ()

(* Scope, keep the locals, go and name the body, and leave the
 * local environment intact
 *)
  let scope env f =
    let genv, lenv = env in
    let lenv_copy = !(lenv.locals) in
    let lenv_pending_copy = !(lenv.pending_locals) in
    let res = f env in
    lenv.locals := lenv_copy;
    lenv.pending_locals := lenv_pending_copy;
    res

end

(*****************************************************************************)
(* Updating the environment *)
(*****************************************************************************)
let remove_decls env (funs, classes, typedefs, consts) =
  let funs = SSet.diff funs predef_tests in
  let canonicalize_set = (fun elt acc -> SSet.add (canon_key elt) acc) in
  let class_namekeys = SSet.fold canonicalize_set classes SSet.empty in
  let typedef_namekeys = SSet.fold canonicalize_set typedefs SSet.empty in
  let fun_namekeys = SSet.fold canonicalize_set funs SSet.empty in
  let iclassmap, iclassnames = env.iclasses in
  let iclassmap, iclassnames =
    SSet.fold SMap.remove classes iclassmap,
    SSet.fold SMap.remove class_namekeys iclassnames
  in
  let iclassmap, iclassnames =
    SSet.fold SMap.remove typedefs iclassmap,
    SSet.fold SMap.remove typedef_namekeys iclassnames
  in
  let ifunmap, ifunnames = env.ifuns in
  let ifunmap, ifunnames =
    SSet.fold SMap.remove funs ifunmap,
    SSet.fold SMap.remove fun_namekeys ifunnames
  in
  let itypedefs = SSet.fold SMap.remove typedefs env.itypedefs in
  let iconsts = SSet.fold SMap.remove consts env.iconsts in
  { env with
    ifuns     = ifunmap, ifunnames;
    iclasses  = iclassmap, iclassnames;
    itypedefs = itypedefs;
    iconsts   = iconsts;
  }

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Alok is constantly complaining that in partial mode,
 * he forgets to bind a type parameter, for example T,
 * and because partial assumes T is just a class that lives
 * in PHP land there is no error message.
 * So to help him, I am adding a rule that if
 * the class name starts with a T and is only 2 characters
 * it is considered a type variable. You will not be able to
 * define a class T in php land in this scheme ... But it is a bad
 * name for a class anyway.
*)
let is_alok_type_name (_, x) = String.length x <= 2 && x.[0] = 'T'

let check_constraint (_, (pos, name), _) =
  (* TODO refactor this in a separate module for errors *)
  if String.lowercase name = "this"
  then Errors.this_reserved pos
  else if name.[0] <> 'T' then Errors.start_with_T pos

let check_repetition s param =
  let x = snd param.param_id in
  if SSet.mem x s
  then Errors.already_bound (fst param.param_id) x;
  if x <> SN.SpecialIdents.placeholder then SSet.add x s else s

(* Check that a name is not a typedef *)
let no_typedef (genv, _) cid =
  let (pos, name) = Namespaces.elaborate_id genv.namespace cid in
  if SMap.mem name !(genv.typedefs)
  then
    let def_pos, _ = SMap.find_unsafe name !(genv.typedefs) in
    Errors.unexpected_typedef pos def_pos

let hint_no_typedef env = function
  | _, Happly (x, _) -> no_typedef env x
  | _ -> ()

let convert_shape_name env = function
  | SFlit (pos, s) -> (pos, N.SFlit (pos, s))
  | SFclass_const (x, (pos, y)) ->
    let class_name = Env.class_name env x in
    (pos, N.SFclass_const (class_name, (pos, y)))

let arg_unpack_unexpected = function
  | [] -> ()
  | (pos, _) :: _ -> Errors.naming_too_few_arguments pos; ()

(*****************************************************************************)
(* The entry point to build the naming environment *)
(*****************************************************************************)

let make_env old_env ~funs ~classes ~typedefs ~consts =
  let genv = Env.empty_global old_env in
  List.iter (Env.new_fun_id genv) funs;
  List.iter (Env.new_class_id genv) classes;
  List.iter (Env.new_typedef_id genv) typedefs;
  List.iter (Env.new_global_const_id genv) consts;
  let new_env = {
    itcopt = old_env.itcopt;
    iclasses = !(genv.classes);
    ifuns = !(genv.funs);
    itypedefs = !(genv.typedefs);
    iconsts = !(genv.gconsts);
  } in
  new_env

(*****************************************************************************)
(* Naming of type hints *)
(*****************************************************************************)

let rec hint
    ?(is_static_var=false)
    ?(forbid_this=false)
    ?(allow_retonly=false) env (p, h) =
  p, hint_ ~forbid_this ~allow_retonly is_static_var p env h

and hint_ ~forbid_this ~allow_retonly is_static_var p env x =
  let hint = hint ~is_static_var ~forbid_this in
  match x with
  | Htuple hl ->
    N.Htuple (List.map (hint ~allow_retonly env) hl)
  | Hoption h ->
    (* void/noreturn are permitted for Typing.option_return_only_typehint *)
    N.Hoption (hint ~allow_retonly env h)
  | Hfun (hl, opt, h) ->
    N.Hfun (List.map (hint env) hl, opt,
            hint ~allow_retonly:true env h)
  | Happly ((p, x) as id, hl) ->
    let hint_id = hint_id ~forbid_this ~allow_retonly env is_static_var id hl in
    (match hint_id with
    | N.Hprim _ | N.Hmixed ->
      if hl <> [] then Errors.unexpected_type_arguments p
    | _ -> ()
    );
    hint_id
  | Haccess ((pos, root_id) as root, id, ids) ->
    let root_ty =
      match root_id with
      | x when x = SN.Classes.cSelf ->
          (match (fst env).current_cls with
          | None ->
             Errors.self_outside_class pos;
             N.Hany
          | Some (cid, _) ->
             let tparaml = (fst env).type_paraml in
             let tparaml = List.map begin fun (param_pos, param_name) ->
               param_pos, N.Habstr (param_name, get_constraint env param_name)
             end tparaml in
             N.Happly (cid, tparaml)
          )
      | x when x = SN.Classes.cStatic || x = SN.Classes.cParent ->
          Errors.invalid_type_access_root root; N.Hany
        | _ ->
          (match hint_id ~forbid_this ~allow_retonly env is_static_var root [] with
          | N.Hthis | N.Happly _ as h -> h
          | _ -> Errors.invalid_type_access_root root; N.Hany
          )
    in
    N.Haccess ((pos, root_ty), id :: ids)
  | Hshape fdl -> N.Hshape
    begin
      List.fold_left begin fun fdm (pname, h) ->
        let pos, name = convert_shape_name env pname in
        if ShapeMap.mem name fdm
        then Errors.fd_name_already_bound pos;
        ShapeMap.add name (hint env h) fdm
      end ShapeMap.empty fdl
  end

and hint_id ~forbid_this ~allow_retonly env is_static_var (p, x as id) hl =
  Naming_hooks.dispatch_hint_hook id;
  let params = (fst env).type_params in
  if   is_alok_type_name id && not (SMap.mem x params)
  then Errors.typeparam_alok id;
  if   is_static_var && SMap.mem x params
  then Errors.generic_class_var (fst id);
  (* some common Xhp screw ups *)
  if   (x = "Xhp") || (x = ":Xhp") || (x = "XHP")
  then Errors.disallowed_xhp_type p x;
  match try_castable_hint ~forbid_this env p x hl with
  | Some h -> h
  | None -> begin
    match x with
      | x when x.[0] = '\\' &&
        ( x = ("\\"^SN.Typehints.void)
        || x = ("\\"^SN.Typehints.noreturn)
        || x = ("\\"^SN.Typehints.int)
        || x = ("\\"^SN.Typehints.bool)
        || x = ("\\"^SN.Typehints.float)
        || x = ("\\"^SN.Typehints.num)
        || x = ("\\"^SN.Typehints.string)
        || x = ("\\"^SN.Typehints.resource)
        || x = ("\\"^SN.Typehints.mixed)
        || x = ("\\"^SN.Typehints.array)
        || x = ("\\"^SN.Typehints.arraykey)
        || x = ("\\"^SN.Typehints.integer)
        || x = ("\\"^SN.Typehints.boolean)
        || x = ("\\"^SN.Typehints.double)
        || x = ("\\"^SN.Typehints.real)
        ) ->
        Errors.primitive_toplevel p;
        N.Hany
    | x when x = SN.Typehints.void && allow_retonly -> N.Hprim N.Tvoid
    | x when x = SN.Typehints.void ->
      Errors.return_only_typehint p `void;
      N.Hany
    | x when x = SN.Typehints.noreturn && allow_retonly -> N.Hprim N.Tnoreturn
    | x when x = SN.Typehints.noreturn ->
      Errors.return_only_typehint p `noreturn;
      N.Hany
    | x when x = SN.Typehints.num  -> N.Hprim N.Tnum
    | x when x = SN.Typehints.resource -> N.Hprim N.Tresource
    | x when x = SN.Typehints.arraykey -> N.Hprim N.Tarraykey
    | x when x = SN.Typehints.mixed -> N.Hmixed
    | x when x = SN.Typehints.this && not forbid_this ->
        if hl != []
        then Errors.this_no_argument p;
        (match (fst env).current_cls with
        | None ->
          Errors.this_hint_outside_class p;
          N.Hany
        | Some c ->
          N.Hthis
        )
    | x when x = SN.Typehints.this ->
        (match (fst env).current_cls with
        | None ->
            Errors.this_hint_outside_class p
        | Some _ ->
            Errors.this_type_forbidden p
        );
        N.Hany
    | x when x = SN.Classes.cClassname && (List.length hl) <> 1 ->
        Errors.classname_param p;
        N.Hprim N.Tstring
    | _ when String.lowercase x = SN.Typehints.this ->
        Errors.lowercase_this p x;
        N.Hany
    | _ when SMap.mem x params ->
        if hl <> [] then
        Errors.tparam_with_tparam p x;
        N.Habstr (x, get_constraint env x)
    | _ ->
       let name = Env.class_name env id in
        N.Happly (name, hintl ~forbid_this ~allow_retonly:true env hl)
  end

(* Hints that are valid both as casts and type annotations.  Neither
 * casts nor annotations are a strict subset of the other: For
 * instance, 'object' is not a valid annotation.  Thus callers will
 * have to handle the remaining cases. *)
and try_castable_hint ?(forbid_this=false) env p x hl =
  let hint = hint ~forbid_this ~allow_retonly:false in
  let canon = String.lowercase x in
  let opt_hint = match canon with
    | nm when nm = SN.Typehints.int    -> Some (N.Hprim N.Tint)
    | nm when nm = SN.Typehints.bool   -> Some (N.Hprim N.Tbool)
    | nm when nm = SN.Typehints.float  -> Some (N.Hprim N.Tfloat)
    | nm when nm = SN.Typehints.string -> Some (N.Hprim N.Tstring)
    | nm when nm = SN.Typehints.array  ->
      Some (match hl with
        | [] -> N.Harray (None, None)
        | [val_] -> N.Harray (Some (hint env val_), None)
        | [key_; val_] -> N.Harray (Some (hint env key_), Some (hint env val_))
        | _ -> Errors.too_many_type_arguments p; N.Hany
      )
    | nm when nm = SN.Typehints.integer ->
      Errors.primitive_invalid_alias p nm SN.Typehints.int;
      Some (N.Hprim N.Tint)
    | nm when nm = SN.Typehints.boolean ->
      Errors.primitive_invalid_alias p nm SN.Typehints.bool;
      Some (N.Hprim N.Tbool)
    | nm when nm = SN.Typehints.double || nm = SN.Typehints.real ->
      Errors.primitive_invalid_alias p nm SN.Typehints.float;
      Some (N.Hprim N.Tfloat)
    | _ -> None
  in
  let () = match opt_hint with
    | Some _ when canon <> x -> Errors.primitive_invalid_alias p x canon
    | _ -> ()
  in opt_hint

and get_constraint env tparam =
  let params = (fst env).type_params in
  let gen_constraint = SMap.find_unsafe tparam params in
  let genv, lenv = env in
  (* this prevents an infinite loop from occurring since hint invokes
   * get_constraint *)
  let genv = { genv with type_params = SMap.add tparam None params } in
  let env = genv, lenv in
  Option.map gen_constraint (constraint_ env)

and constraint_ ?(forbid_this=false) env (ck, h) = ck, hint ~forbid_this env h

and hintl ~forbid_this ~allow_retonly env l =
  List.map (hint ~forbid_this ~allow_retonly env) l

(*****************************************************************************)
(* All the methods and static methods of an interface are "implicitly"
 * declared as abstract
 *)
(*****************************************************************************)

let add_abstract m = {m with N.m_abstract = true}

let add_abstractl methods = List.map add_abstract methods

let interface c constructor methods smethods =
  if c.c_kind <> Cinterface then constructor, methods, smethods else
  let constructor = opt_map add_abstract constructor in
  let methods  = add_abstractl methods in
  let smethods = add_abstractl smethods in
  constructor, methods, smethods

(*****************************************************************************)
(* Checking for collision on method names *)
(*****************************************************************************)

let check_method acc { N.m_name = (p, x); _ } =
  if SSet.mem x acc
  then Errors.method_name_already_bound p x;
  SSet.add x acc

let check_name_collision methods =
  ignore (List.fold_left check_method SSet.empty methods)

(*****************************************************************************)
(* Checking for shadowing of method type parameters *)
(*****************************************************************************)

let check_method_tparams class_tparam_names { N.m_tparams = tparams; _ } =
  List.iter
    (fun (_, (p,x),_) -> List.iter
       (fun (pc,xc) -> if (x = xc) then Errors.shadowed_type_param p pc x)
       class_tparam_names)
    tparams

let check_tparams_shadow class_tparam_names methods =
  List.iter (check_method_tparams class_tparam_names) methods

(*****************************************************************************)
(* The entry point to CHECK the program, and transform the program *)
(*****************************************************************************)

let rec class_constraints tparams =
  let cstrs = make_constraints tparams in
  (* Checking there is no cycle in the type constraints *)
  List.iter (Naming_ast_helpers.HintCycle.check_constraint cstrs) tparams;
  cstrs

(* Naming of a class *)
and class_ nenv c =
  let constraints = class_constraints c.c_tparams in
  let env      = Env.make_class_env nenv constraints c in
  (* Checking for a code smell *)
  List.iter check_constraint c.c_tparams;
  List.iter (hint_no_typedef env) c.c_extends;
  List.iter (hint_no_typedef env) c.c_implements;
  let name     = Env.class_name env c.c_name in
  let smethods = List.fold_right (class_static_method env) c.c_body [] in
  let sprops    = List.fold_right (class_prop_static env) c.c_body [] in
  let props     = List.fold_right (class_prop env) c.c_body [] in
  let prop_names  = List.map (fun x -> snd x.N.cv_id) props in
  let prop_names  = List.fold_right SSet.add prop_names SSet.empty in
  let sm_names = List.map (fun x -> snd x.N.m_name) smethods in
  let sm_names = List.fold_right SSet.add sm_names SSet.empty in
  let parents  = List.map (hint ~allow_retonly:false env) c.c_extends in
  let parents  = match c.c_kind with
    (* Make enums implicitly extend the BuiltinEnum class in order to provide
     * utility methods. *)
    | Cenum ->
        let pos = fst name in
        let enum_type = pos, N.Happly (name, []) in
        let parent =
          pos, N.Happly ((pos, Naming_special_names.Classes.cHH_BuiltinEnum),
                         [enum_type]) in
        parent::parents
    | _ -> parents in
  let fmethod  = class_method env sm_names prop_names in
  let methods  = List.fold_right fmethod c.c_body [] in
  let uses     = List.fold_right (class_use env) c.c_body [] in
  let xhp_attr_uses = List.fold_right (xhp_attr_use env) c.c_body [] in
  let req_implements, req_extends = List.fold_right
    (class_require env c.c_kind) c.c_body ([], []) in
  (* Setting a class type parameters constraint to the 'this' type is weird
   * so lets forbid it for now.
   *)
  let tparam_l  = type_paraml ~forbid_this:true env c.c_tparams in
  let consts   = List.fold_right (class_const env) c.c_body [] in
  let typeconsts = List.fold_right (class_typeconst env) c.c_body [] in
  let implements = List.map (hint env) c.c_implements in
  let constructor = List.fold_left (constructor env) None c.c_body in
  let constructor, methods, smethods =
    interface c constructor methods smethods in
  let class_tparam_names = List.map (fun (_, x,_) -> x) c.c_tparams in
  let enum = opt_map (enum_ env) c.c_enum in
  check_name_collision methods;
  check_tparams_shadow class_tparam_names methods;
  check_name_collision smethods;
  check_tparams_shadow class_tparam_names smethods;
  let named_class =
    { N.c_mode           = c.c_mode;
      N.c_final          = c.c_final;
      N.c_is_xhp         = c.c_is_xhp;
      N.c_kind           = c.c_kind;
      N.c_name           = name;
      N.c_tparams        = (tparam_l, constraints);
      N.c_extends        = parents;
      N.c_uses           = uses;
      N.c_xhp_attr_uses  = xhp_attr_uses;
      N.c_req_extends    = req_extends;
      N.c_req_implements = req_implements;
      N.c_implements     = implements;
      N.c_consts         = consts;
      N.c_typeconsts     = typeconsts;
      N.c_static_vars    = sprops;
      N.c_vars           = props;
      N.c_constructor    = constructor;
      N.c_static_methods = smethods;
      N.c_methods        = methods;
      N.c_user_attributes = user_attributes env c.c_user_attributes;
      N.c_enum           = enum
    }
  in
  Naming_hooks.dispatch_class_named_hook named_class;
  named_class

and user_attributes env attrl =
  let seen = Hashtbl.create 0 in
  let tc_options = (fst env).tcopt in
  let validate_seen = begin fun ua_name ->
    let pos, name = ua_name in
    let existing_attr_pos =
      try Some (Hashtbl.find seen name)
      with Not_found -> None
    in (match existing_attr_pos with
      | Some p -> Errors.duplicate_user_attribute ua_name p; false
      | None -> Hashtbl.add seen name pos; true
    )
  end in
  let validate_name = begin fun ua_name ->
    (validate_seen ua_name) && begin
      let pos, name = ua_name in
      let valid = if str_starts_with name "__"
        then SSet.mem name SN.UserAttributes.as_set
        else (TypecheckerOptions.allowed_attribute tc_options name)
      in if not valid then Errors.unbound_attribute_name pos name;
      valid
    end
  end in
  List.fold_left begin fun acc {ua_name; ua_params} ->
    if not (validate_name ua_name) then acc
    else let attr = {
           N.ua_name = ua_name;
           N.ua_params = List.map (expr env) ua_params
         } in
         attr :: acc
  end [] attrl

and enum_ env e =
  { N.e_base       = hint env e.e_base;
    N.e_constraint = opt_map (hint env) e.e_constraint;
  }

and type_paraml ?(forbid_this = false) env tparams =
  let _, ret = List.fold_left
    (fun (seen, tparaml) ((_, (p, name), _) as tparam) ->
      match SMap.get name seen with
      | None -> (SMap.add name p seen, (type_param ~forbid_this env tparam)::tparaml)
      | Some pos ->
          Errors.shadowed_type_param p pos name;
          seen, tparaml
    )
    (SMap.empty, [])
    tparams in
  List.rev ret

and type_param ~forbid_this env (variance, param_name, cstr_opt) =
  variance,
  param_name,
  Option.map cstr_opt (constraint_ ~forbid_this env)

and class_use env x acc =
  match x with
  | Attributes _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassUse h ->
    hint_no_typedef env h;
    hint env h :: acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst _ -> acc

and xhp_attr_use env x acc =
  match x with
  | Attributes _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse h ->
    hint_no_typedef env h;
    hint env h :: acc
  | ClassTraitRequire _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_require env c_kind x acc =
  match x with
  | Attributes _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire (MustExtend, h)
      when c_kind <> Ast.Ctrait && c_kind <> Ast.Cinterface ->
    let () = Errors.invalid_req_extends (fst h) in
    acc
  | ClassTraitRequire (MustExtend, h) ->
    hint_no_typedef env h;
    let acc_impls, acc_exts = acc in
    (acc_impls, hint env h :: acc_exts)
  | ClassTraitRequire (MustImplement, h) when c_kind <> Ast.Ctrait ->
    let () = Errors.invalid_req_implements (fst h) in
    acc
  | ClassTraitRequire (MustImplement, h) ->
    hint_no_typedef env h;
    let acc_impls, acc_exts = acc in
    (hint env h :: acc_impls, acc_exts)
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst _ -> acc

and constructor env acc = function
  | Attributes _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method ({ m_name = (p, name); _ } as m) when name = SN.Members.__construct ->
      (match acc with
      | None -> Some (method_ (fst env) m)
      | Some _ -> Errors.method_name_already_bound p name; acc)
  | Method _ -> acc
  | TypeConst _ -> acc

and class_const env x acc =
  match x with
  | Attributes _ -> acc
  | Const (h, l) -> const_defl h env l @ acc
  | AbsConst (h, x) -> abs_const_def env h x :: acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_prop_static env x acc =
  match x with
  | Attributes _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassVars (kl, h, cvl) when List.mem Static kl ->
    (* Static variables are shared for all classes in the hierarchy.
     * This makes the 'this' type completely unsafe as a type for a
     * static variable. See test/typecheck/this_tparam_static.php as
     * an example of what can occur.
     *)
    let h = opt_map (hint ~forbid_this:true ~is_static_var:true env) h in
    let cvl = List.map (class_prop_ env) cvl in
    let cvl = List.map (fill_prop kl h) cvl in
    cvl @ acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_prop env x acc =
  match x with
  | Attributes _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassVars (kl, h, cvl) when not (List.mem Static kl) ->
    (* there are no covariance issues with private members *)
    let forbid_this = false (*not (List.mem Private kl)*) in
    let h = opt_map (hint ~forbid_this env) h in
    let cvl = List.map (class_prop_ env) cvl in
    let cvl = List.map (fill_prop kl h) cvl in
    cvl @ acc
  | ClassVars _ -> acc
  | XhpAttr (kl, h, cvl, is_required, maybe_enum) ->
    let default = (match cvl with
      | [(_, v)] -> v
      | _ -> None) in
    let h = (match maybe_enum with
      | Some (pos, items) ->
        let contains_int = List.exists begin function
          | _, Int _ -> true
          | _ -> false
        end items in
        let contains_str = List.exists begin function
          | _, String _ | _, String2 _ -> true
          | _ -> false
        end items in
        if contains_int && not contains_str then
          Some (pos, Happly ((pos, "int"), []))
        else if not contains_int && contains_str then
          Some (pos, Happly ((pos, "string"), []))
        else
          (* If the list was empty, or if there was a mix of
             ints and strings, then fallback to mixed *)
          Some (pos, Happly ((pos, "mixed"), []))
      | _ -> h) in
    let h = (match h with
      | Some (p, ((Hoption _) as x)) -> Some (p, x)
      | Some (p, ((Happly ((_, "mixed"), [])) as x)) -> Some (p, x)
      | Some (p, h) ->
        (* If a non-nullable attribute is not marked as "@required"
           AND it does not have a non-null default value, make the
           typehint nullable for now *)
        if (is_required ||
            (match default with
              | None ->            false
              | Some (_, Null) ->  false
              | Some _ ->          true))
          then Some (p, h)
          else Some (p, Hoption (p, h))
      | None -> None) in
    let h = opt_map (hint env) h in
    let cvl = List.map (class_prop_ env) cvl in
    let cvl = List.map (fill_prop kl h) cvl in
    cvl @ acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_static_method env x acc =
  match x with
  | Attributes _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method m when snd m.m_name = SN.Members.__construct -> acc
  | Method m when List.mem Static m.m_kind -> method_ (fst env) m :: acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_method env sids cv_ids x acc =
  match x with
  | Attributes _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method m when snd m.m_name = SN.Members.__construct -> acc
  | Method m when not (List.mem Static m.m_kind) ->
      let genv = fst env in
      method_ { genv with in_instance_method = true } m :: acc
  | Method _ -> acc
  | TypeConst _ -> acc

and class_typeconst env x acc =
  match x with
  | Attributes _ -> acc
  | Const _ -> acc
  | AbsConst _ -> acc
  | ClassUse _ -> acc
  | XhpAttrUse _ -> acc
  | ClassTraitRequire _ -> acc
  | ClassVars _ -> acc
  | XhpAttr _ -> acc
  | Method _ -> acc
  | TypeConst t -> typeconst env t :: acc

and check_constant_expr (pos, e) =
  match e with
  | Unsafeexpr _ | Id _ | Null | True | False | Int _
  | Float _ | String _
  | String2 ([], _) -> ()
  | Class_const ((_, cls), _) when cls <> "static" -> ()

  | Unop ((Uplus | Uminus | Utild | Unot), e) -> check_constant_expr e
  | Binop (op, e1, e2) ->
    (* Only assignment is invalid *)
    (match op with
      | Eq _ -> Errors.illegal_constant pos
      | _ ->
        check_constant_expr e1;
        check_constant_expr e2)
  | Eif (e1, e2, e3) ->
    check_constant_expr e1;
    ignore (opt_map check_constant_expr e2);
    check_constant_expr e3

  | String2 ((var_pos, _) :: _, _) ->
      Errors.local_const var_pos
  | _ -> Errors.illegal_constant pos

and const_defl h env l = List.map (const_def h env) l
and const_def h env (x, e) =
  check_constant_expr e;
  let new_const = Env.new_const env x in
  let h = opt_map (hint env) h in
  h, new_const, Some (expr env e)

and abs_const_def env h x =
  let new_const = Env.new_const env x in
  let h = opt_map (hint env) h in
  h, new_const, None

and class_prop_ env (x, e) =
  let id = Env.new_const env x in
  let e = Option.map e (expr env) in
  (* If the user has not provided a value, we initialize the member variable
   * ourselves to a value of type Tany. Classes might inherit from our decl
   * mode class that are themselves not in decl, and there's no way to figure
   * out what variables are initialized in a decl class without typechecking
   * its initalizers and constructor, which we don't want to do, so just assume
   * we're covered. *)
  let e =
    if (fst env).in_mode = FileInfo.Mdecl && e = None
    then Some (fst id, N.Any)
    else e
  in
  N.({ cv_final = false;
       cv_is_xhp = ((String.sub (snd x) 0 1) = ":");
       cv_visibility = Public;
       cv_type = None;
       cv_id = id;
       cv_expr = e;
     })

and fill_prop kl ty x =
  let x = { x with N.cv_type = ty } in
  List.fold_left (
  fun x k ->
    (* There is no field Static, they are dissociated earlier.
       An abstract class variable doesn't make sense.
     *)
    match k with
    | Final     -> { x with N.cv_final = true }
    | Static    -> x
    | Abstract  -> x
    | Private   -> { x with N.cv_visibility = N.Private }
    | Public    -> { x with N.cv_visibility = N.Public }
    | Protected -> { x with N.cv_visibility = N.Protected }
 ) x kl

and typeconst env t =
  (* We use the same namespace as constants within the class so we cannot have
   * a const and type const with the same name
   *)
  let name = Env.new_const env t.tconst_name in
  let constr = opt_map (hint env) t.tconst_constraint in
  let hint_ =
    match t.tconst_type with
    | None when not t.tconst_abstract ->
        Errors.not_abstract_without_typeconst name;
        t.tconst_constraint
    | Some h when t.tconst_abstract ->
        Errors.abstract_with_typeconst name;
        None
    | h -> h
  in
  let type_ = opt_map (hint env) hint_ in
  N.({ c_tconst_name = name;
       c_tconst_constraint = constr;
       c_tconst_type = type_;
     })

and func_body_had_unsafe env = !((snd env).has_unsafe)

and method_ genv m =
  let genv = extend_params genv m.m_tparams in
  let env = genv, Env.empty_local() in
  (* Cannot use 'this' if it is a public instance method *)
  let variadicity, paraml = fun_paraml env m.m_params in
  let name = Env.new_const env m.m_name in
  let acc = false, false, N.Public in
  let final, abs, vis = List.fold_left kind acc m.m_kind in
  List.iter check_constraint m.m_tparams;
  let tparam_l = type_paraml env m.m_tparams in
  let ret = opt_map (hint ~allow_retonly:true env) m.m_ret in
  let f_kind = m.m_fun_kind in
  let body = (match genv.in_mode with
    | FileInfo.Mdecl ->
      N.NamedBody {
        N.fnb_nast = [];
        fnb_unsafe = true;
      }
    | FileInfo.Mstrict | FileInfo.Mpartial ->
      N.UnnamedBody {
        N.fub_ast = m.m_body;
        fub_tparams = m.m_tparams;
        fub_namespace = genv.namespace;
      }
  ) in
  let attrs = user_attributes env m.m_user_attributes in
  N.({ m_final           = final       ;
       m_visibility      = vis         ;
       m_abstract        = abs         ;
       m_name            = name        ;
       m_tparams         = tparam_l    ;
       m_params          = paraml      ;
       m_body            = body        ;
       m_fun_kind        = f_kind      ;
       m_ret             = ret         ;
       m_variadic        = variadicity ;
       m_user_attributes = attrs;
     })

and kind (final, abs, vis) = function
  | Final -> true, abs, vis
  | Static -> final, abs, vis
  | Abstract -> final, true, vis
  | Private -> final, abs, N.Private
  | Public -> final, abs, N.Public
  | Protected -> final, abs, N.Protected

and fun_paraml env l =
  let _names = List.fold_left check_repetition SSet.empty l in
  let variadicity, l = determine_variadicity env l in
  variadicity, List.map (fun_param env) l

and determine_variadicity env l =
  match l with
    | [] -> N.FVnonVariadic, []
    | [x] -> (
      match x.param_is_variadic, x.param_id with
        | false, _ -> N.FVnonVariadic, [x]
        (* NOTE: variadic params are removed from the list *)
        | true, (_, "...") -> N.FVellipsis, []
        | true, _ -> N.FVvariadicArg (fun_param env x), []
    )
    | x :: rl ->
      let variadicity, rl = determine_variadicity env rl in
      variadicity, x :: rl

and fun_param env param =
  let x = Env.new_lvar env param.param_id in
  let eopt = opt_map (expr env) param.param_expr in
  let ty = opt_map (hint env) param.param_hint in
  { N.param_hint = ty;
    param_is_reference = param.param_is_reference;
    param_is_variadic = param.param_is_variadic;
    param_id = x;
    param_name = snd param.param_id;
    param_expr = eopt;
  }

and make_constraints paraml =
  List.fold_right begin fun (_, (_, x), cstr_opt) acc ->
    SMap.add x cstr_opt acc
  end paraml SMap.empty

and extend_params genv paraml =
  let params = List.fold_right begin fun (_, (_, x), cstr_opt) acc ->
    SMap.add x cstr_opt acc
  end paraml genv.type_params in
  { genv with type_params = params }

and typechecker_options env : TypecheckerOptions.t = env.itcopt

and uselist_lambda f =
  (* semantic duplication: This is copied from the implementation of the
    `Lfun` variant of `expr_` defined earlier in this file. *)
  let to_capture = ref [] in
  let handle_unbound (p, x) =
    to_capture := x :: !to_capture;
    p, Ident.tmp()
  in
  let tcopt = TypecheckerOptions.permissive in
  let genv = Env.make_fun_decl_genv (empty tcopt) SMap.empty f in
  let lenv = Env.empty_local () in
  let lenv = { lenv with unbound_mode = UBMFunc handle_unbound } in
  let env = genv, lenv in
  ignore (expr_lambda env f);
  uniq !to_capture

and fun_ nenv f =
  let tparams = make_constraints f.f_tparams in
  let genv = Env.make_fun_decl_genv nenv tparams f in
  let lenv = Env.empty_local () in
  let env = genv, lenv in
  let h = opt_map (hint ~allow_retonly:true env) f.f_ret in
  let variadicity, paraml = fun_paraml env f.f_params in
  let x = Env.fun_id env f.f_name in
  List.iter check_constraint f.f_tparams;
  let f_tparams = type_paraml env f.f_tparams in
  let f_kind = f.f_fun_kind in
  let body = match genv.in_mode with
    | FileInfo.Mdecl ->
      N.NamedBody {
        N.fnb_nast = [];
        fnb_unsafe = true;
      }
    | FileInfo.Mstrict | FileInfo.Mpartial ->
      N.UnnamedBody {
        N.fub_ast = f.f_body;
        fub_tparams = f.f_tparams;
        fub_namespace = f.f_namespace;
      }
  in
  let named_fun = {
    N.f_mode = f.f_mode;
    f_ret = h;
    f_name = x;
    f_tparams = f_tparams;
    f_params = paraml;
    f_body = body;
    f_fun_kind = f_kind;
    f_variadic = variadicity;
    f_user_attributes = user_attributes env f.f_user_attributes;
  } in
  Naming_hooks.dispatch_fun_named_hook named_fun;
  named_fun

and cut_and_flatten ?(replacement=Noop) env = function
  | [] -> []
  | Unsafe :: _ -> (snd env).has_unsafe := true ; [replacement]
  | Block b :: rest ->
      (cut_and_flatten ~replacement env b) @
        (cut_and_flatten ~replacement env rest)
  | x :: rest -> x :: (cut_and_flatten ~replacement env rest)

and stmt env st =
  match st with
  | Block _              -> assert false
  | Unsafe               -> assert false
  | Fallthrough          -> N.Fallthrough
  | Noop                 -> N.Noop
  | Break p              -> N.Break p
  | Continue p           -> N.Continue p
  | Throw e              -> let terminal = not (fst env).in_try in
                            N.Throw (terminal, expr env e)
  | Return (p, e)        -> N.Return (p, oexpr env e)
  | Static_var el        -> N.Static_var (static_varl env el)
  | If (e, b1, b2)       -> if_stmt env st e b1 b2
  | Do (b, e)            -> do_stmt env b e
  | While (e, b)         -> while_stmt env e b
  | For (st1, e, st2, b) -> for_stmt env st1 e st2 b
  | Switch (e, cl)       -> switch_stmt env st e cl
  | Foreach (e, aw, ae, b)-> foreach_stmt env e aw ae b
  | Try (b, cl, fb)      -> try_stmt env st b cl fb
  | Expr (cp, Call ((p, Id (fp, fn)), el, uel))
      when fn = SN.SpecialFunctions.invariant ->
    (* invariant is subject to a source-code transform in the HHVM
     * runtime: the arguments to invariant are lazily evaluated only in
     * the case in which the invariant condition does not hold. So:
     *
     *   invariant_violation(<condition>, <format>, <format_args...>)
     *
     * ... is rewritten as:
     *
     *   if (!<condition>) { invariant_violation(<format>, <format_args...>); }
     *)
    (match el with
      | [] | [_]  ->
        Errors.naming_too_few_arguments p;
        N.Expr (cp, N.Any)
      | (cond_p, cond) :: el ->
        let violation = (cp, Call
          ((p, Id (fp, "\\"^SN.SpecialFunctions.invariant_violation)), el, uel)) in
        if cond <> False then
          let b1, b2 = [Expr violation], [Noop] in
          let cond = cond_p, Unop (Unot, (cond_p, cond)) in
          if_stmt env st cond b1 b2
        else (* a false <condition> means unconditional invariant_violation *)
          N.Expr (expr env violation)
    )
  | Expr e               -> N.Expr (expr env e)

and if_stmt env st e b1 b2 =
  let e = expr env e in
  let nsenv = (fst env).namespace in
  let _, vars = Naming_ast_helpers.GetLocals.stmt (nsenv, SMap.empty) st in
  SMap.iter (fun x p -> Env.new_pending_lvar env (p, x)) vars;
  let result = Env.scope env (
  fun env ->
    let _, lenv = env in
    let all_locals_copy = !(lenv.all_locals) in
    let all1, b1 = branch env b1 in
    let all2, b2 = branch env b2 in
    let all_locals = SMap.union all1 all2 in
    lenv.all_locals := SMap.union all_locals all_locals_copy;
    N.If (e, b1, b2)
 ) in
 Env.promote_pending env;
 result

and do_stmt env b e =
  let new_scope = false in
  let b = block ~new_scope env b in
  N.Do (b, expr env e)

and while_stmt env e b =
  let e = expr env e in
  N.While (e, block env b)

and for_stmt env e1 e2 e3 b =
  let e1 = expr env e1 in
  let e2 = expr env e2 in
  let e3 = expr env e3 in
  Env.scope env (
  fun env ->
    N.For (e1, e2, e3, block env b)
 )

and switch_stmt env st e cl =
  let e = expr env e in
  let nsenv = (fst env).namespace in
  let _, vars = Naming_ast_helpers.GetLocals.stmt (nsenv, SMap.empty) st in
  SMap.iter (fun x p -> Env.new_pending_lvar env (p, x)) vars;
  let result = Env.scope env (
  fun env ->
    let _, lenv = env in
    let all_locals_copy = !(lenv.all_locals) in
    let all_locals, cl = casel env cl in
    lenv.all_locals := SMap.union all_locals all_locals_copy;
    N.Switch (e, cl)
 ) in
 Env.promote_pending env;
 result

and foreach_stmt env e aw ae b =
  let e = expr env e in
  Env.scope env (
  fun env ->
    let _, lenv = env in
    let all_locals_copy = !(lenv.all_locals) in
    let ae = as_expr env aw ae in
    let all_locals, b = branch env b in
    lenv.all_locals := SMap.union all_locals all_locals_copy;
    N.Foreach (e, ae, b)
 )

and as_expr env aw = function
  | As_v ev ->
    let nsenv = (fst env).namespace in
    let _, vars = Naming_ast_helpers.GetLocals.lvalue (nsenv, SMap.empty) ev in
    SMap.iter (fun x p -> ignore (Env.new_lvar env (p, x))) vars;
    let ev = expr env ev in
    (match aw with
      | None -> N.As_v ev
      | Some p -> N.Await_as_v (p, ev))
  | As_kv ((p1, Lvar k), ev) ->
    let k = p1, N.Lvar (Env.new_lvar env k) in
    let nsenv = (fst env).namespace in
    let _, vars = Naming_ast_helpers.GetLocals.lvalue (nsenv, SMap.empty) ev in
    SMap.iter (fun x p -> ignore (Env.new_lvar env (p, x))) vars;
    let ev = expr env ev in
    (match aw with
      | None -> N.As_kv (k, ev)
      | Some p -> N.Await_as_kv (p, k, ev))
  | As_kv ((p, _), _) ->
      Errors.expected_variable p;
      let x1 = p, N.Lvar (Env.new_lvar env (p, "__internal_placeholder")) in
      let x2 = p, N.Lvar (Env.new_lvar env (p, "__internal_placeholder")) in
      (match aw with
        | None -> N.As_kv (x1, x2)
        | Some p -> N.Await_as_kv (p, x1, x2))

and try_stmt env st b cl fb =
  let nsenv = (fst env).namespace in
  let _, vars = Naming_ast_helpers.GetLocals.stmt (nsenv, SMap.empty) st in
  SMap.iter (fun x p -> Env.new_pending_lvar env (p, x)) vars;
  let result = Env.scope env (
  fun env ->
    let genv, lenv = env in
    let all_locals_copy = !(lenv.all_locals) in
    (* isolate finally from the rest of the try-catch: if the first
     * statement of the try is an uncaught exception, finally will
     * still be executed *)
    let all_finally, fb = branch (genv, lenv) fb in
    lenv.all_locals := all_locals_copy;
    let all_locals_copy = !(lenv.all_locals) in
    let all1, b = branch ({ genv with in_try = true}, lenv) b in
    let all_locals, cl = catchl env cl in
    let all_locals = SMap.union all1 all_locals in
    lenv.all_locals := SMap.union all_locals all_locals_copy;
    N.Try (b, cl, fb)
  ) in
  Env.promote_pending env;
  result

and block ?(new_scope=true) env stl =
  let stl = cut_and_flatten env stl in
  if new_scope
  then
    Env.scope env (
      fun env -> List.map (stmt env) stl
    )
  else List.map (stmt env) stl

and branch env stmt_l =
  let stmt_l = cut_and_flatten env stmt_l in
  let genv, lenv = env in
  let lenv_copy = !(lenv.locals) in
  let lenv_all_locals_copy = !(lenv.all_locals) in
  let lenv_pending_copy = !(lenv.pending_locals) in
  let res = List.map (stmt env) stmt_l in
  lenv.locals := lenv_copy;
  let lenv_all_locals = !(lenv.all_locals) in
  lenv.all_locals := lenv_all_locals_copy;
  lenv.pending_locals := lenv_pending_copy;
  lenv_all_locals, res

and static_varl env l = List.map (static_var env) l
and static_var env = function
  | p, Lvar _ as lv -> expr env (p, Binop(Eq None, lv, (p, Null)))
  | e -> expr env e

and expr_obj_get_name env = function
  | p, Id x -> p, N.Id x
  | p, e ->
      (match (fst env).in_mode with
        | FileInfo.Mstrict ->
            Errors.dynamic_method_call p
        | FileInfo.Mpartial | FileInfo.Mdecl ->
            ()
      );
      expr env (p, e)

and exprl env l = List.map (expr env) l
and oexpr env e = opt_map (expr env) e
and expr env (p, e) = p, expr_ env e
and expr_ env = function
  | Array l -> N.Array (rev_rev_map (afield env) l)
  | Collection (id, l) -> begin
    let p, cn = Namespaces.elaborate_id ((fst env).namespace) id in
    match cn with
      | x when
          x = SN.Collections.cVector
          || x = SN.Collections.cImmVector
          || x = SN.Collections.cSet
          || x = SN.Collections.cImmSet ->
        N.ValCollection (cn, (List.map (afield_value env cn) l))
      | x when
          x = SN.Collections.cMap
          || x = SN.Collections.cImmMap
          || x = SN.Collections.cStableMap ->
        N.KeyValCollection (cn, (List.map (afield_kvalue env cn) l))
      | x when x = SN.Collections.cPair ->
        (match l with
          | [] ->
              Errors.naming_too_few_arguments p;
              N.Any
          | e1::e2::[] ->
            let pn = SN.Collections.cPair in
            N.Pair (afield_value env pn e1, afield_value env pn e2)
          | _ ->
              Errors.naming_too_many_arguments p;
              N.Any
        )
      | _ ->
          Errors.expected_collection p cn;
          N.Any
  end
  | Clone e -> N.Clone (expr env e)
  | Null -> N.Null
  | True -> N.True
  | False -> N.False
  | Int s -> N.Int s
  | Float s -> N.Float s
  | String s -> N.String s
  | String2 (idl, (_, s)) -> N.String2 (string2 env (List.rev idl), s)
  | Id x ->
    (match snd x with
      | const when const = SN.PseudoConsts.g__LINE__ -> N.Int x
      | const when const = SN.PseudoConsts.g__CLASS__ ->
        (match (fst env).current_cls with
          | None -> Errors.illegal_CLASS (fst x); N.Any
          | Some (cid, _) ->
            (* this isn't quite correct when inside a trait, as
             * __CLASS__ is replaced by the using class, but it's
             * sufficient for typechecking purposes (we require
             * subclass to be compatible with the trait member/method
             * declarations) *)
            N.String cid)
      | const when const = SN.PseudoConsts.g__TRAIT__ ->
        (match (fst env).current_cls with
          | Some (cid, Ctrait) -> N.String cid
          | _ -> Errors.illegal_TRAIT (fst x); N.Any)
      | const when
          const = SN.PseudoConsts.g__FILE__
          || const = SN.PseudoConsts.g__DIR__
          (* could actually check that we are in a function, method, etc *)
          || const = SN.PseudoConsts.g__FUNCTION__
          || const = SN.PseudoConsts.g__METHOD__
          || const = SN.PseudoConsts.g__NAMESPACE__ ->
        N.String x
      | _ -> N.Id (Env.global_const env x)
      )
  | Lvar (_, x) when x = SN.SpecialIdents.this -> N.This
  | Lvar ((_pos, x) as sid) when x = SN.SpecialIdents.placeholder ->
    N.Lplaceholder sid
  | Lvar x ->
      N.Lvar (Env.lvar env x)
  | Obj_get (e1, (p, _ as e2), nullsafe) ->
      (* If we encounter Obj_get(_,_,true) by itself, then it means "?->"
         is being used for instance property access; see the case below for
         handling nullsafe instance method calls to see how this works *)
      let nullsafe = match nullsafe with
        | OG_nullsafe -> N.OG_nullsafe
        | OG_nullthrows -> N.OG_nullthrows
      in
      N.Obj_get (expr env e1, expr_obj_get_name env e2, nullsafe)
  | Array_get ((p, Lvar x), None) ->
      let id = p, N.Lvar (Env.lvar env x) in
      N.Array_get (id, None)
  | Array_get (e1, e2) -> N.Array_get (expr env e1, oexpr env e2)
  | Class_get (x1, x2) ->
      N.Class_get (make_class_id env x1, x2)
  | Class_const (x1, x2) ->
      N.Class_const (make_class_id env x1, x2)
  | Call ((_, Id (p, pseudo_func)), el, uel)
      when pseudo_func = SN.SpecialFunctions.echo ->
      arg_unpack_unexpected uel ;
      N.Call (N.Cnormal, (p, N.Id (p, pseudo_func)), exprl env el, [])
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.call_user_func ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | f :: el -> N.Call (N.Cuser_func, expr env f, exprl env el, [])
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.fun_ ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | [_, String (p2, s)] when String.contains s ':' ->
        Errors.illegal_meth_fun p; N.Any
      | [_, String x] -> N.Fun_id (Env.fun_id env x)
      | [p, _] ->
          Errors.illegal_fun p;
          N.Any
      | _ -> Errors.naming_too_many_arguments p; N.Any
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.inst_meth ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | [_] -> Errors.naming_too_few_arguments p; N.Any
      | instance::(_, String meth)::[] ->
        N.Method_id (expr env instance, meth)
      | (p, _)::(_)::[] ->
        Errors.illegal_inst_meth p;
        N.Any
      | _ -> Errors.naming_too_many_arguments p; N.Any
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.meth_caller ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | [_] -> Errors.naming_too_few_arguments p; N.Any
      | e1::e2::[] ->
          (match (expr env e1), (expr env e2) with
          | (_, N.String cl), (_, N.String meth) ->
            N.Method_caller (Env.class_name env cl, meth)
          | (_, N.Class_const (N.CI cl, (_, mem))), (_, N.String meth)
            when mem = SN.Members.mClass ->
            N.Method_caller (Env.class_name env cl, meth)
          | (p, _), (_) ->
            Errors.illegal_meth_caller p;
            N.Any
          )
      | _ -> Errors.naming_too_many_arguments p; N.Any
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.class_meth ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | [_] -> Errors.naming_too_few_arguments p; N.Any
      | e1::e2::[] ->
          (match (expr env e1), (expr env e2) with
          | (_, N.String cl), (_, N.String meth) ->
            N.Smethod_id (Env.class_name env cl, meth)
          | (_, N.Class_const (N.CI cl, (_, mem))), (_, N.String meth)
            when mem = SN.Members.mClass ->
            N.Smethod_id (Env.class_name env cl, meth)
          | (p, N.Class_const ((N.CIself|N.CIstatic), (_, mem))),
              (_, N.String meth) when mem = SN.Members.mClass ->
            (match (fst env).current_cls with
              | Some (cid, _) -> N.Smethod_id (cid, meth)
              | None -> Errors.illegal_class_meth p; N.Any)
          | (p, _), (_) -> Errors.illegal_class_meth p; N.Any
          )
      | _ -> Errors.naming_too_many_arguments p; N.Any
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.assert_ ->
      arg_unpack_unexpected uel ;
      if List.length el <> 1
      then Errors.assert_arity p;
      N.Assert (N.AE_assert (expr env (List.hd el)))
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.SpecialFunctions.tuple ->
      arg_unpack_unexpected uel ;
      (match el with
      | [] -> Errors.naming_too_few_arguments p; N.Any
      | el -> N.List (exprl env el)
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.FB.fgena ->
      arg_unpack_unexpected uel ;
      (match el with
      | [e] -> N.Special_func (N.Gena (expr env e))
      | _ -> Errors.gena_arity p; N.Any
      )
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.FB.fgenva ->
      arg_unpack_unexpected uel ;
      if List.length el < 1
      then (Errors.genva_arity p; N.Any)
      else N.Special_func (N.Genva (exprl env el))
  | Call ((p, Id (_, cn)), el, uel) when cn = SN.FB.fgen_array_rec ->
      arg_unpack_unexpected uel ;
      (match el with
      | [e] -> N.Special_func (N.Gen_array_rec (expr env e))
      | _ -> Errors.gen_array_rec_arity p; N.Any
      )
  | Call ((p, Id f), el, uel) ->
      N.Call (N.Cnormal, (p, N.Id (Env.fun_id env f)),
              exprl env el, exprl env uel)
  (* Handle nullsafe instance method calls here. Because Obj_get is used
     for both instance property access and instance method calls, we need
     to match the entire "Call(Obj_get(..), ..)" pattern here so that we
     only match instance method calls *)
  | Call ((p, Obj_get (e1, e2, OG_nullsafe)), el, uel) ->
      N.Call
        (N.Cnormal,
         (p, N.Obj_get (expr env e1, expr_obj_get_name env e2, N.OG_nullsafe)),
         exprl env el, exprl env uel)
  (* Handle all kinds of calls that weren't handled by any of
     the cases above *)
  | Call (e, el, uel) ->
      N.Call (N.Cnormal, expr env e, exprl env el, exprl env uel)
  | Yield_break -> N.Yield_break
  | Yield e -> N.Yield (afield env e)
  | Await e -> N.Await (expr env e)
  | List el -> N.List (exprl env el)
  | Expr_list el -> N.Expr_list (exprl env el)
  | Cast (ty, e2) ->
      hint_no_typedef env ty;
      let (p, x), hl = match ty with
      | _, Happly (id, hl) -> (id, hl)
      | _                  -> assert false in
      let ty = match try_castable_hint env p x hl with
      | Some ty -> p, ty
      | None    -> begin
      match x with
      | x when x = SN.Typehints.object_cast ->
          (* (object) is a valid cast but not a valid type annotation *)
          (* FIXME we are not modeling the correct runtime behavior here -- the
           * runtime result type is an stdClass if the original type is
           * primitive. But we should probably just disallow object casts
           * altogether. *)
          p, N.Hany
      | x when x = SN.Typehints.void ->
          Errors.void_cast p;
          p, N.Hany
      | x when x = SN.Typehints.unset_cast ->
          Errors.unset_cast p;
          p, N.Hany
      | _       ->
          (* Let's just assume that any other invalid cases are attempts to
           * cast to specific objects *)
          Errors.object_cast p x;
          hint env ty
      end in
      N.Cast (ty, expr env e2)
  | Unop (uop, e) -> N.Unop (uop, expr env e)
  | Binop (Eq None as op, lv, e2) ->
      let e2 = expr env e2 in
      let nsenv = (fst env).namespace in
      let _, vars = Naming_ast_helpers.GetLocals.lvalue (nsenv, SMap.empty) lv in
      SMap.iter (fun x p -> ignore (Env.new_lvar env (p, x))) vars;
      N.Binop (op, expr env lv, e2)
  | Binop (bop, e1, e2) ->
      let e1 = expr env e1 in
      N.Binop (bop, e1, expr env e2)
  | Eif (e1, e2opt, e3) ->
      (* The order matters here, of course -- e1 can define vars that need to
       * be available in e2 and e3. *)
      let e1 = expr env e1 in
      let e2opt = oexpr env e2opt in
      let e3 = expr env e3 in
      N.Eif (e1, e2opt, e3)
  | InstanceOf (e, (p, Id x)) ->
    let id = match x with
      | px, n when n = SN.Classes.cParent ->
        if (fst env).current_cls = None then
          let () = Errors.parent_outside_class p in
          (px, SN.Classes.cUnknown)
        else (px, n)
      | px, n when n = SN.Classes.cSelf ->
        if (fst env).current_cls = None then
          let () = Errors.self_outside_class p in
          (px, SN.Classes.cUnknown)
        else (px, n)
      | px, n when n = SN.Classes.cStatic ->
        if (fst env).current_cls = None then
          let () = Errors.static_outside_class p in
          (px, SN.Classes.cUnknown)
        else (px, n)
      | _ ->
        no_typedef env x;
        (Env.class_name env x) in
    N.InstanceOf (expr env e, (p, N.Id id))
  | InstanceOf (e1, e2) ->
      N.InstanceOf (expr env e1, expr env e2)
  | New ((_, Id x), el, uel)
  | New ((_, Lvar x), el, uel) ->
    N.New (make_class_id env x, exprl env el, exprl env uel)
  | New ((p, e_), el, uel) ->
    if (fst env).in_mode = FileInfo.Mstrict
    then Errors.dynamic_new_in_strict_mode p;
    N.New (make_class_id env (p, SN.Classes.cUnknown),
           exprl env el, exprl env uel)
  | Efun (f, idl) ->
      let idl = List.map fst idl in
      let idl = List.filter
        (function (_, x) -> (x <> SN.SpecialIdents.this)) idl in
      let idl' = List.map (Env.lvar env) idl in
      let env = (fst env, Env.empty_local ()) in
      List.iter2 (Env.add_lvar env) idl idl';
      let f = expr_lambda env f in
      N.Efun (f, idl')
  | Lfun f ->
      (* We have to build the capture list while we're finding names in
         the closure body---accumulate it in to_capture. *)
      (* semantic duplication: The logic here is also used in `uselist_lambda`.
          The differences are enough that it does not make sense to refactor
          this out for now. *)
      let to_capture = ref [] in
      let handle_unbound (p, x) =
        let cap = Env.lvar env (p, x) in
        to_capture := cap :: !to_capture;
        cap
      in
      let lenv = Env.empty_local () in
      let lenv = { lenv with unbound_mode = UBMFunc handle_unbound } in
      let env = (fst env, lenv) in
      let f = expr_lambda env f in
      N.Efun (f, !to_capture)
  | Xml (x, al, el) -> N.Xml (Env.class_name env x, attrl env al, exprl env el)
  | Shape fdl ->
      N.Shape begin List.fold_left begin fun fdm (pname, value) ->
        let pos, name = convert_shape_name env pname in
        if ShapeMap.mem name fdm
        then Errors.fd_name_already_bound pos;
        ShapeMap.add name (expr env value) fdm
      end ShapeMap.empty fdl
      end
  | Unsafeexpr _ ->
      N.Any
  | Import _ ->
      N.Any
  | Ref (p, e_) -> expr_ env e_

and expr_lambda env f =
  let h = opt_map (hint ~allow_retonly:true env) f.f_ret in
  let previous_unsafe = !((snd env).has_unsafe) in
  (* save unsafe and yield state *)
  (snd env).has_unsafe := false;
  let variadicity, paraml = fun_paraml env f.f_params in
  let f_kind = f.f_fun_kind in
  (* The bodies of lambdas go through naming in the containing local
   * environment *)
  let body_nast = block env f.f_body in
  let unsafe = func_body_had_unsafe env in
  (* restore unsafe state *)
  (snd env).has_unsafe := previous_unsafe;
  let body = N.NamedBody {
    N.fnb_unsafe = unsafe;
    fnb_nast = body_nast;
  } in {
    N.f_mode = (fst env).in_mode;
    f_ret = h;
    f_name = f.f_name;
    f_params = paraml;
    f_tparams = [];
    f_body = body;
    f_fun_kind = f_kind;
    f_variadic = variadicity;
    f_user_attributes = user_attributes env f.f_user_attributes;
  }

and make_class_id env (p, x as cid) =
  no_typedef env cid;
  match x with
    | x when x = SN.Classes.cParent ->
      if (fst env).current_cls = None then
        let () = Errors.parent_outside_class p in
        N.CI (p, SN.Classes.cUnknown)
      else N.CIparent
    | x when x = SN.Classes.cSelf ->
      if (fst env).current_cls = None then
        let () = Errors.self_outside_class p in
        N.CI (p, SN.Classes.cUnknown)
      else N.CIself
    | x when x = SN.Classes.cStatic -> if (fst env).current_cls = None then
        let () = Errors.static_outside_class p in
        N.CI (p, SN.Classes.cUnknown)
      else N.CIstatic
    | x when x = SN.SpecialIdents.this -> N.CIvar (p, N.This)
    | x when x.[0] = '$' -> N.CIvar (p, N.Lvar (Env.lvar env cid))
    | _ -> N.CI (Env.class_name env cid)

and casel env l =
  lfold (case env) SMap.empty l

and case env acc = function
  | Default b ->
    let b = cut_and_flatten ~replacement:Fallthrough env b in
    let all_locals, b = branch env b in
    let acc = SMap.union all_locals acc in
    acc, N.Default b
  | Case (e, b) ->
    let e = expr env e in
    let b = cut_and_flatten ~replacement:Fallthrough env b in
    let all_locals, b = branch env b in
    let acc = SMap.union all_locals acc in
    acc, N.Case (e, b)

and catchl env l = lfold (catch env) SMap.empty l
and catch env acc (x1, x2, b) =
  Env.scope env (
  fun env ->
    let x2 = Env.new_lvar env x2 in
    let all_locals, b = branch env b in
    let acc = SMap.union all_locals acc in
    acc, (Env.class_name env x1, x2, b)
  )

and afield env = function
  | AFvalue e -> N.AFvalue (expr env e)
  | AFkvalue (e1, e2) -> N.AFkvalue (expr env e1, expr env e2)

and afield_value env cname = function
  | AFvalue e -> expr env e
  | AFkvalue (e1, e2) ->
    Errors.unexpected_arrow (fst e1) cname;
    expr env e1

and afield_kvalue env cname = function
  | AFvalue e ->
    Errors.missing_arrow (fst e) cname;
    expr env e, expr env (fst e, Lvar (fst e, "__internal_placeholder"))
  | AFkvalue (e1, e2) -> expr env e1, expr env e2

and attrl env l = List.map (attr env) l
and attr env (x, e) = x, expr env e

and string2 env idl =
  rev_rev_map (expr env) idl


(*****************************************************************************)
(* Function/Method Body Naming: *)
(* Ensure that, given a function / class, any UnnamedBody within is
 * transformed into a a named body *)
(*****************************************************************************)

let func_body nenv f =
  match f.N.f_body with
    | N.NamedBody b -> b
    | N.UnnamedBody { N.fub_ast; N.fub_tparams; N.fub_namespace; _ } ->
      let genv = Env.make_fun_genv nenv
        SMap.empty f.N.f_mode (snd f.N.f_name) fub_namespace in
      let genv = extend_params genv fub_tparams in
      let lenv = Env.empty_local () in
      let env = genv, lenv in
      (* Reuse the ids issued by the naming pass over the params
       * in the declaration *)
      let add_param_as_local = begin fun param env ->
        let p_name = param.N.param_name in
        let p_pos, _ = param.N.param_id in
        let () = Env.add_lvar env (p_pos, p_name) param.N.param_id in
        env
      end in
      let env = List.fold_right add_param_as_local f.N.f_params env in
      let env = match f.N.f_variadic with
        | N.FVellipsis | N.FVnonVariadic -> env
        | N.FVvariadicArg param -> add_param_as_local param env
      in
      let body = block env fub_ast in
      let unsafe = func_body_had_unsafe env in {
        N.fnb_nast = body;
        fnb_unsafe = unsafe;
      }

let meth_body genv m =
  let named_body = (match m.N.m_body with
    | N.NamedBody _ as b -> b
    | N.UnnamedBody {N.fub_ast; N.fub_tparams; N.fub_namespace; _} ->
      let genv = {genv with namespace = fub_namespace} in
      let genv = extend_params genv fub_tparams in
      let env = genv, Env.empty_local() in

      (* Reuse the ids issued by the naming pass over the params
       * in the declaration *)
      let add_param_as_local = begin fun param env ->
        let p_name = param.N.param_name in
        let p_pos, _ = param.N.param_id in
        let () = Env.add_lvar env (p_pos, p_name) param.N.param_id in
        env
      end in
      let env = List.fold_right add_param_as_local m.N.m_params env in
      let env = match m.N.m_variadic with
        | N.FVellipsis | N.FVnonVariadic -> env
        | N.FVvariadicArg param -> add_param_as_local param env
      in
      let body = block env fub_ast in
      let unsafe = func_body_had_unsafe env in
      N.NamedBody {
        N.fnb_nast = body;
        fnb_unsafe = unsafe;
      }
  ) in
  {m with N.m_body = named_body}

let class_meth_bodies nenv nc =
  let n_tparams, cstrs = nc.N.c_tparams in
  let tparams = List.map (fun (_, x, _) -> x) n_tparams in
  let genv  = Env.make_class_genv nenv cstrs
    nc.N.c_mode tparams (nc.N.c_name, nc.N.c_kind) Namespace_env.empty
  in
  let inst_genv = {genv with in_instance_method = true} in
  let inst_meths = List.map (meth_body inst_genv) nc.N.c_methods in
  let opt_constructor = match nc.N.c_constructor with
    | None -> None
    | Some c -> Some (meth_body inst_genv c) in
  let static_genv = {genv with in_instance_method = false} in
  let static_meths = List.map (meth_body static_genv) nc.N.c_static_methods in
  { nc with
    N.c_methods        = inst_meths;
    N.c_static_methods = static_meths ;
    N.c_constructor    = opt_constructor ;
  }

(*****************************************************************************)
(* Typedefs *)
(*****************************************************************************)

let typedef genv tdef =
  let ty = match tdef.t_kind with Alias t | NewType t -> t in
  let cstrs = class_constraints tdef.t_tparams in
  let env = Env.make_typedef_env genv cstrs tdef in
  let tconstraint = opt_map (hint env) tdef.t_constraint in
  List.iter check_constraint tdef.t_tparams;
  let tparaml = type_paraml env tdef.t_tparams in
  List.iter begin function
    | (_, _, Some (_, (pos, _))) ->
        Errors.typedef_constraint pos;
    | _ -> ()
  end tparaml;
  let attrs = user_attributes env tdef.t_user_attributes in
  {
    N.t_tparams = tparaml;
    t_constraint = tconstraint;
    t_kind = hint env ty;
    t_user_attributes = attrs;
  }

(*****************************************************************************)
(* Global constants *)
(*****************************************************************************)

let check_constant cst =
  (match cst.cst_type with
  | None when cst.cst_mode = FileInfo.Mstrict ->
      Errors.add_a_typehint (fst cst.cst_name)
  | None
  | Some _ -> ());
  check_constant_expr cst.cst_value

let global_const genv cst =
  let env = Env.make_const_env genv cst in
  let hint = opt_map (hint env) cst.cst_type in
  let e = match cst.cst_kind with
  | Ast.Cst_const -> check_constant cst; Some (expr env cst.cst_value)
  (* Define allows any expression, so don't call check_constant. Furthermore it
   * often appears at toplevel, which we don't track at all, so don't type or
   * even name that expression, it may refer to "undefined" variables that
   * actually exist, just untracked since they're toplevel. *)
  | Ast.Cst_define -> None in
  { N.cst_mode = cst.cst_mode;
    cst_name = cst.cst_name;
    cst_type = hint;
    cst_value = e;
  }

(*****************************************************************************)
(* Declaring the names in a list of files *)
(*****************************************************************************)

let add_files_to_rename nenv failed defl defs_in_env =
  List.fold_left begin fun failed (_, def) ->
    match SMap.get def defs_in_env with
    | None -> failed
    | Some (previous_definition_position, _) ->
      let filename = Pos.filename previous_definition_position in
      Relative_path.Set.add filename failed
  end failed defl

let ndecl_file fn
    {FileInfo.file_mode; funs;
     classes; typedefs; consts; consider_names_just_for_autoload; comments}
    (errorl, failed, nenv) =
  let errors, nenv = Errors.do_ begin fun () ->
    dn ("Naming decl: "^Relative_path.to_absolute fn);
    if consider_names_just_for_autoload
    then nenv
    else make_env nenv ~funs ~classes ~typedefs ~consts
  end
  in
  match errors with
  | [] -> errorl, failed, nenv
  | l ->
  (* IMPORTANT:
   * If a file has name collisions, we MUST add the list of files that
   * were previously defining the type to the set of "failed" files.
   * If we fail to do so, we will be in a phony state, where a name could
   * be missing.
   *
   * Example:
   * A.php defines class A
   * B.php defines class B
   * Save the state, now let's introduce a new file (foo.php):
   * foo.php defines class A and class B.
   *
   * 2 things happen (cf serverTypeCheck.ml):
   * We remove the names A and B from the global environment.
   * We report the error.
   *
   * But this is clearly not enough. If the user removes the file foo.php,
   * both class A and class B are now missing from the naming environment.
   * If the user has a file using class A (in strict), he now gets the
   * error "Unbound name class A".
   *
   * The solution consist in adding all the files that were previously
   * defining the same things as foo.php to the set of files to recheck.
   *
   * This way, when the user removes foo.php, A.php and B.php are recomputed
   * and the naming environment is in a sane state.
   *)
  let failed = add_files_to_rename nenv failed funs (fst nenv.ifuns) in
  let failed = add_files_to_rename nenv failed classes (fst nenv.iclasses) in
  let failed = add_files_to_rename nenv failed typedefs nenv.itypedefs in
  let failed = add_files_to_rename nenv failed consts nenv.iconsts in
  List.rev_append l errorl, Relative_path.Set.add fn failed, nenv
