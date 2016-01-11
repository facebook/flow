(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Entry point to the experimental Hack based bytecode emitter. Contains
 * code for emitting "top level" constructs like classes and functions. *)

open Core
open Utils
open Nast

open Emitter_core

module SN = Naming_special_names

let emit_generator_prologue env m =
  match m.m_fun_kind with
    | Ast.FGenerator | Ast.FAsyncGenerator ->
      let env = emit_CreateCont env in
      emit_PopC env
    | Ast.FSync | Ast.FAsync -> env

let emit_func_body_code env m =
  let body = assert_named_body m.m_body in
  if body.fnb_unsafe then unimpl "UNSAFE";
  let env = emit_generator_prologue env m in

  let acts = { empty_nonlocal_actions
               with return_action = Emitter_stmt.default_return_action } in
  let env, terminal =
    with_actions env acts Emitter_stmt.emit_block body.fnb_nast in
  let env = if terminal then env else
            Emitter_stmt.default_return_action
              ~has_value:false ~is_initial:true env in
  env

let emit_func_body env m =
  let env, output = collect_output env emit_func_body_code m in
  (* output an iterator count *)
  let env = match env.num_iterators with 0 -> env
    | n -> emit_strs env [".numiters"; string_of_int n; ";"] in
  emit_str_raw env output

(* returns the param text for this param along with
 * Some (name, DV id, expr) if there is a default param and None otherwise *)
let emit_param ~tparams i p =
  if p.param_is_reference then unimpl "reference params";
  if p.param_is_variadic then unimpl "variadic params";
  let type_info =
    Emitter_types.fmt_hint_info ~tparams ~always_extended:false p.param_hint in

  let name = get_lid_name p.param_id in
  match p.param_expr with
  | None -> type_info ^ name, None
  | Some default ->
    let dv_id = "DV" ^ string_of_int i in
    (* TODO: emit eval'able php for the argument for reflection *)
    let phpCode = "(\"\"\"<UNIMPLEMENTED>\"\"\")" in
    type_info ^ name ^ " = " ^ dv_id ^ phpCode,
    Some (name, dv_id, default)

let fmt_params ?(tparams=[]) params =
  let param_strs, defaults =
    List.unzip (List.mapi ~f:(emit_param ~tparams) params) in
  "(" ^ String.concat ", " param_strs ^ ")",
  List.filter_opt defaults


let fmt_visibility vis =
  match vis with
  | Public -> "public"
  | Private -> "private"
  | Protected -> "protected"

let fmt_fun_tags env m ~is_closure =
  (match m.m_fun_kind with
  | Ast.FSync -> ""
  | Ast.FAsync -> " isAsync"
  | Ast.FGenerator -> " isGenerator"
  | Ast.FAsyncGenerator -> " isGenerator isAsync") ^
  (if env.function_props.is_pair_generator then
      " isPairGenerator" else "") ^
  (if is_closure then " isClosureBody" else "")

let fmt_user_attribute attr =
  let array =
    Pos.none, make_varray attr.ua_params in
  let value = match Emitter_lit.fmt_lit array with
    | None -> bug "user attribute value not a literal"
    | Some e -> e in
  quote_str (snd attr.ua_name) ^ "(" ^ value ^ ")"

let fmt_user_attributes attrs = List.map ~f:fmt_user_attribute attrs

let is_internal_attribute attr = str_starts_with (snd attr.ua_name) "__"

(* Do whatever special processing for any __ user attrs;
 * this is mostly just failing on ones we don't expect, but
 * will eventually involve responding to ones that require codegen
 * action (like __Memoize) *)
let handle_func_attrs env m =
  List.fold_left m.m_user_attributes ~init:env ~f:begin fun env attr->
    if not (is_internal_attribute attr) then env else
    let name = snd attr.ua_name in
    if name = Naming_special_names.UserAttributes.uaOverride ||
       name = Naming_special_names.UserAttributes.uaConsistentConstruct ||
       name = Naming_special_names.UserAttributes.uaUnsafeConstruct ||
       name = Naming_special_names.UserAttributes.uaDeprecated
    then env
    else unimpl ("function user attribute: " ^ name)
  end

(* For closure upvars, the exact allocation of local variable ids
 * is very important; they need to come before regular local variables
 * and be in the same order as the properties, so we emit a
 * .declvars directive to assign IDs to the closure variables
 * before any regular locals get assigned IDs. *)
let emit_closure_vars env = function
  | None -> env
  | Some vars ->
    emit_strs env ([".declvars"] @ vars @ [";"])

let emit_default_inits env top_label inits =
  let emit_default_init env (id, dv, expr) =
    let env = emit_label env dv in
    let env = Emitter_expr.emit_expr env expr in
    let env = emit_SetL env id in
    emit_PopC env
  in
  let env = List.fold_left ~f:emit_default_init ~init:env inits in
  if inits = [] then env else
    emit_JmpNS env top_label

(* Emit a method or a function. m, the method, might actually just be
 * a dummy that was built from a function. We need tparams (which will be
 * any tparams that come from the class) because formatting types needs
 * a list of the bound tparams. *)
let emit_method_or_func env ~is_method ~is_static ~tparams ~full_name
                        ?(closure_counter=ref 0) (* makes a new ref each time *)
                        ?closure_vars m name =
  let tparams = tparams @ m.m_tparams in
  let env = start_new_function env in
  let closure_state = { is_static; full_name; tparams; closure_counter } in
  let env = { env with closure_state } in

  if m.m_variadic != FVnonVariadic then unimpl "variadic functions";

  let env = handle_func_attrs env m in

  (* We actually emit the body first, but save the output, so we can
   * gather data on what occurs in the function. *)
  let env, body_output =
    collect_output env
      (fun env -> with_indent env (env.indent+1) emit_func_body) m in


  let options = bool_option "abstract" m.m_abstract @
                bool_option "final" m.m_final @
                bool_option "static" (is_method && is_static) @
                bool_option (fmt_visibility m.m_visibility) is_method @
                ["mayusevv"] @
                fmt_user_attributes m.m_user_attributes in
  let is_closure = closure_vars <> None in
  let param_text, default_args = fmt_params ~tparams m.m_params in
  let post = param_text ^ fmt_fun_tags env m ~is_closure in
  let tag = if is_method then ".method" else ".function" in

  (* return type hints are always "extended" because php doesn't have them *)
  let type_and_name =
    Emitter_types.fmt_hint_info ~tparams ~always_extended:true m.m_ret ^ name in

  let env, top_label = fresh_label env in

  let env = emit_enter env tag options type_and_name post in
  let env = emit_closure_vars env closure_vars in
  let env = emit_label env top_label in
  let env = emit_str_raw env body_output in
  let env = emit_default_inits env top_label default_args in
  let env = run_cleanups env in
  let env = emit_exit env in
  env

let emit_method env ~is_static ~cls m =
  let name = snd m.m_name in
  emit_method_or_func env ~is_method:true ~is_static
    ~tparams:(fst cls.c_tparams)
    ~full_name:(snd cls.c_name ^ "::" ^ name)
    m name

(* Make a dummy method structure from a fun_ to help share code *)
let fun_to_method f =
  {
    (* Things we don't really have *)
    m_final = false; m_abstract = false; m_visibility = Public;
    (* Copy the rest over *)
    m_name = f.f_name; m_tparams = f.f_tparams; m_variadic = f.f_variadic;
    m_params = f.f_params; m_body = f.f_body; m_fun_kind = f.f_fun_kind;
    m_user_attributes = f.f_user_attributes; m_ret = f.f_ret
  }

let emit_fun nenv env (_, x) =
  let f = Naming_heap.FunHeap.find_unsafe x in
  let nb = Naming.func_body nenv f in
  let f = { f with f_body = NamedBody nb } in
  let dummy_method = fun_to_method f in

  let env = start_new_function env in
  let env =
    emit_method_or_func env ~is_method:false ~is_static:true
      ~tparams:[] ~full_name:x dummy_method (fmt_name x) in
  emit_str env ""


(* extends lists and things are hints,
 * but I *think* it needs to be an apply? *)
let fmt_class_hint = function
  | _, Happly ((_, cls), _) -> fmt_name cls
  | _ -> bug "class hint not apply??"
let fmt_implements_list classes =
  match classes with
  | [] -> ""
  | xs -> " implements ("^String.concat " " (List.map ~f:fmt_class_hint xs)^")"
let fmt_extends_list classes =
  match classes with
  | [] -> ""
  | [x] -> " extends " ^ fmt_class_hint x
  | _ -> bug "nonempty extends list"

let emit_use env use =
  emit_strs env [".use"; fmt_class_hint use ^ ";"]

let emit_prop_init env ~is_static = function
  | [] -> env
  | vars ->
    let name, flag, extra_opts =
      if is_static then
        "86sinit", "Static", ["static"]
      else
        "86pinit", "NonStatic", []
    in

    let options = ["private"; "mayusevv"] @ extra_opts in
    let env = emit_enter env ".method" options name "()" in

    let fmt_var_init env (name, expr) =
      let env, skip_label = fresh_label env in
      let env = if is_static then env else
        let env = emit_CheckProp env name in
        emit_cjmp env true skip_label
      in
      let env = Emitter_expr.emit_expr env expr in
      let env = emit_InitProp env name flag in
      emit_label env skip_label
    in
    let env = List.fold_left ~f:fmt_var_init ~init:env vars in

    let env = emit_Null env in
    let env = emit_RetC env in
    let env = emit_exit env in
    env


let emit_prop env name options v =
  emit_strs env
    [".property"; fmt_options options; name; "="; v^";"]

(* Returns None if the case has been handled, and Some (name, expr) if
 * it still needs to be taken care of in a pinit *)
let emit_var env ~is_static var =
  (* XHP props aren't real props; ignore them here. *)
  if var.cv_is_xhp then env, None else

  let fmt_prop = emit_prop env (snd var.cv_id) in

  assert (var.cv_final = false); (* props can't be final *)
  let options = bool_option "static" is_static @
                [fmt_visibility var.cv_visibility] in
  match var.cv_expr with
  | None -> fmt_prop options "\"\"\"N;\"\"\"", None
  | Some expr ->
    match Emitter_lit.fmt_lit expr with
    | Some lit -> fmt_prop options lit, None
    | None ->
      (* If we can't generate a static initializer, set deep_init,
       * which says that the data can't just get memcpy()'d to new
       * objects. This is only necessary for non-static props, since
       * static ones would only need to do the init once anyways.
       * XXX: This a little stricter than emitter.cpp, which I think
       * can emit 86pinit's for certain things that it then doesn't
       * mark deep_init (see requiresDeepInit). *)
      let options = bool_option "deep_init" (not is_static) @ options in
      fmt_prop options "uninit", Some (snd var.cv_id, expr)

(* the constant init function doesn't actually "init" anything;
 * it takes the name of a constant as an argument and returns the
 *  value *)
let emit_const_init env = function
  | [] -> env
  | consts ->
    let name = "86cinit" in
    let options = ["private"; "static"; "mayusevv"] in
    let env = emit_enter env ".method" options name "($constName)" in

    let nconsts = List.length consts in
    let emit_case i env (name, expr) =
      let is_last = i = nconsts-1 in

      let env, skip_label = fresh_label env in
      (* Don't emit the test for the last one *)
      let env = if is_last then env else
          (* Is this the constant we are looking for? *)
          let env = emit_CGetL env "$constName" in
          let env = emit_String env name in
          let env = emit_binop env Ast.Eqeq in
          emit_cjmp env false skip_label
      in
      (* Emit the constant and return it *)
      let env = Emitter_expr.emit_expr env expr in
      let env = emit_RetC env in
      if is_last then env else emit_label env skip_label
    in
    let env = List.foldi ~f:emit_case ~init:env consts in
    let env = emit_exit env in
    env

let emit_const env (_, (_, name), opt_expr) =
  match opt_expr with
  | None -> env, None
  | Some expr ->
    let fmt_const v = emit_strs env [".const"; ""; name; "="; v^";"] in
    match Emitter_lit.fmt_lit expr with
    | Some lit -> fmt_const lit, None
    | None -> fmt_const "uninit", Some (name, expr)

let emit_tconst env tconst =
  match tconst.c_tconst_type with
  | None -> env
  | Some _ ->
    (* TODO: Actually emit the type structure *)
    let structure = Pos.none, Null in
    let v = unsafe_opt (Emitter_lit.fmt_lit structure) in
    emit_strs env [".const"; ""; snd tconst.c_tconst_name; "isType"; "="; v^";"]

let emit_enum ~cls env e =
  emit_strs env [".enum_ty";
                 Emitter_types.fmt_hint_constraint
                   ~tparams:(fst cls.c_tparams) ~always_extended:true e.e_base
                 ^ ";"]

let class_kind_options  = function
  | Ast.Cabstract -> ["abstract"]
  | Ast.Cnormal -> []
  | Ast.Cinterface -> ["interface"]
  | Ast.Ctrait -> ["final"; "trait"]
  | Ast.Cenum -> ["final"; "enum"]

(* do any handling of user attributes we need
 * (just failing on ones we don't recognize now) *)
let handle_class_attrs env cls =
  List.fold_left cls.c_user_attributes ~init:env ~f:begin fun env attr->
    if not (is_internal_attribute attr) then env else
      let name = snd attr.ua_name in
      if name = Naming_special_names.UserAttributes.uaConsistentConstruct ||
         name = Naming_special_names.UserAttributes.uaUnsafeConstruct ||
         name = Naming_special_names.UserAttributes.uaDeprecated
      then env
      else unimpl ("function user attribute: " ^ name)
  end

let emit_class nenv env (_, x) =
  let cls = Naming_heap.ClassHeap.find_unsafe x in
  let cls = Naming.class_meth_bodies nenv cls in

  (* make any adjustments to the class that are needed for xhp *)
  let cls = Emitter_xhp.convert_class cls in

  let env = start_new_function env in

  let env = handle_class_attrs env cls in

  let options = bool_option "final" cls.c_final @
                class_kind_options cls.c_kind @
                fmt_user_attributes cls.c_user_attributes in

  (* The "extends" list of an interface is "implements" to hhvm *)
  let implements, extends = match cls.c_kind with
    | Ast.Cinterface -> cls.c_extends, []
    | Ast.Cenum -> [], [Pos.none,
                        Happly ((Pos.none, SN.Classes.cHH_BuiltinEnum), [])]
    | Ast.Cabstract | Ast.Cnormal | Ast.Ctrait ->
      cls.c_implements, cls.c_extends in

  let extends_list = fmt_extends_list extends in
  let implements_list = fmt_implements_list implements in

  let name = fmt_name x in
  let self_name, parent_name = match cls.c_kind with
    (* interfaces don't have code so we don't need the names;
     * traits can make self and parent calls to things that are actually
     * in a class that uses them, so we don't use the name *)
    | Ast.Cinterface | Ast.Ctrait -> None, None
    | Ast.Cabstract | Ast.Cnormal | Ast.Cenum  ->
      Some name,
      match cls.c_extends with
        | [] -> None
        | [x] -> Some (fmt_class_hint x)
        | _ -> assert false in
  let env = { env with self_name; parent_name } in

  let env = emit_enter env ".class" options name
                       (extends_list^implements_list) in

  (* Emit all of the main content parts of the class *)
  let env = opt_fold (emit_enum ~cls) env cls.c_enum in
  let env = List.fold_left ~f:emit_use ~init:env cls.c_uses in
  let env, uninit_vars =
    List.map_env env cls.c_vars (emit_var ~is_static:false) in
  let env, uninit_svars =
    List.map_env env cls.c_static_vars (emit_var ~is_static:true) in
  let env = List.fold_left ~f:(emit_method ~is_static:false ~cls) ~init:env
    cls.c_methods in
  let env = List.fold_left ~f:(emit_method ~is_static:true ~cls) ~init:env
    cls.c_static_methods in
  let env, uninit_consts = List.map_env env cls.c_consts emit_const in
  let env = List.fold_left ~f:emit_tconst ~init:env cls.c_typeconsts in

  let uninit_vars = List.filter_opt uninit_vars in
  let uninit_svars = List.filter_opt uninit_svars in
  let uninit_consts = List.filter_opt uninit_consts in

  (* Now for 86* stuff *)
  let env = match cls.c_constructor with
            | None -> emit_str env ".default_ctor;"
            | Some m ->
              (* don't emit the mildly bogus return hint for ctors *)
              emit_method ~is_static:false ~cls env {m with m_ret = None} in

  let env = emit_prop_init env ~is_static:false uninit_vars in
  let env = emit_prop_init env ~is_static:true uninit_svars in
  let env = emit_const_init env uninit_consts in

  let env = emit_exit env in
  let env = { env with self_name = None; parent_name = None; } in

  emit_str env ""


let emit_closure (name, {full_name; closure_counter; is_static; tparams},
                  (fun_, vars)) env =
  let env = start_new_function env in
  let options = ["no_override"; "unique"] in
  let extends_list = " extends Closure" in
  let env = emit_enter env ".class" options name extends_list in

  let names = List.map ~f:get_lid_name vars in
  (* Emit the free vars as properties *)
  let env = List.fold_left names ~init:env ~f:begin fun env var ->
    let prop_name = lstrip var "$" in
    emit_prop env prop_name ["private"] "uninit"
  end in

  (* Closures have an extra hidden variable called "0Closure"
   * (see emitMethodMetadata in emitter.cpp) *)
  let declvars = "$0Closure" :: names in

  (* Closures don't close over self_name/parent_made, but it would
   * be nice, possibly. emitter.cpp doesn't, though, and just uses
   * Self/Parent. *)
  let m = fun_to_method fun_ in
  let env = emit_method_or_func env ~is_method:true
    ~closure_vars:declvars
    ~tparams ~is_static ~full_name ~closure_counter
    m "__invoke" in

  let env = emit_exit env in
  emit_str env ""

(* Emit any closure bodies that need to be created *)
let rec emit_all_closures env =
  if env.pending_closures = [] then env else
  (* More closures might be generated as we emit these, so we clear
   * out the list and then loop at the end. *)
  let closures = env.pending_closures in
  let env = { env with pending_closures = [] } in
  let env = List.fold_right closures ~init:env ~f:emit_closure in
  emit_all_closures env


let emit_typedef _nenv env (_, x) =
  let typedef = Naming_heap.TypedefHeap.find_unsafe x in
  emit_strs env [".alias"; fmt_name x; "=";
                 Emitter_types.fmt_hint_constraint
                   ~tparams:[] ~always_extended:false typedef.t_kind ^ ";"]

(* Bogusly emit a call to a hardcoded test function *)
let emit_test_call env =
  let env = emit_FPushFuncD env 0 "test" in
  let env = emit_FCall env 0 in
  emit_PopR env

let emit_main env ~is_test ast =
  let env = start_new_function env in
  let env = emit_enter env ".main" [] "" "" in

  (* We use the original AST to drive emitting DefTypeAlias and
   * DefCls, since their order matters and the nast doesn't track
   * that. (We could have sorted those by pos or something but we'll
   * want something like this to handle toplevel statements eventually
   * anyways). *)
  let rec emit_program env num_classes num_aliases = function
    | [] -> env
    | def :: defs ->
      match def with
      | Ast.Class _ ->
        let env = emit_DefCls env num_classes in
        emit_program env (num_classes+1) num_aliases defs
      | Ast.Typedef _ ->
        let env = emit_DefTypeAlias env num_aliases in
        emit_program env num_classes (num_aliases+1) defs
      (* It probably wouldn't be too hard to handle this by bundling
       * them into a dummy function... *)
      | Ast.Stmt _ -> unimpl "toplevel stmts"
      | Ast.Namespace (_, ns_defs) ->
        emit_program env num_classes num_aliases (ns_defs@defs)
      | Ast.Fun _ | Ast.Constant _ | Ast.NamespaceUse _ ->
        emit_program env num_classes num_aliases defs
  in

  let env = emit_program env 0 0 ast in

  (* emit debugging test *)
  let env = if is_test then emit_test_call env else env in

  (* emit the expected return *)
  let env = emit_Int env "1" in
  let env = emit_RetC env in

  let env = emit_exit env in
  emit_str env ""

let emit_file ~is_test nenv filename ast
    {FileInfo.funs; classes; typedefs; consts; _} =
  if consts <> [] then unimpl "global consts";

  let env = new_env () in

  let env = emit_strs env
    [".filepath"; quote_str (Relative_path.to_absolute filename) ^ ";\n"] in
  let env = emit_main env ~is_test ast in
  let env = List.fold_left ~f:(emit_fun nenv) ~init:env funs in
  let env = List.fold_left ~f:(emit_class nenv) ~init:env classes in
  let env = List.fold_left ~f:(emit_typedef nenv) ~init:env typedefs in
  let env = emit_all_closures env in
  let env = emit_str env "" in

  let output = get_output env in
  output_string stdout output;
  (* Dump all the output to a log file if HH_EMITTER_LOG set *)
  try Sys_utils.append_file ~file:(Sys.getenv "HH_EMITTER_LOG") output
  with _ -> ()
