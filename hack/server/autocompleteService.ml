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

module Phase = Typing_phase

(* Details about functions to be added in json output *)
type func_param_result = {
    param_name     : string;
    param_ty       : string;
    param_variadic : bool;
  }

type func_details_result = {
    params    : func_param_result list;
    return_ty : string;
    min_arity : int;
  }

(* Results ready to be displayed to the user *)
type complete_autocomplete_result = {
    res_pos      : Pos.absolute;
    res_ty       : string;
    res_name     : string;
    expected_ty  : bool;
    func_details : func_details_result option;
  }

(* Results that still need a typing environment to convert ty information
   into strings *)
type autocomplete_result = {
    ty   : Typing_defs.phase_ty;
    name : string;
    desc : string option
  }

(* The type returned to the client *)
type result = complete_autocomplete_result list

let ac_env = ref None
let ac_type = ref None
let autocomplete_results = ref []

let auto_complete_suffix = "AUTO332"
let suffix_len = String.length auto_complete_suffix
let strip_suffix s = String.sub s 0 (String.length s - suffix_len)
let is_auto_complete x =
  if !autocomplete_results = []
  then begin
    String.length x >= suffix_len &&
    let suffix = String.sub x (String.length x - suffix_len) suffix_len in
    suffix = auto_complete_suffix
  end else
    false

let autocomplete_result_to_json res =
  let func_param_to_json param =
    Hh_json.JAssoc [ "name", Hh_json.JString param.param_name;
                     "type", Hh_json.JString param.param_ty;
                     "variadic", Hh_json.JBool param.param_variadic;
                   ]
  in
  let func_details_to_json details =
    match details with
     | Some fd -> Hh_json.JAssoc [ "min_arity", Hh_json.JInt fd.min_arity;
             "return_type", Hh_json.JString fd.return_ty;
             "params", Hh_json.JList (List.map func_param_to_json fd.params);
           ]
     | None -> Hh_json.JNull
  in
  let name = res.res_name in
  let pos = res.res_pos in
  let ty = res.res_ty in
  let expected_ty = res.expected_ty in
  Hh_json.JAssoc [ "name", Hh_json.JString name;
           "type", Hh_json.JString ty;
           "pos", Pos.json pos;
           "func_details", func_details_to_json res.func_details;
           "expected_ty", Hh_json.JBool expected_ty;
         ]

let add_result name ty =
  autocomplete_results :=
    {
      ty   = ty;
      name = name;
      desc = None;
    } :: !autocomplete_results

let add_result_with_desc name ty desc =
  autocomplete_results :=
    {
      ty   = ty;
      name = name;
      desc = Some desc;
    } :: !autocomplete_results

let autocomplete_token ac_type env x =
  if is_auto_complete (snd x)
  then begin
    ac_env := env;
    Autocomplete.auto_complete_pos := Some (fst x);
    Autocomplete.argument_global_type := Some ac_type;
    Autocomplete.auto_complete_for_global := snd x
  end

let autocomplete_id id env = autocomplete_token Autocomplete.Acid (Some env) id

let autocomplete_hint = autocomplete_token Autocomplete.Actype None

let autocomplete_new cid env _ =
  match cid with
  | Nast.CI sid -> autocomplete_token Autocomplete.Acnew (Some env) sid
  | _ -> ()

let autocomplete_method is_static class_ id env cid ~is_method =
  if is_auto_complete (snd id)
  then begin
    ac_env := Some env;
    Autocomplete.auto_complete_pos := Some (fst id);
    Autocomplete.argument_global_type := Some Autocomplete.Acclass_get;
    let results =
      if is_static
      then SMap.union class_.tc_smethods
                      (SMap.union class_.tc_consts class_.tc_sprops)
      else SMap.union class_.tc_methods class_.tc_props
    in
    let results = SMap.filter begin fun _ x ->
      match Typing.is_visible env x.ce_visibility cid with
        | None -> true
        | _ -> false
      end results
    in
    SMap.iter begin fun x class_elt ->
        add_result x (Phase.decl class_elt.ce_type)
      end results
  end

let autocomplete_smethod = autocomplete_method true

let autocomplete_cmethod = autocomplete_method false

let autocomplete_lvar_naming _ id locals =
  if is_auto_complete (snd id)
  then begin
    Autocomplete.argument_global_type := Some Autocomplete.Acprop;
    (* Store the position and a map of name to ident so we can add
     * types at this point later *)
    Autocomplete.auto_complete_pos := Some (fst id);
    Autocomplete.auto_complete_vars := SMap.map snd locals
  end

let autocomplete_lvar_typing id env =
  if Some (fst id)= !(Autocomplete.auto_complete_pos)
  then begin
    (* The typechecker might call this hook more than once (loops) so we
     * need to clear the list of results first or we could have repeat locals *)
    autocomplete_results := [];
    ac_env := Some env;
    Autocomplete.auto_complete_pos := Some (fst id);
    (* Get the types of all the variables in scope at this point *)
    SMap.iter begin fun x ident ->
      let _, ty = Typing_env.get_local env ident in
      add_result x (Phase.locl ty)
    end !Autocomplete.auto_complete_vars;
    (* Add $this if we're in a instance method *)
    let ty = Typing_env.get_self env in
    if not (Typing_env.is_static env) && (fst ty) <> Reason.Rnone
    then add_result
      Naming_special_names.SpecialIdents.this (Phase.locl ty)
  end

let should_complete_class completion_type class_kind =
  match completion_type, class_kind with
  | Some Autocomplete.Acid, Ast.Cnormal
  | Some Autocomplete.Acid, Ast.Cabstract
  | Some Autocomplete.Acnew, Ast.Cnormal
  | Some Autocomplete.Actype, _ -> true
  | _ -> false

let should_complete_fun completion_type =
  completion_type=Some Autocomplete.Acid

let get_constructor_ty c =
  let pos = c.Typing_defs.tc_pos in
  let reason = Typing_reason.Rwitness pos in
  let return_ty = reason, Typing_defs.Tapply ((pos, c.Typing_defs.tc_name), []) in
  match (fst c.Typing_defs.tc_construct) with
    | Some elt ->
        begin match elt.ce_type with
          | (_ as r, Tfun fun_) ->
              (* We have a constructor defined, but the return type is void
               * make it the object *)
              let fun_ = { fun_ with Typing_defs.ft_ret = return_ty } in
              r, Tfun fun_
          | _ -> (* how can a constructor not be a function? *) assert false
        end
    | None ->
        (* Nothing defined, so we need to fake the entire constructor *)
      reason, Typing_defs.Tfun (Typing_env.make_ft pos [] return_ty)

let compute_complete_global funs classes =
  let completion_type = !Autocomplete.argument_global_type in
  let gname = Utils.strip_ns !Autocomplete.auto_complete_for_global in
  let gname = strip_suffix gname in
  let result_count = ref 0 in
  try
    List.iter begin fun name ->
      if !result_count > 100 then raise Exit;
      if str_starts_with (strip_ns name) gname
      then match (Typing_env.Classes.get name) with
        | Some c
          when should_complete_class completion_type c.Typing_defs.tc_kind ->
            incr result_count;
            let s = Utils.strip_ns name in
            (match !ac_env with
              | Some _env when completion_type=Some Autocomplete.Acnew ->
                  add_result s (Phase.decl (get_constructor_ty c))
              | _ ->
                  let desc = match c.Typing_defs.tc_kind with
                    | Ast.Cabstract -> "abstract class"
                    | Ast.Cnormal -> "class"
                    | Ast.Cinterface -> "interface"
                    | Ast.Ctrait -> "trait"
                    | Ast.Cenum -> "enum"
                  in
                  let ty =
                    Typing_reason.Rwitness c.Typing_defs.tc_pos,
                    Typing_defs.Tapply ((c.Typing_defs.tc_pos, name), [])
                  in
                  add_result_with_desc s (Phase.decl ty) desc)
        | _ -> ()
    end classes;
    if should_complete_fun completion_type
    then begin
      (* Disgusting hack alert!
       *
       * In PHP/Hack, namespaced function lookup falls back into the global
       * namespace if no function in the current namespace exists. The
       * typechecker knows everything that exists, and resolves all of this
       * during naming -- meaning that by the time that we get to typing, not
       * only has "gname" been fully qualified, but we've lost whatever it
       * might have looked like originally. This makes it tough to do the full
       * namespace fallback behavior here -- we'd like to know if whatever
       * "gname" corresponds to in the source code has a '\' to qualify it, but
       * since it's already fully qualified here, we can't know.
       *
       * Except, we can kinda reverse engineer and figure it out. We have the
       * positional information, which we can use to figure out how long the
       * original source code token was, and then figure out what portion of
       * "gname" that corresponds to, and see if it has a '\'. Since fully
       * qualifying a name will always prepend, this all works.
       *)
      let gname_gns = match !Autocomplete.auto_complete_pos with
        | None -> None
        | Some p ->
            let len =
              p.Pos.pos_end.Lexing.pos_cnum -
              p.Pos.pos_start.Lexing.pos_cnum -
              suffix_len in
            let start = String.length gname - len in
            if String.contains_from gname start '\\'
            then None else Some (strip_all_ns gname)
      in
      List.iter begin fun name ->
        if !result_count > 100 then raise Exit;
        let stripped_name = strip_ns name in
        let matches_gname = str_starts_with stripped_name gname in
        let matches_gname_gns = match gname_gns with
          | None -> false
          | Some s -> str_starts_with stripped_name s in
        if matches_gname || matches_gname_gns
        then match (Typing_env.Funs.get name) with
          | Some fun_ ->
            incr result_count;
            let ty =
              Typing_reason.Rwitness fun_.Typing_defs.ft_pos,
              Typing_defs.Tfun fun_
            in
            add_result stripped_name (Phase.decl ty)
          | _ -> ()
      end funs
    end
  with Exit -> ()

let process_fun_call fun_args used_args env =
  let is_target target_pos p =
    let line, char_pos, _ = Pos.info_pos target_pos in
    let start_line, start_col, end_col = Pos.info_pos p in
    start_line = line && start_col <= char_pos && char_pos - 1 <= end_col
  in
  match !Autocomplete.auto_complete_pos with
    | Some pos when !ac_type = None ->
        (* This function gets called on the 'way up' of the recursion that
         * processes args. Therefore, inner arguments will hit this function
         * first, so we only care when we don't have a result yet. This has to
         * happen on the way up because autocomplete pos needs to get set
         * before this is called *)
        let argument_index = ref (-1) in
        List.iteri begin fun index arg ->
          if is_target pos arg then argument_index := index;
        end used_args;
        begin try
          let _, arg_ty = List.nth fun_args !argument_index in
          ac_type := Some arg_ty
        with
          | Failure _ ->
              (* They're specifying too many args, so we'll accept anything *)
              ac_type := Some (Typing_reason.Rnone, Typing_defs.Tany)
          | Invalid_argument _ ->
              (* Never matched at all*)
              ()
        end
    | _ -> ()

let rec result_matches_expected_ty ty =
  match !ac_type, !ac_env with
    | Some goal_type, Some env ->
        (match goal_type, ty with
          | (_, Tany), _ | _, (_, Tany) ->
              (* Everything will just be a match so this is pointless *)
              false
          | _, (_, Tfun fun_) ->
              (* if this is a function, we'll check if the return type
               * is a good result as well TODO: stop after enough levels
               * and explore methods on the objects as well *)
              if Typing_subtype.is_sub_type env goal_type ty then true
              else result_matches_expected_ty fun_.Typing_defs.ft_ret
          | _ -> Typing_subtype.is_sub_type env goal_type ty)
    | _ -> false


let result_compare a b =
  if a.expected_ty = b.expected_ty then
    String.compare a.res_name b.res_name
  else if a.expected_ty then -1
  else 1

let get_results funs classes =
  let completion_type = !Autocomplete.argument_global_type in
  if completion_type = Some Autocomplete.Acid ||
     completion_type = Some Autocomplete.Acnew ||
     completion_type = Some Autocomplete.Actype
  then compute_complete_global funs classes;
  let results = !autocomplete_results in
  let env = match !ac_env with
    | Some e -> e
    | None ->
      let tcopt = TypecheckerOptions.permissive in
      Typing_env.empty tcopt Relative_path.default
  in
  let results = List.map begin fun x ->
    let env, ty = match x.ty with
      | DeclTy ty -> Phase.localize_with_self env ty
      | LoclTy ty -> env, ty
    in
    let desc_string = match x.desc with
      | Some s -> s
      | None -> Typing_print.full_strip_ns env ty
    in
    let func_details = match ty with
      | (_, Tfun ft) ->
        let param_to_record ?(is_variadic=false) (name, pty) =
          {
            param_name     = (match name with
                               | Some n -> n
                               | None -> "");
            param_ty       = Typing_print.full_strip_ns env pty;
            param_variadic = is_variadic;
          }
        in
        Some {
          return_ty = Typing_print.full_strip_ns env ft.ft_ret;
          min_arity = arity_min ft.ft_arity;
          params    = List.map param_to_record ft.ft_params @
            (match ft.ft_arity with
               | Fellipsis _ -> let empty = (None, (Reason.none, Tany)) in
                                [param_to_record ~is_variadic:true empty]
               | Fvariadic (_, p) -> [param_to_record ~is_variadic:true p]
               | Fstandard _ -> [])
        }
      | _ -> None
    in
    let expected_ty = result_matches_expected_ty ty in
    let pos = Typing_reason.to_pos (fst ty) in
    {
      res_pos      = Pos.to_absolute pos;
      res_ty       = desc_string;
      res_name     = x.name;
      expected_ty  = expected_ty;
      func_details = func_details;
    }
  end results in
  List.sort result_compare results

let reset () =
  Autocomplete.auto_complete_for_global := "";
  Autocomplete.argument_global_type := None;
  Autocomplete.auto_complete_pos := None;
  Autocomplete.auto_complete_vars := SMap.empty;
  ac_env := None;
  ac_type := None;
  autocomplete_results := []

let attach_hooks () =
  reset();
  Autocomplete.auto_complete := true;
  Typing_hooks.attach_id_hook autocomplete_id;
  Typing_hooks.attach_smethod_hook autocomplete_smethod;
  Typing_hooks.attach_cmethod_hook autocomplete_cmethod;
  Typing_hooks.attach_lvar_hook autocomplete_lvar_typing;
  Typing_hooks.attach_fun_call_hook process_fun_call;
  Typing_hooks.attach_new_id_hook autocomplete_new;
  Naming_hooks.attach_hint_hook autocomplete_hint;
  Naming_hooks.attach_lvar_hook autocomplete_lvar_naming

let detach_hooks () =
  reset();
  Autocomplete.auto_complete := false;
  Typing_hooks.remove_all_hooks();
  Naming_hooks.remove_all_hooks()
