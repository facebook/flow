(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Parsing_heaps_utils

let parameter_name is_opt name =
  let opt =
    if is_opt then
      "?"
    else
      ""
  in
  Base.Option.value name ~default:"_" ^ opt

let string_of_ty = Ty_printer.string_of_t_single_line ~with_comments:false

let documentation_of_param_infos name : Jsdoc.Param.t -> string =
  let open Jsdoc.Param in
  let open Utils_js in
  let rec string_of_path = function
    | Name -> ""
    | Element p -> spf "%s[]" (string_of_path p)
    | Member (p, mem) -> spf "%s.%s" (string_of_path p) mem
  in
  let string_of_optional = function
    | NotOptional -> ""
    | Optional -> " (optional) "
    | OptionalWithDefault default -> spf " (optional, defaults to %s) " default
  in
  let string_of_description = function
    | None -> ""
    | Some description -> spf " - %s" description
  in
  let documentation_of_param_info (path, { optional; description }) =
    spf
      "%s%s%s%s"
      name
      (string_of_path path)
      (string_of_optional optional)
      (string_of_description description)
  in
  Base.List.map ~f:documentation_of_param_info %> String.concat "\n"

let func_details ~jsdoc ~exact_by_default params rest_param return =
  let documentation_of_param name_opt =
    let open Base.Option.Let_syntax in
    let%bind name = name_opt in
    let%bind jsdoc = jsdoc in
    let%map param_infos = Base.List.Assoc.find ~equal:String.equal (Jsdoc.params jsdoc) name in
    documentation_of_param_infos name param_infos
  in
  let param_tys =
    Base.List.map
      ~f:(fun (n, t, fp) ->
        let param_name = parameter_name fp.Ty.prm_optional n in
        let param_ty = string_of_ty ~exact_by_default t in
        let param_documentation = documentation_of_param n in
        { ServerProt.Response.param_name; param_ty; param_documentation })
      params
  in
  let param_tys =
    match rest_param with
    | None -> param_tys
    | Some (name, t) ->
      let rest =
        (* show the rest param's docs for all of the expanded params *)
        let param_documentation = documentation_of_param name in
        match t with
        | Ty.Tup ts ->
          Base.List.mapi
            ~f:(fun i t ->
              let param_name = Printf.sprintf "%s[%d]" (Base.Option.value name ~default:"arg") i in
              let param_ty = string_of_ty ~exact_by_default t in
              { ServerProt.Response.param_name; param_ty; param_documentation })
            ts
        | _ ->
          let param_name = "..." ^ parameter_name false name in
          let param_ty = string_of_ty ~exact_by_default t in
          [{ ServerProt.Response.param_name; param_ty; param_documentation }]
      in
      param_tys @ rest
  in
  let return_ty = string_of_ty ~exact_by_default return in
  let func_documentation = Base.Option.bind jsdoc ~f:Find_documentation.documentation_of_jsdoc in
  { ServerProt.Response.param_tys; return_ty; func_documentation }

(* given a Loc.t within a function call, returns the type of the function being called *)
module Callee_finder = struct
  type t = {
    tparams: (ALoc.t * string) list;
    type_: Type.t;
    active_parameter: int;
    loc: Loc.t;
  }

  exception Found of t option

  (* find the argument whose Loc contains `loc`, or the first one past it.
     the latter covers the `f(x,| y)` case, where your cursor is after the
     comma and before the space; this is not contained in `x` nor `y`, but should
     select `y`. *)
  let rec find_argument ~reader cursor arguments i =
    match arguments with
    | [] -> i
    | Flow_ast.Expression.(Expression ((arg_loc, _), _) | Spread (arg_loc, _)) :: rest ->
      let arg_loc = loc_of_aloc ~reader arg_loc in
      (* if the cursor is within this arg, we obviously found it. if it's immediately after,
         then we're between the arg and the comma/closing paren. if it's before the arg,
         but hasn't been found by earlier args, then we must be in the whitespace before
         the current arg, so we found it. *)
      if
        Reason.in_range cursor arg_loc
        || cursor.Loc.start = arg_loc.Loc._end
        || Loc.compare cursor arg_loc < 0
      then
        i
      else
        find_argument ~reader cursor rest (i + 1)

  class finder ~(reader : State_reader.t) (cursor : Loc.t) =
    object (this)
      inherit Typed_ast_utils.type_parameter_mapper as super

      method covers_target loc = Reason.in_range cursor (loc_of_aloc ~reader loc)

      (* only recurse if this loc covers the target. *)
      method short_circuit : 'a. ALoc.t -> 'a -> ('a -> 'a) -> 'a =
        fun loc x f ->
          if this#covers_target loc then
            f x
          else
            x

      method! statement stmt =
        let (loc, _) = stmt in
        this#short_circuit loc stmt super#statement

      method! expression expr =
        let ((loc, _), _) = expr in
        this#short_circuit loc expr super#expression

      method! call annot expr =
        let { Flow_ast.Expression.Call.callee = ((callee_loc, t), _); arguments; _ } = expr in
        let (args_loc, { Flow_ast.Expression.ArgList.arguments; comments = _ }) = arguments in
        let inside_loc =
          (* exclude the parens *)
          let args_loc = loc_of_aloc ~reader args_loc in
          let start = args_loc.Loc.start in
          let start = { start with Loc.column = start.Loc.column + 1 } in
          ALoc.of_loc { args_loc with Loc.start }
        in
        if this#covers_target inside_loc then
          (* recurse to see if we find a nested call that's narrower. it will raise if so,
             so after this line we know we found the right call. *)
          let _ = super#call annot expr in

          let active_parameter = find_argument ~reader cursor arguments 0 in
          let loc = loc_of_aloc ~reader callee_loc in
          this#annot_with_tparams (fun tparams ->
              raise (Found (Some { tparams; type_ = t; active_parameter; loc })))
        else
          super#call annot expr

      method! class_body body =
        let _ = super#class_body body in
        let (loc, _) = body in
        if this#covers_target loc then
          raise (Found None)
        else
          body

      method! function_body body =
        let _ = super#function_body body in
        match body with
        | Flow_ast.Function.BodyBlock (loc, _) when this#covers_target loc -> raise (Found None)
        | _ -> body
    end

  let find_opt ~reader ~typed_ast loc =
    let finder = new finder ~reader loc in
    try
      let _ = finder#program typed_ast in
      None
    with
    | Found (Some { tparams; type_; active_parameter; loc }) ->
      Some ({ Type.TypeScheme.tparams; type_ }, active_parameter, loc)
    | Found None -> None
end

let ty_normalizer_options =
  Ty_normalizer_env.
    {
      fall_through_merged = true;
      expand_internal_types = true;
      expand_type_aliases = false;
      flag_shadowed_type_params = true;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = true;
      optimize_types = true;
      omit_targ_defaults = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 50;
    }

let rec collect_functions ~jsdoc ~exact_by_default acc = function
  | Ty.Fun { Ty.fun_params; fun_rest_param; fun_return; _ } ->
    let details = func_details ~jsdoc ~exact_by_default fun_params fun_rest_param fun_return in
    details :: acc
  | Ty.Inter (t1, t2, ts) ->
    Base.List.fold_left ~init:acc ~f:(collect_functions ~jsdoc ~exact_by_default) (t1 :: t2 :: ts)
  | _ -> acc

let find_signatures ~options ~reader ~cx ~file_sig ~typed_ast loc =
  match Callee_finder.find_opt ~reader ~typed_ast loc with
  | Some (scheme, active_parameter, callee_loc) ->
    let ty =
      Ty_normalizer.from_scheme
        ~options:ty_normalizer_options
        ~genv:(Ty_normalizer_env.mk_genv ~full_cx:cx ~file:(Context.file cx) ~typed_ast ~file_sig)
        scheme
    in
    let jsdoc =
      let open GetDef_js.Get_def_result in
      match GetDef_js.get_def ~options ~reader ~cx ~file_sig ~typed_ast callee_loc with
      | Def getdef_loc
      | Partial (getdef_loc, _) ->
        Find_documentation.jsdoc_of_getdef_loc ~current_ast:typed_ast ~reader getdef_loc
      | Bad_loc
      | Def_error _ ->
        None
    in
    (match ty with
    | Ok (Ty.Type ty) ->
      let exact_by_default = Context.exact_by_default cx in
      (match collect_functions ~jsdoc ~exact_by_default [] ty with
      | [] -> Ok None
      | funs -> Ok (Some (funs, active_parameter)))
    | Ok _ -> Ok None
    | Error err -> Error err)
  | None -> Ok None
