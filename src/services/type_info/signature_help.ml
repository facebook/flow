(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

let loc_of_aloc = Parsing_heaps.Reader.loc_of_aloc

let parameter_name is_opt name =
  let opt =
    if is_opt then
      "?"
    else
      ""
  in
  Base.Option.value name ~default:"_" ^ opt

let string_of_ty = Ty_printer.string_of_t_single_line ~with_comments:false

let string_of_return_t ~exact_by_default = function
  | Ty.ReturnType t -> Ty_printer.string_of_t_single_line ~exact_by_default ~with_comments:false t
  | Ty.TypeGuard (x, t) ->
    x ^ " is " ^ Ty_printer.string_of_t_single_line ~exact_by_default ~with_comments:false t

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
    | Some (rest_param_name, t) ->
      let rest =
        (* show the rest param's docs for all of the expanded params *)
        let param_documentation = documentation_of_param rest_param_name in
        (* Only show the result if there aren't any spreads, as in that case we
           don't know which param index corresponds to which tuple element. *)
        let els_result =
          match t with
          | Ty.Tup els ->
            Base.List.fold
              els
              ~init:(Some (0, []))
              ~f:(fun acc el ->
                Base.Option.bind acc ~f:(fun (i, els) ->
                    match el with
                    | Ty.TupleElement { name; t; polarity = _; optional = _ } ->
                      let param_name =
                        match name with
                        | Some name -> name
                        | None ->
                          Printf.sprintf
                            "%s[%d]"
                            (Base.Option.value rest_param_name ~default:"arg")
                            i
                      in
                      Some (i + 1, (param_name, t) :: els)
                    | Ty.TupleSpread _ -> None
                ))
          | _ -> None
        in
        match els_result with
        | Some (_, els) ->
          Base.List.rev els
          |> Base.List.map ~f:(fun (param_name, t) ->
                 let param_ty = string_of_ty ~exact_by_default t in
                 { ServerProt.Response.param_name; param_ty; param_documentation }
             )
        | None ->
          let param_name = "..." ^ parameter_name false rest_param_name in
          let param_ty = string_of_ty ~exact_by_default t in
          [{ ServerProt.Response.param_name; param_ty; param_documentation }]
      in
      param_tys @ rest
  in
  let return_ty = string_of_return_t ~exact_by_default return in
  let func_documentation = Base.Option.bind jsdoc ~f:Find_documentation.documentation_of_jsdoc in
  { ServerProt.Response.param_tys; return_ty; func_documentation }

(* given a Loc.t within a function call or `new` expression, returns the type of the function/constructor being called *)
module Callee_finder = struct
  type t = {
    tparams_rev: Type.typeparam list;
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
      (* if the cursor is within this arg, we obviously found it. if it's before the arg,
         but hasn't been found by earlier args, then we must be in the whitespace before
         the current arg, so we found it. *)
      if Reason.in_range cursor arg_loc || Loc.compare cursor arg_loc < 0 then
        i
      else
        find_argument ~reader cursor rest (i + 1)

  class finder ~(reader : State_reader.t) ~(cx : Context.t) (cursor : Loc.t) =
    object (this)
      inherit Typed_ast_finder.type_parameter_mapper as super

      method covers_target loc = Reason.in_range cursor (loc_of_aloc ~reader loc)

      method find
          : 'a.
            (unit -> 'a) ->
            (unit -> Type.t) ->
            (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.ArgList.t ->
            ALoc.t ->
            'a =
        fun recurse get_callee_type arguments callee_loc ->
          let (args_loc, { Flow_ast.Expression.ArgList.arguments; _ }) = arguments in
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
            let _ = recurse () in

            let active_parameter = find_argument ~reader cursor arguments 0 in
            let loc = loc_of_aloc ~reader callee_loc in
            let type_ = get_callee_type () in
            this#annot_with_tparams (fun ~tparams_rev ->
                raise (Found (Some { tparams_rev; type_; active_parameter; loc }))
            )
          else
            recurse ()

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
        let { Flow_ast.Expression.Call.callee = ((callee_loc, t), callee_expr); arguments; _ } =
          expr
        in
        let t = Base.Option.value ~default:t (Context.get_signature_help_callee cx callee_loc) in
        let callee_loc =
          let open Flow_ast.Expression in
          let open Member in
          match callee_expr with
          | Member { property = PropertyIdentifier ((loc, _), _); _ }
          | Member { property = PropertyPrivateName (loc, _); _ }
          | Member { property = PropertyExpression ((loc, _), _); _ } ->
            loc
          | _ -> callee_loc
        in
        this#find (fun () -> super#call annot expr) (fun () -> t) arguments callee_loc

      method! new_ expr =
        let { Flow_ast.Expression.New.callee = ((callee_loc, class_t), _); arguments; _ } = expr in
        match arguments with
        | Some arguments ->
          let get_callee_type () =
            let open Reason in
            let reason = mk_reason (RCustom "signature help get constructor") callee_loc in
            Tvar.mk_where cx reason (fun t_out ->
                let open Type in
                let instance =
                  Tvar.mk_where cx reason (fun instance ->
                      Flow_js.flow_t cx (class_t, DefT (reason, ClassT instance))
                  )
                in
                let propref = TypeUtil.mk_named_prop ~reason (OrdinaryName "constructor") in
                let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction t_out) in
                Flow_js.flow cx (instance, use_t)
            )
          in
          this#find (fun () -> super#new_ expr) get_callee_type arguments callee_loc
        | None -> super#new_ expr

      method! class_body body =
        let _ = super#class_body body in
        let (loc, _) = body in
        if this#covers_target loc then
          raise (Found None)
        else
          body

      method! function_body_any body =
        let _ = super#function_body_any body in
        match body with
        | Flow_ast.Function.BodyBlock (loc, _) when this#covers_target loc -> raise (Found None)
        | _ -> body
    end

  let find_opt ~reader ~cx ~typed_ast loc =
    let finder = new finder ~reader ~cx loc in
    try
      let _ = finder#program typed_ast in
      None
    with
    | Found (Some { tparams_rev; type_; active_parameter; loc }) ->
      Some ({ Type.TypeScheme.tparams_rev; type_ }, active_parameter, loc)
    | Found None -> None
end

let ty_normalizer_options = Ty_normalizer_env.default_options

let rec collect_functions ~jsdoc ~exact_by_default acc = function
  | Ty.Fun { Ty.fun_params; fun_rest_param; fun_return; _ } ->
    let details = func_details ~jsdoc ~exact_by_default fun_params fun_rest_param fun_return in
    details :: acc
  | Ty.Inter (t1, t2, ts) ->
    Base.List.fold_left ~init:acc ~f:(collect_functions ~jsdoc ~exact_by_default) (t1 :: t2 :: ts)
  | _ -> acc

(* Ty_normalizer_flow will attempt to recover an alias name for this type. Given that
 * in collect_functions we try to match against the structure of the type, we
 * would rather bypass the alias on the toplevel of the type. It is still
 * desirable that deeper within the type aliases are maintained. Note that
 * alternatively we could updated the normalizer to perform a one-off expansion
 * of the toplevel alias, but that would be more complex that fixing this here. *)
let rec fix_alias_reason cx t =
  let open Type in
  let t = Flow_js.singleton_concrete_type_for_inspection cx (TypeUtil.reason_of_t t) t in
  let t' = TypeUtil.mod_reason_of_t Reason.(update_desc_reason invalidate_rtype_alias) t in
  match t' with
  | IntersectionT (r, rep) ->
    let (t0, (t1, ts)) = InterRep.members_nel rep in
    let t0 = fix_alias_reason cx t0 in
    let t1 = fix_alias_reason cx t1 in
    let ts = Base.List.map ~f:(fix_alias_reason cx) ts in
    IntersectionT (r, InterRep.make t0 t1 ts)
  | _ -> t'

let find_signatures ~options ~reader ~cx ~file_sig ~ast ~typed_ast loc =
  match Callee_finder.find_opt ~reader ~cx ~typed_ast loc with
  | Some (scheme, active_parameter, callee_loc) ->
    let { Type.TypeScheme.type_ = t; _ } = scheme in
    let t' = fix_alias_reason cx t in
    let scheme = { scheme with Type.TypeScheme.type_ = t' } in
    let genv =
      Ty_normalizer_env.mk_genv
        ~cx
        ~file:(Context.file cx)
        ~typed_ast_opt:(Some typed_ast)
        ~file_sig
    in
    let ty = Ty_normalizer_flow.from_scheme ~options:ty_normalizer_options ~genv scheme in
    let jsdoc =
      match
        GetDef_js.get_def
          ~options
          ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
          ~cx
          ~file_sig
          ~ast
          ~typed_ast
          ~purpose:Get_def_types.Purpose.GoToDefinition
          callee_loc
      with
      | GetDef_js.Get_def_result.Def (locs, _)
      | GetDef_js.Get_def_result.Partial (locs, _, _)
        when LocSet.cardinal locs = 1 ->
        let getdef_loc = LocSet.choose locs in
        Find_documentation.jsdoc_of_getdef_loc ~ast ~reader getdef_loc
      | _ -> None
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
