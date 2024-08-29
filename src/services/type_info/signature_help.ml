(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

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
  | Ty.TypeGuard (impl, x, t) ->
    let impl =
      if impl then
        "implies "
      else
        ""
    in
    let t = Ty_printer.string_of_t_single_line ~exact_by_default ~with_comments:false t in
    Utils_js.spf "%s%s is %s" impl x t

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
          | Ty.Tup { elements; inexact = _ } ->
            Base.List.fold
              elements
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
  ServerProt.Response.SigHelpFunc { param_tys; return_ty; func_documentation }

(* given a Loc.t within a function call or `new` expression, returns the type of the function/constructor being called *)
module Callee_finder = struct
  type t =
    | FunCallData of {
        type_: Type.t;
        active_parameter: int;
        loc: ALoc.t;
      }
    | JsxAttrData of {
        type_: Type.t;
        name: string;
        loc: ALoc.t;
        key_loc: ALoc.t;
      }

  exception Found of t option

  (* Ty_normalizer_flow will attempt to recover an alias name for this type. Given that
   * in collect_functions we try to match against the structure of the type, we
   * would rather bypass the alias on the toplevel of the type. It is still
   * desirable that deeper within the type aliases are maintained. Note that
   * alternatively we could updated the normalizer to perform a one-off expansion
   * of the toplevel alias, but that would be more complex that fixing this here. *)
  let fix_reason_of_t = TypeUtil.mod_reason_of_t Reason.(update_desc_reason invalidate_rtype_alias)

  let simplify_fun_t cx func_t =
    let open Type in
    let reason = TypeUtil.reason_of_t func_t in
    Tvar.mk_no_wrap_where cx reason (fun t ->
        let u =
          CallT
            {
              use_op = unknown_use;
              reason;
              call_action = ConcretizeCallee t;
              return_hint = hint_unavailable;
            }
        in
        Flow_js.flow cx (func_t, u)
    )

  let rec get_func cx reason t =
    (* Lets us coerce things like ClassT to function types if possible. *)
    let t = simplify_fun_t cx t in
    match Flow_js.possible_concrete_types_for_inspection cx reason t with
    | [] -> []
    | t :: _ ->
      (* NOTE since we're not merging unions of signatures to a single one,
       * keep the first member. This shouldn't be common. *)
      get_func_no_union cx (TypeUtil.reason_of_t t) t

  and get_func_no_union cx reason t =
    let open Type in
    match t with
    | GenericT { bound = t; _ } -> get_func cx reason t
    | DefT (_, FunT _)
    | DefT (_, PolyT { t_out = DefT (_, FunT _); _ }) ->
      [fix_reason_of_t t]
    | IntersectionT (_, rep) -> InterRep.members rep |> List.concat_map (get_func cx reason)
    | _ -> []

  let rec get_prop_of_obj cx name reason t =
    Flow_js.possible_concrete_types_for_inspection cx reason t
    |> Base.List.filter_map ~f:(fun t -> get_prop_of_obj_no_union cx name (TypeUtil.reason_of_t t) t)
    |> function
    | [] -> None
    | [t] -> Some t
    | ts ->
      let opt = Base.List.map ~f:snd ts |> Base.List.fold_left ~init:false ~f:( || ) in
      let ts = Base.List.map ~f:fst ts in
      Some (TypeUtil.union_of_ts reason ts, opt)

  and get_prop_of_obj_no_union cx name reason t =
    let open Type in
    match t with
    | GenericT { bound = t; _ } -> get_prop_of_obj cx name reason t
    | DefT (_, ObjT { props_tmap; _ }) ->
      Base.Option.value_map (Context.get_prop cx props_tmap name) ~default:None ~f:(fun prop ->
          match Property.read_t prop with
          | Some (OptionalT { type_ = t; _ }) -> Some (t, true)
          | Some t -> Some (t, false)
          | None -> None
      )
    | IntersectionT (_, rep) ->
      let (ts_rev, opt) =
        Base.List.fold_left (InterRep.members rep) ~init:([], false) ~f:(fun (ts, opt) t ->
            match get_prop_of_obj cx name reason t with
            | Some (t, opt') -> (t :: ts, opt || opt')
            | None -> (ts, opt)
        )
      in
      begin
        match List.rev ts_rev with
        | [] -> None
        | [t] -> Some (t, opt)
        | t0 :: t1 :: ts ->
          let r = Reason.(locationless_reason (RCustom "intersection")) in
          Some (IntersectionT (r, InterRep.make t0 t1 ts), opt)
      end
    | _ -> None

  let get_prop_of_obj_toplevel cx name reason t =
    Flow_js.possible_concrete_types_for_inspection cx reason t
    |> Base.List.filter_map ~f:(fun t -> get_prop_of_obj_no_union cx name (TypeUtil.reason_of_t t) t)

  let get_attribute_type cx loc t name =
    let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementConfig")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type_ = reason }) in
    let id = Type.Eval.generate_id () in
    let conf = Flow_js.mk_type_destructor cx use_op reason t Type.ReactElementConfigType id in
    get_prop_of_obj_toplevel cx name reason conf

  (* find the argument whose Loc contains `loc`, or the first one past it.
     the latter covers the `f(x,| y)` case, where your cursor is after the
     comma and before the space; this is not contained in `x` nor `y`, but should
     select `y`. *)
  let rec find_argument ~loc_of_aloc cursor arguments i =
    match arguments with
    | [] -> i
    | Flow_ast.Expression.(Expression ((arg_loc, _), _) | Spread (arg_loc, _)) :: rest ->
      let arg_loc = loc_of_aloc arg_loc in
      (* if the cursor is within this arg, we obviously found it. if it's before the arg,
         but hasn't been found by earlier args, then we must be in the whitespace before
         the current arg, so we found it. *)
      if Reason.in_range cursor arg_loc || Loc.compare cursor arg_loc < 0 then
        i
      else
        find_argument ~loc_of_aloc cursor rest (i + 1)

  class finder ~loc_of_aloc ~(cx : Context.t) (cursor : Loc.t) =
    object (this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot x = x

      method on_type_annot x = x

      method covers_target loc = Reason.in_range cursor (loc_of_aloc loc)

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
            let args_loc = loc_of_aloc args_loc in
            let start = args_loc.Loc.start in
            let start = { start with Loc.column = start.Loc.column + 1 } in
            ALoc.of_loc { args_loc with Loc.start }
          in
          if this#covers_target inside_loc then
            (* recurse to see if we find a nested call that's narrower. it will raise if so,
               so after this line we know we found the right call. *)
            let _ = recurse () in

            let active_parameter = find_argument ~loc_of_aloc cursor arguments 0 in
            let type_ = get_callee_type () in
            raise (Found (Some (FunCallData { type_; active_parameter; loc = callee_loc })))
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

      method! new_ annot expr =
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
          this#find (fun () -> super#new_ annot expr) get_callee_type arguments callee_loc
        | None -> super#new_ annot expr

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

      method! jsx_element ((loc, _) as annot) elt =
        let open Flow_ast.JSX in
        let elt = super#jsx_element annot elt in
        let (_, { Opening.name = elt_name; attributes; _ }) = elt.opening_element in
        Base.List.iter attributes ~f:(function
            | Flow_ast.JSX.Opening.SpreadAttribute _ -> ()
            | Flow_ast.JSX.Opening.Attribute attr ->
              let (_, { Flow_ast.JSX.Attribute.name = attr_name; value }) = attr in
              Base.Option.iter value ~f:(function
                  | Flow_ast.JSX.Attribute.StringLiteral ((attr_loc, _), _)
                  | Flow_ast.JSX.Attribute.ExpressionContainer ((attr_loc, _), _)
                  ->
                  if this#covers_target attr_loc then (
                    match (elt_name, attr_name) with
                    | ( Identifier ((_, t), _),
                        Attribute.Identifier ((key_loc, _), { Identifier.name; _ })
                      ) ->
                      raise (Found (Some (JsxAttrData { type_ = t; name; loc; key_loc })))
                    | _ -> raise (Found None)
                  )
                  )
            );
        elt
    end

  let find_opt ~loc_of_aloc ~cx ~typed_ast loc =
    let finder = new finder ~loc_of_aloc ~cx loc in
    match finder#program typed_ast with
    | exception Found (Some data) -> Some data
    | exception Found None -> None
    | _ -> None
end

let find_signatures ~loc_of_aloc ~get_ast_from_shared_mem ~cx ~file_sig ~ast ~typed_ast loc =
  match Callee_finder.find_opt ~loc_of_aloc ~cx ~typed_ast loc with
  | Some (Callee_finder.FunCallData { type_ = t; active_parameter; loc = callee_loc }) ->
    let ts = Callee_finder.get_func cx (TypeUtil.reason_of_t t) t in
    let norm_options = Ty_normalizer_env.default_options in
    let genv =
      Ty_normalizer_flow.mk_genv ~options:norm_options ~cx ~typed_ast_opt:(Some typed_ast) ~file_sig
    in
    let func_details_of_type ~jsdoc fn =
      let ty = Ty_normalizer_flow.from_type genv fn in
      match ty with
      | Ok (Ty.Type (Ty.Fun { Ty.fun_params; fun_rest_param; fun_return; _ })) ->
        let exact_by_default = Context.exact_by_default cx in
        Some (func_details ~jsdoc ~exact_by_default fun_params fun_rest_param fun_return)
      | _ -> None
    in
    let funs =
      match ts with
      | [] -> []
      | [t] ->
        (* non-overloaded case: get-def returns reasonable JSDoc results *)
        let jsdoc =
          match
            GetDef_js.get_def
              ~loc_of_aloc
              ~cx
              ~file_sig
              ~ast
              ~available_ast:(Typed_ast_utils.Typed_ast typed_ast)
              ~purpose:Get_def_types.Purpose.JSDoc
              (loc_of_aloc callee_loc)
          with
          | GetDef_js.Get_def_result.Def (locs, _)
          | GetDef_js.Get_def_result.Partial (locs, _, _)
            when LocSet.cardinal locs = 1 ->
            let getdef_loc = LocSet.choose locs in
            Find_documentation.jsdoc_of_getdef_loc ~ast ~get_ast_from_shared_mem getdef_loc
          | _ -> None
        in
        func_details_of_type ~jsdoc t |> Base.Option.to_list
      | ts ->
        (* overloaded case: get-def does not distinguish between overloads, get the
         * JSDoc from the def_loc of the specific overload instead. *)
        Base.List.filter_map ts ~f:(fun fn ->
            let jsdoc =
              let loc = loc_of_aloc (TypeUtil.def_loc_of_t fn) in
              Find_documentation.jsdoc_of_getdef_loc ~ast ~get_ast_from_shared_mem loc
            in
            func_details_of_type ~jsdoc fn
        )
    in
    Ok (Some (funs, active_parameter))
  | Some (Callee_finder.JsxAttrData { type_ = t; name; loc; key_loc }) ->
    let ts =
      if name = "key" then
        (* 'key' is special-cased *)
        let reason_key = Reason.mk_reason (Reason.RCustom "React key") key_loc in
        let t = TypeUtil.maybe (Flow_js.get_builtin_type cx reason_key "React$Key") in
        [(t, true)]
      else
        Callee_finder.get_attribute_type cx loc t (Reason.OrdinaryName name)
    in
    let norm_options = Ty_normalizer_env.default_options in
    let genv =
      Ty_normalizer_flow.mk_genv ~options:norm_options ~cx ~typed_ast_opt:(Some typed_ast) ~file_sig
    in
    let tys =
      Base.List.filter_map
        ~f:(fun (t, optional) ->
          match Ty_normalizer_flow.from_type genv t with
          | Ok (Ty.Type ty) ->
            let exact_by_default = Context.exact_by_default cx in
            let ty = string_of_ty ~exact_by_default ty in
            let loc = loc_of_aloc (TypeUtil.loc_of_t t) in
            let jsdoc = Find_documentation.jsdoc_of_getdef_loc ~ast ~get_ast_from_shared_mem loc in
            let documentation = Base.Option.value_map ~default:None ~f:Jsdoc.description jsdoc in
            Some (ServerProt.Response.SigHelpJsxAttr { documentation; name; ty; optional })
          | Ok _
          | Error _ ->
            None)
        ts
    in
    Ok (Some (tys, 0))
  | None -> Ok None
