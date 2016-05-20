module Anno = Type_annotation
module Ast = Spider_monkey_ast
module Env = Env_js
module Flow = Flow_js
module Sig = Classy_sig

open Reason_js

let rec extract_extends cx allow_multiple = function
  | [] -> [None, None]
  | [_, {Ast.Type.Generic.id; typeParameters}] ->
      [Some id, typeParameters]
  | (loc, {Ast.Type.Generic.id; typeParameters})::others ->
      let extract = extract_extends cx allow_multiple in
      if allow_multiple
      then (Some id, typeParameters)::(extract others)
      else
        let msg = "A class cannot extend multiple classes!" in
        Flow_error.add_error cx (loc, [msg]);
        []

let extract_body class_ast =
  let { Ast.Statement.Interface.body; _ } = class_ast in
  snd body

let intersect_supers reason = Type.(function
  | [] -> AnyT.t
  | [t] -> t
  | ts -> IntersectionT (prefix_reason "super of " reason, InterRep.make ts)
)

let add_property fn_func cx tparams_map iface_sig property = Ast.Type.Object.(
  let (loc, { Property.key; value; static; _method; optional }) = property in
  if optional && _method
  then begin
    let msg = "optional methods are not supported" in
    Flow_error.add_error cx (loc, [msg])
  end;
  Ast.Expression.Object.(match _method, key with
  | _, Property.Literal (loc, _)
  | _, Property.Computed (loc, _) ->
      let msg = "illegal name" in
      Flow_error.add_error cx (loc, [msg]);
      iface_sig
  | true, Property.Identifier (_, {Ast.Identifier.name; _}) ->
      Ast.Type.(match value with
      | _, Function func ->
        let fsig = fn_func cx tparams_map static loc func in
        let append_method = match static, name with
        | false, "constructor" -> Sig.append_constructor
        | _ -> Sig.append_method ~static name
        in
        append_method fsig iface_sig
      | _ ->
        let msg = "internal error: expected function type" in
        Flow_error.add_internal_error cx (loc, [msg]);
        iface_sig)
  | false, Property.Identifier (_, {Ast.Identifier.name; _}) ->
      let t = Anno.convert cx tparams_map value in
      let t = if optional then Type.OptionalT t else t in
      Sig.add_field ~static name (t, None) iface_sig)
)

let add_indexers cx tparams_map indexers iface_sig =
  match indexers with
  | [] -> iface_sig
  | (_, {Ast.Type.Object.Indexer.key; value; static; _})::rest ->
    (* TODO? *)
    List.iter (fun (indexer_loc, _) ->
      let msg = "multiple indexers are not supported" in
      Flow_error.add_error cx (indexer_loc, [msg]);
    ) rest;
    let k = Anno.convert cx tparams_map key in
    let v = Anno.convert cx tparams_map value in
    iface_sig
      |> Sig.add_field ~static "$key" (k, None)
      |> Sig.add_field ~static "$value" (v, None)

let add_call_property cx tparams_map iface_sig call_property =
  let loc, {Ast.Type.Object.CallProperty.value = (_, func); static} =
    call_property in
  let fsig = Func_sig.convert cx tparams_map loc func in
  Sig.append_method ~static "$call" fsig iface_sig

let add_default_constructor loc iface_sig =
  let { Sig.constructor; _ } = iface_sig in
  if constructor <> []
  then iface_sig
  else
    let reason = mk_reason "constructor" loc in
    Sig.add_default_constructor reason iface_sig

module T = struct
  type classy_ast_t = Ast.Statement.Interface.t

  let ct_check_polarity = false
  let structural = true
  let mk_class i = Type.ClassT i
  let remove_this x = x (* there exists no `this`, so remove nothing *)
  let subst_sig = Class_sig.subst_sig

  let mk_type_param_declarations cx tparams_map _ _ class_ast =
    let { Ast.Statement.Interface.typeParameters; _ } = class_ast in
    Anno.mk_type_param_declarations cx tparams_map typeParameters

  let mk_extends_type cx tparams_map reason (ext, targs) =
    let lookup_mode = Env.LookupMode.ForType in
    let super_reason = prefix_reason "super of " reason in
    let id = Anno.convert_qualification ~lookup_mode cx "extends" ext in
    let params = Anno.extract_type_param_instantiations targs in
    Anno.mk_nominal_type cx super_reason tparams_map (id, params)

  let mk_super cx tparams_map _ reason class_ast =
    let { Ast.Statement.Interface.extends; _ } = class_ast in
    let super_ts =
      let typify = Class_sig.mk_extends mk_extends_type cx tparams_map reason in
      List.map typify (extract_extends cx true extends)
    in
    intersect_supers reason super_ts

  let preliminary_checks _ _ _ = ()
  let implicit_body reason _ = Sig.add_name reason

  let explicit_body cx tparams_map loc (class_ast:Ast.Statement.Interface.t) iface_sig =
    let mk_function_property cx tparams_map _ loc func =
      Func_sig.convert cx tparams_map loc func in
    let { Ast.Type.Object.properties; indexers; callProperties } =
      extract_body class_ast in
    let add_properties = add_property mk_function_property cx tparams_map in
    let add_call_properties = add_call_property cx tparams_map in
    iface_sig
      |> Sig.fold_pipe add_properties properties
      |> add_indexers cx tparams_map indexers
      |> Sig.fold_pipe add_call_properties callProperties
      |> add_default_constructor loc
end
