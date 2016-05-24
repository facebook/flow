module Anno = Type_annotation
module Ast = Spider_monkey_ast
module Env = Env_js
module Flow = Flow_js
module Sig = Classy_sig

open Reason_js

module T = struct
  type classy_ast_t = Ast.Statement.Interface.t

  let ct_check_polarity = false
  let structural = false
  let class_ctor c = Type.ThisClassT c
  let remove_this = Class_sig.remove_this
  let subst_sig = Class_sig.subst_sig

  let mk_type_param_declarations cx tparams_map reason self class_ast =
    let { Ast.Statement.Interface.typeParameters = tps; _ } = class_ast in
    Class_sig.mk_type_param_declarations cx tparams_map reason self tps

  let mk_super cx tparams_map _ reason class_sig =
    let extract_mixin (_, {Ast.Type.Generic.id; typeParameters}) =
      (Some id, typeParameters)
    in
    let mk_single_mixin cx tparams_map reason (ext, targs) =
      let lookup_mode = Env.LookupMode.ForValue in
      let super_reason = prefix_reason "super of " reason in
      let i = Flow.mk_tvar_derivable_where cx super_reason (fun tvar ->
        Flow.flow cx (
          Anno.convert_qualification ~lookup_mode cx "mixins" ext,
          Type.MixinT (super_reason, tvar)
        )
      ) in
      Class_sig.mk_super cx tparams_map i targs
    in
    let mk_single_super cx tparams_map _ (ext, targs) =
      let lookup_mode = Env.LookupMode.ForValue in
      let i = Anno.convert_qualification ~lookup_mode cx "mixins" ext in
      Class_sig.mk_super cx tparams_map i targs
    in
    let { Ast.Statement.Interface.mixins; extends; _ } = class_sig in
    let mixin_ts =
      let typify = Class_sig.mk_extends mk_single_mixin cx tparams_map reason in
      List.map typify (List.map extract_mixin mixins)
    in
    let super_ts =
      let typify = Class_sig.mk_extends mk_single_super cx tparams_map reason in
      List.map typify (Iface_sig.extract_extends cx false extends)
    in
    (* mixins override extends *)
    Iface_sig.intersect_supers reason (mixin_ts @ super_ts)

  let preliminary_checks _ _ _ = ()
  let implicit_body reason _ = Sig.add_name reason

  let explicit_body cx tparams_map loc class_ast class_sig =
    let { Ast.Type.Object.properties; indexers; callProperties } =
      Iface_sig.extract_body class_ast in
    let add_properties = Iface_sig.add_property cx tparams_map in
    let add_call_properties = Iface_sig.add_call_property cx tparams_map in
    class_sig
      |> Sig.fold_pipe add_properties properties
      |> Iface_sig.add_indexers cx tparams_map indexers
      |> Sig.fold_pipe add_call_properties callProperties
      |> Iface_sig.add_default_constructor loc
end
