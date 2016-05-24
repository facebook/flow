module Ast = Spider_monkey_ast
module Flow = Flow_js
module Sig = Classy_sig

open Reason_js

type self_t = Type.t
type tparams_t = Type.typeparam list
type tparams_map_t = Type.t SMap.t
type expr_fn_t = Context.t -> tparams_map_t -> Ast.Expression.t -> Type.t
type stmt_fn_t = Context.t -> tparams_map_t -> Ast.Statement.t list -> unit

module type ClassyType =
  sig
    type classy_ast_t

    (* TODO: Ideally we should check polarity for all class types, but this flag
       is flipped off for interface/declare class currently. *)
    val ct_check_polarity: bool
    val structural: bool
    val class_ctor: Type.t -> Type.t
    val remove_this_tparam: Sig.t -> Sig.t
    val subst: Context.t -> tparams_map_t -> Sig.signature -> Sig.signature
    val mk_type_param_declarations: Context.t -> tparams_map_t -> reason ->
      self_t -> classy_ast_t -> tparams_t * tparams_map_t
    val mk_super: Context.t -> tparams_map_t -> expr_fn_t -> reason ->
      classy_ast_t -> Type.t
    val preliminary_checks: Context.t -> Loc.t -> classy_ast_t -> unit
    val implicit_body: reason -> classy_ast_t -> Sig.t -> Sig.t
    val explicit_body: Context.t -> tparams_map_t -> Loc.t -> classy_ast_t ->
      Sig.t -> Sig.t
  end

module Make(Classy : ClassyType) = struct
  type classy_ast_t = Classy.classy_ast_t

  (* Process a class definition, returning a (polymorphic) class type. A class
     type is a wrapper around an instance type, which contains types of instance
     members, a pointer to the super instance type, and a container for types of
     static members. The static members can be thought of as instance members of
     a "metaclass": thus, the static type is itself implemented as an instance
     type. *)
  let mk_sig cx tparams_map expr loc reason self class_ast =
    Classy.preliminary_checks cx loc class_ast;
    let tparams, tparams_map =
      Classy.mk_type_param_declarations cx tparams_map reason self class_ast in
    let super = Classy.mk_super cx tparams_map expr reason class_ast in
    (Sig.empty (Flow.mk_nominal cx) reason tparams tparams_map super)
      |> (Classy.implicit_body reason class_ast)
      |> (Classy.explicit_body cx tparams_map loc class_ast)

  let insttype ~static cx s =
    let open Sig in
    let arg_polarities x =
      List.fold_left Type.(fun acc tp ->
        SMap.add tp.name tp.polarity acc
      ) SMap.empty x.tparams
    in
    let elements cx ?constructor = with_sig (fun s ->
      let methods =
        (* If this is an overloaded method, create an intersection, attributed
           to the first declared function signature. If there is a single
           function signature for this method, simply return the method type. *)
        SMap.map Type.(fun xs ->
          match List.rev_map Func_sig.methodtype xs with
          | [] -> EmptyT.t
          | [t] -> t
          | t::_ as ts -> IntersectionT (reason_of_t t, InterRep.make ts)
        ) s.methods
      in

      (* Re-add the constructor as a method. *)
      let methods = match constructor with
      | Some t -> SMap.add "constructor" t methods
      | None -> methods
      in

      (* If there is a both a getter and a setter, then flow the setter type to
         the getter. Otherwise just use the getter type or the setter type *)
      let getters = SMap.map Func_sig.gettertype s.getters in
      let setters = SMap.map Func_sig.settertype s.setters in
      let getters_and_setters = SMap.fold (fun name t ts ->
        match SMap.get name ts with
        | Some t' -> Flow.unify cx t t'; ts
        | None -> SMap.add name t ts
      ) setters getters in

      (* Treat getters and setters as fields *)
      let fields = SMap.map fst s.fields in
      let fields = SMap.union getters_and_setters fields in

      fields, methods
    )
    in
    let class_id = if static then 0 else s.id in
    let constructor = if static then None else
      let ts = List.rev_map Func_sig.methodtype s.constructor in
      match ts with
      | [] -> None
      | [t] -> Some t
      | t::_ as ts ->
        let open Type in
        let t = IntersectionT (reason_of_t t, InterRep.make ts) in
        Some t
    in
    let fields, methods = elements ?constructor ~static cx s in
    { Type.
      class_id;
      type_args = s.tparams_map;
      arg_polarities = arg_polarities s;
      fields_tmap = Flow.mk_propmap cx fields;
      methods_tmap = Flow.mk_propmap cx methods;
      mixins = false;
      structural = Classy.structural;
    }

  let classtype cx x =
    let x = Classy.remove_this_tparam x in
    let { Sig.
      tparams;
      static = { Sig.reason = sreason; super = ssuper; _ };
      instance = { Sig.reason; super; _ };
      _;
    } = x in
    let open Type in
    let sinsttype, insttype = Sig.mutually (insttype cx x) in
    let static = InstanceT (sreason, MixedT.t, ssuper, sinsttype) in
    let this = InstanceT (reason, static, super, insttype) in
    (if Classy.ct_check_polarity then Flow.check_polarity cx Positive this);
    let t = Classy.class_ctor this in
    if tparams = [] then t else PolyT (tparams, t)

  let generate_tests cx f x =
    let open Sig in
    let {tparams; tparams_map; constructor; static; instance; _} = x in
    Flow.generate_tests cx instance.reason tparams (fun map -> f {
      x with
      tparams_map = SMap.map (Flow.subst cx map) tparams_map;
      constructor = List.map (Func_sig.subst cx map) constructor;
      static = Classy.subst cx map static;
      instance = Classy.subst cx map instance;
    })
  let check_super cx x =
    let open Sig in
    let x = Classy.remove_this_tparam x in
    let reason = x.instance.reason in
    mutually (fun ~static ->
      let super = with_sig ~static (fun s -> s.super) x in
      let insttype = insttype ~static cx x in
      Flow.flow cx (super, Type.SuperT (reason, insttype))
    ) |> ignore
end

module Class = Make(Class_sig.T)
module Interface = Make(Iface_sig.T)
module DeclClass = Make(Decl_class_sig.T)

(* Processes the bodies of instance and static class members. *)
let toplevels cx ~decls ~stmts ~expr x =
  let new_entry t = Scope.Entry.new_var ~loc:(Type.loc_of_t t) t in
  let push () =
    Abnormal.clear_saved Abnormal.Return,
    Abnormal.clear_saved Abnormal.Throw in
  let pop (save_return, save_throw) =
    ignore (Abnormal.swap_saved Abnormal.Return save_return);
    ignore (Abnormal.swap_saved Abnormal.Throw save_throw) in

  let ctor this super f =
    let save = push () in
    f |> Func_sig.generate_tests cx (
      Func_sig.toplevels None cx this super ~decls ~stmts ~expr
    );
    pop save
  in

  let method_ super f =
    let this = new_entry (Func_sig.this f) in
    let super = new_entry super in
    let save = push () in
    f |> Func_sig.generate_tests cx (
      Func_sig.toplevels None cx this super ~decls ~stmts ~expr
    );
    pop save
  in

  let field config this super name (field_t, value) =
    match config, value with
    | Options.ESPROPOSAL_IGNORE, _ -> ()
    | _, None -> ()
    | _, Some ((loc, _) as xp) ->
      let init =
        let desc = Utils.spf "field initializer for `%s`" name in
        let reason = mk_reason desc loc in
        Func_sig.field_initializer x.Sig.tparams_map reason xp field_t
      in
      let this, super = new_entry this, new_entry super in
      let save = push () in
      init |> Func_sig.generate_tests cx (
        Func_sig.toplevels None cx this super ~decls ~stmts ~expr
      );
      pop save
  in

  let open Sig in

  let this = SMap.find_unsafe "this" x.tparams_map in
  let static = Type.ClassT this in

  x |> with_sig ~static:true (fun s ->
    (* process static methods and fields *)
    iter_methods (method_ s.super) s;
    let config = Context.esproposal_class_static_fields cx in
    SMap.iter (field config static s.super) s.fields
  );

  x |> with_sig ~static:false (fun s ->
    (* process constructor *)
    begin
      (* When in a derived constructor, initialize this and super to undefined.
         For internal names, we can afford to initialize with undefined and
         fatten the declared type to allow undefined. This works because we
         always look up the flow-sensitive type of internals, and don't havoc
         them. However, the same trick wouldn't work for normal uninitialized
         locals, e.g., so it cannot be used in general to track definite
         assignments. *)
      let derived_ctor = Type.(match s.super with
        | ClassT (MixedT _) -> false
        | MixedT _ -> false
        | _ -> true
      ) in
      let new_entry t =
        if derived_ctor then
          let open Type in
          let specific =
            let msg = "uninitialized this (expected super constructor call)" in
            VoidT (replace_reason msg (reason_of_t this))
          in
          Scope.Entry.new_var ~loc:(loc_of_t t) ~specific (OptionalT t)
        else
          new_entry t
      in
      let this, super = new_entry this, new_entry s.super in
      x.constructor |> List.iter (ctor this super)
    end;

    (* process instance methods and fields *)
    begin
      iter_methods (method_ s.super) s;
      let config = Context.esproposal_class_instance_fields cx in
      SMap.iter (field config this s.super) s.fields
    end
  )
