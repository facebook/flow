module Anno = Type_annotation
module Ast = Spider_monkey_ast
module Flow = Flow_js
module Sig = Classy_sig
module Utils = Utils_js

open Reason_js

let subst_sig cx map s =
  let subst_field cx tparams_map (t, value) =
    Flow.subst cx tparams_map t, value in
  Sig.({
    reason = s.reason;
    super = Flow.subst cx map s.super;
    fields = SMap.map (subst_field cx map) s.fields;
    methods = SMap.map (List.map (Func_sig.subst cx map)) s.methods;
    getters = SMap.map (Func_sig.subst cx map) s.getters;
    setters = SMap.map (Func_sig.subst cx map) s.setters;
    method_decls = s.method_decls;
  })

let mk_super cx tparams_map c targs = Type.(
  (* A super class must be parameterized by This, so that it can be specialized
     to this class and its subclasses when properties are looked up on their
     instances. *)
  let params = Anno.extract_type_param_instantiations targs in
  let this = SMap.find_unsafe "this" tparams_map in
  match params with
  | None
  | Some [] ->
      (* No type params, but `c` could still be a polymorphic class that must
         be implicitly instantiated. We need to do this before we try to
         this-specialize `c`. *)
      let reason = reason_of_t c in
      let c = Flow.mk_tvar_derivable_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (reason, reason, false, [], tvar))
      ) in
      ThisTypeAppT (c, this, [])
  | Some params ->
      ThisTypeAppT (c, this, List.map (Anno.convert cx tparams_map) params)
)

let mk_extends fn cx tparams_map reason = Type.(function
  | (None, None) -> MixedT (reason_of_string "Object", Mixed_everything)
  | (None, _) -> assert false (* type args with no head expr *)
  | (Some ext, targs) ->
      fn cx tparams_map reason (ext, targs)
)

let add_this self cx reason tparams tparams_map =
  (* We haven't computed the instance type yet, but we can still capture a
     reference to it using the class name (as long as the class has a name). We
     need this reference to constrain the `this` in the class. *)
  let rec_instance_type =
    match tparams with
    | [] ->
      Flow.mk_instance cx reason self
    | _ ->
      let tparams = List.map (fun tp -> Type.BoundT tp) tparams in
      Type.TypeAppT (self, tparams)
  in
  let this_tp = { Type.
    name = "this";
    reason = replace_reason "`this` type" reason;
    bound = rec_instance_type;
    polarity = Type.Positive;
    default = None;
  } in
  (* Add the type of `this` to the end of the list of type parameters. Remember,
     order is important, since we don't have recursive bounds (aka F-bounds):
     the bound of This refers to all the other type parameters! *)
  tparams@[this_tp],
  SMap.add "this" (Type.BoundT this_tp) tparams_map

let remove_this x = Sig.(
  let tparams = List.rev (List.tl (List.rev x.tparams)) in
  let tparams_map = SMap.remove "this" x.tparams_map in
  { x with tparams; tparams_map }
)

let mk_type_param_declarations cx tparams_map reason self typeParameters =
  let tparams, tparams_map =
    Anno.mk_type_param_declarations cx tparams_map typeParameters in
  add_this self cx reason tparams tparams_map

let warn_or_ignore_decorators cx = function
  | [] -> ()
  | (start_loc, _)::ds ->
    let loc = List.fold_left (fun start_loc (end_loc, _) ->
      Loc.btwn start_loc end_loc
    ) start_loc ds in
    Flow_error.warn_or_ignore_decorators cx loc

module T = struct
  type classy_ast_t = Ast.Class.t
  let ct_check_polarity = true
  let structural = false
  let mk_class c = Type.ThisClassT c
  let remove_this = remove_this
  let subst_sig = subst_sig

  let mk_type_param_declarations cx tparams_map reason self class_ast =
    let { Ast.Class.typeParameters; _ } = class_ast in
    mk_type_param_declarations cx tparams_map reason self typeParameters

  let mk_super cx tparams_map expr reason class_ast =
    let mk_extends_type cx tparams_map _ (ext, targs) =
      let c = expr cx tparams_map ext in
      mk_super cx tparams_map c targs
    in
    let { Ast.Class.superClass; superTypeParameters; _ } = class_ast in
    let pair = superClass, superTypeParameters in
    mk_extends mk_extends_type cx tparams_map reason pair

  let preliminary_warnings cx loc { Ast.Class.implements; classDecorators; _ } =
    warn_or_ignore_decorators cx classDecorators;

    (* TODO *)
    if implements <> [] then
      let msg = "implements not supported" in
      Flow_error.add_error cx (loc, [msg])
    else ()

  let implicit_body reason class_ast class_sig =
    let add_ctor class_ast class_sig = if class_ast.Ast.Class.superClass <> None
      then
        (* Subclass default constructors are technically of the form (...args)
           => { super(...args) }, but we can approximate that using flow's
           existing inheritance machinery. *)
        (* TODO: Does this distinction matter for the type checker? *)
        class_sig
      else
        let reason = replace_reason "default constructor" reason in
        Sig.add_default_constructor reason class_sig
    in
    class_sig
      |> add_ctor class_ast
      |> Sig.add_name reason

  let explicit_body cx _ _ class_ast class_sig =
    let add_element class_sig = Ast.Class.(function
      (* instance and static methods *)
      | Body.Method (loc, {
          Method.key = Ast.Expression.Object.Property.Identifier (_,
            { Ast.Identifier.name; _ });
          value = (_, func);
          kind;
          static;
          decorators;
        }) ->

        warn_or_ignore_decorators cx decorators;

        Ast.Class.Method.(match kind with
        | Get | Set -> Flow_error.warn_unsafe_getters_setters cx loc
        | _ -> ());

        let method_desc, add = match kind with
        | Method.Constructor ->
            "constructor",
            Sig.add_constructor
        | Method.Method ->
            Utils.spf "method `%s`" name,
            Sig.add_method ~static name
        | Method.Get ->
            Utils.spf "getter for `%s`" name,
            Sig.add_getter ~static name
        | Method.Set ->
            Utils.spf "setter for `%s`" name,
            Sig.add_setter ~static name
        in
        let reason = mk_reason method_desc loc in
        let method_sig = Sig.mk_method cx class_sig reason func in
        add method_sig class_sig

      (* fields *)
      | Body.Property (loc, {
          Property.key = Ast.Expression.Object.Property.Identifier
            (_, { Ast.Identifier.name; _ });
          typeAnnotation = typeAnno;
          value;
          static;
        }) ->
          if value <> None
          then Flow_error.warn_or_ignore_class_properties cx ~static loc;

          let reason = mk_reason (Utils.spf "class property `%s`" name) loc in
          let field = Sig.mk_field cx class_sig reason typeAnno value in
          Sig.add_field ~static name field class_sig

      (* literal LHS *)
      | Body.Method (loc, {
          Method.key = Ast.Expression.Object.Property.Literal _;
          _
        })
      | Body.Property (loc, {
          Property.key = Ast.Expression.Object.Property.Literal _;
          _
        }) ->
          let msg = "literal properties not yet supported" in
          Flow_error.add_error cx (loc, [msg]);
          class_sig

      (* computed LHS *)
      | Body.Method (loc, {
          Method.key = Ast.Expression.Object.Property.Computed _;
          _
        })
      | Body.Property (loc, {
          Property.key = Ast.Expression.Object.Property.Computed _;
          _
        }) ->
          let msg = "computed property keys not supported" in
          Flow_error.add_error cx (loc, [msg]);
          class_sig
      )
    in

    (* NOTE: We used to mine field declarations from field assignments in a
       constructor as a convenience, but it was not worth it: often, all that
       did was exchange a complaint about a missing field for a complaint about
       a missing annotation. Moreover, it caused fields declared in the super
       class to be redeclared if they were assigned in the constructor. So we
       don't do it. In the future, we could do it again, but only for private
       fields. *)

    let { Ast.Class.body = (_, { Ast.Class.Body.body; _ }); _ } = class_ast in
    class_sig |> Sig.fold_pipe add_element body
end
