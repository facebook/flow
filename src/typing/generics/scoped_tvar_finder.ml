(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_mapper
open Reason

let ( * ) : 'a 'b 'c 'd. ('a -> 'c) -> ('b -> 'd) -> 'a * 'b -> 'c * 'd =
 (fun f g (x, y) -> (f x, g y))

(* Internal module that helps track what generics are in scope during traversal of the AST. The scope data structures
   themselves are handled by generic_scope.ml, but this helper module is what handles pushing and popping scopes onto
   the stack (using the call stack).
*)
module ScopeHelper : sig
  val init : unit -> unit

  val in_function_scope :
    Generic_cx.t -> ALoc.id list -> (Generic_cx.t -> unit) -> (unit -> 'a) -> 'a

  val in_class_toplevel_scope : ALoc.id list -> ALoc.id -> (unit -> 'a) -> 'a

  val in_class_prop_scope : Generic_cx.t -> (Generic_cx.t -> unit) -> (unit -> 'a) -> 'a
end = struct
  let class_tparams : ALoc.id list ref = ref []

  let clear_class_tparams () = class_tparams := []

  let get_class_tparams () = !class_tparams

  let set_class_tparams params = class_tparams := params

  let init () = clear_class_tparams ()

  let in_function_scope cx tparams set_cx f =
    let class_tparams = get_class_tparams () in
    clear_class_tparams ();
    let tps = class_tparams @ tparams in
    let res = Generic_scope.in_scope cx tps (Fn.compose f set_cx) in
    set_class_tparams class_tparams;
    res

  let in_class_toplevel_scope tparams id f =
    let tps = id :: tparams in
    let old_class_tparams = get_class_tparams () in
    set_class_tparams tps;
    let res = f () in
    set_class_tparams old_class_tparams;
    res

  let in_class_prop_scope cx set_cx f =
    let class_tparams = get_class_tparams () in
    clear_class_tparams ();
    let res = Generic_scope.in_scope cx class_tparams (Fn.compose f set_cx) in
    set_class_tparams class_tparams;
    res
end

type ml = ALoc.t

type tl = ALoc.t * Type.t

(* This visitor walks the AST and produces a generic context (generic_cx.ml), which contains information about
   what tvars are in what generic scopes. It's the responsibility of this visitor to call #mark or #blame *)

(* The context passed in here needs to be the file-local context for the module, not the leader context, because this
   process is concerned with munged underscores which is a file-local setting. *)
class finder cx =
  object (this)
    inherit [ml, tl, ml, tl] Flow_polymorphic_ast_mapper.mapper as super

    method on_type_annot a = a

    method on_loc_annot a = a

    val mutable gcx : Generic_cx.t = Generic_cx.new_gcx ()

    method set_gcx gcx' = gcx <- gcx'

    method exec ast =
      gcx <- Generic_scope.init gcx;
      ScopeHelper.init ();
      let (_ : (ml, tl) Ast.Statement.t list) = this#toplevel_statement_list ast in
      gcx

    (* Mark the tvars in a type as being in the current scope, *and* record where an annotation needs to be added
       if an out-of-scope generic flows into the tvars. This blame information is not always available--for example,
       in a case like

       class C { }
       function f<X>(x: X) { C = x }

       there's nothing to annotate that would fix the error. Hence the `mark` method below.
    *)
    method blame reason ty =
      gcx <- Generic_cx.blame_ids_of_type cx gcx ty (Generic_scope.scope_id ()) reason

    (* Mark the tvars in a type as being in the current scope *)
    method mark ty = gcx <- Generic_cx.mark_ids_of_type cx gcx ty (Generic_scope.scope_id ())

    method! statement (loc, stmt) =
      let open Ast.Statement in
      match stmt with
      | ClassDeclaration cls ->
        let (_ : (ml, tl) Ast.Class.t) = this#class_with_loc loc cls in
        (loc, stmt)
      | _ -> super#statement (loc, stmt)

    method! expression ((loc, ty), expr) =
      let open Ast.Expression in
      match expr with
      | Class cls ->
        let (_ : (ml, tl) Ast.Class.t) = this#class_with_loc loc cls in
        ((loc, ty), expr)
      | _ -> super#expression ((loc, ty), expr)

    method! function_ node =
      let {
        Ast.Function.id = ident;
        params;
        body;
        async = _;
        generator = _;
        predicate;
        return;
        tparams;
        sig_loc = _;
        comments = _;
      } =
        node
      in
      let (_ : (ml, tl) Ast.Identifier.t option) = map_opt this#t_function_identifier ident in
      let (_ : (ml, tl) Ast.Type.annotation_or_hint) =
        this#labeled_type_annotation_hint "return" return
      in
      let (_ : (ml, tl) Ast.Type.Predicate.t option) = map_opt this#type_predicate predicate in
      let tparams = this#get_tparams tparams in
      (* Check the function body in a scope that contains both the function's parameters and also any
         class type parameters if we're inside a class toplevel. *)
      ScopeHelper.in_function_scope gcx tparams this#set_gcx (fun () ->
          let (_ : (ml, tl) Ast.Function.Params.t) = this#function_params params in
          let (_ : (ml, tl) Ast.Function.body) = this#function_body body in
          ()
      );
      node

    (* Classes need their declaration locations to have the ALoc.id for `this`, in case they're anonymous.
       Non-anonymous classes use the loc for their identifiers as the ALoc.id for `this`. This mirrors the
       behavior of `this` in statement.ml *)
    method class_with_loc decl_loc cls =
      let { Ast.Class.id; body; tparams; extends; implements; class_decorators; comments = _ } =
        cls
      in
      let (_ : (ml, tl) Ast.Identifier.t option) = map_opt this#class_identifier id in
      let (_ : (ml, tl) Ast.Class.Extends.t option) = map_opt this#class_extends extends in
      let (_ : (ml, tl) Ast.Class.Implements.t option) = map_opt this#class_implements implements in
      let (_ : (ml, tl) Ast.Class.Decorator.t list) =
        map_list this#class_decorator class_decorators
      in
      let tparams = this#get_tparams tparams in

      let id =
        Context.make_aloc_id
          cx
          (Base.Option.value_map ~f:(fun ((loc, _), _) -> loc) ~default:decl_loc id)
      in
      (* This doesn't put the class' tparams and `this` in scope immediately. Instead, we store these tparams,
         and then when we enter a toplevel method body or a property initialization, we put them in scope at that
         point. This allows properties and method params/returns to be out-of-scope for the typeparams, which is
         desirable--we want to ask for annotations if a tparam flows into anything externally visible in the class *)
      ScopeHelper.in_class_toplevel_scope tparams id (fun () ->
          let (_ : (ml, tl) Ast.Class.Body.t) = this#class_body body in
          ()
      );
      cls

    method! class_ _ = failwith "Use class_with_loc"

    method! class_method meth =
      let { Ast.Class.Method.kind = _; key; value; static; decorators; comments } = meth in
      let (_, is_munged) = this#object_key_label key in
      let check_body () =
        let (_ : (ml, tl) Ast.Expression.Object.Property.key) = this#object_key key in
        let (_, _) = (this#on_loc_annot * this#function_expression) value in
        let (_ : (ml, tl) Ast.Class.Decorator.t list) =
          Base.List.map ~f:this#class_decorator decorators
        in
        let (_ : (ml, unit) Ast.Syntax.t option) = Base.Option.map ~f:this#syntax comments in
        ()
      in

      if is_munged && not static then
        ScopeHelper.in_class_prop_scope gcx this#set_gcx check_body
      else
        check_body ();
      meth

    method! class_property prop =
      let { Ast.Class.Property.key; value; annot; static; variance = _; comments = _ } = prop in
      let (label, is_munged) = this#object_key_label key in
      let check_prop () =
        let (_ : (ml, tl) Ast.Expression.Object.Property.key) = this#object_key key in
        let (_ : (ml, tl) Ast.Type.annotation_or_hint) =
          this#labeled_type_annotation_hint label annot
        in
        ()
      in
      ( if is_munged && not static then
        (* Special case for munged properties *)
        ScopeHelper.in_class_prop_scope gcx this#set_gcx (fun () ->
            let () = check_prop () in
            let (_ : (ml, tl) Ast.Class.Property.value) = this#class_property_value value in
            ()
        )
      else
        let () = check_prop () in
        (* Here we actually install the tparams collected in `class_with_loc` into the scope *)
        ScopeHelper.in_class_prop_scope gcx this#set_gcx (fun () ->
            let (_ : (ml, tl) Ast.Class.Property.value) = this#class_property_value value in
            ()
        )
      );
      prop

    method object_key_label key =
      match key with
      | Ast.Expression.Object.Property.Literal _ -> ("literal property", false)
      | Ast.Expression.Object.Property.Identifier (_, { Ast.Identifier.name = label; _ }) ->
        let is_munged =
          Signature_utils.is_munged_property_string label && Context.should_munge_underscores cx
        in
        (Utils_js.spf "property `%s`" label, is_munged)
      | Ast.Expression.Object.Property.PrivateName private_name ->
        let _ = this#private_name private_name in
        let (_, { Ast.PrivateName.name; _ }) = private_name in
        (Utils_js.spf "private property `%s`" name, false)
      | Ast.Expression.Object.Property.Computed _ -> ("computed property", false)

    method! class_private_field field =
      let {
        Ast.Class.PrivateField.key = (_, { Ast.PrivateName.name = label; _ }) as key;
        value;
        annot;
        static;
        variance = _;
        comments = _;
      } =
        field
      in
      let check_prop () =
        let (_ : ml Ast.PrivateName.t) = this#private_name key in
        let (_ : (ml, tl) Ast.Type.annotation_or_hint) =
          this#labeled_type_annotation_hint label annot
        in
        ()
      in
      ( if not static then
        ScopeHelper.in_class_prop_scope gcx this#set_gcx (fun () ->
            let (_ : (ml, tl) Ast.Class.Property.value) = this#class_property_value value in
            check_prop ()
        )
      else
        let () = check_prop () in
        ScopeHelper.in_class_prop_scope gcx this#set_gcx (fun () ->
            let (_ : (ml, tl) Ast.Class.Property.value) = this#class_property_value value in
            ()
        )
      );
      field

    method type_param_id tparam =
      let (_, { Ast.Type.TypeParam.name = (loc, _); bound = _; variance = _; default = _ }) =
        tparam
      in
      Context.make_aloc_id cx loc

    method get_tparams (tparams : (ALoc.t, ALoc.t * Type.t) Ast.Type.TypeParams.t option) =
      this#type_params_opt
        tparams
        (Base.Option.value_map ~default:[] ~f:(fun (_, { Ast.Type.TypeParams.params; _ }) ->
             Base.List.map ~f:this#type_param_id params
         )
        )

    method! pattern_object_property_identifier_key ?kind:_ key =
      (* Don't explore object property identifier keys, only value identifiers. *)
      key

    method labeled_type_annotation_hint name node =
      let open Ast.Type in
      begin
        match node with
        | Missing (loc, ty) ->
          let (_ : ALoc.t * Type.t) = this#on_type_annot (loc, ty) in
          this#blame (mk_reason (RCustom name) loc) ty
        | Available annot ->
          let (_ : (ml, tl) Ast.Type.annotation) = this#type_annotation annot in
          ()
      end;
      node

    method! binding_pattern ?(kind = Ast.Statement.VariableDeclaration.Var) ((_, patt) as expr) =
      (* We can only suggest annotations for bare variable declarations, not destructurings. *)
      let open Ast.Pattern in
      begin
        match patt with
        | Identifier
            {
              Identifier.name = ((loc, ty), { Ast.Identifier.name; _ });
              annot = Ast.Type.Missing _;
              _;
            } ->
          this#blame (mk_reason (RIdentifier (OrdinaryName name)) loc) ty
        | _ -> ()
      end;
      super#binding_pattern ~kind expr

    method! t_pattern_identifier ?kind (((_, ty), _) as node) =
      (* If `kind` is None, then this is the LHS of an assignment, while it it's Some _, it's a variable
         declaration. We only are concerned with declarations here: statement.ml guarantees that the type
         on a declaration is the general type of the variable in its environment, while assignments just
         have its specific type. *)
      Base.Option.iter ~f:(fun _ -> this#mark ty) kind;
      super#t_pattern_identifier ?kind node
  end
