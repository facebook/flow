(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
module ALocMap = Loc_collections.ALocMap

(* TODO(nmote) come up with a consistent story for abstract/concrete locations in this module *)

class type_parameter_mapper =
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    (* Since the mapper wasn't originally written to pass an accumulator value
     through the calls, we're maintaining this accumulator imperatively. *)
    val mutable bound_tparams : Type.typeparam list = []

    method annot_with_tparams : 'a. (Type.typeparam list -> 'a) -> 'a = (fun f -> f bound_tparams)

    (* Imperatively adds type parameter to bound_tparams environment. *)
    method! type_parameter_declaration_type_param tparam =
      let res = super#type_parameter_declaration_type_param tparam in
      (* Recover the Type.typeparams corresponding to AST type parameters *)
      let tparam =
        Ast.Type.ParameterDeclaration.(
          let ( _,
                {
                  TypeParam.name = ((_, t), { Ast.Identifier.name; comments = _ });
                  bound;
                  variance;
                  default;
                } ) =
            tparam
          in
          let reason = Type.reason_of_t t in
          let bound =
            match bound with
            | Ast.Type.Missing _ -> Type.MixedT.make reason |> Type.with_trust Trust.bogus_trust
            | Ast.Type.Available (_, ((_, t), _)) -> t
          in
          let polarity =
            Ast.Variance.(
              match variance with
              | Some (_, Plus) -> Polarity.Positive
              | Some (_, Minus) -> Polarity.Negative
              | None -> Polarity.Neutral)
          in
          let default = Option.map default ~f:(fun ((_, t), _) -> t) in
          { Type.reason; name; bound; polarity; default })
      in
      bound_tparams <- tparam :: bound_tparams;
      res

    (* Record and restore the parameter environment around nodes that might
     update it. *)
    method! type_parameter_declaration_opt pd f =
      let originally_bound_tparams = bound_tparams in
      let res = super#type_parameter_declaration_opt pd f in
      bound_tparams <- originally_bound_tparams;
      res

    (* Classes assume an additional "this" type parameter, which needs to be
     explicitly added to bound_tparams *)
    method! class_ cls =
      let this_tparam =
        Ast.Class.(
          let { body = ((body_loc, self_t), _); id; _ } = cls in
          let name =
            Option.value_map ~f:Flow_ast_utils.name_of_ident id ~default:"<<anonymous class>>"
          in
          let name_loc = Option.value_map ~f:(fun ((loc, _), _) -> loc) id ~default:body_loc in
          {
            Type.name = "this";
            reason = Reason.mk_reason (Reason.RType name) name_loc;
            bound = self_t;
            polarity = Polarity.Positive;
            default = None;
          })
      in
      let originally_bound_tparams = bound_tparams in
      bound_tparams <- this_tparam :: bound_tparams;
      let cls = super#class_ cls in
      bound_tparams <- originally_bound_tparams;
      cls
  end

(* Find exact location match *)
module ExactMatchQuery = struct
  exception Found of Type.TypeScheme.t

  let found t tparams = raise (Found { Type.TypeScheme.tparams; type_ = t })

  class exact_match_searcher (target_loc : ALoc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method! on_type_annot annot =
        let (loc, t) = annot in
        if target_loc = loc then
          self#annot_with_tparams (found t)
        else
          super#on_type_annot annot
    end

  let find typed_ast aloc =
    let searcher = new exact_match_searcher aloc in
    try
      ignore (searcher#program typed_ast);
      None
    with Found scheme -> Some scheme
end

let find_exact_match_annotation = ExactMatchQuery.find

(* Find identifier under location *)
module Type_at_pos = struct
  exception Found of ALoc.t * Type.TypeScheme.t

  (* Kinds of nodes that "type-at-pos" is interested in:
   * - identifiers              (handled in t_identifier)
   * - literal object keys      (handled in object_key)
   * - `this`, `super`          (handled in expression)
   * - private property names   (handled in expression)
   *)
  class type_at_pos_searcher (target_loc : Loc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method find_loc : 'a. ALoc.t -> Type.t -> Type.typeparam list -> 'a =
        (fun loc t tparams -> raise (Found (loc, { Type.TypeScheme.tparams; type_ = t })))

      method! t_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#t_identifier id

      method! jsx_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#jsx_identifier id

      method! object_key key =
        Ast.Expression.Object.Property.(
          match key with
          | Literal ((loc, t), _) when self#covers_target loc ->
            self#annot_with_tparams (self#find_loc loc t)
          | _ -> super#object_key key)

      method! expression expr =
        Ast.Expression.(
          match expr with
          | ((loc, t), (This | Super))
          | ((_, t), Member { Member.property = Member.PropertyPrivateName (loc, _); _ })
          | ( (_, t),
              OptionalMember
                {
                  OptionalMember.member =
                    { Member.property = Member.PropertyPrivateName (loc, _); _ };
                  _;
                } )
            when self#covers_target loc ->
            self#annot_with_tparams (fun tparams -> self#find_loc loc t tparams)
          | _ -> super#expression expr)

      method! implicit (loc, t) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t)
        else
          super#implicit (loc, t)
    end

  let find typed_ast loc =
    let searcher = new type_at_pos_searcher loc in
    try
      ignore (searcher#program typed_ast);
      None
    with Found (loc, scheme) -> Some (ALoc.to_loc_exn loc, scheme)
end

let find_type_at_pos_annotation = Type_at_pos.find

class type_at_aloc_map_folder =
  object
    inherit type_parameter_mapper

    val mutable map = ALocMap.empty

    method! on_type_annot x =
      let (loc, type_) = x in
      let scheme = Type.TypeScheme.{ type_; tparams = bound_tparams } in
      map <- ALocMap.add loc scheme map;
      x

    method to_map = map
  end

class type_at_aloc_list_folder =
  object
    inherit type_parameter_mapper

    val mutable l = []

    method! on_type_annot x =
      let (loc, type_) = x in
      l <- (loc, Type.TypeScheme.{ type_; tparams = bound_tparams }) :: l;
      x

    method to_list = l
  end

let typed_ast_to_map typed_ast : Type.TypeScheme.t ALocMap.t =
  let folder = new type_at_aloc_map_folder in
  ignore (folder#program typed_ast);
  folder#to_map

let typed_ast_to_list typed_ast : (ALoc.t * Type.TypeScheme.t) list =
  let folder = new type_at_aloc_list_folder in
  ignore (folder#program typed_ast);
  folder#to_list

(* Get-def *)

type get_def_object_source =
  | GetDefType of Type.t
  | GetDefRequireLoc of ALoc.t

(* source loc *)

type get_def_member_info = {
  get_def_prop_name: string;
  get_def_object_source: get_def_object_source;
}

type get_def_result =
  | GetDefMember of get_def_member_info
  | GetDefIdentifier

module Get_def = struct
  exception Found of get_def_result

  class searcher (target_loc : Loc.t) =
    object (this)
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_loc_annot (x : ALoc.t) = x

      method on_type_annot (x : ALoc.t * Type.t) = x

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method find_loc x = raise (Found x)

      method! import_declaration import_loc decl =
        Ast.Statement.ImportDeclaration.(
          let { importKind = _; source = (source_loc, _); specifiers; default } = decl in
          Option.iter ~f:(this#import_specifier_with_loc ~source_loc) specifiers;
          Option.iter ~f:(this#import_default_specifier_with_loc ~source_loc) default;
          super#import_declaration import_loc decl)

      method import_specifier_with_loc ~source_loc specifier =
        Ast.Statement.ImportDeclaration.(
          match specifier with
          | ImportNamedSpecifiers named_specifiers ->
            Core_list.iter ~f:(this#import_named_specifier_with_loc ~source_loc) named_specifiers
          | ImportNamespaceSpecifier _ -> ())

      method import_named_specifier_with_loc ~source_loc specifier =
        Ast.Statement.ImportDeclaration.(
          let { kind = _; local; remote } = specifier in
          let ((remote_name_loc, _), { Ast.Identifier.name = remote_name; _ }) = remote in
          let result =
            GetDefMember
              {
                get_def_prop_name = remote_name;
                get_def_object_source = GetDefRequireLoc source_loc;
              }
          in
          if this#covers_target remote_name_loc then this#find_loc result;
          Option.iter
            ~f:(fun local ->
              let ((local_name_loc, _), _) = local in
              if this#covers_target local_name_loc then
                let result =
                  GetDefMember
                    {
                      get_def_prop_name = remote_name;
                      get_def_object_source = GetDefRequireLoc source_loc;
                    }
                in
                this#find_loc result)
            local)

      method! member expr =
        Ast.Expression.Member.(
          let { _object; property } = expr in
          begin
            match property with
            | PropertyIdentifier ((loc, _), { Ast.Identifier.name; _ }) when this#covers_target loc
              ->
              let ((_, t), _) = _object in
              let result =
                GetDefMember { get_def_prop_name = name; get_def_object_source = GetDefType t }
              in
              this#find_loc result
            | _ -> ()
          end;
          super#member expr)

      method import_default_specifier_with_loc ~source_loc default =
        let ((remote_name_loc, _), _) = default in
        if this#covers_target remote_name_loc then
          let result =
            GetDefMember
              {
                get_def_prop_name = "default";
                (* see members.ml *)
                get_def_object_source = GetDefRequireLoc source_loc;
              }
          in
          this#find_loc result

      method! t_identifier (((loc, _t), _) as id) =
        if this#covers_target loc then this#find_loc GetDefIdentifier;
        super#t_identifier id

      method! jsx_identifier (((loc, _t), _) as id) =
        if this#covers_target loc then this#find_loc GetDefIdentifier;
        super#jsx_identifier id

      (* If we found target_loc in a pattern, for now we throw it out and
        let the hooks take care of it because get-def for lvals hasn't been
        refactored onto the typed AST yet. TODO(vijayramamurthy) *)
      method! t_pattern_identifier ?kind id =
        (try super#t_pattern_identifier ?kind id with Found _ -> id)
    end

  let find_get_def_info typed_ast loc =
    let searcher = new searcher loc in
    try
      ignore (searcher#program typed_ast);
      None
    with Found info -> Some info
end

let find_get_def_info = Get_def.find_get_def_info

(* Coverage *)

class ['a, 'l, 't] coverage_folder ~(f : 'l -> 't -> 'a -> 'a) ~(init : 'a) =
  object (this)
    inherit ['l, 'l * 't, 'l, 'l * 't] Flow_polymorphic_ast_mapper.mapper as super

    val mutable acc : 'a = init

    method on_loc_annot x = x

    method on_type_annot x = x

    method! expression exp =
      let ((loc, t), _) = exp in
      acc <- f loc t acc;
      super#expression exp

    method! object_property prop =
      let prop = super#object_property prop in
      Ast.Expression.Object.Property.(
        match prop with
        | (loc, Method { key = Literal ((_, t), _) | Identifier ((_, t), _); _ }) ->
          acc <- f loc t acc;
          prop
        | _ -> prop)

    method! statement stmt =
      let stmt = super#statement stmt in
      match stmt with
      | (loc, Ast.Statement.ClassDeclaration { Ast.Class.id = Some ((_, t), _); _ })
      | (loc, Ast.Statement.DeclareClass { Ast.Statement.DeclareClass.id = ((_, t), _); _ })
      | ( _,
          Ast.Statement.DeclareExportDeclaration
            {
              Ast.Statement.DeclareExportDeclaration.declaration =
                Some
                  ( Ast.Statement.DeclareExportDeclaration.NamedOpaqueType
                      (loc, { Ast.Statement.OpaqueType.id = ((_, t), _); _ })
                  | Ast.Statement.DeclareExportDeclaration.Class
                      (loc, { Ast.Statement.DeclareClass.id = ((_, t), _); _ }) );
              _;
            } )
      | (loc, Ast.Statement.DeclareInterface { Ast.Statement.Interface.id = ((_, t), _); _ })
      | ( loc,
          Ast.Statement.DeclareModule
            {
              Ast.Statement.DeclareModule.id =
                ( Ast.Statement.DeclareModule.Identifier ((_, t), _)
                | Ast.Statement.DeclareModule.Literal ((_, t), _) );
              _;
            } )
      | (loc, Ast.Statement.DeclareTypeAlias { Ast.Statement.TypeAlias.id = ((_, t), _); _ })
      | (loc, Ast.Statement.DeclareOpaqueType { Ast.Statement.OpaqueType.id = ((_, t), _); _ })
      | (loc, Ast.Statement.InterfaceDeclaration { Ast.Statement.Interface.id = ((_, t), _); _ })
      | (loc, Ast.Statement.OpaqueType { Ast.Statement.OpaqueType.id = ((_, t), _); _ })
      | (loc, Ast.Statement.TypeAlias { Ast.Statement.TypeAlias.id = ((_, t), _); _ }) ->
        acc <- f loc t acc;
        stmt
      | _ -> stmt

    method! class_identifier i = i

    (* skip this *)
    method! jsx_name name =
      Ast.JSX.(
        let name = super#jsx_name name in
        match name with
        | MemberExpression (loc, { MemberExpression.property = ((_, t), _); _ }) ->
          acc <- f loc t acc;
          name
        | Identifier _
        | NamespacedName _ ->
          name)

    method! jsx_member_expression_object _object =
      Ast.JSX.MemberExpression.(
        match _object with
        | Identifier ((loc, t), _) ->
          acc <- f loc t acc;
          _object
        | MemberExpression _ -> super#jsx_member_expression_object _object)

    method! t_pattern_identifier ?kind i =
      let ((loc, t), _) = i in
      acc <- f loc t acc;
      super#t_pattern_identifier ?kind i

    method top_level_program prog =
      acc <- init;
      ignore (this#program prog);
      acc
  end

let coverage_fold_tast ~(f : 'l -> 't -> 'acc -> 'acc) ~(init : 'acc) tast =
  let folder = new coverage_folder ~f ~init in
  folder#top_level_program tast

(** Mappers
 *  Used to construct error nodes during type checking.
 *)

(* Error nodes are typed at `any`. Do not change this type as it might change
 * current behavior. *)
let error_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.AnyT.at Type.AnyError loc)
  end

(* Used in unimplemented cases or unsupported nodes *)
let unimplemented_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unimplemented)) loc)
  end

(* Code is not checked at all *)
let unchecked_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(AnyT.at (Unsound Unchecked)) loc)
  end

let unreachable_mapper =
  object
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot loc = loc

    method on_type_annot loc = (loc, Type.(EmptyT.at loc |> with_trust bogus_trust))
  end
