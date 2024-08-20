(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let mk_bound_t cx tparam = Flow_js_utils.generic_of_tparam cx ~f:(fun x -> x) tparam

type ('M, 'T) enclosing_node =
  | EnclosingProgram of ('M, 'T) Ast.Program.t
  | EnclosingStatement of ('M, 'T) Ast.Statement.t
  | EnclosingExpression of ('M, 'T) Ast.Expression.t

class virtual ['M, 'T, 'N, 'U] enclosing_node_mapper =
  object
    inherit ['M, 'T, 'N, 'U] Flow_polymorphic_ast_mapper.mapper as super

    val mutable enclosing_node_stack : ('M, 'T) enclosing_node list = []

    method enclosing_node = List.hd enclosing_node_stack

    method! program prog =
      let old_enclosing_node_stack = enclosing_node_stack in
      enclosing_node_stack <- EnclosingProgram prog :: enclosing_node_stack;
      let program' = super#program prog in
      enclosing_node_stack <- old_enclosing_node_stack;
      program'

    method! statement stmt =
      let old_enclosing_node_stack = enclosing_node_stack in
      enclosing_node_stack <- EnclosingStatement stmt :: enclosing_node_stack;
      let stmt' = super#statement stmt in
      enclosing_node_stack <- old_enclosing_node_stack;
      stmt'

    method! expression expr =
      let old_enclosing_node_stack = enclosing_node_stack in
      enclosing_node_stack <- EnclosingExpression expr :: enclosing_node_stack;
      let expr' = super#expression expr in
      enclosing_node_stack <- old_enclosing_node_stack;
      expr'
  end

module Statement = Fix_statement.Statement_

let infer_node cx node =
  match node with
  | EnclosingProgram prog ->
    let (prog_aloc, { Ast.Program.statements; interpreter; comments; all_comments }) = prog in
    let statements = Statement.statement_list cx statements in
    EnclosingProgram (prog_aloc, { Ast.Program.statements; interpreter; comments; all_comments })
  | EnclosingStatement stmt -> EnclosingStatement (Statement.statement cx stmt)
  | EnclosingExpression expr -> EnclosingExpression (Statement.expression cx expr)

let find_type_annot_in_node loc node =
  let exception Found of Type.t in
  let visitor =
    object (_this)
      inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

      method on_loc_annot loc = loc

      method on_type_annot (loc', t) =
        if loc' = loc then raise (Found t);
        (loc, t)
    end
  in
  try
    begin
      match node with
      | EnclosingProgram prog -> ignore (visitor#program prog)
      | EnclosingStatement stmt -> ignore (visitor#statement stmt)
      | EnclosingExpression expr -> ignore (visitor#expression expr)
    end;
    None
  with
  | Found t -> Some t

class virtual ['M, 'T, 'N, 'U, 'P] type_parameter_mapper_generic =
  object (self)
    inherit ['M, 'T, 'N, 'U] enclosing_node_mapper as super

    method virtual make_typeparam : ('M, 'T) Ast.Type.TypeParam.t -> 'P

    method virtual make_class_this : ('M, 'T) Ast.Class.t -> 'P

    method virtual make_declare_class_this : ('M, 'T) Ast.Statement.DeclareClass.t -> 'P

    (* Since the mapper wasn't originally written to pass an accumulator value
       through the calls, we're maintaining this accumulator imperatively. *)
    val mutable rev_bound_tparams : 'P list = []

    method annot_with_tparams : 'a. (tparams_rev:'P list -> 'a) -> 'a =
      (fun f -> f ~tparams_rev:rev_bound_tparams)

    (* Imperatively adds type parameter to bound_tparams environment. *)
    method! type_param tparam =
      let res = super#type_param tparam in
      let tparam = self#make_typeparam tparam in
      rev_bound_tparams <- tparam :: rev_bound_tparams;
      res

    (* Record and restore the parameter environment around nodes that might
       update it. *)
    method! type_params_opt pd f =
      let originally_bound_tparams = rev_bound_tparams in
      let res = super#type_params_opt pd f in
      rev_bound_tparams <- originally_bound_tparams;
      res

    method! conditional_type t =
      let open Ast.Type.Conditional in
      let { check_type; extends_type; true_type; false_type; comments } = t in
      let check_type' = self#type_ check_type in
      let extends_type' = self#type_ extends_type in
      let fake_tparams_opt =
        let params =
          Infer_type_hoister.hoist_infer_types extends_type
          |> Base.List.map ~f:(fun (_, { Ast.Type.Infer.tparam; _ }) -> tparam)
        in
        match params with
        | [] -> None
        | (loc, _) :: _ -> Some (loc, { Ast.Type.TypeParams.params; comments = None })
      in
      let true_type' = self#type_params_opt fake_tparams_opt (fun _ -> self#type_ true_type) in
      let false_type' = self#type_ false_type in
      let comments' = self#syntax_opt comments in
      {
        check_type = check_type';
        extends_type = extends_type';
        true_type = true_type';
        false_type = false_type';
        comments = comments';
      }

    (* Classes assume an additional "this" type parameter, which needs to be
       explicitly added to bound_tparams *)
    method! class_ cls =
      let this_tparam = self#make_class_this cls in
      let originally_bound_tparams = rev_bound_tparams in
      rev_bound_tparams <- this_tparam :: rev_bound_tparams;
      let cls = super#class_ cls in
      rev_bound_tparams <- originally_bound_tparams;
      cls

    method! declare_class decl =
      let this_tparam = self#make_declare_class_this decl in
      let originally_bound_tparams = rev_bound_tparams in
      rev_bound_tparams <- this_tparam :: rev_bound_tparams;
      let decl = super#declare_class decl in
      rev_bound_tparams <- originally_bound_tparams;
      decl
  end

class type_parameter_mapper =
  object
    inherit ['M, 'T, 'N, 'U, Type.typeparam] type_parameter_mapper_generic

    method on_loc_annot (x : ALoc.t) = x

    method on_type_annot (x : ALoc.t * Type.t) = x

    method private make_typeparam tparam =
      let open Ast.Type.TypeParam in
      let (_, { name = id; bound; bound_kind = _; variance; default }) = tparam in
      let (name_loc, { Ast.Identifier.name; comments = _ }) = id in
      let reason = Reason.mk_annot_reason (Reason.RType (Reason.OrdinaryName name)) name_loc in
      let bound =
        match bound with
        | Ast.Type.Missing (_, t)
        | Ast.Type.Available (_, ((_, t), _)) ->
          t
      in
      let default =
        match default with
        | None -> None
        | Some ((_, t), _) -> Some t
      in
      let polarity = Typed_ast_utils.polarity variance in
      { Type.reason; name = Subst_name.Name name; bound; polarity; default; is_this = false }

    method private make_class_this cls =
      let open Reason in
      let { Ast.Class.body = (body_loc, _); id; _ } = cls in
      let bound =
        match id with
        | Some ((_, t), _) -> t
        | None ->
          let reason = mk_reason (RCustom "<<anonymous class>>") body_loc in
          Type.DefT (reason, Type.MixedT Type.Mixed_everything)
      in
      {
        Type.name = Subst_name.Name "this";
        reason = replace_desc_reason RThisType (TypeUtil.reason_of_t bound);
        bound;
        polarity = Polarity.Positive;
        default = None;
        is_this = true;
      }

    method private make_declare_class_this decl =
      let open Reason in
      let { Ast.Statement.DeclareClass.id; _ } = decl in
      let ((_, bound), _) = id in
      {
        Type.name = Subst_name.Name "this";
        reason = replace_desc_reason RThisType (TypeUtil.reason_of_t bound);
        bound;
        polarity = Polarity.Positive;
        default = None;
        is_this = true;
      }
  end

(* Find exact location match *)
module ExactMatchQuery = struct
  exception Found of Type.t

  class exact_match_searcher (target_loc : ALoc.t) =
    object
      inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

      method on_loc_annot loc = loc

      method on_type_annot annot =
        let (loc, t) = annot in
        if target_loc = loc then
          raise (Found t)
        else
          annot
    end

  let find typed_ast aloc =
    let searcher = new exact_match_searcher aloc in
    match searcher#program typed_ast with
    | exception Found t -> Some t
    | _ -> None
end

let find_exact_match_annotation = ExactMatchQuery.find

(* Find identifier under location *)
module Type_at_pos = struct
  exception Found of ALoc.t * bool * Type.t

  (* Kinds of nodes that "type-at-pos" is interested in:
   * - identifiers              (handled in t_identifier)
   * - type parameters          (handled in type_param_identifier)
   * - literal object keys      (handled in object_key)
   * - `this`, `super`          (handled in expression)
   * - private property names   (handled in expression)
   *)
  class type_at_pos_searcher cx (target_loc : Loc.t) =
    object (self)
      inherit type_parameter_mapper as super

      method covers_target loc = Reason.in_range target_loc (ALoc.to_loc_exn loc)

      method covers_target_loc loc = Reason.in_range target_loc loc

      method find_loc
          : 'a. ALoc.t -> Type.t -> is_type_identifier:bool -> tparams_rev:Type.typeparam list -> 'a
          =
        (fun loc t ~is_type_identifier ~tparams_rev:_ -> raise (Found (loc, is_type_identifier, t)))

      method! t_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#t_identifier id

      method! type_identifier_reference (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:true)
        else
          super#t_identifier id

      method! jsx_identifier (((loc, t), _) as id) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#jsx_identifier id

      method! type_param ((_, { Ast.Type.TypeParam.name = (loc, _); _ }) as tparam) =
        if self#covers_target loc then (
          let tparam = self#make_typeparam tparam in
          rev_bound_tparams <- tparam :: rev_bound_tparams;
          self#annot_with_tparams
            (self#find_loc loc (mk_bound_t cx tparam) ~is_type_identifier:false)
        ) else
          super#type_param tparam

      method! object_key key =
        let open Ast.Expression.Object.Property in
        match key with
        | StringLiteral ((loc, t), _)
        | NumberLiteral ((loc, t), _)
        | BigIntLiteral ((loc, t), _)
          when self#covers_target loc ->
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        | _ -> super#object_key key

      method! expression expr =
        let open Ast.Expression in
        match expr with
        | ((loc, t), (This _ | Super _))
        | ((_, t), Member { Member.property = Member.PropertyPrivateName (loc, _); _ })
        | ( (_, t),
            OptionalMember
              {
                OptionalMember.member = { Member.property = Member.PropertyPrivateName (loc, _); _ };
                _;
              }
          )
          when self#covers_target loc ->
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        | _ -> super#expression expr

      (*
       *     Class information
       *     v
       * new C(e1, e2);
       * ^^^^
       * Constructor information
       *)
      method! new_ ((expr_loc, _) as annot) expr =
        let open Ast.Expression.New in
        let { callee = ((callee_loc, _), _); _ } = expr in
        let expr_start_loc = Loc.first_char (ALoc.to_loc_exn expr_loc) in
        let callee_start_loc = Loc.char_before (Loc.char_before (ALoc.to_loc_exn callee_loc)) in
        let new_loc = Loc.btwn expr_start_loc callee_start_loc in
        if self#covers_target_loc new_loc then
          match Context.get_ctor_callee cx expr_loc with
          | Some t -> self#annot_with_tparams (self#find_loc callee_loc t ~is_type_identifier:false)
          | None -> super#new_ annot expr
        else
          super#new_ annot expr

      method! implicit (((loc, t), _) as impl) =
        if self#covers_target loc then
          self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false)
        else
          super#implicit impl

      method! jsx_attribute_name_identifier (((loc, _), _) as id) =
        if self#covers_target loc then
          let reason = Reason.mk_reason (Reason.RCustom "jsx attr") loc in
          let (_, lazy_hint) = Type_env.get_hint cx loc in
          lazy_hint reason
          |> Type_hint.with_hint_result
               ~ok:(fun t ->
                 self#annot_with_tparams (self#find_loc loc t ~is_type_identifier:false))
               ~error:(fun () -> super#jsx_attribute_name_identifier id)
        else
          super#jsx_attribute_name_identifier id
    end

  let find cx typed_ast loc =
    let searcher = new type_at_pos_searcher cx loc in
    try
      ignore (searcher#program typed_ast);
      None
    with
    | Found (loc, is_type_id, scheme) -> Some (ALoc.to_loc_exn loc, is_type_id, scheme)
end

let find_type_at_pos_annotation = Type_at_pos.find
