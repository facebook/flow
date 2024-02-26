(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(**
 * This module computes whether a type is considered covered. It does so by
 * visiting the type until a "concrete" constructor is found. We consider a
 * constructor concrete if it has some runtime significance. So for example the
 * arrow constructor and the object constructor are concrete, whereas the union
 * or the intersection constructors are not. This means that whenever we encounter
 * a union we need to visit its parts to determine if this is a covered type.
 *
 * Some constructors that we, perhaps controversially, consider concrete are:
 *  - KeysT
 *
 * In addition to being considered concrete the above constructors are also
 * considered covered.
 *)

open Type
module Ast = Flow_ast

type op_mode =
  | OpAnd
  | OpOr

let unit_of_op = function
  | OpAnd -> true (* mixed *)
  | OpOr -> false (* empty *)

module Kind = struct
  type t =
    | Checked
    | Any
    | Empty

  let to_string = function
    | Checked -> "Checked"
    | Any -> "Any"
    | Empty -> "Empty"

  let m_and = function
    | (Any, _) -> Any
    | (_, Any) -> Any
    | (Empty, _) -> Empty
    | (_, Empty) -> Empty
    | (Checked, Checked) -> Checked

  let m_or = function
    | (Any, _) -> Any
    | (_, Any) -> Any
    | (Empty, m2) -> m2
    | (m1, Empty) -> m1
    | (Checked, Checked) -> Checked

  let merge kind x =
    match kind with
    | OpAnd -> m_and x
    | OpOr -> m_or x

  let to_bool = function
    | Any
    | Empty ->
      false
    | Checked -> true
end

type file_coverage = {
  checked: int;
  uncovered: int;
  empty: int;
}

let initial_coverage = { checked = 0; uncovered = 0; empty = 0 }

type tvar_status =
  | Started
  | Done of Kind.t

let visitor =
  object (self)
    (**
     * Type variables may appear in a cycle in the dependency graph, which requires
     * us to track the ones we've visited to avoid infinite recursion. There are three
     * stages of tvar resolution w.r.t coverage:
     *
     * - The tvar has not been seen before (there is no entry in tvar_cache). In this
     *   case we descend into the lower bounds of the tvar, marking its binding as
     *   Started in the tvar_cache.
     *
     * - The tvar has been seen and has been resolved (status = Done _). In this case
     *   we reuse the cached result.
     *
     * - The tvar is in the process of resolution (status = Started).
     *   These are types of the form:
     *
     *     type X = X | number
     *              ^
     *   we consider the recursive occurence as uncovered (Any). This case should
     *   be rare and it's arguable if we should be allowing it in the first place,
     *   so we assign the value that corresponds to the fewest guarantees.
     *)
    val mutable tvar_cache : tvar_status IMap.t = IMap.empty

    method private tvar cx id =
      let (root_id, constraints) = Context.find_constraints cx id in
      if id != root_id then
        self#tvar cx root_id
      else
        match IMap.find_opt root_id tvar_cache with
        | Some Started -> Kind.Any
        | Some (Done cov) -> cov
        | None ->
          tvar_cache <- IMap.add root_id Started tvar_cache;
          Constraint.(
            let cov =
              match constraints with
              | Resolved t
              | FullyResolved (lazy t) ->
                self#type_ cx t
              | Unresolved bounds ->
                let bounds = TypeMap.keys bounds.lower in
                self#types_list cx OpOr bounds
            in
            tvar_cache <- IMap.add root_id (Done cov) tvar_cache;
            cov
          )

    method type_ cx =
      function
      | OpenT (_, id) -> self#tvar cx id
      | EvalT (t, _, id) -> self#eval_t cx t id
      (* Non-concrete (fallthrough) constructors *)
      | AnnotT (_, t, _)
      | ExactT (_, t)
      | DefT (_, PolyT { t_out = t; _ })
      | TypeAppT { reason = _; use_op = _; type_ = t; targs = _; from_value = _; use_desc = _ }
      | DefT (_, TypeT (_, t))
      | GenericT { bound = t; _ }
      | ThisTypeAppT (_, t, _, _) ->
        self#type_ cx t
      | UnionT (_, rep) ->
        let (t0, (t1, ts)) = UnionRep.members_nel rep in
        self#types_nel cx OpOr (t0, t1 :: ts)
      | IntersectionT (_, rep) ->
        let (t0, (t1, ts)) = InterRep.members_nel rep in
        self#types_nel cx OpAnd (t0, t1 :: ts)
      (* Concrete covered constructors *)
      | CustomFunT _
      | FunProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _
      | InternalT _
      | ThisInstanceT _
      | KeysT _
      | MaybeT _
      | ModuleT _
      | NamespaceT _
      | NullProtoT _
      | OpaqueT _
      | ObjProtoT _
      | OptionalT _ ->
        Kind.Checked
      | DefT (_, ArrT _)
      | DefT (_, BigIntT _)
      | DefT (_, BoolT _)
      | DefT (_, CharSetT _)
      | DefT (_, ClassT _)
      | DefT (_, FunT _)
      | DefT (_, InstanceT _)
      | DefT (_, MixedT _)
      | DefT (_, NumT _)
      | DefT (_, NullT)
      | DefT (_, SymbolT)
      | DefT (_, ObjT _)
      | DefT (_, ReactAbstractComponentT _)
      | DefT (_, RendersT _)
      | DefT (_, NumericStrKeyT _)
      | DefT (_, SingletonNumT _)
      | DefT (_, SingletonStrT _)
      | DefT (_, SingletonBigIntT _)
      | DefT (_, SingletonBoolT _)
      | DefT (_, StrT _)
      | DefT (_, VoidT)
      | DefT (_, EnumObjectT _)
      | DefT (_, EnumT _) ->
        Kind.Checked
      (* Concrete uncovered constructors *)
      (* TODO: Rethink coverage for these types *)
      | MatchingPropT _ -> Kind.Empty
      | DefT (_, EmptyT) -> Kind.Empty
      | AnyT _ -> Kind.Any

    method private types_of_use acc =
      function
      | UseT (_, t) -> t :: acc
      | ReposLowerT (_, _, u) -> self#types_of_use acc u
      | _ -> acc

    method private merged_t cx uses =
      let ts = List.fold_left self#types_of_use [] uses in
      self#types_list cx OpAnd ts

    method private eval_t cx t id =
      let evaluated = Context.evaluated cx in
      let t =
        match Eval.Map.find_opt id evaluated with
        | Some cached -> cached
        | None -> t
      in
      self#type_ cx t

    method private types_ cx op acc =
      function
      | [] -> acc
      | t :: ts ->
        let cov = self#type_ cx t in
        let merged_kind = Kind.merge op (cov, acc) in
        begin
          match merged_kind with
          | Kind.Any ->
            (* Cannot recover from Any, so exit early *)
            Kind.Any
          | Kind.Checked
          | Kind.Empty ->
            self#types_ cx op merged_kind ts
        end

    method private types_list cx op ts =
      match ts with
      | [] -> Kind.Empty
      | t :: ts -> self#types_nel cx op (t, ts)

    method private types_nel cx op (t, ts) =
      let init_kind = self#type_ cx t in
      match init_kind with
      | Kind.Any -> Kind.Any
      | Kind.Checked
      | Kind.Empty ->
        self#types_ cx op init_kind ts
  end

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
      let open Ast.Expression.Object in
      match prop with
      | ( loc,
          Property.Method
            {
              key =
                ( Property.Identifier ((_, t), _)
                | Property.StringLiteral ((_, t), _)
                | Property.NumberLiteral ((_, t), _)
                | Property.BigIntLiteral ((_, t), _) );
              _;
            }
        ) ->
        acc <- f loc t acc;
        prop
      | _ -> prop

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
            }
        )
      | (loc, Ast.Statement.DeclareInterface { Ast.Statement.Interface.id = ((_, t), _); _ })
      | ( loc,
          Ast.Statement.DeclareModule
            {
              Ast.Statement.DeclareModule.id =
                ( Ast.Statement.DeclareModule.Identifier ((_, t), _)
                | Ast.Statement.DeclareModule.Literal ((_, t), _) );
              _;
            }
        )
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
    method! jsx_element_name name =
      let open Ast.JSX in
      let name = super#jsx_element_name name in
      match name with
      | MemberExpression (loc, { MemberExpression.property = ((_, t), _); _ }) ->
        acc <- f loc t acc;
        name
      | Identifier _
      | NamespacedName _ ->
        name

    method! jsx_member_expression_object _object =
      let open Ast.JSX.MemberExpression in
      match _object with
      | Identifier ((loc, t), _) ->
        acc <- f loc t acc;
        _object
      | MemberExpression _ -> super#jsx_member_expression_object _object

    method! import_named_specifier ~import_kind specifier =
      let open Ast.Statement.ImportDeclaration in
      ignore import_kind;
      let { kind; local; remote; remote_name_def_loc } = specifier in
      let local' = Base.Option.map ~f:this#pattern_identifier local in
      let remote' = this#pattern_identifier remote in
      { kind; local = local'; remote = remote'; remote_name_def_loc }

    method! pattern_identifier ?kind i =
      let ((loc, t), _) = i in
      acc <- f loc t acc;
      super#pattern_identifier ?kind i

    method top_level_program prog =
      acc <- init;
      ignore (this#program prog);
      acc
  end

let coverage_fold_tast ~(f : 'l -> 't -> 'acc -> 'acc) ~(init : 'acc) tast =
  let folder = new coverage_folder ~f ~init in
  folder#top_level_program tast

let covered_types ~should_check cx tast =
  let compute_cov =
    if should_check then
      visitor#type_ cx
    else
      fun _ ->
    Kind.Empty
  in
  let step loc t acc = (ALoc.to_loc_exn loc, compute_cov t) :: acc in
  coverage_fold_tast ~f:step ~init:[] tast |> List.sort (fun (a, _) (b, _) -> Loc.compare a b)

let file_coverage ~cx tast =
  let open Kind in
  let step _ t acc =
    match visitor#type_ cx t with
    | Any -> { acc with uncovered = acc.uncovered + 1 }
    | Checked -> { acc with checked = acc.checked + 1 }
    | Empty -> { acc with empty = acc.empty + 1 }
  in
  coverage_fold_tast ~f:step ~init:initial_coverage tast
