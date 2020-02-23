(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
 *  - AnyWithLowerBoundT
 *  - AnyWithUpperBoundT
 *  - BoundT
 *  - KeysT
 *  - ExistsT
 *
 * In addition to being considered concrete the above constructors are also
 * considered covered.
 *)

open Type
open Utils_js
module Ast = Flow_ast

type op_mode =
  | OpAnd
  | OpOr

let unit_of_op = function
  | OpAnd -> true (* mixed *)
  | OpOr -> false

(* empty *)

module Taint = struct
  type t =
    | Untainted
    | Tainted

  let of_trust cx tr =
    if Trust_helpers.actual_trust cx tr |> Trust.is_tainted then
      Tainted
    else
      Untainted

  let to_string = function
    | Untainted -> "Untainted"
    | Tainted -> "Tainted"

  let m_and = function
    | (Tainted, t)
    | (t, Tainted) ->
      t
    | (Untainted, Untainted) -> Untainted

  let m_or = function
    | (Tainted, _)
    | (_, Tainted) ->
      Tainted
    | (Untainted, Untainted) -> Untainted

  let to_bool = function
    | Untainted -> true
    | Tainted -> false

  let merge = function
    | OpAnd -> m_and
    | OpOr -> m_or
end

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

let merge op ((k1, t1), (k2, t2)) = (Kind.merge op (k1, k2), Taint.merge op (t1, t2))

type tvar_status =
  | Started
  | Done of (Kind.t * Taint.t)

class visitor =
  object (self)
    val mutable tvar_cache : tvar_status IMap.t = IMap.empty
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

    method private tvar cx id =
      let (root_id, constraints) = Context.find_constraints cx id in
      if id != root_id then
        self#tvar cx root_id
      else
        match IMap.find_opt root_id tvar_cache with
        | Some Started -> (Kind.Any, Taint.Tainted)
        | Some (Done cov) -> cov
        | None ->
          tvar_cache <- IMap.add root_id Started tvar_cache;
          Constraint.(
            let cov =
              match constraints with
              | Resolved (_, t)
              | FullyResolved (_, t) ->
                self#type_ cx t
              | Unresolved bounds ->
                let bounds = TypeMap.keys bounds.lower in
                self#types_list cx OpOr bounds
            in
            tvar_cache <- IMap.add root_id (Done cov) tvar_cache;
            cov)

    method type_ cx =
      function
      | OpenT (_, id) -> self#tvar cx id
      | MergedT (_, uses) -> self#merged_t cx uses
      | EvalT (t, _, id) -> self#eval_t cx t id
      (* Non-concrete (fallthrough) constructors *)
      | AnnotT (_, t, _)
      | ExactT (_, t)
      | DefT (_, _, PolyT { t_out = t; _ })
      | TypeAppT (_, _, t, _)
      | DefT (_, _, TypeT (_, t))
      | OpenPredT { base_t = t; m_pos = _; m_neg = _; reason = _ }
      | ReposT (_, t)
      | ShapeT (_, t)
      | ThisClassT (_, t)
      | ThisTypeAppT (_, t, _, _) ->
        self#type_ cx t
      | UnionT (_, rep) ->
        let (t0, (t1, ts)) = UnionRep.members_nel rep in
        self#types_nel cx OpOr (t0, t1 :: ts)
      | IntersectionT (_, rep) ->
        let (t0, (t1, ts)) = InterRep.members_nel rep in
        self#types_nel cx OpAnd (t0, t1 :: ts)
      (* Concrete covered constructors *)
      | BoundT _
      | CustomFunT _
      | ExistsT _
      | FunProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _
      | InternalT _
      | KeysT _
      | MaybeT _
      | ModuleT _
      | NullProtoT _
      | OpaqueT _
      | ObjProtoT _
      | OptionalT _ ->
        (Kind.Checked, Taint.Untainted)
      | DefT (_, t, ArrT _)
      | DefT (_, t, BoolT _)
      | DefT (_, t, CharSetT _)
      | DefT (_, t, ClassT _)
      | DefT (_, t, FunT _)
      | DefT (_, t, InstanceT _)
      | DefT (_, t, IdxWrapper _)
      | DefT (_, t, MixedT _)
      | DefT (_, t, NumT _)
      | DefT (_, t, NullT)
      | DefT (_, t, SymbolT)
      | DefT (_, t, ObjT _)
      | DefT (_, t, ReactAbstractComponentT _)
      | DefT (_, t, SingletonNumT _)
      | DefT (_, t, SingletonStrT _)
      | DefT (_, t, SingletonBoolT _)
      | DefT (_, t, StrT _)
      | DefT (_, t, VoidT)
      | DefT (_, t, EnumObjectT _)
      | DefT (_, t, EnumT _) ->
        (Kind.Checked, Taint.of_trust cx t)
      (* Concrete uncovered constructors *)
      (* TODO: Rethink coverage and trust for these types *)
      | MatchingPropT _
      | TypeDestructorTriggerT _ ->
        (Kind.Empty, Taint.Untainted)
      | DefT (_, t, EmptyT _) -> (Kind.Empty, Taint.of_trust cx t)
      | AnyT _ -> (Kind.Any, Taint.Tainted)

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
        let ((merged_kind, _) as merged) = merge op (cov, acc) in
        begin
          match merged_kind with
          | Kind.Any ->
            (* Cannot recover from Any, so exit early *)
            (Kind.Any, Taint.Tainted)
          | Kind.Checked
          | Kind.Empty ->
            self#types_ cx op merged ts
        end

    method private types_list cx op ts =
      match ts with
      | [] -> (Kind.Empty, Taint.Tainted)
      | t :: ts -> self#types_nel cx op (t, ts)

    method private types_nel cx op (t, ts) =
      let (init_kind, init_trust) = self#type_ cx t in
      match init_kind with
      | Kind.Any -> (Kind.Any, Taint.Tainted)
      | Kind.Checked
      | Kind.Empty ->
        self#types_ cx op (init_kind, init_trust) ts
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
            { key = Property.Literal ((_, t), _) | Property.Identifier ((_, t), _); _ } ) ->
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
      let open Ast.JSX in
      let name = super#jsx_name name in
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

open Coverage_response

let result_of_coverage = function
  | (Kind.Any, Taint.Untainted) ->
    assert_false "Any coverage kind cannot be associated with untainted"
  | (Kind.Any, _) -> Uncovered
  | (Kind.Empty, _) -> Empty
  | (Kind.Checked, Taint.Tainted) -> Tainted
  | (Kind.Checked, Taint.Untainted) -> Untainted

let to_bool = function
  | Empty
  | Uncovered ->
    false
  | Tainted
  | Untainted ->
    true

let m_or = function
  | (Uncovered, _)
  | (_, Uncovered) ->
    Uncovered
  | (Empty, m2)
  | (Untainted, m2) ->
    m2
  | (m1, Empty)
  | (m1, Untainted) ->
    m1
  | (Tainted, Tainted) -> Tainted

let initial_coverage = { untainted = 0; tainted = 0; uncovered = 0; empty = 0 }

let covered_types ~should_check ~check_trust cx tast =
  let check_trust =
    if check_trust then
      fun x ->
    x
    else
      function
    | Coverage_response.Tainted -> Coverage_response.Untainted
    | x -> x
  in
  let compute_cov =
    if should_check then
      (new visitor)#type_ cx %> result_of_coverage %> check_trust
    else
      fun _ ->
    Coverage_response.Empty
  in
  let step loc t acc = (ALoc.to_loc_exn loc, compute_cov t) :: acc in
  coverage_fold_tast ~f:step ~init:[] tast |> List.sort (fun (a, _) (b, _) -> Loc.compare a b)

let file_coverage :
    full_cx:Context.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
    Coverage_response.file_coverage =
  let coverage_computer = new visitor in
  let step cx _ t acc =
    let coverage = coverage_computer#type_ cx t in
    match result_of_coverage coverage with
    | Uncovered -> { acc with uncovered = acc.uncovered + 1 }
    | Untainted -> { acc with untainted = acc.untainted + 1 }
    | Tainted -> { acc with tainted = acc.tainted + 1 }
    | Empty -> { acc with empty = acc.empty + 1 }
  in
  fun ~full_cx ->
    let step = step full_cx in
    coverage_fold_tast ~f:step ~init:initial_coverage
