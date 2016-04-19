(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Type

(* We walk types in a lot of places for all kinds of things, but often most of
   the code is boilerplate. The following visitor class for types aims to
   reduce that boilerplate. It is designed as a fold on the structure of types,
   parameterized by an accumulator.

   WARNING: This is only a partial implementation, sufficient for current
   purposes but intended to be completed in a later diff.
*)
class ['a] t = object(self)
  method type_ cx (acc: 'a) = function
  | OpenT (r, id) -> self#tvar cx acc r id

  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | TaintT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | CustomFunT _
    -> acc

  | FunT (_, static, prototype, funtype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc prototype in
    let acc = self#fun_type cx acc funtype in
    acc

  | ObjT (_, { dict_t; props_tmap; proto_t; _ }) ->
    let acc = self#opt (self#dict_ cx) acc dict_t in
    let acc = self#props cx acc props_tmap in
    let acc = self#type_ cx acc proto_t in
    acc

  | ArrT (_, t, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | ClassT t -> self#type_ cx acc t

  | InstanceT (_, static, super, insttype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc super in
    let acc = self#inst_type cx acc insttype in
    acc

  | OptionalT t -> self#type_ cx acc t

  | RestT t -> self#type_ cx acc t

  | AbstractT t -> self#type_ cx acc t

  | EvalT (t, defer_use_t, id) ->
    let acc = self#type_ cx acc t in
    let acc = self#defer_use_type cx acc defer_use_t in
    let acc = self#eval_id cx acc id in
    acc

  | PolyT (typeparams, t) ->
    let acc = self#list (self#type_param cx) acc typeparams in
    let acc = self#type_ cx acc t in
    acc

  | TypeAppT (t, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | ThisClassT t -> self#type_ cx acc t

  | ThisTypeAppT (t, this, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#type_ cx acc this in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | BoundT typeparam -> self#type_param cx acc typeparam

  | ExistsT _ -> acc

  | MaybeT t -> self#type_ cx acc t

  | IntersectionT (_, rep) ->
    self#list (self#type_ cx) acc (InterRep.members rep)

  | UnionT (_, rep) ->
    self#list (self#type_ cx) acc (UnionRep.members rep)

  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t -> self#type_ cx acc t

  | AnyObjT _
  | AnyFunT _ -> acc

  | ShapeT t -> self#type_ cx acc t

  | DiffT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | KeysT (_, t) -> self#type_ cx acc t

  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _ -> acc

  | TypeT (_, t) -> self#type_ cx acc t

  | AnnotT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | SpeculativeMatchT (_, t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#use_type_ cx acc t2 in
    acc

  | ModuleT (_, exporttypes) ->
    self#export_types cx acc exporttypes

  | ReposUpperT (_, t) ->
    self#type_ cx acc t

  | ExtendsT (ts, t1, t2) ->
    let acc = self#list (self#type_ cx) acc ts in
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  method private defer_use_type cx acc = function
  | DestructuringT (_, s) -> self#selector cx acc s

  method private selector cx acc = function
  | Prop _ -> acc
  | Elem key -> self#type_ cx acc key
  | ObjRest _ -> acc
  | ArrRest _ -> acc
  | Default -> acc

  method private use_type_ cx acc = function
  | UseT t ->
    self#type_ cx acc t
  (* Currently not walking use types. This will change in an upcoming diff. *)
  | SummarizeT (_, _)
  | ApplyT (_, _, _)
  | BindT (_, _)
  | CallT (_, _)
  | MethodT (_, _, _)
  | BecomeT (_, _)
  | ReposLowerT (_, _)
  | SetPropT (_, _, _)
  | GetPropT (_, _, _)
  | SetElemT (_, _, _)
  | GetElemT (_, _, _)
  | ConstructorT (_, _, _)
  | SuperT (_, _)
  | MixinT (_, _)
  | AdderT (_, _, _)
  | ComparatorT (_, _)
  | PredicateT (_, _)
  | EqT (_, _)
  | AndT (_, _, _, _)
  | OrT (_, _, _, _)
  | NotT (_, _)
  | ReifyTypeT (_, _)
  | SpecializeT (_, _, _, _)
  | ThisSpecializeT (_, _, _)
  | VarianceCheckT (_, _, _)
  | LookupT (_, _, _, _, _)
  | ObjAssignT (_, _, _, _, _)
  | ObjFreezeT (_, _)
  | ObjRestT (_, _, _)
  | ObjSealT (_, _)
  | ObjTestT (_, _, _)
  | ArrRestT (_, _, _)
  | UnaryMinusT (_, _)
  | UnifyT (_, _)
  | ConcretizeLowerT (_, _, _, _)
  | ConcretizeUpperT (_, _, _, _)
  | GetKeysT (_, _)
  | HasOwnPropT (_, _)
  | HasPropT (_, _, _)
  | ElemT (_, _, _)
  | CJSRequireT (_, _)
  | ImportModuleNsT (_, _)
  | ImportDefaultT (_, _, _, _)
  | ImportNamedT (_, _, _, _)
  | ImportTypeT (_, _, _)
  | ImportTypeofT (_, _, _)
  | AssertImportIsValueT (_, _)
  | CJSExtractNamedExportsT (_, _, _)
  | ExportNamedT (_, _, _)
  | ExportStarFromT (_, _, _)
  | DebugPrintT (_)
  | TupleMapT (_, _, _)
    -> self#__TODO__ cx acc

  (* The default behavior here could be fleshed out a bit, to look up the graph,
     handle Resolved and Unresolved cases, etc. *)
  method tvar _cx acc _r _id = acc

  method private dict_ cx acc { key; value; _ } =
    let acc = self#type_ cx acc key in
    let acc = self#type_ cx acc value in
    acc

  method props cx acc id =
    Context.property_maps cx
    |> IMap.find_unsafe id
    |> self#smap (self#type_ cx) acc

  method private eval_id cx acc id =
    match IMap.get id (Context.evaluated cx) with
    | None -> acc
    | Some t -> self#type_ cx acc t

  method private type_param cx acc { bound; _ } =
    self#type_ cx acc bound

  method fun_type cx acc { this_t; params_tlist; return_t; _ } =
    let acc = self#type_ cx acc this_t in
    let acc = self#list (self#type_ cx) acc params_tlist in
    let acc = self#type_ cx acc return_t in
    acc

  method private inst_type cx acc { type_args; fields_tmap; methods_tmap; _ } =
    let acc = self#smap (self#type_ cx) acc type_args in
    let acc = self#props cx acc fields_tmap in
    let acc = self#props cx acc methods_tmap in
    acc

  method private export_types cx acc { exports_tmap; cjs_export } =
    let acc = self#props cx acc exports_tmap in
    let acc = self#opt (self#type_ cx) acc cjs_export in
    acc

  method private __TODO__ _cx acc = acc

  method private list: 't. ('a -> 't -> 'a) -> 'a -> 't list -> 'a =
    List.fold_left

  method private opt: 't. ('a -> 't -> 'a) -> 'a -> 't option -> 'a =
    fun f acc -> function
    | None -> acc
    | Some x -> f acc x

  method private smap: 't. ('a -> 't -> 'a) -> 'a -> 't SMap.t -> 'a =
    fun f acc map ->
      SMap.fold (fun _ t acc -> f acc t) map acc
end
