(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module defines the ML data types that represent types in Flow. *)

open Utils
open Modes_js
open Reason_js

let assert_false s =
  let callstack = Printexc.(get_callstack 10 |> raw_backtrace_to_string) in
  prerr_endline (spf "<<<<<<<<<\n%s:\n%s>>>>>>>>>" s callstack);
  failwith s

let __DEBUG__ ?(s="") f =
  try f() with _ -> assert_false s

module Ast = Spider_monkey_ast

(******************************************************************************)
(* Types                                                                      *)
(******************************************************************************)

(* Some types represent definitions. These include numbers, strings, booleans,
   functions, classes, objects, arrays, and such. The shape of these types
   should be fairly obvious.
   Other types represent uses. These include function applications, class
   instantiations, property accesses, element accesses, operations such as
   addition, predicate refinements, etc. The shape of these types is somewhat
   trickier, but do follow a pattern. Typically, such a type consists of the
   arguments to the operation, and a type variable capturing the result of the
   operation. A full understanding of the semantics of such types requires a
   look at the subtyping relation, described in the module Flow_js. *)

(* Every type has (or should have, if not already) a "reason" for its
   existence. This information is captured in the type itself for now, but
   should be separated out in the future.
   Types that represent definitions point to the positions of such
   definitions (or values). Types that represent uses point to the positions of
   such uses (or operations). These reasons are logged, chained, etc. by the
   implementation of the subtyping algorithm, that effectively constructs a
   proof of the typing derivation based on these reasons as axioms. *)

type ident = int
type name = string

module Type = struct
  type t =
  | OpenT of reason * ident

  (*************)
  (* def types *)
  (*************)

  (* TODO: constant types *)

  | NumT of reason * literal
  | StrT of reason * literal
  | BoolT of reason
  | UndefT of reason
  | MixedT of reason
  | AnyT of reason
  | NullT of reason
  | VoidT of reason

  | FunT of reason * static * prototype * funtype
  | ObjT of reason * objtype
  | ArrT of reason * t * t list

  | ClassT of t
  | InstanceT of reason * static * super * insttype

  | OptionalT of t
  | RestT of t

  | PolyT of typeparam list * t
  | TypeAppT of t * t list
  | BoundT of typeparam

  | MaybeT of t

  | IntersectionT of reason * t list

  | UnionT of reason * t list

  | UpperBoundT of t
  | LowerBoundT of t

  | EnumT of reason * t
  | RecordT of reason * t

  | CustomClassT of name * t list * t

  | TypeT of reason * t

  (* forcing a list of types to be concretized *)
  | ConcretizeT of reason * t list * t * t
  (* sufficiently concrete type *)
  | ConcreteT of t

  (*************)
  (* use types *)
  (*************)

  (* operation on literals *)
  | SummarizeT of reason * t

  (* operations on runtime values, such as functions, objects, and arrays *)
  | CallT of reason * funtype
  | MethodT of reason * name * t * t list * t * int
  | SetT of reason * name * t
  | GetT of reason * name * t
  | SetElemT of reason * t * t
  | GetElemT of reason * t * t

  (* operations on runtime types, such as classes and functions *)
  | ConstructorT of reason * t list * t
  | SuperT of reason * insttype
  | ParentT of reason * insttype

  (* overloaded +, could be subsumed by general overloading *)
  | AdderT of reason * t * t
  (* overloaded relational operator, could be subsumed by general overloading *)
  | ComparatorT of reason * t

  (* operation specifying a type refinement via a predicate *)
  | PredicateT of predicate * t

  (* == *)
  | EqT of reason * t

  (* operation on polymorphic types *)
  | SpecializeT of reason * t list * t

  (* operation on prototypes *)
  | LookupT of reason * reason option * string * t

  (* JSX *)
  | MarkupT of reason * t * t

  (* operations on objects *)
  | ObjAssignT of reason * t * t
  | ObjRestT of reason * string list * t
  | ObjExtendT of reason * t SMap.t * t

  (* Guarded unification (bidirectional).
     Remodel as unidirectional GuardT(l,u)? *)
  | UnifyT of t * t

  (* Keys *)
  | KeyT of reason * t
  | HasT of reason * string

  (* Element access *)
  | ElemT of reason * t * t

  and predicate =
  | AndP of predicate * predicate
  | OrP of predicate * predicate
  | NotP of predicate

  (* truthy *)
  | ExistsP

   (* instanceof *)
  | InstanceofP of t
  | ConstructorP of t

  (* typeof, null check, Array.isArray *)
  | IsP of string

  and literal = string option

  (* used by FunT and CallT *)
  and funtype = {
    this_t: t;
    params_tlist: t list;
    params_names: string list option;
    return_t: t;
    closure_t: int
  }

  and objtype = {
    sealed: bool;
    dict_t: t * t;
    props_tmap: int;
    proto_t: prototype
  }

  and insttype = {
    class_id: ident;
    type_args: t IMap.t;
    fields_tmap: fields;
    methods_tmap: methods
  }

  and typeparam = {
    reason: reason;
    id: ident;
    name: string;
  }

  and prototype = t

  and super = t

  and static = t

  and fields = t SMap.t

  and methods = t SMap.t

  let compare = Pervasives.compare

  let open_tvar tvar =
    match tvar with
    | OpenT(reason,id) -> (reason,id)
    | _ -> assert false

  let mk_predicate (p,t) =
    PredicateT(p,t)

end

(* The typechecking algorithm often needs to maintain sets of types. In
   addition, for logging we need to associate some provenanced information to
   types. Both these purposes are served by using type maps. *)

module TypeMap : MapSig with type key = Type.t
  = MyMap(Type)

(*****************************************************************)


type rule =
  | FunThis
  | FunArg of int * int
  | FunRet
  | ObjProp of string
  | ObjKey
  | ArrIndex
  | ArrElem
  | DecomposeNullable
  | InstantiatePoly
  | ClassInst
  | FunInst
  | FunProto
  | CopyProto
  | InstanceProp of string
  | ObjProto
  | StrIndex
  | StrElem
  | InstanceSuper
  | Op of string
  | ReactProps
  | ReactComponent
  | LibMethod of string
  | FunStatics
  | ClassStatics

(* These strings should be read as answering *)
let string_of_rule = function
  | FunThis -> "this type of function type"
  | FunArg (i, j) -> spf "arg %d ~ param %d type of function type flow" i j
  | FunRet -> "return type of function type"
  | ObjProp x -> spf "type of property %s of object type" x
  | ObjKey -> "key type of object type"
  | ArrIndex -> "index type of array type"
  | ArrElem -> "element type of array type"
  | DecomposeNullable -> "base type of nullable type"
  | InstantiatePoly -> "specialization of polymorphic type"
  | ClassInst -> "instance type of class type"
  | FunInst -> "instance type of function type"
  | FunProto -> "prototype type of function type"
  | CopyProto -> "[INTERNAL] copy object type to prototype type"
  | InstanceProp x -> spf "type of property %s of instance type" x
  | ObjProto -> "prototype type of object type"
  | StrIndex -> "index type of string type"
  | StrElem -> "element type of string type"
  | InstanceSuper -> "super type of instance type"
  | Op x -> spf "type of operation %s" x
  | ReactProps -> "properties type of React class"
  | ReactComponent -> "component type of React class"
  | LibMethod x -> spf "type of %s method" x
  | FunStatics -> "statics type of function type"
  | ClassStatics -> "statics type of class type"

type link =
  (* intermediate node, expect it to be a tvar *)
  | Node of Type.t

  (* derived node, expect it to be the decomposition of a type structure *)
  | Embed of rule * trace

and trace = Type.t * link list * Type.t

(* ------------- *)

(* The following functions are used to build larger traces from their
   subparts. The "leaves" of a trace are the reasons associated with the types
   of definitions and uses in code. The "internal nodes" are caused by (1)
   transitively propagating the effects of a definition to a use through
   statements in the code, such as assignments; and (2) by subtyping rules that
   simplify a constraint between higher-order types into constraints involving
   their subparts, such as propagating an argument of a function call to a
   parameter of a function. *)

let unit_trace tl tu = (tl, [], tu)

let is_empty_trace = function
  | (tl, [], tu) -> tl = tu
  | _ -> false

let join_trace (tl, cl, ti) (_, cu, tu) =
  let links = if modes.traces_enabled  || modes.newtraces_enabled
    then (cl @ cu)
    else [] in
  (tl, links, tu)

let concat_trace traces =
  List.fold_left join_trace (List.hd traces) (List.tl traces)

let select_trace rl ru trace rule =
  if modes.traces_enabled || modes.newtraces_enabled
    then (rl, [Embed (rule, trace)], ru)
    else trace

(*****************************************************************)



open Type

(* type constants *)

module type PrimitiveType = sig
  val desc: string
  val make: reason -> Type.t
end

module type PrimitiveT = sig
  val desc: string
  val t: Type.t
  val at: Spider_monkey_ast.Loc.t -> Type.t
  val why: reason -> Type.t
  val tag: string -> Type.t
end

module Primitive (P: PrimitiveType) = struct
  let desc = P.desc
  let t = P.make (reason_of_string desc)
  let at tok = P.make (mk_reason desc tok)
  let why reason = P.make (replace_reason desc reason)
  let tag s = P.make (reason_of_string (desc ^ " (" ^ s ^ ")"))
end

module NumT = Primitive (struct
  let desc = "number"
  let make r = NumT (r, None)
end)

module StrT = Primitive (struct
  let desc = "string"
  let make r = StrT (r, None)
end)

module BoolT = Primitive (struct
  let desc = "boolean"
  let make r = BoolT r
end)

module MixedT = Primitive (struct
  let desc = "mixed"
  let make r = MixedT r
end)

module UndefT = Primitive (struct
  let desc = ""
  let make r = UndefT r
end)

module AnyT = Primitive (struct
  let desc = "any"
  let make r = AnyT r
end)

module VoidT = Primitive (struct
  let desc = "undefined"
  let make r = VoidT r
end)

module NullT = Primitive (struct
  let desc = "null"
  let make r = NullT r
end)

(* type bounds *)

(* A unifier is a type variable that is either the root of a set of unified type
   variables, in which case it has a rank, or points to another unifier *)
type unifier =
| Goto of ident
| Rank of int

type bounds = {
  mutable lower: trace TypeMap.t;
  mutable upper: trace TypeMap.t;
  mutable lowertvars: trace IMap.t;
  mutable uppertvars: trace IMap.t;

  (* indicates whether the type variable is a unifier *)
  mutable unifier: unifier option;
  (* indicates whether the type variable is resolved to some type *)
  mutable solution: Type.t option;
}

let new_bounds id reason =
  let tvar = OpenT (reason, id) in {
  lower = TypeMap.empty;
  upper = TypeMap.empty;
  lowertvars = IMap.singleton id (unit_trace tvar tvar);
  uppertvars = IMap.singleton id (unit_trace tvar tvar);
  unifier = None;
  solution = None;
}

type block_entry = {
  specific: Type.t;
  general: Type.t;
  def_loc: Spider_monkey_ast.Loc.t option;
}
type block = block_entry SMap.t ref
type stack = int list

let create_env_entry specific general loc =
  { specific;
    general;
    def_loc = loc; }

(* type context *)

(* The following fields can be pulled out of context :) and cached.  However,
   caching these may not help a lot yet since, in comparison to the other
   (non-cacheable) parts of the context, their sizes are quite small and their
   accesses not much more frequent.

   Basically for caching to win we must have:
     R1 * S1 >> R2 * S2 + (S1 - S2)
   where
     R1 is number of context reads
     S1 is size of context

     R2 is number of non-cacheable context reads
     S2 is size of non-cacheable context
*)

(*
type cacheable_context = {
  file: string;
  _module: string;
  required: Utils.SSet.t;
  modulemap: Type.t Utils.SMap.t;
  strict_required: Utils.SSet.t;
}
*)

type context = {
  file: string;
  _module: string;
  mutable checked: bool;
  mutable weak: bool;
  mutable required: SSet.t;
  mutable require_loc: Ast.Loc.t SMap.t;

  mutable graph: bounds IMap.t;
  mutable parents: Type.t IMap.t IMap.t IMap.t;
  mutable closures: (stack * block list) IMap.t;
  mutable property_maps: Type.t SMap.t IMap.t;
  mutable modulemap: Type.t SMap.t;

  mutable strict_required: SSet.t;

  mutable errors: Errors_js.ErrorSet.t;
  mutable globals: SSet.t;

  type_table: (Spider_monkey_ast.Loc.t, Type.t) Hashtbl.t;
  annot_table: (Pos.t, Type.t) Hashtbl.t;
}

let new_context file _module = {
  file = file;
  _module = _module;
  checked = false;
  weak = false;
  required = SSet.empty;
  require_loc = SMap.empty;

  graph = IMap.empty;
  parents = IMap.empty;
  closures = IMap.empty;
  property_maps = IMap.empty;
  modulemap = SMap.empty;

  strict_required = SSet.empty;

  errors = Errors_js.ErrorSet.empty;
  globals = SSet.empty;

  type_table = Hashtbl.create 0;
  annot_table = Hashtbl.create 0;
}

(********************************************************************)

(* def types vs. use types *)
let is_use = function
  | CallT _
  | MethodT _
  | SetT _
  | GetT _
  | SetElemT _
  | GetElemT _
  | ConstructorT _
  | SuperT _
  | ParentT _
  | AdderT _
  | ComparatorT _
  | PredicateT _
  | EqT _
  | SpecializeT _
  | LookupT _
  | MarkupT _
  | ObjAssignT _
  | ObjRestT _
  | ObjExtendT _
  | UnifyT _
  | KeyT _
  | HasT _
  | ElemT _
    -> true

  | _ -> false


(********************************************************************)

(* printing *)

let string_of_ctor = function
  | OpenT _ -> "OpenT"
  | NumT _ -> "NumT"
  | StrT _ -> "StrT"
  | BoolT _ -> "BoolT"
  | UndefT _ -> "UndefT"
  | MixedT _ -> "MixedT"
  | AnyT _ -> "AnyT"
  | NullT _ -> "NullT"
  | VoidT _ -> "VoidT"
  | FunT _ -> "FunT"
  | PolyT _ -> "PolyT"
  | BoundT _ -> "BoundT"
  | ObjT _ -> "ObjT"
  | ArrT _ -> "ArrT"
  | ClassT _ -> "ClassT"
  | InstanceT _ -> "InstanceT"
  | SummarizeT _ -> "SummarizeT"
  | SuperT _ -> "SuperT"
  | ParentT _ -> "ParentT"
  | CallT _ -> "CallT"
  | MethodT _ -> "MethodT"
  | SetT _ -> "SetT"
  | GetT _ -> "GetT"
  | SetElemT _ -> "SetElemT"
  | GetElemT _ -> "GetElemT"
  | ConstructorT _ -> "ConstructorT"
  | AdderT _ -> "AdderT"
  | ComparatorT _ -> "ComparatorT"
  | TypeT _ -> "TypeT"
  | OptionalT _ -> "OptionalT"
  | RestT _ -> "RestT"
  | PredicateT _ -> "PredicateT"
  | EqT _ -> "EqT"
  | MarkupT _ -> "MarkupT"
  | SpecializeT _ -> "SpecializeT"
  | TypeAppT _ -> "TypeAppT"
  | MaybeT _ -> "MaybeT"
  | IntersectionT _ -> "IntersectionT"
  | UnionT _ -> "UnionT"
  | LookupT _ -> "LookupT"
  | UnifyT _ -> "UnifyT"
  | ObjAssignT _ -> "ObjAssignT"
  | ObjRestT _ -> "ObjRestT"
  | ObjExtendT _ -> "ObjExtendT"
  | UpperBoundT _ -> "UpperBoundT"
  | LowerBoundT _ -> "LowerBoundT"
  | EnumT _ -> "EnumT"
  | RecordT _ -> "RecordT"
  | KeyT _ -> "KeyT"
  | HasT _ -> "HasT"
  | ElemT _ -> "ElemT"
  | ConcretizeT _ -> "ConcretizeT"
  | ConcreteT _ -> "ConcreteT"
  | CustomClassT _ -> "CustomClassT"

(* Usually types carry enough information about the "reason" for their
   existence (e.g., position in code, introduction/elimination rules in
   the type system), so printing the reason provides a good idea of what the
   type means to the programmer. *)

let rec reason_of_t = function
  (* note: keep in order of decls in constraint_js *)

  | OpenT (reason,_)

  | NumT (reason, _)
  | StrT (reason, _)
  | BoolT reason
  | UndefT reason
  | MixedT reason
  | AnyT reason
  | NullT reason
  | VoidT reason

  | FunT (reason,_,_,_)
      -> reason

  | PolyT (_,t) ->
      prefix_reason "polymorphic type: " (reason_of_t t)
  | BoundT typeparam ->
      typeparam.reason

  | ObjT (reason,_)
  | ArrT (reason,_,_)
      -> reason

  | ClassT t ->
      prefix_reason "class type: " (reason_of_t t)

  | InstanceT (reason,_,_,_)
  | SuperT (reason,_)
  | ParentT (reason,_)

  | CallT (reason, _)

  | MethodT (reason,_,_,_,_,_)
  | SetT (reason,_,_)
  | GetT (reason,_,_)

  | SetElemT (reason,_,_)
  | GetElemT (reason,_,_)

  | ConstructorT (reason,_,_)

  | AdderT (reason,_,_)
  | ComparatorT (reason,_)

  | TypeT (reason,_)
      -> reason

  | OptionalT t ->
      prefix_reason "optional of " (reason_of_t t)

  | RestT t ->
      prefix_reason "rest array of " (reason_of_t t)

  | PredicateT (pred,t) -> prefix_reason
      ((string_of_predicate pred) ^ " # ")
      (reason_of_t t)

  | EqT (reason, t) ->
      reason

  | MarkupT(reason,_,_)

  | SpecializeT(reason,_,_)
      -> reason

  | TypeAppT(t,_)
      -> prefix_reason "type application of " (reason_of_t t)

  | MaybeT t ->
      prefix_reason "?" (reason_of_t t)

  | IntersectionT (reason, _) ->
      reason

  | UnionT (reason, _) ->
      reason

  | LookupT(reason, _, _, _) ->
      reason

  | UnifyT(_,t) ->
      reason_of_t t

  | ObjAssignT (reason, _, _)
  | ObjRestT (reason, _, _)
  | ObjExtendT (reason, _, _)
    ->
      reason

  | UpperBoundT (t)
  | LowerBoundT (t)
      -> reason_of_t t

  | EnumT (reason, _)
  | RecordT (reason, _)
      ->
      reason

  | KeyT (reason, _) -> reason
  | HasT (reason, _) -> reason

  | ElemT (reason, _, _) -> reason

  | ConcretizeT (reason, _, _, _) -> reason
  | ConcreteT (t) -> reason_of_t t

  | SummarizeT (reason, t) -> reason

  | CustomClassT (_, _, t) -> reason_of_t t

and string_of_predicate = function
  | AndP (p1,p2) ->
      (string_of_predicate p1) ^ " && " ^ (string_of_predicate p2)
  | OrP (p1,p2) ->
      (string_of_predicate p1) ^ " || " ^ (string_of_predicate p2)
  | NotP p -> "not " ^ (string_of_predicate p)

  | ExistsP -> "truthy"
  | InstanceofP t -> "instanceof " ^ (streason_of_t t)
  | ConstructorP t -> "typeof " ^ (streason_of_t t)
  | IsP s -> "is " ^ s

and pos_of_predicate = function
  | AndP (p1,p2)
  | OrP (p1,p2)
    -> pos_of_predicate p1

  | NotP p
    -> pos_of_predicate p

  | ExistsP
    -> Pos.none (* TODO!!!!!!!!!!!! *)

  | InstanceofP t
  | ConstructorP t
    -> pos_of_t t

  | IsP _
    -> Pos.none (* TODO!!!!!!!!!!!! *)

and streason_of_t t = string_of_reason (reason_of_t t)

and desc_of_t t = desc_of_reason (reason_of_t t)

and pos_of_t t = pos_of_reason (reason_of_t t)

and loc_of_t t = loc_of_reason (reason_of_t t)

and get_typeparam_names xs tnames =
  List.fold_left (fun tnames param ->
    IMap.add param.id param.name tnames
  ) tnames xs

(* TODO make a type visitor *)
let rec mod_reason_of_t f = function

  | OpenT (reason, t) -> OpenT (f reason, t)
  | NumT (reason, t) -> NumT (f reason, t)
  | StrT (reason, t) -> StrT (f reason, t)
  | BoolT reason -> BoolT (f reason)
  | UndefT reason -> UndefT (f reason)
  | MixedT reason -> MixedT (f reason)
  | AnyT reason -> AnyT (f reason)
  | NullT reason -> NullT (f reason)
  | VoidT reason -> VoidT (f reason)

  | FunT (reason, s, p, ft) -> FunT (f reason, s, p, ft)
  | PolyT (plist, t) -> PolyT (plist, mod_reason_of_t f t)
  | BoundT { reason; id; name } -> BoundT { reason = f reason; id; name }
  | ObjT (reason, ot) -> ObjT (f reason, ot)
  | ArrT (reason, t, ts) -> ArrT (f reason, t, ts)

  | ClassT t -> ClassT (mod_reason_of_t f t)
  | InstanceT (reason, st, su, inst) -> InstanceT (f reason, st, su, inst)
  | SuperT (reason, inst) -> SuperT (f reason, inst)
  | ParentT (reason, inst) -> ParentT (f reason, inst)

  | CallT (reason, ft) -> CallT (f reason, ft)

  | MethodT (reason, name, s, ts, t, n) -> MethodT(f reason, name, s, ts, t, n)
  | SetT (reason, n, t) -> SetT (f reason, n, t)
  | GetT (reason, n, t) -> GetT (f reason, n, t)

  | SetElemT (reason, it, et) -> SetElemT (f reason, it, et)
  | GetElemT (reason, it, et) -> GetElemT (f reason, it, et)

  | ConstructorT (reason, ts, t) -> ConstructorT (f reason, ts, t)

  | AdderT (reason, rt, lt) -> AdderT (f reason, rt, lt)
  | ComparatorT (reason, t) -> ComparatorT (f reason, t)

  | TypeT (reason, t) -> TypeT (f reason, t)

  | OptionalT t -> OptionalT (mod_reason_of_t f t)

  | RestT t -> RestT (mod_reason_of_t f t)

  | PredicateT (pred, t) -> PredicateT (pred, mod_reason_of_t f t)

  | EqT (reason, t) -> EqT (f reason, t)

  | MarkupT(reason, t, t2) -> MarkupT (f reason, t, t2)

  | SpecializeT(reason, ts, t) -> SpecializeT (f reason, ts, t)

  | TypeAppT (t, ts) -> TypeAppT (mod_reason_of_t f t, ts)

  | MaybeT t -> MaybeT (mod_reason_of_t f t)

  | IntersectionT (reason, ts) -> IntersectionT (f reason, ts)

  | UnionT (reason, ts) -> UnionT (f reason, ts)

  | LookupT (reason, r2, x, t) -> LookupT (f reason, r2, x, t)

  | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)

  | ObjAssignT (reason, t, t2) -> ObjAssignT (f reason, t, t2)
  | ObjRestT (reason, t, t2) -> ObjRestT (f reason, t, t2)
  | ObjExtendT (reason, t, t2) -> ObjExtendT (f reason, t, t2)

  | UpperBoundT t -> UpperBoundT (mod_reason_of_t f t)
  | LowerBoundT t -> LowerBoundT (mod_reason_of_t f t)

  | EnumT (reason, t) -> EnumT (f reason, t)
  | RecordT (reason, t) -> RecordT (f reason, t)

  | KeyT (reason, t) -> KeyT (f reason, t)
  | HasT (reason, t) -> HasT (f reason, t)

  | ElemT (reason, t, t2) -> ElemT (f reason, t, t2)

  | ConcretizeT (reason, ts, t, t2) -> ConcretizeT (f reason, ts, t, t2)
  | ConcreteT t -> ConcreteT (mod_reason_of_t f t)

  | SummarizeT (reason, t) -> SummarizeT (f reason, t)

  | CustomClassT (name, ts, t) ->
      CustomClassT (name, ts, mod_reason_of_t f t)

let name_prefix_of_t = function
  | RestT _ -> "..."
  | _ -> ""

let name_suffix_of_t = function
  | OptionalT _ -> "?"
  | _ -> ""

let rec pretty_type_printer cx in_union in_intersect is_param t =
  match t with
  | BoundT typeparam -> typeparam.name

  | OpenT (_, id) -> spf "TYPE_%d" id

  | NumT _
  | StrT _
  | BoolT _
  | UndefT _
  | MixedT _
  | AnyT _
  | NullT _
    ->
      desc_of_reason (reason_of_t t)

  (* reasons for VoidT use "undefined" for more understandable error output.
     For parsable types we need to use "void" though, thus overwrite it. *)
  | VoidT _ -> "void"

  | FunT (_,_,_,{params_tlist = ts; params_names = pns; return_t = t; _}) ->
      let pns =
        match pns with
        | Some pns -> pns
        | None -> List.map (fun _ -> "_") ts in
      let type_s = spf "(%s) => %s"
        (List.map2 (fun n t ->
            let n = (name_prefix_of_t t) ^ n ^ (name_suffix_of_t t) in
            n ^ ": " ^ (pretty_type_printer cx false false true t)
          ) pns ts
         |> String.concat ", "
        )
        (pretty_type_printer cx false false false t) in
      if in_union || in_intersect
      then "(" ^ type_s ^ ")"
      else type_s

  | ObjT (_, {props_tmap = flds; _}) ->
      spf "{%s}"
        (IMap.find_unsafe flds cx.property_maps
         |> SMap.elements
         |> List.rev
         |> List.map (fun (x,t) ->
              x ^ ": " ^
              (pretty_type_printer cx false false false t) ^
              ";"
            )
         |> String.concat " "
        )

  | ArrT (_, t, _) ->
      spf "Array<%s>" (pretty_type_printer cx false false false t)

  | InstanceT (reason,static,super,instance) ->
      desc_of_reason reason (* nominal type *)

  | TypeAppT (c,ts) ->
      spf "%s <%s>"
        (pretty_type_printer cx false false false c)
        (ts
         |> List.map (pretty_type_printer cx false false false)
         |> String.concat ", "
        )

  | MaybeT t ->
      spf "?%s" (pretty_type_printer cx false false false t)

(* The following types are not syntax-supported in all cases *)
  | RestT t ->
      let type_s =
        spf "Array<%s>" (pretty_type_printer cx false false false t) in
      if is_param
      then type_s
      else "..." ^ type_s

  | OptionalT t ->
      let type_s = pretty_type_printer cx false false false t in
      if is_param
      then type_s
      else "=" ^ type_s

  | IntersectionT (_, ts) ->
      let type_s =
        (ts
          |> List.map (pretty_type_printer cx false true false)
          |> String.concat " & "
        ) in
      if in_union
      then "(" ^ type_s ^ ")"
      else type_s

  | UnionT (_, ts) ->
      let type_s =
        (ts
          |> List.map (pretty_type_printer cx true false false)
          |> String.concat " | "
        ) in
      if in_intersect
      then "(" ^ type_s ^ ")"
      else type_s

(* The following types are not syntax-supported *)
  | ClassT t ->
      spf "[class: %s]" (pretty_type_printer cx false false false t)

  | TypeT (_, t) ->
      spf "[type: %s]" (pretty_type_printer cx false false false t)

  | PolyT (xs,t) ->
      spf "<%s> %s"
        (xs
         |> List.map (fun param -> param.name)
         |> String.concat ", "
        )
        (pretty_type_printer cx false false false t)

  | t -> assert_false (string_of_ctor t)

let string_of_t cx t = pretty_type_printer cx false false false t

let rec is_printed_type_parsable_impl
          cx in_function_args in_function_ret = function
  (* Base cases *)
  | BoundT _
  | NumT _
  | StrT _
  | BoolT _
  | AnyT _
    ->
      true

  | VoidT _ -> in_function_ret

  (* Composed types *)
  | ArrT (_, t, _)
  | MaybeT t
  | TypeT (_, t)
    ->
      is_printed_type_parsable_impl cx false false t

  | RestT t
  | OptionalT t
    ->
      in_function_args &&
      is_printed_type_parsable_impl cx false false t

  | FunT (_, _, _, { params_tlist; return_t; _ }) ->
      (is_printed_type_parsable_impl cx false true return_t) &&
      List.fold_left (fun acc t ->
          (is_printed_type_parsable_impl cx true false t) && acc
        ) true params_tlist

  | ObjT (_, { props_tmap; _ }) ->
      let prop_map = IMap.find_unsafe props_tmap cx.property_maps in
      SMap.fold (fun _ t acc ->
          (is_printed_type_parsable_impl cx false false t) && acc
        ) prop_map true

  | InstanceT _ ->
      true

  | IntersectionT (_, ts)
  | UnionT (_, ts)
    ->
      List.fold_left (fun acc t ->
          (is_printed_type_parsable_impl cx false false t) && acc
        ) true ts

  | _ ->
      false

let is_printed_type_parsable cx t =
  is_printed_type_parsable_impl cx false false t

(* ------------- *)

(* traces *)

(* TODO arg num *)
let eval_funarg = function
  | (FunT (r1, _,_,ft1), links, FunT (r2, _,_,ft2))
  | (FunT (r1, _,_,ft1), links, CallT (r2, ft2)) ->
      (List.hd ft2.params_tlist, [], List.hd ft1.params_tlist)
  | _ -> assert false

(* DISABLING
let desc_reason t conn r =
  let desc = desc_of_t t in
  prefix_reason (desc ^ conn) r
*)

let desc_reason t conn r =
  reason_of_t t

let reasons_of_funarg i j = function
  | (FunT (r1, _,_,ft1), links, FunT (r2, _,_,ft2)) ->
      let arg_type = List.nth ft2.params_tlist j in
      let arg_conn = spf " param %d of " j in
      let arg_reason = desc_reason arg_type arg_conn r2 in
      let par_type = List.nth ft1.params_tlist i in
      let par_conn = spf " param %d of " i in
      let par_reason = desc_reason par_type par_conn r1 in
      [arg_reason; par_reason]

  | (FunT (r1, _,_,ft1), links, CallT (r2, ft2)) ->
      let arg_type = List.nth ft2.params_tlist j in
      let arg_conn = spf " arg %d of " j in
      let arg_reason = desc_reason arg_type arg_conn r2 in
      let par_type = List.nth ft1.params_tlist i in
      let par_conn = spf " param %d of " i in
      let par_reason = desc_reason par_type par_conn r1 in
      [arg_reason; par_reason]

  | _ -> [] (* TODO *)

let rec reasons_of_embed = function
  | (FunArg (i, j), t) -> reasons_of_funarg i j t
  | embed -> []

and reasons_of_link = function
  | Node ty -> [reason_of_t ty]
  | Embed (rule, trace) -> reasons_of_embed (rule, trace)

and reasons_of_trace (t1, links, t2) =
  List.flatten (List.map reasons_of_link links)

(* old trace printing *)

let vertical_bar = "|"
let horizontal_bar = "--"
let space_bar = " "

let slant_bar = "/"
let terminal_bar = "."

(* print out a bottom-up ASCII tree of the trace *)
let rec string_of_link prefix b = function
  | Node t ->
      prefix ^ (if b then terminal_bar else vertical_bar) ^
        (horizontal_bar ^ space_bar) ^ (string_of_reason (reason_of_t t)) ^ "\n"

  | Embed (r,t) ->
      (string_of_trace_old
        (prefix ^ (if b then space_bar else vertical_bar))
        true t
      ) ^
      (prefix ^ (if b then space_bar else vertical_bar) ^
        (slant_bar ^ horizontal_bar ^ space_bar) ^
        (string_of_rule r) ^ "\n")

and string_of_link_list prefix b = function

  | (t::ts) ->
      (string_of_link
        (prefix ^ (if b then space_bar else vertical_bar))
        true t
      ) ^
      (List.fold_left (fun s t ->  s ^
        (string_of_link
          (prefix ^ (if b then space_bar else vertical_bar))
          false t
        )) "" ts)

  | [] -> ""

and string_of_trace_old prefix b (r1, t, r2) =
  string_of_link_list prefix b
    ([Node r1] @ t @ [Node r2])

and string_of_trace prefix b (r1, t, r2) =
  let li = List.map string_of_reason (reasons_of_trace (r1, t, r2)) in
  (String.concat "\n" li) ^ "\n"
