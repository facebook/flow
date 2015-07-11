(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


open Utils

module Reason = Typing_reason
module SN = Naming_special_names

type visibility =
  | Vpublic
  | Vprivate of string
  | Vprotected of string

(* All the possible types, reason is a trace of why a type
   was inferred in a certain way.

   Types exists in two phases. Phase one is 'decl', meaning it is a type that
   was declared in user code. Phase two is 'locl', meaning it is a type that is
   inferred via local inference.
*)
(* create private types to represent the different type phases *)
type decl = private DeclPhase
type locl = private LoclPhase

type 'phase ty = Reason.t * 'phase ty_
and _ ty_ =
  (*========== Following Types Exist Only in the Declared Phase ==========*)
  (* The late static bound type of a class *)
  | Tthis : decl ty_

  (* Either an object type or a type alias, ty list are the arguments *)
  | Tapply : Nast.sid * decl ty list -> decl ty_

  (* The type of a generic inside a function using that generic, with an
   * optional "as" or "super" constraint. For example:
   *
   * function f<T as int>(T $x) {
   *   // ...
   * }
   *
   * The type of $x inside f() is
   * Tgeneric("T", Some(Constraint_as, Tprim Tint))
   *)
  | Tgeneric : string * (Ast.constraint_kind * decl ty) option -> decl ty_

  (* Name of class, name of type const, remaining names of type consts *)
  | Taccess : taccess_type -> decl ty_

  (*========== Following Types Exist in Both Phases ==========*)
  (* "Any" is the type of a variable with a missing annotation, and "mixed" is
   * the type of a variable annotated as "mixed". THESE TWO ARE VERY DIFFERENT!
   * Any unifies with anything, i.e., it is both a supertype and subtype of any
   * other type. You can do literally anything to it; it's the "trust me" type.
   * Mixed, on the other hand, is only a supertype of everything. You need to do
   * a case analysis to figure out what it is (i.e., its elimination form).
   *
   * Here's an example to demonstrate:
   *
   * function f($x): int {
   *   return $x + 1;
   * }
   *
   * In that example, $x has type Tany. This unifies with anything, so adding
   * one to it is allowed, and returning that as int is allowed.
   *
   * In contrast, if $x were annotated as mixed, adding one to that would be
   * a type error -- mixed is not a subtype of int, and you must be a subtype
   * of int to take part in addition. (The converse is true though -- int is a
   * subtype of mixed.) A case analysis would need to be done on $x, via
   * is_int or similar.
   *)
  | Tany
  | Tmixed

  (* The type of the various forms of "array":
   * Tarray (None, None)         => "array"
   * Tarray (Some ty, None)      => "array<ty>"
   * Tarray (Some ty1, Some ty2) => "array<ty1, ty2>"
   * Tarray (None, Some ty)      => [invalid]
   *)
  | Tarray : 'phase ty option * 'phase ty option -> 'phase ty_

  (* Nullable, called "option" in the ML parlance. *)
  | Toption : 'phase ty -> 'phase ty_

  (* All the primitive types: int, string, void, etc. *)
  | Tprim : Nast.tprim -> 'phase ty_

  (* A wrapper around fun_type, which contains the full type information for a
   * function, method, lambda, etc. Note that lambdas have an additional layer
   * of indirection before you get to Tfun -- see Tanon below. *)
  | Tfun : 'phase fun_type -> 'phase ty_


  (* Tuple, with ordered list of the types of the elements of the tuple. *)
  | Ttuple : 'phase ty list -> 'phase ty_

  (* Whether all fields of this shape are known, types of each of the
   * known arms.
   *)
  | Tshape : shape_fields_known * ('phase ty Nast.ShapeMap.t) -> 'phase ty_

  (*========== Below Are Types That Cannot Be Declared In User Code ==========*)

  (* A type variable (not to be confused with a type parameter). This is the
   * core of how type inference works. If you aren't familiar with it, a
   * suitable explanation couldn't possibly fit here; terms to google for
   * include "Hindley-Milner type inference", "unification", and "algorithm W".
   *)
  | Tvar : Ident.t -> locl ty_

  (* The type of an opaque type (e.g. a "newtype" outside of the file where it
   * was defined). They are "opaque", which means that they only unify with
   * themselves. However, it is possible to have a constraint that allows us to
   * relax this. For example:
   *
   *   newtype my_type as int = ...
   *
   * Outside of the file where the type was defined, this translates to:
   *
   *   Tabstract (AKnewtype (pos, "my_type", []), Some (Tprim Tint))
   *
   * Which means that my_type is abstract, but is subtype of int as well.
   *
   * We also create abstract types for generic parameters of a function, i.e.
   *
   *   function foo<T>(T $x): void {
   *     // Body
   *   }
   *
   * The type 'T' will be represented as an abstract type when type checking
   * the body of 'foo'.
   *
   * Finally abstract types are also derived from the 'this' type and
   * accessing type constants on it, resulting in a dependent type.
   *)
  | Tabstract : abstract_kind * locl ty option -> locl ty_

  (* An anonymous function, including the fun arity, and the identifier to
   * type the body of the function. (The actual closure is stored in
   * Typing_env.env.genv.anons) *)
  | Tanon : locl fun_arity * Ident.t -> locl ty_

  (* This is a kinda-union-type we use in order to defer picking which common
   * ancestor for a type we should use until we hit a type annotation.
   * For example:
   *
   * interface I {}
   * class C implements I {}
   * class D extends C {}
   * function f(): I {
   *   if (...) {
   *     $x = new C();
   *   } else {
   *     $x = new D();
   *   }
   *   return $x;
   * }
   *
   * What is the type of $x? We need to pick some common ancestor, but which
   * one? Both C and I would be acceptable, which do we mean? This is where
   * Tunresolved comes in -- after the if/else, the type of $x is
   * Unresolved[C, D] -- it could be *either one*, and we defer the check until
   * we hit an annotation. In particular, when we hit the "return", we make sure
   * that it is compatible with both C and D, and then we know we've found the
   * right supertype. Since we don't do global inference, we'll always either
   * hit an annotation to check, or hit a place an annotation is missing in
   * which case we can just throw away the type.
   *
   * Note that this is *not* really a union type -- most notably, it's allowed
   * to grow as inference goes on, which union types don't. For example:
   *
   * function f(): Vector<num> {
   *   $v = Vector {};
   *   $v[] = 1;
   *   $v[] = 3.14;
   *   return $v;
   * }
   *
   * (Eliding some Tvar for clarity) On the first line, $v is
   * Vector<Unresolved[]>. On the second, Vector<Unresolved[int]>. On the third,
   * Vector<Unresolved[int,float]> -- it grows! Then when we finally return $v,
   * we see that int and float are both compatible with num, and we have found
   * our suitable supertype.
   *
   * One final implication of this growing is that if an unresolved is used in
   * a contravariant position, we must collapse it down to whatever is annotated
   * right then, in order to be sound.
   *)
  | Tunresolved : locl ty list -> locl ty_

  (* Tobject is an object type compatible with all objects. This type is also
   * compatible with some string operations (since a class might implement
   * __toString), but not with string type hints. In a similar way, Tobject
   * is compatible with some array operations (since a class might implement
   * ArrayAccess), but not with array type hints.
   *
   * Tobject is currently used to type code like:
   *   ../test/typecheck/return_unknown_class.php
   *)
  | Tobject : locl ty_

  (* An instance of a class or interface, ty list are the arguments *)
  | Tclass : Nast.sid * locl ty list -> locl ty_

(* An abstract type derived from either a newtype, a type parameter, or some
 * dependent type
 *)
and abstract_kind =
    (* newtype foo<T1, T2> ... *)
  | AKnewtype of string * locl ty list
    (* <T super C> ; None if 'as' constrained *)
  | AKgeneric of string * locl ty option
    (* see dependent_type *)
  | AKdependent of dependent_type

(* A dependent type consists of a base kind which indicates what the type is
 * dependent on. It is either dependent on:
 *  - The type 'this'
 *  - The class context (what 'static' is resolved to in a class)
 *  - A class
 *  - An expression
 *
 * Dependent types also have a path component (derived from accessing a type
 * constant). Thus the dependent type (`expr 0, ['A', 'B', 'C']) roughly means
 * "The type resulting from accessing the type constant A then the type constant
 * B and then the type constant C on the expression reference by 0"
 *)
and dependent_type =
  (* Type that is the subtype of the late bound type within a class. *)
  [ `this
  (* The late bound type within a class. It is the type of 'new static()' and
   * '$this'. This is different than the 'this' type. The 'this' type isn't
   * quite strong enough in some cases. It means you are a subtype of the late
   * bound class, but there are instances where you need the exact type.
   * We may not need both since the only way to make something of type 'this'
   * that is not 'static' is with 'instanceof static'.
   *)
  | `static
  (* A class name, new type, or generic, i.e.
   *
   * abstract class C { abstract const type T }
   *
   * The type C::T is (`cls '\C', ['T'])
   *)
  | `cls of string
  (* A reference to some expression. For example:
   *
   *  $x->foo()
   *
   *  The expression $x would have a reference Ident.t
   *  The expression $x->foo() would have a different one
   *)
  | `expr of Ident.t
  ] * string list

and taccess_type = decl ty * Nast.sid list

(* Local shape constructed using "shape" keyword has all the fields
 * known:
 *
 *   $s = shape('x' => 4, 'y' => 4);
 *
 * It has fields 'x' and 'y' and definitely no other fields. On the other
 * hand, shape types that come from typehints may (due to structural
 * subtyping of shapes) have some other, unlisted fields:
 *
 *   type s = shape('x' => int);
 *
 *   function f(s $s) {
 *   }
 *
 *   f(shape('x' => 4, 'y' => 5));
 *
 * The call to f is valid because of structural subtyping - shapes are
 * permitted to "forget" fields. But the 'y' field still exists at runtime,
 * and we cannot say inside the body of $f that we know that 'x' is the only
 * field. This is relevant when deciding if it's safe to omit optional fields
 * - if shape fields are not fully known, even optional fields have to be
 * explicitly set/unset.
 *
 * We also track in additional map of FieldsPartiallyKnown names of fields
 * that are known to not exist (because they were explicitly unset).
 *)
and shape_fields_known =
  | FieldsFullyKnown
  | FieldsPartiallyKnown of Pos.t Nast.ShapeMap.t

(* The type of a function AND a method.
 * A function has a min and max arity because of optional arguments *)
and 'phase fun_type = {
  ft_pos       : Pos.t;
  ft_deprecated: string option   ;
  ft_abstract  : bool            ;
  ft_arity     : 'phase fun_arity    ;
  ft_tparams   : tparam list     ;
  ft_params    : 'phase fun_params   ;
  ft_ret       : 'phase ty           ;
}

(* Arity information for a fun_type; indicating the minimum number of
 * args expected by the function and the maximum number of args for
 * standard, non-variadic functions or the type of variadic argument taken *)
and 'phase fun_arity =
  | Fstandard of int * int (* min ; max *)
  (* PHP5.6-style ...$args finishes the func declaration *)
  | Fvariadic of int * 'phase fun_param (* min ; variadic param type *)
  (* HH-style ... anonymous variadic arg; body presumably uses func_get_args *)
  | Fellipsis of int       (* min *)


and 'phase fun_param = (string option * 'phase ty)

and 'phase fun_params = 'phase fun_param list

and class_elt = {
  ce_final       : bool;
  ce_is_xhp_attr : bool;
  ce_override    : bool;
  (* true if this elt arose from require-extends or other mechanisms
     of hack "synthesizing" methods that were not written by the
     programmer. The eventual purpose of this is to make sure that
     elts that *are* written by the programmer take precedence over
     synthesized elts. *)
  ce_synthesized : bool;
  ce_visibility  : visibility;
  ce_type        : decl ty;
  (* identifies the class from which this elt originates *)
  ce_origin      : string;
}

and class_type = {
  tc_need_init           : bool;
  (* Whether the typechecker knows of all (non-interface) ancestors
   * and thus known all accessible members of this class *)
  tc_members_fully_known : bool;
  tc_abstract            : bool;
  tc_final               : bool;
  (* When a class is abstract (or in a trait) the initialization of
   * a protected member can be delayed *)
  tc_deferred_init_members : SSet.t;
  tc_kind                : Ast.class_kind;
  tc_name                : string ;
  tc_pos                 : Pos.t ;
  tc_tparams             : tparam list ;
  tc_consts              : class_elt SMap.t;
  tc_typeconsts          : typeconst_type SMap.t;
  tc_props               : class_elt SMap.t;
  tc_sprops              : class_elt SMap.t;
  tc_methods             : class_elt SMap.t;
  tc_smethods            : class_elt SMap.t;
  tc_construct           : class_elt option * bool;
  (* This includes all the classes, interfaces and traits this class is
   * using. *)
  tc_ancestors           : decl ty SMap.t ;
  tc_req_ancestors       : decl ty SMap.t;
  tc_req_ancestors_extends : SSet.t; (* the extends of req_ancestors *)
  tc_extends             : SSet.t;
  tc_user_attributes     : Nast.user_attribute list;
  tc_enum_type           : enum_type option;
}

and typeconst_type = {
  ttc_name        : Nast.sid;
  ttc_constraint  : decl ty option;
  ttc_type        : decl ty option;
  ttc_origin      : string;
}

and enum_type = {
  te_base       : decl ty;
  te_constraint : decl ty option;
}

and tparam = Ast.variance * Ast.id * (Ast.constraint_kind * decl ty) option

type phase_ty =
  | DeclTy of decl ty
  | LoclTy of locl ty

(* Tracks information about how a type was expanded *)
type expand_env = {
  (* A list of the type defs and type access we have expanded thus far. Used
   * to prevent entering into a cycle when expanding these types
   *)
  type_expansions : (Pos.t * string) list;
  substs : locl ty SMap.t;
  this_ty : locl ty;
  (* The class that the type is extracted from. Used for creating expression
   * dependent types for type constants.
   *)
  from_class : Nast.class_id option;
}

type ety = expand_env * locl ty

(* The identifier for this *)
let this = Ident.make "$this"

let arity_min ft_arity : int = match ft_arity with
  | Fstandard (min, _) | Fvariadic (min, _) | Fellipsis min -> min

module AbstractKind = struct
  let to_string = function
    | AKnewtype (name, _) -> name
    | AKgeneric (name, _) -> name
    | AKdependent (dt, ids) ->
       let dt =
         match dt with
         | `this -> SN.Typehints.this
         | `static -> "<"^SN.Classes.cStatic^">"
         | `cls c -> c
         | `expr i ->
             let display_id = Reason.get_expr_display_id i in
             "<expr#"^string_of_int display_id^">" in
       String.concat "::" (dt::ids)
  let is_classname = function
    | AKnewtype (name, _) -> (name = Naming_special_names.Classes.cClassname)
    | AKgeneric _ -> false
    | AKdependent _ -> false
end

(*****************************************************************************)
(* Accumulate method calls mode *)
(*****************************************************************************)

let accumulate_method_calls = ref false
let (accumulate_method_calls_result: (Pos.t * string) list ref) = ref []

(*****************************************************************************)
(* Suggest mode *)
(*****************************************************************************)

(* Set to true when we are trying to infer the missing type hints. *)
let is_suggest_mode = ref false
