(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason

let kind_of_name = function
  | "Uppercase" -> Some StringMappingUppercase
  | "Lowercase" -> Some StringMappingLowercase
  | "Capitalize" -> Some StringMappingCapitalize
  | "Uncapitalize" -> Some StringMappingUncapitalize
  | _ -> None

let name_of_kind = function
  | StringMappingUppercase -> "Uppercase"
  | StringMappingLowercase -> "Lowercase"
  | StringMappingCapitalize -> "Capitalize"
  | StringMappingUncapitalize -> "Uncapitalize"

(* ASCII-only casing. We do not match tsc's full Unicode `String.prototype.toUpperCase`
   semantics here (e.g. tsc returns `'STRASSE'` for `Uppercase<'straße'>` because JS
   expands `ß` to `SS`), since OCaml's stdlib has no Unicode case-folding table.
   ASCII-only is a deliberate, soundness-preserving choice for v1: tests that
   exercise non-ASCII input cannot rely on a particular result. *)
let transform_string kind s =
  match kind with
  | StringMappingUppercase -> String.uppercase_ascii s
  | StringMappingLowercase -> String.lowercase_ascii s
  | StringMappingCapitalize ->
    if String.length s = 0 then
      s
    else
      let head = Char.uppercase_ascii s.[0] in
      String.make 1 head ^ String.sub s 1 (String.length s - 1)
  | StringMappingUncapitalize ->
    if String.length s = 0 then
      s
    else
      let head = Char.lowercase_ascii s.[0] in
      String.make 1 head ^ String.sub s 1 (String.length s - 1)

(* Predicate matching `transform_string kind s = s`. Used in subtyping to test
   whether a literal `s` is a member of `Kind<arg>` — the literal must already
   be in canonical form for the kind. *)
let is_canonical kind s = transform_string kind s = s

(* Like [is_canonical] but the empty string is NEVER canonical for cap/uncap.
   Used by `Template_literal_type.lexical_validator_of` to reject an empty
   placeholder match for `Capitalize`/`Uncapitalize`: in placeholder context,
   an empty match means the leading character comes from a neighboring quasi
   or interpolation, so the cap/uncap constraint must not "discharge" on the
   empty match. *)
let is_canonical_for_placeholder kind s =
  match kind with
  | StringMappingUppercase
  | StringMappingLowercase ->
    is_canonical kind s
  | StringMappingCapitalize
  | StringMappingUncapitalize ->
    String.length s > 0 && is_canonical kind s

(* Apply Capitalize/Uncapitalize quasi rewriting. Uppercase/Lowercase touch
   every quasi; the cap/uncap kinds only affect the very first character of the
   produced string, which is the first character of quasis[0] — _unless_
   quasis[0] is empty, in which case the leading character lives in an
   interpolated type and the caller must push the transform into types[0]
   instead. *)
let transform_quasis kind quasis =
  match kind with
  | StringMappingUppercase -> List.map String.uppercase_ascii quasis
  | StringMappingLowercase -> List.map String.lowercase_ascii quasis
  | StringMappingCapitalize
  | StringMappingUncapitalize ->
    (match quasis with
    | [] -> []
    | first :: rest ->
      let first' = transform_string kind first in
      first' :: rest)

let mk_singleton_str loc s =
  let r = mk_annot_reason (RStringLit (OrdinaryName s)) loc in
  DefT (r, SingletonStrT { from_annot = true; value = OrdinaryName s })

let mk_deferred ~reason kind arg = StringMappingT { reason; kind; arg }

(* Conservative predicate: can `t`, viewed as a template literal placeholder,
   stringify to the empty string? `string` and `''` can; `number`, `bigint`,
   `boolean`, `null`, `undefined`, and any non-empty string literal cannot.
   Unions: yes iff any member can. Generics: defer to their bound. Anything
   we can't analyze (opens, evals, etc.) is treated as "yes" — that's the
   safe direction for soundness (we'll widen the result type, never narrow). *)
let rec can_be_empty_string = function
  | DefT (_, StrGeneralT _) -> true
  | DefT (_, SingletonStrT { value = OrdinaryName s; _ }) -> String.length s = 0
  | DefT
      ( _,
        ( NumGeneralT _ | SingletonNumT _ | NumericStrKeyT _ | BigIntGeneralT _ | SingletonBigIntT _
        | BoolGeneralT | SingletonBoolT _ | NullT | VoidT )
      ) ->
    false
  | UnionT (_, rep) -> List.exists can_be_empty_string (UnionRep.members rep)
  | GenericT { bound; _ } -> can_be_empty_string bound
  | _ -> true

(* Concretizing variant. When [possible_concrete_types_for_inspection] is
   provided, resolve indirection (`EvalT` for indexed/property access, `OpenT`,
   `AnnotT`, `TypeAppT`, ...) before applying the syntactic predicate, so that
   e.g. `['Hello'][0]` is recognized as the non-empty literal it is rather than
   falling through to the conservative `true` fallback. Without it (e.g. from
   `type_sig_merge`, which deliberately blocks Flow_js), the syntactic check
   stands. *)
let can_be_empty_string_concretized ?possible_concrete_types_for_inspection cx t =
  match possible_concrete_types_for_inspection with
  | None -> can_be_empty_string t
  | Some f -> List.exists can_be_empty_string (f cx (TypeUtil.reason_of_t t) t)

(* Build the result type for `Kind<arg>`. Eager when `arg` is a shape whose
   strings we can enumerate; deferred (a `StringMappingT`) otherwise so the
   dependency on a generic / `string` / unresolved EvalT / etc. is preserved
   through substitution and surfaces in subtyping. The subtyping rule for
   `_ <: StringMappingT` concretizes `arg` lazily before checking, which is
   where opaque shapes (EvalT for indexed/property access, OpenT, AnnotT)
   get a chance to reduce to a literal. *)
let rec resolve ?possible_concrete_types_for_inspection cx ~kind loc arg =
  let result_reason = mk_annot_reason (RType (OrdinaryName (name_of_kind kind))) loc in
  match arg with
  | DefT (_, SingletonStrT { value = OrdinaryName s; _ }) ->
    mk_singleton_str loc (transform_string kind s)
  | DefT (_, EmptyT) ->
    let r = mk_annot_reason REmpty loc in
    DefT (r, EmptyT)
  | UnionT (_, rep) ->
    let members = UnionRep.members rep in
    let transformed =
      List.map (resolve ?possible_concrete_types_for_inspection cx ~kind loc) members
    in
    (match transformed with
    | [] ->
      let r = mk_annot_reason REmpty loc in
      DefT (r, EmptyT)
    | [t] -> t
    | t0 :: t1 :: rest ->
      UnionT (result_reason, UnionRep.make ~source_aloc:(Context.make_aloc_id cx loc) t0 t1 rest))
  | TemplateLiteralT { reason = _; quasis; types } ->
    let r = mk_annot_reason RTemplateLiteralType loc in
    (match kind with
    | StringMappingUppercase
    | StringMappingLowercase ->
      let quasis' = transform_quasis kind quasis in
      let types' = List.map (resolve ?possible_concrete_types_for_inspection cx ~kind loc) types in
      TemplateLiteralT { reason = r; quasis = quasis'; types = types' }
    | StringMappingCapitalize
    | StringMappingUncapitalize ->
      (* Only the leading character of the produced string changes. That char
         lives in quasis[0] when quasis[0] is non-empty; otherwise it's the
         leading char of types[0] when types[0] is non-empty at runtime, OR
         the leading char of quasis[1] when types[0] stringifies to "". When
         types[0] can be the empty string, we must produce a union of both
         arms so subtyping enforces the right thing on either runtime value. *)
      (match (quasis, types) with
      | (first :: _, _) when String.length first <> 0 ->
        let quasis' = transform_quasis kind quasis in
        TemplateLiteralT { reason = r; quasis = quasis'; types }
      | (_ :: rest_quasis, t0 :: rest_types)
        when can_be_empty_string_concretized ?possible_concrete_types_for_inspection cx t0 ->
        (* Non-empty arm: push transform into types[0]; quasis unchanged. *)
        let non_empty_arm =
          let types' =
            resolve ?possible_concrete_types_for_inspection cx ~kind loc t0 :: rest_types
          in
          TemplateLiteralT { reason = r; quasis; types = types' }
        in
        (* Empty arm: treat as the suffix template literal (quasis[1:], types[1:])
           with the transform applied to it. Recursing handles nested empty
           interpolations correctly. *)
        let suffix = TemplateLiteralT { reason = r; quasis = rest_quasis; types = rest_types } in
        let empty_arm = resolve ?possible_concrete_types_for_inspection cx ~kind loc suffix in
        UnionT
          ( result_reason,
            UnionRep.make ~source_aloc:(Context.make_aloc_id cx loc) empty_arm non_empty_arm []
          )
      | (_ :: _, t0 :: rest_types) ->
        (* types[0] cannot be empty; just push the transform into it. *)
        let types' =
          resolve ?possible_concrete_types_for_inspection cx ~kind loc t0 :: rest_types
        in
        TemplateLiteralT { reason = r; quasis; types = types' }
      | _ ->
        (* No types or arity invariant violated; preserve original. *)
        let quasis' = transform_quasis kind quasis in
        TemplateLiteralT { reason = r; quasis = quasis'; types }))
  | _ ->
    (* `string`, generics, opens, evals, etc. — defer. The subtyping rule
       for `_ <: StringMappingT` will concretize `arg` lazily and re-resolve
       if it reduces. *)
    mk_deferred ~reason:result_reason kind arg
