(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type
open TypeUtil

(***********************)
(* instantiation utils *)
(***********************)

module ImplicitTypeArgument = struct
  (* Make a type argument for a given type parameter, given a reason. Note that
     not all type arguments are tvars; the following function is used only when
     polymorphic types need to be implicitly instantiated, because there was no
     explicit instantiation (via a type application), or when we want to cache a
     unique instantiation and unify it with other explicit instantiations. *)
  let mk_targ cx typeparam reason_op reason_tapp =
    (* Create a reason that is positioned at reason_op, but has a def_loc at
     * typeparam.reason. *)
    let loc_op = aloc_of_reason reason_op in
    let desc =
      RTypeParam
        ( typeparam.name,
          (desc_of_reason reason_op, loc_op),
          (desc_of_reason reason_tapp, def_aloc_of_reason reason_tapp) )
    in
    let reason = mk_reason desc (def_aloc_of_reason typeparam.reason) in
    let reason = repos_reason loc_op reason in
    Tvar.mk cx reason

  (* Abstract a type argument that is created by implicit instantiation
     above. Sometimes, these type arguments are involved in type expansion
     loops, so we abstract them to detect such loops. *)
  let abstract_targ tvar =
    let (reason, _) = open_tvar tvar in
    let desc = desc_of_reason reason in
    match desc with
    | RTypeParam _ -> Some (OpenT (locationless_reason desc, 0))
    | _ -> None
end

(* We maintain a stack of entries representing type applications processed
   during calls to flow, for the purpose of terminating unbounded expansion of
   type applications. Intuitively, we may have a potential infinite loop when
   processing a type application leads to another type application with the same
   root, but expanding type arguments. The entries in a stack contain
   approximate measurements that allow us to detect such expansion.

   An entry representing a type application with root C and type args T1,...,Tn
   is of the form (C, [A1,...,An]), where each Ai is a list of the roots of type
   applications nested in Ti. We consider a stack to indicate a potential
   infinite loop when the top of the stack is (C, [A1,...,An]) and there is
   another entry (C, [B1,...,Bn]) in the stack, such that each Bi is non-empty
   and is contained in Ai. *)

module TypeAppExpansion : sig
  type entry

  val push_unless_loop : 'phase Context.t_ -> Type.t * Type.t list -> bool

  val pop : unit -> unit

  val get : unit -> entry list

  val set : entry list -> unit
end = struct
  (* Array types function like type applications but are not implemented as such. Unless
     we decide to unify their implementation with regular typeapps, they need special
     handling here *)
  type root =
    | Type of Type.t
    | Array of reason
    | ROArray of reason
    | Tuple of reason * int

  (* arity *)

  module RootSet : Set.S with type elt = root = Set.Make (struct
    type elt = root

    type t = elt

    let compare = Stdlib.compare
  end)

  type entry = Type.t * RootSet.t list

  let stack = ref ([] : entry list)

  let collect_roots (type phase) (cx : phase Context.t_) =
    (* visitor to collect roots of type applications nested in a type *)
    let roots_collector =
      object (self)
        inherit [RootSet.t, phase] Type_visitor.t as super

        method arrtype r =
          function
          | ArrayAT _ -> Array r
          | ROArrayAT _ -> ROArray r
          | TupleAT (_, ts) -> Tuple (r, List.length ts)

        method! type_ cx pole acc t =
          match t with
          | TypeAppT (_, _, c, _) -> super#type_ cx pole (RootSet.add (Type c) acc) t
          | DefT (r, _, ArrT a) -> super#type_ cx pole (RootSet.add (self#arrtype r a) acc) t
          | OpenT _ ->
            (match ImplicitTypeArgument.abstract_targ t with
            | None -> acc
            | Some t -> RootSet.add (Type t) acc)
          | _ -> super#type_ cx pole acc t
      end
    in
    roots_collector#type_ cx Polarity.Neutral RootSet.empty

  (* Util to stringify a list, given a separator string and a function that maps
     elements of the list to strings. Should probably be moved somewhere else
     for general reuse. *)
  let string_of_list list sep f = list |> Base.List.map ~f |> String.concat sep

  let string_of_desc_of_t t = DescFormat.name_of_instance_reason (reason_of_t t)

  let string_of_desc_of_root = function
    | Type t -> string_of_desc_of_t t
    | Array r
    | ROArray r
    | Tuple (r, _) ->
      DescFormat.name_of_instance_reason r

  (* show entries in the stack *)
  let show_entry (c, tss) =
    spf
      "%s<%s>"
      (string_of_desc_of_t c)
      (string_of_list tss "," (fun ts ->
           let ts = RootSet.elements ts in
           spf "[%s]" (string_of_list ts ";" string_of_desc_of_root)))

  let _dump_stack () = string_of_list !stack "\n" show_entry

  (* Detect whether pushing would cause a loop. Push only if no loop is
     detected, and return whether push happened. *)

  let push_unless_loop (type phase) =
    (* Say that targs are possibly expanding when, given previous targs and
       current targs, each previously non-empty targ is contained in the
       corresponding current targ. *)
    let possibly_expanding_targs prev_tss tss =
      (* The following helper carries around a bit that indicates whether
         prev_tss contains at least one non-empty set. *)
      let rec loop seen_nonempty_prev_ts = function
        | (prev_ts :: prev_tss, ts :: tss) ->
          (* if prev_ts is not a subset of ts, we have found a counterexample
             and we can bail out *)
          RootSet.subset prev_ts ts
          && (* otherwise, we recurse on the remaining targs, updating the bit *)
          loop (seen_nonempty_prev_ts || not (RootSet.is_empty prev_ts)) (prev_tss, tss)
        | ([], []) ->
          (* we have found no counterexamples, so it comes down to whether we've
             seen any non-empty prev_ts *)
          seen_nonempty_prev_ts
        | ([], _)
        | (_, []) ->
          (* something's wrong around arities, but that's not our problem, so
             bail out *)
          false
      in
      loop false (prev_tss, tss)
    in
    fun (cx : phase Context.t_) (c, ts) ->
      let tss = Base.List.map ~f:(collect_roots cx) ts in
      let loop =
        !stack
        |> List.exists (fun (prev_c, prev_tss) ->
               c = prev_c && possibly_expanding_targs prev_tss tss)
      in
      if loop then
        false
      else (
        stack := (c, tss) :: !stack;
        if Context.is_verbose cx then prerr_endlinef "typeapp stack entry: %s" (show_entry (c, tss));
        true
      )

  let pop () = stack := List.tl !stack

  let get () = !stack

  let set _stack = stack := _stack
end
