(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason
open Type
open Utils_js

let dedup_strlist list =
  let _, rlist = List.fold_left (fun (set, rlist) s ->
    SSet.add s set, if SSet.mem s set then rlist else s :: rlist
  ) (SSet.empty, []) list
  in List.rev rlist

let name_prefix_of_t = function
  | RestT _ -> "..."
  | _ -> ""

let name_suffix_of_t = function
  | OptionalT _ -> "?"
  | _ -> ""

let parameter_name _cx n t =
  (name_prefix_of_t t) ^ n ^ (name_suffix_of_t t)

let prop_name _cx n t =
  n ^ (name_suffix_of_t t)

type enclosure_t =
  | EnclosureNone
  | EnclosureUnion
  | EnclosureIntersect
  | EnclosureParam
  | EnclosureMaybe
  | EnclosureAppT
  | EnclosureRet
  | EnclosureProp

let parenthesize t_str enclosure triggers =
  if List.mem enclosure triggers
  then "(" ^ t_str ^ ")"
  else t_str

(* general-purpose type printer. not the cleanest visitor in the world,
   but reasonably general. override gets a chance to print the incoming
   type first. if it passes, the bulk of printable types are formatted
   in a reasonable way. enclosure drives delimiter choice. see e.g.
   string_of_t for callers.
 *)
let rec type_printer_impl ~size override enclosure cx t =
  let pp = type_printer ~size override in

  let rec prop x = function
    | Field (t, polarity) -> spf "%s%s: %s"
      (Polarity.sigil polarity)
      (prop_name cx x t)
      (pp EnclosureProp cx t)
    | Get t -> spf "get %s(): %s" x (pp EnclosureRet cx t)
    | Set t -> spf "set %s(value: %s): void" x (pp EnclosureParam cx t)
    | GetSet (t1, t2) ->
      String.concat ", " [
        prop x (Get t1);
        prop x (Set t2);
      ]
  in

  match override cx t with
  | Some s -> s
  | None ->
    match t with
    | OpenT (_, id) ->
        spf "TYPE_%d" id

    | NumT _
    | StrT _
    | BoolT _
    | EmptyT _
    | MixedT _
    | AnyT _
    | NullT _ ->
        string_of_desc (desc_of_reason (reason_of_t t))

    | BoundT typeparam -> typeparam.name

    | SingletonStrT (_, s) -> spf "'%s'" s
    | SingletonNumT (_, (_, raw)) -> raw
    | SingletonBoolT (_, b) -> string_of_bool b

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
             (parameter_name cx n t) ^ ": " ^ (pp EnclosureParam cx t))
             pns ts
           |> String.concat ", "
          )
          (pp EnclosureNone cx t) in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureIntersect]

    | ObjT (_, {props_tmap = flds; dict_t; _}) ->
        let props =
          Context.find_props cx flds
          |> SMap.elements
          |> List.filter (fun (x,_) -> not (Reason.is_internal_name x))
          |> List.rev
          |> List.map (fun (x, p) -> prop x p)
          |> String.concat ", "
        in
        let indexer =
          (match dict_t with
          | Some { dict_name; key; value; dict_polarity } ->
              let indexer_prefix =
                if props <> ""
                then ", "
                else ""
              in
              let dict_name = match dict_name with
                | None -> "_"
                | Some name -> name
              in
              (spf "%s%s[%s: %s]: %s,"
                indexer_prefix
                (Polarity.sigil dict_polarity)
                dict_name
                (pp EnclosureNone cx key)
                (pp EnclosureNone cx value)
              )
          | None -> "")
        in
        spf "{%s%s}" props indexer

    | ArrT (_, t, ts) ->
        begin match ts with
        | [] -> spf "Array<%s>" (pp EnclosureNone cx t)
        | _ ->
          ts
          |> List.map (pp EnclosureNone cx)
          |> String.concat ", "
          |> spf "[%s]"
        end

    | InstanceT (reason, _, _, _) ->
        DescFormat.name_of_instance_reason reason

    | TypeAppT (c,ts) ->
        let type_s =
          spf "%s<%s>"
            (instance_of_poly_type_printer ~size override EnclosureAppT cx c)
            (ts
              |> List.map (pp EnclosureNone cx)
              |> String.concat ", "
            )
        in
        parenthesize type_s enclosure [EnclosureMaybe]

    | MaybeT t ->
        spf "?%s" (pp EnclosureMaybe cx t)

    | PolyT (xs,t) ->
        let xs_str =
          xs
          |> List.map (fun param -> param.name)
          |> String.concat ", "
        in
        let type_s = match t with
        | ClassT u
        | ThisClassT u ->
          spf "%s<%s>" (pp EnclosureNone cx u) xs_str
        | _ ->
          spf "<%s>%s" xs_str (pp EnclosureNone cx t)
        in
        parenthesize type_s enclosure [EnclosureAppT; EnclosureMaybe]

    | IntersectionT (_, rep) ->
        let mems = List.map (pp EnclosureIntersect cx) (InterRep.members rep) in
        let mems = dedup_strlist mems in
        let type_s = String.concat " & " mems in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureMaybe]

    | UnionT (_, rep) ->
        let mems = List.map (pp EnclosureUnion cx) (UnionRep.members rep) in
        let mems = dedup_strlist mems in
        let type_s = String.concat " | " mems in
        parenthesize type_s enclosure [EnclosureIntersect; EnclosureMaybe]

    | OptionalT t ->
        let type_s = pp EnclosureNone cx t in
        begin match enclosure with
        | EnclosureParam | EnclosureProp -> type_s
        | _ -> type_s ^ " | void"
        end

    (* The following types are not syntax-supported in all cases *)
    | RestT t ->
        let type_s =
          spf "Array<%s>" (pp EnclosureNone cx t) in
        if enclosure == EnclosureParam
        then type_s
        else "..." ^ type_s

    | AnnotT (_, t) -> pp EnclosureNone cx t
    | KeysT (_, t) -> spf "$Keys<%s>" (pp EnclosureNone cx t)
    | ShapeT t -> spf "$Shape<%s>" (pp EnclosureNone cx t)
    | TaintT (_) -> spf "$Tainted<any>"

    (* The following types are not syntax-supported *)
    | ClassT t ->
        spf "[class: %s]" (pp EnclosureNone cx t)

    | TypeT (_, t) ->
        spf "[type: %s]" (pp EnclosureNone cx t)

    | AnyWithUpperBoundT t ->
        spf "$Subtype<%s>" (pp EnclosureNone cx t)

    | AnyWithLowerBoundT t ->
        spf "$Supertype<%s>" (pp EnclosureNone cx t)

    | AnyObjT _ ->
        "Object"

    | AnyFunT _ ->
        "Function"

    | IdxWrapper (_, t) ->
      spf "$IdxWrapper<%s>" (pp enclosure cx t)

    | ThisClassT _ ->
        "This"

    | OpenPredT (_, t, m_pos, m_neg) ->
        let l_pos = Key_map.elements m_pos in
        let l_neg = Key_map.elements m_neg in
        let str_of_pair (k,p) = spf "%s -> %s"
          (Key.string_of_key k) (string_of_predicate p) in
        spf "$OpenPred (%s) [+: %s] [-: %s]" (pp EnclosureNone cx t)
          (l_pos |> List.map str_of_pair |> String.concat ", ")
          (l_neg |> List.map str_of_pair |> String.concat ", ")

    | ExistsT _ ->
        "*"

    (* TODO: Fix these *)

    | FunProtoT _ ->
        "function proto"

    | FunProtoBindT _ ->
        "FunctionProtoBind"

    | CustomFunT _ ->
        "CustomFunction"

    | FunProtoApplyT _ ->
        "FunctionProtoApply"

    | EvalT _ ->
        "Eval"

    | ThisTypeAppT _ ->
        "This Type App"

    | ModuleT _ ->
        "Module"

    | ChoiceKitT _ ->
        "ChoiceKit"

    | FunProtoCallT _
    | ObjProtoT _
    | AbstractT _
    | ExactT (_, _)
    | DiffT (_, _)
    | ExtendsT (_, _, _)
    | TypeMapT (_, _, _, _) ->
        assert_false (spf "Missing printer for %s" (string_of_ctor t))

and instance_of_poly_type_printer ~size override enclosure cx = function
  | PolyT (_, ThisClassT t)
  | PolyT (_, ClassT t)
    -> type_printer ~size override enclosure cx t

  | PolyT (_, TypeT (reason, _))
    -> DescFormat.name_of_type_reason reason

  (* NOTE: t = FunT is legit, others probably mean upstream errors *)
  | PolyT (_, t)
    -> type_printer ~size override enclosure cx t

  (* since we're called with args that aren't statically guaranteed
     to be `PolyT`s, fall back here instead of blowing up *)
  | t
    -> type_printer ~size override enclosure cx t

and type_printer ~size override enclosure cx t =
  count_calls ~counter:size ~default:"..." (fun () ->
    type_printer_impl ~size override enclosure cx t
  )

(* pretty printer *)
let string_of_t_ =
  let override _cx _t = None in
  fun ?(size=5000) enclosure cx t ->
    type_printer ~size:(ref size) override enclosure cx t

let string_of_t ?size cx t =
  string_of_t_ ?size EnclosureNone cx t

let string_of_param_t =
  string_of_t_ EnclosureParam

(* for debugging *)
let type_printer ?(size=5000) override enclosure cx t =
  type_printer ~size:(ref size) override enclosure cx t

let rec is_printed_type_parsable_impl weak cx enclosure = function
  (* Base cases *)
  | BoundT _
  | NumT _
  | StrT _
  | BoolT _
  | AnyT _
  | NullT _
  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _
    ->
      true

  | VoidT _
    when (enclosure == EnclosureRet)
    ->
      true

  | AnnotT (_, t) ->
      is_printed_type_parsable_impl weak cx enclosure t

  (* Composed types *)
  | MaybeT t
    ->
      is_printed_type_parsable_impl weak cx EnclosureMaybe t
  | TaintT (_)
    ->
      true

  | ArrT (_, t, ts)
    ->
      (match ts with
      | [] -> is_printed_type_parsable_impl weak cx EnclosureNone t
      | ts -> is_printed_type_list_parsable weak cx EnclosureNone ts)

  | RestT t
    when (enclosure == EnclosureParam)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | OptionalT t ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | VoidT _ -> true

  | FunT (_, _, _, { params_tlist; return_t; _ })
    ->
      (is_printed_type_parsable_impl weak cx EnclosureRet return_t) &&
      (is_printed_type_list_parsable weak cx EnclosureParam params_tlist)

  | ObjT (_, { props_tmap; dict_t; _ })
    ->
      let is_printable =
        match dict_t with
        | Some { key; value; _ } ->
            (is_printed_type_parsable_impl weak cx EnclosureNone key) &&
            (is_printed_type_parsable_impl weak cx EnclosureNone value)
        | None -> true
      in
      let prop_map = Context.find_props cx props_tmap in
      SMap.fold (fun name p acc ->
          acc && (
            (* We don't print internal properties, thus we do not care whether
               their type is printable or not *)
            (Reason.is_internal_name name) ||
            (p |> Type.Property.forall_t
              (is_printed_type_parsable_impl weak cx EnclosureNone))
          )
        ) prop_map is_printable

  | InstanceT _
    ->
      true

  | IntersectionT (_, rep)
    ->
      let ts = InterRep.members rep in
      is_printed_type_list_parsable weak cx EnclosureIntersect ts

  | UnionT (_, rep)
    ->
      let ts = UnionRep.members rep in
      is_printed_type_list_parsable weak cx EnclosureUnion ts

  | PolyT (_, t)
    ->
      (* unwrap PolyT (ClassT t) because class names are parsable as part of a
         polymorphic type declaration. *)
      let t = match t with
        | ThisClassT u
        | ClassT u -> u
        | _ -> t
      in
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | AnyObjT _ -> true
  | AnyFunT _ -> true

  | ThisTypeAppT (t, _, ts)
  | TypeAppT (t, ts)
    ->
      (is_instantiable_poly_type weak cx EnclosureAppT t) &&
      (is_printed_type_list_parsable weak cx EnclosureNone ts)

  (* weak mode *)

  (* these are types which are not really parsable, but they make sense to a
     human user in cases of autocompletion *)
  | RestT t
  | TypeT (_, t)
  | AnyWithUpperBoundT t
  | AnyWithLowerBoundT t
  | ThisClassT t
  | ClassT t
    when weak
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | OpenPredT (_, t, _, _) ->
    is_printed_type_parsable_impl weak cx EnclosureNone t

  | _
    ->
      false

and is_instantiable_poly_type weak cx enclosure = function
  | PolyT (_, ThisClassT t)
  | PolyT (_, ClassT t)
    -> is_printed_type_parsable_impl weak cx enclosure t

  | PolyT (_, TypeT _)
    -> true

  | _ -> false

and is_printed_type_list_parsable weak cx enclosure ts =
  List.fold_left (fun acc t ->
      acc && (is_printed_type_parsable_impl weak cx enclosure t)
    ) true ts

let is_printed_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureNone t

let is_printed_param_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureParam t
