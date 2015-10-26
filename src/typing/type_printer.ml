(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Reason_js
open Type
open Utils
open Utils_js

let name_prefix_of_t = function
  | RestT _ -> "..."
  | _ -> ""

let name_suffix_of_t = function
  | OptionalT _ -> "?"
  | _ -> ""

let parameter_name cx n t =
  (name_prefix_of_t t) ^ n ^ (name_suffix_of_t t)

type enclosure_t =
    EnclosureNone
  | EnclosureUnion
  | EnclosureIntersect
  | EnclosureParam
  | EnclosureMaybe
  | EnclosureAppT
  | EnclosureRet

let parenthesize t_str enclosure triggers =
  if List.mem enclosure triggers
  then "(" ^ t_str ^ ")"
  else t_str

(* general-purpose type printer. not the cleanest visitor in the world,
   but reasonably general. override gets a chance to print the incoming
   type first. if it passes, the bulk of printable types are formatted
   in a reasonable way. fallback is sent the rest. enclosure drives
   delimiter choice. see e.g. string_of_t for callers.
 *)
let rec type_printer override fallback enclosure cx t =
  let pp = type_printer override fallback in
  match override cx t with
  | Some s -> s
  | None ->
    match t with
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
              (parameter_name cx n t) ^
              ": "
              ^ (pp EnclosureParam cx t)
            ) pns ts
           |> String.concat ", "
          )
          (pp EnclosureNone cx t) in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureIntersect]

    | ObjT (_, {props_tmap = flds; dict_t; _}) ->
        let props =
          Context.property_maps cx
          |> IMap.find_unsafe flds
          |> SMap.elements
          |> List.filter (fun (x,_) -> not (Reason_js.is_internal_name x))
          |> List.rev
          |> List.map (fun (x,t) -> x ^ ": " ^ (pp EnclosureNone cx t) ^ ",")
          |> String.concat " "
        in
        let indexer =
          (match dict_t with
          | Some { dict_name; key; value } ->
              let indexer_prefix =
                if props <> ""
                then " "
                else ""
              in
              let dict_name = match dict_name with
                | None -> "_"
                | Some name -> name
              in
              (spf "%s[%s: %s]: %s,"
                indexer_prefix
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

    | InstanceT (reason,static,super,instance) ->
        desc_of_reason reason (* nominal type *)

    | TypeAppT (c,ts) ->
        let type_s =
          spf "%s <%s>"
            (pp EnclosureAppT cx c)
            (ts
              |> List.map (pp EnclosureNone cx)
              |> String.concat ", "
            )
        in
        parenthesize type_s enclosure [EnclosureMaybe]

    | MaybeT t ->
        spf "?%s" (pp EnclosureMaybe cx t)

    | PolyT (xs,t) ->
        let type_s =
          spf "<%s> %s"
            (xs
              |> List.map (fun param -> param.name)
              |> String.concat ", "
            )
            (pp EnclosureNone cx t)
        in
        parenthesize type_s enclosure [EnclosureAppT; EnclosureMaybe]

    | IntersectionT (_, ts) ->
        let type_s =
          (ts
            |> List.map (pp EnclosureIntersect cx)
            |> String.concat " & "
          ) in
        parenthesize type_s enclosure [EnclosureUnion; EnclosureMaybe]

    | UnionT (_, ts) ->
        let type_s =
          (ts
            |> List.map (pp EnclosureUnion cx)
            |> String.concat " | "
          ) in
        parenthesize type_s enclosure [EnclosureIntersect; EnclosureMaybe]

    (* The following types are not syntax-supported in all cases *)
    | RestT t ->
        let type_s =
          spf "Array<%s>" (pp EnclosureNone cx t) in
        if enclosure == EnclosureParam
        then type_s
        else "..." ^ type_s

    | OptionalT t ->
        let type_s = pp EnclosureNone cx t in
        if enclosure == EnclosureParam
        then type_s
        else "=" ^ type_s

    | AnnotT (_, t) -> pp EnclosureNone cx t
    | KeysT (_, t) -> spf "$Keys<%s>" (pp EnclosureNone cx t)
    | ShapeT t -> spf "$Shape<%s>" (pp EnclosureNone cx t)
    | TaintedT (_,t) -> spf "$Tainted<%s>" (pp EnclosureNone cx t)

    (* The following types are not syntax-supported *)
    | ClassT t ->
        spf "[class: %s]" (pp EnclosureNone cx t)

    | TypeT (_, t) ->
        spf "[type: %s]" (pp EnclosureNone cx t)

    | BecomeT (_, t) ->
        spf "[become: %s]" (pp EnclosureNone cx t)

    | LowerBoundT t ->
        spf "$Subtype<%s>" (pp EnclosureNone cx t)

    | UpperBoundT t ->
        spf "$Supertype<%s>" (pp EnclosureNone cx t)

    | AnyObjT _ ->
        "Object"

    | AnyFunT _ ->
        "Function"

    | t ->
        fallback t

(* pretty printer *)
let string_of_t_ =
  let override cx t = match t with
    | OpenT (r, id) -> Some (spf "TYPE_%d" id)
    | NumT _
    | StrT _
    | BoolT _
    | UndefT _
    | MixedT _
    | AnyT _
    | NullT _ -> Some (desc_of_reason (reason_of_t t))
    | _ -> None
  in
  let fallback t =
    assert_false (spf "Missing printer for %s" (string_of_ctor t))
  in
  fun enclosure cx t ->
    type_printer override fallback enclosure cx t

let string_of_t =
  string_of_t_ EnclosureNone

let string_of_param_t =
  string_of_t_ EnclosureParam

let rec is_printed_type_parsable_impl weak cx enclosure = function
  (* Base cases *)
  | BoundT _
  | NumT _
  | StrT _
  | BoolT _
  | AnyT _
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
  | TaintedT (_, t)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | ArrT (_, t, ts)
    ->
      (*(match ts with
      | [] -> *)is_printed_type_parsable_impl weak cx EnclosureNone t
      (*| _ ->
          is_printed_type_list_parsable weak cx EnclosureNone t*)

  | RestT t
  | OptionalT t
    when (enclosure == EnclosureParam)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

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
      let prop_map = IMap.find_unsafe props_tmap (Context.property_maps cx) in
      SMap.fold (fun name t acc ->
          acc && (
            (* We don't print internal properties, thus we do not care whether
               their type is printable or not *)
            (Reason_js.is_internal_name name) ||
            (is_printed_type_parsable_impl weak cx EnclosureNone t)
          )
        ) prop_map is_printable

  | InstanceT _
    ->
      true

  | IntersectionT (_, ts)
    ->
      is_printed_type_list_parsable weak cx EnclosureIntersect ts

  | UnionT (_, ts)
    ->
      is_printed_type_list_parsable weak cx EnclosureUnion ts

  | PolyT (_, t)
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | AnyObjT _ -> true
  | AnyFunT _ -> true

  (* weak mode *)

  (* these are types which are not really parsable, but they make sense to a
     human user in cases of autocompletion *)
  | OptionalT t
  | RestT t
  | TypeT (_, t)
  | LowerBoundT t
  | UpperBoundT t
  | ClassT t
    when weak
    ->
      is_printed_type_parsable_impl weak cx EnclosureNone t

  | VoidT _
    when weak
    ->
      true

  (* This gives really ugly output, but would need to figure out a better way
     to print these types otherwise, maybe substitute on printing? *)
  | TypeAppT (t, ts)
    when weak
    ->
      (is_printed_type_parsable_impl weak cx EnclosureAppT t) &&
      (is_printed_type_list_parsable weak cx EnclosureNone ts)

  | _
    ->
      false

and is_printed_type_list_parsable weak cx enclosure ts =
  List.fold_left (fun acc t ->
      acc && (is_printed_type_parsable_impl weak cx enclosure t)
    ) true ts

let is_printed_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureNone t

let is_printed_param_type_parsable ?(weak=false) cx t =
  is_printed_type_parsable_impl weak cx EnclosureParam t
