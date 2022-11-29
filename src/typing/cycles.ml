(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Name_def_ordering

let handle_element cx elt =
  match elt with
  | Normal _
  | Resolvable _ ->
    false
  | Illegal { reason; recursion; payload = _; annot_locs } ->
    Flow_js.add_output cx (Error_message.ERecursiveDefinition { reason; recursion; annot_locs });
    true

let key_of_element elt =
  match elt with
  | Normal key
  | Resolvable key
  | Illegal { payload = key; _ } ->
    key

let handle_component cx graph scc =
  match scc with
  | Singleton elt ->
    let (_ : bool) = handle_element cx elt in
    ()
  | ResolvableSCC elts ->
    let (_ : _ Nel.t) = Nel.map (handle_element cx) elts in
    ()
  | IllegalSCC elts_blame ->
    let blame =
      Base.List.filter_map
        ~f:(fun ({ payload = elt; reason; recursion = blame; annot_locs }, display) ->
          let illegal_elt = handle_element cx elt in
          if display then
            let (def, _, _, _) = Env_api.EnvMap.find (key_of_element elt) graph in
            Some ((def, illegal_elt), (reason, blame, annot_locs))
          else
            None)
        (Nel.to_list elts_blame)
    in
    (* If at least one element of the cycle is recursive, and every element is
       either an expression or a recursive element, don't emit the cycle error
       -- the recursion error will contain all the actionable advice *)
    (match
       Base.List.fold_result blame ~init:false ~f:(fun has_illegal ((def, illegal_elt), _) ->
           match def with
           | Name_def.ExpressionDef _ -> Ok (has_illegal || illegal_elt)
           | _ when illegal_elt -> Ok true
           | _ -> Error ()
       )
     with
    | Ok true -> ()
    | Ok false
    | Error () ->
      Flow_js.add_output
        cx
        Error_message.(EDefinitionCycle (Base.List.map ~f:snd blame |> Nel.of_list_exn)))
