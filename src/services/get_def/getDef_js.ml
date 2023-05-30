(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Get_def_result = struct
  type t =
    | Def of Loc.t list  (** the final location of the definition *)
    | Partial of Loc.t list * string
        (** if an intermediate get-def failed, return partial progress and the error message *)
    | Bad_loc of string  (** the input loc didn't point at anything you can call get-def on *)
    | Def_error of string  (** an unexpected, internal error *)
end

open Get_def_result

let extract_member_def ~loc_of_aloc ~cx ~file_sig ~typed_ast ~force_instance type_ name =
  let ( let* ) = Result.bind in
  let* Ty_members.{ members; _ } =
    Ty_members.extract
      ~force_instance
      ~cx
      ~typed_ast
      ~file_sig
      Type.TypeScheme.{ tparams_rev = []; type_ }
  in
  let def_locs =
    Base.Option.bind
      (NameUtils.Map.find_opt (Reason.OrdinaryName name) members)
      ~f:(fun Ty_members.{ def_locs; _ } -> Nel.of_list def_locs
    )
  in
  match def_locs with
  | Some def_locs -> Ok (Nel.map loc_of_aloc def_locs)
  | None -> Error (Printf.sprintf "failed to find member %s in members map" name)

let rec process_request ~options ~loc_of_aloc ~cx ~is_legit_require ~ast ~typed_ast ~file_sig :
    (ALoc.t, ALoc.t * Type.t) Get_def_request.t -> (Loc.t Nel.t, string) result = function
  | Get_def_request.Identifier { name = _; loc = (aloc, type_) } ->
    let loc = loc_of_aloc aloc in
    let scope_info =
      Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:true ast
    in
    let all_uses = Scope_api.With_Loc.all_uses scope_info in
    let matching_uses = Loc_collections.LocSet.filter (fun use -> Loc.contains use loc) all_uses in
    (match Loc_collections.LocSet.elements matching_uses with
    | [use] ->
      let def = Scope_api.With_Loc.def_of_use scope_info use in
      let def_loc = def.Scope_api.With_Loc.Def.locs in
      Ok def_loc
    | [] ->
      process_request
        ~options
        ~loc_of_aloc
        ~cx
        ~is_legit_require
        ~ast
        ~typed_ast
        ~file_sig
        (Get_def_request.Type (aloc, type_))
    | _ :: _ :: _ -> Error "Scope builder found multiple matching identifiers")
  | Get_def_request.(Member { prop_name = name; object_type = (_loc, t); force_instance }) ->
    extract_member_def ~loc_of_aloc ~cx ~file_sig ~typed_ast ~force_instance t name
  | Get_def_request.(Type (_, v) | Typeof (_, v)) as request ->
    (* here lies the difference between "Go to Definition" and "Go to Type Definition":
       the former should stop on annot_loc (where the value was annotated), while the
       latter should jump to the def_loc (where the type was defined).

       for now, we only implement Go to Definition; if we want to do Go to Type
       Definition, it would ignore the annot loc. *)
    let rec loop =
      let open Type in
      function
      | OpenT _ as t ->
        (match Flow_js_utils.possible_types_of_type cx t with
        | [t'] -> loop t'
        | [] -> Error "No possible types"
        | _ :: _ -> Error "More than one possible type")
      | AnnotT (r, t, _) ->
        (match request with
        | Get_def_request.Typeof _ -> loop t
        | _ ->
          (* `annot_aloc` is set when an AnnotT is the result of an actual source annotation *)
          (match Reason.annot_loc_of_reason r with
          | Some aloc -> Ok (Nel.one (loc_of_aloc aloc))
          | None -> loop t))
      | DefT (_, _, TypeT ((ImportTypeofKind | ImportClassKind | ImportEnumKind), t)) -> loop t
      | t ->
        let r = TypeUtil.reason_of_t t in
        let aloc =
          match Reason.annot_loc_of_reason r with
          | Some aloc -> aloc
          | None -> Reason.def_loc_of_reason r
        in
        Ok (Nel.one (loc_of_aloc aloc))
    in
    loop v
  | Get_def_request.JsxAttribute { component_t = (_, component_t); name; loc } ->
    let reason = Reason.mk_reason (Reason.RCustom name) loc in
    let props_object =
      Tvar.mk_where cx reason (fun tvar ->
          let use_op = Type.Op Type.UnknownUse in
          Flow_js.flow cx (component_t, Type.ReactKitT (use_op, reason, Type.React.GetConfig tvar))
      )
    in
    process_request
      ~options
      ~loc_of_aloc
      ~cx
      ~is_legit_require
      ~ast
      ~typed_ast
      ~file_sig
      Get_def_request.(
        Member { prop_name = name; object_type = (loc, props_object); force_instance = false }
      )

module Depth = struct
  let limit = 9999

  type t = {
    length: int;
    locs: Loc.t list;
  }

  let empty = { length = 0; locs = [] }

  let add loc { length; locs } = { length = length + 1; locs = loc :: locs }
end

let get_def ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast requested_loc =
  let require_loc_map = File_sig.require_loc_map file_sig in
  let is_legit_require (source_aloc, _) =
    let source_loc = loc_of_aloc source_aloc in
    SMap.exists (fun _ locs -> Nel.exists (fun loc -> loc = source_loc) locs) require_loc_map
  in
  let rec loop ~depth req_loc =
    if depth.Depth.length > Depth.limit then
      let trace_str =
        depth.Depth.locs |> Base.List.map ~f:Reason.string_of_loc |> Base.String.concat ~sep:"\n"
      in
      let log_message =
        Printf.sprintf
          "GetDef_js loop depth exceeded %d. Trace (most recent first):\n%s"
          Depth.limit
          trace_str
      in
      Partial ([req_loc], log_message)
    else
      let open Get_def_process_location in
      match process_location_in_typed_ast ~is_legit_require ~typed_ast req_loc with
      | OwnDef aloc -> Def [loc_of_aloc aloc]
      | Request request -> begin
        match
          process_request
            ~options
            ~loc_of_aloc
            ~cx
            ~is_legit_require
            ~ast
            ~typed_ast
            ~file_sig
            request
        with
        | Ok res_locs ->
          res_locs
          |> Nel.map (fun res_loc ->
                 (* two scenarios where we stop trying to recur:
                    - when req_loc = res_loc, meaning we've reached a fixed point so
                      continuing would make us infinite loop
                    - when res_loc is not in the file the request originated from, meaning
                      the typed_ast we have is the wrong one to recur with *)
                 if Loc.equal req_loc res_loc || Loc.(res_loc.source <> requested_loc.source) then
                   Def [res_loc]
                 else
                   match loop ~depth:(Depth.add req_loc depth) res_loc with
                   | Bad_loc _ -> Def [res_loc]
                   | Def_error msg -> Partial ([res_loc], msg)
                   | (Def _ | Partial _) as res -> res
             )
          |> Nel.reduce (fun res1 res2 ->
                 match (res1, res2) with
                 | (Def locs1, Def locs2) -> Def (Base.List.unordered_append locs1 locs2)
                 | (Partial (locs1, msg1), Partial (locs2, msg2)) ->
                   Partial (Base.List.unordered_append locs1 locs2, msg1 ^ msg2)
                 | (Def locs1, Partial (locs2, msg))
                 | (Partial (locs1, msg), Def locs2) ->
                   Partial (Base.List.unordered_append locs1 locs2, msg)
                 | ((Bad_loc _ | Def_error _), other)
                 | (other, (Bad_loc _ | Def_error _)) ->
                   other
             )
        | Error msg -> Def_error msg
      end
      | Empty msg -> Bad_loc msg
      | LocNotFound -> Bad_loc "not found"
  in
  loop ~depth:Depth.empty requested_loc
