(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

type enum_search_result =
  | Empty
  | SingleEnum of (Reason.t * enum_t)
  | EnumInUnion
  | NonEnumTypes

let search_for_enum_type cx t =
  let rec f ((seen_ids, result) as acc) = function
    | DefT (reason, _, EnumT enum) ->
      let result =
        match result with
        | Empty -> SingleEnum (reason, enum)
        | SingleEnum _
        | EnumInUnion
        | NonEnumTypes ->
          EnumInUnion
      in
      (seen_ids, result)
    | OpenT (_, id) ->
      if ISet.mem id seen_ids then
        acc
      else
        List.fold_left f (ISet.add id seen_ids, result) (Flow_js.possible_types cx id)
    | UnionT (_, rep) -> List.fold_left f acc (UnionRep.members rep)
    | MaybeT (_, t)
    | OptionalT { type_ = t; _ } ->
      (match result with
      | Empty
      | NonEnumTypes ->
        f (seen_ids, NonEnumTypes) t
      | SingleEnum _
      | EnumInUnion ->
        (seen_ids, EnumInUnion))
    | AnnotT (_, t, _)
    | ReposT (_, t) ->
      f acc t
    | _ ->
      let result =
        match result with
        | Empty
        | NonEnumTypes ->
          NonEnumTypes
        | SingleEnum _
        | EnumInUnion ->
          EnumInUnion
      in
      (seen_ids, result)
  in
  let (_, result) = f (ISet.empty, Empty) t in
  result

let detect_invalid_check cx (t, (check_reason, check)) =
  match search_for_enum_type cx t with
  | Empty
  | NonEnumTypes ->
    ()
  | EnumInUnion ->
    Flow_js.add_output
      cx
      (Error_message.EEnumExhaustiveCheckOfUnion
         { reason = check_reason; union_reason = reason_of_t (Type_mapper.unwrap_type cx t) })
  | SingleEnum (enum_reason, { members; enum_name; enum_id; _ }) ->
    begin
      match check with
      | ExhaustiveCheckPossiblyValid { checks; default_case } ->
        let check_member (members_remaining, seen) (case_reason, member_name, check_t) =
          match search_for_enum_type cx check_t with
          | SingleEnum (_, { enum_id = check_enum_id; _ }) when ALoc.equal_id enum_id check_enum_id
            ->
            if not @@ SSet.mem member_name members_remaining then
              Flow_js.add_output
                cx
                (Error_message.EEnumMemberAlreadyChecked
                   {
                     reason = case_reason;
                     prev_check_reason = SMap.find member_name seen;
                     enum_reason;
                     member_name;
                   });
            (SSet.remove member_name members_remaining, SMap.add member_name case_reason seen)
          | _ ->
            let use_op = Op (ExhaustiveCheck { case = case_reason; switch = check_reason }) in
            Flow_js.add_output
              cx
              (Error_message.EIncompatibleWithUseOp (reason_of_t check_t, enum_reason, use_op));
            (members_remaining, seen)
        in
        let (left_over, _) = List.fold_left check_member (members, SMap.empty) checks in
        begin
          match (SSet.choose_opt left_over, default_case) with
          | (Some remaining_member_to_check, None) ->
            Flow_js.add_output
              cx
              (Error_message.EEnumNotAllChecked
                 {
                   reason = check_reason;
                   enum_reason;
                   remaining_member_to_check;
                   number_remaining_members_to_check = SSet.cardinal left_over;
                 })
          | (None, Some default_case_reason) ->
            Flow_js.add_output
              cx
              (Error_message.EEnumAllMembersAlreadyChecked
                 { reason = default_case_reason; enum_reason })
          | _ -> ()
        end
      | ExhaustiveCheckInvalid reasons ->
        List.iter
          (fun reason ->
            Flow_js.add_output cx (Error_message.EEnumInvalidCheck { reason; enum_name; members }))
          reasons
    end
