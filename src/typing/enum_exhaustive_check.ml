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
  | Other

let search_for_enum_object_type cx t =
  let rec f ((seen_ids, result) as acc) = function
    | DefT (reason, _, EnumObjectT enum) ->
      let result =
        match result with
        | Empty -> SingleEnum (reason, enum)
        | SingleEnum _
        | Other ->
          Other
      in
      (seen_ids, result)
    | OpenT (_, id) ->
      if ISet.mem id seen_ids then
        acc
      else
        List.fold_left f (ISet.add id seen_ids, result) (Flow_js.possible_types cx id)
    | AnnotT (_, t, _)
    | ReposT (_, t) ->
      f acc t
    | _ -> (seen_ids, Other)
  in
  let (_, result) = f (ISet.empty, Empty) t in
  result

let detect_invalid_check
    cx
    {
      Context.check_reason;
      enum_reason;
      enum = { members; enum_id; _ };
      exhaustive_check = { checks; default_case };
    } =
  let check_member (members_remaining, seen) (EnumCheck { reason; member_name; obj_t }) =
    match search_for_enum_object_type cx obj_t with
    | SingleEnum (_, { enum_id = check_enum_id; _ })
      when ALoc.equal_id enum_id check_enum_id && SMap.mem member_name members ->
      if not @@ SMap.mem member_name members_remaining then
        Flow_js.add_output
          cx
          (Error_message.EEnumMemberAlreadyChecked
             { reason; prev_check_reason = SMap.find member_name seen; enum_reason; member_name });
      (SMap.remove member_name members_remaining, SMap.add member_name reason seen)
    | _ -> (members_remaining, seen)
  in
  let (left_over, _) = List.fold_left check_member (members, SMap.empty) checks in
  match (SMap.is_empty left_over, default_case) with
  | (false, None) ->
    Flow_js.add_output
      cx
      (Error_message.EEnumNotAllChecked
         { reason = check_reason; enum_reason; left_to_check = SMap.keys left_over })
  | (true, Some default_case_reason) ->
    Flow_js.add_output
      cx
      (Error_message.EEnumAllMembersAlreadyChecked { reason = default_case_reason; enum_reason })
  | _ -> ()
