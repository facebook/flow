/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::rc::Rc;

use flow_data_structure_wrapper::ord_map::FlowOrdMap;

use super::helpers::*;
use super::*;

#[allow(clippy::too_many_arguments)]
pub(super) fn enum_exhaustive_check(
    cx: &Context,
    trace: DepthTrace,
    check_reason: &Reason,
    enum_reason: &Reason,
    enum_info: &EnumConcreteInfo,
    mut possible_checks: VecDeque<(Type, EnumCheck)>,
    checks: &[EnumCheck],
    default_case_loc: Option<ALoc>,
    incomplete_out: &Type,
    discriminant_after_check: Option<&Type>,
) -> Result<(), FlowJsException> {
    let Some((obj_t, check)) = possible_checks.pop_front() else {
        // No possible checks left to resolve, analyze the exhaustive check.
        let members = &enum_info.members;
        let has_unknown_members = enum_info.has_unknown_members;

        let check_member = |members_remaining: &mut FlowOrdMap<FlowSmolStr, ALoc>,
                            seen: &mut FlowOrdMap<FlowSmolStr, ALoc>,
                            check: &EnumCheck|
         -> Result<(), FlowJsException> {
            let EnumCheck {
                case_test_loc,
                member_name,
            } = check;
            if !members_remaining.contains_key(member_name) {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked {
                        case_test_loc: case_test_loc.dupe(),
                        prev_check_loc: seen[member_name].dupe(),
                        enum_reason: enum_reason.dupe(),
                        member_name: member_name.dupe(),
                    }),
                )?;
            }
            members_remaining.remove(member_name);
            seen.insert(member_name.dupe(), case_test_loc.dupe());
            Ok(())
        };

        let mut members_remaining = members.dupe();
        let mut seen: FlowOrdMap<FlowSmolStr, ALoc> = FlowOrdMap::new();
        for check in checks {
            check_member(&mut members_remaining, &mut seen, check)?;
        }
        let left_over = members_remaining;
        match (left_over.is_empty(), default_case_loc, has_unknown_members) {
            (false, default_case_loc, _) => {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked {
                        reason: check_reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                        left_to_check: left_over.keys().duped().collect(),
                        default_case_loc,
                    }),
                )?;
                // enum_exhaustive_check_incomplete cx ~trace ~reason:check_reason incomplete_out
                enum_exhaustive_check_incomplete(cx, trace, check_reason, None, incomplete_out)?;
            }
            // When we have unknown members, a default is required
            // even when we've checked all known members.
            (true, None, true) => {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked {
                        reason: check_reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                    }),
                )?;
                enum_exhaustive_check_incomplete(cx, trace, check_reason, None, incomplete_out)?;
            }
            (true, Some(_), true) => {}
            (true, Some(default_case_loc), false) => {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked {
                        loc: default_case_loc,
                        enum_reason: enum_reason.dupe(),
                    }),
                )?;
            }
            _ => {}
        }
        return Ok(());
    };

    // There are still possible checks to resolve, continue to resolve them.
    let exhaustive_check = UseT::new(UseTInner::EnumExhaustiveCheckT {
        reason: check_reason.dupe(),
        check: Box::new(
            EnumPossibleExhaustiveCheckT::EnumExhaustiveCheckPossiblyValid {
                tool: EnumExhaustiveCheckToolT::EnumResolveCaseTest {
                    discriminant_enum: enum_info.dupe(),
                    discriminant_reason: enum_reason.dupe(),
                    check: check.clone(),
                },
                possible_checks,
                checks: Rc::from(checks),
                default_case_loc,
            },
        ),
        incomplete_out: incomplete_out.dupe(),
        discriminant_after_check: discriminant_after_check.map(|t| t.dupe()),
    });
    rec_flow(cx, trace, (&obj_t, &exhaustive_check))
}

pub(super) fn enum_exhaustive_check_incomplete(
    cx: &Context,
    trace: DepthTrace,
    reason: &Reason,
    trigger: Option<&Type>,
    incomplete_out: &Type,
) -> Result<(), FlowJsException> {
    let default_trigger = void::why(reason.dupe());
    let trigger = trigger.unwrap_or(&default_trigger);
    rec_flow_t(cx, trace, unknown_use(), (trigger, incomplete_out))
}
