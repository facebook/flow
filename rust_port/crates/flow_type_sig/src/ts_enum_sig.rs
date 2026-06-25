/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// TypeScript enum member numbering and validation. This is the single source of
// truth for resolving a TS enum's members to their literal values, shared by the
// local-checking path ([ts_enum.rs's mk_ts_enum_namespace]) and the signature path
// ([type_sig_parse.rs]). The member value representation itself ([ts_enum_member_value])
// lives in [type_sig.rs] since it is part of the packed signature.

use dupe::Dupe;
use flow_common::js_number;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast_utils;

use crate::type_sig::TsEnumMemberValue;

// A TypeScript enum member that Flow accepts syntactically but TypeScript
// rejects. Reported by the local-checking path so the file errors instead of
// silently typing an invalid enum; the signature path ignores it (the model
// still degrades the member to [type_sig.rs's TSEnumMemberComputed]).
pub enum TsEnumMemberError {
    // A boolean or bigint initializer. TS enum members must be number or string
    // literals.
    TSEnumMemberInvalidLiteral,
    // A defaulted (uninitialized) member whose preceding member is not a numeric
    // constant, in a non-ambient enum: it cannot be auto-numbered (TypeScript's
    // "Enum member must have initializer"). Defaulted members are always allowed in
    // an ambient enum, where they are computed.
    TSEnumMemberMissingInitializer,
    // A member with a numeric name, e.g. `enum E { "1" = 5 }`. TypeScript rejects
    // this (TS2452 "An enum member cannot have a numeric name").
    TSEnumMemberNumericName,
}

// Whether [name] is a numeric name in TypeScript's sense, i.e. it round-trips
// through `Number(name).toString()` and is finite. `"NaN"`/`"Infinity"` are not
// numeric names (matching TS's `isInfinityOrNaNString` exception), nor are forms
// like `"0x1"`, `"01"`, or `"1e3"` whose canonical string differs. Identifiers
// can never be numeric names, so in practice this only fires for string-literal
// member names.
pub fn is_numeric_enum_member_name(name: &str) -> bool {
    match name.parse::<f64>() {
        #[expect(
            clippy::eq_op,
            reason = "finite: f - f == 0 is false for NaN and +/-Infinity (ports OCaml's `f -. f = 0.`)"
        )]
        Ok(f) if f - f == 0.0 => js_number::ecma_string_of_float(f) == name,
        _ => false,
    }
}

// Resolve each TypeScript enum member to its literal value, applying TS
// auto-numbering: a defaulted (uninitialized) member auto-increments from 0 (or
// from one past the preceding numeric member), and an explicit numeric
// initializer resets the counter. In an ambient enum (~ambient:true, i.e.
// `declare enum` / .d.ts), a defaulted member has no statically-known value and
// resolves to [type_sig.rs's TSEnumMemberComputed].
//
// Members that are invalid in TypeScript are paired with a [ts_enum_member_error]
// (see that type): boolean/bigint initializers and (in a non-ambient enum) a
// defaulted member that does not follow a numeric constant are also degraded to
// [type_sig.rs's TSEnumMemberComputed]; a numeric member name keeps its literal value
// (only the error is reported) so the rest of the enum still checks.
//
// Returns each member in source order as (name, member_loc, init_loc, value,
// error), where init_loc is the initializer's location for an initialized member
// and the member's location for a defaulted one.
pub fn ts_enum_member_values<M: Dupe>(
    ambient: bool,
    members: &[ast::statement::enum_declaration::Member<M>],
) -> Vec<(
    FlowSmolStr,
    M,
    M,
    TsEnumMemberValue,
    Option<TsEnumMemberError>,
)> {
    use ast::statement::enum_declaration::Member;
    let mut acc: Vec<(
        FlowSmolStr,
        M,
        M,
        TsEnumMemberValue,
        Option<TsEnumMemberError>,
    )> = Vec::new();
    let mut next: f64 = 0.0;
    let mut prev_numeric: bool = true;
    for member in members.iter() {
        let (id, member_loc, init_loc, value, error, next_prime, prev_numeric_prime) = match member
        {
            Member::NumberMember(im) => {
                let id = &im.id;
                let loc = im.loc.dupe();
                let (init_loc, lit) = (im.init.0.dupe(), &im.init.1);
                let value = lit.value;
                let raw = lit.raw.dupe();
                (
                    id,
                    loc,
                    init_loc,
                    TsEnumMemberValue::TSEnumMemberNumber(value, raw),
                    None,
                    value + 1.0,
                    true,
                )
            }
            Member::DefaultedMember(dm) => {
                let id = &dm.id;
                let loc = dm.loc.dupe();
                // Ambient: every uninitialized member is computed (even after a
                // numeric member), and that is valid TS.
                if ambient {
                    (
                        id,
                        loc.dupe(),
                        loc,
                        TsEnumMemberValue::TSEnumMemberComputed,
                        None,
                        next,
                        false,
                    )
                } else if prev_numeric {
                    (
                        id,
                        loc.dupe(),
                        loc,
                        TsEnumMemberValue::TSEnumMemberNumber(
                            next,
                            FlowSmolStr::from(js_number::ecma_string_of_float(next)),
                        ),
                        None,
                        next + 1.0,
                        true,
                    )
                } else {
                    // Can't auto-number: previous member is not a numeric constant.
                    (
                        id,
                        loc.dupe(),
                        loc,
                        TsEnumMemberValue::TSEnumMemberComputed,
                        Some(TsEnumMemberError::TSEnumMemberMissingInitializer),
                        next,
                        false,
                    )
                }
            }
            Member::StringMember(im) => {
                let id = &im.id;
                let loc = im.loc.dupe();
                let (init_loc, lit) = (im.init.0.dupe(), &im.init.1);
                let value = lit.value.dupe();
                (
                    id,
                    loc,
                    init_loc,
                    TsEnumMemberValue::TSEnumMemberString(value),
                    None,
                    next,
                    false,
                )
            }
            Member::BooleanMember(im) => {
                let id = &im.id;
                let loc = im.loc.dupe();
                let init_loc = im.init.0.dupe();
                (
                    id,
                    loc,
                    init_loc,
                    TsEnumMemberValue::TSEnumMemberComputed,
                    Some(TsEnumMemberError::TSEnumMemberInvalidLiteral),
                    next,
                    false,
                )
            }
            Member::BigIntMember(im) => {
                let id = &im.id;
                let loc = im.loc.dupe();
                let init_loc = im.init.0.dupe();
                (
                    id,
                    loc,
                    init_loc,
                    TsEnumMemberValue::TSEnumMemberComputed,
                    Some(TsEnumMemberError::TSEnumMemberInvalidLiteral),
                    next,
                    false,
                )
            }
        };
        let name = FlowSmolStr::new(ast_utils::string_of_enum_member_name(id));
        // A numeric member name is invalid TS regardless of the initializer; keep
        // any more specific error already found, otherwise flag the numeric name.
        // The value is retained (not degraded) so the rest of the enum checks.
        let error = match error {
            Some(_) => error,
            None => {
                if is_numeric_enum_member_name(&name) {
                    Some(TsEnumMemberError::TSEnumMemberNumericName)
                } else {
                    None
                }
            }
        };
        acc.push((name, member_loc, init_loc, value, error));
        next = next_prime;
        prev_numeric = prev_numeric_prime;
    }
    acc
}
