/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::js_number;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast_utils;

pub fn loc_and_string_of_property_key<Loc: Dupe>(
    key: &ast::expression::object::Key<Loc, Loc>,
) -> Option<(Loc, FlowSmolStr)> {
    use ast::expression::object::Key;
    match key {
        Key::Computed(_) | Key::PrivateName(_) => None,
        Key::Identifier(id) => Some((id.loc.dupe(), id.name.dupe())),
        Key::StringLiteral((loc, lit)) => Some((loc.dupe(), lit.value.dupe())),
        Key::NumberLiteral((loc, lit)) => Some((
            loc.dupe(),
            FlowSmolStr::new(js_number::ecma_string_of_float(lit.value)),
        )),
        Key::BigIntLiteral((loc, lit)) => Some((
            loc.dupe(),
            FlowSmolStr::new(ast_utils::string_of_bigint(lit)),
        )),
    }
}

pub fn defaulted_props_of_record<Loc: Dupe>(
    record: &ast::statement::RecordDeclaration<Loc, Loc>,
) -> BTreeSet<FlowSmolStr> {
    use ast::statement::record_declaration;

    record
        .body
        .body
        .iter()
        .filter_map(|element| {
            if let record_declaration::BodyElement::Property(record_declaration::Property {
                key,
                default_value: Some(_),
                ..
            }) = element
            {
                return loc_and_string_of_property_key(key).map(|(_, name)| name);
            }
            None
        })
        .collect()
}
