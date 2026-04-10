/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::ops::Deref;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::loc::Loc;

use crate::export_condition_map::ExportConditionMap;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct PackageExports {
    subpaths: HashMap<FlowSmolStr, ExportConditionMap>,
}

impl PackageExports {
    pub fn empty() -> Self {
        PackageExports {
            subpaths: HashMap::new(),
        }
    }

    pub fn create(subpaths: HashMap<FlowSmolStr, ExportConditionMap>) -> Self {
        PackageExports { subpaths }
    }

    fn create_shorthand_index_exports(export_path: FlowSmolStr) -> Self {
        let condition_map = ExportConditionMap::create_from_shorthand(export_path);
        let mut subpaths = HashMap::new();
        subpaths.insert(FlowSmolStr::new("."), condition_map);
        PackageExports::create(subpaths)
    }

    fn resolve_wildcard_package(
        &self,
        subpath: &str,
        conditions: &[FlowSmolStr],
    ) -> Option<FlowSmolStr> {
        let mut expansion_keys: Vec<&FlowSmolStr> = self
            .subpaths
            .keys()
            .filter(|key| key.contains('*'))
            .collect();
        expansion_keys.sort_by(|a, b| pattern_key_compare(a, b));

        resolve_matched_wildcard_path(&self.subpaths, conditions, subpath, &expansion_keys)
    }

    pub fn resolve_package(
        &self,
        subpath: &str,
        conditions: &[FlowSmolStr],
    ) -> Option<FlowSmolStr> {
        match self.subpaths.get(subpath) {
            Some(condition_map) => condition_map.resolve_package_target(None, conditions),
            None => self.resolve_wildcard_package(subpath, conditions),
        }
    }

    pub fn parse(expr: &ast::expression::Expression<Loc, Loc>) -> Option<Self> {
        match expr.deref() {
            ExpressionInner::StringLiteral { inner, .. } => Some(
                PackageExports::create_shorthand_index_exports(inner.value.dupe()),
            ),
            ExpressionInner::Object { inner, .. } => {
                if is_conditional_exports_main_sugar(&inner.properties) {
                    parse_conditional_exports_main_sugar(expr)
                } else {
                    Some(parse_package_exports_obj(&inner.properties))
                }
            }
            _ => None,
        }
    }
}

fn pattern_key_compare(a: &str, b: &str) -> std::cmp::Ordering {
    let base_length_a = a.find('*').map_or(-1, |i| i as i32);
    let base_length_b = b.find('*').map_or(-1, |i| i as i32);

    if base_length_a > base_length_b {
        std::cmp::Ordering::Less
    } else if base_length_b > base_length_a {
        std::cmp::Ordering::Greater
    } else {
        let length_a = a.len();
        let length_b = b.len();
        if length_a > length_b {
            std::cmp::Ordering::Less
        } else if length_b > length_a {
            std::cmp::Ordering::Greater
        } else {
            std::cmp::Ordering::Equal
        }
    }
}

fn resolve_matched_wildcard_path(
    subpath_map: &HashMap<FlowSmolStr, ExportConditionMap>,
    conditions: &[FlowSmolStr],
    match_key: &str,
    expansion_keys: &[&FlowSmolStr],
) -> Option<FlowSmolStr> {
    for expansion_key in expansion_keys {
        if let Some(pattern_base_index) = expansion_key.find('*') {
            let pattern_base = &expansion_key[..pattern_base_index];

            if match_key.starts_with(pattern_base) && match_key != pattern_base {
                let pattern_trailer = &expansion_key[pattern_base_index + 1..];

                if pattern_trailer.is_empty()
                    || (match_key.ends_with(pattern_trailer)
                        && match_key.len() >= expansion_key.len())
                {
                    if let Some(target) = subpath_map.get(*expansion_key) {
                        let pattern_match =
                            &match_key[pattern_base.len()..match_key.len() - pattern_trailer.len()];

                        return target.resolve_package_target(Some(pattern_match), conditions);
                    }
                }
            }
        }
    }
    None
}

/// Given a list of JSON properties, loosely extract the properties and turn it into a
/// [Expression.t SMap.t]. We aren't looking to validate the file, and don't currently
/// care about any non-literal properties, so we skip over everything else.
fn extract_property(
    map: &mut HashMap<FlowSmolStr, ast::expression::Expression<Loc, Loc>>,
    property: &ast::expression::object::Property<Loc, Loc>,
) {
    if let ast::expression::object::Property::NormalProperty(
        ast::expression::object::NormalProperty::Init {
            key: ast::expression::object::Key::StringLiteral((_loc, lit)),
            value,
            ..
        },
    ) = property
    {
        map.insert(lit.value.dupe(), value.clone());
    }
}

fn collect_export_paths(
    input_path: FlowSmolStr,
    value: &ast::expression::Expression<Loc, Loc>,
    subpaths_map: &mut HashMap<FlowSmolStr, ExportConditionMap>,
) {
    if let Some(condition_map) = ExportConditionMap::parse(value) {
        subpaths_map.insert(input_path, condition_map);
    }
}

fn parse_package_exports_obj(
    properties: &[ast::expression::object::Property<Loc, Loc>],
) -> PackageExports {
    let mut prop_map = HashMap::new();
    for property in properties {
        extract_property(&mut prop_map, property);
    }

    let mut subpaths = HashMap::new();
    for (input_path, value) in prop_map {
        collect_export_paths(input_path, &value, &mut subpaths);
    }

    PackageExports::create(subpaths)
}

fn parse_conditional_exports_main_sugar(
    obj: &ast::expression::Expression<Loc, Loc>,
) -> Option<PackageExports> {
    match ExportConditionMap::parse(obj) {
        Some(condition_map) => {
            let mut subpaths = HashMap::new();
            subpaths.insert(FlowSmolStr::new("."), condition_map);
            Some(PackageExports::create(subpaths))
        }
        None => None,
    }
}

fn is_conditional_exports_main_sugar(
    properties: &[ast::expression::object::Property<Loc, Loc>],
) -> bool {
    fn is_path_prop(property: &ast::expression::object::Property<Loc, Loc>) -> bool {
        if let ast::expression::object::Property::NormalProperty(
            ast::expression::object::NormalProperty::Init {
                key: ast::expression::object::Key::StringLiteral((_loc, lit)),
                ..
            },
        ) = property
        {
            lit.value.starts_with('.')
        } else {
            false
        }
    }

    !properties.iter().any(is_path_prop)
}
