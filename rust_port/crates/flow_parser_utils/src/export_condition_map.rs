/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::ast::expression::ExpressionInner;
use flow_parser::loc::Loc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConditionValue {
    Null,
    Path(FlowSmolStr),
    Nested(Vec<(FlowSmolStr, ConditionValue)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportConditionMap {
    conditions: Vec<(FlowSmolStr, ConditionValue)>,
}

impl ExportConditionMap {
    pub fn empty() -> Self {
        ExportConditionMap {
            conditions: Vec::new(),
        }
    }

    pub fn create(conditions: Vec<(FlowSmolStr, ConditionValue)>) -> Self {
        ExportConditionMap { conditions }
    }

    fn create_from_shorthand_value(value: ConditionValue) -> Self {
        let conditions = vec![(FlowSmolStr::new("default"), value)];
        ExportConditionMap::create(conditions)
    }

    pub fn create_from_shorthand(path: FlowSmolStr) -> Self {
        ExportConditionMap::create_from_shorthand_value(ConditionValue::Path(path))
    }

    pub fn resolve_package_target(
        &self,
        pattern_match: Option<&str>,
        valid_conditions: &[FlowSmolStr],
    ) -> Option<FlowSmolStr> {
        pick_target(valid_conditions, pattern_match, &self.conditions)
    }

    pub fn parse(expr: &ast::expression::Expression<Loc, Loc>) -> Option<Self> {
        match expr.deref() {
            ExpressionInner::NullLiteral { .. } => Some(
                ExportConditionMap::create_from_shorthand_value(ConditionValue::Null),
            ),
            ExpressionInner::StringLiteral { inner, .. } => Some(
                ExportConditionMap::create_from_shorthand(inner.value.dupe()),
            ),
            ExpressionInner::Object { inner, .. } => {
                let conditions = parse_condition_map(&inner.properties);
                Some(ExportConditionMap::create(conditions))
            }
            _ => None,
        }
    }
}

fn is_targeted_condition(valid_conditions: &[FlowSmolStr], candidate_condition: &str) -> bool {
    valid_conditions
        .iter()
        .any(|c| c.as_str() == candidate_condition)
        || candidate_condition == "default"
}

fn pick_target(
    valid_conditions: &[FlowSmolStr],
    pattern_match: Option<&str>,
    conditions: &[(FlowSmolStr, ConditionValue)],
) -> Option<FlowSmolStr> {
    for (candidate_condition, value) in conditions {
        match value {
            ConditionValue::Null => {
                return None;
            }
            ConditionValue::Nested(child_condition_map) => {
                if is_targeted_condition(valid_conditions, candidate_condition.as_str()) {
                    if let Some(t) =
                        pick_target(valid_conditions, pattern_match, child_condition_map)
                    {
                        return Some(t);
                    }
                }
            }
            ConditionValue::Path(target_path) => {
                if is_targeted_condition(valid_conditions, candidate_condition.as_str()) {
                    return Some(match pattern_match {
                        Some(pattern_match) => {
                            FlowSmolStr::new(target_path.replacen('*', pattern_match, 1))
                        }
                        None => target_path.clone(),
                    });
                }
            }
        }
    }
    None
}

fn parse_condition_property_value(
    expr: &ast::expression::Expression<Loc, Loc>,
) -> Option<ConditionValue> {
    match expr.deref() {
        ExpressionInner::NullLiteral { .. } => Some(ConditionValue::Null),
        ExpressionInner::StringLiteral { inner, .. } => {
            Some(ConditionValue::Path(inner.value.dupe()))
        }
        ExpressionInner::Object { inner, .. } => Some(ConditionValue::Nested(parse_condition_map(
            &inner.properties,
        ))),
        _ => None,
    }
}

fn parse_condition_property(
    property: &ast::expression::object::Property<Loc, Loc>,
) -> Option<(FlowSmolStr, ConditionValue)> {
    if let ast::expression::object::Property::NormalProperty(
        ast::expression::object::NormalProperty::Init {
            key: ast::expression::object::Key::StringLiteral((_loc, lit)),
            value,
            ..
        },
    ) = property
    {
        if let Some(value) = parse_condition_property_value(value) {
            return Some((lit.value.dupe(), value));
        }
    }
    None
}

fn parse_condition_map(
    properties: &[ast::expression::object::Property<Loc, Loc>],
) -> Vec<(FlowSmolStr, ConditionValue)> {
    properties
        .iter()
        .filter_map(parse_condition_property)
        .collect()
}
