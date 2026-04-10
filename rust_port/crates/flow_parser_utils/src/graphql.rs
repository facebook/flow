/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::ast;
use lazy_static::lazy_static;
use regex::Regex;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum GraphqlError {
    InvalidTaggedTemplate,
    InvalidGraphQL,
}

// GraphQL spec references:
// Comment: https://spec.graphql.org/June2018/#sec-Comments
// Ignored tokens (includes commas): https://spec.graphql.org/June2018/#sec-Source-Text.Ignored-Tokens
const COMMENT_REGEXP_STR: &str = r"(?m)^[ \t\n\r,]*#.*";

// GraphQL spec references:
// OperationType: https://spec.graphql.org/June2018/#OperationType
// FragmentDefinition: https://spec.graphql.org/June2018/#FragmentDefinition
// Whitespace: https://spec.graphql.org/June2018/#sec-White-Space
// Ignored tokens: (includes commas) https://spec.graphql.org/June2018/#sec-Source-Text.Ignored-Tokens
// Name: https://spec.graphql.org/June2018/#Name
const NAME_REGEXP_STR: &str =
    r"^[ \t\n\r,]*(?:query|mutation|subscription|fragment)[ \t\n\r,]+([_A-Za-z][_0-9A-Za-z]*)";

pub fn extract_module_name<Loc: Dupe>(
    quasi: &ast::expression::TemplateLiteral<Loc, Loc>,
    module_prefix: Option<&str>,
) -> Result<String, GraphqlError> {
    lazy_static! {
        static ref COMMENT_REGEXP: Regex = Regex::new(COMMENT_REGEXP_STR).unwrap();
        static ref NAME_REGEXP: Regex = Regex::new(NAME_REGEXP_STR).unwrap();
    }
    if quasi.quasis.len() != 1 || !quasi.expressions.is_empty() {
        return Err(GraphqlError::InvalidTaggedTemplate);
    }
    let element = &quasi.quasis[0];
    if !element.tail {
        return Err(GraphqlError::InvalidTaggedTemplate);
    }
    let cooked = &element.value.cooked;
    // Remove GraphQL line comments first.
    let comment_free = COMMENT_REGEXP.replace_all(cooked, "");
    if let Some(captures) = NAME_REGEXP.captures(&comment_free) {
        if let Some(name_match) = captures.get(1) {
            let name = name_match.as_str();
            let prefix = module_prefix.unwrap_or("");
            Ok(format!("{}{}.graphql", prefix, name))
        } else {
            Err(GraphqlError::InvalidGraphQL)
        }
    } else {
        Err(GraphqlError::InvalidGraphQL)
    }
}
