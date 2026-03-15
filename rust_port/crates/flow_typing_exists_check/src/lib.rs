/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_parser::loc_sig::LocSig;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ExistsCheck {
    pub null_loc: Option<ALoc>,
    pub bool_loc: Option<ALoc>,
    pub string_loc: Option<ALoc>,
    pub number_loc: Option<ALoc>,
    pub bigint_loc: Option<ALoc>,
    pub mixed_loc: Option<ALoc>,
    pub enum_bool_loc: Option<ALoc>,
    pub enum_string_loc: Option<ALoc>,
    pub enum_number_loc: Option<ALoc>,
    pub enum_bigint_loc: Option<ALoc>,
}

impl ExistsCheck {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn debug_to_string(&self) -> String {
        fn string_of_loc_option(loc: &Option<ALoc>) -> String {
            match loc {
                None => "None".to_string(),
                Some(loc) => loc.debug_to_string(true),
            }
        }

        let fields = [
            ("null_loc", &self.null_loc),
            ("bool_loc", &self.bool_loc),
            ("string_loc", &self.string_loc),
            ("number_loc", &self.number_loc),
            ("bigint_loc", &self.bigint_loc),
            ("mixed_loc", &self.mixed_loc),
            ("enum_bool_loc", &self.enum_bool_loc),
            ("enum_string_loc", &self.enum_string_loc),
            ("enum_number_loc", &self.enum_number_loc),
            ("enum_bigint_loc", &self.enum_bigint_loc),
        ];

        let inner: String = fields
            .iter()
            .map(|(name, loc_opt)| format!("  {}: {};\n", name, string_of_loc_option(loc_opt)))
            .collect();

        format!("{{\n{}}}", inner)
    }
}
