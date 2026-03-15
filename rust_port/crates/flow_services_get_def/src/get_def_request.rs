/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(Debug, Clone)]
pub struct MemberInfo<T> {
    pub prop_name: FlowSmolStr,
    pub object_type: T,
    pub force_instance: bool,
}

#[derive(Debug, Clone)]
pub enum GetDefRequest<M, T> {
    Identifier {
        name: FlowSmolStr,
        loc: M,
    },
    Member(MemberInfo<T>),
    JsxAttribute {
        component_t: T,
        name: FlowSmolStr,
        loc: M,
    },
}
