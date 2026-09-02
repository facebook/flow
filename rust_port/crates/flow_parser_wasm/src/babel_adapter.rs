/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

mod builders;
mod component_lowering;
mod enum_lowering;
mod error;
mod gen_id;
mod match_lowering;
mod record_lowering;
mod strip_flow;

#[cfg(test)]
mod tests;

pub use enum_lowering::EnumRuntime;
pub use error::BabelLoweringError;
use flow_parser::ast;
use flow_parser::loc::Loc;

pub struct BabelLoweringOptions {
    pub lower_enums: bool,
    pub enum_runtime: EnumRuntime,
    pub react_runtime_target: ReactRuntimeTarget,
}

pub const COMPONENT_FUNCTION_FLAG: u32 = 1 << 0;
pub const HOOK_FUNCTION_FLAG: u32 = 1 << 1;
pub const COMPONENT_PATTERN_OPTIONAL_FLAG: u32 = 1 << 2;
pub const COMPONENT_REST_TYPE_ANNOTATION_FLAG: u32 = 1 << 3;

pub struct BabelMetadata {
    component: component_lowering::ComponentMetadata,
}

impl BabelMetadata {
    pub fn flags_for_loc(&self, loc: &Loc) -> u32 {
        let mut flags = 0;
        if self.component.component_functions.contains(loc) {
            flags |= COMPONENT_FUNCTION_FLAG;
        }
        if self.component.hook_functions.contains(loc) {
            flags |= HOOK_FUNCTION_FLAG;
        }
        if self
            .component
            .component_patterns_with_optional
            .contains(loc)
        {
            flags |= COMPONENT_PATTERN_OPTIONAL_FLAG;
        }
        if self
            .component
            .component_rests_with_type_annotation
            .contains(loc)
        {
            flags |= COMPONENT_REST_TYPE_ANNOTATION_FLAG;
        }
        flags
    }
}

pub struct BabelProgram {
    pub program: ast::Program<Loc, Loc>,
    pub metadata: BabelMetadata,
}

#[derive(Clone, Copy)]
pub enum ReactRuntimeTarget {
    React18,
    React19,
}

pub fn lower_program(
    source: &str,
    program: &ast::Program<Loc, Loc>,
    options: &BabelLoweringOptions,
) -> Result<BabelProgram, BabelLoweringError> {
    let program = if options.lower_enums {
        enum_lowering::lower_program(program, options.enum_runtime)
    } else {
        program.clone()
    };

    let program = match_lowering::lower_program(source, &program)?;
    let (program, component) =
        component_lowering::lower_program(&program, options.react_runtime_target)?;
    let program = record_lowering::lower_program(&program)?;
    let program = strip_flow::lower_program(&program);

    Ok(BabelProgram {
        program,
        metadata: BabelMetadata { component },
    })
}
