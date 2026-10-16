// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

//! Native Flow runtime emission shared with the Wasm entry point.

use dupe::Dupe;
use flow_parser::ParseOptions;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::offset_utils::OffsetTable;
use flow_parser_utils_output::indented_printer;
use flow_parser_utils_output::source_map::SourceMap;
use flow_parser_utils_output::source_map::SourceMapError;
use serde::Serialize;
use thiserror::Error;

use crate::babel_adapter;
use crate::babel_adapter::BabelLoweringError;
use crate::babel_adapter::BabelLoweringOptions;
use crate::babel_adapter::EnumRuntime;
pub use crate::babel_adapter::ReactRuntimeTarget;
use crate::strip_flow;

/// The module system used to load the emitted code and enum runtime.
#[derive(Debug, Clone, Copy, Default)]
pub enum SourceType {
    #[default]
    CommonJs,
    Module,
}

/// Runtime lowering options. Flow runtime syntax and type erasure are enabled.
#[derive(Debug, Default)]
pub struct RunnableOptions {
    pub source_type: SourceType,
    pub react_runtime_target: ReactRuntimeTarget,
    pub allow_return_outside_function: bool,
}

/// JavaScript and its source-map v3 document for the supplied source filename.
#[derive(Debug, Serialize)]
pub struct RunnableJs {
    pub code: String,
    pub map: SourceMap,
}

/// A source diagnostic uses one-based lines and zero-based UTF-16 columns.
#[derive(Debug, Error)]
pub enum RunnableError {
    #[error("{message}")]
    Syntax { message: String, loc: Position },
    #[error(transparent)]
    SourceMap(#[from] SourceMapError),
    #[error(transparent)]
    Printer(#[from] indented_printer::PrintError),
}

fn syntax_error(
    source: &str,
    location: &Loc,
    message: String,
) -> Result<RunnableError, SourceMapError> {
    let loc = OffsetTable::make(source)
        .convert_flow_position_to_js_position(location.start)
        .map_err(|error| SourceMapError::InvalidPosition(error.position))?;
    Ok(RunnableError::Syntax { message, loc })
}

/// Parse Flow source, lower its runtime constructs, erase types, and emit
/// indented JavaScript with a source map. Both outputs retain `source_filename`.
pub fn flow_to_runnable_js_with_source_map(
    source: &str,
    source_filename: &str,
    options: &RunnableOptions,
) -> Result<RunnableJs, RunnableError> {
    let file = FileKey::new(FileKeyInner::SourceFile(source_filename.to_owned()));
    let parse_options = ParseOptions {
        components: true,
        enums: true,
        pattern_matching: true,
        records: true,
        esproposal_decorators: false,
        types: true,
        ambiguous_types: true,
        enable_types_in_comments: false,
        use_strict: false,
        assert_operator: true,
        module_ref_prefix: None,
        ambient: false,
        allow_return_outside_function: options.allow_return_outside_function,
    };
    let (program, errors) = flow_parser::parse_program_file::<()>(
        false,
        None,
        Some(parse_options),
        file.dupe(),
        Ok(source),
    );
    if let Some((location, error)) = errors.into_iter().next() {
        return Err(syntax_error(source, &location, error.to_string())?);
    }
    let program = match babel_adapter::lower_program(
        source,
        &program,
        &BabelLoweringOptions {
            lower_enums: true,
            enum_runtime: match options.source_type {
                SourceType::CommonJs => EnumRuntime::Default,
                SourceType::Module => EnumRuntime::Import,
            },
            react_runtime_target: options.react_runtime_target,
        },
    ) {
        Ok(lowered) => strip_flow::lower_program(&lowered.program),
        Err(BabelLoweringError::Syntax { message, loc }) => {
            return Err(syntax_error(source, &loc, message)?);
        }
    };
    let (code, map) = indented_printer::print_with_source_map(&program, &file, source)?;
    Ok(RunnableJs {
        code: code.contents(),
        map,
    })
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn erases_types_and_emits_a_map_for_the_supplied_source() {
        let source = "type N = number;\nconst value: N = 1;\nthrow value;";
        let emitted =
            flow_to_runnable_js_with_source_map(source, "original.js", &RunnableOptions::default())
                .unwrap();
        assert_eq!(emitted.code, "const value = 1;\nthrow (value);\n");
        let map = serde_json::to_value(emitted.map).unwrap();
        assert_eq!(map["sources"], json!(["original.js"]));
        assert_eq!(map["sourcesContent"], json!([source]));
        assert_eq!(map["names"], json!(["value"]));
        assert_ne!(map["mappings"], "");
    }

    #[test]
    fn esm_enums_share_an_import_that_avoids_later_user_bindings() {
        let source = "enum E {A}\nenum F {B}\nconst $$gen$enum_runtime0 = 1;";
        let emitted = flow_to_runnable_js_with_source_map(
            source,
            "original.mjs",
            &RunnableOptions {
                source_type: SourceType::Module,
                ..RunnableOptions::default()
            },
        )
        .unwrap();
        assert!(
            emitted
                .code
                .starts_with("import $$gen$enum_runtime1 from 'flow-enums-runtime';\n")
        );
        assert_eq!(emitted.code.matches("flow-enums-runtime").count(), 1);
        assert_eq!(emitted.code.matches(".Mirrored").count(), 2);
        assert!(emitted.code.contains("const $$gen$enum_runtime0 = 1;"));
        let no_enum = flow_to_runnable_js_with_source_map(
            "export const x: number = 1;",
            "plain.mjs",
            &RunnableOptions {
                source_type: SourceType::Module,
                ..RunnableOptions::default()
            },
        )
        .unwrap();
        assert!(!no_enum.code.contains("flow-enums-runtime"));
    }

    #[test]
    fn preserves_the_interpreter_and_unicode_source_contents() {
        let source = "#!/usr/bin/env node\r\nconst text: string = '😀';";
        let emitted =
            flow_to_runnable_js_with_source_map(source, "unicode.js", &RunnableOptions::default())
                .unwrap();
        assert!(emitted.code.starts_with("#!/usr/bin/env node\n"));
        assert_eq!(
            serde_json::to_value(emitted.map).unwrap()["sourcesContent"],
            json!([source])
        );
    }

    #[test]
    fn syntax_diagnostics_use_utf16_columns() {
        assert!(matches!(
            flow_to_runnable_js_with_source_map(
                "const text = '😀'; const x = ;",
                "bad.js",
                &RunnableOptions::default()
            ),
            Err(RunnableError::Syntax {
                loc: Position {
                    line: 1,
                    column: 29
                },
                ..
            })
        ));
    }

    #[test]
    fn return_outside_function_is_an_explicit_option() {
        assert!(matches!(
            flow_to_runnable_js_with_source_map(
                "return 42;",
                "script.js",
                &RunnableOptions::default()
            ),
            Err(RunnableError::Syntax { .. })
        ));
        let emitted = flow_to_runnable_js_with_source_map(
            "return 42;",
            "script.js",
            &RunnableOptions {
                allow_return_outside_function: true,
                ..RunnableOptions::default()
            },
        )
        .unwrap();
        assert_eq!(emitted.code, "return (42);\n");
    }
}
