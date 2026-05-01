/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! WASM entry point for the Flow Rust parser.
//!
//! Exports the same C ABI functions as Hermes (`hermesParse`, `hermesParseResult_*`)
//! so the existing `HermesParser.js` can load this WASM binary as a drop-in replacement.
//!
//! # Crate structure
//!
//! - [`node_kinds`] — Schema-driven node definitions (`define_nodes!` macro).
//!   Single source of truth for node IDs, names, and property schemas.
//! - [`serializer`] — Binary encoder: Flow AST → `programBuffer` (Vec<u32>).
//! - [`position`] — Source position tracking (UTF-16 offsets for JS).
//! - `bin/codegen.rs` — Generates `FlowParserNodeDeserializers.js` from the schema.
//!   Run via `buck run fbcode//flow/rust_port/crates/flow_parser_wasm:codegen`.

pub mod node_kinds;
pub mod position;
pub mod serializer;

use std::ffi::CString;

use position::PositionResult;

/// Opaque parse result stored in WASM memory.
pub struct ParseResult {
    error: Option<CString>,
    error_line: u32,
    error_column: u32,
    program_buffer: Vec<u32>,
    position_buffer: Vec<PositionResult>,
    /// Concatenated UTF-8 bytes for every string written by the
    /// serializer. The program buffer encodes string references as
    /// `(offset+1, len)` into this buffer, with 0 reserved as the null
    /// sentinel. The buffer must stay alive until the ParseResult is
    /// freed because the JS-side deserializer dereferences pointers
    /// computed from the base pointer + offset.
    string_buffer: Vec<u8>,
}

/// Saturate a possibly-negative line/column (`Loc.none` uses `-1`) to a
/// nonnegative `u32`. A naive `as u32` cast wraps `-1` to `4294967295`,
/// which is meaningless on the JS side (and worse, looks like a real
/// position). Clamp to 0 instead so callers see "line 0 / col 0" for
/// unknown locations.
fn nonneg_u32(v: i32) -> u32 {
    v.max(0) as u32
}

/// Parse source and return an opaque ParseResult pointer.
///
/// Arguments match the Hermes WASM API plus an extra `source_filename`
/// pair for plumbing the source path through to the parser. The path is
/// used for file-extension based parser modes (`.flow`, `.d.ts`,
/// `.d.cts`, `.d.mts`) — without it, ambient detection is impossible and
/// declarations like bodyless class methods inside `.d.ts` cannot be
/// classified correctly.
///
/// - source: pointer to UTF-8 source text in WASM memory (null-terminated)
/// - source_size: length including null terminator
/// - source_filename: pointer to UTF-8 filename in WASM memory (null-terminated, may be null)
/// - source_filename_size: length including null terminator (0 if filename is null)
/// - enable_components: enable component syntax
/// - enable_match: enable match expression syntax
/// - enable_decorators: enable Stage-1 decorator syntax (when 0, `@` is a parse error)
/// - tokens: if true, serialize token information
/// - allow_return_outside: accepted but unused; OCaml flow_parser's
///   `parse_options` has no equivalent gate, and the `IllegalReturn`
///   diagnostic is raised unconditionally at statement_parser.rs:722-723
///   when `!env.in_function()`. The hermes-parser-compatible
///   `allowReturnOutsideFunction` semantics are implemented at the JS
///   adapter layer (src/index.js) by filtering this specific diagnostic
///   from the error list before throwing — keeping the Rust port faithful
///   to OCaml without adding a new option to ParseOptions.
/// - assert_operator: enable Flow `expr!` non-null assertion
/// - enable_enums: enable Flow enum syntax (off by default per OCaml `default_parse_options`)
/// - enable_records: enable Flow record syntax (`#{ ... }`); off by default
/// - enable_types: enable Flow/TS type-grammar (on by default; fixtures opt out via `types: false`)
/// - enable_types_pragma_detection: when non-zero, scan the docblock for
///   `@flow` and override `enable_types` based on whether the pragma is
///   present (matches Hermes' C++ pragma scan in
///   `xplat/static_h/lib/Parser/FlowHelpers.cpp`). When zero, `enable_types`
///   is honored verbatim. The pragma scan walks leading whitespace, `//` and
///   `/* */` comments, and directive-prologue string literals up to the
///   first non-directive token; in each collected comment it searches for
///   `@flow` followed by a non-word boundary.
/// - source_type: 0 = unspecified, 1 = script, 2 = module. flow_parser has
///   no script/module gate (parser_env.rs:218): `init_env` pins
///   `allow_yield: false, allow_await: false` at the top level
///   (parser_env.rs:392-393), while `parse_module_body_with_directives`
///   (statement_parser.rs:5106) unconditionally accepts top-level
///   `import`/`export`. All three values map to the same parse: imports
///   and exports are accepted regardless, and top-level `await` is
///   rejected with the same `Parse_error.AwaitAsIdentifierReference` that
///   the OCaml flow_parser raises (parser_common.ml:481-491) — OCaml has
///   no top-level-await gate either, so accepting `module` here matches
///   OCaml semantics rather than diverging from it.
#[unsafe(no_mangle)]
pub extern "C" fn hermesParse(
    source: *const u8,
    source_size: usize,
    source_filename: *const u8,
    source_filename_size: usize,
    enable_components: i32,
    enable_match: i32,
    enable_decorators: i32,
    tokens: i32,
    _allow_return_outside: i32,
    assert_operator: i32,
    enable_enums: i32,
    enable_records: i32,
    enable_types: i32,
    source_type: i32,
    enable_types_pragma_detection: i32,
) -> *mut ParseResult {
    match source_type {
        0..=2 => {}
        other => panic!(
            "hermesParse: invalid source_type={other}; expected 0 (unspecified), \
             1 (script), or 2 (module)."
        ),
    }

    // SAFETY: The JS bridge in flow-parser-oxidized/src/FlowParser.js wraps
    // every call to `hermesParse` in a try/finally that allocates `source`
    // via `_malloc`, copies `source_size` bytes into it, calls this
    // function, and frees the allocation in `finally`. The pointer is
    // therefore valid for `source_size` bytes for the duration of this
    // call. `source_size` is the JS-side `Buffer.length + 1` (UTF-8 source
    // plus a null terminator), so the slice is in-bounds.
    let source_bytes = unsafe { std::slice::from_raw_parts(source, source_size) };
    // Source is null-terminated; exclude the null for parsing
    let source_len = if source_size > 0 { source_size - 1 } else { 0 };
    let source_str = match std::str::from_utf8(&source_bytes[..source_len]) {
        Ok(s) => s,
        Err(_) => {
            let result = Box::new(ParseResult {
                error: CString::new("Invalid UTF-8 in source").ok(),
                error_line: 1,
                error_column: 0,
                program_buffer: Vec::new(),
                position_buffer: Vec::new(),
                string_buffer: Vec::new(),
            });
            return Box::into_raw(result);
        }
    };

    // Decode the optional filename. Mirrors source decoding — null pointer or
    // zero-length means "no filename", which collapses to an empty `SourceFile`
    // (matching the original behavior). Invalid UTF-8 is treated the same way
    // rather than failing the whole parse, since the filename only affects
    // file-extension dispatch.
    let filename_str: String = if source_filename.is_null() || source_filename_size == 0 {
        String::new()
    } else {
        // SAFETY: When non-null, the JS bridge mallocs and copies
        // `filename` into WASM memory before this call (see
        // FlowParser.js:154-159) and frees it in the same `finally` block,
        // so the pointer is valid for `source_filename_size` bytes for the
        // duration of this call. Length includes the null terminator.
        let filename_bytes =
            unsafe { std::slice::from_raw_parts(source_filename, source_filename_size) };
        let len = if source_filename_size > 0 {
            source_filename_size - 1
        } else {
            0
        };
        match std::str::from_utf8(&filename_bytes[..len]) {
            Ok(s) => s.to_string(),
            Err(_) => String::new(),
        }
    };

    // When pragma detection is requested, scan the docblock for `@flow` and
    // override `enable_types` accordingly. Mirrors Hermes' C++
    // `parser::hasFlowPragma` + `parser::getCommentsInDocBlock`
    // (xplat/static_h/lib/Parser/FlowHelpers.cpp:17-82) and the JS adapter
    // helper that previously lived in
    // flow-parser-oxidized/src/index.js::hasFlowPragma.
    let resolved_enable_types = if enable_types_pragma_detection != 0 {
        has_flow_pragma(source_str)
    } else {
        enable_types != 0
    };

    let parse_options = flow_parser::ParseOptions {
        components: enable_components != 0,
        enums: enable_enums != 0,
        pattern_matching: enable_match != 0,
        records: enable_records != 0,
        esproposal_decorators: enable_decorators != 0,
        types: resolved_enable_types,
        use_strict: false,
        assert_operator: assert_operator != 0,
        module_ref_prefix: None,
        ambient: false,
    };

    let file_key = flow_parser::file_key::FileKey::new(
        flow_parser::file_key::FileKeyInner::SourceFile(filename_str),
    );

    // When the JS caller asks for `tokens: true`, install a token sink that
    // captures every lexed `TokenSinkResult` for the duration of the parse.
    // The buffer is owned here, mutated by the sink callback, then handed to
    // the serializer below. When tokens are not requested we pass `None` to
    // the parser to avoid the per-token callback overhead.
    let mut token_buffer: Vec<flow_parser::TokenSinkResult> = Vec::new();
    let (program, errors) = if tokens != 0 {
        let mut sink = |t: flow_parser::TokenSinkResult| token_buffer.push(t);
        flow_parser::parse_program_file::<()>(
            false, // panic_if_failed
            Some(&mut sink),
            Some(parse_options),
            file_key,
            Ok(source_str),
        )
    } else {
        flow_parser::parse_program_file::<()>(
            false, // panic_if_failed
            None,  // token_sink
            Some(parse_options),
            file_key,
            Ok(source_str),
        )
    };

    let mut result = Box::new(ParseResult {
        error: None,
        error_line: 0,
        error_column: 0,
        program_buffer: Vec::new(),
        position_buffer: Vec::new(),
        string_buffer: Vec::new(),
    });

    // Always serialize the (possibly partial) program so JS-side consumers
    // see the full `errors` array; only surface the first error via the
    // single-error C ABI for callers that don't read the array.
    if let Some((loc, err)) = errors.first() {
        result.error = CString::new(format!("{}", err)).ok();
        result.error_line = nonneg_u32(loc.start.line);
        result.error_column = nonneg_u32(loc.start.column);
    }
    let ser = serializer::Serializer::new(source_str);
    let token_slice = if tokens != 0 {
        Some(token_buffer.as_slice())
    } else {
        None
    };
    let buffers = ser.serialize_program(&program, &errors, token_slice);
    result.program_buffer = buffers.program_buffer;
    result.position_buffer = buffers.position_buffer;
    result.string_buffer = buffers.string_buffer;

    Box::into_raw(result)
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_free(result: *mut ParseResult) {
    if !result.is_null() {
        // SAFETY: Non-null path. The pointer was obtained from
        // `Box::into_raw` in `hermesParse` (the only producer of a
        // `*mut ParseResult` in this crate), so reconstituting the
        // `Box<ParseResult>` here is the matching `Box::from_raw` for
        // that allocation. Each ParseResult is freed exactly once: the
        // JS bridge calls `_hermesParseResult_free` from a `finally`
        // block and then drops its handle (FlowParser.js:215-217).
        unsafe {
            drop(Box::from_raw(result));
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getError(result: *const ParseResult) -> *const u8 {
    if result.is_null() {
        return std::ptr::null();
    }
    // SAFETY: Non-null per the check above. `result` was produced by
    // `Box::into_raw(Box<ParseResult>)` in `hermesParse` and remains
    // valid until `hermesParseResult_free` runs; the JS bridge
    // (FlowParser.js:148-217) only calls getters between the producing
    // `flowParse` call and the matching `flowParseResult_free` in the
    // same `finally` block. The shared reference is dropped before the
    // function returns, so it cannot outlive the allocation.
    let result = unsafe { &*result };
    match &result.error {
        Some(err) => err.as_ptr() as *const u8,
        None => std::ptr::null(),
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getErrorLine(result: *const ParseResult) -> u32 {
    if result.is_null() {
        return 0;
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    unsafe { (*result).error_line }
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getErrorColumn(result: *const ParseResult) -> u32 {
    if result.is_null() {
        return 0;
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    unsafe { (*result).error_column }
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getProgramBuffer(result: *const ParseResult) -> *const u32 {
    if result.is_null() {
        return std::ptr::null();
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    let result = unsafe { &*result };
    // Empty buffer signals "the serializer never produced anything" (e.g.
    // invalid UTF-8); represent that as a null pointer to JS.
    if result.program_buffer.is_empty() {
        return std::ptr::null();
    }
    result.program_buffer.as_ptr()
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getPositionBuffer(result: *const ParseResult) -> *const u32 {
    if result.is_null() {
        return std::ptr::null();
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    let result = unsafe { &*result };
    if result.program_buffer.is_empty() {
        return std::ptr::null();
    }
    // PositionResult is #[repr(C)] with 5 x u32, so we can reinterpret the pointer
    result.position_buffer.as_ptr() as *const u32
}

#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getPositionBufferSize(result: *const ParseResult) -> usize {
    if result.is_null() {
        return 0;
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    let result = unsafe { &*result };
    if result.program_buffer.is_empty() {
        return 0;
    }
    result.position_buffer.len()
}

/// Return a pointer to the contiguous UTF-8 byte buffer holding all
/// strings referenced by the program buffer. The program buffer encodes
/// each non-null string reference as `(offset+1, len)` into this buffer
/// (and `(0,)` for null strings). The JS deserializer reads
/// `actual_ptr = base + (encoded_offset - 1)` for non-null entries.
///
/// Returns null when the result is null or the string buffer is empty.
#[unsafe(no_mangle)]
pub extern "C" fn hermesParseResult_getStringBuffer(result: *const ParseResult) -> *const u8 {
    if result.is_null() {
        return std::ptr::null();
    }
    // SAFETY: Same lifetime contract as `hermesParseResult_getError`.
    let result = unsafe { &*result };
    if result.string_buffer.is_empty() {
        return std::ptr::null();
    }
    result.string_buffer.as_ptr()
}

/// Emscripten entry point. Only compiled for wasm32 targets to avoid
/// conflicting with binary crates (e.g. codegen) that link this library
/// and define their own `main`.
#[cfg(target_arch = "wasm32")]
#[unsafe(no_mangle)]
pub extern "C" fn main() -> i32 {
    0
}

/// Scan `code`'s docblock for an `@flow` pragma. Mirrors C++ hermes-parser
/// `parser::hasFlowPragma` + `parser::getCommentsInDocBlock` semantics
/// (`xplat/static_h/lib/Parser/FlowHelpers.cpp:17-82`): walk leading
/// whitespace, `//` and `/* ... */` comments, and directive-prologue string
/// literals (each optionally followed by `;`) up to the first non-directive
/// token, then in each collected comment search for `@flow` followed by a
/// non-word boundary (end of comment, or any character outside
/// `[A-Za-z0-9_]`).
fn has_flow_pragma(code: &str) -> bool {
    let bytes = code.as_bytes();
    let len = bytes.len();
    let mut i = 0;
    while i < len {
        let ch = bytes[i];
        // Whitespace
        if ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r' {
            i += 1;
            continue;
        }
        // Comments
        if ch == b'/' && i + 1 < len {
            let next = bytes[i + 1];
            if next == b'/' {
                i += 2;
                let start = i;
                while i < len {
                    let c = bytes[i];
                    if c == b'\n' || c == b'\r' {
                        break;
                    }
                    i += 1;
                }
                if comment_has_flow_pragma(bytes, start, i) {
                    return true;
                }
                continue;
            }
            if next == b'*' {
                i += 2;
                let start = i;
                while i + 1 < len && !(bytes[i] == b'*' && bytes[i + 1] == b'/') {
                    i += 1;
                }
                let end = i;
                if i + 1 < len {
                    i += 2; // skip the closing `*/`
                }
                if comment_has_flow_pragma(bytes, start, end) {
                    return true;
                }
                continue;
            }
        }
        // String-literal directive prologue
        if ch == b'"' || ch == b'\'' {
            let quote = ch;
            i += 1;
            while i < len {
                let c = bytes[i];
                if c == b'\\' && i + 1 < len {
                    // Skip the escape character; common case is `\"`/`\\`/`\n`.
                    i += 2;
                    continue;
                }
                if c == quote {
                    i += 1;
                    break;
                }
                i += 1;
            }
            continue;
        }
        // Optional `;` between directives
        if ch == b';' {
            i += 1;
            continue;
        }
        // First non-directive token: stop. Anything past this point is
        // regular code, not docblock.
        break;
    }
    false
}

fn comment_has_flow_pragma(bytes: &[u8], start: usize, end: usize) -> bool {
    let needle = b"@flow";
    let mut from = start;
    while from + needle.len() <= end {
        // Manual indexOf (no allocations).
        let mut found_at = None;
        let mut search = from;
        while search + needle.len() <= end {
            if &bytes[search..search + needle.len()] == needle {
                found_at = Some(search);
                break;
            }
            search += 1;
        }
        let idx = match found_at {
            Some(i) => i,
            None => return false,
        };
        let after = idx + needle.len();
        if after >= end {
            return true;
        }
        let c = bytes[after];
        let is_word = c.is_ascii_digit() || c.is_ascii_alphabetic() || c == b'_';
        if !is_word {
            return true;
        }
        from = idx + 1;
    }
    false
}

#[cfg(test)]
mod tests {
    use crate::node_kinds::NodeKind;
    use crate::serializer::Serializer;

    /// Parse `source`, serialize it, and return the program_buffer.
    fn parse_and_serialize(source: &str) -> Vec<u32> {
        let parse_options = flow_parser::ParseOptions {
            components: true,
            enums: true,
            pattern_matching: true,
            records: true,
            esproposal_decorators: true,
            types: true,
            use_strict: false,
            assert_operator: false,
            module_ref_prefix: None,
            ambient: false,
        };
        let file_key = flow_parser::file_key::FileKey::new(
            flow_parser::file_key::FileKeyInner::SourceFile(String::new()),
        );
        let (program, errors) = flow_parser::parse_program_file::<()>(
            false,
            None,
            Some(parse_options),
            file_key,
            Ok(source),
        );
        assert!(
            errors.is_empty(),
            "parse errors for {:?}: {:?}",
            source,
            errors
        );
        let ser = Serializer::new(source);
        let buffers = ser.serialize_program(&program, &[], None);
        buffers.program_buffer
    }

    /// Returns true if any u32 in the buffer equals (kind as u32 + 1),
    /// which is the encoding used by `write_node_header`.
    fn buffer_contains_kind(buf: &[u32], kind: NodeKind) -> bool {
        let needle = kind as u32 + 1;
        buf.contains(&needle)
    }

    #[test]
    fn hook_type_annotation_emitted_for_hook_function_type() {
        let buf = parse_and_serialize("type T = hook () => void;");
        assert!(
            buffer_contains_kind(&buf, NodeKind::HookTypeAnnotation),
            "expected HookTypeAnnotation node header in buffer"
        );
        assert!(
            !buffer_contains_kind(&buf, NodeKind::FunctionTypeAnnotation),
            "should not emit FunctionTypeAnnotation for `hook (...) => void`"
        );
    }

    #[test]
    fn object_type_private_field_emitted_for_private_field_in_declared_class() {
        // `ObjectTypePrivateField` is only legal inside a class-shaped object
        // type (parser threads `is_class: true`), e.g. inside `declare class`.
        // Source matches fixture `flow/ts_syntax/private_class_field.js`.
        let buf = parse_and_serialize("declare class Bar {\n  #private;\n}");
        assert!(
            buffer_contains_kind(&buf, NodeKind::ObjectTypePrivateField),
            "expected ObjectTypePrivateField node header in buffer"
        );
    }

    #[test]
    fn tuple_type_element_emitted_for_optional_unlabeled_tuple_element() {
        let buf = parse_and_serialize("type T = [number?];");
        assert!(
            buffer_contains_kind(&buf, NodeKind::TupleTypeElement),
            "expected TupleTypeElement node header in buffer for optional element"
        );
    }

    #[test]
    fn tuple_type_element_not_emitted_for_required_unlabeled_tuple_element() {
        let buf = parse_and_serialize("type T = [number];");
        assert!(
            !buffer_contains_kind(&buf, NodeKind::TupleTypeElement),
            "should not wrap a non-optional unlabeled element in TupleTypeElement"
        );
    }

    /// Exercise the FFI `hermesParse` entry point with the supplied
    /// `source_type` and return the resulting `ParseResult`. Test callers
    /// own the returned pointer and must call `hermesParseResult_free`.
    fn ffi_parse_with_source_type(source: &str, source_type: i32) -> *mut super::ParseResult {
        // The C ABI expects a null-terminated UTF-8 buffer; mirror the JS
        // bridge which appends `\0` and passes `Buffer.length + 1`.
        let mut bytes = source.as_bytes().to_vec();
        bytes.push(0);
        let size = bytes.len();
        let ptr = bytes.as_ptr();
        let result = super::hermesParse(
            ptr,
            size,
            std::ptr::null(),
            0,
            1, // enable_components
            1, // enable_match
            1, // enable_decorators
            0, // tokens
            0, // allow_return_outside
            0, // assert_operator
            1, // enable_enums
            1, // enable_records
            1, // enable_types
            source_type,
            0, // enable_types_pragma_detection
        );
        // `bytes` must outlive the call — ensure rustc cannot drop it
        // before `hermesParse` returns by reading it after.
        drop(bytes);
        result
    }

    #[test]
    fn module_source_type_does_not_panic() {
        // Pre-fix this would panic with "source_type=2 (module) is not
        // supported. flow_parser pins `allow_await: false` at the top
        // level...". Now `source_type=2` is accepted: imports/exports
        // already parsed regardless of the integer code, and `await` at
        // top level remains usable as an identifier reference (matching
        // OCaml flow_parser, which also pins `allow_await:false` and
        // therefore treats `await` as a valid identifier outside async
        // function bodies — see parser_common.ml:481-491).
        let result = ffi_parse_with_source_type("export const x = 1;", 2);
        assert!(!result.is_null(), "ParseResult pointer should be non-null");
        let buf_ptr = super::hermesParseResult_getProgramBuffer(result);
        assert!(
            !buf_ptr.is_null(),
            "program buffer should be non-null for valid module input"
        );
        let err_ptr = super::hermesParseResult_getError(result);
        assert!(
            err_ptr.is_null(),
            "valid module input should not produce a parse error"
        );
        super::hermesParseResult_free(result);
    }
}
