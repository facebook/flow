// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use std::collections::HashMap;

use dupe::Dupe;
#[cfg(test)]
use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::offset_utils::OffsetTable;
use serde::Serialize;
use thiserror::Error;

/// A source-map v3 document. Columns and offsets use UTF-16 code units.
#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct SourceMap {
    version: u8,
    file: String,
    sources: Vec<String>,
    sources_content: Vec<String>,
    names: Vec<FlowSmolStr>,
    mappings: String,
}

/// A source location that cannot be mapped against the supplied source.
#[derive(Debug, Error)]
pub enum SourceMapError {
    #[error("Source-map printing requires concrete locations")]
    KeyedLocation,
    #[error("Source location belongs to {actual:?}, expected {expected:?}")]
    ForeignSource { expected: FileKey, actual: FileKey },
    #[error("Invalid source position: {0:?}")]
    InvalidPosition(Position),
}

#[derive(Debug, PartialEq, Eq)]
struct OriginalPosition {
    line: i64,
    column: i64,
    name: Option<i64>,
}

struct Segment {
    column: i64,
    original: Option<OriginalPosition>,
}

pub(crate) struct SourceMapTracker {
    source_file: FileKey,
    source_text: String,
    offsets: OffsetTable,
    generated_line: usize,
    generated_column: i64,
    after_cr: bool,
    lines: Vec<Vec<Segment>>,
    names: Vec<FlowSmolStr>,
    name_indices: HashMap<FlowSmolStr, i64>,
}

impl SourceMapTracker {
    pub(crate) fn new(source_file: &FileKey, source_text: &str) -> Self {
        Self {
            source_file: source_file.dupe(),
            source_text: source_text.to_owned(),
            offsets: OffsetTable::make(source_text),
            generated_line: 0,
            generated_column: 0,
            after_cr: false,
            lines: Vec::new(),
            names: Vec::new(),
            name_indices: HashMap::new(),
        }
    }

    pub(crate) fn advance(&mut self, text: &str) {
        for ch in text.chars() {
            match ch {
                '\n' if self.after_cr => {}
                '\r' | '\n' | '\u{2028}' | '\u{2029}' => {
                    self.generated_line += 1;
                    self.generated_column = 0;
                }
                _ => self.generated_column += ch.len_utf16() as i64,
            }
            self.after_cr = ch == '\r';
        }
    }

    pub(crate) fn advance_indent(&mut self, indent: usize) {
        self.generated_column += indent as i64;
        self.after_cr = false;
    }

    #[cfg(test)]
    pub(crate) fn record(
        &mut self,
        location: Option<&ALoc>,
        name: Option<&FlowSmolStr>,
    ) -> Result<(), SourceMapError> {
        if let Some(location) = location {
            if location.is_keyed() {
                return Err(SourceMapError::KeyedLocation);
            }
            return self.record_loc(Some(location.to_loc_exn()), name);
        }
        self.record_loc(None, name)
    }

    pub(crate) fn record_loc(
        &mut self,
        location: Option<&Loc>,
        name: Option<&FlowSmolStr>,
    ) -> Result<(), SourceMapError> {
        let original = match location {
            None => None,
            Some(location) => {
                if location.is_none_ignore_source() {
                    None
                } else {
                    if let Some(source_file) = &location.source
                        && source_file != &self.source_file
                    {
                        return Err(SourceMapError::ForeignSource {
                            expected: self.source_file.dupe(),
                            actual: source_file.dupe(),
                        });
                    }
                    if location.start.line < 1 || location.start.column < 0 {
                        return Err(SourceMapError::InvalidPosition(location.start));
                    }
                    let position = self
                        .offsets
                        .convert_flow_position_to_js_position(location.start)
                        .map_err(|error| SourceMapError::InvalidPosition(error.position))?;
                    let name = name.map(|name| {
                        let next = self.names.len() as i64;
                        *self.name_indices.entry(name.dupe()).or_insert_with(|| {
                            self.names.push(name.dupe());
                            next
                        })
                    });
                    Some(OriginalPosition {
                        line: i64::from(position.line - 1),
                        column: i64::from(position.column),
                        name,
                    })
                }
            }
        };
        self.lines.resize_with(self.generated_line + 1, Vec::new);
        let line = &mut self.lines[self.generated_line];
        if line.last().is_some_and(|last| last.original == original) {
            return Ok(());
        }
        line.push(Segment {
            column: self.generated_column,
            original,
        });
        Ok(())
    }

    pub(crate) fn finish(self) -> SourceMap {
        let mut mappings = String::new();
        let mut original_line = 0;
        let mut original_column = 0;
        let mut name_index = 0;
        for (line_index, line) in self.lines.iter().enumerate() {
            if line_index != 0 {
                mappings.push(';');
            }
            let mut generated_column = 0;
            for (index, segment) in line.iter().enumerate() {
                if index != 0 {
                    mappings.push(',');
                }
                encode_vlq(&mut mappings, segment.column - generated_column);
                generated_column = segment.column;
                if let Some(original) = &segment.original {
                    encode_vlq(&mut mappings, 0);
                    encode_vlq(&mut mappings, original.line - original_line);
                    encode_vlq(&mut mappings, original.column - original_column);
                    original_line = original.line;
                    original_column = original.column;
                    if let Some(name) = original.name {
                        encode_vlq(&mut mappings, name - name_index);
                        name_index = name;
                    }
                }
            }
        }
        let filename = self.source_file.as_str().to_owned();
        SourceMap {
            version: 3,
            file: filename.clone(),
            sources: vec![filename],
            sources_content: vec![self.source_text],
            names: self.names,
            mappings,
        }
    }
}

fn encode_vlq(output: &mut String, value: i64) {
    const DIGITS: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut value = (value.unsigned_abs() << 1) | u64::from(value < 0);
    loop {
        let mut digit = (value & 31) as usize;
        value >>= 5;
        if value != 0 {
            digit |= 32;
        }
        output.push(char::from(DIGITS[digit]));
        if value == 0 {
            break;
        }
    }
}
