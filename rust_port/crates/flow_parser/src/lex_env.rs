/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::ops::Range;

use dupe::Dupe;

use crate::file_key::FileKey;
use crate::loc::Loc;
use crate::loc::Position;
use crate::parse_error::ParseError;

/// Beginning Of Line position information
#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq)]
pub(super) struct Bol {
    pub(super) line: u32,
    pub(super) offset: u32,
}

/// Lexer state containing accumulated errors
#[derive(Debug, Clone, PartialEq)]
pub struct LexErrors {
    errors: Cow<'static, [(Loc, ParseError)]>,
}

static EMPTY_LEX_ERRORS: Vec<(Loc, ParseError)> = Vec::new();

impl LexErrors {
    pub fn empty() -> Self {
        Self {
            errors: Cow::from(&EMPTY_LEX_ERRORS),
        }
    }

    pub fn push(&mut self, loc: Loc, error: ParseError) {
        self.errors.to_mut().push((loc, error));
    }

    pub fn as_errors(&self) -> &[(Loc, ParseError)] {
        &self.errors
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn into_vec(self) -> Vec<(Loc, ParseError)> {
        self.errors.into_owned()
    }
}

/// Lexer environment containing all state for lexing
#[derive(Debug, Clone, Dupe)]
pub struct LexEnv {
    source: Option<FileKey>,
    lex_bol: Bol,
    comment_syntax_enabled: bool,
    pub(super) in_comment_syntax: bool,
    pub(super) last_loc: Loc,
}

#[derive(Debug, Clone, Copy, Dupe)]
pub(super) struct LexEnvState {
    lex_bol: Bol,
    in_comment_syntax: bool,
    last_loc_has_source: bool,
    last_loc_start: Position,
    last_loc_end: Position,
}

/// The last_loc should initially be set to the beginning of the first line, so that
/// comments on the first line are reported as not being on a new line.
const INITIAL_LAST_LOC: Loc = Loc {
    source: None,
    start: Position { line: 1, column: 0 },
    end: Position { line: 1, column: 0 },
};

impl LexEnv {
    pub fn new(lex_source: Option<FileKey>, enable_types_in_comments: bool) -> Self {
        Self {
            source: lex_source,
            lex_bol: Bol { line: 1, offset: 0 },
            comment_syntax_enabled: enable_types_in_comments,
            in_comment_syntax: false,
            last_loc: INITIAL_LAST_LOC,
        }
    }

    pub(super) fn comment_syntax_enabled(&self) -> bool {
        self.comment_syntax_enabled
    }

    pub(super) fn source(&self) -> Option<FileKey> {
        self.source.dupe()
    }

    #[expect(unused)]
    pub(super) fn line(&self) -> usize {
        self.lex_bol.line as usize
    }

    pub(super) fn new_line(&mut self, span: Range<usize>) {
        let lex_bol = Bol {
            line: self.lex_bol.line + 1,
            offset: span.end as u32,
        };
        self.lex_bol = lex_bol
    }

    pub(super) fn pos_at_offset(&self, offset: usize) -> Position {
        Position {
            line: self.lex_bol.line as i32,
            column: ((offset as u32) - self.lex_bol.offset) as i32,
        }
    }

    pub(super) fn loc_of_offsets(&self, start_offset: usize, end_offset: usize) -> Loc {
        Loc {
            source: self.source.dupe(),
            start: self.pos_at_offset(start_offset),
            end: self.pos_at_offset(end_offset),
        }
    }

    pub(super) fn loc_of_span(&self, span: &Range<usize>) -> Loc {
        self.loc_of_offsets(span.start, span.end)
    }

    pub(super) fn state(&self) -> LexEnvState {
        LexEnvState {
            lex_bol: self.lex_bol,
            in_comment_syntax: self.in_comment_syntax,
            last_loc_has_source: self.last_loc.source.is_some(),
            last_loc_start: self.last_loc.start,
            last_loc_end: self.last_loc.end,
        }
    }

    pub(super) fn restore_state(&mut self, state: LexEnvState) {
        self.lex_bol = state.lex_bol;
        self.in_comment_syntax = state.in_comment_syntax;
        self.last_loc = Loc {
            source: if state.last_loc_has_source {
                self.source.dupe()
            } else {
                None
            },
            start: state.last_loc_start,
            end: state.last_loc_end,
        };
    }
}
