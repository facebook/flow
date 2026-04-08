/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    NR(String),
    ID(String),
    Hyphen,
    Plus,
    Dot,
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
    Caret,
    Eof,
    Error,
}

pub struct Lexer<'a> {
    input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.as_bytes(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<u8> {
        if self.pos < self.input.len() {
            Some(self.input[self.pos])
        } else {
            None
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn is_identifier_start(c: u8) -> bool {
        c.is_ascii_alphanumeric()
    }

    fn is_identifier_cont(c: u8) -> bool {
        c.is_ascii_alphanumeric() || c == b'_'
    }

    pub fn token(&mut self) -> Token {
        while let Some(c) = self.peek() {
            if c == b' ' || c == b'\t' {
                self.advance();
            } else {
                break;
            }
        }

        match self.peek() {
            None => Token::Eof,
            Some(c) => match c {
                c if Self::is_identifier_start(c) => {
                    let start = self.pos;
                    let mut all_digits = c.is_ascii_digit();
                    self.advance();
                    while let Some(c) = self.peek() {
                        if Self::is_identifier_cont(c) {
                            if !c.is_ascii_digit() {
                                all_digits = false;
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    let s = std::str::from_utf8(&self.input[start..self.pos])
                        .unwrap()
                        .to_string();
                    if all_digits {
                        if s == "0" || s.as_bytes()[0] != b'0' {
                            Token::NR(s)
                        } else {
                            Token::ID(s)
                        }
                    } else {
                        Token::ID(s)
                    }
                }
                b'-' => {
                    self.advance();
                    Token::Hyphen
                }
                b'+' => {
                    self.advance();
                    Token::Plus
                }
                b'.' => {
                    self.advance();
                    Token::Dot
                }
                b'<' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        Token::Lte
                    } else {
                        Token::Lt
                    }
                }
                b'>' => {
                    self.advance();
                    if self.peek() == Some(b'=') {
                        self.advance();
                        Token::Gte
                    } else {
                        Token::Gt
                    }
                }
                b'=' => {
                    self.advance();
                    Token::Eq
                }
                b'^' => {
                    self.advance();
                    Token::Caret
                }
                _ => {
                    self.advance();
                    Token::Error
                }
            },
        }
    }
}
