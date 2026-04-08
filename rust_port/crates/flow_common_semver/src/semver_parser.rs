/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::semver_comparator::Comparator;
use crate::semver_comparator::Op;
use crate::semver_lexer::Lexer;
use crate::semver_lexer::Token;
use crate::semver_range::Part;
use crate::semver_version::Identifier;
use crate::semver_version::Version;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseError;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer::new(input);
        let current = lexer.token();
        Parser { lexer, current }
    }

    fn advance(&mut self) {
        self.current = self.lexer.token();
    }

    fn expect_eof(&mut self) -> Result<(), ParseError> {
        if self.current == Token::Eof {
            Ok(())
        } else {
            Err(ParseError)
        }
    }

    pub fn version_top(&mut self) -> Result<Version, ParseError> {
        let v = self.version()?;
        self.expect_eof()?;
        Ok(v)
    }

    #[allow(dead_code)]
    pub fn comparator_top(&mut self) -> Result<Comparator, ParseError> {
        let c = self.comparator()?;
        self.expect_eof()?;
        Ok(c)
    }

    pub fn range_top(&mut self) -> Result<Vec<Part>, ParseError> {
        let r = self.range()?;
        self.expect_eof()?;
        Ok(r)
    }

    fn version(&mut self) -> Result<Version, ParseError> {
        let major = self.number()?;
        let minor = self.part()?;
        let patch = self.part()?;
        let prerelease = self.prerelease()?;
        let build = self.build()?;
        Ok(Version {
            major,
            minor,
            patch,
            prerelease,
            build,
        })
    }

    fn comparator(&mut self) -> Result<Comparator, ParseError> {
        let op = self.op();
        let version = self.version()?;
        Ok(Comparator { op, version })
    }

    fn number(&mut self) -> Result<i64, ParseError> {
        match &self.current {
            Token::NR(s) => {
                let n = s.parse::<i64>().map_err(|_| ParseError)?;
                self.advance();
                Ok(n)
            }
            _ => Err(ParseError),
        }
    }

    fn part(&mut self) -> Result<i64, ParseError> {
        if self.current == Token::Dot {
            self.advance();
            self.number()
        } else {
            Ok(0)
        }
    }

    fn prerelease(&mut self) -> Result<Vec<Identifier>, ParseError> {
        if self.current == Token::Hyphen {
            self.advance();
            self.identifier_list()
        } else {
            Ok(vec![])
        }
    }

    fn build(&mut self) -> Result<Vec<Identifier>, ParseError> {
        if self.current == Token::Plus {
            self.advance();
            self.identifier_list()
        } else {
            Ok(vec![])
        }
    }

    fn identifier_list(&mut self) -> Result<Vec<Identifier>, ParseError> {
        let first = self.identifier_part()?;
        let mut result = vec![first];
        while self.current == Token::Dot {
            self.advance();
            let part = self.identifier_part()?;
            result.push(part);
        }
        Ok(result)
    }

    fn identifier_part(&mut self) -> Result<Identifier, ParseError> {
        match &self.current {
            Token::ID(s) => {
                let id = Identifier::Str(s.clone());
                self.advance();
                Ok(id)
            }
            Token::NR(s) => {
                let n = s.parse::<i64>().map_err(|_| ParseError)?;
                self.advance();
                Ok(Identifier::Int(n))
            }
            _ => Err(ParseError),
        }
    }

    fn op(&mut self) -> Option<Op> {
        match self.current {
            Token::Lt => {
                self.advance();
                Some(Op::Less)
            }
            Token::Lte => {
                self.advance();
                Some(Op::LessOrEqual)
            }
            Token::Gt => {
                self.advance();
                Some(Op::Greater)
            }
            Token::Gte => {
                self.advance();
                Some(Op::GreaterOrEqual)
            }
            Token::Eq => {
                self.advance();
                Some(Op::Equal)
            }
            _ => None,
        }
    }

    fn range(&mut self) -> Result<Vec<Part>, ParseError> {
        let mut parts = vec![];
        let first = self.range_part()?;
        parts.push(first);
        while self.current != Token::Eof {
            let part = self.range_part()?;
            parts.push(part);
        }
        Ok(parts)
    }

    fn range_part(&mut self) -> Result<Part, ParseError> {
        if self.current == Token::Caret {
            self.advance();
            let v = self.version()?;
            Ok(Part::Caret(v))
        } else {
            let c = self.comparator()?;
            Ok(Part::Comparator(c))
        }
    }
}
