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

#[cfg(test)]
mod tests {
    #[derive(Debug)]
    struct SemverParseError(String);

    use super::Parser;
    use crate::semver_comparator;
    use crate::semver_comparator::Comparator;
    use crate::semver_comparator::Op;
    use crate::semver_range;
    use crate::semver_range::Part;
    use crate::semver_version;
    use crate::semver_version::Identifier;
    use crate::semver_version::Version;

    fn parse_version(str: &str) -> Result<Version, SemverParseError> {
        let mut parser = Parser::new(str);
        parser
            .version_top()
            .map_err(|_| SemverParseError(str.to_string()))
    }

    fn parse_comparator(str: &str) -> Result<Comparator, SemverParseError> {
        let mut parser = Parser::new(str);
        parser
            .comparator_top()
            .map_err(|_| SemverParseError(str.to_string()))
    }

    fn parse_range(str: &str) -> Result<Vec<Part>, SemverParseError> {
        let mut parser = Parser::new(str);
        parser
            .range_top()
            .map_err(|_| SemverParseError(str.to_string()))
    }

    #[test]
    fn version_basics() {
        let cases: Vec<(&str, Version)> = vec![
            ("0", semver_version::zero()),
            (
                "0.1",
                Version {
                    minor: 1,
                    ..semver_version::zero()
                },
            ),
            (
                "1",
                Version {
                    major: 1,
                    ..semver_version::zero()
                },
            ),
            (
                "1.2",
                Version {
                    major: 1,
                    minor: 2,
                    ..semver_version::zero()
                },
            ),
            (
                "1.2.3",
                Version {
                    major: 1,
                    minor: 2,
                    patch: 3,
                    ..semver_version::zero()
                },
            ),
            (
                "1.2.3-alpha",
                Version {
                    major: 1,
                    minor: 2,
                    patch: 3,
                    prerelease: vec![Identifier::Str("alpha".to_string())],
                    ..semver_version::zero()
                },
            ),
            (
                "1.2.3-alpha.2",
                Version {
                    major: 1,
                    minor: 2,
                    patch: 3,
                    prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(2)],
                    ..semver_version::zero()
                },
            ),
        ];
        for (str, version) in &cases {
            match parse_version(str) {
                Ok(parsed) => assert_eq!(
                    semver_version::to_string(version),
                    semver_version::to_string(&parsed),
                    "Expected {}, got {}",
                    semver_version::to_string(version),
                    semver_version::to_string(&parsed),
                ),
                Err(SemverParseError(token)) => {
                    panic!("Failed to parse {}: unexpected token {}", str, token)
                }
            }
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }

    // fixes ounit error reporting

    #[test]
    fn comparator_basics() {
        let v1 = Version {
            major: 1,
            ..semver_version::zero()
        };
        let cases: Vec<(&str, Comparator)> = vec![
            (
                ">1",
                Comparator {
                    op: Some(Op::Greater),
                    version: v1.clone(),
                },
            ),
            (
                ">=1",
                Comparator {
                    op: Some(Op::GreaterOrEqual),
                    version: v1.clone(),
                },
            ),
            (
                "<1",
                Comparator {
                    op: Some(Op::Less),
                    version: v1.clone(),
                },
            ),
            (
                "<=1",
                Comparator {
                    op: Some(Op::LessOrEqual),
                    version: v1.clone(),
                },
            ),
            (
                "=1",
                Comparator {
                    op: Some(Op::Equal),
                    version: v1.clone(),
                },
            ),
            (
                "1",
                Comparator {
                    op: None,
                    version: v1.clone(),
                },
            ),
            (
                "= 1",
                Comparator {
                    op: Some(Op::Equal),
                    version: v1.clone(),
                },
            ),
            (
                " = 1",
                Comparator {
                    op: Some(Op::Equal),
                    version: v1.clone(),
                },
            ),
            (
                "  = 1 ",
                Comparator {
                    op: Some(Op::Equal),
                    version: v1.clone(),
                },
            ),
        ];
        for (str, comparator) in &cases {
            match parse_comparator(str) {
                Ok(parsed) => assert_eq!(
                    semver_comparator::to_string(comparator),
                    semver_comparator::to_string(&parsed),
                    "Expected {}, got {}",
                    semver_comparator::to_string(comparator),
                    semver_comparator::to_string(&parsed),
                ),
                Err(SemverParseError(token)) => {
                    panic!("Failed to parse {}: unexpected token {}", str, token)
                }
            }
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }

    // fixes ounit error reporting

    #[test]
    fn range_basics() {
        let v1 = Version {
            major: 1,
            ..semver_version::zero()
        };
        let v2 = Version {
            major: 2,
            ..semver_version::zero()
        };
        let ge1 = Part::Comparator(Comparator {
            op: Some(Op::GreaterOrEqual),
            version: v1.clone(),
        });
        let lt2 = Part::Comparator(Comparator {
            op: Some(Op::Less),
            version: v2.clone(),
        });
        let cases: Vec<(&str, Vec<Part>)> = vec![
            (">=1", vec![ge1.clone()]),
            (">=1 <2", vec![ge1.clone(), lt2.clone()]),
            ("^1", vec![Part::Caret(v1.clone())]),
            ("^1.0", vec![Part::Caret(v1.clone())]),
            ("^1.0.0", vec![Part::Caret(v1.clone())]),
            (
                "^1 ^2",
                vec![Part::Caret(v1.clone()), Part::Caret(v2.clone())],
            ),
            (">=1 ^2", vec![ge1.clone(), Part::Caret(v2.clone())]),
        ];
        for (str, range) in &cases {
            match parse_range(str) {
                Ok(parsed) => assert_eq!(
                    semver_range::to_string(range),
                    semver_range::to_string(&parsed),
                    "Expected {}, got {}",
                    semver_range::to_string(range),
                    semver_range::to_string(&parsed),
                ),
                Err(SemverParseError(token)) => {
                    panic!("Failed to parse {}: unexpected token {}", str, token)
                }
            }
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }
}
