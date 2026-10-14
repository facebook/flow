// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

//! Direct JavaScript AST emission with indentation and conservative parentheses.

mod expressions;
mod jsx;
mod patterns;
mod statements;

use std::collections::HashSet;
use std::sync::Arc;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast;
use flow_parser::loc::Loc;
use thiserror::Error;

use crate::source::Source;

/// The printer accepts JavaScript after runtime lowering and type erasure.
#[derive(Debug, Error)]
pub enum PrintError {
    #[error("Statement requires lowering before JavaScript emission at {0:?}")]
    UnloweredStatement(Loc),
    #[error("Expression requires lowering before JavaScript emission at {0:?}")]
    UnloweredExpression(Loc),
    #[error("Type annotation remains before JavaScript emission at {0:?}")]
    TypeAnnotation(Loc),
    #[error("Class element requires lowering before JavaScript emission at {0:?}")]
    UnloweredClassElement(Loc),
    #[error("Non-arrow function requires a block body at {0:?}")]
    FunctionBody(Loc),
    #[error("Template requires one more quasi than expressions at {0:?}")]
    TemplateShape(Loc),
    #[error("Directive requires a string literal at {0:?}")]
    DirectiveShape(Loc),
}

type Expr = ast::expression::Expression<Loc, Loc>;
type Stmt = ast::statement::Statement<Loc, Loc>;
type Pattern = ast::pattern::Pattern<Loc, Loc>;
type Program = ast::Program<Loc, Loc>;
type Printed<T = ()> = Result<T, PrintError>;

struct Writer {
    source: Source,
    indent: usize,
    line_start: bool,
    empty: bool,
    location: Loc,
    comments: HashSet<Loc>,
}

impl Writer {
    fn new() -> Self {
        Self {
            source: Source::default(),
            indent: 0,
            line_start: true,
            empty: true,
            location: Loc::default(),
            comments: HashSet::new(),
        }
    }

    fn at<T>(&mut self, loc: &Loc, f: impl FnOnce(&mut Self) -> Printed<T>) -> Printed<T> {
        let previous = self.location.dupe();
        self.location = loc.dupe();
        let result = f(self);
        self.location = previous;
        result
    }

    fn token(&mut self, text: &str) -> Printed {
        self.write(text, None)
    }

    fn write(&mut self, text: &str, _name: Option<&FlowSmolStr>) -> Printed {
        if text.is_empty() {
            return Ok(());
        }
        if self.line_start {
            let spaces = self.indent * 2;
            self.source.add_space(spaces);
            self.line_start = false;
        }
        self.source.add_string(text);
        self.empty = false;
        Ok(())
    }

    fn newline(&mut self) {
        self.source.add_newline();
        self.empty = false;
        self.line_start = true;
    }

    fn comment(&mut self, comment: &ast::Comment<Loc>) -> Printed {
        if !self.comments.insert(comment.loc.dupe()) {
            return Ok(());
        }
        if comment.on_newline && !self.line_start {
            self.newline();
        }
        self.at(&comment.loc, |writer| {
            match comment.kind {
                ast::CommentKind::Line => {
                    writer.token("//")?;
                    writer.token(&comment.text)?;
                    writer.newline();
                }
                ast::CommentKind::Block => {
                    writer.token("/*")?;
                    writer.token(&comment.text)?;
                    writer.token("*/ ")?;
                }
            }
            Ok(())
        })
    }

    fn leading<I: Dupe>(&mut self, syntax: &Option<ast::Syntax<Loc, I>>) -> Printed {
        if let Some(syntax) = syntax {
            for comment in syntax.leading.iter() {
                self.comment(comment)?;
            }
        }
        Ok(())
    }

    fn trailing<I: Dupe>(&mut self, syntax: &Option<ast::Syntax<Loc, I>>) -> Printed {
        if let Some(syntax) = syntax {
            for comment in syntax.trailing.iter() {
                self.comment(comment)?;
            }
        }
        Ok(())
    }

    fn commented<I: Dupe>(
        &mut self,
        syntax: &Option<ast::Syntax<Loc, I>>,
        f: impl FnOnce(&mut Self) -> Printed,
    ) -> Printed {
        self.leading(syntax)?;
        f(self)?;
        self.trailing(syntax)
    }

    fn internal(&mut self, syntax: &Option<ast::Syntax<Loc, Arc<[ast::Comment<Loc>]>>>) -> Printed {
        if let Some(syntax) = syntax {
            for comment in syntax.internal.iter() {
                self.comment(comment)?;
            }
        }
        Ok(())
    }

    fn identifier(&mut self, id: &ast::Identifier<Loc, Loc>) -> Printed {
        self.at(&id.loc, |writer| {
            writer.leading(&id.comments)?;
            writer.write(&id.name, Some(&id.name))?;
            writer.trailing(&id.comments)
        })
    }

    fn private_name(&mut self, name: &ast::PrivateName<Loc>) -> Printed {
        self.at(&name.loc, |writer| {
            writer.leading(&name.comments)?;
            writer.token("#")?;
            writer.write(&name.name, Some(&name.name))?;
            writer.trailing(&name.comments)
        })
    }

    fn string(&mut self, literal: &ast::StringLiteral<Loc>) -> Printed {
        self.leading(&literal.comments)?;
        if literal.raw.is_empty() {
            self.quoted(&literal.value)?;
        } else {
            self.token(&literal.raw)?;
        }
        self.trailing(&literal.comments)
    }

    fn quoted(&mut self, value: &str) -> Printed {
        let mut text = String::from("\"");
        for ch in value.chars() {
            match ch {
                '"' => text.push_str("\\\""),
                '\\' => text.push_str("\\\\"),
                '\n' => text.push_str("\\n"),
                '\r' => text.push_str("\\r"),
                '\t' => text.push_str("\\t"),
                '\u{2028}' => text.push_str("\\u2028"),
                '\u{2029}' => text.push_str("\\u2029"),
                ch if ch < ' ' => text.push_str(&format!("\\u{:04x}", u32::from(ch))),
                ch => text.push(ch),
            }
        }
        text.push('"');
        self.token(&text)
    }

    fn annotation(&self, annotation: &ast::types::AnnotationOrHint<Loc, Loc>) -> Printed {
        match annotation {
            ast::types::AnnotationOrHint::Missing(_) => Ok(()),
            ast::types::AnnotationOrHint::Available(annotation) => {
                Err(PrintError::TypeAnnotation(annotation.loc.dupe()))
            }
        }
    }

    fn program(&mut self, program: &Program) -> Printed {
        if let Some((loc, interpreter)) = &program.interpreter {
            self.at(loc, |writer| writer.token(&format!("#!{interpreter}")))?;
            self.newline();
        }
        self.leading(&program.comments)?;
        self.statements(&program.statements)?;
        self.trailing(&program.comments)?;
        for comment in program.all_comments.iter() {
            self.comment(comment)?;
        }
        if !self.line_start || self.empty {
            self.newline();
        }
        Ok(())
    }
}

/// Print the lowered JavaScript AST directly, with fixed indentation.
pub fn print(program: &Program) -> Printed<Source> {
    let mut writer = Writer::new();
    writer.program(program)?;
    Ok(writer.source)
}
